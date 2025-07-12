#include "bigint.h"
#include "bigint_extra.h"
#include "timing.hpp"
#include <algorithm>
#include <cctype>
#include <cmath>
#include <iostream>
#include <map>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <unordered_set>
#include <vector>

namespace evl {
enum EvalVersion { BIGINT, DOUBLE, LOGIC, BOOLEAN, COMMAND, PHYSICS, ERROR };

enum TokenType {
  NUMBER,
  OPERATOR,
  FUNCTION,
  PARENTHESIS,
  PREV,
  VARIABLE,
  ASSIGNMENT
};

// Mathematical constants
const std::map<std::string, std::string> math_constants = {
    {"pi", "3.141592653589793"},
    {"e", "2.718281828459045"},
    {"phi", "1.618033988749895"}};

// Physics constants
const std::map<std::string, std::string> physics_constants = {
    {"c", "2.99792458e8"}};

// Variable storage
std::map<std::string, double> double_variables;
std::map<std::string, bigint> bigint_variables;

// Banned names for variables
const std::unordered_set<std::string> BANNED_NAMES = {
    // Mathematical constants
    "pi", "e", "phi", "euler", "gamma",
    // Trigonometric functions
    "sin", "cos", "tan", "tg", "ctg", "cot", "sec", "csc", "asin", "acos",
    "atan", "atan2", "arcsin", "arccos", "arctan", "arctg", "arcctg", "arccot",
    "sinh", "cosh", "tanh", "asinh", "acosh", "atanh",
    // Logarithmic functions
    "ln", "lg", "log", "log2", "log10", "exp", "exp2",
    // Power and root functions
    "sqrt", "cbrt", "pow", "sqr", "square", "cube",
    // Other math functions
    "abs", "fabs", "ceil", "floor", "round", "trunc", "max", "min", "sum",
    "avg", "mean", "factorial", "fact", "gamma", "lgamma",
    // BigInt functions
    "fib", "fibonacci", "gcd", "lcm", "powmod", "binomial",
    // Special tokens and commands
    "ans", "prev", "last", "result", "help", "exit", "quit", "clear", "cls",
    "deg", "rad", "mode", "vars", "variables", "var", "v", "double", "int",
    "bigint", "exact", "aprox",
    // Operators and keywords
    "mod", "div", "and", "or", "not", "xor", "if", "then", "else", "for",
    "while", "do", "true", "false", "null", "undefined", "infinity", "nan",
    // Physics constants
    "c", "h", "k", "g", "R", "Na", "F", "lightspeed", "planck", "boltzmann",
    "avogadro",
    // Units
    "m", "kg", "s", "A", "K", "mol", "cd", "mm", "cm", "km", "g", "mg", "ms",
    "ns",
    // System names
    "system", "eval", "calc", "function", "let", "const"};

static bool time_testing = false;
static bool trig_mode_degrees = false;
// Storage for results
static double double_answer = 0;
bigint bigint_answer = 0;
const double DEG_TO_RAD = 0.01745329251994329576923690768488612713412398;

struct Token {
  TokenType type;
  std::string value;
};

// Tokenizer
std::vector<Token> tokenize(const std::string &expr) {
  if (time_testing) {
    time();
  }

  std::vector<Token> tokens;
  std::string num;

  for (size_t i = 0; i < expr.size(); ++i) {
    char ch = expr[i];

    if (isspace(ch) || ch == ',')
      continue;

    if (isdigit(ch) || ch == '.') {
      bool has_dot = (ch == '.');
      num += ch;
      while (i + 1 < expr.size() &&
             (isdigit(expr[i + 1]) || expr[i + 1] == '.')) {
        if (expr[i + 1] == '.') {
          if (has_dot) {
            throw std::runtime_error("Double .");
          } else {
            has_dot = true;
          }
        }
        num += expr[++i];
      }
      tokens.push_back({NUMBER, num});
      num.clear();
    } else if (isalpha(ch)) {
      std::string func;
      func += ch;
      while (i + 1 < expr.size() && isalpha(expr[i + 1]))
        func += expr[++i];

      if (func == "ans") {
        tokens.push_back({PREV, ""});
      } else if (math_constants.find(func) != math_constants.end()) {
        tokens.push_back({NUMBER, math_constants.at(func)});
      } else if (double_variables.find(func) != double_variables.end() ||
                 bigint_variables.find(func) != bigint_variables.end()) {
        tokens.push_back({VARIABLE, func});
      } else {
        tokens.push_back({FUNCTION, func});
      }
    }
    // Separate handling of minus for unary detection
    else if (ch == '-') {
      if (tokens.empty() || tokens.back().type == OPERATOR ||
          (tokens.back().type == PARENTHESIS && tokens.back().value == "(")) {
        tokens.push_back({OPERATOR, "~"}); // unary minus
      } else {
        tokens.push_back({OPERATOR, "-"});
      }
    } else if (ch == '+' || ch == '*' || ch == '/' || ch == '%' || ch == '^' ||
               ch == '!') {
      tokens.push_back({OPERATOR, std::string(1, ch)});
    } else if (ch == '=') {
      tokens.push_back({ASSIGNMENT, "="});
    }
    // Parentheses
    else if (ch == '(' || ch == ')') {
      tokens.push_back({PARENTHESIS, std::string(1, ch)});
    } else {
      throw std::runtime_error("Unknown character in expression");
    }
  }

  return tokens;
}

int precedence(const std::string &op) {
  if (op == "!" || op == "~")
    return 5;
  if (op == "^")
    return 4;
  if (op == "*" || op == "/" || op == "%")
    return 3;
  if (op == "+" || op == "-")
    return 2;
  return 0;
}

bool is_right_associative(const std::string &op) {
  return op == "^" || op == "!" || op == "~";
}

std::vector<Token> to_postfix(const std::vector<Token> &tokens) {
  std::vector<Token> output;
  std::stack<Token> ops;

  for (const auto &token : tokens) {
    if (token.type == NUMBER) {
      output.push_back(token);
    } else if (token.type == PREV || token.type == VARIABLE) {
      output.push_back(token);
    } else if (token.type == ASSIGNMENT) {
      ops.push(token);
    } else if (token.type == FUNCTION) {
      ops.push(token);
    } else if (token.type == OPERATOR) {
      while (!ops.empty() &&
             ((ops.top().type == FUNCTION) ||
              (ops.top().type == OPERATOR &&
               (precedence(ops.top().value) > precedence(token.value) ||
                (precedence(ops.top().value) == precedence(token.value) &&
                 !is_right_associative(token.value)))))) {
        output.push_back(ops.top());
        ops.pop();
      }
      ops.push(token);
    } else if (token.value == "(") {
      ops.push(token);
    } else if (token.value == ")") {
      while (!ops.empty() && ops.top().value != "(") {
        output.push_back(ops.top());
        ops.pop();
      }
      if (!ops.empty())
        ops.pop(); // remove '('
      if (!ops.empty() && ops.top().type == FUNCTION) {
        output.push_back(ops.top());
        ops.pop();
      }
    }
  }

  while (!ops.empty()) {
    output.push_back(ops.top());
    ops.pop();
  }

  return output;
}

// Helper function for safe stack operations
double safe_pop_double(std::stack<double> &stk) {
  if (stk.empty())
    throw std::runtime_error("Stack underflow - insufficient operands");
  double val = stk.top();
  stk.pop();
  return val;
}

double eval_postfix_double(const std::vector<Token> &postfix) {
  std::stack<double> stk;

  for (const auto &token : postfix) {
    if (token.type == NUMBER) {
      stk.push(stod(token.value));
    } else if (token.type == PREV) {
      stk.push(double_answer);
    } else if (token.type == VARIABLE) {
      if (double_variables.find(token.value) != double_variables.end()) {
        stk.push(double_variables[token.value]);
      } else {
        throw std::runtime_error("Undefined variable: " + token.value);
      }
    } else if (token.type == OPERATOR || token.type == FUNCTION) {
      if (token.value == "+") {
        if (stk.size() < 2)
          throw std::runtime_error("Insufficient operands for +");
        double b = safe_pop_double(stk);
        double a = safe_pop_double(stk);
        stk.push(a + b);
      } else if (token.value == "-") {
        if (stk.size() < 2)
          throw std::runtime_error("Insufficient operands for -");
        double b = safe_pop_double(stk);
        double a = safe_pop_double(stk);
        stk.push(a - b);
      } else if (token.value == "*") {
        if (stk.size() < 2)
          throw std::runtime_error("Insufficient operands for *");
        double b = safe_pop_double(stk);
        double a = safe_pop_double(stk);
        stk.push(a * b);
      } else if (token.value == "/") {
        if (stk.size() < 2)
          throw std::runtime_error("Insufficient operands for /");
        double b = safe_pop_double(stk);
        double a = safe_pop_double(stk);
        if (b == 0)
          throw std::runtime_error("Division by zero");
        stk.push(a / b);
      } else if (token.value == "%") {
        if (stk.size() < 2)
          throw std::runtime_error("Insufficient operands for %");
        double b = safe_pop_double(stk);
        double a = safe_pop_double(stk);
        stk.push(fmod(a, b));
      } else if (token.value == "^") {
        if (stk.size() < 2)
          throw std::runtime_error("Insufficient operands for ^");
        double b = safe_pop_double(stk);
        double a = safe_pop_double(stk);
        stk.push(pow(a, b));
      } else if (token.value == "~") {
        if (stk.empty())
          throw std::runtime_error("Insufficient operands for unary -");
        double a = safe_pop_double(stk);
        stk.push(-a);
      } else {
        // Binary functions
        if (token.value == "log") {
          if (stk.size() < 2)
            throw std::runtime_error("Insufficient operands for log");
          double arg = safe_pop_double(stk);
          double base = safe_pop_double(stk);
          if (arg <= 0 || base <= 0 || base == 1)
            throw std::runtime_error("Logarithm error");
          stk.push(log(arg) / log(base));
        } else {
          // Unary functions
          if (stk.empty())
            throw std::runtime_error("Insufficient operands for function " +
                                     token.value);
          double a = safe_pop_double(stk);

          if (token.value == "sin") {
            if (trig_mode_degrees) {
              stk.push(sin(a * DEG_TO_RAD));
            } else {
              stk.push(sin(a));
            }
          } else if (token.value == "cos") {
            if (trig_mode_degrees) {
              stk.push(cos(a * DEG_TO_RAD));
            } else {
              stk.push(cos(a));
            }
          } else if (token.value == "tan" || token.value == "tg") {
            stk.push(tan(a));
          } else if (token.value == "ctg") {
            stk.push(1.0 / tan(a));
          } else if (token.value == "arcsin") {
            if (a < -1 || a > 1)
              throw std::runtime_error("arcsin out of domain");
            stk.push(asin(a));
          } else if (token.value == "arccos") {
            if (a < -1 || a > 1)
              throw std::runtime_error("arccos out of domain");
            stk.push(acos(a));
          } else if (token.value == "arctan" || token.value == "arctg") {
            stk.push(atan(a));
          } else if (token.value == "arcctg" || token.value == "arccot") {
            if (a == 0)
              throw std::runtime_error("arcctg undefined for zero");
            stk.push(atan(1.0 / a));
          } else if (token.value == "ln")
            stk.push(log(a));
          else if (token.value == "lg")
            stk.push(log10(a));
          else if (token.value == "sqrt")
            stk.push(sqrt(a));
          else if (token.value == "abs")
            stk.push(abs(a));
          else
            throw std::runtime_error("Unknown function: " + token.value);
        }
      }
    }
  }

  if (stk.size() != 1)
    throw std::runtime_error("Incorrect expression");
  if (time_testing) {
    time();
  }
  return stk.top();
}

// Helper function for safe bigint stack operations
bigint safe_pop_bigint(std::stack<bigint> &stk) {
  if (stk.empty())
    throw std::runtime_error("Stack underflow - insufficient operands");
  bigint val = stk.top();
  stk.pop();
  return val;
}

bigint eval_postfix_bigint(const std::vector<Token> &postfix) {
  std::stack<bigint> stk;

  for (const auto &token : postfix) {
    if (token.type == NUMBER) {
      stk.push(bigint(token.value));
    } else if (token.type == PREV) {
      stk.push(bigint_answer);
    } else if (token.type == VARIABLE) {
      if (bigint_variables.find(token.value) != bigint_variables.end()) {
        stk.push(bigint_variables[token.value]);
      } else {
        throw std::runtime_error("Undefined variable: " + token.value);
      }
    } else if (token.type == OPERATOR || token.type == FUNCTION) {
      if (token.value == "+") {
        if (stk.size() < 2)
          throw std::runtime_error("Insufficient operands for +");
        bigint b = safe_pop_bigint(stk);
        bigint a = safe_pop_bigint(stk);
        stk.push(a + b);
      } else if (token.value == "-") {
        if (stk.size() < 2)
          throw std::runtime_error("Insufficient operands for -");
        bigint b = safe_pop_bigint(stk);
        bigint a = safe_pop_bigint(stk);
        stk.push(a - b);
      } else if (token.value == "*") {
        if (stk.size() < 2)
          throw std::runtime_error("Insufficient operands for *");
        bigint b = safe_pop_bigint(stk);
        bigint a = safe_pop_bigint(stk);
        stk.push(a * b);
      } else if (token.value == "/") {
        if (stk.size() < 2)
          throw std::runtime_error("Insufficient operands for /");
        bigint b = safe_pop_bigint(stk);
        bigint a = safe_pop_bigint(stk);
        if (b.isZero())
          throw std::runtime_error("Division by zero");
        stk.push(a / b);
      } else if (token.value == "%") {
        if (stk.size() < 2)
          throw std::runtime_error("Insufficient operands for %");
        bigint b = safe_pop_bigint(stk);
        bigint a = safe_pop_bigint(stk);
        stk.push(a % b);
      } else if (token.value == "^") {
        if (stk.size() < 2)
          throw std::runtime_error("Insufficient operands for ^");
        bigint b = safe_pop_bigint(stk);
        bigint a = safe_pop_bigint(stk);
        stk.push(a.to_pow(b));
      } else if (token.value == "!") {
        if (stk.empty())
          throw std::runtime_error("Insufficient operands for !");
        bigint a = safe_pop_bigint(stk);
        stk.push(factorial(a));
      } else if (token.value == "~") {
        if (stk.empty())
          throw std::runtime_error("Insufficient operands for unary -");
        bigint a = safe_pop_bigint(stk);
        stk.push(-a);
      } else {
        // Binary functions
        if (token.value == "C") {
          if (stk.size() < 2)
            throw std::runtime_error("Insufficient operands for C");
          bigint a = safe_pop_bigint(stk);
          bigint b = safe_pop_bigint(stk);
          if (a >= b) {
            stk.push(binomial(a, b));
          } else {
            stk.push(binomial(b, a));
          }
        } else if (token.value == "powmod") {
          if (stk.size() < 3)
            throw std::runtime_error("Insufficient operands for powmod");
          bigint a = safe_pop_bigint(stk);
          bigint pow = safe_pop_bigint(stk);
          bigint mod = safe_pop_bigint(stk);
          stk.push(pow_mod(a, pow, mod));
        } else if (token.value == "lcm") {
          if (stk.size() < 2)
            throw std::runtime_error("Insufficient operands for lcm");
          bigint a = safe_pop_bigint(stk);
          bigint b = safe_pop_bigint(stk);
          stk.push(lcm(a, b));
        } else if (token.value == "gcd") {
          if (stk.size() < 2)
            throw std::runtime_error("Insufficient operands for gcd");
          bigint a = safe_pop_bigint(stk);
          bigint b = safe_pop_bigint(stk);
          stk.push(gcd(a, b));
        } else {
          // Unary functions
          if (stk.empty())
            throw std::runtime_error("Insufficient operands for function " +
                                     token.value);
          bigint a = safe_pop_bigint(stk);
          if (token.value == "sqr")
            stk.push(a * a);
          else if (token.value == "sqrt")
            stk.push(sqrt(a));
          else if (token.value == "abs")
            stk.push(abs(a));
          else if (token.value == "fib")
            stk.push(fibonacci(a));
          else
            throw std::runtime_error("Unknown function " + token.value);
        }
      }
    }
  }

  if (stk.size() != 1)
    throw std::runtime_error("Incorrect expression");
  if (time_testing) {
    time();
  }
  return stk.top();
}

double evaluate_double_expression(const std::string &expression) {
  auto tokens = tokenize(expression);
  auto postfix = to_postfix(tokens);
  return eval_postfix_double(postfix);
}

bigint evaluate_bigint_expression(const std::string &expression) {
  auto tokens = tokenize(expression);
  auto postfix = to_postfix(tokens);
  return eval_postfix_bigint(postfix);
}

// Check whether a string has an assigment operator
// Writes name to var_name and expression to var_expression
bool assign(const std::string &expression, std::string &var_name,
            std::string &var_expression) {
  size_t eq_pos = expression.find('=');
  if (eq_pos == std::string::npos || eq_pos == 0 ||
      eq_pos == expression.length() - 1)
    return false;

  var_name = expression.substr(0, eq_pos);
  var_expression = expression.substr(eq_pos + 1);

  // Remove spaces
  var_name.erase(std::remove_if(var_name.begin(), var_name.end(), ::isspace),
                 var_name.end());

  // Check that variable name is correct
  if (var_name.empty() || !std::isalpha(var_name[0]))
    return false;

  for (char c : var_name) {
    if (!std::isalnum(c) && c != '_')
      return false;
  }

  return true;
}

// Assign variable (double)
void assign_variable_double(const std::string &name, double value) {
  double_variables[name] = value;
  // Remove from bigint if it was there
  bigint_variables.erase(name);
}

// Assign variable (bigint)
void assign_variable_bigint(const std::string &name, const bigint &value) {
  bigint_variables[name] = value;
  // Remove from double if it was there
  double_variables.erase(name);
}

// Clear all variables
void clear_variables() {
  double_variables.clear();
  bigint_variables.clear();
}

// Evaluates a string, recognizes colon [mode:] notation, adds brackets
EvalVersion analyze_and_fix(std::string &input) {
  bool colon_command = false;
  char colon_char;

  if (input.length() == 2) {
    if (input == "d:") {
      return COMMAND;
    }
    if (input == "i:") {
      return COMMAND;
    }
  }

  if (input.length() > 2 && input[1] == ':') {
    colon_char = input[0];
    input.erase(0, 2);
    colon_command = true;
  }

  bool has_double_triggers = false;
  bool has_int_triggers = false;
  int bracket_index = 0;

  for (size_t i = 0; i < input.size(); i++) {
    if (isdigit(input[i])) {
      has_int_triggers = true;
    } else if (input[i] == '+' || input[i] == '*' || input[i] == '-' ||
               input[i] == '/' || input[i] == '^' || input[i] == '!') {
      has_int_triggers = true;
    } else if (input[i] == '.') {
      has_double_triggers = true;
      break;
    } else if (input[i] == '(') {
      bracket_index++;
      has_int_triggers = true;
    } else if (input[i] == ')') {
      bracket_index--;
      has_int_triggers = true;
    }
  }

  // Try to fix brackets by adding from left or right
  if (bracket_index > 0) {
    while (bracket_index) {
      input += ')';
      bracket_index--;
    }
  } else if (bracket_index < 0) {
    bracket_index = abs(bracket_index);
    std::string temp;
    while (bracket_index) {
      temp += '(';
      bracket_index--;
    }
    input = temp + input;
  }

  // Return colon_mode command
  if (colon_command) {
    switch (colon_char) {
    case 'd':
      return DOUBLE;
    case 'c':
      return COMMAND;
    case 'i':
      return BIGINT;
    case 'l':
      return LOGIC;
    case 'b':
      return BOOLEAN;
    default:
      return ERROR;
    }
  }

  if (has_double_triggers) {
    return DOUBLE;
  }
  if (has_int_triggers) {
    return BIGINT;
  }
  return COMMAND;
}

// Display help message
void show_help(bool double_eval, bool bigint_approximate, bool loud) {
  system("cls");
  std::cout << "[*] BASIC COMMANDS:\n";
  std::cout << "  help         - Show this help message\n";
  std::cout << "  quit/q/exit  - Exit calculator\n";
  std::cout << "  clear/c      - Clear screen\n";
  std::cout << "  vars/v       - Show all variables\n";
  std::cout << "[>] MODE SETTINGS:\n";
  std::cout << "  deg          - Switch to degrees mode\n";
  std::cout << "  rad          - Switch to radians mode\n";
  std::cout << "  double/d     - Force double precision mode\n";
  std::cout << "  int/bigint/i - Force integer mode\n";
  std::cout << "  exact        - Exact bigint output\n";
  std::cout << "  aprox        - Approximate bigint output\n";
  std::cout << "[=] EXPRESSIONS:\n";
  std::cout << "  Basic: 2+3*4, sin(pi/2), sqrt(16), 5!\n";
  std::cout << "  Variables: x = 5, y = x*2, ans (previous result)\n";
  std::cout << "  Mode prefix: d:3.14*2, i:100!\n";
  std::cout << "[f] FUNCTIONS:\n";
  std::cout << "  Trig: sin, cos, tan, arcsin, arccos, arctan\n";
  std::cout << "  Log: ln, lg, log(base,arg)\n";
  std::cout << "  Other: sqrt, abs, factorial(!), gcd, lcm\n";
  std::cout << "[i] CURRENT STATUS:\n";
  std::cout << "  Trig mode: " << (trig_mode_degrees ? "Degrees" : "Radians")
            << "\n";
  std::cout << "  Eval mode: " << (double_eval ? "Double" : "Auto-detect")
            << "\n";
  std::cout << "  BigInt output: "
            << (bigint_approximate ? "Approximate" : "Exact") << "\n";
  std::cout << "  Verbose mode: " << (loud ? "Loud" : "Quiet") << "\n\n";
}

void show_status(bool double_eval, bool bigint_approximate, bool loud) {
  std::cout << "[i] CURRENT STATUS:\n";
  std::cout << "  Trig mode: " << (trig_mode_degrees ? "Degrees" : "Radians")
            << "\n";
  std::cout << "  Eval mode: " << (double_eval ? "Double" : "Auto-detect")
            << "\n";
  std::cout << "  BigInt output: "
            << (bigint_approximate ? "Approximate" : "Exact") << "\n";
  std::cout << "  Verbose mode: " << (loud ? "Loud" : "Quiet") << "\n\n";
}

void show_welcome() {
  std::cout << "=== Expression Evaluator - Calculator Mode ===\n";
  std::cout << "Ready to evaluate mathematical expressions with BigInt and "
               "Double precision support.\n";
  std::cout << "Type 'help' for commands, 'exit' to quit. Examples: 2+3*4, "
               "x=5, sin(pi/2), 100!\n\n";
}

enum CommandType {
  CMD_CLEAR_VARS,
  CMD_STATUS,
  CMD_RESET,
  CMD_HELP,
  CMD_DOUBLE,
  CMD_INT,
  CMD_DEG,
  CMD_RAD,
  CMD_LOUD,
  CMD_QUIET,
  CMD_APROX,
  CMD_EXACT,
  CMD_CLEAR,
  CMD_VARS
};

// Enter calculator mode. Simple API for external use
void calculator_mode() {
  show_welcome();
  bool eval_state = true;
  bool double_eval = false;
  bool bigint_approximate = false;
  bool loud = true;
  std::string text_input;
  EvalVersion eval_version = COMMAND;

  std::map<std::string, CommandType> command_map = {
      {"clear vars", CMD_CLEAR_VARS},
      {"cvar", CMD_CLEAR_VARS},
      {"s", CMD_STATUS},
      {"status", CMD_STATUS},
      {"reset", CMD_RESET},
      {"help", CMD_HELP},
      {"double", CMD_DOUBLE},
      {"d", CMD_DOUBLE},
      {"d:", CMD_DOUBLE},
      {"int", CMD_INT},
      {"bigint", CMD_INT},
      {"i:", CMD_INT},
      {"i", CMD_INT},
      {"deg", CMD_DEG},
      {"rad", CMD_RAD},
      {"loud", CMD_LOUD},
      {"verbose", CMD_LOUD},
      {"quiet", CMD_QUIET},
      {"silent", CMD_QUIET},
      {"aprox", CMD_APROX},
      {"approximate", CMD_APROX},
      {"exact", CMD_EXACT},
      {"clear", CMD_CLEAR},
      {"c", CMD_CLEAR},
      {"vars", CMD_VARS},
      {"variables", CMD_VARS},
      {"var", CMD_VARS},
      {"v", CMD_VARS}};

  while (eval_state) {
    std::getline(std::cin, text_input);
    if (text_input.size() == 0 || text_input == " ") {
      continue;
    }

    if (text_input == "exit" || text_input == "q" || text_input == "quit") {
      system("cls");
      std::cout << "regular mode \n";
      return;
    }

    eval_version = analyze_and_fix(text_input);
    if (eval_version == BIGINT && double_eval == true) {
      eval_version = DOUBLE;
    }

    if (eval_version == COMMAND) {
      auto cmd_it = command_map.find(text_input);
      if (cmd_it != command_map.end()) {
        switch (cmd_it->second) {
        case CMD_CLEAR_VARS:
          clear_variables();
          if (loud)
            std::cout << "Variables cleared\n";
          break;
        case CMD_STATUS:
          show_status(double_eval, bigint_approximate, loud);
          break;
        case CMD_RESET:
          clear_variables();
          bigint_approximate = false;
          double_eval = false;
          trig_mode_degrees = false;
          double_answer = 0;
          bigint_answer = 0;
          loud = true;
          system("cls");
          std::cout << "Settings reset\n";
          break;
        case CMD_HELP:
          show_help(double_eval, bigint_approximate, loud);
          break;
        case CMD_DOUBLE:
          double_eval = true;
          if (loud)
            std::cout << "Double mode enabled\n";
          break;
        case CMD_INT:
          double_eval = false;
          if (loud)
            std::cout << "Integer mode enabled\n";
          break;
        case CMD_DEG:
          trig_mode_degrees = true;
          if (loud)
            std::cout << "Degrees mode enabled\n";
          break;
        case CMD_RAD:
          trig_mode_degrees = false;
          if (loud)
            std::cout << "Radians mode enabled\n";
          break;
        case CMD_LOUD:
          loud = true;
          break;
        case CMD_QUIET:
          loud = false;
          break;
        case CMD_APROX:
          bigint_approximate = true;
          if (loud)
            std::cout << "Approximate output enabled\n";
          break;
        case CMD_EXACT:
          bigint_approximate = false;
          if (loud)
            std::cout << "Exact output enabled\n";
          break;
        case CMD_CLEAR:
          system("cls");
          break;
        case CMD_VARS:
          std::cout << "Variables:\n";
          for (const auto &var : double_variables)
            std::cout << var.first << " = " << var.second << " (float)\n";
          for (const auto &var : bigint_variables)
            std::cout << var.first << " = " << var.second << " (integer)\n";
          if (double_variables.empty() && bigint_variables.empty())
            std::cout << "No variables defined\n";
          break;
        }
        continue;
      }
      std::cout << "Unknown command\n";
    } else if (eval_version == DOUBLE) {
      try {
        std::string var_name, var_expr;
        if (assign(text_input, var_name, var_expr)) {
          // Check for banned names
          bool is_banned = std::find(BANNED_NAMES.begin(), BANNED_NAMES.end(),
                                     var_name) != BANNED_NAMES.end();
          if (is_banned) {
            std::cerr
                << "Error: '" << var_name
                << "' is a reserved name and cannot be used as variable\n";
            continue;
          }

          double result = evaluate_double_expression(var_expr);
          assign_variable_double(var_name, result);
          double_answer = result;
          if (loud)
            std::cout << var_name << " = " << result << "\n";
        } else {
          double_answer = evaluate_double_expression(text_input);
          std::cout << double_answer << "\n";
        }
      } catch (const std::exception &e) {
        std::cerr << "Error: " << e.what() << std::endl;
      }
    }
  elsehttps: // github.com/balloffur/expression_evaluator
  {
    try {
      std::string var_name, var_expr;
      if (assign(text_input, var_name, var_expr)) {
        // Check for banned names
        bool is_banned = std::find(BANNED_NAMES.begin(), BANNED_NAMES.end(),
                                   var_name) != BANNED_NAMES.end();
        if (is_banned) {
          std::cerr << "Error: '" << var_name
                    << "' is a reserved name and cannot be used as variable\n";
          continue;
        }

        bigint result = evaluate_bigint_expression(var_expr);
        assign_variable_bigint(var_name, result);
        bigint_answer = result;
        if (bigint_approximate) {
          if (loud)
            std::cout << var_name << " ~ " << aproximation_print(result)
                      << "\n";
        } else {
          if (loud)
            std::cout << var_name << " = " << result << "\n";
        }
      } else {
        if (bigint_approximate) {
          bigint_answer = evaluate_bigint_expression(text_input);
          std::cout << aproximation_print(bigint_answer) << "\n";
        } else {
          bigint_answer = evaluate_bigint_expression(text_input);
          std::cout << bigint_answer << "\n";
        }
      }
    } catch (const std::exception &e) {
      std::cerr << "Error: " << e.what() << std::endl;
    }
  }
  }
}
} 