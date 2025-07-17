#pragma once
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

    if (isspace(ch))
      continue;
    
    if (ch == ',') {
      tokens.push_back({OPERATOR, ","});
      continue;
    }

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
      if (tokens.empty() || 
          (tokens.back().type == OPERATOR && tokens.back().value != "!") ||
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
  if (op == "!")
    return 6;
  if (op == "~")
    return 5;
  if (op == "^")
    return 4;
  if (op == "*" || op == "/" || op == "%")
    return 3;
  if (op == "+" || op == "-")
    return 2;
  if (op == ",")
    return 1;
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
      if (token.value == "!") {
        // Factorial is postfix unary operator - add directly to output
        output.push_back(token);
      } else {
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
      }
    } else if (token.value == "(") {
      ops.push(token);
    } else if (token.value == ")") {
      while (!ops.empty() && ops.top().value != "(") {
        output.push_back(ops.top());
        ops.pop();
      }
      if (!ops.empty())
        ops.pop(); // remove '('
      while (!ops.empty() && ops.top().type == FUNCTION) {
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
      } else if (token.value == ",") {
        // Comma is just a separator, do nothing
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
      } else if (token.value == ",") {
        // Comma is just a separator, do nothing
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
          bigint mod = safe_pop_bigint(stk);
          bigint pow = safe_pop_bigint(stk);
          bigint a = safe_pop_bigint(stk);
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

//evaluate float expressions
double eval_do(const std::string &expression) {
  std::string var_name, var_expr;
  if (assign(expression, var_name, var_expr)) {
    // Check for banned names
    bool is_banned = std::find(BANNED_NAMES.begin(), BANNED_NAMES.end(),
                               var_name) != BANNED_NAMES.end();
    if (is_banned) {
      throw std::runtime_error("Error: '" + var_name + "' is a reserved name and cannot be used as variable");
    }

    auto tokens = tokenize(var_expr);
    auto postfix = to_postfix(tokens);
    double result = eval_postfix_double(postfix);
    assign_variable_double(var_name, result);
    double_answer = result;
    return result;
  } else {
    auto tokens = tokenize(expression);
    auto postfix = to_postfix(tokens);
    double result = eval_postfix_double(postfix);
    double_answer = result;
    return result;
  }
}

//evaluate bigint/int expressions
bigint eval_bi(const std::string &expression) {
  std::string var_name, var_expr;
  if (assign(expression, var_name, var_expr)) {
    // Check for banned names
    bool is_banned = std::find(BANNED_NAMES.begin(), BANNED_NAMES.end(),
                               var_name) != BANNED_NAMES.end();
    if (is_banned) {
      throw std::runtime_error("Error: '" + var_name + "' is a reserved name and cannot be used as variable");
    }

    auto tokens = tokenize(var_expr);
    auto postfix = to_postfix(tokens);
    bigint result = eval_postfix_bigint(postfix);
    assign_variable_bigint(var_name, result);
    bigint_answer = result;
    return result;
  } else {
    auto tokens = tokenize(expression);
    auto postfix = to_postfix(tokens);
    bigint result = eval_postfix_bigint(postfix);
    bigint_answer = result;
    return result;
  }
}

}