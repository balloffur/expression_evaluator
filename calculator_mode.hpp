#pragma once
#include "evaluate.hpp"
namespace evl{
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
    input.append(bracket_index, ')');
  } else if (bracket_index < 0) {
    input.insert(0, -bracket_index, '(');
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



// Enter calculator mode. Terminal 
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
        double result = eval_do(text_input);
        std::string var_name, var_expr;
        if (assign(text_input, var_name, var_expr)) {
          if (loud)
            std::cout << var_name << " = " << result << "\n";
        } else {
          std::cout << result << "\n";
        }
      } catch (const std::exception &e) {
        std::cerr << "Error: " << e.what() << std::endl;
      }
    }
  else
  {
    try {
      bigint result = eval_bi(text_input);
      std::string var_name, var_expr;
      if (assign(text_input, var_name, var_expr)) {
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
          std::cout << aproximation_print(result) << "\n";
        } else {
          std::cout << result << "\n";
        }
      }
    } catch (const std::exception &e) {
      std::cerr << "Error: " << e.what() << std::endl;
    }
  }
  }
}
}
