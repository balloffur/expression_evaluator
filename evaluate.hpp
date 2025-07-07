#include <iostream>
#include <sstream>
#include <stack>
#include <cmath>
#include <map>
#include <cctype>
#include <stdexcept>
#include <vector>
#include "bigint.h"
#include "bigint_extra.h"
#include "timing.hpp"

namespace evl
{
    enum EvalVersion
    {
        BIGINT,
        DOUBLE,
        LOGIC,
        BOOLEAN,
        COMMAND,
        PHYSICS,
        ERROR
    };
    enum TokenType
    {   
        NUMBER,
        OPERATOR,
        FUNCTION,
        PARENTHESIS,
        PREV
    };
    // Можно добавить константы
    std::map<std::string, std::string> constants_math = {{"pi", "3.141592653589793"}, {"e", "2.718281828459045"}, {"phi", "1.618033988749895"}};
    std::map<std::string, std::string> physic_constants = {{"с", " 2.99792458e8"}};
    static bool TIME_TESTING = false;
    static bool TRIG_MODE_DEG = false;
    static double DOUBLE_ANS=0;
    bigint BIGINT_ANS=0;
    const double DEG_TO_RAD = 0.01745329251994329576923690768488612713412398;

    struct Token
    {
        TokenType type;
        std::string value;
    };

    // Токенайзер
    std::vector<Token> tokenize(const std::string &expr)
    {
        if (TIME_TESTING)
        {
            time();
        }
        std::vector<Token> tokens;
        std::string num;
        for (size_t i = 0; i < expr.size(); ++i)
        {
            char ch = expr[i];

            if (isspace(ch) || ch == ',')
                continue;

            if (isdigit(ch) || ch == '.')
            {
                bool has_dot = (ch == '.');
                num += ch;
                while (i + 1 < expr.size() && (isdigit(expr[i + 1]) || expr[i + 1] == '.'))
                {
                    if (expr[i + 1] == '.')
                    {
                        if (has_dot)
                        {
                            throw std::runtime_error("Double .");
                        }
                        else
                        {
                            has_dot = true;
                        }
                    }
                    num += expr[++i];
                }
                tokens.push_back({NUMBER, num});
                num.clear();
            }
            else if (isalpha(ch))
            {
                std::string func;
                func += ch;
                while (i + 1 < expr.size() && isalpha(expr[i + 1]))
                    func += expr[++i];
                if(func=="ans"){
                    tokens.push_back({PREV,""});
                } else 
                if (constants_math.find(func) != constants_math.end())
                {
                    tokens.push_back({NUMBER, constants_math[func]});
                }
                else
                {
                    tokens.push_back({FUNCTION, func});
                }
            }
            // Отдельна обработка минуса для определения унарного
            else if (ch == '-')
            {
                if (tokens.empty() ||
                    tokens.back().type == OPERATOR ||
                    (tokens.back().type == PARENTHESIS && tokens.back().value == "("))
                {
                    tokens.push_back({OPERATOR, "~"}); // унарный минус
                }
                else
                {
                    tokens.push_back({OPERATOR, "-"});
                }
            }
            else if (ch == '+' || ch == '*' || ch == '/' || ch == '%' || ch == '^' || ch == '!')
            {
                tokens.push_back({OPERATOR, std::string(1, ch)});
            }
            // Скобки
            else if (ch == '(' || ch == ')')
            {
                tokens.push_back({PARENTHESIS, std::string(1, ch)});
            }
            else
            {
                throw std::runtime_error("Unknown character in expression");
            }
        }

        return tokens;
    }

    int precedence(const std::string &op)
    {
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

    bool isRightAssociative(const std::string &op)
    {
        return op == "^" || op == "!" || op == "~";
    }

    std::vector<Token> toPostfix(const std::vector<Token> &tokens)
    {
        std::vector<Token> output;
        std::stack<Token> ops;

        for (const auto &token : tokens)
        {
            if (token.type == NUMBER)
            {
                output.push_back(token);
            }
            else if (token.type == FUNCTION)
            {
                ops.push(token);
            }
            else if (token.type == OPERATOR)
            {
                while (!ops.empty() && ((ops.top().type == FUNCTION) ||
                                        (ops.top().type == OPERATOR &&
                                         (precedence(ops.top().value) > precedence(token.value) ||
                                          (precedence(ops.top().value) == precedence(token.value) &&
                                           !isRightAssociative(token.value))))))
                {
                    output.push_back(ops.top());
                    ops.pop();
                }
                ops.push(token);
            }
            else if (token.value == "(")
            {
                ops.push(token);
            }
            else if (token.value == ")")
            {
                while (!ops.empty() && ops.top().value != "(")
                {
                    output.push_back(ops.top());
                    ops.pop();
                }
                if (!ops.empty())
                    ops.pop(); // remove '('
                if (!ops.empty() && ops.top().type == FUNCTION)
                {
                    output.push_back(ops.top());
                    ops.pop();
                }
            }
        }

        while (!ops.empty())
        {
            output.push_back(ops.top());
            ops.pop();
        }

        return output;
    }

    double evalPostfix_double(const std::vector<Token> &postfix)
    {
        std::stack<double> stk;

        for (const auto &token : postfix)
        {
            if (token.type == NUMBER)
            {
                stk.push(stod(token.value));
            }
            else if (token.type == OPERATOR || token.type == FUNCTION)
            {
                if (token.value == "+")
                {
                    double b = stk.top();
                    stk.pop();
                    double a = stk.top();
                    stk.pop();
                    stk.push(a + b);
                }
                else if (token.value == "-")
                {
                    double b = stk.top();
                    stk.pop();
                    double a = stk.top();
                    stk.pop();
                    stk.push(a - b);
                }
                else if (token.value == "*")
                {
                    double b = stk.top();
                    stk.pop();
                    double a = stk.top();
                    stk.pop();
                    stk.push(a * b);
                }
                else if (token.value == "/")
                {
                    double b = stk.top();
                    stk.pop();
                    double a = stk.top();
                    stk.pop();
                    if (b == 0)
                        throw std::runtime_error("Divsion by zero");
                    stk.push(a / b);
                }
                else if (token.value == "%")
                {
                    double b = stk.top();
                    stk.pop();
                    double a = stk.top();
                    stk.pop();
                    stk.push(fmod(a, b));
                }
                else if (token.value == "^")
                {
                    double b = stk.top();
                    stk.pop();
                    double a = stk.top();
                    stk.pop();
                    stk.push(pow(a, b));
                }
                else if (token.value == "~")
                {
                    double a = stk.top();
                    stk.pop();
                    stk.push(-a);
                }
                else
                {
                    // бинарные функции
                    if (token.value == "log")
                    {
                        double arg = stk.top();
                        stk.pop();
                        double base = stk.top();
                        stk.pop();
                        if (arg <= 0 || base <= 0 || base == 1)
                            throw std::runtime_error("Logarithm error");
                        stk.push(log(arg) / log(base));
                    }
                    else
                    {
                        // унарные функции
                        double a = stk.top();
                        stk.pop();

                        if (token.value == "sin")
                        {
                            if (TRIG_MODE_DEG)
                            {
                                stk.push(sin(a * DEG_TO_RAD));
                            }
                            else
                            {
                                stk.push(sin(a));
                            }
                        }
                        else if (token.value == "cos")
                        {
                            if (TRIG_MODE_DEG)
                            {
                                stk.push(cos(a * DEG_TO_RAD));
                            }
                            else
                            {
                                stk.push(cos(a));
                            }
                        }
                        else if (token.value == "tan" || token.value == "tg")
                        {
                            stk.push(tan(a));
                        }
                        else if (token.value == "ctg")
                        {
                            stk.push(1.0 / tan(a));
                        }
                        else if (token.value == "arcsin")
                        {
                            if (a < -1 || a > 1)
                                throw std::runtime_error("arcsin out of domain");
                            stk.push(asin(a));
                        }
                        else if (token.value == "arccos")
                        {
                            if (a < -1 || a > 1)
                                throw std::runtime_error("arccos out of domain");
                            stk.push(acos(a));
                        }
                        else if (token.value == "arctan" || token.value == "arctg")
                        {
                            stk.push(atan(a));
                        }
                        else if (token.value == "arcctg" || token.value == "arccot")
                        {
                            if (a == 0)
                                throw std::runtime_error("arcctg undefined for zero");
                            stk.push(atan(1.0 / a));
                        }
                        else if (token.value == "ln")
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
        if (TIME_TESTING)
        {
            time();
        }
        return stk.top();
    }

    bigint evalPostfix_bigint(const std::vector<Token> &postfix)
    {
        std::stack<bigint> stk;

        for (const auto &token : postfix)
        {
            if (token.type == NUMBER)
            {
                stk.push(bigint(token.value));
            }
            else if (token.type == OPERATOR || token.type == FUNCTION)
            {
                if (token.value == "+")
                {
                    bigint b = stk.top();
                    stk.pop();
                    bigint a = stk.top();
                    stk.pop();
                    stk.push(a + b);
                }
                else if (token.value == "-")
                {
                    bigint b = stk.top();
                    stk.pop();
                    bigint a = stk.top();
                    stk.pop();
                    stk.push(a - b);
                }
                else if (token.value == "*")
                {
                    bigint b = stk.top();
                    stk.pop();
                    bigint a = stk.top();
                    stk.pop();
                    stk.push(a * b);
                }
                else if (token.value == "/")
                {
                    bigint b = stk.top();
                    stk.pop();
                    bigint a = stk.top();
                    stk.pop();
                    if (b.isZero())
                        throw std::runtime_error("Division by zero");
                    stk.push(a / b);
                }
                else if (token.value == "%")
                {
                    bigint b = stk.top();
                    stk.pop();
                    bigint a = stk.top();
                    stk.pop();
                    stk.push(a % b);
                }
                else if (token.value == "^")
                {
                    bigint b = stk.top();
                    stk.pop();
                    bigint a = stk.top();
                    stk.pop();
                    stk.push(a.to_pow(b));
                }
                else if (token.value == "!")
                {
                    bigint a = stk.top();
                    stk.pop();
                    stk.push(factorial(a));
                }
                else if (token.value == "~")
                {
                    bigint a = stk.top();
                    stk.pop();
                    stk.push(-a);
                }
                else
                {
                    // бинарные функции
                    if (token.value == "C")
                    {
                        bigint a = stk.top();
                        stk.pop();
                        bigint b = stk.top();
                        stk.pop();
                        if (a >= b)
                        {
                            stk.push(binomial(a, b));
                        }
                        else
                        {
                            stk.push(binomial(b, a));
                        }
                    }
                    else if (token.value == "powmod")
                    {
                        bigint a = stk.top();
                        stk.pop();
                        bigint pow = stk.top();
                        stk.pop();
                        bigint mod = stk.top();
                        stk.pop();
                        stk.push(pow_mod(a, pow, mod));
                    }
                    else if (token.value == "lcm")
                    {
                        bigint a = stk.top();
                        stk.pop();
                        bigint b = stk.top();
                        stk.pop();
                        stk.push(lcm(a, b));
                    }
                    else if (token.value == "gcd")
                    {
                        bigint a = stk.top();
                        stk.pop();
                        bigint b = stk.top();
                        stk.pop();
                        stk.push(gcd(a, b));
                    }
                    else
                    {
                        // унарные функции
                        bigint a = stk.top();
                        stk.pop();
                        if (token.value == "sqr")
                            stk.push(a * a);
                        else if (token.value == "sqrt")
                            stk.push(sqrt(a));
                        else if (token.value == "abs")
                            stk.push(abs(a));
                        else if (token.value == "fib")
                            stk.push(fibonacci(a));
                        else
                            throw std::runtime_error("Unknowm function " + token.value);
                    }
                }
            }
        }

        if (stk.size() != 1)
            throw std::runtime_error("Incorrect expression");
        if (TIME_TESTING)
        {
            time();
        }
        return stk.top();
    }

    double evaluate_double_expression(const std::string &expression)
    {
        auto tokens = tokenize(expression);
        auto postfix = toPostfix(tokens);
        return evalPostfix_double(postfix);
    }

    bigint evaluate_bigint_expression(const std::string &expression)
    {
        auto tokens = tokenize(expression);
        auto postfix = toPostfix(tokens);
        return evalPostfix_bigint(postfix);
    }

    // Evaluates a string, recognises colon [mode:] notation, adds brackets
    // Temporary function. Should move this to tokenizer and add support of double functions recognition
    EvalVersion Analize_fix(std::string &input)
    {
        bool colon_command = false;
        char colon_char;
        if (input.length() == 2)
        {
            if (input == "d:")
            {
                return COMMAND;
            }
            if (input == "i:")
            {
                return COMMAND;
            }
        }
        if (input.length() > 2 && input[1] == ':')
        {
            colon_char = input[0];
            input.erase(0, 2);
            colon_command = true;
        }
        bool has_double_triggers = false;
        bool has_int_triggers = false;
        int bracket_index = 0;
        for (int i = 0; i < input.size(); i++)
        {
            if (isdigit(input[i]))
            {
                has_int_triggers = true;
            }
            else if (input[i] == '+' || input[i] == '*' || input[i] == '-' || input[i] == '/' || input[i] == '^' || input[i] == '!')
            {
                has_int_triggers = true;
            }
            else if (input[i] == '.')
            {
                has_double_triggers = true;
                break;
            }
            else if (input[i] == '(')
            {
                bracket_index++;
                has_int_triggers = true;
            }
            else if (input[i] == ')')
            {
                bracket_index--;
                has_int_triggers = true;
            }
        }
        //trying to fix brackets just by adding from left or right
        if (bracket_index > 0)
        {
            while (bracket_index)
            {
                input += ')';
                bracket_index--;
            }
        }
        else if (bracket_index < 0)
        {
            bracket_index = abs(bracket_index);
            std::string temp;
            while (bracket_index)
            {
                temp += '(';
                bracket_index--;
            }
            input = temp + input;
        }
        //returning colon_mode command
        if (colon_command)
        {
            switch (colon_char)
            {
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
        if (has_double_triggers)
        {
            return DOUBLE;
        }
        if (has_int_triggers)
        {
            return BIGINT;
        }
        return COMMAND;
    }

    /// enters calculator mode. Simple API for external use
    void calculator_mode()
    {
        std::cout << "Evaluation mode. To change trigonometric functions mode use [rad] and [deg]\n\n";
        bool eval_state = true;
        bool double_eval = false;
        bool bigint_aproximate = false;
        std::string text_input;
        EvalVersion eval_version = COMMAND;
        while (eval_state)
        {
            std::getline(std::cin, text_input);
            if (text_input.size() == 0 || text_input == " ")
            {
                continue;
            };
            if (text_input == "exit" || text_input == "q" || text_input == "quit")
            {
                system("cls");
                std::cout << "regular mode \n";
                return;
                eval_state = false;
            }
            eval_version = Analize_fix(text_input);
            if (eval_version == BIGINT && double_eval == true)
            {
                eval_version = DOUBLE;
            }
            if (eval_version == COMMAND)
            {
                if (text_input == "help")
                {
                    std::cout << "help -- help \ndeg/rad -- switch trig mode \nclear/c -- clear screen \n";
                    std::cout << "quit -- quit\n";
                    std::cout << "trig -- show trig mode\n";
                    std::cout << "aprox/exact -- presentation mode for bigint calculations\n";
                    std::cout << "You can enter calculations as is \nIf you want to specify, you can use [d:] for double and [i:] for integer\n\n";
                    std::cout << "Trigonometry mode : " << (TRIG_MODE_DEG ? "degrees" : "radian") << "\n";
                    continue;
                }
                if (text_input == "double" || text_input == "d" || text_input == "d:")
                {
                    double_eval = true;
                    continue;
                }
                if (text_input == "int" || text_input == "bigint" || text_input == "i:" || text_input == "i")
                {
                    double_eval = false;
                    continue;
                }
                if (text_input == "deg")
                {
                    evl::TRIG_MODE_DEG = true;
                    continue;
                }
                if (text_input == "rad")
                {
                    evl::TRIG_MODE_DEG = false;
                    continue;
                }
                if (text_input == "aprox" || text_input == "aproximate")
                {
                    bigint_aproximate = true;
                    continue;
                }
                if (text_input == "exact")
                {
                    bigint_aproximate = false;
                    continue;
                }
                if (text_input == "clear" || text_input == "c")
                {
                    system("cls");
                    if (TRIG_MODE_DEG)
                    {
                        std::cout << "Trigonometry mode: degrees\n";
                    }
                    if (double_eval)
                    {
                        std::cout << "Evaluating as double\n";
                    }
                    if (bigint_aproximate)
                    {
                        std::cout << "Aproximating bigint output\n";
                    }
                    continue;
                }
                if (text_input == "quit" || text_input == "q")
                {
                    eval_state = false;
                }
                std::cout << "Unknown command \n";
            }
            else if (eval_version == DOUBLE)
            {
                try
                {   
                    DOUBLE_ANS=evl::evaluate_double_expression(text_input);
                    std::cout << DOUBLE_ANS << "\n";
                }
                catch (const std::exception &e)
                {
                    std::cerr << "Error: " << e.what() << std::endl;
                }
            }
            else
            {
                try
                {
                    if (bigint_aproximate == true)
                    {
                        BIGINT_ANS=evl::evaluate_bigint_expression(text_input);
                        std::cout << aproximation_print(BIGINT_ANS) << "\n";
                    }
                    else
                    {
                        BIGINT_ANS=evl::evaluate_bigint_expression(text_input);
                        std::cout << BIGINT_ANS << "\n";
                    }
                }
                catch (const std::exception &e)
                {
                    std::cerr << "Error: " << e.what() << std::endl;
                }
            }
        }
        return;
    }

}