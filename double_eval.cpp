#include <iostream>
#include <sstream>
#include <stack>
#include <cmath>
#include <map>
#include <cctype>
#include <stdexcept>
#include <vector>


enum TokenType {NUMBER, OPERATOR, FUNCTION, PARENTHESIS};
std::map<std::string,std::string> constants={{"pi","3.141592653589793"},{"e","2.718281828459045"},{"phi","1.618033988749895"}};

struct Token {
    TokenType type;
    std::string value;
};

std::vector<Token> tokenize(const std::string& expr) {
    std::vector<Token> tokens;
    std::string num;

    for (size_t i = 0; i < expr.size(); ++i) {
        char ch = expr[i];

        if (isspace(ch)) continue;

        if (isdigit(ch) || ch == '.') {
            bool has_dot=(ch=='.');
            num += ch;
            while (i + 1 < expr.size() && (isdigit(expr[i + 1]) || expr[i + 1] == '.'))
                num += expr[++i];
            tokens.push_back({NUMBER, num});
            num.clear();
        }
        else if (isalpha(ch)) {
            std::string func;
            func += ch;
            while (i + 1 < expr.size() && isalpha(expr[i + 1]))
                func += expr[++i];
            if(constants.find(func)!=constants.end()){
                tokens.push_back({NUMBER, constants[func]});
            } else {
            tokens.push_back({FUNCTION, func});
            }
        }
        //Отдельна обработка минуса для определения унарного
        else if (ch == '-') {
            if (tokens.empty() || 
                tokens.back().type == OPERATOR || 
                (tokens.back().type == PARENTHESIS && tokens.back().value == "(")) {
                tokens.push_back({OPERATOR, "~"}); // унарный минус
            } else {
                tokens.push_back({OPERATOR, "-"});
            }
        }
        else if (ch == '+' || ch == '*' || ch == '/' || ch == '%' || ch == '^' || ch == '!') {
            tokens.push_back({OPERATOR, std::string(1, ch)});
        }
        //Скобки
        else if (ch == '(' || ch == ')') {
            tokens.push_back({PARENTHESIS, std::string(1, ch)});
        }
        else {
            throw std::runtime_error("Unknown character in expression");
        }
    }

    return tokens;
}

int precedence(const std::string& op) {
    if (op == "!" || op == "~") return 5;
    if (op == "^") return 4;
    if (op == "*" || op == "/" || op == "%") return 3;
    if (op == "+" || op == "-") return 2;
    return 0;
}

bool isRightAssociative(const std::string& op) {
    return op == "^" || op == "!" || op == "~";
}

std::vector<Token> toPostfix(const std::vector<Token>& tokens) {
    std::vector<Token> output;
    std::stack<Token> ops;

    for (const auto& token : tokens) {
        if (token.type == NUMBER) {
            output.push_back(token);
        }
        else if (token.type == FUNCTION) {
            ops.push(token);
        }
        else if (token.type == OPERATOR) {
            while (!ops.empty() && (
                (ops.top().type == FUNCTION) ||
                (ops.top().type == OPERATOR &&
                 (precedence(ops.top().value) > precedence(token.value) ||
                  (precedence(ops.top().value) == precedence(token.value) &&
                   !isRightAssociative(token.value)))
                )
            )) {
                output.push_back(ops.top());
                ops.pop();
            }
            ops.push(token);
        }
        else if (token.value == "(") {
            ops.push(token);
        }
        else if (token.value == ")") {
            while (!ops.empty() && ops.top().value != "(") {
                output.push_back(ops.top());
                ops.pop();
            }
            if (!ops.empty()) ops.pop(); // remove '('
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

double factorial(double val) {
    if (val < 0) throw std::runtime_error("Факториал от отрицательного числа");
    if (val != floor(val)) throw std::runtime_error("Факториал от нецелого числа");
    double result = 1;
    for (int i = 2; i <= (int)val; ++i)
        result *= i;
    return result;
}

double evalPostfix(const std::vector<Token>& postfix) {
    std::stack<double> stk;

    for (const auto& token : postfix) {
        if (token.type == NUMBER) {
            stk.push(stod(token.value));
        }
        else if (token.type == OPERATOR || token.type == FUNCTION) {
            if (token.value == "+") {
                double b = stk.top(); stk.pop();
                double a = stk.top(); stk.pop();
                stk.push(a + b);
            } else if (token.value == "-") {
                double b = stk.top(); stk.pop();
                double a = stk.top(); stk.pop();
                stk.push(a - b);
            } else if (token.value == "*") {
                double b = stk.top(); stk.pop();
                double a = stk.top(); stk.pop();
                stk.push(a * b);
            } else if (token.value == "/") {
                double b = stk.top(); stk.pop();
                double a = stk.top(); stk.pop();
                if (b == 0) throw std::runtime_error("Деление на ноль");
                stk.push(a / b);
            } else if (token.value == "%") {
                double b = stk.top(); stk.pop();
                double a = stk.top(); stk.pop();
                stk.push(fmod(a, b));
            } else if (token.value == "^") {
                double b = stk.top(); stk.pop();
                double a = stk.top(); stk.pop();
                stk.push(pow(a, b));
            } else if (token.value == "!") {
                double a = stk.top(); stk.pop();
                stk.push(factorial(a));
            } else if (token.value == "~") {
                double a = stk.top(); stk.pop();
                stk.push(-a);
            } else {
                    //бинарные функции
                if (token.value == "log") {
                    double arg = stk.top(); stk.pop();
                    double base = stk.top(); stk.pop();
                    if (arg <= 0 || base <= 0 || base == 1)
                        throw std::runtime_error("Недопустимые значения для log(base, arg)");
                    stk.push(log(arg) / log(base));
                } else {
                    //унарные функции
                    double a = stk.top(); stk.pop();

                    if (token.value == "sin") stk.push(sin(a));
                    else if (token.value == "cos") stk.push(cos(a));
                    else if (token.value == "tan" || token.value == "tg") stk.push(tan(a));
                    else if (token.value == "ctg") stk.push(1.0 / tan(a));
                    else if (token.value == "arcsin") {
                        if (a < -1 || a > 1) throw std::runtime_error("arcsin out of domain");
                        stk.push(asin(a));
                    }
                    else if (token.value == "arccos") {
                        if (a < -1 || a > 1) throw std::runtime_error("arccos out of domain");
                        stk.push(acos(a));
                    }
                    else if (token.value == "arctan" || token.value == "arctg") stk.push(atan(a));
                    else if (token.value == "arcctg" || token.value == "arccot") {
                        if (a == 0) throw std::runtime_error("arcctg undefined for zero");
                        stk.push(atan(1.0 / a));
                    }
                    else if (token.value == "ln") stk.push(log(a));
                    else if (token.value == "lg") stk.push(log10(a));
                    else if (token.value == "sqrt") stk.push(sqrt(a));
                    else if (token.value == "abs") stk.push(abs(a));
                    else throw std::runtime_error("Неизвестная функция: " + token.value);
                }
            }
        }
    }

    if (stk.size() != 1) throw std::runtime_error("Некорректное выражение");
    return stk.top();
}

double evaluate(const std::string& expression) {
    auto tokens = tokenize(expression);
    auto postfix = toPostfix(tokens);
    return evalPostfix(postfix);
}



int main() {
    std::string expr;
    std::cout << "Введите выражение: ";
    getline(std::cin, expr);
    try {
        double result = evaluate(expr);
        std::cout << "Результат: " << result << std::endl;
    } catch (const std::exception& ex) {
        std::cerr << "Ошибка: " << ex.what() << std::endl;
    }
    return 0;
}
