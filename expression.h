#pragma once
#include <iostream>
#include <stack>
#include <string>
#include <vector>
#include <sstream>
#include <map>
#include <cmath>
#include "bigint.h"
#include "bigint_extra.h"

// Структура для хранения токенов
struct Token {
    std::string value;
    bool isNumber;
};

// Преобразование выражения в обратную польскую нотацию
std::vector<Token> toRPN(const std::string& expression) {
    std::map<char, int> precedence = {
        {'+', 1}, {'-', 1},
        {'*', 2}, {'/', 2}, {'%', 2},
        {'^', 3}
    };
    
    std::stack<char> operators;
    std::vector<Token> output;
    std::stringstream ss;
    
    for (size_t i = 0; i < expression.length(); ++i) {
        char ch = expression[i];
        
        if (isdigit(ch)) {
            ss << ch;
            if (i + 1 == expression.length() || !isdigit(expression[i + 1])) {
                output.push_back({ss.str(), true});
                ss.str("");
            }
        }
        else if (ch == '(') {
            operators.push(ch);
        }
        else if (ch == ')') {
            while (!operators.empty() && operators.top() != '(') {
                output.push_back({std::string(1, operators.top()), false});
                operators.pop();
            }
            operators.pop();
        }
        else {
            while (!operators.empty() && operators.top() != '(' &&
                   precedence[ch] <= precedence[operators.top()]) {
                output.push_back({std::string(1, operators.top()), false});
                operators.pop();
            }
            operators.push(ch);
        }
    }
    
    while (!operators.empty()) {
        output.push_back({std::string(1, operators.top()), false});
        operators.pop();
    }
    
    return output;
}

// Вычисление значения выражения
bigint evaluateRPN(const std::vector<Token>& rpn) {
    std::stack<bigint> stack;
    
    for (const auto& token : rpn) {
        if (token.isNumber) {
            bigint a;
            a.read(token.value);
            stack.push(a);
        }
        else {
            if (stack.size() < 2) {
                throw std::runtime_error("Incorrect expression");
            }
            
            bigint b = stack.top(); stack.pop();
            bigint a = stack.top(); stack.pop();
            
            switch(token.value[0]) {
                case '+': stack.push(a + b); break;
                case '-': stack.push(a - b); break;
                case '*': stack.push(a * b); break;
                case '/': 
                    if (b == 0) throw std::runtime_error("Division by 0");
                    stack.push(a / b); 
                    break;
                case '^': 
                    if (b < 0) throw std::runtime_error("Negative power");
                    stack.push(a.to_pow(b)); 
                    break;
                case '%': 
                    if (b == 0) throw std::runtime_error("Divisions by 0");
                    stack.push(a % b); 
                    break;
                default:
                    throw std::runtime_error("Unknown operator. Supported operations: + - * / % ^");
            }
        }
    }
    
    return stack.top();
}
