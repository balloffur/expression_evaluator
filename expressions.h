#include <vector>
#include <iostream>
#include "bigint.h"
#include <string>
#include <map>
#include <stack>
namespace expressions{
//availabe operations    
enum operations{
    small_number,
    big_number,
    float_number,
    left_bracket,
    right_bracket,
    plus,
    min,
    u_min,
    mul,
    modulo,
    div,
    power,
    factorial,
    sin,
    cos,
    tg,
    ctg,
    arcsin,
    arccos,
    arctg,
    arcctg,
    sinh,
    cosh,
    tanh,
    cotanh,
    log,
    ln,
    lg,
    sqrt,
    root,
    abs,
    ceil,
    floor,
    exponent
};

std::map<operations,int> precedence={{plus,1},{min,2}};

//


enum modes{
bigint_eval,
double_eval,
logic,
binary
};

struct Tokens{
    std::vector<operations> list_of_operations;
    std::vector<int> small_numbers;
    std::vector<bigint> big_numbers;
    std::vector<double> float_numbers;
    bool has_float=false;
    bool has_bigint=false;
    bool correct=true; 
};

Tokens tokenize(const std::string& input){
    Tokens result;
    int lefts=0;
    int rights=0;
    std::string current_number;
    bool no_decimal=true;
    for(int i=0;i<input.length();i++){
        char current_char=input[i];
        if(current_char<='9' && current_char>='0' || current_char=='.' ||current_char==','){
            if(current_char=='.' || current_char==','){
                if(current_number.length()>0 && no_decimal){
                    no_decimal=false;
                    current_number+='.';
                    continue;
                } else {
                    result.correct=false;
                    return result;
                }
            } else {
                current_number+=current_char;
            }
            continue;
        }
        if(current_number.length()>0){
            if(no_decimal){
                if(current_number.length()>9){
                    result.list_of_operations.push_back(big_number);
                    bigint a;
                    a.read(current_number);
                    result.big_numbers.push_back(a);
                } else {
                    result.list_of_operations.push_back(small_number);
                    result.small_numbers.push_back(std::stoi(current_number));
                }
            } else {
                result.list_of_operations.push_back(float_number);
                result.float_numbers.push_back(std::stod(current_number));
                no_decimal=true;
            }
            current_number.clear();
        }
        switch (current_char)
        {
        case ' ':
        break;
        case '+':
            result.list_of_operations.push_back(plus);
            break;
        case '-':
            if(i==0 || input[i-1]=='('){
                result.list_of_operations.push_back(u_min);
            } else {
            result.list_of_operations.push_back(min);}
            break;
        case '*':
            result.list_of_operations.push_back(mul);
            break;
        case '/':
            result.list_of_operations.push_back(div);
            break;
        case '%':
            result.list_of_operations.push_back(modulo);
            break;
        case '!':
            result.list_of_operations.push_back(factorial);
            break;
        case '^':
            result.list_of_operations.push_back(power);
            break; 
        case '(':
            result.list_of_operations.push_back(left_bracket);
            lefts++;
            break;
        case ')':
            result.list_of_operations.push_back(right_bracket);
            rights++;
            break;
        //letter functions
        case 's':
            if(i+2>=input.length() || input[i+1]!='i' || input[i+2]!='n'){
                result.correct=false;
                return result;
            } else {
                result.list_of_operations.push_back(sin);
                i+=2;
                break;
            }
        case 'c':
            if(i+2>=input.length()){
                result.correct=false;
                return result;
            }
            if(input[i+1]=='o' && input[i+2]=='s'){
                i+=2;
                result.list_of_operations.push_back(cos);
                break;
            } else if(input[i+1]=='t' && input[i+2]=='g') {
                i+=2;
                result.list_of_operations.push_back(ctg);
                break;
            } else {
                result.correct=false;
                return result;
            }
        case 't':
            if(i+1>=input.length() || input[i+1]!='g'){
                result.correct=false;
                return result;
            } else {
                i+=1;
                result.list_of_operations.push_back(tg);
                break;
            }
        case 'l':
            if(i+1>=input.length()){
                result.correct=false;
                return result;
            }
            if(input[i+1]=='n'){
                i+=1;
                result.list_of_operations.push_back(ln);
                break;
            }
            if(input[i+1]=='g'){
                i+=1;
                result.list_of_operations.push_back(lg);
                break;
            }
            if(i+2>=input.length()){
                result.correct=false;
                return result;
            } else
            if(input[i+1]=='o' && input[i+2]=='g'){
                i+=2;
                result.list_of_operations.push_back(log);
                break;
            } else {
                result.correct=false;
                return result;
            }

        default:
            result.correct=false;
            return result;
            break;
        }
    }
    if(lefts!=rights){result.correct=false; return result;}
    if(current_number.length()>0){
            if(no_decimal){
                if(current_number.length()>9){
                    result.list_of_operations.push_back(big_number);
                    bigint a;
                    a.read(current_number);
                    result.big_numbers.push_back(a);
                } else {
                    result.list_of_operations.push_back(small_number);
                    result.small_numbers.push_back(std::stoi(current_number));
                }
            } else {
                result.list_of_operations.push_back(float_number);
                result.float_numbers.push_back(std::stod(current_number));
            }
            current_number.clear();
        }
    return result;
};
//test
void test_print_tokens(Tokens token){
if(token.correct==false){
    std::cout<<"incorrect!"<<"\n";
    return;
}
int smalls=0;
int bigs=0;
int floats=0;
    for(int i=0;i<token.list_of_operations.size();i++){
        if(token.list_of_operations[i]==small_number){
            std::cout<<token.small_numbers[smalls]<<" || ";
            smalls++;
        } else 
        if(token.list_of_operations[i]==big_number){
            std::cout<<token.big_numbers[bigs]<<" || ";
            bigs++;
        } else 
        if(token.list_of_operations[i]==float_number){
            std::cout<<token.float_numbers[floats]<<" || ";
            floats++;
        } else {
            std::cout<<token.list_of_operations[i]<<" || ";
        }

    }
    std::cout<<"\n";
};

}