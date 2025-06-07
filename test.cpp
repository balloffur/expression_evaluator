#include "expressions.h"
using namespace expressions;
#include "timing.hpp"

int main()
{
    std::string test_expr="13())(7.9";
    test_print_tokens(tokenize(test_expr));
    return 0;    
}