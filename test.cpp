#include "bigint.h"
#include "evaluate.hpp"


int main(){
    bigint a("-1");
    bigint b(-1);
    bigint c(0);
    bigint d((double)-1000);
    std::cout<<a.isNegative()<<" "<<b.isNegative()<<" "<<c.isNegative()<<" "<<d.isNegative()<<"\n";    
}