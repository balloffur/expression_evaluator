#include "timing.hpp"
#include "bigint.h"

int main() {
    bigint a(std::string(100000,1));
    time();
    for(int i=0;i<10000;i++){
        a*=-1;
    }
    time();
}
