#include <iostream>
#include <cassert>
#include <cmath>
#include <stdexcept>
#include <chrono>
#include <random>
#include <sstream>
#include "../bigint.h"

bool test_exceptions = true;
bool verbose_mode = false;
bool benchmark_mode = false;

#define ASSERT_EXCEPTION(code, exception_type) \
    if (test_exceptions) { \
        bool caught = false; \
        try { code; } \
        catch (const exception_type&) { caught = true; } \
        assert(caught); \
    }

#define VERBOSE_PRINT(msg) if (verbose_mode) std::cout << "  " << msg << std::endl;

class Timer {
public:
    Timer() : start_time(std::chrono::high_resolution_clock::now()) {}
    
    double elapsed() {
        auto end_time = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time);
        return duration.count() / 1000.0; // milliseconds
    }
    
    void reset() {
        start_time = std::chrono::high_resolution_clock::now();
    }
    
private:
    std::chrono::high_resolution_clock::time_point start_time;
};

std::string generate_random_number(int digits) {
    static std::random_device rd;
    static std::mt19937 gen(rd());
    static std::uniform_int_distribution<> dis(0, 9);
    
    std::string result;
    result.reserve(digits);
    
    // First digit can't be 0
    result += std::to_string(std::uniform_int_distribution<>(1, 9)(gen));
    
    for (int i = 1; i < digits; i++) {
        result += std::to_string(dis(gen));
    }
    
    return result;
}

void test_io_and_constructors() {
    VERBOSE_PRINT("Testing basic constructors...");
    // Basic constructors
    bigint a("12345678901234567890");
    assert(a.to_string() == "12345678901234567890");

    bigint b(-1234567890123456789LL);
    assert(b.to_string() == "-1234567890123456789");

    bigint c;
    assert(c.isZero());

    bigint d = 0;
    assert(d.to_string() == "0");

    bigint e("0000000000123");
    assert(e.to_string() == "123");
    
    // Edge cases
    bigint f("+123");
    assert(f.to_string() == "123");
    
    bigint g("-123");
    assert(g.to_string() == "-123");
    
    bigint h("0");
    assert(h.isZero());
    
    // Very large numbers
    bigint large("999999999999999999999999999999999999999999999999999999999999");
    assert(large.to_string() == "999999999999999999999999999999999999999999999999999999999999");
    VERBOSE_PRINT("Large number constructor: " + large.to_string().substr(0, 20) + "...");
    
    // Test with random large numbers
    for (int i = 0; i < 5; i++) {
        std::string random_str = generate_random_number(50 + i * 10);
        bigint random_big(random_str);
        assert(random_big.to_string() == random_str);
        VERBOSE_PRINT("Random " + std::to_string(50 + i * 10) + "-digit number: " + random_str.substr(0, 15) + "...");
    }
    
    // Test copy constructor
    bigint original("123456789012345678901234567890");
    bigint copy(original);
    assert(copy == original);
    VERBOSE_PRINT("Copy constructor test passed");
    
    // Test assignment operator
    bigint assigned;
    assigned = original;
    assert(assigned == original);
    VERBOSE_PRINT("Assignment operator test passed");
}

void test_comparisons() {
    VERBOSE_PRINT("Testing comparison operators...");
    bigint a = 100;
    bigint b = 200;
    assert(a < b);
    assert(b > a);
    assert(a != b);
    assert(a == 100);
    assert(!(a == b));
    assert(a <= 100);
    assert(b >= 100);
    
    // Edge cases
    bigint zero1(0);
    bigint zero2(0);
    assert(zero1 == zero2);
    assert(!(zero1 != zero2));
    
    // Negative comparisons
    bigint neg1(-100);
    bigint neg2(-200);
    assert(neg2 < neg1); // -200 < -100
    assert(neg1 > neg2);
    
    // Mixed signs
    bigint pos(50);
    bigint neg(-50);
    assert(neg < pos);
    assert(pos > neg);
    
    // Different lengths
    bigint small("99");
    bigint big("100");
    assert(small < big);
    
    bigint huge("999999999999999999999");
    bigint tiny("1");
    assert(tiny < huge);
    VERBOSE_PRINT("Large vs small comparison: " + tiny.to_string() + " < " + huge.to_string().substr(0, 10) + "...");
    
    // Test comparison with very close numbers
    bigint close1("999999999999999999999999999999");
    bigint close2("999999999999999999999999999998");
    assert(close2 < close1);
    assert(close1 > close2);
    VERBOSE_PRINT("Close number comparison test passed");
    
    // Test comparison performance with large numbers
    if (benchmark_mode) {
        Timer timer;
        for (int i = 0; i < 10000; i++) {
            bool result = close1 > close2;
            (void)result; // suppress unused variable warning
        }
        std::cout << "    Comparison benchmark: " << timer.elapsed() << " ms for 10000 operations" << std::endl;
    }
}

void test_add_subtract() {
    VERBOSE_PRINT("Testing addition and subtraction...");
    bigint a = 999999999;
    bigint b = 1;
    assert((a + b).to_string() == "1000000000");

    a += b;
    assert(a.to_string() == "1000000000");

    a -= b;
    assert(a.to_string() == "999999999");

    bigint c = a - a;
    assert(c.isZero());
    
    // Edge cases
    bigint zero(0);
    bigint num(123);
    assert((zero + num) == num);
    assert((num + zero) == num);
    assert((num - zero) == num);
    assert((zero - num) == -num);
    
    // Negative operations
    bigint pos(500);
    bigint neg(-300);
    assert((pos + neg).to_string() == "200");
    assert((neg + pos).to_string() == "200");
    assert((pos - neg).to_string() == "800");
    assert((neg - pos).to_string() == "-800");
    
    // Large number carry
    bigint large1("999999999999999999999999999999");
    bigint one(1);
    assert((large1 + one).to_string() == "1000000000000000000000000000000");
    
    // Unary minus
    bigint positive(123);
    assert((-positive).to_string() == "-123");
    assert((-(-positive)) == positive);
    assert((-zero).isZero());
    VERBOSE_PRINT("Unary minus test: -0 = " + (-zero).to_string());
    
    // Test with very large numbers
    bigint huge1("999999999999999999999999999999999999999999999999");
    bigint huge2("111111111111111111111111111111111111111111111111");
    bigint huge_sum = huge1 + huge2;
    VERBOSE_PRINT("Large addition result: " + huge_sum.to_string().substr(0, 20) + "...");
    
    // Benchmark addition
    if (benchmark_mode) {
        Timer timer;
        bigint sum_result;
        for (int i = 0; i < 1000; i++) {
            sum_result = huge1 + huge2;
        }
        std::cout << "    Addition benchmark: " << timer.elapsed() << " ms for 1000 large additions" << std::endl;
    }
    
    // Test subtraction edge cases
    bigint almost_zero = bigint("1000000000000000000000000000000") - bigint("999999999999999999999999999999");
    assert(almost_zero == 1);
    VERBOSE_PRINT("Subtraction edge case: large - (large-1) = " + almost_zero.to_string());
}

void test_multiplication() {
    VERBOSE_PRINT("Testing multiplication...");
    bigint a("123456789");
    bigint b("1000000000");
    bigint c = a * b;
    assert(c.to_string() == "123456789000000000");

    a *= 10;
    assert(a.to_string() == "1234567890");

    bigint d = a * 0;
    assert(d.isZero());
    
    // Edge cases
    bigint zero(0);
    bigint num(999);
    assert((zero * num).isZero());
    assert((num * zero).isZero());
    
    bigint one(1);
    assert((one * num) == num);
    assert((num * one) == num);
    
    // Negative multiplication
    bigint pos(12);
    bigint neg(-5);
    assert((pos * neg).to_string() == "-60");
    assert((neg * pos).to_string() == "-60");
    assert((neg * neg).to_string() == "25");
    
    // Large multiplication
    bigint big1("999999999");
    bigint big2("999999999");
    assert((big1 * big2).to_string() == "999999998000000001");
    
    // Multiplication by int
    bigint base("123");
    assert((base * 0) == zero);
    assert((base * 1) == base);
    assert((base * (-1)) == -base);
    assert((base * 10).to_string() == "1230");
    VERBOSE_PRINT("Multiplication by int: " + base.to_string() + " * 10 = " + (base * 10).to_string());
    
    // Test large number multiplication
    bigint large_a("123456789012345678901234567890");
    bigint large_b("987654321098765432109876543210");
    bigint large_product = large_a * large_b;
    VERBOSE_PRINT("Large multiplication result length: " + std::to_string(large_product.to_string().length()) + " digits");
    
    // Test different multiplication algorithms
    bigint small_mul = bigint("12345") * bigint("67890");
    VERBOSE_PRINT("Small multiplication: 12345 * 67890 = " + small_mul.to_string());
    
    // Benchmark multiplication
    if (benchmark_mode) {
        Timer timer;
        bigint mult_result;
        for (int i = 0; i < 100; i++) {
            mult_result = large_a * large_b;
        }
        std::cout << "    Multiplication benchmark: " << timer.elapsed() << " ms for 100 large multiplications" << std::endl;
    }
    
    // Test Karatsuba vs simple multiplication threshold
    bigint medium1(generate_random_number(50));
    bigint medium2(generate_random_number(50));
    bigint medium_product = medium1 * medium2;
    VERBOSE_PRINT("Medium multiplication (50 digits each) completed");
}

void test_div_mod() {
    VERBOSE_PRINT("Testing division and modulo...");
    bigint a("123456789000000000");
    bigint b("123456789");
    assert((a / b).to_string() == "1000000000");

    bigint c("12345678901234567890");
    bigint d("987654321");
    bigint r = c % d;
    assert(r < d && r >= 0);

    bigint x = 1000;
    x /= 10;
    assert(x.to_string() == "100");

    x %= 3;
    assert(x.to_string() == "1");
    
    // Division by 1
    bigint num("999888777");
    bigint one(1);
    assert((num / one) == num);
    assert((num % one).isZero());
    
    // Negative division (C++ style)
    bigint pos_div("123");
    bigint neg_div("-10");
    assert((pos_div / neg_div).to_string() == "-12");
    assert((pos_div % neg_div).to_string() == "3"); // remainder has sign of dividend
    
    bigint neg_num("-123");
    bigint pos_div2("10");
    assert((neg_num / pos_div2).to_string() == "-12");
    assert((neg_num % pos_div2).to_string() == "-3");
    
    // Division by int
    bigint big("1000");
    assert((big / 10).to_string() == "100");
    assert((big % 10) == 0);
    
    bigint test_mod("123");
    assert((test_mod % 10) == 3);
    assert((test_mod % (-10)) == 3);
    
    // Exception tests
    ASSERT_EXCEPTION(bigint("100") % bigint(0), std::runtime_error);
    ASSERT_EXCEPTION(bigint("100") % 0, std::runtime_error);
    
    bigint temp("100");
    ASSERT_EXCEPTION(temp %= 0, std::runtime_error);
    VERBOSE_PRINT("Division by zero exception tests passed");
    
    // Test large division
    bigint large_dividend("123456789012345678901234567890123456789012345678901234567890");
    bigint large_divisor("987654321098765432109876543210");
    bigint large_quotient = large_dividend / large_divisor;
    bigint large_remainder = large_dividend % large_divisor;
    VERBOSE_PRINT("Large division quotient: " + large_quotient.to_string());
    VERBOSE_PRINT("Large division remainder: " + large_remainder.to_string());
    
    // Verify division correctness
    bigint verification = large_quotient * large_divisor + large_remainder;
    assert(verification == large_dividend);
    VERBOSE_PRINT("Division verification: quotient * divisor + remainder == dividend");
    
    // Benchmark division
    if (benchmark_mode) {
        Timer timer;
        bigint div_result;
        for (int i = 0; i < 100; i++) {
            div_result = large_dividend / large_divisor;
        }
        std::cout << "    Division benchmark: " << timer.elapsed() << " ms for 100 large divisions" << std::endl;
    }
}

void test_increment_decrement() {
    VERBOSE_PRINT("Testing increment and decrement operators...");
    bigint a = 0;
    ++a;
    assert(a == 1);
    a++;
    assert(a == 2);
    --a;
    assert(a == 1);
    a--;
    assert(a == 0);
    
    // Test return values
    bigint b = 5;
    bigint pre_inc = ++b;
    assert(b == 6 && pre_inc == 6);
    
    bigint post_inc = b++;
    assert(b == 7 && post_inc == 6);
    
    bigint pre_dec = --b;
    assert(b == 6 && pre_dec == 6);
    
    bigint post_dec = b--;
    assert(b == 5 && post_dec == 6);
    
    // Negative numbers
    bigint neg(-5);
    ++neg;
    assert(neg.to_string() == "-4");
    --neg;
    assert(neg.to_string() == "-5");
    
    // Boundary cases
    bigint boundary("999999999999999999");
    ++boundary;
    assert(boundary.to_string() == "1000000000000000000");
     --boundary;
    assert(boundary.to_string() == "999999999999999999");
    
    // Zero crossing
    bigint cross(-1);
    ++cross;
    assert(cross.isZero());
    ++cross;
    assert(cross == 1);
    --cross;
    assert(cross.isZero());
    --cross;
    assert(cross == -1);
    VERBOSE_PRINT("Zero crossing test: -1 -> 0 -> 1 -> 0 -> -1");
    
    // Test increment/decrement with large numbers
    bigint large_boundary("999999999999999999999999999999");
    ++large_boundary;
    assert(large_boundary.to_string() == "1000000000000000000000000000000");
    --large_boundary;
    assert(large_boundary.to_string() == "999999999999999999999999999999");
    VERBOSE_PRINT("Large number increment/decrement test passed");
    
    // Benchmark increment/decrement
    if (benchmark_mode) {
        Timer timer;
        bigint bench_num("500000000000000000000000000000");
        for (int i = 0; i < 10000; i++) {
            ++bench_num;
            --bench_num;
        }
        std::cout << "    Increment/Decrement benchmark: " << timer.elapsed() << " ms for 20000 operations" << std::endl;
    }
    
    // Test edge case: decrementing 1 to 0
    bigint one_to_zero(1);
    --one_to_zero;
    assert(one_to_zero.isZero());
    VERBOSE_PRINT("Decrement 1 to 0 test passed");
    
    // Test edge case: incrementing -1 to 0
    bigint neg_one_to_zero(-1);
    ++neg_one_to_zero;
    assert(neg_one_to_zero.isZero());
    VERBOSE_PRINT("Increment -1 to 0 test passed");
}

void test_bit_shifts() {
    VERBOSE_PRINT("Testing bitwise shift operations...");
    bigint a = 8;
    bigint b = a << 3;
    assert(b == 64);
    b >>= 2;
    assert(b == 16);
    
    // More comprehensive shift tests
    bigint c = 1;
    assert((c << 10) == 1024);
    assert((c << 0) == 1);
    
    bigint d = 1024;
    assert((d >> 10) == 1);
    assert((d >> 0) == 1024);
    
    // Large shifts
    bigint e = 1;
    e <<= 60;
    assert(e.to_string() == "1152921504606846976");
    e >>= 60;
    assert(e == 1);
    
    // Zero shifts
    bigint zero(0);
    assert((zero << 100).isZero());
    assert((zero >> 100).isZero());
    
    // Negative numbers
    bigint neg(-8);
    assert((neg << 2).to_string() == "-32");
    assert((neg >> 1).to_string() == "-4");
    VERBOSE_PRINT("Negative shift: -8 >> 1 = " + (neg >> 1).to_string());
    
    // Test large shifts
    bigint large_shift_test("123456789");
    bigint left_shifted = large_shift_test << 100;
    bigint right_shifted = left_shifted >> 100;
    assert(right_shifted == large_shift_test);
    VERBOSE_PRINT("Large shift round-trip test passed (shift left 100, then right 100)");
    
    // Benchmark shifts
    if (benchmark_mode) {
        Timer timer;
        bigint shift_num("123456789012345678901234567890");
        for (int i = 0; i < 1000; i++) {
            bigint temp = shift_num << 10;
            temp >>= 10;
        }
        std::cout << "    Shift benchmark: " << timer.elapsed() << " ms for 2000 shift operations" << std::endl;
    }
    
    // Test shift by large amounts
    bigint shift_large = bigint(1) << 200;
    VERBOSE_PRINT("1 << 200 has " + std::to_string(shift_large.to_string().length()) + " digits");
}

void test_gcd_lcm() {
    bigint a = 48, b = 180;
    assert(gcd(a, b) == 12);
    assert(lcm(a, b) == 720);
}

void test_pow_mod_and_sqr_mod() {
    VERBOSE_PRINT("Testing power and modular arithmetic...");
    bigint base = 2, power = 10, mod = 1000;
    assert(pow_mod(base, power, mod) == 24);

    bigint a = 3;
    assert(sqr_mod(a, 7) == 2);

    a = 3;
    a.pow_mod(4, 5);
    assert(a == 1);
    
    // Edge cases
    bigint zero_pow = pow_mod(bigint(5), bigint(0), bigint(7));
    assert(zero_pow == 1); // Any number^0 = 1
    
    bigint one_pow = pow_mod(bigint(123), bigint(1), bigint(1000));
    assert(one_pow == 123);
    
    // Large modular exponentiation
    bigint large_result = pow_mod(bigint(2), bigint(100), bigint("1000000007"));
    assert(large_result >= 0 && large_result < bigint("1000000007"));
    
    // sqr_mod edge cases
    bigint sqr_zero = sqr_mod(bigint(0), bigint(10));
    assert(sqr_zero.isZero());
    
    bigint sqr_one = sqr_mod(bigint(1), bigint(10));
    assert(sqr_one == 1);
    VERBOSE_PRINT("sqr_mod edge cases passed");
    
    // Test large modular exponentiation
    bigint large_base("123456789");
    bigint large_exp("987654321");
    bigint large_mod("1000000007");
    bigint large_pow_result = pow_mod(large_base, large_exp, large_mod);
    assert(large_pow_result >= 0 && large_pow_result < large_mod);
    VERBOSE_PRINT("Large modular exponentiation: " + large_base.to_string() + "^" + large_exp.to_string() + " mod " + large_mod.to_string() + " = " + large_pow_result.to_string());
    
    // Benchmark modular exponentiation
    if (benchmark_mode) {
        Timer timer;
        bigint bench_result;
        for (int i = 0; i < 100; i++) {
            bench_result = pow_mod(bigint(2), bigint(1000), bigint("1000000007"));
        }
        std::cout << "    Modular exponentiation benchmark: " << timer.elapsed() << " ms for 100 operations" << std::endl;
    }
}

void test_to_pow() {
    VERBOSE_PRINT("Testing power functions...");
    bigint a = 2;
    a.to_pow(10);
    assert(a == 1024);

    bigint b = 3;
    bigint exp = 4;
    b.to_pow(exp);
    assert(b == 81);
    
    // Edge cases
    bigint zero_base = 0;
    zero_base.to_pow(5);
    assert(zero_base.isZero());
    
    bigint any_base = 123;
    any_base.to_pow(0);
    assert(any_base == 1);
    
    bigint one_base = 1;
    one_base.to_pow(1000);
    assert(one_base == 1);
    
    // Negative base
    bigint neg_base = -2;
    neg_base.to_pow(3);
    assert(neg_base == -8);
    
    bigint neg_base2 = -2;
    neg_base2.to_pow(4);
    assert(neg_base2 == 16);
    
    // Large power
    bigint small_base = 2;
    small_base.to_pow(20);
    assert(small_base.to_string() == "1048576");
    VERBOSE_PRINT("Power calculation: 2^20 = " + small_base.to_string());
    
    // Test very large powers
    bigint large_power_base(3);
    large_power_base.to_pow(100);
    VERBOSE_PRINT("3^100 has " + std::to_string(large_power_base.to_string().length()) + " digits");
    
    // Test power with bigint exponent
    bigint base_for_bigint_exp(2);
    base_for_bigint_exp.to_pow(bigint(50));
    VERBOSE_PRINT("2^50 = " + base_for_bigint_exp.to_string());
    
    // Benchmark power calculation
    if (benchmark_mode) {
        Timer timer;
        for (int i = 0; i < 100; i++) {
            bigint power_bench(2);
            power_bench.to_pow(100);
        }
        std::cout << "    Power benchmark: " << timer.elapsed() << " ms for 100 power calculations (2^100)" << std::endl;
    }
}

void test_properties() {
    bigint a("0");
    assert(a.isZero());
    assert(!a.isPositive());
    assert(!a.isNegative());

    bigint b("-1");
    assert(b.isNegative());
    assert(!b.isPositive());

    bigint c("2");
    assert(c.even());
    assert(!c.odd());

    bigint d("3");
    assert(d.odd());
    assert(!d.even());
}

void test_utilities() {
    VERBOSE_PRINT("Testing utility functions...");
    bigint a("98765432109876543210");
    int count = a.number_of_digits();
    assert(count == a.to_string().length());

    assert(a.at(0) == 0);
    assert(a.at(1) == 1);
    assert(a.at(19) == 9);
    assert(a.at(100) == 0); // out of bounds

    bigint b("64");
    int s = b.shift_to_odd();
    assert(s == 6); // 64 -> divide by 2^6

    bigint c("72");
    assert(c.possible_shifts_to_odd() == 3); // 72 -> 9
    
    // Test abs function
    bigint pos("123");
    assert(pos.abs() == pos);
    
    bigint neg("-456");
    assert(neg.abs().to_string() == "456");
    
    bigint zero(0);
    assert(zero.abs().isZero());
    
    // Global abs function
    assert(abs(bigint("-789")).to_string() == "789");
    assert(abs(bigint("789")).to_string() == "789");
    
    // Test to_int
    bigint small("123");
    assert(small.to_int() == 123);
    
    bigint small_neg("-456");
    assert(small_neg.to_int() == -456);
    
    // Test to_double
    assert(to_double(bigint("123")) == 123.0);
    assert(to_double(bigint("-456")) == -456.0);
    assert(to_double(bigint(0)) == 0.0);
    
    // Test sqrt
    assert(sqrt(bigint("4")) == 2);
    assert(sqrt(bigint("9")) == 3);
    assert(sqrt(bigint("16")) == 4);
    assert(sqrt(bigint("100")) == 10);
    assert(sqrt(bigint("8")) == 2); // floor of sqrt
    assert(sqrt(bigint(0)).isZero());
    VERBOSE_PRINT("sqrt tests: sqrt(4)=2, sqrt(9)=3, sqrt(16)=4, sqrt(100)=10");
    
    // Test sqrt with large numbers
    bigint large_square("1000000000000000000000000000000");
    bigint large_sqrt = sqrt(large_square);
    VERBOSE_PRINT("sqrt of large number: " + large_sqrt.to_string());
    
    // Test number_of_digits with various sizes
    for (int digits = 10; digits <= 100; digits += 10) {
        std::string test_num = generate_random_number(digits);
        bigint test_big(test_num);
        assert(test_big.number_of_digits() == digits);
        VERBOSE_PRINT("Number with " + std::to_string(digits) + " digits verified");
    }
    
    // Test at() function with large numbers
    bigint at_test("1234567890123456789012345678901234567890");
    VERBOSE_PRINT("at() function test: digit at position 0 = " + std::to_string(at_test.at(0)));
    VERBOSE_PRINT("at() function test: digit at position 39 = " + std::to_string(at_test.at(39)));
    
    // Benchmark utility functions
    if (benchmark_mode) {
        Timer timer;
        bigint util_test(generate_random_number(1000));
        for (int i = 0; i < 10000; i++) {
            int digits = util_test.number_of_digits();
            bool is_even = util_test.even();
            (void)digits; (void)is_even; // suppress warnings
        }
        std::cout << "    Utility functions benchmark: " << timer.elapsed() << " ms for 20000 operations" << std::endl;
    }
}


void run_comprehensive_benchmarks() {
    std::cout << "\n=== COMPREHENSIVE BENCHMARKS ===\n";
    
    // Construction benchmark
    {
        Timer timer;
        for (int i = 0; i < 1000; i++) {
            std::string random_str = generate_random_number(100);
            bigint test(random_str);
        }
        std::cout << "Construction (100-digit): " << timer.elapsed() << " ms for 1000 constructions\n";
    }
    
    // String conversion benchmark
    {
        Timer timer;
        bigint large_num(generate_random_number(1000));
        for (int i = 0; i < 1000; i++) {
            std::string str = large_num.to_string();
            (void)str;
        }
        std::cout << "String conversion (1000-digit): " << timer.elapsed() << " ms for 1000 conversions\n";
    }
    
    // Factorial benchmark
    {
        Timer timer;
        bigint factorial(1);
        for (int i = 1; i <= 100; i++) {
            factorial *= i;
        }
        std::cout << "Factorial 100!: " << timer.elapsed() << " ms (result has " << factorial.number_of_digits() << " digits)\n";
    }
    
    // Fibonacci benchmark
    {
        Timer timer;
        bigint fib_prev(0), fib_curr(1);
        for (int i = 2; i <= 1000; i++) {
            bigint fib_next = fib_prev + fib_curr;
            fib_prev = fib_curr;
            fib_curr = fib_next;
        }
        std::cout << "Fibonacci 1000: " << timer.elapsed() << " ms (result has " << fib_curr.number_of_digits() << " digits)\n";
    }
    
    // GCD benchmark
    {
        Timer timer;
        bigint a(generate_random_number(100));
        bigint b(generate_random_number(100));
        for (int i = 0; i < 100; i++) {
            bigint result = gcd(a, b);
            (void)result;
        }
        std::cout << "GCD (100-digit numbers): " << timer.elapsed() << " ms for 100 operations\n";
    }
}

void run_stress_tests() {
    std::cout << "\n=== STRESS TESTS ===\n";
    
    // Test with extremely large numbers
    std::cout << "Testing 10000-digit number operations...\n";
    bigint huge1(generate_random_number(10000));
    bigint huge2(generate_random_number(10000));
    
    Timer timer;
    bigint huge_sum = huge1 + huge2;
    std::cout << "  Addition: " << timer.elapsed() << " ms\n";
    
    timer.reset();
    bigint huge_diff = huge1 - huge2;
    std::cout << "  Subtraction: " << timer.elapsed() << " ms\n";
    
    timer.reset();
    bigint huge_product = huge1 * huge2;
    std::cout << "  Multiplication: " << timer.elapsed() << " ms (result has " << huge_product.number_of_digits() << " digits)\n";
    
    // Test increment/decrement on large numbers
    timer.reset();
    ++huge1;
    --huge1;
    std::cout << "  Increment/Decrement: " << timer.elapsed() << " ms\n";
    
    // Test many small operations
    std::cout << "Testing 100000 small operations...\n";
    timer.reset();
    bigint accumulator(0);
    for (int i = 1; i <= 100000; i++) {
        accumulator += i;
    }
    std::cout << "  Sum 1 to 100000: " << timer.elapsed() << " ms (result: " << accumulator.to_string() << ")\n";
}

int main(int argc, char* argv[]) {
    // Parse command line arguments
    for (int i = 1; i < argc; i++) {
        std::string arg = argv[i];
        if (arg == "--no-exceptions") {
            test_exceptions = false;
        } else if (arg == "--verbose" || arg == "-v") {
            verbose_mode = true;
        } else if (arg == "--benchmark" || arg == "-b") {
            benchmark_mode = true;
        } else if (arg == "--help" || arg == "-h") {
            std::cout << "Usage: " << argv[0] << " [options]\n";
            std::cout << "Options:\n";
            std::cout << "  --no-exceptions    Skip exception testing\n";
            std::cout << "  --verbose, -v      Enable verbose output\n";
            std::cout << "  --benchmark, -b    Enable benchmark mode\n";
            std::cout << "  --help, -h         Show this help\n";
            return 0;
        }
    }
    
    std::cout << "=== BIGINT COMPREHENSIVE TEST SUITE ===\n";
    std::cout << "Configuration:\n";
    std::cout << "  Exception testing: " << (test_exceptions ? "ON" : "OFF") << "\n";
    std::cout << "  Verbose mode: " << (verbose_mode ? "ON" : "OFF") << "\n";
    std::cout << "  Benchmark mode: " << (benchmark_mode ? "ON" : "OFF") << "\n\n";
    
    Timer total_timer;
    
    try {
        std::cout << "Running basic functionality tests...\n";
        test_io_and_constructors();
        std::cout << "(^_^) I/O and constructors tests passed\n";
        
        test_comparisons();
        std::cout << "(^_^) Comparison tests passed\n";
        
        test_add_subtract();
        std::cout << "(^_^) Addition/subtraction tests passed\n";
        
        test_multiplication();
        std::cout << "(^_^) Multiplication tests passed\n";
        
        test_div_mod();
        std::cout << "(^_^) Division/modulo tests passed\n";
        
        test_increment_decrement();
        std::cout << "(^_^) Increment/decrement tests passed\n";
        
        test_bit_shifts();
        std::cout << "(^_^) Bitwise shift tests passed\n";
        
        test_gcd_lcm();
        std::cout << "(^_^) GCD/LCM tests passed\n";
        
        test_pow_mod_and_sqr_mod();
        std::cout << "(^_^) Power/modular arithmetic tests passed\n";
        
        test_to_pow();
        std::cout << "(^_^) Power function tests passed\n";
        
        test_properties();
        std::cout << "(^_^) Property tests passed\n";
        
        test_utilities();
        std::cout << "(^_^) Utility function tests passed\n";
        
        if (benchmark_mode) {
            run_comprehensive_benchmarks();
            run_stress_tests();
        }
        
        std::cout << "\n=== TEST SUMMARY ===\n";
        std::cout << " (^_^) All extended bigint tests passed successfully!\n";
        std::cout << "Total execution time: " << total_timer.elapsed() << " ms\n";
        
        return 0;
    }
    catch (const std::exception& e) {
        std::cout << " (O_O) Test failed with exception: " << e.what() << std::endl;
        return 1;
    }
    catch (...) {
        std::cout << " (O_O) Test failed with unknown exception\n";
        return 1;
    }
}
