#include <iostream>
#include <fstream>
#include <cassert>
#include <stdexcept>
#include <chrono>
#include <vector>
#include <string>
#include <cmath>
#include <iomanip>
#include "../bigint.h"
#include "../evaluate.hpp"

bool test_exceptions = true;
bool verbose_mode = false;
bool benchmark_mode = false;
bool file_output = false;
std::ofstream output_file;

#define ASSERT_EXCEPTION(code, exception_type) \
    if (test_exceptions) { \
        bool caught = false; \
        try { code; } \
        catch (const exception_type&) { caught = true; } \
        assert(caught); \
    }

#define PRINT(msg) \
    do { \
        std::cout << msg << std::endl; \
        if (file_output && output_file.is_open()) { \
            output_file << msg << std::endl; \
        } \
    } while(0)

#define VERBOSE_PRINT(msg) \
    if (verbose_mode) { \
        PRINT("  " << msg); \
    }

class Timer {
public:
    Timer() : start_time(std::chrono::high_resolution_clock::now()) {}
    
    double elapsed() {
        auto end_time = std::chrono::high_resolution_clock::now();
        auto duration = std::chrono::duration_cast<std::chrono::microseconds>(end_time - start_time);
        return duration.count() / 1000.0;
    }
    
    void reset() {
        start_time = std::chrono::high_resolution_clock::now();
    }
    
private:
    std::chrono::high_resolution_clock::time_point start_time;
};

void test_eval_bi_basic_arithmetic() {
    VERBOSE_PRINT("Testing eval_bi basic arithmetic...");
    
    // Addition
    assert(evl::eval_bi("2+3") == bigint(5));
    assert(evl::eval_bi("100+200") == bigint(300));
    assert(evl::eval_bi("0+0") == bigint(0));
    assert(evl::eval_bi("-5+10") == bigint(5));
    assert(evl::eval_bi("123456789+987654321") == bigint("1111111110"));
    
    // Subtraction
    assert(evl::eval_bi("10-4") == bigint(6));
    assert(evl::eval_bi("100-200") == bigint(-100));
    assert(evl::eval_bi("0-5") == bigint(-5));
    assert(evl::eval_bi("1000000000-1") == bigint(999999999));
    
    // Multiplication
    assert(evl::eval_bi("7*8") == bigint(56));
    assert(evl::eval_bi("123*456") == bigint(56088));
    assert(evl::eval_bi("0*999") == bigint(0));
    assert(evl::eval_bi("-5*6") == bigint(-30));
    assert(evl::eval_bi("999999*999999") == bigint("999998000001"));
    
    // Division
    assert(evl::eval_bi("15/3") == bigint(5));
    assert(evl::eval_bi("100/10") == bigint(10));
    assert(evl::eval_bi("7/2") == bigint(3));
    assert(evl::eval_bi("1000000/1000") == bigint(1000));
    
    // Modulo
    assert(evl::eval_bi("17%5") == bigint(2));
    assert(evl::eval_bi("100%7") == bigint(2));
    assert(evl::eval_bi("1000%13") == bigint(12));
    
    VERBOSE_PRINT("Basic arithmetic tests passed");
}

void test_eval_bi_power_operations() {
    VERBOSE_PRINT("Testing eval_bi power operations...");
    
    assert(evl::eval_bi("2^3") == bigint(8));
    assert(evl::eval_bi("2^10") == bigint(1024));
    assert(evl::eval_bi("3^4") == bigint(81));
    assert(evl::eval_bi("5^0") == bigint(1));
    assert(evl::eval_bi("1^100") == bigint(1));
    assert(evl::eval_bi("10^6") == bigint(1000000));
    
    VERBOSE_PRINT("Power operation tests passed");
}

void test_eval_bi_factorial() {
    VERBOSE_PRINT("Testing eval_bi factorial...");
    
    assert(evl::eval_bi("0!") == bigint(1));
    assert(evl::eval_bi("1!") == bigint(1));
    assert(evl::eval_bi("5!") == bigint(120));
    assert(evl::eval_bi("6!") == bigint(720));
    assert(evl::eval_bi("10!") == bigint(3628800));
    
    VERBOSE_PRINT("Factorial tests passed");
}

void test_eval_bi_unary_operations() {
    VERBOSE_PRINT("Testing eval_bi unary operations...");
    
    // Unary minus
    assert(evl::eval_bi("-5") == bigint(-5));
    assert(evl::eval_bi("-(3+2)") == bigint(-5));
    assert(evl::eval_bi("-(-10)") == bigint(10));
    
    VERBOSE_PRINT("Unary operation tests passed");
}

void test_eval_bi_functions() {
    VERBOSE_PRINT("Testing eval_bi functions...");
    
    // Absolute value
    assert(evl::eval_bi("abs(-10)") == bigint(10));
    assert(evl::eval_bi("abs(15)") == bigint(15));
    assert(evl::eval_bi("abs(0)") == bigint(0));
    
    // Square
    assert(evl::eval_bi("sqr(5)") == bigint(25));
    assert(evl::eval_bi("sqr(10)") == bigint(100));
    assert(evl::eval_bi("sqr(0)") == bigint(0));
    
    // Square root
    assert(evl::eval_bi("sqrt(16)") == bigint(4));
    assert(evl::eval_bi("sqrt(25)") == bigint(5));
    assert(evl::eval_bi("sqrt(100)") == bigint(10));
    
    // GCD
    assert(evl::eval_bi("gcd(12,8)") == bigint(4));
    assert(evl::eval_bi("gcd(15,25)") == bigint(5));
    assert(evl::eval_bi("gcd(17,19)") == bigint(1));
    
    // LCM
    assert(evl::eval_bi("lcm(4,6)") == bigint(12));
    assert(evl::eval_bi("lcm(12,18)") == bigint(36));
    assert(evl::eval_bi("lcm(7,11)") == bigint(77));
    
    // Fibonacci
    assert(evl::eval_bi("fib(0)") == bigint(0));
    assert(evl::eval_bi("fib(1)") == bigint(1));
    assert(evl::eval_bi("fib(10)") == bigint(55));
    assert(evl::eval_bi("fib(15)") == bigint(610));
    
    VERBOSE_PRINT("Function tests passed");
}

void test_eval_bi_complex_expressions() {
    VERBOSE_PRINT("Testing eval_bi complex expressions...");
    
    // Operator precedence
    assert(evl::eval_bi("2+3*4") == bigint(14));
    assert(evl::eval_bi("(2+3)*4") == bigint(20));
    assert(evl::eval_bi("2^3+1") == bigint(9));
    assert(evl::eval_bi("2+3^2") == bigint(11));
    
    // Nested expressions
    assert(evl::eval_bi("((2+3)*4-1)^2") == bigint(361));
    assert(evl::eval_bi("2^(3+1)") == bigint(16));
    assert(evl::eval_bi("(5!)/(3!)") == bigint(20));
    
    // Mixed operations
    assert(evl::eval_bi("3!+4!") == bigint(30));
    assert(evl::eval_bi("5!-4!") == bigint(96));
    assert(evl::eval_bi("gcd(48,18)*lcm(4,6)") == bigint(72));
    
    VERBOSE_PRINT("Complex expression tests passed");
}

void test_eval_do_basic_arithmetic() {
    VERBOSE_PRINT("Testing eval_do basic arithmetic...");
    
    const double EPSILON = 1e-10;
    
    // Addition
    assert(std::abs(evl::eval_do("2.5+3.7") - 6.2) < EPSILON);
    assert(std::abs(evl::eval_do("0.1+0.2") - 0.3) < 1e-9);
    assert(std::abs(evl::eval_do("-5.5+10.3") - 4.8) < EPSILON);
    
    // Subtraction
    assert(std::abs(evl::eval_do("10.0-4.5") - 5.5) < EPSILON);
    assert(std::abs(evl::eval_do("3.14-2.14") - 1.0) < EPSILON);
    
    // Multiplication
    assert(std::abs(evl::eval_do("2.5*4.0") - 10.0) < EPSILON);
    assert(std::abs(evl::eval_do("3.14*2") - 6.28) < EPSILON);
    
    // Division
    assert(std::abs(evl::eval_do("15.0/3.0") - 5.0) < EPSILON);
    assert(std::abs(evl::eval_do("22.0/7.0") - 3.142857142857143) < 1e-12);
    
    // Modulo
    assert(std::abs(evl::eval_do("5.5%2.0") - 1.5) < EPSILON);
    
    VERBOSE_PRINT("Basic arithmetic tests passed");
}

void test_eval_do_trigonometric() {
    VERBOSE_PRINT("Testing eval_do trigonometric functions...");
    
    const double EPSILON = 1e-10;
    
    // Basic trig functions
    assert(std::abs(evl::eval_do("sin(0)") - 0.0) < EPSILON);
    assert(std::abs(evl::eval_do("cos(0)") - 1.0) < EPSILON);
    assert(std::abs(evl::eval_do("tan(0)") - 0.0) < EPSILON);
    
    // Pi-based values
    assert(std::abs(evl::eval_do("sin(pi/2)") - 1.0) < EPSILON);
    assert(std::abs(evl::eval_do("cos(pi)") - (-1.0)) < EPSILON);
    
    // Inverse trig functions
    assert(std::abs(evl::eval_do("arcsin(0)") - 0.0) < EPSILON);
    assert(std::abs(evl::eval_do("arccos(1)") - 0.0) < EPSILON);
    assert(std::abs(evl::eval_do("arctan(0)") - 0.0) < EPSILON);
    
    VERBOSE_PRINT("Trigonometric function tests passed");
}

void test_eval_do_logarithmic() {
    VERBOSE_PRINT("Testing eval_do logarithmic functions...");
    
    const double EPSILON = 1e-10;
    
    // Natural logarithm
    assert(std::abs(evl::eval_do("ln(1)") - 0.0) < EPSILON);
    assert(std::abs(evl::eval_do("ln(e)") - 1.0) < EPSILON);
    
    // Base-10 logarithm
    assert(std::abs(evl::eval_do("lg(1)") - 0.0) < EPSILON);
    assert(std::abs(evl::eval_do("lg(10)") - 1.0) < EPSILON);
    assert(std::abs(evl::eval_do("lg(100)") - 2.0) < EPSILON);
    
    // Custom base logarithm
    assert(std::abs(evl::eval_do("log(2,8)") - 3.0) < EPSILON);
    assert(std::abs(evl::eval_do("log(3,27)") - 3.0) < EPSILON);
    
    VERBOSE_PRINT("Logarithmic function tests passed");
}

void test_eval_do_power_and_roots() {
    VERBOSE_PRINT("Testing eval_do power and root functions...");
    
    const double EPSILON = 1e-10;
    
    // Power operations
    assert(std::abs(evl::eval_do("2^3") - 8.0) < EPSILON);
    assert(std::abs(evl::eval_do("2.5^2") - 6.25) < EPSILON);
    assert(std::abs(evl::eval_do("4^0.5") - 2.0) < EPSILON);
    
    // Square root
    assert(std::abs(evl::eval_do("sqrt(16)") - 4.0) < EPSILON);
    assert(std::abs(evl::eval_do("sqrt(2)") - 1.4142135623730951) < EPSILON);
    assert(std::abs(evl::eval_do("sqrt(0.25)") - 0.5) < EPSILON);
    
    VERBOSE_PRINT("Power and root function tests passed");
}

void test_eval_do_constants() {
    VERBOSE_PRINT("Testing eval_do mathematical constants...");
    
    const double EPSILON = 1e-10;
    
    // Pi
    assert(std::abs(evl::eval_do("pi") - 3.141592653589793) < EPSILON);
    
    // Euler's number
    assert(std::abs(evl::eval_do("e") - 2.718281828459045) < EPSILON);
    
    // Golden ratio
    assert(std::abs(evl::eval_do("phi") - 1.618033988749895) < EPSILON);
    
    // Constants in expressions
    assert(std::abs(evl::eval_do("2*pi") - 6.283185307179586) < EPSILON);
    assert(std::abs(evl::eval_do("e^2") - 7.3890560989306504) < EPSILON);
    
    VERBOSE_PRINT("Mathematical constant tests passed");
}

void test_eval_do_complex_expressions() {
    VERBOSE_PRINT("Testing eval_do complex expressions...");
    
    const double EPSILON = 1e-10;
    
    // Mixed operations
    assert(std::abs(evl::eval_do("sqrt(2^2+3^2)") - 3.605551275463989) < EPSILON);
    assert(std::abs(evl::eval_do("sin(pi/4)^2+cos(pi/4)^2") - 1.0) < EPSILON);
    assert(std::abs(evl::eval_do("ln(e^3)") - 3.0) < EPSILON);
    
    // Nested functions
    assert(std::abs(evl::eval_do("sqrt(abs(-16))") - 4.0) < EPSILON);
    assert(std::abs(evl::eval_do("sin(arcsin(0.5))") - 0.5) < EPSILON);
    
    VERBOSE_PRINT("Complex expression tests passed");
}

void test_error_handling() {
    VERBOSE_PRINT("Testing error handling...");
    
    if (test_exceptions) {
        // Division by zero
        ASSERT_EXCEPTION(evl::eval_bi("5/0"), std::runtime_error);
        ASSERT_EXCEPTION(evl::eval_do("5.0/0.0"), std::runtime_error);
        
        // Invalid expressions
        ASSERT_EXCEPTION(evl::eval_bi("2+"), std::runtime_error);
        ASSERT_EXCEPTION(evl::eval_bi("*3"), std::runtime_error);
        ASSERT_EXCEPTION(evl::eval_bi("((2+3)"), std::runtime_error);
        
        // Domain errors
        ASSERT_EXCEPTION(evl::eval_do("sqrt(-1)"), std::runtime_error);
        ASSERT_EXCEPTION(evl::eval_do("ln(0)"), std::runtime_error);
        ASSERT_EXCEPTION(evl::eval_do("arcsin(2)"), std::runtime_error);
        ASSERT_EXCEPTION(evl::eval_do("arccos(-2)"), std::runtime_error);
        
        // Invalid functions
        ASSERT_EXCEPTION(evl::eval_bi("unknown_func(5)"), std::runtime_error);
        ASSERT_EXCEPTION(evl::eval_do("invalid(3.14)"), std::runtime_error);
    }
    
    VERBOSE_PRINT("Error handling tests passed");
}

void test_large_numbers() {
    VERBOSE_PRINT("Testing large number operations...");
    
    // Large factorials
    bigint fact20 = evl::eval_bi("20!");
    assert(fact20.number_of_digits() == 19);
    
    bigint fact50 = evl::eval_bi("50!");
    assert(fact50.number_of_digits() == 65);
    
    // Large powers
    bigint pow2_100 = evl::eval_bi("2^100");
    assert(pow2_100.number_of_digits() == 31);
    
    bigint pow10_50 = evl::eval_bi("10^50");
    assert(pow10_50.number_of_digits() == 51);
    
    // Large Fibonacci numbers
    bigint fib50 = evl::eval_bi("fib(50)");
    assert(fib50.number_of_digits() == 11);
    
    VERBOSE_PRINT("Large number tests passed");
}

void run_benchmarks() {
    PRINT("\n=== EVALUATION BENCHMARKS ===");
    
    Timer timer;
    const int iterations = 10000;
    
    // Basic arithmetic benchmarks
    timer.reset();
    for (int i = 0; i < iterations; i++) {
        evl::eval_bi("123+456");
    }
    PRINT("eval_bi addition: " << std::fixed << std::setprecision(3) << timer.elapsed() << " ms for " << iterations << " operations");
    
    timer.reset();
    for (int i = 0; i < iterations; i++) {
        evl::eval_do("123.456+789.012");
    }
    PRINT("eval_do addition: " << std::fixed << std::setprecision(3) << timer.elapsed() << " ms for " << iterations << " operations");
    
    // Multiplication benchmarks
    timer.reset();
    for (int i = 0; i < iterations; i++) {
        evl::eval_bi("123*456");
    }
    PRINT("eval_bi multiplication: " << std::fixed << std::setprecision(3) << timer.elapsed() << " ms for " << iterations << " operations");
    
    timer.reset();
    for (int i = 0; i < iterations; i++) {
        evl::eval_do("123.456*789.012");
    }
    PRINT("eval_do multiplication: " << std::fixed << std::setprecision(3) << timer.elapsed() << " ms for " << iterations << " operations");
    
    // Power benchmarks
    timer.reset();
    for (int i = 0; i < 1000; i++) {
        evl::eval_bi("2^20");
    }
    PRINT("eval_bi power (2^20): " << std::fixed << std::setprecision(3) << timer.elapsed() << " ms for 1000 operations");
    
    timer.reset();
    for (int i = 0; i < 1000; i++) {
        evl::eval_do("2.0^20.0");
    }
    PRINT("eval_do power (2^20): " << std::fixed << std::setprecision(3) << timer.elapsed() << " ms for 1000 operations");
    
    // Function benchmarks
    timer.reset();
    for (int i = 0; i < 1000; i++) {
        evl::eval_bi("10!");
    }
    PRINT("eval_bi factorial (10!): " << std::fixed << std::setprecision(3) << timer.elapsed() << " ms for 1000 operations");
    
    timer.reset();
    for (int i = 0; i < iterations; i++) {
        evl::eval_do("sin(1.0)");
    }
    PRINT("eval_do sin function: " << std::fixed << std::setprecision(3) << timer.elapsed() << " ms for " << iterations << " operations");
    
    // Complex expression benchmarks
    timer.reset();
    for (int i = 0; i < 1000; i++) {
        evl::eval_bi("(2+3)*4^2-1");
    }
    PRINT("eval_bi complex expression: " << std::fixed << std::setprecision(3) << timer.elapsed() << " ms for 1000 operations");
    
    timer.reset();
    for (int i = 0; i < 1000; i++) {
        evl::eval_do("sqrt(sin(pi/4)^2+cos(pi/4)^2)");
    }
    PRINT("eval_do complex expression: " << std::fixed << std::setprecision(3) << timer.elapsed() << " ms for 1000 operations");
    
    // Large number benchmarks
    timer.reset();
    evl::eval_bi("100!");
    PRINT("eval_bi large factorial (100!): " << std::fixed << std::setprecision(3) << timer.elapsed() << " ms");
    
    timer.reset();
    evl::eval_bi("2^1000");
    PRINT("eval_bi large power (2^1000): " << std::fixed << std::setprecision(3) << timer.elapsed() << " ms");
    
    timer.reset();
    evl::eval_bi("fib(100)");
    PRINT("eval_bi large Fibonacci (fib(100)): " << std::fixed << std::setprecision(3) << timer.elapsed() << " ms");
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
        } else if (arg == "--file" || arg == "-f") {
            file_output = true;
        } else if (arg == "--help" || arg == "-h") {
            std::cout << "Usage: " << argv[0] << " [options]\n";
            std::cout << "Options:\n";
            std::cout << "  --no-exceptions    Skip exception testing\n";
            std::cout << "  --verbose, -v      Enable verbose output\n";
            std::cout << "  --benchmark, -b    Enable benchmark mode\n";
            std::cout << "  --file, -f         Output to file\n";
            std::cout << "  --help, -h         Show this help\n";
            return 0;
        }
    }
    
    if (file_output) {
        output_file.open("evaluate_test_results.txt");
        if (!output_file.is_open()) {
            std::cerr << "Failed to open output file\n";
            return 1;
        }
    }
    
    PRINT("=== EXPRESSION EVALUATOR TEST SUITE ===");
    PRINT("Configuration:");
    PRINT("  Exception testing: " << (test_exceptions ? "ON" : "OFF"));
    PRINT("  Verbose mode: " << (verbose_mode ? "ON" : "OFF"));
    PRINT("  Benchmark mode: " << (benchmark_mode ? "ON" : "OFF"));
    PRINT("  File output: " << (file_output ? "ON" : "OFF"));
    PRINT("");
    
    Timer total_timer;
    
    try {
        PRINT("Running expression evaluation tests...");
        
        test_eval_bi_basic_arithmetic();
        PRINT("[PASS] eval_bi basic arithmetic tests");
        
        test_eval_bi_power_operations();
        PRINT("[PASS] eval_bi power operation tests");
        
        test_eval_bi_factorial();
        PRINT("[PASS] eval_bi factorial tests");
        
        test_eval_bi_unary_operations();
        PRINT("[PASS] eval_bi unary operation tests");
        
        test_eval_bi_functions();
        PRINT("[PASS] eval_bi function tests");
        
        test_eval_bi_complex_expressions();
        PRINT("[PASS] eval_bi complex expression tests");
        
        test_eval_do_basic_arithmetic();
        PRINT("[PASS] eval_do basic arithmetic tests");
        
        test_eval_do_trigonometric();
        PRINT("[PASS] eval_do trigonometric function tests");
        
        test_eval_do_logarithmic();
        PRINT("[PASS] eval_do logarithmic function tests");
        
        test_eval_do_power_and_roots();
        PRINT("[PASS] eval_do power and root function tests");
        
        test_eval_do_constants();
        PRINT("[PASS] eval_do mathematical constant tests");
        
        test_eval_do_complex_expressions();
        PRINT("[PASS] eval_do complex expression tests");
        
        test_error_handling();
        PRINT("[PASS] Error handling tests");
        
        test_large_numbers();
        PRINT("[PASS] Large number tests");
        
        if (benchmark_mode) {
            run_benchmarks();
        }
        
        PRINT("\n=== TEST SUMMARY ===");
        PRINT("[SUCCESS] All expression evaluator tests passed!");
        PRINT("Total execution time: " << std::fixed << std::setprecision(3) << total_timer.elapsed() << " ms");
        
        if (file_output && output_file.is_open()) {
            output_file.close();
            PRINT("Results saved to evaluate_test_results.txt");
        }
        
        return 0;
    }
    catch (const std::exception& e) {
        PRINT("[FAIL] Test failed with exception: " << e.what());
        if (file_output && output_file.is_open()) {
            output_file.close();
        }
        return 1;
    }
    catch (...) {
        PRINT("[FAIL] Test failed with unknown exception");
        if (file_output && output_file.is_open()) {
            output_file.close();
        }
        return 1;
    }
}