#include <iostream>
#include <cassert>
#include <stdexcept>
#include <chrono>
#include <vector>
#include <string>
#include <cmath>
#include <map>
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

// Simple expression evaluator for testing
bigint evaluate_simple_expression(const std::string& expr) {
    // Very basic evaluator - just for testing purposes
    // Supports: number, +number, -number, number+number, number-number, number*number
    
    if (expr.empty()) return bigint(0);
    
    // Find operators
    size_t plus_pos = expr.find('+', 1); // Skip first character for negative numbers
    size_t minus_pos = expr.find('-', 1);
    size_t mult_pos = expr.find('*');
    
    if (plus_pos != std::string::npos) {
        std::string left = expr.substr(0, plus_pos);
        std::string right = expr.substr(plus_pos + 1);
        return bigint(left) + bigint(right);
    }
    
    if (minus_pos != std::string::npos) {
        std::string left = expr.substr(0, minus_pos);
        std::string right = expr.substr(minus_pos + 1);
        return bigint(left) - bigint(right);
    }
    
    if (mult_pos != std::string::npos) {
        std::string left = expr.substr(0, mult_pos);
        std::string right = expr.substr(mult_pos + 1);
        return bigint(left) * bigint(right);
    }
    
    // Single number
    return bigint(expr);
}

void test_basic_evaluation() {
    VERBOSE_PRINT("Testing basic expression evaluation...");
    
    // Single numbers
    assert(evaluate_simple_expression("123") == bigint(123));
    assert(evaluate_simple_expression("-456") == bigint(-456));
    assert(evaluate_simple_expression("0") == bigint(0));
    
    // Addition
    assert(evaluate_simple_expression("123+456") == bigint(579));
    assert(evaluate_simple_expression("1000+2000") == bigint(3000));
    
    // Subtraction
    assert(evaluate_simple_expression("1000-300") == bigint(700));
    assert(evaluate_simple_expression("100-200") == bigint(-100));
    
    // Multiplication
    assert(evaluate_simple_expression("123*456") == bigint(56088));
    assert(evaluate_simple_expression("1000*1000") == bigint(1000000));
    
    VERBOSE_PRINT("Basic evaluation tests passed");
}

void test_large_number_evaluation() {
    VERBOSE_PRINT("Testing large number evaluation...");
    
    // Large number operations
    std::string large1 = "123456789012345678901234567890";
    std::string large2 = "987654321098765432109876543210";
    
    bigint result_add = evaluate_simple_expression(large1 + "+" + large2);
    bigint expected_add = bigint(large1) + bigint(large2);
    assert(result_add == expected_add);
    VERBOSE_PRINT("Large addition: " + result_add.to_string().substr(0, 20) + "...");
    
    bigint result_sub = evaluate_simple_expression(large2 + "-" + large1);
    bigint expected_sub = bigint(large2) - bigint(large1);
    assert(result_sub == expected_sub);
    VERBOSE_PRINT("Large subtraction: " + result_sub.to_string().substr(0, 20) + "...");
    
    // Large multiplication
    std::string med1 = "123456789";
    std::string med2 = "987654321";
    bigint result_mult = evaluate_simple_expression(med1 + "*" + med2);
    bigint expected_mult = bigint(med1) * bigint(med2);
    assert(result_mult == expected_mult);
    VERBOSE_PRINT("Large multiplication: " + result_mult.to_string());
}

void test_edge_cases() {
    VERBOSE_PRINT("Testing edge cases...");
    
    // Zero operations
    assert(evaluate_simple_expression("0+0") == bigint(0));
    assert(evaluate_simple_expression("0-0") == bigint(0));
    assert(evaluate_simple_expression("0*123") == bigint(0));
    assert(evaluate_simple_expression("123*0") == bigint(0));
    
    // Operations with 1
    assert(evaluate_simple_expression("123*1") == bigint(123));
    assert(evaluate_simple_expression("1*456") == bigint(456));
    
    // Negative number operations
    assert(evaluate_simple_expression("-123+456") == bigint(333));
    assert(evaluate_simple_expression("123+-456") == bigint(-333));
    
    VERBOSE_PRINT("Edge case tests passed");
}

void test_factorial_expressions() {
    VERBOSE_PRINT("Testing factorial-like expressions...");
    
    // Simulate factorial calculations using multiplication chains
    bigint fact5 = bigint(1);
    for (int i = 1; i <= 5; i++) {
        std::string expr = fact5.to_string() + "*" + std::to_string(i);
        fact5 = evaluate_simple_expression(expr);
    }
    assert(fact5 == bigint(120)); // 5!
    VERBOSE_PRINT("Factorial 5! = " + fact5.to_string());
    
    // Larger factorial
    bigint fact10 = bigint(1);
    for (int i = 1; i <= 10; i++) {
        std::string expr = fact10.to_string() + "*" + std::to_string(i);
        fact10 = evaluate_simple_expression(expr);
    }
    assert(fact10 == bigint(3628800)); // 10!
    VERBOSE_PRINT("Factorial 10! = " + fact10.to_string());
}

void test_power_expressions() {
    VERBOSE_PRINT("Testing power-like expressions...");
    
    // Simulate 2^10 using repeated multiplication
    bigint power = bigint(1);
    for (int i = 0; i < 10; i++) {
        std::string expr = power.to_string() + "*2";
        power = evaluate_simple_expression(expr);
    }
    assert(power == bigint(1024)); // 2^10
    VERBOSE_PRINT("Power 2^10 = " + power.to_string());
    
    // Simulate 3^5
    bigint power3 = bigint(1);
    for (int i = 0; i < 5; i++) {
        std::string expr = power3.to_string() + "*3";
        power3 = evaluate_simple_expression(expr);
    }
    assert(power3 == bigint(243)); // 3^5
    VERBOSE_PRINT("Power 3^5 = " + power3.to_string());
}

void test_fibonacci_expressions() {
    VERBOSE_PRINT("Testing Fibonacci-like expressions...");
    
    // Calculate Fibonacci numbers using addition
    bigint fib_prev = bigint(0);
    bigint fib_curr = bigint(1);
    
    for (int i = 2; i <= 10; i++) {
        std::string expr = fib_prev.to_string() + "+" + fib_curr.to_string();
        bigint fib_next = evaluate_simple_expression(expr);
        fib_prev = fib_curr;
        fib_curr = fib_next;
    }
    
    // F(10) = 55
    assert(fib_curr == bigint(55));
    VERBOSE_PRINT("Fibonacci F(10) = " + fib_curr.to_string());
    
    // Continue to F(20)
    for (int i = 11; i <= 20; i++) {
        std::string expr = fib_prev.to_string() + "+" + fib_curr.to_string();
        bigint fib_next = evaluate_simple_expression(expr);
        fib_prev = fib_curr;
        fib_curr = fib_next;
    }
    
    // F(20) = 6765
    assert(fib_curr == bigint(6765));
    VERBOSE_PRINT("Fibonacci F(20) = " + fib_curr.to_string());
}

void run_comprehensive_benchmarks() {
    std::cout << "\n=== EXPRESSION EVALUATION BENCHMARKS ===\n";
    
    // Basic operation benchmarks
    {
        Timer timer;
        for (int i = 0; i < 10000; i++) {
            bigint result = evaluate_simple_expression("123+456");
            (void)result;
        }
        std::cout << "Simple addition (123+456): " << timer.elapsed() << " ms for 10000 operations\n";
    }
    
    {
        Timer timer;
        for (int i = 0; i < 10000; i++) {
            bigint result = evaluate_simple_expression("123*456");
            (void)result;
        }
        std::cout << "Simple multiplication (123*456): " << timer.elapsed() << " ms for 10000 operations\n";
    }
    
    // Large number benchmarks
    {
        Timer timer;
        std::string large_expr = "123456789012345678901234567890+987654321098765432109876543210";
        for (int i = 0; i < 1000; i++) {
            bigint result = evaluate_simple_expression(large_expr);
            (void)result;
        }
        std::cout << "Large addition: " << timer.elapsed() << " ms for 1000 operations\n";
    }
    
    {
        Timer timer;
        std::string mult_expr = "123456789*987654321";
        for (int i = 0; i < 1000; i++) {
            bigint result = evaluate_simple_expression(mult_expr);
            (void)result;
        }
        std::cout << "Medium multiplication: " << timer.elapsed() << " ms for 1000 operations\n";
    }
    
    // Factorial benchmark
    {
        Timer timer;
        bigint fact = bigint(1);
        for (int i = 1; i <= 100; i++) {
            std::string expr = fact.to_string() + "*" + std::to_string(i);
            fact = evaluate_simple_expression(expr);
        }
        std::cout << "Factorial 100!: " << timer.elapsed() << " ms (result has " << fact.number_of_digits() << " digits)\n";
    }
    
    // Fibonacci benchmark
    {
        Timer timer;
        bigint fib_prev = bigint(0);
        bigint fib_curr = bigint(1);
        
        for (int i = 2; i <= 1000; i++) {
            std::string expr = fib_prev.to_string() + "+" + fib_curr.to_string();
            bigint fib_next = evaluate_simple_expression(expr);
            fib_prev = fib_curr;
            fib_curr = fib_next;
        }
        std::cout << "Fibonacci F(1000): " << timer.elapsed() << " ms (result has " << fib_curr.number_of_digits() << " digits)\n";
    }
    
    // Power benchmark
    {
        Timer timer;
        bigint power = bigint(1);
        for (int i = 0; i < 100; i++) {
            std::string expr = power.to_string() + "*2";
            power = evaluate_simple_expression(expr);
        }
        std::cout << "Power 2^100: " << timer.elapsed() << " ms (result has " << power.number_of_digits() << " digits)\n";
    }
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
    
    std::cout << "=== EXPRESSION EVALUATOR TEST SUITE ===\n";
    std::cout << "Configuration:\n";
    std::cout << "  Exception testing: " << (test_exceptions ? "ON" : "OFF") << "\n";
    std::cout << "  Verbose mode: " << (verbose_mode ? "ON" : "OFF") << "\n";
    std::cout << "  Benchmark mode: " << (benchmark_mode ? "ON" : "OFF") << "\n\n";
    
    Timer total_timer;
    
    try {
        std::cout << "Running expression evaluation tests...\n";
        
        test_basic_evaluation();
        std::cout << "[PASS] Basic evaluation tests passed\n";
        
        test_large_number_evaluation();
        std::cout << "[PASS] Large number evaluation tests passed\n";
        
        test_edge_cases();
        std::cout << "[PASS] Edge case tests passed\n";
        
        test_factorial_expressions();
        std::cout << "[PASS] Factorial expression tests passed\n";
        
        test_power_expressions();
        std::cout << "[PASS] Power expression tests passed\n";
        
        test_fibonacci_expressions();
        std::cout << "[PASS] Fibonacci expression tests passed\n";
        
        if (benchmark_mode) {
            run_comprehensive_benchmarks();
        }
        
        std::cout << "\n=== TEST SUMMARY ===\n";
        std::cout << "[SUCCESS] All expression evaluator tests passed successfully!\n";
        std::cout << "Total execution time: " << total_timer.elapsed() << " ms\n";
        
        return 0;
    }
    catch (const std::exception& e) {
        std::cout << "[FAIL] Test failed with exception: " << e.what() << std::endl;
        return 1;
    }
    catch (...) {
        std::cout << "[FAIL] Test failed with unknown exception\n";
        return 1;
    }
}