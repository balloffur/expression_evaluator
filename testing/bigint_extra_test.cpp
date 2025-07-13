#include "../bigint_extra.h"
#include <cassert>
#include <chrono>
#include <iostream>
#include <stdexcept>
#include <vector>

bool test_exceptions = false;
bool verbose_mode = false;
bool benchmark_mode = false;

#define ASSERT_EXCEPTION(code, exception_type)                                 \
  if (test_exceptions) {                                                       \
    bool caught = false;                                                       \
    try {                                                                      \
      code;                                                                    \
    } catch (const exception_type &) {                                         \
      caught = true;                                                           \
    }                                                                          \
    assert(caught);                                                            \
  }

#define VERBOSE_PRINT(msg)                                                     \
  if (verbose_mode)                                                            \
    std::cout << "  " << msg << std::endl;

class Timer {
public:
  Timer() : start_time(std::chrono::high_resolution_clock::now()) {}

  double elapsed() {
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(
        end_time - start_time);
    return duration.count() / 1000.0; // milliseconds
  }

  void reset() { start_time = std::chrono::high_resolution_clock::now(); }

private:
  std::chrono::high_resolution_clock::time_point start_time;
};

void test_egcd() {
  VERBOSE_PRINT("Testing extended GCD...");

  // Basic test
  std::vector<bigint> result = egcd(bigint(48), bigint(18));
  bigint gcd_val = result[2];
  assert(gcd_val == 6);

  // Verify Bezout coefficients: ax + by = gcd(a,b)
  bigint a = 48, b = 18;
  bigint x = result[0], y = result[1];
  assert(a * x + b * y == gcd_val);
  VERBOSE_PRINT("Bezout identity verified: 48*" + x.to_string() + " + 18*" +
                y.to_string() + " = " + gcd_val.to_string());

  // Test with larger numbers
  result = egcd(bigint(123456), bigint(789012));
  assert(result[2] > 0);
  VERBOSE_PRINT("Large EGCD test passed");

  // Test with very large numbers
  bigint large_a("123456789012345678901234567890");
  bigint large_b("987654321098765432109876543210");
  result = egcd(large_a, large_b);
  assert(result[2] > 0);
  // Verify Bezout identity for large numbers
  assert(large_a * result[0] + large_b * result[1] == result[2]);
  VERBOSE_PRINT("Very large EGCD test passed, GCD = " + result[2].to_string());

  // Test with huge numbers (1000+ digits)
  std::string huge_str1 = "1" + std::string(1000, '2');
  std::string huge_str2 = "9" + std::string(999, '8');
  bigint huge_a(huge_str1);
  bigint huge_b(huge_str2);
  result = egcd(huge_a, huge_b);
  assert(result[2] > 0);
  VERBOSE_PRINT("Huge number EGCD test passed (1000+ digits)");
}

void test_modular_inverse() {
  VERBOSE_PRINT("Testing modular inverse...");

  // Basic test
  bigint inv = modular_inverse(bigint(3), bigint(7));
  assert(inv == 5); // 3 * 5 ≡ 1 (mod 7)

  // Verify inverse
  assert((bigint(3) * inv) % bigint(7) == 1);
  VERBOSE_PRINT("3^(-1) ≡ " + inv.to_string() + " (mod 7)");

  // Test no inverse exists (even numbers)
  bigint no_inv = modular_inverse(bigint(4), bigint(6));
  assert(no_inv == -1);
  VERBOSE_PRINT("No inverse for gcd > 1 case");

  // Test edge cases
  assert(modular_inverse(bigint(1), bigint(5)) == 1);
  assert(modular_inverse(bigint(0), bigint(5)) == -1);
  assert(modular_inverse(bigint(5), bigint(0)) == -1);

  // Test with large numbers
  bigint large_a("123456789012345678901234567891"); // Large odd number
  bigint large_mod("1000000007");                   // Large prime
  bigint large_inv = modular_inverse(large_a, large_mod);
  if (large_inv != -1) {
    assert((large_a * large_inv) % large_mod == 1);
    VERBOSE_PRINT("Large modular inverse verified");
  }

  // Test with very large modulus
  bigint very_large_mod(
      "123456789012345678901234567890123456789012345678901234567891");
  bigint test_num(
      "987654321098765432109876543210987654321098765432109876543211");
  bigint very_large_inv = modular_inverse(test_num, very_large_mod);
  if (very_large_inv != -1) {
    VERBOSE_PRINT("Very large modular inverse computed successfully");
  }
}

void test_random_bigint() {
  VERBOSE_PRINT("Testing random bigint generation...");

  // Test basic generation
  bigint rand1 = random_bigint();
  bigint rand2 = random_bigint();
  assert(!(rand1 == rand2)); // Very unlikely to be equal
  VERBOSE_PRINT("Random bigint 1: " + rand1.to_string().substr(0, 20) + "...");

  // Test fixed length generation
  bigint rand_fixed = random_bigint(5);
  assert(rand_fixed.digits.size() <= 5);
  VERBOSE_PRINT("Random 5-digit bigint: " + rand_fixed.to_string());

  // Test edge cases
  bigint rand_zero = random_bigint(0);
  assert(rand_zero.isZero());

  ASSERT_EXCEPTION(random_bigint(-1), std::invalid_argument);

  // Test large random numbers
  bigint rand_large = random_bigint(20);
  VERBOSE_PRINT("Random 20-digit bigint: " + rand_large.to_string());

  // Test very large random numbers
  bigint rand_very_large = random_bigint(100);
  VERBOSE_PRINT("Random 100-digit bigint length: " +
                std::to_string(rand_very_large.number_of_digits()) + " digits");

  // Test huge random numbers
  bigint rand_huge = random_bigint(500);
  VERBOSE_PRINT("Random 500-digit bigint generated successfully");

  // Verify different lengths produce different ranges
  for (int len = 1; len <= 10; len++) {
    bigint rand_test = random_bigint(len);
    assert(rand_test.digits.size() <= len);
    VERBOSE_PRINT("Length " + std::to_string(len) + " random number: " +
                  std::to_string(rand_test.digits.size()) + " actual digits");
  }
}

void test_prime_functions() {
  VERBOSE_PRINT("Testing prime functions...");

  // Test small primes
  assert(test_if_prime(bigint(2)) == true);
  assert(test_if_prime(bigint(3)) == true);
  assert(test_if_prime(bigint(5)) == true);
  assert(test_if_prime(bigint(7)) == true);
  assert(test_if_prime(bigint(11)) == true);

  // Test small composites
  assert(test_if_prime(bigint(4)) == false);
  assert(test_if_prime(bigint(6)) == false);
  assert(test_if_prime(bigint(8)) == false);
  assert(test_if_prime(bigint(9)) == false);
  assert(test_if_prime(bigint(10)) == false);

  // Test edge cases
  assert(test_if_prime(bigint(0)) == false);
  assert(test_if_prime(bigint(1)) == false);
  assert(test_if_prime(bigint(-5)) == false);

  // Test larger primes
  assert(test_if_prime(bigint(97)) == true);
  assert(test_if_prime(bigint(101)) == true);
  assert(test_if_prime(bigint(997)) == true);

  VERBOSE_PRINT("Basic primality tests passed");

  // Test Miller-Rabin for int
  assert(MillerRabbin(2) == true);
  assert(MillerRabbin(97) == true);
  assert(MillerRabbin(4) == false);
  assert(MillerRabbin(9) == false);

  // Test Miller-Rabin for bigint
  assert(MillerRabbin(bigint(97)) == true);
  assert(MillerRabbin(bigint(101)) == true);
  assert(MillerRabbin(bigint(4)) == false);

  VERBOSE_PRINT("Miller-Rabin tests passed");

  // Test large known primes
  bigint large_prime1("1000000007");
  assert(test_if_prime(large_prime1) == true);
  VERBOSE_PRINT("Large prime 1000000007 verified");

  bigint large_prime2("982451653");
  assert(test_if_prime(large_prime2) == true);
  VERBOSE_PRINT("Large prime 982451653 verified");

  // Test large composites
  bigint large_composite = large_prime1 * large_prime2;
  assert(test_if_prime(large_composite) == false);
  VERBOSE_PRINT("Large composite number correctly identified");

  // Test very large probable primes (Mersenne-like)
  bigint very_large_prime("2305843009213693951"); // 2^61 - 1
  bool is_prime = test_if_prime(very_large_prime);
  VERBOSE_PRINT("Very large number primality test completed: " +
                std::string(is_prime ? "prime" : "composite"));

  // Test huge numbers (probabilistic)
  std::string huge_odd = "1" + std::string(100, '0') + "1";
  bigint huge_num(huge_odd);
  bool huge_result = test_if_prime(huge_num);
  VERBOSE_PRINT("Huge 100+ digit number primality test completed");
}

void test_random_prime() {
  VERBOSE_PRINT("Testing random prime generation...");

  // Test random prime generation
  bigint prime1 = random_prime_bigint();
  assert(test_if_prime(prime1));
  VERBOSE_PRINT("Generated random prime: " + prime1.to_string().substr(0, 20) +
                "...");

  // Test fixed length random prime
  bigint prime2 = random_prime_bigint(3);
  assert(test_if_prime(prime2));
  VERBOSE_PRINT("Generated 3-digit random prime: " + prime2.to_string());

  // Test exception for invalid length
  ASSERT_EXCEPTION(random_prime_bigint(-1), std::invalid_argument);
  ASSERT_EXCEPTION(random_prime_bigint(0), std::invalid_argument);

  if (benchmark_mode) {
    Timer timer;
    for (int i = 0; i < 10; i++) {
      bigint p = random_prime_bigint(2);
      assert(test_if_prime(p));
    }
    std::cout << "    Random prime generation benchmark: " << timer.elapsed()
              << " ms for 10 primes" << std::endl;
  }
}


void test_fermat() {
  VERBOSE_PRINT("Testing Fermat primality test...");

  // Test known primes
  assert(Fermat(bigint(97), 5) == true);
  assert(Fermat(bigint(101), 5) == true);

  // Test known composites
  assert(Fermat(bigint(4), 5) == false);
  assert(Fermat(bigint(9), 5) == false);

  // Test edge cases
  assert(Fermat(bigint(0), 5) == false);
  assert(Fermat(bigint(1), 5) == false);
  assert(Fermat(bigint(2), 5) == true);

  // Test larger primes
  bigint large_prime("1000000007");
  bool fermat_result = Fermat(large_prime, 10);
  VERBOSE_PRINT("Fermat test on large prime 1000000007: " +
                std::string(fermat_result ? "passed" : "(O_O)ed"));

  // Test large composites
  bigint large_composite = bigint("1000000007") * bigint("1000000009");
  bool fermat_composite = Fermat(large_composite, 10);
  VERBOSE_PRINT("Fermat test on large composite: " +
                std::string(fermat_composite ? "passed" : "(O_O)ed"));

  // Test very large numbers
  bigint very_large(
      "123456789012345678901234567890123456789012345678901234567891");
  bool very_large_result = Fermat(very_large, 5);
  VERBOSE_PRINT("Fermat test on very large number completed");

  VERBOSE_PRINT("Fermat test passed");
}

void test_combinatorics() {
  VERBOSE_PRINT("Testing combinatorial functions...");

  // Test factorial
  bigint fact5 = factorial(5);
  assert(fact5 == 120);
  VERBOSE_PRINT("5! = " + fact5.to_string());

  bigint fact0 = factorial(0);
  assert(fact0 == 1);

  bigint fact_neg = factorial(-1);
  assert(fact_neg == -1);

  // Test larger factorials
  bigint fact10 = factorial(10);
  assert(fact10 == 3628800);
  VERBOSE_PRINT("10! = " + fact10.to_string());

  bigint fact20 = factorial(20);
  VERBOSE_PRINT("20! = " + fact20.to_string() + " (" +
                std::to_string(fact20.number_of_digits()) + " digits)");

  // Test very large factorial
  bigint fact100 = factorial(100);
  VERBOSE_PRINT("100! has " + std::to_string(fact100.number_of_digits()) +
                " digits");

  // Test binomial coefficients
  bigint binom = binomial(5, 2);
  assert(binom == 10); // C(5,2) = 10
  VERBOSE_PRINT("C(5,2) = " + binom.to_string());

  bigint binom_edge = binomial(5, 0);
  assert(binom_edge == 1);

  // Test larger binomial coefficients
  bigint binom_large = binomial(50, 25);
  VERBOSE_PRINT("C(50,25) = " + binom_large.to_string().substr(0, 20) +
                "... (" + std::to_string(binom_large.number_of_digits()) +
                " digits)");

  bigint binom_very_large = binomial(100, 50);
  VERBOSE_PRINT("C(100,50) has " +
                std::to_string(binom_very_large.number_of_digits()) +
                " digits");

  // Test Fibonacci
  bigint fib5 = fibonacci(5);
  assert(fib5 == 5); // F(5) = 5
  VERBOSE_PRINT("F(5) = " + fib5.to_string());

  bigint fib0 = fibonacci(0);
  assert(fib0 == 0);

  bigint fib1 = fibonacci(1);
  assert(fib1 == 1);

  // Test larger Fibonacci
  bigint fib20 = fibonacci(20);
  VERBOSE_PRINT("F(20) = " + fib20.to_string());

  bigint fib50 = fibonacci(50);
  VERBOSE_PRINT("F(50) = " + fib50.to_string() + " (" +
                std::to_string(fib50.number_of_digits()) + " digits)");

  // Test very large Fibonacci
  bigint fib100 = fibonacci(100);
  VERBOSE_PRINT("F(100) has " + std::to_string(fib100.number_of_digits()) +
                " digits");

  bigint fib500 = fibonacci(500);
  VERBOSE_PRINT("F(500) has " + std::to_string(fib500.number_of_digits()) +
                " digits");

  // Test Fibonacci with bigint input
  bigint fib_big_input = fibonacci(bigint(30));
  VERBOSE_PRINT("F(30) via bigint input = " + fib_big_input.to_string());

  VERBOSE_PRINT("Combinatorics tests passed");
}

void test_sieve_functions() {
  VERBOSE_PRINT("Testing sieve functions...");

  // Test prime count
  int count = prime_count(100);
  assert(count == 25); // There are 25 primes less than 100
  VERBOSE_PRINT("Primes less than 100: " + std::to_string(count));

  // Test edge cases
  assert(prime_count(2) == 1);
  assert(prime_count(1) == -1);
  assert(prime_count(-1) == -1);

  // Test sieve_mil function
  sieve_mil();
  int count_1000 = prime_count(1000);
  assert(count_1000 == 168); // There are 168 primes less than 1000
  VERBOSE_PRINT("Primes less than 1000: " + std::to_string(count_1000));

  if (benchmark_mode) {
    Timer timer;
    sieve_mil();
    std::cout << "    Sieve to 1M benchmark: " << timer.elapsed() << " ms"
              << std::endl;
  }
}

void run_comprehensive_benchmarks() {
  std::cout << "\n=== BIGINT EXTRA BENCHMARKS ===\n";

  // EGCD benchmark
  {
    Timer timer;
    for (int i = 0; i < 100; i++) {
      bigint a = random_bigint(3);
      bigint b = random_bigint(3);
      std::vector<bigint> result = egcd(a, b);
      (void)result;
    }
    std::cout << "EGCD (3-digit numbers): " << timer.elapsed()
              << " ms for 100 operations\n";
  }

  // Large EGCD benchmark
  {
    Timer timer;
    for (int i = 0; i < 10; i++) {
      bigint a = random_bigint(10);
      bigint b = random_bigint(10);
      std::vector<bigint> result = egcd(a, b);
      (void)result;
    }
    std::cout << "EGCD (10-digit numbers): " << timer.elapsed()
              << " ms for 10 operations\n";
  }

  // Primality testing benchmark
  {
    Timer timer;
    for (int i = 0; i < 100; i++) {
      bigint n = random_bigint(2);
      bool is_prime = test_if_prime(n);
      (void)is_prime;
    }
    std::cout << "Primality testing (2-digit): " << timer.elapsed()
              << " ms for 100 tests\n";
  }

  // Large primality testing benchmark
  {
    Timer timer;
    for (int i = 0; i < 10; i++) {
      bigint n = random_bigint(5);
      bool is_prime = test_if_prime(n);
      (void)is_prime;
    }
    std::cout << "Primality testing (5-digit): " << timer.elapsed()
              << " ms for 10 tests\n";
  }

 
  // Fibonacci benchmark
  {
    Timer timer;
    bigint fib_result = fibonacci(1000);
    std::cout << "Fibonacci 1000: " << timer.elapsed() << " ms (result has "
              << fib_result.number_of_digits() << " digits)\n";
  }

  // Large Fibonacci benchmark
  {
    Timer timer;
    bigint fib_result = fibonacci(5000);
    std::cout << "Fibonacci 5000: " << timer.elapsed() << " ms (result has "
              << fib_result.number_of_digits() << " digits)\n";
  }

  // Very Large Fibonacci benchmark
  {
    Timer timer;
    bigint fib_result = fibonacci(100000);
    std::cout << "Fibonacci 100000: " << timer.elapsed() << " ms (result has "
              << fib_result.number_of_digits() << " digits)\n";
  }

  // Extremley Large Fibonacci benchmark
  {
    Timer timer;
    bigint fib_result = fibonacci(1000000);
    std::cout << "Fibonacci 1000000: " << timer.elapsed() << " ms (result has "
              << fib_result.number_of_digits() << " digits)\n";
  }

  // Factorial benchmark
  {
    Timer timer;
    bigint fact_result = factorial(1000);
    std::cout << "Factorial 1000: " << timer.elapsed() << " ms (result has "
              << fact_result.number_of_digits() << " digits)\n";
  }

  // Large Factorial benchmark
  {
    Timer timer;
    bigint fact_result = factorial(10000);
    std::cout << "Factorial 10000: " << timer.elapsed() << " ms (result has "
              << fact_result.number_of_digits() << " digits)\n";
  }

  // Very Large Factorial benchmark
  {
    Timer timer;
    bigint fact_result = factorial(12345);
    std::cout << "Factorial 12345: " << timer.elapsed() << " ms (result has "
              << fact_result.number_of_digits() << " digits)\n";
  }
}

int main(int argc, char *argv[]) {
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

  std::cout << "=== BIGINT EXTRA COMPREHENSIVE TEST SUITE ===\n";
  std::cout << "Configuration:\n";
  std::cout << "  Exception testing: " << (test_exceptions ? "ON" : "OFF")
            << "\n";
  std::cout << "  Verbose mode: " << (verbose_mode ? "ON" : "OFF") << "\n";
  std::cout << "  Benchmark mode: " << (benchmark_mode ? "ON" : "OFF")
            << "\n\n";

  Timer total_timer;

  try {
    std::cout << "Running bigint_extra functionality tests...\n";

    test_egcd();
    std::cout << "[(^_^)] Extended GCD tests passed\n";

    test_modular_inverse();
    std::cout << "[(^_^)] Modular inverse tests passed\n";

    test_random_bigint();
    std::cout << "[(^_^)] Random bigint tests passed\n";

    test_prime_functions();
    std::cout << "[(^_^)] Prime function tests passed\n";

    test_random_prime();    
    std::cout << "[(^_^)] Random prime tests passed\n";

    test_fermat();
    std::cout << "[(^_^)] Fermat test passed\n";

    test_combinatorics();
    std::cout << "[(^_^)] Combinatorics tests passed\n";

    test_sieve_functions();
    std::cout << "[(^_^)] Sieve function tests passed\n";

    if (benchmark_mode) {
      run_comprehensive_benchmarks();
    }

    std::cout << "\n=== TEST SUMMARY ===\n";
    std::cout << "[SUCCESS] All bigint_extra tests passed successfully!\n";
    std::cout << "Total execution time: " << total_timer.elapsed() << " ms\n";

    return 0;
  } catch (const std::exception &e) {
    std::cout << "[(O_O)] Test (O_O)ed with exception: " << e.what()
              << std::endl;
    return 1;
  } catch (...) {
    std::cout << "[(O_O)] Test (O_O)ed with unknown exception\n";
    return 1;
  }
}