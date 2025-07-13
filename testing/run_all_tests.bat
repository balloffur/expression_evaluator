@echo off
echo ========================================
echo Compiling and Running All Tests
echo ========================================

REM Clear previous results
if exist "test_results.txt" del "test_results.txt"

echo Starting comprehensive test suite... > test_results.txt
echo Date: %date% %time% >> test_results.txt
echo ======================================== >> test_results.txt

REM Compile all tests - Debug versions
echo.
echo [1/6] Compiling debug versions...
g++ -std=c++20 -O0 -o bigint_test_debug.exe bigint_test.cpp
g++ -std=c++20 -O0 -o bigint_extra_test_debug.exe bigint_extra_test.cpp
g++ -std=c++20 -O0 -o evaluate_test_debug.exe evaluate_test.cpp

REM Compile all tests - Release versions
echo [2/6] Compiling release versions...
g++ -std=c++20 -O3 -o bigint_test_release.exe bigint_test.cpp
g++ -std=c++20 -O3 -o bigint_extra_test_release.exe bigint_extra_test.cpp
g++ -std=c++20 -O3 -o evaluate_test_release.exe evaluate_test.cpp

echo.
echo [3/6] Running bigint_test (debug)...
echo. >> test_results.txt
echo === BIGINT TEST DEBUG === >> test_results.txt
bigint_test_debug.exe --verbose --benchmark --no-exceptions >> test_results.txt 2>&1

echo [4/6] Running bigint_test (release)...
echo. >> test_results.txt
echo === BIGINT TEST RELEASE === >> test_results.txt
bigint_test_release.exe --verbose --benchmark --no-exceptions >> test_results.txt 2>&1

echo [5/6] Running bigint_extra_test (debug)...
echo. >> test_results.txt
echo === BIGINT EXTRA TEST DEBUG === >> test_results.txt
bigint_extra_test_debug.exe --verbose --benchmark --no-exceptions >> test_results.txt 2>&1

echo [6/6] Running bigint_extra_test (release)...
echo. >> test_results.txt
echo === BIGINT EXTRA TEST RELEASE === >> test_results.txt
bigint_extra_test_release.exe --verbose --benchmark --no-exceptions >> test_results.txt 2>&1

echo [7/8] Running evaluate_test (debug)...
echo. >> test_results.txt
echo === EVALUATE TEST DEBUG === >> test_results.txt
evaluate_test_debug.exe --verbose --benchmark --no-exceptions >> test_results.txt 2>&1

echo [8/8] Running evaluate_test (release)...
echo. >> test_results.txt
echo === EVALUATE TEST RELEASE === >> test_results.txt
evaluate_test_release.exe --verbose --benchmark --no-exceptions >> test_results.txt 2>&1

echo. >> test_results.txt
echo ======================================== >> test_results.txt
echo Test suite completed: %date% %time% >> test_results.txt

echo.
echo ========================================
echo All tests completed!
echo Results saved to: test_results.txt
echo ========================================
echo.
type test_results.txt | find "[FAIL]" >nul
if %errorlevel% equ 0 (
    echo WARNING: Some tests failed! Check test_results.txt
) else (
    echo All tests appear to have passed successfully!
)
echo.
pause