@echo off
echo ========================================
echo Compiling and Testing evaluate_test
echo ========================================

REM Check if source file exists
if not exist "evaluate_test.cpp" (
    echo Error: evaluate_test.cpp not found!
    pause
    exit /b 1
)

echo.
echo [1/4] Compiling without optimization (-O0)...
g++ -std=c++20 -O0 -o evaluate_test_debug.exe evaluate_test.cpp
if %errorlevel% neq 0 (
    echo Failed to compile debug version
    pause
    exit /b 1
)
echo Debug version compiled successfully: evaluate_test_debug.exe

echo.
echo [2/4] Compiling with optimization (-O3)...
g++ -std=c++20 -O3 -o evaluate_test_release.exe evaluate_test.cpp
if %errorlevel% neq 0 (
    echo Failed to compile release version
    pause
    exit /b 1
)
echo Release version compiled successfully: evaluate_test_release.exe

echo.
echo ========================================
echo Running Tests
echo ========================================

echo.
echo [3/4] Running DEBUG version with --verbose --benchmark --no-exceptions...
echo ----------------------------------------
evaluate_test_debug.exe --verbose --benchmark --no-exceptions
if %errorlevel% neq 0 (
    echo Debug version failed with exit code %errorlevel%
) else (
    echo Debug version completed successfully
)

echo.
echo [4/4] Running RELEASE version with --verbose --benchmark --no-exceptions...
echo ----------------------------------------
evaluate_test_release.exe --verbose --benchmark --no-exceptions
if %errorlevel% neq 0 (
    echo Release version failed with exit code %errorlevel%
) else (
    echo Release version completed successfully
)

echo.
echo ========================================
echo Test Summary
echo ========================================
echo Debug executable:   evaluate_test_debug.exe
echo Release executable: evaluate_test_release.exe
echo.
echo Both versions have been compiled and tested.
echo Check the output above for any failures.
echo.
pause