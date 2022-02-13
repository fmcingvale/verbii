
REM run this before checking in revisions so i can see if the unittest results
REM changed from the expected values
dotnet run -- -test ../unittests/unittest_core.txt > ../unittests/RESULTS/csharp/actual_unittest_core.txt
dotnet run -- -test ../unittests/unittest_basic.txt > ../unittests/RESULTS/csharp/actual_unittest_basic.txt
dotnet run -- -test ../unittests/unittest_errors.txt > ../unittests/RESULTS/csharp/actual_unittest_errors.txt

REM demos are full programs, so run without -test
dotnet run -- ../unittests/demo_math.txt > ../unittests/RESULTS/csharp/actual_demo_math.txt
