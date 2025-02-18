#!/bin/zsh

# Function to run a command 5 times and append output to results.txt
run_five_times() {
  for i in {1..5}; do
    { time $1; } 2>> results.txt
  done
}


# Run oat_experiments with hw4programs/regalloctest.oat
echo "Running oat_experiments with hw4programs/regalloctest.oat" >> results.txt
make oat_experiments FILE=hw4programs/regalloctest.oat >> results.txt 2>&1
echo "./a_baseline.out" >> results.txt
run_five_times "./a_baseline.out"
echo "./a_greedy.out" >> results.txt
run_five_times "./a_greedy.out"
echo "./a_better.out" >> results.txt
run_five_times "./a_better.out"
echo "./a_clang.out" >> results.txt
run_five_times "./a_clang.out"

# Run oat_experiments with hw4programs/regalloctest.oat and optimization -O1
echo "Running oat_experiments with hw4programs/regalloctest.oat OPT=-O1" >> results.txt
make oat_experiments FILE=hw4programs/regalloctest.oat OPT=-O1 >> results.txt 2>&1
echo "./a_baseline-O1.out" >> results.txt
run_five_times "./a_baseline-O1.out"
echo "./a_greedy-O1.out" >> results.txt
run_five_times "./a_greedy-O1.out"
echo "./a_better-O1.out" >> results.txt
run_five_times "./a_better-O1.out"
echo "./a_clang-O1.out" >> results.txt
run_five_times "./a_clang-O1.out"

# Run ll_experiments with llprograms/matmul.ll
echo "Running ll_experiments with llprograms/matmul.ll" >> results.txt
make ll_experiments FILE=llprograms/matmul.ll >> results.txt 2>&1
echo "./a_baseline.out" >> results.txt
run_five_times "./a_baseline.out"
echo "./a_greedy.out" >> results.txt
run_five_times "./a_greedy.out"
echo "./a_better.out" >> results.txt
run_five_times "./a_better.out"
echo "./a_clang.out" >> results.txt
run_five_times "./a_clang.out"

# Run ll_experiments with llprograms/matmul.ll and optimization -O1
echo "Running ll_experiments with llprograms/matmul.ll OPT=-O1" >> results.txt
make ll_experiments FILE=llprograms/matmul.ll OPT=-O1 >> results.txt 2>&1
echo "./a_baseline-O1.out" >> results.txt
run_five_times "./a_baseline-O1.out"
echo "./a_greedy-O1.out" >> results.txt
run_five_times "./a_greedy-O1.out"
echo "./a_better-O1.out" >> results.txt
run_five_times "./a_better-O1.out"
echo "./a_clang-O1.out" >> results.txt
run_five_times "./a_clang-O1.out"

# Run oat_experiments with bin/Studenttest.oat
echo "Running oat_experiments with bin/Studenttest.oat" >> results.txt
make oat_experiments FILE=bin/Studenttest.oat >> results.txt 2>&1
echo "./a_baseline.out" >> results.txt
run_five_times "./a_baseline.out"
echo "./a_greedy.out" >> results.txt
run_five_times "./a_greedy.out"
echo "./a_better.out" >> results.txt
run_five_times "./a_better.out"
echo "./a_clang.out" >> results.txt
run_five_times "./a_clang.out"

# Run oat_experiments with bin/Studenttest.oat and optimization -O1
echo "Running oat_experiments with bin/Studenttest.oat OPT=-O1" >> results.txt
make oat_experiments FILE=bin/Studenttest.oat OPT=-O1 >> results.txt 2>&1
echo "./a_baseline-O1.out" >> results.txt
run_five_times "./a_baseline-O1.out"
echo "./a_greedy-O1.out" >> results.txt
run_five_times "./a_greedy-O1.out"
echo "./a_better-O1.out" >> results.txt
run_five_times "./a_better-O1.out"
echo "./a_clang-O1.out" >> results.txt
run_five_times "./a_clang-O1.out"