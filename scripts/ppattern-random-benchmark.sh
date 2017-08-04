#!/bin/bash

# number of samples
TRIALS=10

# p permutation
PMINSIZE=5
PMAXSIZE=10
PSTEPSIZE=1

# q permutation
QSIZE=100

# generate
for ((PSIZE=$PMINSIZE; PSIZE<=$PMAXSIZE ; PSIZE+=$PSTEPSIZE)); do
  # output csv file
  CSV=../data/ppattern-random-benchmark-psize-${PSIZE}-qsize-${QSIZE}.csv

  for ((I=1; I<=TRIALS ; I+=1)); do
    DATE=`date +"%T"`
    echo "RUN: #$1 - ITERATION: #${I} - ${DATE}";

    # random generator seed
    SEED=$RANDOM

    # benchmark
    echo ppattern-random-benchmark --psize=${PSIZE} --qsize=${QSIZE} --seed=${SEED}
    ../dist/build/ppattern-random-benchmark/ppattern-random-benchmark --psize=${PSIZE} --qsize=${QSIZE} --seed=${SEED} >> ${CSV}
    echo
  done
done
