#!/bin/bash

# number of samples
TRIALS=10

# p permutation
MINPSIZE=5
MAXPSIZE=15
STEPPSIZE=1

# q permutation
MINQSIZE=100
MAXQSIZE=150
STEPQSIZE=25

# generate
while true; do
  for ((QSIZE=$MINQSIZE; QSIZE<=$MAXQSIZE ; QSIZE+=$STEPQSIZE)); do
    for ((PSIZE=$MINPSIZE; PSIZE<=$MAXPSIZE ; PSIZE+=$STEPPSIZE)); do
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
  done
done
