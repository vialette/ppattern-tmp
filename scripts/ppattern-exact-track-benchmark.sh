#!/bin/bash

# number of runs per iteration
N=10

# p permutation
PMINSIZE=10
PMAXSIZE=20
PSTEPSIZE=10
PMINTRACK=2
PMAXTRACK=6
PSTEPTRACK=1

# q permutation
QMINSIZE=${1:-200}
QMAXSIZE=${2:-600}
QSTEPSIZE=${3:-200}

#!/bin/sh

# number of runs
RUN=1

# running for ever
while true; do
  # generate
  for ((QSIZE=$QMINSIZE; QSIZE<=$QMAXSIZE; QSIZE+=$QSTEPSIZE)); do
    for ((PSIZE=$PMINSIZE; PSIZE<=$PMAXSIZE; PSIZE+=$PSTEPSIZE)); do
      for ((PTRACK=$PMINTRACK; PTRACK <= $PMAXTRACK ; PTRACK+=$PSTEPTRACK)); do
        QTRACK=${PTRACK}

        # output csv file
        CSV=../data/ppattern-exact-track-benchmark-psize-${PSIZE}-qsize-${QSIZE}-PTRACK-${PTRACK}-QTRACK-${QTRACK}.csv

        for ((I = 1; I <= $N; I++)); do
          DATE=`date +"%T"`
          echo "RUN: #$RUN - ITERATION: #${I} - ${DATE}";

          # random generator seed
          SEED=$RANDOM

          # benchmark
          echo ppattern-exact-track-benchmark --psize=${PSIZE} --qsize=${QSIZE} --ptrack=${PTRACK} --qtrack=${QTRACK} --seed=${SEED}
          ../dist/build/ppattern-exact-track-benchmark/ppattern-exact-track-benchmark --psize=${PSIZE} --qsize=${QSIZE} --ptrack=$PTRACK --qtrack=${QTRACK} --seed=${SEED} >> ${CSV}
          echo
        done
      done
    done
  done
  RUN=$((RUN+1))
done
