#!/bin/bash

# number of runs per iteration
N=10

# p permutation
PMINSIZE=10
PMAXSIZE=20
PSTEPSIZE=2
PMINSPLIT=2
PMAXSPLIT=6
PSTEPSPLIT=1

# q permutation
QSIZE=$1
QSTEPSPLIT=1

#!/bin/sh

# number of runs
RUN=1

# running for ever
while true; do
  # generate
  for ((PSIZE=$PMINSIZE; PSIZE<=$PMAXSIZE; PSIZE+=$PSTEPSIZE)); do
    for ((PSPLIT=$PMINSPLIT; PSPLIT <= $PMAXSPLIT ; PSPLIT+=$PSTEPSPLIT)); do
      QSPLITMAX=$((PSPLIT+2))
      for ((QSPLIT = $PSPLIT; QSPLIT <= $QSPLITMAX ; QSPLIT+=$QSTEPSPLIT)); do

        # output csv file
        CSV=../data/ppattern-split-benchmark-psize-${PSIZE}-qsize-${QSIZE}-psplit-${PSPLIT}-qsplit-${QSPLIT}.csv

        for ((I = 1; I <= $N; I++)); do
          DATE=`date +"%T"`
          echo "RUN: #$RUN - ITERATION: #${I} - ${DATE}";

          # random generator seed
          SEED=$RANDOM

          # benchmark
          echo ppattern-split-benchmark2 --psize=${PSIZE} --qsize=${QSIZE} --psplit=${PSPLIT} --qsplit=${QSPLIT} --seed=${SEED}
          ../dist/build/ppattern-split-benchmark2/ppattern-split-benchmark2 --psize=${PSIZE} --qsize=${QSIZE} --psplit=$PSPLIT --qsplit=${QSPLIT} --seed=${SEED} >> ${CSV}
          echo
        done
      done
    done
  done
  RUN=$((RUN+1))
done
