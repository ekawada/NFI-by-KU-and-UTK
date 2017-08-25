#!/bin/bash

#$ -pe smp 1
#$ -q long
#$ -N mcsub
#$ -t 1-105:1

taskindex=$((SGE_TASK_ID - 1))

chains=(1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3 1 2 3)
models=(1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5)
datasets=(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 6 6 6 6 6 6 6 6 6 6 6 6 6 6 6 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7)

chain=$((chains[taskindex]))
model=$((models[taskindex]))
species=$((datasets[taskindex]))

if [ "$model" -eq "1" ]
then ~/stan/nfi/mc51/mc1_nullmodel sample num_samples=10000 num_warmup=5000 thin=5 data file=~/stan/nfi/rdump/subsampledata${species}.R init=0.1 output file=/scratch365/qread/stan/mcsub/dat${species}mod${model}samples${chain}.csv
fi

if [ "$model" -eq "2" ]
then ~/stan/nfi/mc51/mc2_neutralmodel sample num_samples=10000 num_warmup=5000 thin=5 data file=~/stan/nfi/rdump/subsampledata${species}.R init=0.1 output file=/scratch365/qread/stan/mcsub/dat${species}mod${model}samples${chain}.csv
fi

if [ "$model" -eq "3" ]
then ~/stan/nfi/mc51/mc3_speciesmodel sample num_samples=10000 num_warmup=5000 thin=5 data file=~/stan/nfi/rdump/subsampledata${species}.R init=0.1 output file=/scratch365/qread/stan/mcsub/dat${species}mod${model}samples${chain}.csv
fi

if [ "$model" -eq "4" ]
then ~/stan/nfi/mc51/mc4_traitmodelsimple sample num_samples=10000 num_warmup=5000 thin=5 data file=~/stan/nfi/rdump/subsampledata${species}.R init=0.1 output file=/scratch365/qread/stan/mcsub/dat${species}mod${model}samples${chain}.csv
fi

if [ "$model" -eq "5" ]
then ~/stan/nfi/mc51/mc5_nichemodelsimple sample num_samples=10000 num_warmup=5000 thin=5 data file=~/stan/nfi/rdump/subsampledata${species}.R init=0.1 output file=/scratch365/qread/stan/mcsub/dat${species}mod${model}samples${chain}.csv
fi
