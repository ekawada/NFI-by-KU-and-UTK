#!/bin/bash

#$ -pe smp 2
#$ -q long
#$ -N mc2_52_s3c2b

	~/stan/nfi/mc51/mc2_neutralmodel sample num_samples=10000 num_warmup=5000 thin=5 data file=~/stan/nfi/scaleddata30Nov3.R init=0.1 output file=/scratch365/qread/stan/mc52out/sp3mod2samples2b.csv