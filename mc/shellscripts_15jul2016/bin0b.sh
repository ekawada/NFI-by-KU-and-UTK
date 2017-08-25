#!/bin/sh -login
#PBS -l walltime=240:00:00
#PBS -l nodes=1:ppn=1
#PBS -l mem=2gb
#PBS -N jbin0
#PBS -t 1-3

	~/nfi/June2016b sample num_samples=20000 num_warmup=10000 thin=10 data file=~/nfi/rdump/rdump0.R init=0.1 output file=~nfi/stanoutput/bin0chain${PBS_JOBID}.csv
