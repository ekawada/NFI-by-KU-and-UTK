#!/bin/bash -login
#PBS -l walltime=72:00:00
#PBS -l nodes=1:ppn=1
#PBS -l mem=2gb
#PBS -N chainarray
#PBS -t 1-15

cd /mnt/home/qdr

cmd=`tail -n ${PBS_ARRAYID} ./nfi/shellscripts/allchains.txt | head -n 1`
echo ${cmd}
${cmd}

qstat -f ${PBS_JOBID}
