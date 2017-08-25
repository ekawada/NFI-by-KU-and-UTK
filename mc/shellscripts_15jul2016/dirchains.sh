#!/bin/bash -login
#PBS -l walltime=24:00:00
#PBS -l nodes=1:ppn=1
#PBS -l mem=2gb
#PBS -N chainarray

#Note, I changed the walltime to 4 hours. This will give you the most flexibility on 
# the HPCC because there are a lot of "Buy-in" nodes that are primiarly for the owners
# but let other jobs run on them if they have a walltime of four hours or less
# This will cause your jobs to resubmit more but in the end run much faster due to the
# increase in flexibilty of their scheduling. 

#Load the powertools module
module load powertools

# Submission script should be in home directory.
cd ${PBS_O_WORKDIR} 
mkdir ./stanoutput/${PBS_ARRAYID}
cd ./stanoutput/${PBS_ARRAYID}

# Let longjob script know which file is needed to resubmit the job to the queue
export PBS_JOBSCRIPT=$0

cmd=`tail -n ${PBS_ARRAYID} /mnt/home/qdr/nfi/shellscripts/allchains.txt | head -n 1`
echo ${cmd}
longjob ${cmd}

qstat -f ${PBS_JOBID}


