#!/bin/bash -login
#PBS -l walltime=4:00:00
#PBS -l nodes=1:ppn=1
#PBS -l mem=2gb
#PBS -N chainarray
#PBS -t 1-15

#Note, I changed the walltime to 4 hours. This will give you the most flexibility on 
# the HPCC because there are a lot of "Buy-in" nodes that are primiarly for the owners
# but let other jobs run on them if they have a walltime of four hours or less
# This will cause your jobs to resubmit more but in the end run much faster due to the
# increase in flexibilty of their scheduling. 

#Load the powertools module
module load powertools

#I reccommend puthing this submission script in your /mnt/home/qdr directory and
# add the following command. 
cd ${PBS_O_WORKDIR} 
# cd /mnt/home/qdr #Delete this one

cmd=`tail -n ${PBS_ARRAYID} ./nfi/shellscripts/allchains.txt | head -n 1`
echo ${cmd}
longjob ${cmd}

qstat -f ${PBS_JOBID}


