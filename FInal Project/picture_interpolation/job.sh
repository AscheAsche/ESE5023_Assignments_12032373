#!/bin/sh
#BSUB -J PCA_interp2     ## job name
#BSUB -q short    ## queue name
#BSUB -n 40        ## number of total cores
#BSUB -W 00:30    ## walltime in hh:mm
#BSUB -e err.log  ## error log
#BSUB -o job.log  ## job log

# Load R
module load R

# Run the script and save output to result.log
echo "--------------------"
echo "RUN main.R at:"
date
Rscript main.R > result.log
echo "End at:"
date
echo "--------------------"
