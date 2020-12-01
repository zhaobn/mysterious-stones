#!/bin/sh
# Grid Engine options (lines prefixed with #$)
#$ -N dpa
#$ -cwd
#$ -l h_rt=47:59:59
#$ -l h_vmem=2G
#$ -M s1941626@ed.ac.uk
#$ -m beas 

# Initialise the environment modules
. /etc/profile.d/modules.sh

# Load module
module load R

# Run the program
Rscript ./fit_dpa.R
