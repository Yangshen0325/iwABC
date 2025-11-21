#!/bin/bash
#SBATCH --time=10:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1      #
#SBATCH --job-name=yl_run_ABC_lpi
#SBATCH --output=logsFirstTen/yl_run_ABC_lpi-%j.log
#SBATCH --mem=20GB
#SBATCH --partition=regular

# Ensure log directory exists (avoids 'No such file or directory' on some systems)
mkdir -p logsFirstTen

# Check number of arguments
if [ $# -ne 7 ]; then
  echo "Usage: sbatch start_ABC_lpi.sh <param_set> <lac> <mu> <K> <gam> <laa> <ss_set>"
  exit 1
fi

param_set=$1
idparsopt_lac=$2
idparsopt_mu=$3
idparsopt_K=$4
idparsopt_gam=$5
idparsopt_laa=$6
ss_set=$7

module load R-bundle-CRAN/2023.12-foss-2023a

Rscript ~/iwABC/script/job_run_ABC_lpi.R ${param_set} \
                                     ${idparsopt_lac} \
                                     ${idparsopt_mu} \
                                     ${idparsopt_K} \
                                     ${idparsopt_gam} \
                                     ${idparsopt_laa} \
                                     ${ss_set}

