#!/bin/bash
#SBATCH --time=3-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1      #
#SBATCH --job-name=job_run_ABC_spi_rest
#SBATCH --output=logsRest90/ABC_spi_rest-%j.log
#SBATCH --mem=25GB
#SBATCH --partition=regular

# Ensure log directory exists (avoids 'No such file or directory' on some systems)
mkdir -p logsRest90

# Check number of arguments
if [ $# -ne 7 ]; then
  echo "Usage: sbatch start_ABC_spi_rest.sh <param_set> <lac> <mu> <K> <gam> <laa> <ss_set>"
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

Rscript ~/iwABC/script/job_run_ABC_spi.R ${param_set} \
                                     ${idparsopt_lac} \
                                     ${idparsopt_mu} \
                                     ${idparsopt_K} \
                                     ${idparsopt_gam} \
                                     ${idparsopt_laa} \
                                     ${ss_set}

