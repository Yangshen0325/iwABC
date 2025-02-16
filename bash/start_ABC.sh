#!/bin/bash
#SBATCH --time=10:55:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=job_run_ABC
#SBATCH --output=logs/ABC-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

# Arguments to follow the Rscript are as follows:
param_set=${1}
idparsopt_lac=${2}
idparsopt_mu=${3}
idparsopt_K=${4}
idparsopt_gam=${5}
idparsopt_laa=${6}
ss_set=${7}


module load R-bundle-CRAN/2023.12-foss-2023a
Rscript -e "devtools::install_github('rsetienne/DAISIE', ref = 'develop')"
Rscript -e "devtools::install_github('Yangshen0325/iwABC')"

Rscript ~/iwABC/script/job_run_ABC.R ${param_set} \
                                     ${idparsopt_lac} \
                                     ${idparsopt_mu} \
                                     ${idparsopt_K} \
                                     ${idparsopt_gam} \
                                     ${idparsopt_laa} \
                                     ${ss_set}

