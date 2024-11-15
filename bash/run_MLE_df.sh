#!/bin/bash
#SBATCH --time=23:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=MLE_df
#SBATCH --output=logs/MLE_df-%j.log
#SBATCH --mem=10GB
#SBATCH --partition=regular

module load R-bundle-CRAN/2023.12-foss-2023a

Rscript -e "devtools::install_github('rsetienne/DAISIE', ref = 'develop')"
Rscript -e "devtools::install_github('Yangshen0325/iwABC')"

Rscript ~/iwABC/script/job_MLE_df.R
