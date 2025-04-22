#!/bin/bash
#SBATCH --time=1-10:55:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=MLE_df
#SBATCH --output=logs/MLE_df-%j.log
#SBATCH --mem=10GB
#SBATCH --partition=regular

module load R-bundle-CRAN/2023.12-foss-2023a

Rscript -e "if (!requireNamespace('DAISIE', quietly = TRUE)) devtools::install_github('rsetienne/DAISIE', ref = 'develop')"
Rscript -e "if (!requireNamespace('iwABC', quietly = TRUE)) devtools::install_github('Yangshen0325/iwABC')"

Rscript ~/iwABC/script/job_MLE_df.R
