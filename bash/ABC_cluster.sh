#!/bin/bash
#SBATCH --time=14:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=run_ABC
#SBATCH --output=logs/ABC-%j.log
#SBATCH --mem=10GB
#SBATCH --partition=regular

module load R-bundle-CRAN/2023.12-foss-2023a

Rscript -e "devtools::install_github('rsetienne/DAISIE', ref = 'develop')"
Rscript -e "devtools::install_github('Yangshen0325/iwABC')"

Rscript ~/iwABC/script/run_ABC.R
