#!/bin/bash
#SBATCH --time=2:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=t_ss_distri
#SBATCH --output=logs/ssdf-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

module load R-bundle-CRAN/2023.12-foss-2023a

Rscript -e "devtools::install_github('rsetienne/DAISIE', ref = 'develop')"
Rscript -e "devtools::install_github('Yangshen0325/iwABC')"

Rscript ~/iwABC/small_test/define_epsilon/t_ss_distri.R
