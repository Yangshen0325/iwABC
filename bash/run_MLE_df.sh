#!/bin/bash
#SBATCH --time=6-10:55:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8              # request 8 cores per job
#SBATCH --job-name=MLE_df
#SBATCH --mem=10GB
#SBATCH --partition=regular
#SBATCH --array=1-48                    # one task per parameter set
#SBATCH --output=logs/MLE_df-%A_%a.log

module load R-bundle-CRAN/2023.12-foss-2023a

Rscript -e "if (!requireNamespace('DAISIE', quietly = TRUE)) devtools::install_github('rsetienne/DAISIE', ref = 'develop')"
Rscript -e "if (!requireNamespace('iwABC', quietly = TRUE)) devtools::install_github('Yangshen0325/iwABC')"

Rscript ~/iwABC/script/job_MLE_df.R \
   --index $SLURM_ARRAY_TASK_ID \
   --ncores $SLURM_CPUS_PER_TASK
