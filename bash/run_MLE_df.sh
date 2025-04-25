#!/bin/bash
#SBATCH --time=3-10:55:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=8              # request 8 cores per job
#SBATCH --job-name=MLE_df
#SBATCH --mem=10GB
#SBATCH --partition=regular
#SBATCH --array=1-48                    # one task per parameter set
#SBATCH --output=logs/MLE_df-%A_%a.out
#SBATCH --error=logs/MLE_df-%A_%a.err

module load R-bundle-CRAN/2023.12-foss-2023a

Rscript -e "if (!requireNamespace('DAISIE', quietly = TRUE)) devtools::install_github('rsetienne/DAISIE', ref = 'develop')"
Rscript -e "if (!requireNamespace('iwABC', quietly = TRUE)) devtools::install_github('Yangshen0325/iwABC')"

# export so mclapply/foreach picks it up if you use OpenMP
# Sets the environment variable OMP_NUM_THREADS to the number of cores you requested from Slurm
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

# now call your per‐parameter‐set R script,
# passing it both the array index and the number of cores
Rscript ~/iwABC/script/job_MLE_df.R \
        --index $SLURM_ARRAY_TASK_ID \
        --ncores $SLURM_CPUS_PER_TASK
