#!/bin/bash
#SBATCH --time=12:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --job-name=MLE_df_flat
#SBATCH --mem=4GB
#SBATCH --partition=gelifes
#SBATCH --array=1-800
#SBATCH --output=~/iwABC/logs/MLE_df_flat.log

module load R-bundle-CRAN/2023.12-foss-2023a

Rscript ~/iwABC/script/job_MLE_df_flat.R \
   --offset 4000 \
   --index $SLURM_ARRAY_TASK_ID \
   --outdir "~/iwABC/out/mle/flat/" \
   > /dev/null 2>&1
