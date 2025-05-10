#!/bin/bash
#SBATCH --time=3:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1              # request 16 cores per job
#SBATCH --job-name=MLE_df_nomc
#SBATCH --mem=8GB
#SBATCH --partition=gelifes
#SBATCH --array=1-48                    # one task per parameter set
#SBATCH --output=logs/MLE_df_nomc-%A_%a.log

# module load R-bundle-CRAN/2023.12-foss-2023a

Rscript ~/iwABC/script/job_MLE_df_flat.R \
   --index $SLURM_ARRAY_TASK_ID \
   --outdir "~/iwABC/out/mle/local/" \
   > /dev/null 2>&1
