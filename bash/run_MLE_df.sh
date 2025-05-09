#!/bin/bash
#SBATCH --time=6-10:55:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=16              # request 16 cores per job
#SBATCH --job-name=MLE_df_nomc
#SBATCH --mem=8GB
#SBATCH --partition=regular
#SBATCH --array=1-48                    # one task per parameter set
#SBATCH --output=logs/MLE_df_nomc-%A_%a.log

Rscript ~/iwABC/script/job_MLE_df.R \
   --index $SLURM_ARRAY_TASK_ID \
   --njobs $SLURM_ARRAY_TASK_COUNT \
   > /dev/null 2>&1
