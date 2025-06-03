#!/bin/bash
#SBATCH --time=24:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --job-name=MLE_df_dry
#SBATCH --mem=16GB
#SBATCH --partition=gelifes
#SBATCH --array=1-48
#SBATCH --output=~/iwABC/logs/MLE_df_dry.log

module load R-bundle-CRAN/2023.12-foss-2023a

Rscript ~/iwABC/script/job_MLE_dry.R \
--sim $SLURM_ARRAY_TASK_ID \
--ofs 0 \
--ncpu $SLURM_CPUS_PER_TASK \
--outdir "~/iwABC/data/mle_dry" \
> /dev/null 2>&1
