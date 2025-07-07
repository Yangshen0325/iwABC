#!/bin/bash
#SBATCH --time=10-0
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --job-name=MLE_run_list
#SBATCH --mem=4GB
#SBATCH --partition=regulars
#SBATCH --array=1-31
#SBATCH --output=~/iwABC/logs/MLE_run_list.log

module load R-bundle-CRAN/2023.12-foss-2023a

Rscript ~/iwABC/script/job_MLE_run_list.R \
   --ofs 0 \
   --idx $SLURM_ARRAY_TASK_ID \
   --outdir "~/iwABC/data/num_cycles5/" \
   > /dev/null 2>&1

