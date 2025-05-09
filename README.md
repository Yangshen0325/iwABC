# iwABC


```
git clone https://github.com/Yangshen0325/iwABC.git
cd iwABC
git checkout hanno
```

## Before we submit any jobs

*Source* the [preparation script](./bash/prep.sh).<br>
We need to do this only once (or when we want to update packages).<br>
Consider to do this on a interactive node since DAISIE tent to install half of CRAN.'<br>
Btw. pulling from 'DAISIE/develop' seems to be a bit brittle...<br>

```
. ./bash/prep.sh    # note the '.' for 'source'
```

## bash/run_MLE_df.sh

The slurm skript hasn't change much:

```
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

module load R-bundle-CRAN/2023.12-foss-2023a

Rscript ~/iwABC/script/job_MLE_df.R \
   --index $SLURM_ARRAY_TASK_ID \
   --ncores $SLURM_CPUS_PER_TASK \
   --outdir ~/iwABC/out/gelifes/mle/ \
   > /dev/null 2>&1
```

## script/job_MLE_df.R

creates a text file `sim_rep.txt` for every simulation in the output
directory given in:

```R
15: outdir <- "~/iwABC/out/mle/"  # output directory, 1 txt-file per sim
```


