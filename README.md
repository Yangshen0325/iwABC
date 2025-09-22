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

## running MLE

`iw_sim_list.rds` contains 1000 possible repetitions for 48 parameter sets, e.g.<br>
`iw_sim_list[[10]][[123]]` is rep. 123 for set 10.<br>
We need to run at least 100 reps for each set.

### Create random samples

First, `script/gen_run_list.R` creates a random sample of *unfinished* runs by looking into
the output folder `data/num_cycles5`. The choosen samples are written to `script/run_list.Rdata`,
an vector of integers encoded as `set * 10000 + "rep`.
Initially, `data/num_cycles5` is empty, thus we get a list of 4800 elements.

### Single MLE

A single MLE is performed by the R-script `script/job_MLE_run_list.R`.<br>

```
Rscript script/job_MLE_run_list.R --idx 100 --ofs 0 --outdir "~/iwABC/data/num_cycles5"
```

It picks the element `i = idx + ofs` from `script/run_list.Rdata` and runs MLE for
set `sim = i / 10000` and rep `rep = i - (sim * 10000)`.

### Slurm job `script/run_MLE_run_list.sh`

The first submission would look like this:

```
#!/bin/bash
#SBATCH --time=1-0
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --job-name=MLE_run_list
#SBATCH --mem=4GB
#SBATCH --partition=regulars
#SBATCH --array=1-1000
#SBATCH --output=~/iwABC/logs/MLE_run_list.log

module load R-bundle-CRAN/2023.12-foss-2023a

Rscript ~/iwABC/script/job_MLE_run_list.R \
   --ofs 0 \
   --idx $SLURM_ARRAY_TASK_ID \
   --outdir "~/iwABC/data/num_cycles5/" \
   > /dev/null 2>&1
```

Initially, we *would* like to run all 4800 MLEs. However, we can't do this
with a single submission since the max `--array` size is limited to 1000.
Instead have to iterate over the submissions:

```
...
Rscript ~/iwABC/script/job_MLE_run_list.R \
   --ofs 1000 \
...
```

```
...
Rscript ~/iwABC/script/job_MLE_run_list.R \
   --ofs 2000 \
...
```

```
...
Rscript ~/iwABC/script/job_MLE_run_list.R \
   --ofs 3000 \
...
```

```
...
#SBATCH --array=1-800
...
Rscript ~/iwABC/script/job_MLE_run_list.R \
   --ofs 4000 \
...
```

Manual adjustment is a bit tedious but not to bad here.

### Dealing with time-outs

Note that we have set `#SBATCH --time=1-0` since we scheduled a lot of jobs and
we don't run out of quota.
Sure, we can expect that some MLEs have timed-out.
Thus, we need to create another `run_list` by calling `script/gen_run_list.R`
again:

```
Rscript script/gen_run_list.R
to do: 345
```

and submit with adjusted time and array arguments:

```
...
#SBATCH --time=2-0      # <-- 
#SBATCH --array=1-345   # <-- "to do" from above
...
Rscript ~/iwABC/script/job_MLE_run_list.R \
   --ofs 0 \
...
```

We can repeat until we are done (`to do: 0`) or we reach `--time=10-0`, the max. time limit.

