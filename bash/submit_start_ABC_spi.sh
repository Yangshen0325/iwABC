#!/bin/bash
#SBATCH --time=0:29:30
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_ABC_spi
#SBATCH --output=logs2/start_ABC_spi-%j.log
#SBATCH --mem=1GB
#SBATCH --partition=regular

set -euo pipefail
mkdir -p logs2

# Arguments: lac mu K gam laa ss_set
if [ $# -ne 6 ]; then
  echo "Usage: sbatch submit_start_ABC.sh <lac> <mu> <K> <gam> <laa> <ss_set>"
  exit 1
fi

idparsopt_lac=$1
idparsopt_mu=$2
idparsopt_K=$3
idparsopt_gam=$4
idparsopt_laa=$5
ss_set=$6

# --- CONFIGURABLE KNOBS ---
GROUP_SIZE=${GROUP_SIZE:-100}   # how many reps per parameter combo
NUM_GROUPS=${NUM_GROUPS:-48}    # how many parameter combos
REPS_PER_GROUP=${REPS_PER_GROUP:-2}  # test first N reps from each group (set to 10 later if you want)
# --------------------------

# Optional: sanity clamp
if (( REPS_PER_GROUP > GROUP_SIZE )); then
  echo "REPS_PER_GROUP ($REPS_PER_GROUP) cannot exceed GROUP_SIZE ($GROUP_SIZE)." >&2
  exit 1
fi

# Loop over groups (0..NUM_GROUPS-1), then the first REPS_PER_GROUP reps (1..N)
for (( g=0; g<NUM_GROUPS; g++ )); do
  base=$(( g * GROUP_SIZE ))   # 0, 100, 200, ...
  for (( r=1; r<=REPS_PER_GROUP; r++ )); do
    param_set=$(( base + r ))  # 1,2 then 101,102 then 201,202, ...
    echo "Submitting job for parameter set ${param_set} (group $((g+1)) rep ${r})..."
    sbatch ~/iwABC/bash/start_ABC_spi.sh \
           "${param_set}" \
           "${idparsopt_lac}" \
           "${idparsopt_mu}" \
           "${idparsopt_K}" \
           "${idparsopt_gam}" \
           "${idparsopt_laa}" \
           "${ss_set}"
  done
done


# If test for the first 10, then on cluster you can run:
# REPS_PER_GROUP=10 sbatch bash/submit_start_ABC.sh 1 1 1 1 1 0








