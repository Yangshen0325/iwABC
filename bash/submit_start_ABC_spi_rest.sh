#!/bin/bash
#SBATCH --time=0:29:30
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_ABC_spi_rest
#SBATCH --output=logsRest90/start_ABC_spi_rest-%j.log
#SBATCH --mem=5GB
#SBATCH --partition=regular
#SBATCH --export=ALL   # <-- ensure env vars like START_OFFSET pass through

set -euo pipefail
mkdir -p logsRest90

# Arguments: lac mu K gam laa ss_set
if [ $# -ne 6 ]; then
  echo "Usage: sbatch submit_start_ABC_spi_rest.sh <lac> <mu> <K> <gam> <laa> <ss_set>"
  exit 1
fi

idparsopt_lac="$1"
idparsopt_mu="$2"
idparsopt_K="$3"
idparsopt_gam="$4"
idparsopt_laa="$5"
ss_set="$6"

# --- CONFIGURABLE KNOBS ---
GROUP_SIZE=${GROUP_SIZE:-100}
NUM_GROUPS=${NUM_GROUPS:-48}
REPS_PER_GROUP=${REPS_PER_GROUP:-10}
START_OFFSET=${START_OFFSET:-0}    # how many reps to skip at start of each group
# --------------------------
echo "[SPI SUBMIT] GROUP_SIZE=${GROUP_SIZE} NUM_GROUPS=${NUM_GROUPS} START_OFFSET=${START_OFFSET} REPS_PER_GROUP=${REPS_PER_GROUP}"

if (( REPS_PER_GROUP > GROUP_SIZE )); then
  echo "REPS_PER_GROUP ($REPS_PER_GROUP) cannot exceed GROUP_SIZE ($GROUP_SIZE)." >&2
  exit 1
fi

for (( g=0; g<NUM_GROUPS; g++ )); do
  base=$(( g * GROUP_SIZE ))   # 0, 100, 200, ...
  for (( r=1; r<=REPS_PER_GROUP; r++ )); do
    rep=$(( r + START_OFFSET ))            # <-- shift
    if (( rep < 1 || rep > GROUP_SIZE )); then
      continue
    fi
    param_set=$(( base + rep ))            # <-- use rep here
    echo "Submitting job for parameter set ${param_set} (group $((g+1)) rep ${rep})..."
    sbatch ~/iwABC/bash/start_ABC_spi_rest.sh \
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
# REPS_PER_GROUP=10 sbatch bash/submit_start_ABC_spi.sh 1 2 3 4 5 0

# submit the rest:
# export START_OFFSET=10
# export REPS_PER_GROUP=90
# sbatch submit_start_ABC_spi.sh 1 2 3 4 5 0








