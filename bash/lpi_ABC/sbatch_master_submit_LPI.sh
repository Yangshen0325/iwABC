#!/bin/bash
#SBATCH --job-name=LPI_master_submit
#SBATCH --output=logsMaster/yl_sbatch_master_submit-%j.log
#SBATCH --time=01:20:00
#SBATCH --partition=regular
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=100M
#SBATCH --export=ALL

set -u

# ------------------ USER CONFIGURABLE KNOBS ------------------
# Chunking: how many submissions per batch and sleep between batches
CHUNK_SIZE=${CHUNK_SIZE:-40}           # submit this many jobs per batch
SLEEP_BETWEEN=${SLEEP_BETWEEN:-20m}      # sleep time between batches (e.g., "1h" or "3600")

# Range of param_set to iterate
START_SET=${START_SET:-161}
END_SET=${END_SET:-320}

# Paths
RUNNER=~/iwABC/bash/lpi_ABC/start_ABC_lpi.sh
OUT_DIR=~/iwABC/bash/lpi_ABC/outNewSimABC_lpi
CHK_ROOT=~/iwABC/bash/lpi_ABC/newSimABC_lpi_firstTen

# ------------------ ARGUMENTS ------------------
# Usage: sbatch sbatch_master_submit_SPI.sh <lac> <mu> <K> <gam> <laa> <ss_set>
if [ $# -ne 6 ]; then
  echo "Usage: sbatch $(basename "$0") <lac> <mu> <K> <gam> <laa> <ss_set>" >&2
  exit 1
fi

idparsopt_lac="$1"
idparsopt_mu="$2"
idparsopt_K="$3"
idparsopt_gam="$4"
idparsopt_laa="$5"
ss_set="$6"

# ------------------ PREP ------------------
mkdir -p logsMaster

echo "[MASTER] Starting master submission controller"
echo "[MASTER] Range: ${START_SET}-${END_SET}, chunk=${CHUNK_SIZE}, sleep=${SLEEP_BETWEEN}"
echo "[MASTER] Runner: ${RUNNER}"
echo "[MASTER] Output dir: ${OUT_DIR}"
echo "[MASTER] Checkpoint root: ${CHK_ROOT}"
echo "[MASTER] Args: lac=${idparsopt_lac} mu=${idparsopt_mu} K=${idparsopt_K} gam=${idparsopt_gam} laa=${idparsopt_laa} ss_set=${ss_set}"

# ------------------ HELPERS ------------------
# returns 0 if output exists, 1 otherwise
output_exists () {
  local n="$1"       # unpadded N (1..4800)
  local ss="$2"
  local f="${OUT_DIR}/param_set_${n}_ss_${ss}.rds"
  [[ -f "$f" ]]
}

# returns 0 if checkpoint dir exists (any iter), 1 otherwise
checkpoint_exists () {
  local n="$1"       # unpadded N
  local n4
  n4=$(printf "%04d" "$n")
  local d="${CHK_ROOT}/checkpoints_lpi_set_${n4}"
  # existence + at least one checkpoint file inside (iter RDS)
  if [[ -d "$d" ]] && compgen -G "${d}/chk_lpi_set${n4}_iter*.rds" > /dev/null; then
    return 0
  else
    return 1
  fi
}

# ------------------ MAIN LOOP ------------------
submitted_in_batch=0
total_submitted=0
total_skipped=0
total_resumed=0
total_fresh=0

for (( param_set=${START_SET}; param_set<=${END_SET}; param_set++ )); do
  # 1) Skip if final output exists
  if output_exists "${param_set}" "${ss_set}"; then
    echo "[SKIP] param_set=${param_set} → final output exists: ${OUT_DIR}/param_set_${param_set}_ss_${ss_set}.rds"
    echo "[DEBUG] Moving to next param_set..."
    ((total_skipped++))
    continue
  fi

  # 2) Determine resume vs fresh (checkpoint uses 4-digit padded set number)
  if checkpoint_exists "${param_set}"; then
    echo "[RESUME] param_set=${param_set} → checkpoint found. Submitting runner for auto-resume..."
    sbatch "${RUNNER}" \
           "${param_set}" \
           "${idparsopt_lac}" \
           "${idparsopt_mu}" \
           "${idparsopt_K}" \
           "${idparsopt_gam}" \
           "${idparsopt_laa}" \
           "${ss_set}"
    ((total_resumed++))
  else
    echo "[START] param_set=${param_set} → no output and no checkpoint. Submitting fresh run..."
    sbatch "${RUNNER}" \
           "${param_set}" \
           "${idparsopt_lac}" \
           "${idparsopt_mu}" \
           "${idparsopt_K}" \
           "${idparsopt_gam}" \
           "${idparsopt_laa}" \
           "${ss_set}"
    ((total_fresh++))
  fi

  ((submitted_in_batch++))
  ((total_submitted++))

  # 3) Chunk gate: after CHUNK_SIZE submissions, sleep
  if (( submitted_in_batch >= CHUNK_SIZE )); then
    echo "[BATCH] Submitted ${submitted_in_batch} jobs in this batch (total so far: ${total_submitted}). Sleeping ${SLEEP_BETWEEN}..."
    sleep "${SLEEP_BETWEEN}"
    submitted_in_batch=0
  fi
done

# If the last batch had fewer than CHUNK_SIZE, we finish without extra sleep.
echo "[DONE] Total submitted: ${total_submitted} (fresh=${total_fresh}, resume=${total_resumed}), skipped=${total_skipped}"




