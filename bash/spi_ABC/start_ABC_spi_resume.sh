#!/bin/bash
#SBATCH --job-name=start_ABC_spi_resume
#SBATCH --output=logsMaster/tj_sbatch_resume_submit-%j.log
#SBATCH --time=03:00:00
#SBATCH --partition=regular
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=100M
#SBATCH --export=ALL

set -euo pipefail

# ------------------ PATHS ------------------
RUNNER=~/iwABC/bash/spi_ABC/start_ABC_spi.sh
OUT_DIR=~/iwABC/bash/spi_ABC/outNewSimABC_spi
CHK_ROOT=~/iwABC/bash/spi_ABC/newSimABC_spi_firstTen

PARAM_SET_LIST_RDS=~/iwABCdata/param_set_list.rds

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
echo "[MASTER] Runner: ${RUNNER}"
echo "[MASTER] Output dir: ${OUT_DIR}"
echo "[MASTER] Checkpoint root: ${CHK_ROOT}"
echo "[MASTER] param_set_list: ${PARAM_SET_LIST_RDS}"
echo "[MASTER] Args: lac=${idparsopt_lac} mu=${idparsopt_mu} K=${idparsopt_K} gam=${idparsopt_gam} laa=${idparsopt_laa} ss_set=${ss_set}"
echo

# ------------------ READ param_set_list ------------------
echo "[MASTER] Reading param_set_list from ${PARAM_SET_LIST_RDS} ..."

param_set_list=$(
  Rscript -e 'x <- readRDS(Sys.getenv("PARAM_SET_LIST_RDS")); cat(x, sep=" ")' \
  PARAM_SET_LIST_RDS="${PARAM_SET_LIST_RDS}"
)

if [ -z "${param_set_list}" ]; then
  echo "ERROR: param_set_list is empty or could not be read from ${PARAM_SET_LIST_RDS}" >&2
  exit 1
fi

echo "[MASTER] Will iterate over these param_set values:"
echo "${param_set_list}"
echo

# ------------------ HELPERS ------------------
# returns 0 if output exists, 1 otherwise
output_exists () {
  local n="$1"       # param_set (e.g., 4701)
  local ss="$2"
  local f="${OUT_DIR}/param_set_${n}_ss_${ss}.rds"
  [[ -f "$f" ]]
}

# returns 0 if checkpoint dir exists (any iter), 1 otherwise
checkpoint_exists () {
  local n="$1"       # param_set, may be 1..4800 OR 4-digit-ish (e.g., 4701)
  local n4
  n4=$(printf "%04d" "$n")
  local d="${CHK_ROOT}/checkpoints_spi_set_${n4}"
  if [[ -d "$d" ]] && compgen -G "${d}/chk_spi_set${n4}_iter*.rds" > /dev/null; then
    return 0
  else
    return 1
  fi
}

# ------------------ MAIN LOOP ------------------
total_submitted=0
total_skipped=0
total_resumed=0
total_fresh=0

for param_set in ${param_set_list}; do
  echo "=========================================================="
  echo "[MASTER] Handling param_set=${param_set}"

  # 1) Skip if final output exists
  if output_exists "${param_set}" "${ss_set}"; then
    echo "[SKIP] param_set=${param_set} → final output exists: ${OUT_DIR}/param_set_${param_set}_ss_${ss_set}.rds"
    ((total_skipped++))
    continue
  fi

  # 2) Determine resume vs fresh
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

  ((total_submitted++))
done

echo "=========================================================="
echo "[DONE] Total submitted: ${total_submitted} (fresh=${total_fresh}, resume=${total_resumed}), skipped=${total_skipped}"
