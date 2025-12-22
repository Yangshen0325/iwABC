#!/bin/bash
#SBATCH --job-name=start_ABC_spi_resume
#SBATCH --output=logsMaster/tj_sbatch_resume_submit-%j.log
#SBATCH --time=03:00:00
#SBATCH --partition=regular
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=5G
#SBATCH --export=ALL

set -u

# ------------------ PATHS ------------------
RUNNER=~/iwABC/bash/spi_ABC/start_ABC_spi.sh
OUT_DIR=~/iwABC/bash/spi_ABC/outNewSimABC_spi
CHK_ROOT=~/iwABC/bash/spi_ABC/newSimABC_spi_firstTen
PARAM_SET_LIST_RDS=~/iwABCdata/param_set_list.rds

# ------------------ ARGUMENTS ------------------
# Usage: sbatch sbatch_master_submit_SPI_from_list_all_at_once.sh <lac> <mu> <K> <gam> <laa> <ss_set>
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

echo "[MASTER] Starting master submission controller (param_set_list.rds, all-at-once)"
echo "[MASTER] Runner: ${RUNNER}"
echo "[MASTER] Output dir: ${OUT_DIR}"
echo "[MASTER] Checkpoint root: ${CHK_ROOT}"
echo "[MASTER] param_set_list: ${PARAM_SET_LIST_RDS}"
echo "[MASTER] Args: lac=${idparsopt_lac} mu=${idparsopt_mu} K=${idparsopt_K} gam=${idparsopt_gam} laa=${idparsopt_laa} ss_set=${ss_set}"
echo

# ------------------ READ param_set_list ------------------
module load R-bundle-CRAN/2023.12-foss-2023a

param_set_list=$(
  Rscript -e 'args<-commandArgs(TRUE); x<-readRDS(args[1]); cat(as.integer(x), sep=" ")' \
  "${PARAM_SET_LIST_RDS}"
)

if [ -z "${param_set_list}" ]; then
  echo "ERROR: param_set_list is empty or could not be read from ${PARAM_SET_LIST_RDS}" >&2
  exit 1
fi

echo "[MASTER] Will iterate over $(wc -w <<< "${param_set_list}") param_set values."
echo

# ------------------ HELPERS ------------------
output_exists () {
  local n="$1"
  local ss="$2"
  local f="${OUT_DIR}/param_set_${n}_ss_${ss}.rds"
  [[ -f "$f" ]]
}

checkpoint_exists () {
  local n="$1"
  local n4
  n4=$(printf "%04d" "$n")
  local d="${CHK_ROOT}/checkpoints_spi_set_${n4}"
  if [[ -d "$d" ]] && compgen -G "${d}/chk_spi_set${n4}_iter*.rds" > /dev/null; then
    return 0
  else
    return 1
  fi
}

# ------------------ MAIN LOOP (ALL SUBMITTED IMMEDIATELY) ------------------
total_submitted=0
total_skipped=0
total_resumed=0
total_fresh=0

for param_set in ${param_set_list}; do
  # 1) Skip if final output exists
  if output_exists "${param_set}" "${ss_set}"; then
    echo "[SKIP] param_set=${param_set} → final output exists: ${OUT_DIR}/param_set_${param_set}_ss_${ss_set}.rds"
    echo "[DEBUG] Moving to next param_set..."
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

echo "[DONE] Total submitted: ${total_submitted} (fresh=${total_fresh}, resume=${total_resumed}), skipped=${total_skipped}"
