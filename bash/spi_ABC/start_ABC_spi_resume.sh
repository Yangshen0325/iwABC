#!/bin/bash
#SBATCH --job-name=start_ABC_spi_resume
#SBATCH --output=logsMaster/tj_sbatch_resume_submit-%j.log
#SBATCH --time=03:00:00
#SBATCH --partition=regular
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=5G
#SBATCH --export=ALL

set -euo pipefail

# ------------------ PATHS ------------------
RUNNER="$HOME/iwABC/bash/spi_ABC/start_ABC_spi.sh"
OUT_DIR="$HOME/iwABC/bash/spi_ABC/outNewSimABC_spi"
CHK_ROOT="$HOME/iwABC/bash/spi_ABC/newSimABC_spi_firstTen"
PARAM_SET_LIST_RDS="$HOME/iwABCdata/param_set_list.rds"

# ------------------ ARGUMENTS ------------------
# Usage: sbatch start_ABC_spi_resume.sh <lac> <mu> <K> <gam> <laa> <ss_set>
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
module load R-bundle-CRAN/2023.12-foss-2023a

echo "[MASTER] Starting master submission controller (submit all at once)"
echo "[MASTER] Runner: ${RUNNER}"
echo "[MASTER] Output dir: ${OUT_DIR}"
echo "[MASTER] Checkpoint root: ${CHK_ROOT}"
echo "[MASTER] param_set_list: ${PARAM_SET_LIST_RDS}"
echo "[MASTER] Args: lac=${idparsopt_lac} mu=${idparsopt_mu} K=${idparsopt_K} gam=${idparsopt_gam} laa=${idparsopt_laa} ss_set=${ss_set}"
echo

[[ -f "${PARAM_SET_LIST_RDS}" ]] || { echo "ERROR: missing ${PARAM_SET_LIST_RDS}" >&2; exit 1; }
[[ -f "${RUNNER}" ]] || { echo "ERROR: missing ${RUNNER}" >&2; exit 1; }

# ------------------ READ param_set_list ------------------
param_set_list=$(
  Rscript -e 'args<-commandArgs(TRUE); x<-readRDS(args[1]); cat(as.integer(x), sep=" ")' \
  "${PARAM_SET_LIST_RDS}"
)

if [ -z "${param_set_list}" ]; then
  echo "ERROR: param_set_list is empty (read from ${PARAM_SET_LIST_RDS})" >&2
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
  [[ -d "$d" ]] && compgen -G "${d}/chk_spi_set${n4}_iter*.rds" > /dev/null
}

# ------------------ MAIN LOOP (NO WAITING) ------------------
total_submitted=0
total_skipped=0
total_resumed=0
total_fresh=0

for param_set in ${param_set_list}; do
  # Skip finished
  if output_exists "${param_set}" "${ss_set}"; then
    echo "[SKIP] param_set=${param_set} → final output exists"
    ((total_skipped++))
    continue
  fi

  # Resume vs fresh
  if checkpoint_exists "${param_set}"; then
    echo "[RESUME] param_set=${param_set} → checkpoint found. Submitting..."
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
    echo "[START] param_set=${param_set} → no output and no checkpoint. Submitting..."
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

echo
echo "[DONE] submitted=${total_submitted} (fresh=${total_fresh}, resume=${total_resumed}), skipped=${total_skipped}"









































