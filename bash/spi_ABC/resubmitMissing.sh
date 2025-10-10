#!/bin/bash
#SBATCH --time=0:29:30
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=resubmit_msing_1to500
#SBATCH --output=logsMsing1to500/resubmit_msing_1to500-%j.log
#SBATCH --mem=30M
#SBATCH --partition=regular

set -euo pipefail
mkdir -p logsMsing1to500

# --- CONFIGURATION ---
OUTDIR=~/iwABC/bash/spi_ABC/newSimABC_spi_firstTen
SCRIPT=~/iwABC/bash/spi_ABC/start_ABC_spi.sh

START_ID=1
END_ID=100

# Parameter values
idparsopt_lac="$1"
idparsopt_mu="$2"
idparsopt_K="$3"
idparsopt_gam="$4"
idparsopt_laa="$5"
ss_set="$6"
# ----------------------

echo "[Resubmit] Checking param_set ${START_ID}-${END_ID} in ${OUTDIR} ..."
missing_count=0

for (( param_set=${START_ID}; param_set<=${END_ID}; param_set++ )); do
  dir_name=$(printf "checkpoints_spi_set_%04d" "$param_set")
  full_path="${OUTDIR}/${dir_name}"

  if [ -d "$full_path" ]; then
    # Exists — skip
    continue
  else
    echo "❌ Missing: ${dir_name} → submitting job..."
    sbatch "$SCRIPT" \
           "${param_set}" \
           "${idparsopt_lac}" \
           "${idparsopt_mu}" \
           "${idparsopt_K}" \
           "${idparsopt_gam}" \
           "${idparsopt_laa}" \
           "${ss_set}"
    ((missing_count++))
  fi
done

echo "✅ Finished checking param_set ${START_ID}-${END_ID}."
echo "Total resubmitted: ${missing_count}"





