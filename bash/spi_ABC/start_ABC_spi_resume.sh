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

# ---- paths (use absolute paths; avoid ~ surprises) ----
START_SCRIPT="$HOME/iwABC/bash/spi_ABC/start_ABC_spi.sh"
PARAM_SET_LIST_RDS="$HOME/iwABCdata/param_set_list.rds"

# ---- args: lac mu K gam laa ss_set ----
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

# ---- checks ----
[[ -f "$PARAM_SET_LIST_RDS" ]] || { echo "ERROR: missing $PARAM_SET_LIST_RDS" >&2; exit 1; }
[[ -f "$START_SCRIPT" ]] || { echo "ERROR: missing $START_SCRIPT" >&2; exit 1; }

module load R-bundle-CRAN/2023.12-foss-2023a

echo "[MASTER] start script: $START_SCRIPT"
echo "[MASTER] param_set_list: $PARAM_SET_LIST_RDS"
echo "[MASTER] args: lac=$lac mu=$mu K=$K gam=$gam laa=$laa ss_set=$ss_set"
echo

# ------------------ PREP ------------------
mkdir -p logsMaster

# ---- read param_set_list from RDS ----
param_set_list=$(
  Rscript -e 'args <- commandArgs(TRUE); x <- readRDS(args[1]); cat(as.integer(x), sep=" ")' \
  "$PARAM_SET_LIST_RDS"
)

if [ -z "$param_set_list" ]; then
  echo "ERROR: param_set_list is empty (read from $PARAM_SET_LIST_RDS)" >&2
  exit 1
fi

echo "[MASTER] will submit $(wc -w <<< "$param_set_list") jobs."
echo

# ---- submit one job per param_set ----
submitted=0
for param_set in $param_set_list; do
  echo "[MASTER] submitting param_set=$param_set"
  sbatch "$START_SCRIPT" "$param_set" "$idparsopt_lac" "$idparsopt_mu" "$idparsopt_K" "$idparsopt_gam" "$idparsopt_laa" "$ss_set"
  ((submitted++))
done

echo
echo "[MASTER] done. submitted=$submitted"
