#!/usr/bin/env bash
# Resume all SPI runs with < 10 iterations by submitting sbatch jobs that auto-resume.
# Usage:
#   ./resume_failed_spi.sh <lac> <mu> <K> <gam> <laa> <ss_set> [--dry-run]

set -euo pipefail

if [ $# -lt 6 ]; then
  echo "Usage: $0 <lac> <mu> <K> <gam> <laa> <ss_set> [--dry-run]" >&2
  exit 1
fi

lac="$1"; mu="$2"; K="$3"; gam="$4"; laa="$5"; ss_set="$6"
dry="${7:-}"

ROOT="${HOME}/p302656/iwABC"
START_SCRIPT="${HOME}/p302656/iwABC/bash/start_ABC_spi.sh"   # <-- SPI launcher
LOG_DIR="${ROOT}/logs_resume_spi"
mkdir -p "${LOG_DIR}"

[[ -x "${START_SCRIPT}" ]] || { echo "Missing or non-executable: ${START_SCRIPT}" >&2; exit 1; }

echo "[INFO] Scanning ${ROOT} for checkpoints_spi_set_XXXX/ ..."
shopt -s nullglob

count_total=0 count_sub=0 count_skip=0
for d in "${ROOT}"/checkpoints_spi_set_*; do
  [[ -d "$d" ]] || continue
  b=$(basename "$d")
  [[ "$b" =~ checkpoints_spi_set_([0-9]+)$ ]] || { echo "[WARN] Skip: $d"; ((count_skip++)); continue; }
  param_set="${BASH_REMATCH[1]}"

  files=( "$d"/chk_spi_set"${param_set}"_iter??.rds )
  if (( ${#files[@]} == 0 )); then
    echo "[WARN] No checkpoints in $d; skipping."
    ((count_skip++)); continue
  fi

  max_iter=0
  for f in "${files[@]}"; do
    [[ "$(basename "$f")" =~ _iter([0-9]{2})\.rds$ ]] || continue
    it=$((10#${BASH_REMATCH[1]}))
    (( it > max_iter )) && max_iter=$it
  done

  ((count_total++))
  if (( max_iter >= 10 )); then
    echo "[OK]   set=${param_set} already at iter ${max_iter}; skip."
    ((count_skip++)); continue
  fi

  echo "[RESUME] set=${param_set} latest_iter=${max_iter} → sbatch start_ABC_spi.sh ..."
  if [[ "$dry" == "--dry-run" ]]; then
    echo "  sbatch ${START_SCRIPT} ${param_set} ${lac} ${mu} ${K} ${gam} ${laa} ${ss_set}"
  else
    sbatch "${START_SCRIPT}" "${param_set}" "${lac}" "${mu}" "${K}" "${gam}" "${laa}" "${ss_set}" \
      | tee -a "${LOG_DIR}/submitted.log"
    sleep 0.1  # small throttle to be nice to scheduler
    ((count_sub++))
  fi
done
shopt -u nullglob

echo
echo "[SUMMARY] checkpoint-dirs=${count_total} submitted=${count_sub} skipped=${count_skip}"
echo "[SUMMARY] log file: ${LOG_DIR}/submitted.log"
