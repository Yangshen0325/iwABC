#!/bin/bash
#SBATCH --time=3-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1      #
#SBATCH --job-name=tj_run_ABC_spi
#SBATCH --output=logsFirstTen/tj_ABC_spi-%j.log
#SBATCH --mem=30G
#SBATCH --partition=regular

set -euo pipefail

# Ensure log directory exists (avoids 'No such file or directory' on some systems)
mkdir -p logsFirstTen

# Check number of arguments
if [ $# -ne 7 ]; then
  echo "Usage: sbatch start_ABC_spi_resume.sh <param_set> <lac> <mu> <K> <gam> <laa> <ss_set>"
  exit 1
fi

param_set=$1
idparsopt_lac=$2
idparsopt_mu=$3
idparsopt_K=$4
idparsopt_gam=$5
idparsopt_laa=$6
ss_set=$7

module load R-bundle-CRAN/2023.12-foss-2023a

# ---- get param_set_list from RDS ----
echo "Reading param_set_list from ~/iwABCdata/param_set_list.rds ..."

param_set_list=$(Rscript -e 'x <- readRDS("~/iwABCdata/param_set_list.rds"); cat(x, sep = " ")')

if [ -z "${param_set_list}" ]; then
  echo "ERROR: param_set_list is empty or could not be read."
  exit 1
fi

echo "Will run the following param_set values:"
echo "${param_set_list}"
echo

# ---- loop over all param_set values ----
for param_set in ${param_set_list}; do
  echo "=========================================================="
  echo ">>> Starting param_set = ${param_set}"
  echo "    lac=${idparsopt_lac}, mu=${idparsopt_mu}, K=${idparsopt_K}, gam=${idparsopt_gam}, laa=${idparsopt_laa}, ss_set=${ss_set}"
  echo "----------------------------------------------------------"

  Rscript ~/iwABC/script/job_run_ABC_spi.R "${param_set}" \
                                           "${idparsopt_lac}" \
                                           "${idparsopt_mu}" \
                                           "${idparsopt_K}" \
                                           "${idparsopt_gam}" \
                                           "${idparsopt_laa}" \
                                           "${ss_set}"

  echo "<<< Finished param_set = ${param_set}"
  echo
done

echo "All param_set runs from param_set_list completed."

