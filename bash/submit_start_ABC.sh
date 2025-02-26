#!/bin/bash
#SBATCH --time=0:29:30
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --job-name=start_ABC
#SBATCH --output=logs/start_ABC.log
#SBATCH --mem=5GB
#SBATCH --partition=regular

idparsopt_lac=${1}
idparsopt_mu=${2}
idparsopt_K=${3}
idparsopt_gam=${4}
idparsopt_laa=${5}
ss_set=${6}

for (( param_set = 1; param_set <= 2; param_set++ ))
do
  echo "Submitting job for parameter set ${param_set}..."
  sbatch ~/iwABC/bash/start_ABC.sh \
         ${param_set} \
         ${idparsopt_lac} \
         ${idparsopt_mu} \
         ${idparsopt_K} \
         ${idparsopt_gam} \
         ${idparsopt_laa} \
         ${ss_set}
done
