#!/bin/bash
# This script checks checkpoints for each param_set and resubmits jobs if incomplete.

# Base directory containing all checkpoint folders
BASE_DIR=~/iwABC/bash/spi_ABC/newSimABC_spi_firstTen

# Loop over all checkpoint folders
for folder in "$BASE_DIR"/checkpoints_spi_set_*; do
    # Extract param_set id (e.g., 4701 from checkpoints_spi_set_4701)
    param_set=$(basename "$folder" | sed 's/checkpoints_spi_set_//')

    # Count how many iterations completed (how many .rds files exist)
    num_files=$(ls "$folder"/chk_spi_set${param_set}_iter*.rds 2>/dev/null | wc -l)

    # If < 20 iterations, then failed → need resubmit
    if [ "$num_files" -lt 20 ]; then
        # Next iteration to start from
        next_iter=$((num_files + 1))

        echo "Param_set $param_set failed at iteration $next_iter. Resubmitting..."

        # Submit job (adjust arguments as you need)
        # Example: sbatch bash/start_ABC_spi.sh <param_set> 1 2 3 4 5 0
        sbatch bash/spi_ABC/start_ABC_spi_rest.sh "$param_set" 1 2 3 4 5 1
    fi
done
