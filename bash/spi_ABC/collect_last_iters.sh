#!/bin/bash

# Base directory where all checkpoint folders live
BASE_DIR=~/iwABC/newSimABC_spi_firstTen

# Output directory to store the last-iteration RDS files
OUT_DIR="$BASE_DIR/last_iter_results"

# Create output directory if it doesn't exist
mkdir -p "$OUT_DIR"

# Optional: CSV index of what was collected
INDEX_FILE="$OUT_DIR/last_iter_index.csv"
echo "param_set,last_iter,source_file" > "$INDEX_FILE"

# Loop over all checkpoint folders
for folder in "$BASE_DIR"/checkpoints_spi_set_*; do
    # Skip if no such folder (in case of glob issues)
    [ -d "$folder" ] || continue

    # Example folder name: checkpoints_spi_set_4701
    # Extract the 4-digit param_set string (0001, 0101, 4701, etc.)
    param_set_str=$(basename "$folder" | sed 's/checkpoints_spi_set_//')

    # List all iteration files inside that folder
    files=( "$folder"/chk_spi_set${param_set_str}_iter*.rds )

    # If there is no chk_spi_setXXXX_iter*.rds file, skip
    if [ ! -e "${files[0]}" ]; then
        echo "No iteration files found for param_set ${param_set_str}, skipping."
        continue
    fi

    # Extract iteration numbers (01–20), sort numerically, and take the largest
    last_iter_num=$(
        ls "$folder"/chk_spi_set${param_set_str}_iter*.rds \
        | sed -E 's/.*_iter([0-9]+)\.rds/\1/' \
        | sort -n \
        | tail -1
    )

    # Zero-pad iteration to 2 digits (1 -> 01)
    last_iter_padded=$(printf "%02d" "$last_iter_num")

    # Construct source file path
    src_file="$folder/chk_spi_set${param_set_str}_iter${last_iter_padded}.rds"

    # Double-check file exists
    if [ ! -f "$src_file" ]; then
        echo "Expected file not found: $src_file"
        continue
    fi

    # Copy to output folder (keep same filename)
    cp "$src_file" "$OUT_DIR/"

    # Append to index CSV
    echo "${param_set_str},${last_iter_padded},${src_file}" >> "$INDEX_FILE"

    echo "Collected param_set ${param_set_str}, last iteration ${last_iter_padded}"
done

echo "Done. Last iteration results saved to: $OUT_DIR"
echo "Index file: $INDEX_FILE"
