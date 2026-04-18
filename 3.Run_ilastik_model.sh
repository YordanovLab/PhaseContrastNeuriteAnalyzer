#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck disable=SC1091
source "${SCRIPT_DIR}/launchers/load_pipeline_settings.sh"

start_time=$(date +%s)

PROJECT_FILE="${ILASTIK_PROJECT_PATH}"
INPUT_DIR="${PIPELINE_INPUT_DIR_OVERRIDE:-$OUT_CLEAR_BCGND_DIR}"
OUTPUT_DIR="${PIPELINE_OUTPUT_DIR_OVERRIDE:-$OUT_SEGMENTATION_DIR}"
MAX_PROCESSES="${ILASTIK_MAX_PROCESSES}"
APPLY_RENORM="${APPLY_ILASTIK_RENORM}"

if ! [[ "$MAX_PROCESSES" =~ ^[0-9]+$ ]] || (( MAX_PROCESSES < 1 )); then
  echo "Invalid ILASTIK_MAX_PROCESSES value: $MAX_PROCESSES"
  echo "Set ILASTIK_MAX_PROCESSES to a positive integer in config/pipeline_settings.env."
  exit 1
fi

if [[ ! -x "$ILASTIK_BIN" ]]; then
  echo "Ilastik executable not found or not executable at: $ILASTIK_BIN"
  exit 1
fi

if [[ ! -f "$PROJECT_FILE" ]]; then
  echo "Ilastik project file not found: $PROJECT_FILE"
  echo
  echo "How to fix this:"
  echo "  - If using the Zenodo dataset, unzip it directly inside pipeline_inputs/ so the model is at pipeline_inputs/ilastik/model/*.ilp."
  echo "  - If using your own model, put the trained ilastik Pixel Classification .ilp file in models/ or pipeline_inputs/ilastik/model/."
  echo "  - Or set ILASTIK_PROJECT_FILE in config/pipeline_settings.env to the exact .ilp path."
  exit 1
fi

if [[ ! -d "$INPUT_DIR" ]]; then
  echo "Input directory not found: $INPUT_DIR"
  echo
  echo "How to fix this:"
  echo "  - Run the setup/preprocessing steps first so OUT_clear_bcgnd exists."
  echo "  - If starting from Zenodo data, unzip the archive inside pipeline_inputs/ and run the setup module from step 2.A.1."
  echo "  - Do not use raw images directly for ilastik segmentation in this script; it expects Fiji-preprocessed *_RGavg.tif images."
  exit 1
fi

mkdir -p "$OUTPUT_DIR"

RENORM_TOOL=""
if [[ "$APPLY_RENORM" == true ]]; then
  if command -v magick >/dev/null 2>&1; then
    RENORM_TOOL="magick"
  elif command -v convert >/dev/null 2>&1; then
    RENORM_TOOL="convert"
  else
    echo "ImageMagick was not found, but APPLY_ILASTIK_RENORM=true."
    echo
    echo "How to fix this:"
    echo "  - On Ubuntu/Debian/WSL: sudo apt update && sudo apt install imagemagick"
    echo "  - On Fedora: sudo dnf install ImageMagick"
    echo "  - On Arch: sudo pacman -S imagemagick"
    echo "  - Or set APPLY_ILASTIK_RENORM=false only if downstream steps are configured for the raw ilastik mask labels."
    exit 1
  fi
fi

wait_for_jobs() {
    while (( $(jobs -rp | wc -l) >= MAX_PROCESSES )); do
        sleep 1
    done
}

find "$INPUT_DIR" -type f -iname "*.tif" ! -iname "*Simple Segmentation*" | while read -r input_image; do
    wait_for_jobs

    (
        base_name=$(basename "$input_image" .tif)
        output_mask="${OUTPUT_DIR}/${base_name}_mask.tif"
        output_final="${OUTPUT_DIR}/${base_name}_mask_renorm.tif"

        echo "?? Running Ilastik on: $base_name"

        "$ILASTIK_BIN" \
          --headless \
          --project="$PROJECT_FILE" \
          --export_source="Simple Segmentation" \
          --output_filename_format="$output_mask" \
          --output_format="tif" \
          "$input_image"

        if [[ "$APPLY_RENORM" == true && -f "$output_mask" ]]; then
            echo "?? Renormalizing: $output_mask"
            "$RENORM_TOOL" "$output_mask" -evaluate multiply 2.0 -evaluate add 254 "$output_final"
            rm -f "$output_mask"
        fi

        echo "? Done: $base_name"

    ) &
done

wait

end_time=$(date +%s)
duration=$((end_time - start_time))

minutes=$((duration / 60))
seconds=$((duration % 60))

echo "?? All segmentations completed using $INPUT_DIR as input."
echo "?? Total processing time: ${minutes} minutes and ${seconds} seconds."
