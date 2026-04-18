#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck disable=SC1091
source "${SCRIPT_DIR}/launchers/load_pipeline_settings.sh"

default_path="${PRE_RENAMED_DIR}"
base_path="${1:-}"
append_only="${PIPELINE_APPEND_ONLY:-0}"
paths_file="${PIPELINE_PATHS_FILE:-}"
output_list_path="${PIPELINE_IMAGE_INPUT_LIST_PATH:-$IMAGE_INPUT_LIST}"

if [[ -z "$base_path" ]]; then
  read -r -p "Enter the directory path to process [${default_path}]: " base_path
  base_path="${base_path:-$default_path}"
fi

if [[ "$append_only" == "1" && -n "$paths_file" ]]; then
  mkdir -p "$(dirname "$output_list_path")"
  touch "$output_list_path"
  while IFS= read -r img_path; do
    [[ -z "$img_path" ]] && continue
    real_img="$(realpath "$img_path")"
    grep -Fqx "$real_img" "$output_list_path" || printf '%s\n' "$real_img" >> "$output_list_path"
  done < "$paths_file"
  echo "Appended missing image paths from '$paths_file' into $output_list_path"
  exit 0
fi

if [[ ! -d "$base_path" ]]; then
  echo "Invalid directory: $base_path"
  exit 1
fi

mkdir -p "$(dirname "$output_list_path")"
find "$base_path" -type f \( -iname "*.jpg" -o -iname "*.jpeg" -o -iname "*.png" -o -iname "*.tif" -o -iname "*.tiff" \) -exec realpath {} \; | sort > "$output_list_path"

echo "Saved list of all images in '$base_path' to $output_list_path"
