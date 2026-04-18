#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck disable=SC1091
source "${SCRIPT_DIR}/launchers/load_pipeline_settings.sh"

BASE_DIR="${PRE_RENAMED_DIR}"
SEARCH_ROOT="${1:-${INPUT_MASTER_DIR:-$PROJECT_ROOT}}"
OUTPUT_ROOT_CANON="$(realpath -m "$OUTPUT_ROOT_DIR")"
BASE_DIR_CANON="$(realpath -m "$BASE_DIR")"
PATHS_FILE="${PIPELINE_PATHS_FILE:-}"

mkdir -p "$BASE_DIR"

check_input_permissions() {
  local bad_dirs=""
  local bad_files=""

  bad_dirs="$(find "$SEARCH_ROOT" -type d ! -executable -print 2>/dev/null | head -20 || true)"
  bad_files="$(find "$SEARCH_ROOT" -type f \( -iname "*.tif" -o -iname "*.tiff" -o -iname "*.png" -o -iname "*.jpg" -o -iname "*.jpeg" \) ! -readable -print 2>/dev/null | head -20 || true)"

  if [[ -n "$bad_dirs" || -n "$bad_files" ]]; then
    echo "[ERROR] Input permission problem detected under: $SEARCH_ROOT" >&2
    echo "[ERROR] Some folders/files cannot be traversed or read, so the pipeline may silently find only part of the image dataset." >&2
    if [[ -n "$bad_dirs" ]]; then
      echo "[ERROR] Example untraversable folders:" >&2
      echo "$bad_dirs" >&2
    fi
    if [[ -n "$bad_files" ]]; then
      echo "[ERROR] Example unreadable image files:" >&2
      echo "$bad_files" >&2
    fi
    echo "[ERROR] In the browser app, click 'Repair Input Folder Permissions'." >&2
    echo "[ERROR] Terminal fallback: bash ./launchers/repair_input_permissions.sh '$SEARCH_ROOT'" >&2
    exit 1
  fi
}

check_input_permissions

iter_source_files() {
  if [[ -n "$PATHS_FILE" && -f "$PATHS_FILE" ]]; then
    cat "$PATHS_FILE"
  else
    find "$SEARCH_ROOT" \
      \( -path "$BASE_DIR_CANON" -o -path "$OUTPUT_ROOT_CANON" \) -prune -o \
      \( -path "*/ilastik/*" -o -path "*/software_reference/*" -o -path "*/example_expected_outputs/*" -o -path "*/training_images_raw/*" -o -path "*/training_images_preprocessed/*" \) -prune -o \
      -type f \
      \( -iname "*.tif" -o -iname "*.tiff" -o -iname "*.png" -o -iname "*.jpg" -o -iname "*.jpeg" \) \
      ! -iname "*_RGavg.*" \
      ! -iname "*_mask.*" \
      ! -iname "*_mask_renorm.*" \
      -print
  fi
}

declare -A SEEN_FILE_IDS=()

iter_source_files | while read -r filepath; do
    [[ -z "$filepath" ]] && continue
    canon_filepath="$(realpath -m "$filepath")"

    # Skip anything already under the generated-output tree, even if the caller
    # accidentally points SEARCH_ROOT too broadly.
    if [[ "$canon_filepath" == "$OUTPUT_ROOT_CANON" || "$canon_filepath" == "$OUTPUT_ROOT_CANON/"* ]]; then
        continue
    fi

    # Deduplicate by underlying file identity so reruns do not re-import
    # previously generated hard links as if they were fresh source images.
    file_id="$(stat -Lc '%d:%i' "$filepath" 2>/dev/null || true)"
    if [[ -n "$file_id" ]]; then
        if [[ -n "${SEEN_FILE_IDS[$file_id]:-}" ]]; then
            continue
        fi
        SEEN_FILE_IDS[$file_id]=1
    fi

    # Remove leading ./ for clean relative path
    relpath="${filepath#${SEARCH_ROOT}/}"

    # Directory containing the file
    filedir="$(dirname "$relpath")"

    # Original filename
    filename="$(basename "$relpath")"

    # Replace '/' with '-' in relative path (excluding filename)
    pathpart="${filedir//\//-}"

    # Construct new filename: pathpart-filename.tif
    if [[ -n "$pathpart" && "$pathpart" != "." ]]; then
        newname="${pathpart}-${filename}"
    else
        newname="${filename}"
    fi

    # Create mirrored directory inside PRE_renamed
    mkdir -p "$BASE_DIR/$filedir"

    # Create or refresh the hard link with the stable renamed output path.
    # If the input and output folders are on different filesystems, hard links
    # can fail; in that case, fall back to a normal copy so WSL/native Linux
    # users can still run the workflow with external input drives.
    dest_path="$BASE_DIR/$filedir/$newname"
    if ! ln -f "$filepath" "$dest_path" 2>/dev/null; then
        echo "[WARN] Could not hard-link across filesystems, copying instead: $filepath"
        cp -p "$filepath" "$dest_path"
    fi
done
