#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck disable=SC1091
source "${SCRIPT_DIR}/launchers/load_pipeline_settings.sh"

BASE_DIR="${PROJECT_ROOT}"
OUT_DIR="${OUT_OPTIMIZE_DIR}"
AUTO_CONFIRM="${PIPELINE_NONINTERACTIVE:-0}"

for arg in "$@"; do
  case "$arg" in
    --yes|--auto-confirm)
      AUTO_CONFIRM="1"
      ;;
  esac
done

# ?? Start-of-script reminder ????????????????????????????????????????????????????
cat <<'REMINDER'

???????????????????????????????????????????????????????????????????????????????
REMINDER: Check your R parameters before running
File: CombineFiles_GetValGroups.R and config/pipeline_settings.env

Verify and, if needed, edit this function's parameters so they match your data:

select_quadrants <- function(df,
                             confluence_col = "confluence",
                             neurites_col  = "neurites_percent",
                             image_name_col = "image_name",
                             target_n = 15,
                             hi_start = 0.95,   # starting "high" quantile
                             lo_start = 0.05,   # starting "low" quantile
                             relax_step = 0.01, # how much to relax per step
                             min_hi = 0.65,     # don't relax beyond these
                             max_lo = 0.35,
                             seed = 42) {

# Make sure column names and thresholds reflect your dataset.
# Also confirm the shared pipeline settings file is correct before continuing.
???????????????????????????????????????????????????????????????????????????????
REMINDER

if [[ "$AUTO_CONFIRM" != "1" ]]; then
  read -rp "Have you reviewed & saved CombineFiles_GetValGroups.R? Continue (y/N): " _ans
  case "${_ans:-N}" in
    [yY]|[yY][eE][sS]) ;;
    *) echo "Aborting. Please edit and save the R file, then re-run this script."; exit 1 ;;
  esac
else
  echo "[INFO] Noninteractive mode enabled. Continuing without prompt."
fi

if [[ ! -f "$SAMPLE_METADATA_PATH" ]]; then
  echo "[ERROR] Sample metadata file not found: $SAMPLE_METADATA_PATH"
  echo
  echo "How to fix this:"
  echo "  - If using the Zenodo dataset, unzip it inside pipeline_inputs/ and use pipeline_inputs/metadata/IN_sample_metadata_zenodo_paths.csv."
  echo "  - Or copy that file to pipeline_inputs/IN_sample_metadata.csv."
  echo "  - If using your own data, provide a CSV with an image_name column matching the path-flattened image names."
  echo "    Example: A/B/C/img01.tif becomes A-B-C-img01.tif."
  exit 1
fi

if [[ ! -f "$MASK_PERCENTAGES_PATH" ]]; then
  echo "[ERROR] Mask percentages file not found: $MASK_PERCENTAGES_PATH"
  exit 1
fi

echo "[INFO] Running R script headless..."
Rscript "${PROJECT_ROOT}/CombineFiles_GetValGroups.R" "${BASE_DIR}"

CSV_OUT="${OPTIM_GROUPS_PATH}"
if [[ ! -f "$CSV_OUT" ]]; then
  echo "[ERROR] Expected CSV not found: $CSV_OUT"
  exit 1
fi

echo "[INFO] Creating group folders and soft links from: $CSV_OUT"

# Folder names with spaces (use ' - ' instead of '/' since '/' is not allowed in names)
declare -A GROUP_COL_TO_DIR=(
  ["grp_high_neurites_low_confluence"]="high_neurites_low_confluence"
  ["grp_low_neurites_high_confluence"]="low_neurites_high_confluence"
  ["grp_high_neurites_high_confluence"]="high_neurites_high_confluence"
  ["grp_low_neurites_low_confluence"]="low_neurites_low_confluence"
)

mkdir -p "$OUT_DIR"

# Helper: get column index by header name (1-based)
get_col_index() {
  local colname="$1"
  awk -v target="$colname" -F',' '
    NR==1{
      for(i=1;i<=NF;i++){
        gsub(/^"|"$/,"",$i);     # strip quotes
        gsub(/\r$/,"",$i);       # strip trailing CR if CRLF
        if($i==target){print i; exit}
      }
    }' "$CSV_OUT"
}

# Resolve indices
IDX_DIR=$(get_col_index "dir")
IDX_NAME=$(get_col_index "image_name")
if [[ -z "${IDX_DIR:-}" || -z "${IDX_NAME:-}" ]]; then
  echo "[ERROR] Could not locate 'dir' or 'image_name' columns in $CSV_OUT"
  exit 1
fi

# Iterate over our four group columns
for COL in "${!GROUP_COL_TO_DIR[@]}"; do
  IDX_COL=$(get_col_index "$COL")
  if [[ -z "${IDX_COL:-}" ]]; then
    echo "[WARN] Column $COL not found in CSV ? skipping."
    continue
  fi
  DEST_DIR="${OUT_DIR}/${GROUP_COL_TO_DIR[$COL]}"
  mkdir -p "$DEST_DIR"

  echo "[INFO] Linking images for group: $COL -> $DEST_DIR"

  # Extract rows where indicator column == 1 and print "dir<TAB>image_name"
  awk -v c_dir="$IDX_DIR" -v c_name="$IDX_NAME" -v c_flag="$IDX_COL" -F',' '
    NR>1 {
      d=$c_dir; n=$c_name; f=$c_flag;
      # crude de-quoting for simple CSV (assumes no embedded commas in these fields)
      gsub(/^"|"$/, "", d); gsub(/^"|"$/, "", n); gsub(/^"|"$/, "", f);
      gsub(/\r$/, "", d);  gsub(/\r$/, "", n);  gsub(/\r$/, "", f);  # strip CR if present
      if (f=="1") printf("%s\t%s\n", d, n);
    }' "$CSV_OUT" | while IFS=$'\t' read -r dir_path link_name; do
      # trim any trailing CRs from read variables (belt-and-suspenders)
      dir_path="${dir_path%$'\r'}"
      link_name="${link_name%$'\r'}"

      [[ -z "$dir_path" || -z "$link_name" ]] && continue

      # Build full source path = dir + "/" + image_name (avoid double slashes)
      src_path="${dir_path%/}/${link_name}"

      if [[ ! -f "$src_path" ]]; then
        fallback_src="$(find "$PRE_RENAMED_DIR" -type f -name "$link_name" -print -quit 2>/dev/null || true)"
        if [[ -n "$fallback_src" && -f "$fallback_src" ]]; then
          echo "[WARN] Source path from metadata is stale; using current PRE_renamed match instead: $fallback_src"
          src_path="$fallback_src"
        else
          echo "[WARN] Source file not found in metadata dir or current PRE_renamed, skipping: $src_path"
          continue
        fi
      fi
      src_path="$(realpath "$src_path")"
      dest_path="${DEST_DIR}/${link_name}"
      rm -f "$dest_path"
      if ln "$src_path" "$dest_path" 2>/dev/null; then
        :
      else
        echo "[WARN] Hard link failed; copying image for Windows/R preview compatibility: $dest_path"
        cp -p "$src_path" "$dest_path"
      fi
    done
done

echo "[INFO] Done. Linked images are in: $OUT_DIR"

# ?? End-of-script curation reminders ??????????????????????????????????????????
cat <<ENDNOTE

???????????????????????????????????????????????????????????????????????????????
NEXT STEPS: Curate selections and proceed with optimization

1) In the optimization workspace folder, there are the subfolders:
     "high_neurites_low_confluence",
     "low_neurites_high_confluence",
     "high_neurites_high_confluence",
     "low_neurites_low_confluence"

2) In each group's subfolder, create (by hand) two subfolders:
       choice/   and   noise/
   - Move images with clearly valid (true) neurite & confluence levels into: choice
   - Move images where the neurites % appears to be mere noise into: noise

   Example:
     mkdir -p "${OUT_DIR}/high_neurites_low_confluence/choice" \
              "${OUT_DIR}/high_neurites_low_confluence/noise"

3) After visual curation, run the next script to continue optimization,
   e.g.:
     ./6.SelectedValidationImgWriteSee.sh

???????????????????????????????????????????????????????????????????????????????
ENDNOTE
