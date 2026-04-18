#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck disable=SC1091
source "${SCRIPT_DIR}/launchers/load_pipeline_settings.sh"

BASE_DIR="${PROJECT_ROOT}"
OUT_DIR="${OUT_OPTIMIZE_DIR}"
R_SCRIPT="${PROJECT_ROOT}/CombinedFiles_after_inspection.R"
AUTO_CONFIRM="${PIPELINE_NONINTERACTIVE:-0}"

for arg in "$@"; do
  case "$arg" in
    --yes|--auto-confirm)
      AUTO_CONFIRM="1"
      ;;
  esac
done

# Ask user for confirmation
echo "Have you finished the visual inspection and placed files into the 'choice' and 'noise' subfolders of each group?"
if [[ "$AUTO_CONFIRM" != "1" ]]; then
  read -r -p "Continue with marking manual choices/noise and regenerating outputs? [y/N] " REPLY
  case "${REPLY,,}" in
    y|yes) echo "[INFO] Proceeding...";;
    *)     echo "[INFO] Aborted by user."; exit 0;;
  esac
else
  echo "[INFO] Noninteractive mode enabled. Proceeding without prompt."
fi

# Run R script headless
if [[ ! -f "$R_SCRIPT" ]]; then
  echo "[ERROR] R script not found at: $R_SCRIPT"
  exit 1
fi

echo "[INFO] Running R script..."
Rscript "$R_SCRIPT" "$BASE_DIR"

CSV_OUT="${OPTIM_GROUPS_PATH}"
CSV_INSPECTED="${OPTIM_GROUPS_INSPECTED_PATH}"
JPG_OUT="${OUT_DIR}/mask_area_percentages+optim_groups+inspected.jpg"

# Report results
[[ -f "$CSV_OUT"       ]] && echo "[INFO] Updated CSV: $CSV_OUT" || echo "[WARN] CSV not found: $CSV_OUT"
[[ -f "$CSV_INSPECTED" ]] && echo "[INFO] Inspected CSV: $CSV_INSPECTED" || echo "[WARN] Inspected CSV not found: $CSV_INSPECTED"
[[ -f "$JPG_OUT"       ]] && echo "[INFO] Plot: $JPG_OUT" || echo "[WARN] Plot not found: $JPG_OUT"

echo "[INFO] Done."
