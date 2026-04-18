#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck disable=SC1091
source "${SCRIPT_DIR}/launchers/load_pipeline_settings.sh"

BASE_DIR="${OUTPUT_ROOT_DIR}"
MACRO="${PROJECT_ROOT}/saveROIs.ijm"
ROI_ROOT="${OUT_OPTIMIZE_DIR}/${OUT_ROI_DIR_NAME}"
ROI_DIAG_CSV="${OUT_OPTIMIZE_DIR}/roi_extraction_diagnostics.csv"
ROI_DIAG_LOG="${OUT_OPTIMIZE_DIR}/roi_extraction_detailed_log.txt"

if [[ ! -x "$FIJI_BIN" ]]; then
  echo "[ERROR] Fiji executable not found or not executable at: $FIJI_BIN"
  exit 1
fi

if [[ ! -f "$MACRO" ]]; then
  echo "[ERROR] Macro file not found: $MACRO"
  exit 1
fi

if [[ -n "${TMPDIR:-}" && -d "${TMPDIR:-}" ]]; then
  find "$TMPDIR" -maxdepth 1 -type f -name 'ImageJ-*.stub' -delete 2>/dev/null || true
fi
find /tmp -maxdepth 1 -type f -name 'ImageJ-*.stub' -delete 2>/dev/null || true

export _JAVA_AWT_WM_NONREPARENTING=1

MACRO_ARG="${BASE_DIR}|${SEG_LABEL_NEURITES:-3}"

echo "[INFO] Using neurite segmentation label for ROI extraction: ${SEG_LABEL_NEURITES:-3}"

"$FIJI_BIN" -batch "$MACRO" "$MACRO_ARG"

if [[ -d "$ROI_ROOT" ]]; then
  find "$ROI_ROOT" -mindepth 1 -type d -empty -delete 2>/dev/null || true
fi

roi_file_count=0
roi_zip_count=0
if [[ -d "$ROI_ROOT" ]]; then
  roi_file_count=$(find "$ROI_ROOT" -type f -iname '*.roi' | wc -l | tr -d ' ')
  roi_zip_count=$(find "$ROI_ROOT" -type f -iname '*_ROIs.zip' | wc -l | tr -d ' ')
fi

if [[ "$roi_file_count" -eq 0 && "$roi_zip_count" -eq 0 ]]; then
  echo "[ERROR] ROI extraction completed without producing any .roi or _ROIs.zip outputs."
  echo "[ERROR] Check whether the segmentation masks contain neurite-class pixels and whether the ROI extraction macro is targeting the correct label values."
  echo "[ERROR] Detailed macro log: $ROI_DIAG_LOG"
  echo "[ERROR] Detailed diagnostics table: $ROI_DIAG_CSV"
  exit 1
fi

echo "[INFO] ROI extraction created ${roi_file_count} .roi files and ${roi_zip_count} ROI zip bundles."
echo "[INFO] Detailed macro log: $ROI_DIAG_LOG"
echo "[INFO] Detailed diagnostics table: $ROI_DIAG_CSV"
