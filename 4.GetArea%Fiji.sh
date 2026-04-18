#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck disable=SC1091
source "${SCRIPT_DIR}/launchers/load_pipeline_settings.sh"

FIJI="${FIJI_BIN}"
MACRO="${SCRIPT_DIR}/Macro_2-get_mask_area_percentages.ijm"
BASE_DIR="${OUTPUT_ROOT_DIR}"

if [[ ! -x "$FIJI" ]]; then
  echo "Fiji executable not found or not executable at: $FIJI"
  exit 1
fi

if [[ ! -f "$MACRO" ]]; then
  echo "Macro file $MACRO not found!"
  exit 1
fi

if [[ ! -d "$OUT_SEGMENTATION_DIR" ]]; then
  echo "Segmentation folder not found: $OUT_SEGMENTATION_DIR"
  exit 1
fi

if [[ -n "${TMPDIR:-}" && -d "${TMPDIR:-}" ]]; then
  find "$TMPDIR" -maxdepth 1 -type f -name 'ImageJ-*.stub' -delete 2>/dev/null || true
fi
find /tmp -maxdepth 1 -type f -name 'ImageJ-*.stub' -delete 2>/dev/null || true

export _JAVA_AWT_WM_NONREPARENTING=1

MACRO_ARG="${BASE_DIR}|${SEG_LABEL_CELL_BODIES:-1}|${SEG_LABEL_NEURITES:-3}|${SEG_LABEL_BACKGROUND:-255}"

echo "Using segmentation labels for mask quantification:"
echo "  cell bodies = ${SEG_LABEL_CELL_BODIES:-1}"
echo "  neurites    = ${SEG_LABEL_NEURITES:-3}"
echo "  background  = ${SEG_LABEL_BACKGROUND:-255}"

"$FIJI" --console -macro "$MACRO" "$MACRO_ARG"

echo "Done. Results saved in: $OUT_OPTIMIZE_DIR"
