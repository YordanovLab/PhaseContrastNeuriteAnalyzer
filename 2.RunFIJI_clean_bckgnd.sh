#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck disable=SC1091
source "${SCRIPT_DIR}/launchers/load_pipeline_settings.sh"

FIJI="${FIJI_BIN}"
MACRO="${SCRIPT_DIR}/Macro_1-clear_bckgnd.ijm"
BASE_DIR="${OUTPUT_ROOT_DIR}/"
IMAGE_LIST_FILE="${PIPELINE_IMAGE_LIST_FILE:-$IMAGE_INPUT_LIST}"
OUT_CLEAR_DIR="${PIPELINE_OUT_CLEAR_DIR:-$OUT_CLEAR_BCGND_DIR}"

if [[ ! -x "$FIJI" ]]; then
  echo "Fiji executable not found or not executable at: $FIJI"
  exit 1
fi

if [[ ! -f "$MACRO" ]]; then
  echo "Macro file $MACRO not found!"
  exit 1
fi

if [[ ! -f "$IMAGE_LIST_FILE" ]]; then
  echo "Image input list not found: $IMAGE_LIST_FILE"
  echo "Run 1.get_img_list.sh first."
  exit 1
fi

if [[ -n "${TMPDIR:-}" && -d "${TMPDIR:-}" ]]; then
  find "$TMPDIR" -maxdepth 1 -type f -name 'ImageJ-*.stub' -delete 2>/dev/null || true
fi
find /tmp -maxdepth 1 -type f -name 'ImageJ-*.stub' -delete 2>/dev/null || true

export _JAVA_AWT_WM_NONREPARENTING=1

"$FIJI" --console -macro "$MACRO" "${BASE_DIR}||${IMAGE_LIST_FILE}||${OUT_CLEAR_DIR}"

echo "Batch macro execution finished. Results saved in $OUT_CLEAR_DIR"
