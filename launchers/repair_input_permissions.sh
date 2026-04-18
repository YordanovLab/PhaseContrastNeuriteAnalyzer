#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck disable=SC1091
source "${SCRIPT_DIR}/load_pipeline_settings.sh"

TARGET_DIR="${1:-${INPUT_MASTER_DIR:-}}"
if [[ -z "$TARGET_DIR" || ! -d "$TARGET_DIR" ]]; then
  echo "[ERROR] Input directory does not exist or was not provided: ${TARGET_DIR}" >&2
  exit 1
fi

echo "[INFO] Repairing readable/traversable permissions under: ${TARGET_DIR}"
echo "[INFO] This only targets the selected input tree. It does not modify scripts, cache, or pipeline_outputs."

echo "[INFO] Ensuring directories have owner read/write/search permissions (u+rwx)."
find "$TARGET_DIR" -type d -exec chmod u+rwx {} +

echo "[INFO] Ensuring files have owner read permission (u+r)."
find "$TARGET_DIR" -type f -exec chmod u+r {} +

echo "[INFO] Permission repair completed."
