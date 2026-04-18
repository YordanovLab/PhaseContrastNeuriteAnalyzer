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
TARGET_DIR="$(realpath -m "$TARGET_DIR")"

echo "[INFO] Repairing readable/traversable permissions under: ${TARGET_DIR}"
echo "[INFO] This only targets the selected input tree. It does not modify scripts, cache, or pipeline_outputs."

echo "[INFO] Ensuring the input root itself is readable and traversable."
chmod u+rwx "$TARGET_DIR"

echo "[INFO] Repairing directories/files in repeated passes so deeper folders become reachable."
for pass in $(seq 1 20); do
  before_denied="$(find "$TARGET_DIR" -type d -print >/dev/null 2>&1; printf "%s" "$?")"

  find "$TARGET_DIR" -type d -exec chmod u+rwx {} + 2>/dev/null || true
  find "$TARGET_DIR" -type f -exec chmod u+rw {} + 2>/dev/null || true

  denied_log="$(mktemp)"
  find "$TARGET_DIR" -type d -print >/dev/null 2>"$denied_log" || true
  denied_count="$(grep -ci "Permission denied" "$denied_log" || true)"
  rm -f "$denied_log"

  echo "[INFO] Permission repair pass ${pass}: remaining inaccessible directory messages = ${denied_count}"
  if [[ "$denied_count" -eq 0 && "$before_denied" -eq 0 ]]; then
    break
  fi
done

final_denied_log="$(mktemp)"
find "$TARGET_DIR" -type d -print >/dev/null 2>"$final_denied_log" || true
final_denied_count="$(grep -ci "Permission denied" "$final_denied_log" || true)"
if [[ "$final_denied_count" -gt 0 ]]; then
  echo "[ERROR] Some input folders are still inaccessible after automatic repair." >&2
  echo "[ERROR] This can happen if files are owned by another user or the filesystem/mount blocks chmod." >&2
  echo "[ERROR] Examples:" >&2
  grep -i "Permission denied" "$final_denied_log" | head -20 >&2 || true
  rm -f "$final_denied_log"
  exit 1
fi
rm -f "$final_denied_log"

echo "[INFO] Permission repair completed."
