#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck disable=SC1091
source "${SCRIPT_DIR}/load_pipeline_settings.sh"

PROFILE_NAME="${1:-}"
if [[ -z "$PROFILE_NAME" ]]; then
  echo "Usage: ./launchers/apply_optimization_profile.sh <profile_name>"
  exit 1
fi

PRIMARY_PROFILE_ROOT="${VALIDATION_PROFILE_DIR:-${PROJECT_ROOT}/cache/validation_profiles}"
LEGACY_PROFILE_ROOT="${PROJECT_ROOT}/cache/optimization_profiles"
PROFILE_DIR="${PRIMARY_PROFILE_ROOT}/${PROFILE_NAME}"

if [[ ! -d "$PROFILE_DIR" && -d "${LEGACY_PROFILE_ROOT}/${PROFILE_NAME}" ]]; then
  PROFILE_DIR="${LEGACY_PROFILE_ROOT}/${PROFILE_NAME}"
fi

if [[ ! -d "$PROFILE_DIR" ]]; then
  echo "Profile not found in:"
  echo "  ${PRIMARY_PROFILE_ROOT}/${PROFILE_NAME}"
  echo "  ${LEGACY_PROFILE_ROOT}/${PROFILE_NAME}"
  exit 1
fi

mkdir -p "${OUT_OPTIMIZE_DIR}"

find "$PROFILE_DIR" -maxdepth 1 -type f ! -name "profile_info.txt" -exec cp -f {} "${OUT_OPTIMIZE_DIR}/" \;

echo "Applied validation cutoff profile from: ${PROFILE_DIR}"
