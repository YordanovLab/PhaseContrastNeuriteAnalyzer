#!/usr/bin/env bash
set -euo pipefail

LOAD_PIPELINE_SETTINGS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
DETECTED_PROJECT_ROOT="$(cd "${LOAD_PIPELINE_SETTINGS_DIR}/.." && pwd)"
PROJECT_ROOT="$DETECTED_PROJECT_ROOT"
SETTINGS_FILE="${PIPELINE_SETTINGS_FILE:-${PROJECT_ROOT}/config/pipeline_settings.env}"

if [[ ! -f "$SETTINGS_FILE" ]]; then
  EXAMPLE_SETTINGS_FILE="${PROJECT_ROOT}/config/pipeline_settings.example.env"
  if [[ -f "$EXAMPLE_SETTINGS_FILE" ]]; then
    echo "[WARN] Shared settings file not found: $SETTINGS_FILE" >&2
    echo "[WARN] Falling back to example settings: $EXAMPLE_SETTINGS_FILE" >&2
    echo "[WARN] For permanent local settings, copy config/pipeline_settings.example.env to config/pipeline_settings.env and edit executable paths." >&2
    SETTINGS_FILE="$EXAMPLE_SETTINGS_FILE"
  else
    echo "[ERROR] Shared settings file not found: $SETTINGS_FILE" >&2
    exit 1
  fi
fi

set -a
# shellcheck disable=SC1090
source "$SETTINGS_FILE"
set +a

CONFIG_PROJECT_ROOT="${PROJECT_ROOT:-.}"
if [[ "$CONFIG_PROJECT_ROOT" == "." ]]; then
  PROJECT_ROOT="$DETECTED_PROJECT_ROOT"
elif [[ "$CONFIG_PROJECT_ROOT" == /* ]]; then
  PROJECT_ROOT="$CONFIG_PROJECT_ROOT"
elif [[ "$CONFIG_PROJECT_ROOT" =~ ^[A-Za-z]:/ ]]; then
  if command -v wslpath >/dev/null 2>&1; then
    PROJECT_ROOT="$(wslpath -u "$CONFIG_PROJECT_ROOT")"
  else
    PROJECT_ROOT="$CONFIG_PROJECT_ROOT"
  fi
else
  PROJECT_ROOT="${DETECTED_PROJECT_ROOT}/${CONFIG_PROJECT_ROOT}"
fi

normalize_dir() {
  local value="$1"
  if [[ "$value" == /* ]]; then
    printf "%s" "$value"
  elif [[ "$value" =~ ^[A-Za-z]:/ ]]; then
    if command -v wslpath >/dev/null 2>&1; then
      wslpath -u "$value"
    else
      printf "%s" "$value"
    fi
  else
    printf "%s/%s" "$PROJECT_ROOT" "$value"
  fi
}

normalize_executable_path() {
  local value="${1:-}"
  if [[ -z "$value" ]]; then
    printf "%s" ""
  elif [[ "$value" =~ ^[A-Za-z]:[/\\] ]] && command -v wslpath >/dev/null 2>&1; then
    wslpath -u "$value"
  else
    printf "%s" "$value"
  fi
}

first_existing_file() {
  local candidate
  for candidate in "$@"; do
    if [[ -n "$candidate" && -f "$candidate" ]]; then
      printf "%s" "$candidate"
      return 0
    fi
  done
  return 1
}

first_matching_file() {
  local dir="$1"
  local pattern="$2"
  if [[ -d "$dir" ]]; then
    find "$dir" -type f -iname "$pattern" -print -quit
  fi
}

print_input_setup_guidance() {
  cat >&2 <<'GUIDANCE'

[INPUT SETUP HELP]
No usable input dataset/model was found, or a required input path is missing.

Option 1 - use the published Zenodo dataset:
  1. Download https://zenodo.org/records/19634700
  2. Unzip the archive directly inside pipeline_inputs/
  3. The fallback layout will be detected automatically:
     pipeline_inputs/raw_images/       -> raw image root
     pipeline_inputs/metadata/         -> metadata fallback
     pipeline_inputs/ilastik/model/    -> ilastik .ilp fallback

Option 2 - use your own compatible data:
  1. Put raw image folders under pipeline_inputs/ or set INPUT_MASTER_DIR in config/pipeline_settings.env.
  2. Provide a CSV metadata file and set SAMPLE_METADATA_FILE. It must contain an image_name column.
     The expected image_name format is the path-flattened rename output:
     A/B/C/img01.tif becomes A-B-C-img01.tif.
  3. Provide a trained ilastik Pixel Classification .ilp model with compatible classes
     for background, cell bodies, and neurites. Put it in models/ or pipeline_inputs/ilastik/model/,
     or set ILASTIK_PROJECT_FILE in config/pipeline_settings.env.
  4. Do not point INPUT_MASTER_DIR at generated folders such as pipeline_outputs, PRE_renamed,
     OUT_clear_bcgnd, or OUT_segmentation.

After copying files, rerun the app or save settings again in tab 1. Configuration.
GUIDANCE
}

PROJECT_ROOT="$(normalize_dir "${PROJECT_ROOT}")"
OUTPUT_ROOT_DIR="$(normalize_dir "${OUTPUT_ROOT_DIR_NAME:-pipeline_outputs}")"
INPUT_WORKSPACE_DIR="$(normalize_dir "${INPUT_WORKSPACE_DIR_NAME:-pipeline_inputs}")"
INPUT_MASTER_DIR="$(normalize_dir "${INPUT_MASTER_DIR:-pipeline_inputs}")"
if [[ "$INPUT_MASTER_DIR" == "$INPUT_WORKSPACE_DIR" && -d "${INPUT_WORKSPACE_DIR}/raw_images" ]]; then
  INPUT_MASTER_DIR="${INPUT_WORKSPACE_DIR}/raw_images"
fi
PRE_RENAMED_DIR="${OUTPUT_ROOT_DIR}/${PRE_RENAMED_DIR_NAME}"
IMAGE_INPUT_LIST="${OUTPUT_ROOT_DIR}/${IMAGE_INPUT_LIST_NAME}"
OUT_CLEAR_BCGND_DIR="${OUTPUT_ROOT_DIR}/${OUT_CLEAR_BCGND_DIR_NAME}"
OUT_SEGMENTATION_DIR="${OUTPUT_ROOT_DIR}/${OUT_SEGMENTATION_DIR_NAME}"
OUT_OPTIMIZE_DIR="${OUTPUT_ROOT_DIR}/${OUT_OPTIMIZE_DIR_NAME}"
OUT_GENERALIZED_DIR="${OUTPUT_ROOT_DIR}/OUT_generalized_filtering"
OUT_METADATA_PCA_DIR="${OUTPUT_ROOT_DIR}/OUT_metadata_PCA"
OUT_EXPERIMENTAL_DIR="${OUTPUT_ROOT_DIR}/OUT_experimental_analysis"
OUT_FINAL_DIR="${OUTPUT_ROOT_DIR}/OUT_biological_analysis_enhanced"
ILASTIK_PROJECT_PATH="$(normalize_dir "${ILASTIK_PROJECT_FILE:-models/Ilastik1_probably_working.ilp}")"
SAMPLE_METADATA_PATH="$(normalize_dir "${SAMPLE_METADATA_FILE:-pipeline_inputs/IN_sample_metadata.csv}")"
if [[ ! -f "$SAMPLE_METADATA_PATH" ]]; then
  SAMPLE_METADATA_PATH="$(first_existing_file \
    "${INPUT_WORKSPACE_DIR}/IN_sample_metadata.csv" \
    "${INPUT_WORKSPACE_DIR}/metadata/IN_sample_metadata_zenodo_paths.csv" \
    "${INPUT_WORKSPACE_DIR}/metadata/IN_sample_metadata_original.csv" \
    || true)"
fi
if [[ ! -f "$ILASTIK_PROJECT_PATH" ]]; then
  ILASTIK_PROJECT_PATH="$(first_existing_file \
    "${PROJECT_ROOT}/models/Ilastik1_probably_working.ilp" \
    "${INPUT_WORKSPACE_DIR}/ilastik/model/Ilastik1_probably_working.ilp" \
    || true)"
fi
if [[ ! -f "$ILASTIK_PROJECT_PATH" ]]; then
  ILASTIK_PROJECT_PATH="$(first_matching_file "${INPUT_WORKSPACE_DIR}/ilastik/model" "*.ilp" || true)"
fi
if [[ ! -f "$ILASTIK_PROJECT_PATH" ]]; then
  ILASTIK_PROJECT_PATH="$(first_matching_file "$INPUT_WORKSPACE_DIR" "*.ilp" || true)"
fi
if [[ ! -d "$INPUT_MASTER_DIR" || ! -f "$SAMPLE_METADATA_PATH" || ! -f "$ILASTIK_PROJECT_PATH" ]]; then
  print_input_setup_guidance
fi
MASK_PERCENTAGES_PATH="${OUT_OPTIMIZE_DIR}/${MASK_PERCENTAGES_FILE}"
OPTIM_GROUPS_PATH="${OUT_OPTIMIZE_DIR}/${OPTIM_GROUPS_FILE}"
OPTIM_GROUPS_INSPECTED_PATH="${OUT_OPTIMIZE_DIR}/${OPTIM_GROUPS_INSPECTED_FILE}"
OPT_RDS_PATH="${OUT_OPTIMIZE_DIR}/${OPT_RDS_FILE}"
VALIDATION_PROFILE_DIR="$(normalize_dir "${VALIDATION_PROFILE_DIR_NAME:-cache/validation_profiles}")"
OPTIMIZATION_SCORING_MODE="${OPTIMIZATION_SCORING_MODE:-continuity_aware}"
FIJI_BIN="$(normalize_executable_path "${FIJI_BIN:-}")"
ILASTIK_BIN="$(normalize_executable_path "${ILASTIK_BIN:-}")"
export OPTIMIZATION_SCORING_MODE
