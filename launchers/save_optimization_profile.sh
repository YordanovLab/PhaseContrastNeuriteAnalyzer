#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
# shellcheck disable=SC1091
source "${SCRIPT_DIR}/load_pipeline_settings.sh"

PROFILE_NAME="${1:-}"
if [[ -z "$PROFILE_NAME" ]]; then
  PROFILE_NAME="$(date +%F)_default"
fi

PROFILE_ROOT="${VALIDATION_PROFILE_DIR:-${PROJECT_ROOT}/cache/validation_profiles}"
PROFILE_DIR="${PROFILE_ROOT}/${PROFILE_NAME}"
mkdir -p "$PROFILE_DIR"

PROFILE_CREATED_AT="$(date -Iseconds)"
PROFILE_METADATA="${PROFILE_DIR}/profile_metadata.tsv"
PROFILE_MANIFEST="${PROFILE_DIR}/profile_file_manifest.tsv"
PROFILE_REPORT="${PROFILE_DIR}/profile_recreation_report.txt"

copy_if_present() {
  local src="$1"
  if [[ -f "$src" ]]; then
    cp -f "$src" "$PROFILE_DIR/"
  fi
}

file_bytes() {
  local src="$1"
  if [[ -f "$src" ]]; then
    stat -c "%s" "$src" 2>/dev/null || wc -c < "$src"
  else
    printf ""
  fi
}

file_mtime() {
  local src="$1"
  if [[ -f "$src" ]]; then
    stat -c "%y" "$src" 2>/dev/null || printf ""
  else
    printf ""
  fi
}

file_sha256() {
  local src="$1"
  if [[ -f "$src" ]] && command -v sha256sum >/dev/null 2>&1; then
    sha256sum "$src" | awk '{print $1}'
  else
    printf ""
  fi
}

command_version() {
  local label="$1"
  shift
  if command -v "$1" >/dev/null 2>&1; then
    "$@" 2>&1 | head -n 1 | tr '\t' ' '
  else
    printf "%s not found" "$label"
  fi
}

append_manifest() {
  local role="$1"
  local src="$2"
  local copied_name=""
  if [[ -f "$src" ]]; then
    copied_name="$(basename "$src")"
  fi
  printf "%s\t%s\t%s\t%s\t%s\t%s\n" \
    "$role" \
    "$src" \
    "$copied_name" \
    "$(file_bytes "$src")" \
    "$(file_mtime "$src")" \
    "$(file_sha256 "$src")" >> "$PROFILE_MANIFEST"
}

append_metadata() {
  local key="$1"
  local value="${2:-}"
  value="${value//$'\t'/ }"
  value="${value//$'\n'/ }"
  printf "%s\t%s\n" "$key" "$value" >> "$PROFILE_METADATA"
}

copy_if_present "${OUT_OPTIMIZE_DIR}/roi_analysis_raw.csv"
copy_if_present "${OUT_OPTIMIZE_DIR}/roi_analysis_raw.rds"
copy_if_present "${OUT_OPTIMIZE_DIR}/roi_analysis_raw_manifest.csv"
copy_if_present "${OUT_OPTIMIZE_DIR}/parameter_optimization_results.csv"
copy_if_present "${OUT_OPTIMIZE_DIR}/filtered_rois_best_params.csv"
copy_if_present "${OUT_OPTIMIZE_DIR}/optROIs_summary.csv"
copy_if_present "${OUT_OPTIMIZE_DIR}/optimization_detailed_report.txt"
copy_if_present "${OUT_OPTIMIZE_DIR}/optimization_input_diagnostics.csv"
copy_if_present "${OUT_OPTIMIZE_DIR}/optimization_model_metadata.csv"
copy_if_present "${OUT_OPTIMIZE_DIR}/optimization_score_vs_area.png"
copy_if_present "${OUT_OPTIMIZE_DIR}/optimization_scatter.png"
copy_if_present "${OPTIM_GROUPS_PATH}"
copy_if_present "${OPTIM_GROUPS_INSPECTED_PATH}"
copy_if_present "${OPT_RDS_PATH}"
copy_if_present "${SETTINGS_FILE}"
if [[ -f "${SETTINGS_FILE}" ]]; then
  cp -f "${SETTINGS_FILE}" "${PROFILE_DIR}/profile_settings.env"
fi

cat > "${PROFILE_DIR}/profile_info.txt" <<EOF
profile_name=${PROFILE_NAME}
created_at=${PROFILE_CREATED_AT}
project_root=${PROJECT_ROOT}
mode=${PIPELINE_OPERATION_MODE}
profile_type=validation_cutoff_profile
EOF

printf "role\tsource_path\tcopied_name\tbytes\tmodified_time\tsha256\n" > "$PROFILE_MANIFEST"
append_manifest "settings_file" "$SETTINGS_FILE"
append_manifest "sample_metadata" "$SAMPLE_METADATA_PATH"
append_manifest "ilastik_project" "$ILASTIK_PROJECT_PATH"
append_manifest "mask_percentages" "$MASK_PERCENTAGES_PATH"
append_manifest "validation_groups" "$OPTIM_GROUPS_PATH"
append_manifest "validation_groups_inspected" "$OPTIM_GROUPS_INSPECTED_PATH"
append_manifest "packaged_roi_rds" "$OPT_RDS_PATH"
append_manifest "roi_summary" "${OUT_OPTIMIZE_DIR}/optROIs_summary.csv"
append_manifest "roi_analysis_raw_csv" "${OUT_OPTIMIZE_DIR}/roi_analysis_raw.csv"
append_manifest "roi_analysis_raw_rds" "${OUT_OPTIMIZE_DIR}/roi_analysis_raw.rds"
append_manifest "roi_analysis_manifest" "${OUT_OPTIMIZE_DIR}/roi_analysis_raw_manifest.csv"
append_manifest "optimization_results" "${OUT_OPTIMIZE_DIR}/parameter_optimization_results.csv"
append_manifest "filtered_rois" "${OUT_OPTIMIZE_DIR}/filtered_rois_best_params.csv"
append_manifest "optimization_detailed_report" "${OUT_OPTIMIZE_DIR}/optimization_detailed_report.txt"
append_manifest "optimization_input_diagnostics" "${OUT_OPTIMIZE_DIR}/optimization_input_diagnostics.csv"
append_manifest "optimization_model_metadata" "${OUT_OPTIMIZE_DIR}/optimization_model_metadata.csv"
append_manifest "optimization_score_plot" "${OUT_OPTIMIZE_DIR}/optimization_score_vs_area.png"
append_manifest "optimization_scatter_plot" "${OUT_OPTIMIZE_DIR}/optimization_scatter.png"

append_manifest "script_0_rename" "${PROJECT_ROOT}/0.Rename.sh"
append_manifest "script_1_image_list" "${PROJECT_ROOT}/1.get_img_list.sh"
append_manifest "script_2_fiji_preprocess" "${PROJECT_ROOT}/2.RunFIJI_clean_bckgnd.sh"
append_manifest "script_3_ilastik" "${PROJECT_ROOT}/3.Run_ilastik_model.sh"
append_manifest "script_4_area_fiji" "${PROJECT_ROOT}/4.GetArea%Fiji.sh"
append_manifest "script_5_groups" "${PROJECT_ROOT}/5.SelectValidationImgGroups.sh"
append_manifest "script_6_writeback" "${PROJECT_ROOT}/6.SelectedValidationImgWriteSee.sh"
append_manifest "script_7_roi_extract" "${PROJECT_ROOT}/7.RunOptimization.sh"
append_manifest "macro_clear_background" "${PROJECT_ROOT}/Macro_1-clear_bckgnd.ijm"
append_manifest "macro_mask_area" "${PROJECT_ROOT}/Macro_2-get_mask_area_percentages.ijm"
append_manifest "macro_save_rois" "${PROJECT_ROOT}/saveROIs.ijm"
append_manifest "r_groups" "${PROJECT_ROOT}/CombineFiles_GetValGroups.R"
append_manifest "r_writeback" "${PROJECT_ROOT}/CombinedFiles_after_inspection.R"
append_manifest "r_package_rois" "${PROJECT_ROOT}/0.ReadROIdata-to-optimize.R"
append_manifest "r_optimize" "${PROJECT_ROOT}/1.OptimizeNeuriteParams-MAJOR_OPTIMIZE.R"

printf "key\tvalue\n" > "$PROFILE_METADATA"
append_metadata "profile_name" "$PROFILE_NAME"
append_metadata "created_at" "$PROFILE_CREATED_AT"
append_metadata "profile_type" "validation_cutoff_profile"
append_metadata "project_root_at_creation" "$PROJECT_ROOT"
append_metadata "settings_file" "$SETTINGS_FILE"
append_metadata "output_root_dir" "$OUTPUT_ROOT_DIR"
append_metadata "out_optimize_dir" "$OUT_OPTIMIZE_DIR"
append_metadata "input_workspace_dir" "$INPUT_WORKSPACE_DIR"
append_metadata "sample_metadata_path" "$SAMPLE_METADATA_PATH"
append_metadata "ilastik_project_path" "$ILASTIK_PROJECT_PATH"
append_metadata "fiji_bin" "${FIJI_BIN:-}"
append_metadata "ilastik_bin" "${ILASTIK_BIN:-}"
append_metadata "ilastik_max_processes" "${ILASTIK_MAX_PROCESSES:-}"
append_metadata "apply_ilastik_renorm" "${APPLY_ILASTIK_RENORM:-}"
append_metadata "seg_label_neurites" "${SEG_LABEL_NEURITES:-}"
append_metadata "seg_label_cell_bodies" "${SEG_LABEL_CELL_BODIES:-}"
append_metadata "seg_label_background" "${SEG_LABEL_BACKGROUND:-}"
append_metadata "seg_label_mapping_confirmed" "${SEG_LABEL_MAPPING_CONFIRMED:-}"
append_metadata "seg_label_mapping_confirmed_at" "${SEG_LABEL_MAPPING_CONFIRMED_AT:-}"
append_metadata "pipeline_operation_mode" "${PIPELINE_OPERATION_MODE:-}"
append_metadata "optimization_scoring_mode_setting" "${OPTIMIZATION_SCORING_MODE:-continuity_aware}"
if [[ -f "${OUT_OPTIMIZE_DIR}/optimization_model_metadata.csv" ]]; then
  while IFS=, read -r key value _rest; do
    [[ "$key" == "key" ]] && continue
    [[ -z "$key" ]] && continue
    key="${key%\"}"
    key="${key#\"}"
    value="${value%\"}"
    value="${value#\"}"
    append_metadata "optimization_${key}" "$value"
  done < "${OUT_OPTIMIZE_DIR}/optimization_model_metadata.csv"
fi
append_metadata "bash_version" "${BASH_VERSION:-}"
append_metadata "uname" "$(uname -a 2>/dev/null || true)"
append_metadata "git_commit" "$(cd "$PROJECT_ROOT" 2>/dev/null && git rev-parse HEAD 2>/dev/null || printf "not a git repository")"
append_metadata "git_status_short" "$(cd "$PROJECT_ROOT" 2>/dev/null && git status --short 2>/dev/null | tr '\n' ';' || true)"
append_metadata "rscript_version" "$(command_version Rscript Rscript --version)"
append_metadata "bash_command_version" "$(command_version bash bash --version)"
append_metadata "fiji_path_exists" "$(if [[ -n "${FIJI_BIN:-}" && -e "${FIJI_BIN:-}" ]]; then printf "true"; else printf "false"; fi)"
append_metadata "ilastik_path_exists" "$(if [[ -n "${ILASTIK_BIN:-}" && -e "${ILASTIK_BIN:-}" ]]; then printf "true"; else printf "false"; fi)"

if command -v Rscript >/dev/null 2>&1; then
  Rscript -e 'pkgs <- c("dplyr","ggplot2","tidyr","readr","RImageJROI","shiny"); cat(paste(sprintf("r_package_%s=%s", pkgs, vapply(pkgs, function(p) if (requireNamespace(p, quietly=TRUE)) as.character(utils::packageVersion(p)) else "not installed", character(1))), collapse="\n"))' \
    | while IFS='=' read -r key value; do append_metadata "$key" "$value"; done
fi

cat > "$PROFILE_REPORT" <<EOF
Validation Cutoff Profile Recreation Report
===========================================

Profile name: ${PROFILE_NAME}
Created at: ${PROFILE_CREATED_AT}
Project root at creation: ${PROJECT_ROOT}
Profile directory: ${PROFILE_DIR}

Purpose
-------
This folder stores the validated neurite cutoff profile generated by the browser app.
It is intended to make the cutoff selection reproducible later without guessing which
settings, inputs, scripts, labels, or output tables were used.

Core Files
----------
- profile_info.txt: short legacy summary used by the app profile browser.
- profile_metadata.tsv: key-value metadata for settings, software, labels, and runtime context.
- profile_file_manifest.tsv: source path, copied filename, size, modification time, and SHA-256 fingerprint for relevant inputs, scripts, and outputs.
- profile_settings.env: copy of the shared settings file at the time this profile was saved.

Main Cutoff Outputs
-------------------
- parameter_optimization_results.csv: all tested cutoff combinations and scores.
- filtered_rois_best_params.csv: ROI rows retained by the selected best cutoff.
- roi_analysis_raw.csv / roi_analysis_raw.rds: reusable ROI analysis checkpoint used by optimization.
- optimization_detailed_report.txt: staged optimizer log and selected-parameter report.
- optimization_input_diagnostics.csv: optimizer readiness diagnostics.
- optimization_model_metadata.csv: exact optimization mode, continuity-aware scoring settings, and selected best-cutoff metrics.

Manual Validation Context
-------------------------
- mask_area_percentages+optim_groups.csv: automatic validation quadrant grouping.
- mask_area_percentages+optim_groups+inspected.csv: manual choice/noise labels written back into the table.
- optROIs_data.rds and optROIs_summary.csv: packaged ROI data used to create the optimization table.

How To Recreate
---------------
1. Restore or reproduce the same software environment shown in profile_metadata.tsv.
2. Use profile_settings.env as the reference settings file.
3. Confirm that the input files and scripts match profile_file_manifest.tsv fingerprints.
4. Re-run the validation chain through ROI packaging if the source data changed.
5. Re-run 2.A.6. Optimize Neurite Thresholds. If roi_analysis_raw.* still matches the same optROIs_data.rds, the optimizer can reuse the checkpoint and continue from grid optimization.

Important Notes
---------------
- If script or input SHA-256 fingerprints differ, the exact profile may not be fully reproducible.
- If manual labels differ, cutoff behavior can change substantially.
- If segmentation label mapping differs, mask percentages and validation groups should be regenerated.
EOF

echo "Saved validation cutoff profile to: ${PROFILE_DIR}"
