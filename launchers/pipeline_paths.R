`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

read_pipeline_settings <- function(settings_file) {
  if (!file.exists(settings_file)) return(list())
  lines <- readLines(settings_file, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  out <- list()
  for (line in lines) {
    if (!grepl("=", line, fixed = TRUE)) next
    parts <- strsplit(line, "=", fixed = TRUE)[[1]]
    key <- trimws(parts[1])
    value <- trimws(paste(parts[-1], collapse = "="))
    value <- sub('^"', "", value)
    value <- sub('"$', "", value)
    out[[key]] <- value
  }
  out
}

normalize_pipeline_path <- function(project_root, value, default = NULL) {
  value <- if (is.null(value) || !nzchar(value)) default else value
  if (is.null(value) || !nzchar(value)) {
    return(normalizePath(project_root, winslash = "/", mustWork = FALSE))
  }
  if (grepl("^([A-Za-z]:[/\\\\]|/)", value)) {
    return(normalizePath(value, winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(project_root, value), winslash = "/", mustWork = FALSE)
}

first_existing_file <- function(paths) {
  paths <- paths[nzchar(paths %||% "")]
  for (path in paths) {
    if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  ""
}

first_matching_file <- function(dirs, pattern) {
  for (dir in dirs) {
    if (!dir.exists(dir)) next
    files <- list.files(dir, pattern = pattern, recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    if (length(files)) return(normalizePath(files[[1]], winslash = "/", mustWork = FALSE))
  }
  ""
}

count_raw_image_files <- function(dir) {
  if (!dir.exists(dir)) return(0L)
  files <- list.files(dir, pattern = "\\.(tif|tiff|png|jpg|jpeg)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) return(0L)
  files <- files[!grepl("/(ilastik|software_reference|example_expected_outputs|training_images_raw|training_images_preprocessed)/", normalizePath(files, winslash = "/", mustWork = FALSE), ignore.case = TRUE)]
  files <- files[!grepl("(_RGavg|_mask|_mask_renorm)\\.", basename(files), ignore.case = TRUE)]
  length(files)
}

resolve_raw_images_root <- function(configured, input_workspace) {
  candidates <- unique(c(
    configured,
    file.path(configured, "raw_images"),
    file.path(input_workspace, "raw_images"),
    list.files(input_workspace, pattern = "^raw_images$", recursive = TRUE, full.names = TRUE, ignore.case = FALSE)
  ))
  candidates <- candidates[dir.exists(candidates)]
  if (!length(candidates)) return(configured)
  counts <- vapply(candidates, count_raw_image_files, integer(1))
  if (max(counts, na.rm = TRUE) <= 0) return(configured)
  normalizePath(candidates[[which.max(counts)]], winslash = "/", mustWork = FALSE)
}

resolve_zenodo_input_root <- function(project_root, settings) {
  configured <- normalize_pipeline_path(project_root, settings$INPUT_MASTER_DIR, "pipeline_inputs")
  input_workspace <- normalize_pipeline_path(project_root, settings$INPUT_WORKSPACE_DIR_NAME, "pipeline_inputs")
  resolve_raw_images_root(configured, input_workspace)
}

resolve_zenodo_metadata_file <- function(project_root, settings) {
  configured <- normalize_pipeline_path(project_root, settings$SAMPLE_METADATA_FILE, "pipeline_inputs/IN_sample_metadata.csv")
  input_workspace <- normalize_pipeline_path(project_root, settings$INPUT_WORKSPACE_DIR_NAME, "pipeline_inputs")
  found <- first_existing_file(c(
    configured,
    file.path(input_workspace, "IN_sample_metadata.csv"),
    file.path(input_workspace, "metadata", "IN_sample_metadata_zenodo_paths.csv"),
    file.path(input_workspace, "metadata", "IN_sample_metadata_original.csv")
  ))
  if (nzchar(found)) found else configured
}

resolve_zenodo_ilastik_project <- function(project_root, settings) {
  configured <- normalize_pipeline_path(project_root, settings$ILASTIK_PROJECT_FILE, "models/Ilastik1_probably_working.ilp")
  input_workspace <- normalize_pipeline_path(project_root, settings$INPUT_WORKSPACE_DIR_NAME, "pipeline_inputs")
  found <- first_existing_file(c(
    configured,
    file.path(project_root, "models", "Ilastik1_probably_working.ilp"),
    file.path(input_workspace, "ilastik", "model", "Ilastik1_probably_working.ilp")
  ))
  if (nzchar(found)) return(found)
  found <- first_matching_file(c(file.path(input_workspace, "ilastik", "model"), input_workspace), "\\.ilp$")
  if (nzchar(found)) found else configured
}

pipeline_context <- function(base_dir = getwd(), settings_file = NULL) {
  project_root <- normalizePath(base_dir, winslash = "/", mustWork = FALSE)
  if (is.null(settings_file)) {
    settings_file <- file.path(project_root, "config", "pipeline_settings.env")
  }
  settings <- read_pipeline_settings(settings_file)

  output_root <- normalize_pipeline_path(project_root, settings$OUTPUT_ROOT_DIR_NAME, "pipeline_outputs")
  input_workspace <- normalize_pipeline_path(project_root, settings$INPUT_WORKSPACE_DIR_NAME, "pipeline_inputs")
  input_master <- resolve_zenodo_input_root(project_root, settings)
  out_opt <- file.path(output_root, settings$OUT_OPTIMIZE_DIR_NAME %||% "OUT_optimize")

  list(
    project_root = project_root,
    settings = settings,
    output_root = output_root,
    paths = list(
      output_root = output_root,
      input_workspace = input_workspace,
      input_master = input_master,
      pre_renamed = file.path(output_root, settings$PRE_RENAMED_DIR_NAME %||% "PRE_renamed"),
      image_list = file.path(output_root, settings$IMAGE_INPUT_LIST_NAME %||% "OUT_img_input1.txt"),
      out_clear = file.path(output_root, settings$OUT_CLEAR_BCGND_DIR_NAME %||% "OUT_clear_bcgnd"),
      out_seg = file.path(output_root, settings$OUT_SEGMENTATION_DIR_NAME %||% "OUT_segmentation"),
      out_opt = out_opt,
      roi_root = file.path(out_opt, settings$OUT_ROI_DIR_NAME %||% "ROIs"),
      metadata = resolve_zenodo_metadata_file(project_root, settings),
      ilastik_project = resolve_zenodo_ilastik_project(project_root, settings),
      mask_percentages = file.path(out_opt, settings$MASK_PERCENTAGES_FILE %||% "mask_area_percentages.csv"),
      optim_groups = file.path(out_opt, settings$OPTIM_GROUPS_FILE %||% "mask_area_percentages+optim_groups.csv"),
      optim_groups_inspected = file.path(out_opt, settings$OPTIM_GROUPS_INSPECTED_FILE %||% "mask_area_percentages+optim_groups+inspected.csv"),
      opt_rois_rds = file.path(out_opt, settings$OPT_RDS_FILE %||% "optROIs_data.rds"),
      all_rois_data = file.path(out_opt, "allROIs_data.rds"),
      all_rois_analysis = file.path(out_opt, "allROIs_analysis.rds"),
      all_rois_analysis_skeletons = file.path(out_opt, "allROIs_analysis+skeletons.rds"),
      generalized_dir = file.path(output_root, "OUT_generalized_filtering"),
      generalized_rds = file.path(output_root, "OUT_generalized_filtering", "skeletons_filtered_generalized.rds"),
      metadata_pca_dir = file.path(output_root, "OUT_metadata_PCA"),
      experimental_dir = file.path(output_root, "OUT_experimental_analysis"),
      final_dir = file.path(output_root, "OUT_biological_analysis_enhanced"),
      validation_profiles = normalize_pipeline_path(project_root, settings$VALIDATION_PROFILE_DIR_NAME, "cache/validation_profiles")
    )
  )
}
