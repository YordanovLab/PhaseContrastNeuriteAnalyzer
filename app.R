library(shiny)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

project_root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
settings_path <- file.path(project_root, "config", "pipeline_settings.env")
run_log_dir <- file.path(project_root, "cache", "run_logs")
run_history_file <- file.path(run_log_dir, "command_history.csv")
has_processx <- requireNamespace("processx", quietly = TRUE)
app_version_marker <- "PCNA_APP_VERSION_2026-04-22_combo_filters_and_facet_stats_v1"

read_settings_file <- function(path) {
  if (!file.exists(path)) return(list())
  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !startsWith(lines, "#")]
  out <- list()
  for (line in lines) {
    if (!grepl("=", line, fixed = TRUE)) next
    bits <- strsplit(line, "=", fixed = TRUE)[[1]]
    key <- trimws(bits[1])
    value <- trimws(paste(bits[-1], collapse = "="))
    value <- sub('^"', "", value)
    value <- sub('"$', "", value)
    out[[key]] <- value
  }
  out
}

write_settings_file <- function(path, values) {
  lines <- c(
    "# Shared pipeline settings",
    "",
    sprintf('PROJECT_ROOT="%s"', values$PROJECT_ROOT %||% "."),
    sprintf('OUTPUT_ROOT_DIR_NAME="%s"', values$OUTPUT_ROOT_DIR_NAME %||% "pipeline_outputs"),
    sprintf('INPUT_WORKSPACE_DIR_NAME="%s"', values$INPUT_WORKSPACE_DIR_NAME %||% "pipeline_inputs"),
    sprintf('INPUT_MASTER_DIR="%s"', values$INPUT_MASTER_DIR %||% "pipeline_inputs"),
    sprintf('PRE_RENAMED_DIR_NAME="%s"', values$PRE_RENAMED_DIR_NAME %||% "PRE_renamed"),
    sprintf('IMAGE_INPUT_LIST_NAME="%s"', values$IMAGE_INPUT_LIST_NAME %||% "OUT_img_input1.txt"),
    sprintf('OUT_CLEAR_BCGND_DIR_NAME="%s"', values$OUT_CLEAR_BCGND_DIR_NAME %||% "OUT_clear_bcgnd"),
    sprintf('OUT_SEGMENTATION_DIR_NAME="%s"', values$OUT_SEGMENTATION_DIR_NAME %||% "OUT_segmentation"),
    sprintf('OUT_OPTIMIZE_DIR_NAME="%s"', values$OUT_OPTIMIZE_DIR_NAME %||% "OUT_optimize"),
    sprintf('OUT_ROI_DIR_NAME="%s"', values$OUT_ROI_DIR_NAME %||% "ROIs"),
    "",
    sprintf('FIJI_BIN="%s"', values$FIJI_BIN %||% ""),
    sprintf('ILASTIK_BIN="%s"', values$ILASTIK_BIN %||% ""),
    sprintf('ILASTIK_PROJECT_FILE="%s"', values$ILASTIK_PROJECT_FILE %||% ""),
    sprintf('ILASTIK_MAX_PROCESSES="%s"', values$ILASTIK_MAX_PROCESSES %||% "2"),
    sprintf('APPLY_ILASTIK_RENORM="%s"', values$APPLY_ILASTIK_RENORM %||% "true"),
    sprintf('SEG_LABEL_NEURITES="%s"', values$SEG_LABEL_NEURITES %||% "3"),
    sprintf('SEG_LABEL_CELL_BODIES="%s"', values$SEG_LABEL_CELL_BODIES %||% "1"),
    sprintf('SEG_LABEL_BACKGROUND="%s"', values$SEG_LABEL_BACKGROUND %||% "255"),
    sprintf('SEG_LABEL_MAPPING_CONFIRMED="%s"', values$SEG_LABEL_MAPPING_CONFIRMED %||% "false"),
    sprintf('SEG_LABEL_MAPPING_CONFIRMED_AT="%s"', values$SEG_LABEL_MAPPING_CONFIRMED_AT %||% ""),
    "",
    sprintf('SAMPLE_METADATA_FILE="%s"', values$SAMPLE_METADATA_FILE %||% "pipeline_inputs/IN_sample_metadata.csv"),
    sprintf('MASK_PERCENTAGES_FILE="%s"', values$MASK_PERCENTAGES_FILE %||% "mask_area_percentages.csv"),
    sprintf('OPTIM_GROUPS_FILE="%s"', values$OPTIM_GROUPS_FILE %||% "mask_area_percentages+optim_groups.csv"),
    sprintf('OPTIM_GROUPS_INSPECTED_FILE="%s"', values$OPTIM_GROUPS_INSPECTED_FILE %||% "mask_area_percentages+optim_groups+inspected.csv"),
    sprintf('OPT_RDS_FILE="%s"', values$OPT_RDS_FILE %||% "optROIs_data.rds"),
    "",
    sprintf('PIPELINE_OPERATION_MODE="%s"', values$PIPELINE_OPERATION_MODE %||% "optimization"),
    sprintf('OPTIMIZATION_SCORING_MODE="%s"', values$OPTIMIZATION_SCORING_MODE %||% "continuity_aware"),
    sprintf('ACTIVE_OPTIMIZATION_PROFILE="%s"', values$ACTIVE_OPTIMIZATION_PROFILE %||% ""),
    sprintf('ACTIVE_VALIDATION_PROFILE="%s"', values$ACTIVE_VALIDATION_PROFILE %||% ""),
    sprintf('VALIDATION_PROFILE_DIR_NAME="%s"', values$VALIDATION_PROFILE_DIR_NAME %||% "cache/validation_profiles")
  )
  writeLines(lines, path, useBytes = TRUE)
}

normalize_project_path <- function(value, root = project_root) {
  value <- value %||% ""
  if (!nzchar(value)) return(root)
  if (grepl("^([A-Za-z]:/|[A-Za-z]:\\\\|/)", value)) {
    return(normalizePath(value, winslash = "/", mustWork = FALSE))
  }
  normalizePath(file.path(root, value), winslash = "/", mustWork = FALSE)
}

first_existing_path <- function(paths) {
  paths <- paths[nzchar(paths %||% "")]
  for (path in paths) {
    if (file.exists(path)) return(normalizePath(path, winslash = "/", mustWork = FALSE))
  }
  ""
}

first_matching_path <- function(dirs, pattern) {
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
  files <- normalizePath(files, winslash = "/", mustWork = FALSE)
  files <- files[!grepl("/(ilastik|software_reference|example_expected_outputs|training_images_raw|training_images_preprocessed)/", files, ignore.case = TRUE)]
  files <- files[!grepl("(_RGavg|_mask|_mask_renorm)\\.", basename(files), ignore.case = TRUE)]
  length(files)
}

input_permission_diagnostics <- function(root, max_report = 12L) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  out <- list(untraversable_dirs = character(), unreadable_files = character())
  if (!dir.exists(root)) return(out)

  extract_find_permission_paths <- function(lines) {
    lines <- as.character(lines %||% character())
    lines <- lines[grepl("Permission denied", lines, ignore.case = TRUE)]
    if (!length(lines)) return(character())
    paths <- sub("^find:\\s*", "", lines)
    paths <- sub("\\s*:\\s*Permission denied.*$", "", paths, ignore.case = TRUE)
    paths <- gsub("^[`'\u2018\u2019\"]|[`'\u2018\u2019\"]$", "", paths)
    paths[nzchar(paths)]
  }

  if (.Platform$OS.type != "windows" && nzchar(Sys.which("find"))) {
    bad_dirs <- tryCatch(
      system2("find", c(root, "-type", "d", "!", "-executable", "-print"), stdout = TRUE, stderr = TRUE),
      error = function(e) character()
    )
    bad_files <- tryCatch(
      system2("find", c(root, "-type", "f", "!", "-readable", "-print"), stdout = TRUE, stderr = TRUE),
      error = function(e) character()
    )
    denied_paths <- unique(c(extract_find_permission_paths(bad_dirs), extract_find_permission_paths(bad_files)))
    bad_dirs <- bad_dirs[nzchar(bad_dirs) & !grepl("Permission denied", bad_dirs, ignore.case = TRUE)]
    bad_files <- bad_files[nzchar(bad_files) & !grepl("Permission denied", bad_files, ignore.case = TRUE)]
    if (length(bad_dirs)) {
      out$untraversable_dirs <- unique(normalizePath(utils::head(bad_dirs, max_report), winslash = "/", mustWork = FALSE))
    }
    if (length(denied_paths)) {
      out$untraversable_dirs <- unique(c(
        out$untraversable_dirs,
        normalizePath(utils::head(denied_paths, max_report), winslash = "/", mustWork = FALSE)
      ))
    }
    if (length(bad_files)) {
      out$unreadable_files <- unique(normalizePath(utils::head(bad_files, max_report), winslash = "/", mustWork = FALSE))
    }
    if (length(out$untraversable_dirs) + length(out$unreadable_files) >= max_report) return(out)
  }

  queue <- root
  seen <- character()
  while (length(queue) && (length(out$untraversable_dirs) + length(out$unreadable_files)) < max_report) {
    current <- queue[[1]]
    queue <- queue[-1]
    current <- normalizePath(current, winslash = "/", mustWork = FALSE)
    if (current %in% seen) next
    seen <- c(seen, current)
    if (file.access(current, mode = 1) != 0) {
      out$untraversable_dirs <- c(out$untraversable_dirs, current)
      next
    }
    children <- tryCatch(list.files(current, all.files = TRUE, no.. = TRUE, full.names = TRUE), error = function(e) character())
    if (!length(children)) next
    child_info <- file.info(children)
    dirs <- children[!is.na(child_info$isdir) & child_info$isdir]
    bad_dirs <- dirs[file.access(dirs, mode = 1) != 0]
    if (length(bad_dirs)) {
      out$untraversable_dirs <- unique(c(out$untraversable_dirs, normalizePath(bad_dirs, winslash = "/", mustWork = FALSE)))
    }
    files <- children[!is.na(child_info$isdir) & !child_info$isdir]
    candidate_files <- files[grepl("\\.(tif|tiff|png|jpg|jpeg|csv|ilp)$", files, ignore.case = TRUE)]
    bad_files <- candidate_files[file.access(candidate_files, mode = 4) != 0]
    if (length(bad_files)) {
      out$unreadable_files <- unique(c(out$unreadable_files, normalizePath(bad_files, winslash = "/", mustWork = FALSE)))
    }
    queue <- c(queue, setdiff(dirs, bad_dirs))
  }
  out
}

format_input_permission_guidance <- function(root, diagnostics) {
  bad_dirs <- diagnostics$untraversable_dirs %||% character()
  bad_files <- diagnostics$unreadable_files %||% character()
  if (!length(bad_dirs) && !length(bad_files)) return(character())
  c(
    sprintf("Input permission problem detected under the selected image tree: %s", root),
    "This can happen after unzipping the Zenodo archive on Linux/WSL if directories lack the execute/search permission bit. Such folders can be listed by name but cannot be entered, so recursive image discovery may find only part of the dataset.",
    if (length(bad_dirs)) paste("Example untraversable folder(s):", paste(utils::head(bad_dirs, 6), collapse = " | ")) else character(),
    if (length(bad_files)) paste("Example unreadable file(s):", paste(utils::head(bad_files, 6), collapse = " | ")) else character(),
    "Click 'Repair Input Folder Permissions' in this browser window to fix the selected input tree automatically.",
    sprintf("Terminal fallback if the browser button is unavailable: bash ./launchers/repair_input_permissions.sh %s", bash_quote(relative_to_project(root))),
    "The repair is equivalent to chmod -R u+rwX <input-folder>. The capital X adds execute/search permission to directories without making every image file executable."
  )
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

effective_input_master_dir <- function(settings) {
  input_workspace <- normalize_project_path(settings$INPUT_WORKSPACE_DIR_NAME %||% "pipeline_inputs")
  configured <- normalize_project_path(settings$INPUT_MASTER_DIR %||% "pipeline_inputs")
  resolve_raw_images_root(configured, input_workspace)
}

effective_metadata_file <- function(settings) {
  input_workspace <- normalize_project_path(settings$INPUT_WORKSPACE_DIR_NAME %||% "pipeline_inputs")
  configured <- normalize_project_path(settings$SAMPLE_METADATA_FILE %||% "pipeline_inputs/IN_sample_metadata.csv")
  found <- first_existing_path(c(
    configured,
    file.path(input_workspace, "IN_sample_metadata.csv"),
    file.path(input_workspace, "metadata", "IN_sample_metadata_zenodo_paths.csv"),
    file.path(input_workspace, "metadata", "IN_sample_metadata_original.csv")
  ))
  if (nzchar(found)) found else configured
}

effective_ilastik_project_file <- function(settings) {
  input_workspace <- normalize_project_path(settings$INPUT_WORKSPACE_DIR_NAME %||% "pipeline_inputs")
  configured <- normalize_project_path(settings$ILASTIK_PROJECT_FILE %||% "models/Ilastik1_probably_working.ilp")
  found <- first_existing_path(c(
    configured,
    file.path(project_root, "models", "Ilastik1_probably_working.ilp"),
    file.path(input_workspace, "ilastik", "model", "Ilastik1_probably_working.ilp")
  ))
  if (nzchar(found)) return(found)
  found <- first_matching_path(c(file.path(input_workspace, "ilastik", "model"), input_workspace), "\\.ilp$")
  if (nzchar(found)) found else configured
}

input_setup_guidance_text <- function(settings = read_settings_file(settings_path)) {
  p <- tryCatch(build_paths(settings), error = function(e) NULL)
  input_root <- if (!is.null(p)) p$input_master else normalize_project_path(settings$INPUT_MASTER_DIR %||% "pipeline_inputs")
  metadata_path <- if (!is.null(p)) p$metadata else effective_metadata_file(settings)
  ilastik_path <- if (!is.null(p)) p$ilastik_project else effective_ilastik_project_file(settings)
  paste(
    "No usable input dataset/model was found. You have two straightforward options:",
    "Option 1 - use the published Zenodo dataset. Download https://zenodo.org/records/19634700 and unzip it directly inside pipeline_inputs/. The app will automatically detect pipeline_inputs/raw_images/, pipeline_inputs/metadata/IN_sample_metadata_zenodo_paths.csv, and pipeline_inputs/ilastik/model/*.ilp.",
    "Option 2 - use your own compatible data. Put your raw image folders under pipeline_inputs/ or choose another Master input directory in Configuration. Use raw image files such as .tif, .tiff, .png, .jpg, or .jpeg. Do not point the master input directory at generated folders such as OUT_clear_bcgnd, OUT_segmentation, PRE_renamed, or pipeline_outputs.",
    "For your own metadata, provide a CSV file and select it in Configuration. It must contain an image_name column. For this pipeline, image_name values should match the path-flattened name created from the raw-image folder path: for example raw folder A/B/C/img01.tif becomes A-B-C-img01.tif after the rename step.",
    "For your own model, provide a trained ilastik Pixel Classification .ilp project compatible with the Fiji-preprocessed images and with classes for background, cell bodies, and neurites. Put it in models/ or pipeline_inputs/ilastik/model/, or select it in Configuration. After segmentation, use the label-mapping check to confirm the actual numeric labels before quantification.",
    sprintf("Current effective image root: %s", input_root),
    sprintf("Current effective metadata file: %s", metadata_path),
    sprintf("Current effective ilastik project file: %s", ilastik_path),
    "After copying files, open tab 1. Configuration, confirm or browse to the paths, save settings, and rerun the step.",
    sep = "\n"
  )
}

relative_to_project <- function(path, root = project_root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  root_prefix <- paste0(root, "/")
  if (identical(path, root)) return(".")
  if (startsWith(path, root_prefix)) return(substr(path, nchar(root_prefix) + 1L, nchar(path)))
  path
}

to_wsl_path <- function(path) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (grepl("^[A-Za-z]:/", path)) {
    drive <- tolower(substr(path, 1, 1))
    rest <- substr(path, 3, nchar(path))
    return(paste0("/mnt/", drive, rest))
  }
  path
}

bash_quote <- function(x) paste0("'", gsub("'", "'\"'\"'", x, fixed = TRUE), "'")

settings_help <- list(
  project_root = list(
    title = "Project root",
    body = "This is the base folder that contains the active scripts, config, frontend, docs, and cache folders. A value of . means here, namely the current project folder where the app and scripts already live. That default is usually the correct choice and usually should not be changed unless you deliberately moved the whole project."
  ),
  output_root = list(
    title = "Central output workspace",
    body = "This is the single folder where generated outputs and intermediates are stored in a structured hierarchy. Keeping outputs here makes it safe to clear results later without risking deletion of scripts, config, docs, or launchers."
  ),
  input_master = list(
    title = "Master input directory",
    body = "This is the top-level folder where the app should look recursively for the raw images before renaming. It can point either to the managed input workspace inside the project or to an external image directory elsewhere on the machine. This makes the raw-image source explicit and reusable, while keeping it separate from scripts and generated outputs."
  ),
  input_workspace = list(
    title = "Managed input workspace",
    body = "This is a dedicated folder for input files that belong to the current project run. You can paste or copy raw image folders and metadata here, then later wipe or replace them without risking deletion of scripts, config, launchers, or generated outputs. It is optional: the pipeline can also read from input directories outside this workspace when you want to analyze files in place."
  ),
  pre_renamed = list(
    title = "PRE_renamed folder name",
    body = "This is the folder inside the central output workspace where the renaming step writes hard-linked images with path-derived unique names. It matters because later image-list creation and preprocessing usually start from this folder."
  ),
  image_list_name = list(
    title = "Image list filename",
    body = "This is the handoff text file, usually OUT_img_input1.txt, inside the central output workspace. It freezes the exact image set used by later preprocessing and is one of the most important continuity files in the whole workflow."
  ),
  out_optimize = list(
    title = "Optimization output folder",
    body = "This is the main working output folder inside the central output workspace for validation, ROI extraction, optimization tables, inspected labels, and many downstream intermediate files. Many scripts read and write here, so it needs to stay consistent."
  ),
  metadata_file = list(
    title = "Sample metadata file",
    body = "This should be a CSV file linking image names to experimental or biological metadata. It is used when creating validation groups and in later analysis and interpretation steps."
  ),
  fiji_bin = list(
    title = "Fiji executable",
    body = "This is the executable used for the Fiji or ImageJ-driven preprocessing and ROI steps. If this path is missing or wrong, the Bash wrappers that call Fiji macros will fail. For Windows users running the backend in WSL, GUI support should be enabled because Fiji in GUI-capable mode is preferred for reproducing the same outputs; some headless Fiji runs can behave differently."
  ),
  ilastik_bin = list(
    title = "ilastik executable",
    body = "This is the batch executable for ilastik. It is needed for the segmentation stage that transforms the preprocessed images into neurite masks."
  ),
  ilastik_project = list(
    title = "ilastik project file",
    body = "This is the trained ilastik model project, usually a .ilp file. The segmentation step depends on it to apply the same learned labeling logic to your images."
  ),
  segmentation_label_mapping = list(
    title = "Segmentation label mapping",
    body = "This maps the pixel labels found in the ilastik segmentation masks to the biological classes used later by Fiji and R. It must match the actual mask peaks before mask-area quantification is trusted. The defaults are neurites = 3, cell bodies = 1, and background = 255, but you can reassign them if the segmentation output uses a different label order. The in-app TIFF preview uses the R package magick, so install magick in the same R environment that runs Shiny if this check cannot read masks."
  ),
  validation_dir = list(
    title = "Validation profile folder",
    body = "This is the preferred folder for saved validated cutoff profiles. If a trusted profile already exists there, the workflow can reuse it and continue directly with generalization and interpretation instead of repeating validation."
  )
)

app_help <- list(
  runtime_choice = list(
    title = "Runtime choice",
    body = "This decides where the backend scripts should actually run. Use the current Linux or WSL session when the required Bash, R, Fiji, and ilastik tools are installed there. Use the Windows-to-WSL option when the app is opened from Windows but the real pipeline should run in WSL. If Fiji is being run inside WSL, GUI support should be enabled so Fiji can run in its normal GUI-capable mode, which is preferred for reproducing outputs consistently."
  ),
  rename_search_root = list(
    title = "Search root for image renaming",
    body = "This is the top folder that 0.Rename.sh will scan for raw images. By default it should match the master input directory. That can be the managed input workspace or an external image folder elsewhere on the machine. The script creates hard links under PRE_renamed and should point at the real image source tree, not at the generated output workspace."
  ),
  image_list_source = list(
    title = "Folder to scan for the image list",
    body = "This is the folder that 1.get_img_list.sh uses to build OUT_img_input1.txt. In the usual workflow this should be PRE_renamed inside the central output workspace so the later pipeline works from the uniquely named linked images."
  ),
  workflow_strategy = list(
    title = "Fresh validation or profile reuse",
    body = "This is the first major workflow decision. Choose fresh validation when no trusted validated cutoff profile exists yet, or when you want to repeat the cutoff-learning process. Choose reuse when a previous validated profile is still appropriate for the new images."
  ),
  profile_choice = list(
    title = "Saved validation profile choice",
    body = "Each saved profile represents a previously trusted set of validated cutoff outputs. Choose by date or folder name. Applying it restores those reusable outputs into the working optimization folder so later steps can continue without repeating validation."
  ),
  new_profile_name = list(
    title = "New validation profile name",
    body = "This is the name used when saving the current validated cutoff outputs for later reuse. A date-first naming style is best because it makes profile selection much easier for beginners."
  ),
  runtime_table = list(
    title = "Runtime summary table",
    body = "This table is a quick readiness check. It shows whether the app can currently see the runtime tools it needs, such as WSL, Bash, Rscript, and the configured Fiji and ilastik paths. For Windows users relying on WSL, also make sure GUI support is available before running Fiji-based steps."
  ),
  software_versions = list(
    title = "Software versions for reproducibility",
    body = "This table summarizes the current software environment that the app can detect, including the running R session, command-line tools, and configured external executables such as Fiji and ilastik. Use it when preparing a reproducible setup for GitHub or another machine."
  ),
  package_versions = list(
    title = "R package versions detected from project scripts",
    body = "This table scans the project R scripts for referenced packages and reports whether they are currently installed in the running R session and, when available, which version is installed. It is a reproducibility aid rather than a guarantee that every optional branch of the project has been executed."
  ),
  profile_table = list(
    title = "Detected profiles table",
    body = "This table lists the saved validated cutoff profiles the app can currently find. It is useful for checking whether an older profile is available before deciding to rerun validation."
  ),
  output_status = list(
    title = "Key output checks",
    body = "This table shows whether the most important pipeline outputs already exist. It helps a beginner understand which stages have already been completed and which ones still need to run."
  ),
  execution_log = list(
    title = "Execution log",
    body = "This is the plain-text record of commands the app tried to run and the output they returned. If a step fails, this is the first place to look for the reason."
  ),
  guided_execution = list(
    title = "Step-by-step execution area",
    body = "These cards represent the runnable workflow. Each card explains what the step is for, what it expects as input, and what output it should create. If you reuse a saved profile, you can usually skip the validation-with-test-images cards."
  ),
  step_setup = list(
    title = "Setup and Preprocessing Step",
    body = "These steps prepare the image inputs, create the preprocessing outputs, and produce the first quantitative mask summaries. They are the technical foundation of the rest of the workflow."
  ),
  step_validation = list(
    title = "Validation with Test Images Step",
    body = "These steps learn and confirm cutoff behavior on curated test images. They are where manual inspection and threshold optimization happen, so they do not need to be repeated every time if a trusted saved profile already exists."
  ),
  step_generalization = list(
    title = "Generalization with New Images Step",
    body = "These steps apply the learned or reused validated cutoff logic to broader image-derived data. They are the bridge from test-image validation to production-style analysis."
  ),
  step_interpretation = list(
    title = "Visualization and Interpretation Step",
    body = "These steps help the user understand what the image-analysis pipeline produced. They create representative views, PCA-style interpretations, and final reporting plots."
  ),
  cutoff_review = list(
    title = "Review Obtained or Selected Cutoffs",
    body = "This review panel reads the current optimization/profile outputs and summarizes whether the selected cutoff result is trustworthy. It checks for missing files, empty columns, non-finite scores, missing signal/noise labels, low ROI retention, possible all-identical metrics, and whether the best result is meaningfully separated from the next alternatives."
  ),
  install_wsl = list(
    title = "Install WSL",
    body = "For Windows users, use the official Microsoft Learn WSL installation documentation. Search online for Microsoft Learn WSL install and follow the current guide for enabling WSL and a Linux distribution."
  ),
  install_r = list(
    title = "Install R",
    body = "Install R from the official CRAN project website. Search online for CRAN R download and choose the installer or repository instructions for your operating system."
  ),
  install_fiji = list(
    title = "Install Fiji",
    body = "Install Fiji from the official Fiji or ImageJ distribution page. Search online for Fiji download ImageJ and use the build that matches your runtime environment."
  ),
  install_ilastik = list(
    title = "Install ilastik",
    body = "Install ilastik from the official ilastik website. Search online for ilastik download and choose the package that matches your Linux, WSL, or Windows runtime."
  ),
  install_imagemagick = list(
    title = "Install ImageMagick",
    body = "Install ImageMagick so the command-line convert or magick utility is available in the same runtime environment as the backend scripts. It is used for mask renormalization in ilastik segmentation workflows. On Linux/WSL this is commonly installed through the system package manager; on Windows use the official ImageMagick installer if running the backend natively."
  ),
  install_r_packages = list(
    title = "Install required R packages",
    body = "Install required R packages inside the same R environment that runs the app and scripts. Core packages include shiny, dplyr, readr, ggplot2, tidyr, stringr, purrr, R.utils, magick, and png. Optional analysis packages such as uwot, Rtsne, pheatmap, and emmeans enable additional visualization/statistical panels. The package versions table below helps identify which packages are referenced by the current project scripts."
  ),
  input_data_setup = list(
    title = "Input data and model setup",
    body = "For the example workflow, download the Zenodo dataset and unzip it directly inside pipeline_inputs/. The app is designed to detect raw_images, metadata CSV files, ilastik model files, and ilastik training-image folders from that structure. For your own data, keep raw images in nested folders, provide metadata with image_name values that can match the renamed images, and select or place a compatible trained ilastik .ilp project file."
  )
)

help_button <- function(id) {
  actionLink(
    inputId = paste0("help_", id),
    label = NULL,
    icon = NULL,
    class = "help-dot",
    `aria-label` = "Open parameter help",
    title = "Open detailed help for this setting",
    `data-help-id` = id,
    tags$span("?")
  )
}

label_with_help <- function(label, help_id) {
  tags$div(
    class = "setting-label-row",
    tags$span(class = "setting-label", label),
    help_button(help_id)
  )
}

setting_row <- function(label, input_tag, help_id) {
  tags$div(
    class = "setting-row",
    label_with_help(label, help_id),
    input_tag
  )
}

setting_row_with_browse <- function(label, input_tag, help_id, browse_id) {
  tags$div(
    class = "setting-row",
    label_with_help(label, help_id),
    tags$div(
      style = "display:flex; gap:8px; align-items:flex-start;",
      tags$div(style = "flex:1;", input_tag),
      actionButton(browse_id, "Browse", class = "btn btn-default")
    )
  )
}

browseable_files <- function(path, pattern = NULL) {
  if (!dir.exists(path)) return(character())
  files <- list.files(path, recursive = FALSE, full.names = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  if (!is.null(pattern)) files <- files[grepl(pattern, basename(files), ignore.case = TRUE)]
  files[order(basename(files))]
}

guess_image_content_type <- function(path) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    png = "image/png",
    jpg = "image/jpeg",
    jpeg = "image/jpeg",
    gif = "image/gif",
    bmp = "image/bmp",
    tif = "image/tiff",
    tiff = "image/tiff",
    "application/octet-stream"
  )
}

manual_view_image_source <- function(img_path) {
  ext <- tolower(tools::file_ext(img_path))
  src_path <- normalizePath(img_path, winslash = "/", mustWork = FALSE)
  content_type <- guess_image_content_type(img_path)
  note <- NULL

  if (ext %in% c("tif", "tiff")) {
    if (requireNamespace("magick", quietly = TRUE)) {
      preview_cache_dir <- file.path(project_root, "cache", "manual_previews")
      dir.create(preview_cache_dir, recursive = TRUE, showWarnings = FALSE)
      cache_key <- paste0(
        gsub("[^A-Za-z0-9._-]+", "_", basename(dirname(src_path))),
        "_",
        gsub("[^A-Za-z0-9._-]+", "_", tools::file_path_sans_ext(basename(src_path))),
        ".png"
      )
      cached_png <- file.path(preview_cache_dir, cache_key)
      src_info <- file.info(src_path)
      cache_ok <- file.exists(cached_png)
      if (cache_ok) {
        cache_info <- file.info(cached_png)
        cache_ok <- isTRUE(!is.na(src_info$mtime) && !is.na(cache_info$mtime) && cache_info$mtime >= src_info$mtime)
      }
      if (!cache_ok) {
        ok <- tryCatch({
          magick::image_read(src_path) |>
            magick::image_write(path = cached_png, format = "png")
          TRUE
        }, error = function(e) FALSE)
        cache_ok <- isTRUE(ok) && file.exists(cached_png)
      }
      if (cache_ok) {
        src_path <- cached_png
        content_type <- "image/png"
      } else {
        note <- "This TIFF image could not be converted for browser display. Try installing or checking the `magick` package in the app R environment."
      }
    } else {
      note <- "This image is TIFF. For reliable in-app display, install the R package `magick` in the same R environment that runs the app."
    }
  }

  list(src = src_path, content_type = content_type, note = note)
}

segmentation_mask_files <- function(settings) {
  p <- build_paths(settings)
  if (!dir.exists(p$out_seg)) return(character())
  files <- list.files(p$out_seg, pattern = "\\.(tif|tiff|png|jpg|jpeg)$", full.names = TRUE, ignore.case = TRUE)
  files[file.exists(files) & !dir.exists(files)]
}

seg_label_values <- function(settings) {
  c(
    neurites = suppressWarnings(as.integer(settings$SEG_LABEL_NEURITES %||% "3")),
    cell_bodies = suppressWarnings(as.integer(settings$SEG_LABEL_CELL_BODIES %||% "1")),
    background = suppressWarnings(as.integer(settings$SEG_LABEL_BACKGROUND %||% "255"))
  )
}

read_mask_pixels_magick <- function(path) {
  if (!requireNamespace("magick", quietly = TRUE)) {
    return(list(ok = FALSE, message = "The R package `magick` is required to inspect segmentation masks in the app. Install it in the same R environment that runs this Shiny app.", values = integer(), width = NA_integer_, height = NA_integer_))
  }
  tryCatch({
    img <- magick::image_read(path)[1]
    dat <- magick::image_data(img, channels = "gray")
    channel <- dat[1, , ]
    vals <- if (is.raw(channel)) {
      as.integer(channel)
    } else if (is.character(channel)) {
      strtoi(channel, base = 16L)
    } else {
      as.integer(channel)
    }
    dims <- dim(channel)
    list(ok = TRUE, message = "", values = vals, width = dims[1], height = dims[2])
  }, error = function(e) {
    list(ok = FALSE, message = paste("Could not read this mask image:", conditionMessage(e)), values = integer(), width = NA_integer_, height = NA_integer_)
  })
}

mask_histogram_info <- function(path) {
  px <- read_mask_pixels_magick(path)
  if (!isTRUE(px$ok)) return(list(ok = FALSE, message = px$message, counts = integer(), peaks = integer(), width = px$width, height = px$height))
  vals <- px$values
  vals <- vals[is.finite(vals) & vals >= 0 & vals <= 255]
  counts <- tabulate(vals + 1L, nbins = 256)
  peaks <- which(counts > 0) - 1L
  list(ok = TRUE, message = "", counts = counts, peaks = peaks, width = px$width, height = px$height, values = vals)
}

label_mapping_status <- function(peaks, settings) {
  labels <- seg_label_values(settings)
  expected <- as.integer(labels)
  missing <- setdiff(expected, peaks)
  extra <- setdiff(peaks, expected)
  duplicate_labels <- names(labels)[duplicated(expected)]
  if (length(peaks) != 3L) {
    return(list(ok = FALSE, class = "step-summary-missing", message = sprintf("Expected exactly 3 label peaks, but detected %s: %s.", length(peaks), paste(peaks, collapse = ", "))))
  }
  if (length(missing) || length(extra) || length(unique(expected)) != 3L) {
    details <- c()
    if (length(missing)) details <- c(details, sprintf("Missing configured label value(s): %s.", paste(missing, collapse = ", ")))
    if (length(extra)) details <- c(details, sprintf("Detected unassigned peak value(s): %s.", paste(extra, collapse = ", ")))
    if (length(unique(expected)) != 3L) details <- c(details, "The three class assignments must use three different pixel values.")
    return(list(ok = FALSE, class = "step-summary-warn", message = paste(details, collapse = " ")))
  }
  list(ok = TRUE, class = "step-summary-good", message = sprintf("Detected peaks match the configured labels: neurites=%s, cell bodies=%s, background=%s.", labels[["neurites"]], labels[["cell_bodies"]], labels[["background"]]))
}

write_label_mapping_preview <- function(path, settings) {
  out_dir <- file.path(project_root, "cache", "label_mapping_previews")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, paste0(gsub("[^A-Za-z0-9._-]+", "_", tools::file_path_sans_ext(basename(path))), "_labels.png"))
  info <- read_mask_pixels_magick(path)
  if (!isTRUE(info$ok)) return(list(ok = FALSE, path = "", message = info$message))
  labels <- seg_label_values(settings)
  vals <- info$values
  if (!length(vals) || !is.finite(info$width) || !is.finite(info$height)) {
    return(list(ok = FALSE, path = "", message = "Mask image has no readable pixel values."))
  }
  mat <- matrix(vals, nrow = info$width, ncol = info$height)
  mat <- t(mat)
  h <- nrow(mat)
  w <- ncol(mat)
  rgb <- array(0, dim = c(h, w, 3))
  rgb[, , 1] <- ifelse(mat == labels[["neurites"]], 1, 0)
  rgb[, , 3] <- ifelse(mat == labels[["cell_bodies"]], 1, 0)
  rgb[, , 1] <- pmax(rgb[, , 1], ifelse(mat == labels[["background"]], 1, 0))
  rgb[, , 2] <- pmax(rgb[, , 2], ifelse(mat == labels[["background"]], 0.85, 0))
  ok <- tryCatch({
    grDevices::png(out_path, width = w, height = h)
    op <- par(mar = c(0, 0, 0, 0))
    on.exit({ par(op); grDevices::dev.off() }, add = TRUE)
    plot.new()
    rasterImage(rgb, 0, 0, 1, 1, interpolate = FALSE)
    TRUE
  }, error = function(e) {
    try(grDevices::dev.off(), silent = TRUE)
    FALSE
  })
  if (!ok || !file.exists(out_path)) return(list(ok = FALSE, path = "", message = "Could not write the colored label preview."))
  list(ok = TRUE, path = normalizePath(out_path, winslash = "/", mustWork = FALSE), message = "")
}

detect_runtime_choices <- function() {
  choices <- c()
  if (.Platform$OS.type != "windows" || nzchar(Sys.getenv("WSL_DISTRO_NAME"))) {
    choices["Current Linux or WSL session"] <- "posix"
  }
  if (nzchar(Sys.which("wsl"))) {
    choices["Windows app controlling WSL"] <- "wsl"
  }
  if (!length(choices)) choices["No supported runtime detected"] <- "none"
  choices
}

build_paths <- function(settings) {
  output_root <- normalize_project_path(settings$OUTPUT_ROOT_DIR_NAME %||% "pipeline_outputs")
  out_opt <- file.path(output_root, settings$OUT_OPTIMIZE_DIR_NAME %||% "OUT_optimize")
  input_workspace <- normalize_project_path(settings$INPUT_WORKSPACE_DIR_NAME %||% "pipeline_inputs")
  list(
    input_workspace = input_workspace,
    input_master = effective_input_master_dir(settings),
    output_root = output_root,
    pre_renamed = file.path(output_root, settings$PRE_RENAMED_DIR_NAME %||% "PRE_renamed"),
    image_list = file.path(output_root, settings$IMAGE_INPUT_LIST_NAME %||% "OUT_img_input1.txt"),
    out_clear = file.path(output_root, settings$OUT_CLEAR_BCGND_DIR_NAME %||% "OUT_clear_bcgnd"),
    out_seg = file.path(output_root, settings$OUT_SEGMENTATION_DIR_NAME %||% "OUT_segmentation"),
    out_opt = out_opt,
    metadata = effective_metadata_file(settings),
    ilastik_project = effective_ilastik_project_file(settings),
    mask_percentages = file.path(out_opt, settings$MASK_PERCENTAGES_FILE %||% "mask_area_percentages.csv"),
    groups = file.path(out_opt, settings$OPTIM_GROUPS_FILE %||% "mask_area_percentages+optim_groups.csv"),
    inspected = file.path(out_opt, settings$OPTIM_GROUPS_INSPECTED_FILE %||% "mask_area_percentages+optim_groups+inspected.csv"),
    opt_rds = file.path(out_opt, settings$OPT_RDS_FILE %||% "optROIs_data.rds"),
    param_results = file.path(out_opt, "parameter_optimization_results.csv"),
    filtered_rois = file.path(out_opt, "filtered_rois_best_params.csv"),
    generalized = file.path(output_root, "OUT_generalized_filtering", "skeletons_filtered_generalized.rds"),
    pca_rds = file.path(output_root, "OUT_metadata_PCA", "PCA_metadata_objects.rds"),
    final_dir = file.path(output_root, "OUT_biological_analysis_enhanced")
  )
}

validation_group_map <- function() {
  c(
    high_neurites_low_confluence = "High neurites / low confluence",
    low_neurites_high_confluence = "Low neurites / high confluence",
    high_neurites_high_confluence = "High neurites / high confluence",
    low_neurites_low_confluence = "Low neurites / low confluence"
  )
}

validation_group_dirs <- function(settings) {
  p <- build_paths(settings)
  base <- p$out_opt
  groups <- validation_group_map()
  stats::setNames(file.path(base, names(groups)), names(groups))
}

validation_group_images <- function(group_dir) {
  if (!dir.exists(group_dir)) return(character())
  files <- list.files(group_dir, full.names = TRUE)
  files <- files[file.exists(files) & !dir.exists(files)]
  files <- files[grepl("\\.(tif|tiff|png|jpg|jpeg)$", files, ignore.case = TRUE)]
  files[order(basename(files))]
}

manual_review_manifest_path <- function(settings) {
  file.path(build_paths(settings)$out_opt, "manual_review_labels.csv")
}

read_manual_review_manifest <- function(settings) {
  path <- manual_review_manifest_path(settings)
  if (!file.exists(path)) {
    return(data.frame(group_id = character(), image_name = character(), manual_label = character(), source_path = character(), updated_at = character(), stringsAsFactors = FALSE))
  }
  out <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (is.null(out) || !nrow(out)) {
    return(data.frame(group_id = character(), image_name = character(), manual_label = character(), source_path = character(), updated_at = character(), stringsAsFactors = FALSE))
  }
  needed <- c("group_id", "image_name", "manual_label", "source_path", "updated_at")
  for (nm in setdiff(needed, names(out))) out[[nm]] <- ""
  out <- out[needed]
  out[nzchar(out$image_name), , drop = FALSE]
}

write_manual_review_manifest <- function(settings, manifest_df) {
  path <- manual_review_manifest_path(settings)
  utils::write.csv(manifest_df, path, row.names = FALSE, na = "")
}

manual_labels_for_group <- function(group_dir, settings) {
  group_id <- basename(group_dir)
  manifest <- read_manual_review_manifest(settings)
  sub <- manifest[manifest$group_id == group_id, , drop = FALSE]
  labels <- c("choice", "noise", "not_sure")
  out <- setNames(vector("list", length(labels)), labels)
  for (lab in labels) {
    vals <- sub$image_name[sub$manual_label == lab]
    out[[lab]] <- sort(unique(vals[nzchar(vals)]))
  }
  out
}

manual_label_summary_all_groups <- function(settings) {
  group_dirs <- validation_group_dirs(settings)
  group_labels <- validation_group_map()
  do.call(rbind, lapply(names(group_dirs), function(group_id) {
    labels <- manual_labels_for_group(group_dirs[[group_id]], settings)
    data.frame(
      group_id = group_id,
      group_label = unname(group_labels[[group_id]]),
      choice = length(labels$choice),
      noise = length(labels$noise),
      not_sure = length(labels$not_sure),
      unlabeled = max(length(validation_group_images(group_dirs[[group_id]])) - length(unique(c(labels$choice, labels$noise, labels$not_sure))), 0L),
      stringsAsFactors = FALSE
    )
  }))
}

manual_review_integrity <- function(settings) {
  group_dirs <- validation_group_dirs(settings)
  manifest <- read_manual_review_manifest(settings)
  expected_pairs <- unlist(lapply(names(group_dirs), function(group_id) {
    imgs <- basename(validation_group_images(group_dirs[[group_id]]))
    imgs <- imgs[nzchar(imgs)]
    paste(group_id, imgs, sep = "::")
  }), use.names = FALSE)
  expected_pairs <- unique(expected_pairs[nzchar(expected_pairs)])
  labeled_pairs <- unique(paste(manifest$group_id[nzchar(manifest$group_id)], manifest$image_name[nzchar(manifest$image_name)], sep = "::"))
  labeled_pairs <- labeled_pairs[nzchar(labeled_pairs)]
  dropouts <- sort(setdiff(expected_pairs, labeled_pairs))
  extras <- sort(setdiff(labeled_pairs, expected_pairs))
  list(
    manifest_path = manual_review_manifest_path(settings),
    expected_count = length(expected_pairs),
    found_count = length(intersect(expected_pairs, labeled_pairs)),
    found_total_count = length(labeled_pairs),
    dropouts = dropouts,
    extras = extras,
    has_manifest = file.exists(manual_review_manifest_path(settings))
  )
}

set_manual_label <- function(group_dir, image_file, label = c("choice", "noise", "not_sure", "clear")) {
  label <- match.arg(label)
  settings <- read_settings_file(settings_path)
  group_id <- basename(group_dir)
  image_name <- basename(image_file)
  manifest <- read_manual_review_manifest(settings)
  manifest <- manifest[!(manifest$group_id == group_id & manifest$image_name == image_name), , drop = FALSE]

  if (identical(label, "clear")) {
    write_manual_review_manifest(settings, manifest)
    return(list(ok = TRUE, message = "Cleared manual label."))
  }

  new_row <- data.frame(
    group_id = group_id,
    image_name = image_name,
    manual_label = label,
    source_path = normalizePath(image_file, winslash = "/", mustWork = FALSE),
    updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    stringsAsFactors = FALSE
  )
  manifest <- rbind(manifest, new_row)
  write_manual_review_manifest(settings, manifest)
  list(
    ok = TRUE,
    message = sprintf("Saved the manual label '%s' for %s.", label, image_name)
  )
}

list_validation_profiles <- function(settings) {
  roots <- unique(c(
    normalize_project_path(settings$VALIDATION_PROFILE_DIR_NAME %||% "cache/validation_profiles"),
    normalize_project_path("cache/optimization_profiles")
  ))
  rows <- list()
  for (root_dir in roots) {
    if (!dir.exists(root_dir)) next
    for (d in list.dirs(root_dir, recursive = FALSE, full.names = TRUE)) {
      info_file <- file.path(d, "profile_info.txt")
      created <- ""
      if (file.exists(info_file)) {
        lines <- readLines(info_file, warn = FALSE)
        hit <- grep("^created_at=", lines, value = TRUE)
        if (length(hit)) created <- sub("^created_at=", "", hit[1])
      }
      rows[[length(rows) + 1]] <- data.frame(
        profile_name = basename(d),
        source = basename(dirname(d)),
        created_at = created,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(data.frame(profile_name = character(), source = character(), created_at = character()))
  out <- do.call(rbind, rows)
  out[order(out$profile_name, decreasing = TRUE), , drop = FALSE]
}

validation_profile_root_for_source <- function(source, settings) {
  configured <- normalize_project_path(settings$VALIDATION_PROFILE_DIR_NAME %||% "cache/validation_profiles")
  if (identical(source %||% "", basename(configured))) return(configured)
  normalize_project_path(file.path("cache", source %||% "validation_profiles"))
}

validation_profile_dir <- function(profile_name, settings) {
  profile_name <- profile_name %||% ""
  if (!nzchar(profile_name) || identical(profile_name, "__current__")) return(NA_character_)
  prof <- list_validation_profiles(settings)
  row <- prof[prof$profile_name == profile_name, , drop = FALSE]
  if (!nrow(row)) return(NA_character_)
  file.path(validation_profile_root_for_source(row$source[[1]], settings), profile_name)
}

read_csv_safe_file <- function(path) {
  if (!nzchar(path %||% "") || !file.exists(path) || dir.exists(path)) return(data.frame())
  tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
}

cutoff_source_paths <- function(source_id, settings) {
  p <- build_paths(settings)
  if (identical(source_id %||% "__current__", "__current__")) {
    root <- p$out_opt
    label <- "Current working outputs"
  } else {
    root <- validation_profile_dir(source_id, settings)
    label <- source_id
  }
  list(
    id = source_id %||% "__current__",
    label = label,
    root = root,
    params = file.path(root, "parameter_optimization_results.csv"),
    filtered = file.path(root, "filtered_rois_best_params.csv"),
    raw = file.path(root, "roi_analysis_raw.csv"),
    raw_rds = file.path(root, "roi_analysis_raw.rds"),
    manifest = file.path(root, "roi_analysis_raw_manifest.csv"),
    diagnostics = file.path(root, "optimization_input_diagnostics.csv"),
    model_metadata = file.path(root, "optimization_model_metadata.csv"),
    opt_rois_rds = file.path(root, "optROIs_data.rds"),
    metadata = file.path(root, "profile_metadata.tsv"),
    settings = file.path(root, "profile_settings.env")
  )
}

cutoff_source_summary <- function(source_id, settings) {
  src <- cutoff_source_paths(source_id, settings)
  params <- read_csv_safe_file(src$params)
  filtered <- read_csv_safe_file(src$filtered)
  raw <- read_csv_safe_file(src$raw)
  manifest <- read_csv_safe_file(src$manifest)
  diagnostics <- read_csv_safe_file(src$diagnostics)
  model_metadata <- read_csv_safe_file(src$model_metadata)
  model_value <- function(key, fallback = "") {
    if (!nrow(model_metadata) || !all(c("key", "value") %in% names(model_metadata))) return(fallback)
    hit <- model_metadata$value[model_metadata$key == key]
    if (length(hit) && nzchar(hit[[1]] %||% "")) hit[[1]] else fallback
  }
  if (nrow(params) && "score" %in% names(params)) {
    params$score <- suppressWarnings(as.numeric(params$score))
    params <- params[order(params$score, decreasing = TRUE), , drop = FALSE]
  }
  best <- if (nrow(params)) params[1, , drop = FALSE] else data.frame()
  num <- function(df, col) {
    if (!nrow(df) || !col %in% names(df)) return(NA_real_)
    suppressWarnings(as.numeric(df[[col]][1]))
  }
  bool_sum <- function(df, col) {
    if (!nrow(df) || !col %in% names(df)) return(NA_real_)
    vals <- df[[col]]
    if (is.logical(vals)) return(sum(vals, na.rm = TRUE))
    vals <- tolower(trimws(as.character(vals)))
    sum(vals %in% c("true", "t", "1", "yes", "y"), na.rm = TRUE)
  }
  raw_n <- nrow(raw)
  filtered_n <- nrow(filtered)
    out <- data.frame(
      Profile = src$label,
      Source = if (identical(source_id %||% "__current__", "__current__")) "working" else "saved profile",
      Score = num(best, "score"),
    Retained_ROIs = if (is.finite(num(best, "n_retained"))) num(best, "n_retained") else filtered_n,
    Raw_ROIs = raw_n,
    Retention_Pct = if (raw_n > 0) round(100 * filtered_n / raw_n, 2) else NA_real_,
    Neurite_Separation = num(best, "neurite_separation"),
    Confluence_Separation = num(best, "confluence_separation"),
    Signal_Noise_Ratio = num(best, "signal_noise_ratio"),
    Min_Area = num(best, "min_area"),
    Max_Area = num(best, "max_area"),
    Min_Circularity = num(best, "min_circularity"),
    Max_Circularity = num(best, "max_circularity"),
    Min_Aspect = num(best, "min_aspect_ratio"),
    Max_Aspect = num(best, "max_aspect_ratio"),
    Signal_Rows = bool_sum(raw, "is_signal"),
    Noise_Rows = bool_sum(raw, "is_noise"),
    Optimization_Mode = model_value("optimization_mode", if (nrow(best) && "optimization_mode" %in% names(best)) best$optimization_mode[[1]] else ""),
    Scoring_Mode = if (nrow(diagnostics) && "scoring_mode" %in% names(diagnostics)) diagnostics$scoring_mode[[1]] else if (nrow(best) && "scoring_mode" %in% names(best)) best$scoring_mode[[1]] else "",
    Length_Weighted_Retention = num(best, "length_weighted_retention"),
    Fragmentation_Penalty = num(best, "fragmentation_penalty"),
    Topology_Score = num(best, "topology_score"),
    Dominant_Path_Ratio = num(best, "dominant_path_ratio"),
    Mesh_Penalty = num(best, "mesh_penalty"),
      Orientation_Coherence = num(best, "orientation_coherence"),
      Created = if (nrow(manifest) && "created_at" %in% names(manifest)) manifest$created_at[[1]] else "",
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  out$Net_Biological_Utility <- round(
    rowSums(cbind(
      pmax(0, out$Neurite_Separation),
      pmax(0, out$Length_Weighted_Retention),
      pmax(0, out$Dominant_Path_Ratio),
      pmax(0, out$Orientation_Coherence),
      -pmax(0, out$Confluence_Separation),
      -pmax(0, out$Fragmentation_Penalty),
      -pmax(0, out$Mesh_Penalty)
    ), na.rm = TRUE),
    4
  )
  out$Specificity_Risk <- round(rowSums(cbind(
    pmax(0, out$Confluence_Separation),
    pmax(0, out$Fragmentation_Penalty),
    pmax(0, out$Mesh_Penalty)
  ), na.rm = TRUE), 4)
  out$Recovery_Proxy <- round(rowSums(cbind(
    pmax(0, out$Retention_Pct / 100),
    pmax(0, out$Length_Weighted_Retention),
    pmax(0, out$Dominant_Path_Ratio)
  ), na.rm = TRUE), 4)
  out
}

sanitize_profile_name <- function(name) {
  name <- trimws(name %||% "")
  name <- gsub("[^A-Za-z0-9._-]+", "_", name)
  name <- gsub("_+", "_", name)
  name <- gsub("^_+|_+$", "", name)
  if (!nzchar(name)) name <- paste0(format(Sys.Date()), "_validated_cutoffs")
  name
}

profile_name_exists <- function(profile_name, settings) {
  profile_name <- sanitize_profile_name(profile_name)
  prof <- list_validation_profiles(settings)
  nrow(prof) > 0 && profile_name %in% prof$profile_name
}

suggest_unique_profile_name <- function(settings, base_name = paste0(format(Sys.Date()), "_validated_cutoffs")) {
  base_name <- sanitize_profile_name(base_name)
  prof <- list_validation_profiles(settings)
  existing <- if (nrow(prof)) prof$profile_name else character()
  if (!base_name %in% existing) return(base_name)
  for (i in 2:999) {
    candidate <- sprintf("%s_run%02d", base_name, i)
    if (!candidate %in% existing) return(candidate)
  }
  paste0(base_name, "_", format(Sys.time(), "%H%M%S"))
}

run_runtime_command <- function(command, runtime) {
  if (runtime == "none") {
    return(list(status = 1L, output = "No supported runtime is available.", command = command))
  }
  if (runtime == "wsl") {
    full_cmd <- sprintf("cd %s && %s", bash_quote(to_wsl_path(project_root)), command)
    out <- tryCatch(system2("wsl", c("bash", "-lc", full_cmd), stdout = TRUE, stderr = TRUE),
                    error = function(e) structure(e$message, status = 1L))
  } else {
    full_cmd <- sprintf("cd %s && %s", bash_quote(project_root), command)
    out <- tryCatch(system2("bash", c("-lc", full_cmd), stdout = TRUE, stderr = TRUE),
                    error = function(e) structure(e$message, status = 1L))
  }
  list(status = as.integer(attr(out, "status") %||% 0L), output = paste(out, collapse = "\n"), command = command)
}

safe_system_first_line <- function(command, args = character()) {
  if (!nzchar(command)) return("")
  out <- tryCatch(system2(command, args, stdout = TRUE, stderr = TRUE), error = function(e) character())
  out <- trimws(out)
  out <- out[nzchar(out)]
  if (!length(out)) "" else out[1]
}

infer_version_from_string <- function(text) {
  if (!nzchar(text)) return("")
  m <- regexpr("[0-9]+(?:\\.[0-9A-Za-z_-]+)+", text, perl = TRUE)
  if (m[1] > 0) regmatches(text, m)[1] else text
}

infer_version_from_path <- function(path) {
  if (!nzchar(path)) return("")
  parts <- strsplit(gsub("\\\\", "/", path), "/", fixed = FALSE)[[1]]
  parts <- rev(parts[nzchar(parts)])
  for (part in parts) {
    ver <- infer_version_from_string(part)
    if (nzchar(ver) && !identical(ver, part)) return(ver)
  }
  ""
}

configured_tool_version <- function(path, label) {
  path <- path %||% ""
  if (!nzchar(path)) {
    return(data.frame(
      component = label,
      scope = "configured path",
      source = "not configured",
      version = "",
      stringsAsFactors = FALSE
    ))
  }
  resolved <- normalize_project_path(path, project_root)
  exists_now <- file.exists(resolved) || dir.exists(resolved)
  inferred <- infer_version_from_path(resolved)
  version_text <- if (nzchar(inferred)) inferred else if (exists_now) "version not inferable from path" else "path not found"
  data.frame(
    component = label,
    scope = "configured path",
    source = resolved,
    version = version_text,
    stringsAsFactors = FALSE
  )
}

scan_project_r_packages <- function(root = project_root) {
  script_files <- unique(c(
    file.path(root, "app.R"),
    list.files(root, pattern = "\\.[Rr]$", full.names = TRUE),
    list.files(file.path(root, "launchers"), pattern = "\\.[Rr]$", full.names = TRUE, recursive = TRUE)
  ))
  script_files <- script_files[file.exists(script_files)]
  packages <- character()
  for (path in script_files) {
    lines <- tryCatch(readLines(path, warn = FALSE), error = function(e) character())
    if (!length(lines)) next
    text <- paste(lines, collapse = "\n")
    patterns <- c(
      "library\\((['\"]?)([A-Za-z][A-Za-z0-9._]*)\\1\\)",
      "require\\((['\"]?)([A-Za-z][A-Za-z0-9._]*)\\1\\)",
      "([A-Za-z][A-Za-z0-9._]*)::"
    )
    for (pat in patterns) {
      matches <- gregexpr(pat, text, perl = TRUE)
      if (matches[[1]][1] < 0) next
      captures <- regmatches(text, matches)[[1]]
      if (!length(captures)) next
      for (hit in captures) {
        pkg <- if (grepl("::", hit, fixed = TRUE)) {
          sub("::.*$", "", hit)
        } else {
          tmp <- sub("^(library|require)\\((['\"]?)", "", hit)
          sub("(['\"]?)\\)$", "", tmp)
        }
        pkg <- trimws(pkg)
        if (nzchar(pkg)) packages <- c(packages, pkg)
      }
    }
  }
  sort(unique(packages))
}

current_software_versions <- function(settings) {
  bash_path <- Sys.which("bash")
  wsl_path <- Sys.which("wsl")
  java_path <- Sys.which("java")
  rows <- list(
    data.frame(component = "R session", scope = "current session", source = R.home(), version = R.version.string, stringsAsFactors = FALSE),
    data.frame(component = "Bash", scope = "current session", source = bash_path %||% "", version = if (nzchar(bash_path)) safe_system_first_line(bash_path, "--version") else "not found", stringsAsFactors = FALSE),
    data.frame(component = "WSL", scope = "current session", source = wsl_path %||% "", version = if (nzchar(wsl_path)) safe_system_first_line(wsl_path, "--version") else "not found", stringsAsFactors = FALSE),
    data.frame(component = "Java", scope = "current session", source = java_path %||% "", version = if (nzchar(java_path)) safe_system_first_line(java_path, "-version") else "not found", stringsAsFactors = FALSE),
    configured_tool_version(settings$FIJI_BIN %||% "", "Fiji / ImageJ"),
    configured_tool_version(settings$ILASTIK_BIN %||% "", "ilastik"),
    configured_tool_version(effective_ilastik_project_file(settings), "ilastik project")
  )
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

current_package_versions <- function(root = project_root) {
  pkgs <- scan_project_r_packages(root)
  if (!length(pkgs)) {
    return(data.frame(package = "No packages detected from project scripts", installed = "", version = "", stringsAsFactors = FALSE))
  }
  data.frame(
    package = pkgs,
    installed = vapply(pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1)),
    version = vapply(pkgs, function(pkg) if (requireNamespace(pkg, quietly = TRUE)) as.character(utils::packageVersion(pkg)) else "not installed", character(1)),
    stringsAsFactors = FALSE
  )
}

runtime_root_arg <- function(runtime) {
  if (runtime == "wsl") to_wsl_path(project_root) else project_root
}

runtime_path_arg <- function(path, runtime) {
  resolved <- normalize_project_path(path %||% ".", project_root)
  if (runtime == "wsl") to_wsl_path(resolved) else resolved
}

available_root_places <- function() {
  places <- list(
    "Project root" = project_root,
    "Home" = normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE)
  )
  docs <- normalizePath(file.path(path.expand("~"), "Documents"), winslash = "/", mustWork = FALSE)
  desk <- normalizePath(file.path(path.expand("~"), "Desktop"), winslash = "/", mustWork = FALSE)
  if (dir.exists(docs)) places[["Documents"]] <- docs
  if (dir.exists(desk)) places[["Desktop"]] <- desk

  if (.Platform$OS.type == "windows") {
    drive_letters <- LETTERS[vapply(LETTERS, function(d) dir.exists(paste0(d, ":/")), logical(1))]
    for (d in drive_letters) {
      places[[paste0("Drive ", d)]] <- paste0(d, ":/")
    }
  } else {
    places[["Filesystem root"]] <- "/"
    mnt <- "/mnt"
    if (dir.exists(mnt)) {
      mount_dirs <- list.dirs(mnt, recursive = FALSE, full.names = TRUE)
      for (mp in mount_dirs) {
        places[[paste0("Mount ", basename(mp))]] <- normalizePath(mp, winslash = "/", mustWork = FALSE)
      }
    }
  }

  places
}

ensure_run_log_store <- function() {
  dir.create(run_log_dir, recursive = TRUE, showWarnings = FALSE)
}

empty_history <- function() {
  data.frame(
    run_id = character(),
    category = character(),
    step_id = character(),
    title = character(),
    runtime = character(),
    machine_signature = character(),
    cpu_cores = numeric(),
    task_size = numeric(),
    task_size_label = character(),
    seconds_per_unit = numeric(),
    command = character(),
    started_at = character(),
    finished_at = character(),
    duration_seconds = numeric(),
    status = character(),
    exit_status = integer(),
    log_file = character(),
    stringsAsFactors = FALSE
  )
}

read_run_history <- function() {
  ensure_run_log_store()
  if (!file.exists(run_history_file)) {
    return(empty_history())
  }
  out <- tryCatch(read.csv(run_history_file, stringsAsFactors = FALSE), error = function(e) empty_history())
  template <- empty_history()
  for (nm in names(template)) {
    if (!nm %in% names(out)) {
      proto <- template[[nm]]
      if (!nrow(out)) {
        out[[nm]] <- proto
      } else if (is.numeric(proto)) {
        out[[nm]] <- rep(NA_real_, nrow(out))
      } else if (is.integer(proto)) {
        out[[nm]] <- rep(NA_integer_, nrow(out))
      } else {
        out[[nm]] <- rep("", nrow(out))
      }
    }
  }
  out
}

write_run_history <- function(df) {
  ensure_run_log_store()
  write.csv(df, run_history_file, row.names = FALSE)
}

append_run_history <- function(entry) {
  history <- read_run_history()
  history <- rbind(history, as.data.frame(entry, stringsAsFactors = FALSE))
  write_run_history(history)
}

seconds_per_unit_value <- function(duration_seconds, task_size) {
  duration_seconds <- as.numeric(duration_seconds)
  task_size <- as.numeric(task_size)
  if (!is.finite(duration_seconds) || !is.finite(task_size) || task_size <= 0) return(NA_real_)
  duration_seconds / task_size
}

history_table_view <- function(history_df, n = 20) {
  if (!nrow(history_df)) return(data.frame(message = "No commands have been completed and recorded yet."))
  history_df$started_at_parsed <- suppressWarnings(as.POSIXct(history_df$started_at, format = "%Y-%m-%d %H:%M:%S", tz = Sys.timezone()))
  history_df <- history_df[order(history_df$started_at_parsed, decreasing = TRUE), , drop = FALSE]

  step_ids <- history_df$step_id
  step_ids[is.na(step_ids)] <- ""
  titles <- history_df$title
  titles[is.na(titles)] <- ""
  key <- ifelse(nzchar(step_ids), step_ids, titles)
  keep_idx <- !duplicated(key)
  view <- history_df[keep_idx, , drop = FALSE]
  view <- utils::head(view, n)

  out <- view[, c("started_at", "title", "runtime", "cpu_cores", "task_size_label", "task_size", "seconds_per_unit", "duration_seconds", "status"), drop = FALSE]
  out$cpu_cores <- ifelse(is.na(out$cpu_cores), "", out$cpu_cores)
  out$task_size <- ifelse(is.na(out$task_size), "", round(as.numeric(out$task_size), 2))
  out$seconds_per_unit <- ifelse(is.na(out$seconds_per_unit), "", round(as.numeric(out$seconds_per_unit), 4))
  out$duration_seconds <- vapply(out$duration_seconds, format_seconds, character(1))
  names(out) <- c("started_at", "title", "runtime", "cpu_cores", "task_metric", "task_size", "sec_per_unit", "duration", "status")
  out
}

format_seconds <- function(x) {
  x <- as.numeric(x %||% 0)
  if (!is.finite(x)) return("unknown")
  mins <- floor(x / 60)
  secs <- round(x %% 60)
  if (mins <= 0) return(sprintf("%ss", secs))
  sprintf("%sm %ss", mins, secs)
}

machine_info <- function() {
  list(
    machine_signature = paste(
      Sys.info()[["sysname"]] %||% "unknown-os",
      Sys.info()[["release"]] %||% "unknown-release",
      Sys.info()[["machine"]] %||% "unknown-arch",
      sep = "|"
    ),
    cpu_cores = tryCatch(parallel::detectCores(logical = TRUE), error = function(e) NA_real_)
  )
}

count_files_by_pattern <- function(path, pattern) {
  if (!dir.exists(path)) return(0)
  length(list.files(path, pattern = pattern, full.names = TRUE, recursive = TRUE, ignore.case = TRUE))
}

delete_path_safely <- function(path) {
  if (!nzchar(path)) return(invisible(FALSE))
  if (dir.exists(path)) {
    unlink(path, recursive = TRUE, force = TRUE)
    return(invisible(TRUE))
  }
  if (file.exists(path)) {
    unlink(path, force = TRUE)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

safe_csv_row_count <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  out <- tryCatch(nrow(utils::read.csv(path, stringsAsFactors = FALSE)), error = function(e) NA_integer_)
  as.numeric(out)
}

safe_selected_manual_row_count <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  tbl <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (is.null(tbl) || !nrow(tbl)) return(NA_real_)
  choice_col <- if ("_manual_choise" %in% names(tbl)) "_manual_choise" else if ("manual_choice" %in% names(tbl)) "manual_choice" else NA_character_
  noise_col <- if ("manual_noise" %in% names(tbl)) "manual_noise" else NA_character_
  choice <- if (!is.na(choice_col)) suppressWarnings(as.integer(tbl[[choice_col]])) else rep(0L, nrow(tbl))
  noise <- if (!is.na(noise_col)) suppressWarnings(as.integer(tbl[[noise_col]])) else rep(0L, nrow(tbl))
  as.numeric(sum((choice %in% 1L) | (noise %in% 1L), na.rm = TRUE))
}

safe_rds_object_count <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj)) return(NA_real_)
  if (is.data.frame(obj)) return(as.numeric(nrow(obj)))
  if (is.matrix(obj)) return(as.numeric(nrow(obj)))
  if (is.list(obj)) {
    if (all(vapply(obj, function(x) is.list(x) && "rois" %in% names(x), logical(1)))) {
      return(as.numeric(sum(vapply(obj, function(x) length(x$rois %||% list()), numeric(1)))))
    }
    if (all(vapply(obj, function(x) is.atomic(x) || is.null(x), logical(1)))) {
      return(as.numeric(length(obj)))
    }
  }
  NA_real_
}

safe_rds_image_record_count <- function(path) {
  if (!file.exists(path)) return(NA_real_)
  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj)) return(NA_real_)
  if (is.data.frame(obj)) return(as.numeric(nrow(obj)))
  if (is.matrix(obj)) return(as.numeric(nrow(obj)))
  if (is.list(obj)) return(as.numeric(length(obj)))
  NA_real_
}

safe_optrois_summary_counts <- function(out_opt) {
  path <- file.path(out_opt, "optROIs_summary.csv")
  empty <- list(path = path, rows = NA_real_, roi_count = NA_real_, mtime = NA_real_)
  if (!file.exists(path)) return(empty)
  tbl <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (is.null(tbl)) return(empty)
  roi_count <- if ("roi_count" %in% names(tbl)) sum(suppressWarnings(as.numeric(tbl$roi_count)), na.rm = TRUE) else NA_real_
  list(
    path = path,
    rows = as.numeric(nrow(tbl)),
    roi_count = as.numeric(roi_count),
    mtime = as.numeric(file.info(path)$mtime)
  )
}

safe_read_lines <- function(path) {
  if (!file.exists(path)) return(character())
  tryCatch(trimws(readLines(path, warn = FALSE)), error = function(e) character())
}

safe_csv_column_values <- function(path, candidates) {
  if (!file.exists(path)) return(character())
  tbl <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (is.null(tbl) || !nrow(tbl)) return(character())
  hit <- candidates[candidates %in% names(tbl)]
  if (!length(hit)) return(character())
  vals <- tbl[[hit[1]]]
  vals <- as.character(vals %||% character())
  unique(trimws(vals[nzchar(vals)]))
}

produced_image_basenames <- function(path, pattern = NULL) {
  if (!dir.exists(path)) return(character())
  files <- list.files(path, recursive = TRUE, full.names = FALSE)
  if (!is.null(pattern)) files <- files[grepl(pattern, files, ignore.case = TRUE)]
  files <- basename(files[nzchar(files)])
  unique(files)
}

rename_expected_names <- function(image_paths, search_root) {
  image_paths <- image_paths[nzchar(image_paths)]
  if (!length(image_paths)) return(character())
  search_root <- normalizePath(search_root, winslash = "/", mustWork = FALSE)
  vapply(image_paths, function(filepath) {
    fp <- normalizePath(filepath, winslash = "/", mustWork = FALSE)
    relpath <- sub(paste0("^", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", paste0(search_root, "/"))), "", fp)
    filedir <- dirname(relpath)
    filename <- basename(relpath)
    pathpart <- gsub("/", "-", filedir, fixed = TRUE)
    if (nzchar(pathpart) && pathpart != ".") paste0(pathpart, "-", filename) else filename
  }, character(1), USE.NAMES = FALSE)
}

fiji_expected_names <- function(image_list_path) {
  image_paths <- safe_read_lines(image_list_path)
  image_paths <- image_paths[nzchar(image_paths)]
  if (!length(image_paths)) return(character())
  vapply(image_paths, function(x) {
    nm <- basename(x)
    stem <- sub("\\.(tif|tiff|jpg|jpeg|png)$", "", nm, ignore.case = TRUE)
    paste0(stem, "_RGavg.tif")
  }, character(1), USE.NAMES = FALSE)
}

ilastik_expected_names <- function(out_clear_dir) {
  files <- produced_image_basenames(out_clear_dir, "\\.tif$")
  if (!length(files)) return(character())
  vapply(files, function(nm) sub("\\.tif$", "_mask_renorm.tif", nm, ignore.case = TRUE), character(1), USE.NAMES = FALSE)
}

ilastik_expected_names_with_suffix <- function(out_clear_dir, suffix = c("_mask_renorm.tif", "_mask.tif")) {
  suffix <- suffix[[1]]
  files <- produced_image_basenames(out_clear_dir, "\\.tif$")
  if (!length(files)) return(character())
  vapply(files, function(nm) sub("\\.tif$", suffix, nm, ignore.case = TRUE), character(1), USE.NAMES = FALSE)
}

detected_segmentation_suffix <- function(out_seg_dir, settings = NULL) {
  renorm_names <- produced_image_basenames(out_seg_dir, "_mask_renorm\\.tif$")
  if (length(renorm_names)) return("_mask_renorm.tif")
  mask_names <- produced_image_basenames(out_seg_dir, "_mask\\.tif$")
  if (length(mask_names)) return("_mask.tif")
  apply_renorm <- tolower(settings$APPLY_ILASTIK_RENORM %||% "true")
  if (identical(apply_renorm, "true")) "_mask_renorm.tif" else "_mask.tif"
}

segmentation_mask_names <- function(out_seg_dir, settings = NULL) {
  suffix <- detected_segmentation_suffix(out_seg_dir, settings)
  pattern <- if (identical(suffix, "_mask_renorm.tif")) "_mask_renorm\\.tif$" else "_mask\\.tif$"
  produced_image_basenames(out_seg_dir, pattern)
}

roi_zip_paths <- function(roi_root) {
  if (!dir.exists(roi_root)) return(character())
  roi_dirs <- list.dirs(roi_root, recursive = FALSE, full.names = TRUE)
  if (!length(roi_dirs)) return(character())
  candidates <- file.path(roi_dirs, paste0(basename(roi_dirs), ".tif_ROIs.zip"))
  candidates[file.exists(candidates)]
}

roi_payload_names <- function(roi_root) {
  if (!dir.exists(roi_root)) return(character())
  zip_files <- roi_zip_paths(roi_root)
  if (!length(zip_files)) return(character())
  unique(basename(dirname(zip_files)))
}

count_roi_payload_dirs <- function(roi_root) {
  length(roi_payload_names(roi_root))
}

count_roi_zip_bundles <- function(roi_root) {
  length(roi_zip_paths(roi_root))
}

count_roi_payload_files <- function(roi_root) {
  count_roi_zip_bundles(roi_root)
}

path_mtime_num <- function(path) {
  if (!nzchar(path) || !(file.exists(path) || dir.exists(path))) return(NA_real_)
  as.numeric(file.info(path)$mtime)
}

newest_roi_payload_mtime <- function(roi_root) {
  if (!dir.exists(roi_root)) return(NA_real_)
  payloads <- roi_zip_paths(roi_root)
  if (!length(payloads)) return(NA_real_)
  suppressWarnings(max(as.numeric(file.info(payloads)$mtime), na.rm = TRUE))
}

summarize_name_dropout <- function(expected_names, found_names) {
  expected_names <- unique(expected_names[nzchar(expected_names)])
  found_names <- unique(found_names[nzchar(found_names)])
  dropouts <- setdiff(expected_names, found_names)
  matched <- intersect(expected_names, found_names)
  extras <- setdiff(found_names, expected_names)
  list(
    expected_count = length(expected_names),
    found_count = length(matched),
    found_total_count = length(found_names),
    dropouts = dropouts,
    extras = extras
  )
}

task_size_for_step <- function(step_id, settings) {
  p <- build_paths(settings)
  if (step_id %in% c("rename", "img_list", "fiji")) {
    image_count <- if (file.exists(p$image_list)) length(readLines(p$image_list, warn = FALSE)) else count_files_by_pattern(p$pre_renamed, "\\.(tif|tiff|png|jpg|jpeg)$")
    return(list(value = image_count, label = "image_count"))
  }
  if (step_id %in% c("ilastik", "quantify")) {
    n <- count_files_by_pattern(p$out_clear, "\\.tif$")
    return(list(value = n, label = "preprocessed_image_count"))
  }
  if (step_id %in% c("groups", "writeback")) {
    n <- if (file.exists(p$mask_percentages)) max(nrow(tryCatch(read.csv(p$mask_percentages, stringsAsFactors = FALSE), error = function(e) data.frame())) - 0, 0) else 0
    return(list(value = n, label = "measured_image_rows"))
  }
  if (step_id == "roi_extract") {
    n <- count_files_by_pattern(p$out_seg, "\\.(tif|tiff|png|jpg|jpeg)$")
    return(list(value = n, label = "segmentation_image_count"))
  }
  if (step_id == "package_rois") {
    n <- safe_selected_manual_row_count(p$inspected)
    if (!is.finite(n)) n <- count_roi_payload_files(file.path(p$out_opt, settings$OUT_ROI_DIR_NAME %||% "ROIs"))
    return(list(value = n, label = if (file.exists(p$inspected)) "selected_image_rows" else "roi_zip_bundle_count"))
  }
  if (step_id %in% c("optimize", "combine", "viz_roi", "viz_pca")) {
    n <- safe_rds_object_count(file.path(p$out_opt, "allROIs_data.rds"))
    if (!is.finite(n)) n <- safe_rds_object_count(p$opt_rds)
    if (!is.finite(n)) n <- count_roi_payload_files(file.path(p$out_opt, settings$OUT_ROI_DIR_NAME %||% "ROIs"))
    return(list(value = n, label = "roi_or_bundle_count"))
  }
  if (step_id == "production_generalization") {
    n <- count_roi_payload_files(file.path(p$out_opt, settings$OUT_ROI_DIR_NAME %||% "ROIs"))
    return(list(value = as.numeric(n), label = "all_image_roi_payload_count"))
  }
  if (step_id %in% c("generalize", "meta_pca", "final_plots")) {
    n <- safe_rds_object_count(p$generalized)
    if (!is.finite(n)) n <- count_roi_payload_files(file.path(p$out_opt, settings$OUT_ROI_DIR_NAME %||% "ROIs"))
    return(list(value = as.numeric(n), label = "skeleton_or_object_count"))
  }
  list(value = NA_real_, label = "unknown")
}

step_integrity_check <- function(step, settings) {
  p <- build_paths(settings)
  step_id <- step$id
  output_paths <- unique(unlist(step$outputs))
  output_paths <- output_paths[nzchar(output_paths)]
  existing_outputs <- output_paths[file.exists(output_paths) | dir.exists(output_paths)]

  result <- list(
    state = "missing",
    summary = "No output detected yet.",
    details = character(),
    existing_outputs = existing_outputs,
    expected_count = NA_real_,
    found_count = NA_real_,
    found_total_count = NA_real_,
    dropouts = character(),
    extras = character()
  )

  if (step_id == "rename") {
    input_root <- p$input_master
    source_files <- if (dir.exists(input_root)) {
      source_files <- list.files(input_root, pattern = "\\.(tif|tiff|png|jpg|jpeg)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      source_files <- normalizePath(source_files, winslash = "/", mustWork = FALSE)
      source_files <- source_files[!grepl("/(ilastik|software_reference|example_expected_outputs|training_images_raw|training_images_preprocessed)/", source_files, ignore.case = TRUE)]
      source_files[!grepl("(_RGavg|_mask|_mask_renorm)\\.", basename(source_files), ignore.case = TRUE)]
    } else {
      character()
    }
    expected_names <- rename_expected_names(source_files, input_root)
    found_names <- produced_image_basenames(p$pre_renamed, "\\.(tif|tiff|png|jpg|jpeg)$")
    cmp <- summarize_name_dropout(expected_names, found_names)
    expected <- cmp$expected_count
    found <- cmp$found_count
    result$expected_count <- expected
    result$found_count <- found
    result$found_total_count <- cmp$found_total_count
    result$dropouts <- cmp$dropouts
    result$extras <- cmp$extras
    if (found == 0) {
      result$state <- "missing"
      result$summary <- "No renamed TIFF files detected."
    } else if (is.finite(expected) && expected > 0 && found < expected) {
      result$state <- "partial"
      result$summary <- "Renamed image set looks incomplete."
      result$details <- c(sprintf("Detected %s renamed TIFF files but found %s source TIFF files in the master input directory.", found, expected))
      if (length(result$dropouts)) {
        result$details <- c(result$details, sprintf("%s image(s) from the source tree do not appear in PRE_renamed.", length(result$dropouts)))
      }
    } else {
      result$state <- "complete"
      result$summary <- "Renamed image output looks complete."
      if (length(result$extras)) {
        result$details <- c(result$details, sprintf("%s extra file(s) are present in PRE_renamed beyond the current expected image set.", length(result$extras)))
      }
    }
    return(result)
  }

  if (step_id == "img_list") {
    expected_names <- produced_image_basenames(p$pre_renamed, "\\.(tif|tiff|png|jpg|jpeg)$")
    found_names <- basename(safe_read_lines(p$image_list))
    cmp <- summarize_name_dropout(expected_names, found_names)
    expected <- cmp$expected_count
    found <- cmp$found_count
    result$expected_count <- expected
    result$found_count <- found
    result$found_total_count <- cmp$found_total_count
    result$dropouts <- cmp$dropouts
    result$extras <- cmp$extras
    if (found == 0) {
      result$state <- "missing"
      result$summary <- "Image list is missing or empty."
    } else if (expected > 0 && found < expected) {
      result$state <- "partial"
      result$summary <- "Image list looks shorter than the renamed image set."
      result$details <- c(sprintf("The list contains %s entries, while %s image files were found under PRE_renamed.", found, expected))
      if (length(result$dropouts)) {
        result$details <- c(result$details, sprintf("%s renamed image(s) are missing from OUT_img_input1.txt.", length(result$dropouts)))
      }
    } else {
      result$state <- "complete"
      result$summary <- "Image list looks complete."
      if (length(result$extras)) {
        result$details <- c(result$details, sprintf("%s extra list entry or stale file name(s) were found beyond the current renamed image set.", length(result$extras)))
      }
    }
    return(result)
  }

  if (step_id == "fiji") {
    expected_names <- fiji_expected_names(p$image_list)
    found_names <- produced_image_basenames(p$out_clear, "_RGavg\\.tif$")
    cmp <- summarize_name_dropout(expected_names, found_names)
    expected <- cmp$expected_count
    found <- cmp$found_count
    result$expected_count <- expected
    result$found_count <- found
    result$found_total_count <- cmp$found_total_count
    result$dropouts <- cmp$dropouts
    result$extras <- cmp$extras
    if (found == 0) {
      result$state <- "missing"
      result$summary <- "No Fiji-preprocessed images detected."
    } else if (expected > 0 && found < expected) {
      result$state <- "partial"
      result$summary <- "Fiji preprocessing looks incomplete."
      result$details <- c(sprintf("Detected %s RG-averaged images but the image list contains %s entries.", found, expected))
      if (length(result$dropouts)) {
        result$details <- c(result$details, sprintf("%s input image(s) did not produce an _RGavg.tif output.", length(result$dropouts)))
      }
    } else {
      result$state <- "complete"
      result$summary <- "Fiji preprocessing output looks complete."
      if (length(result$extras)) {
        result$details <- c(result$details, sprintf("%s extra Fiji output file(s) are present beyond the current input image set.", length(result$extras)))
      }
    }
    return(result)
  }

  if (step_id == "ilastik") {
    detected_suffix <- detected_segmentation_suffix(p$out_seg, settings)
    expected_names <- ilastik_expected_names_with_suffix(p$out_clear, detected_suffix)
    found_names <- segmentation_mask_names(p$out_seg, settings)
    cmp <- summarize_name_dropout(expected_names, found_names)
    expected <- cmp$expected_count
    found <- cmp$found_count
    result$expected_count <- expected
    result$found_count <- found
    result$found_total_count <- cmp$found_total_count
    result$dropouts <- cmp$dropouts
    result$extras <- cmp$extras
    if (found == 0) {
      result$state <- "missing"
      result$summary <- "No ilastik segmentation masks detected."
    } else if (expected > 0 && found < expected) {
      result$state <- "partial"
      result$summary <- "Ilastik segmentation looks incomplete."
      result$details <- c(sprintf("Detected %s segmentation masks with suffix %s but %s preprocessed Fiji images are present.", found, detected_suffix, expected))
      if (length(result$dropouts)) {
        result$details <- c(result$details, sprintf("%s preprocessed image(s) did not produce the expected ilastik mask output.", length(result$dropouts)))
      }
    } else {
      result$state <- "complete"
      result$summary <- "Ilastik segmentation output looks complete."
      if (length(result$extras)) {
        result$details <- c(result$details, sprintf("%s extra segmentation output file(s) are present beyond the current expected set.", length(result$extras)))
      }
    }
    return(result)
  }

  if (step_id == "quantify") {
    expected_names <- segmentation_mask_names(p$out_seg, settings)
    found_names <- safe_csv_column_values(p$mask_percentages, c("image_name", "Image", "image"))
    cmp <- summarize_name_dropout(expected_names, found_names)
    expected <- cmp$expected_count
    found <- if (length(found_names)) cmp$found_count else safe_csv_row_count(p$mask_percentages)
    result$expected_count <- expected
    result$found_count <- found
    result$found_total_count <- cmp$found_total_count
    result$dropouts <- cmp$dropouts
    result$extras <- cmp$extras
    if (!is.finite(found) || found <= 0) {
      result$state <- "missing"
      result$summary <- "No mask area table detected."
    } else if (expected > 0 && found < expected) {
      result$state <- "partial"
      result$summary <- "Mask area quantification looks incomplete."
      result$details <- c(sprintf("The mask area table has %s rows but %s segmentation masks are present.", found, expected))
      if (length(result$dropouts)) {
        result$details <- c(result$details, sprintf("%s segmentation mask(s) do not appear in the mask area table.", length(result$dropouts)))
      }
    } else {
      result$state <- "complete"
      result$summary <- "Mask area quantification output looks complete."
      if (length(result$extras)) {
        result$details <- c(result$details, sprintf("%s extra table row name(s) were found beyond the current segmentation mask set.", length(result$extras)))
      }
    }
    return(result)
  }

  if (step_id %in% c("groups", "writeback")) {
    found <- if (step_id == "groups") safe_csv_row_count(p$groups) else safe_csv_row_count(p$inspected)
    expected <- safe_csv_row_count(p$mask_percentages)
    output_path <- if (step_id == "groups") p$groups else p$inspected
    output_mtime <- path_mtime_num(output_path)
    input_mtime <- if (step_id == "groups") {
      suppressWarnings(max(c(path_mtime_num(p$metadata), path_mtime_num(p$mask_percentages)), na.rm = TRUE))
    } else {
      suppressWarnings(max(c(path_mtime_num(p$groups), path_mtime_num(manual_review_manifest_path(settings))), na.rm = TRUE))
    }
    if (!is.finite(found) || found <= 0) {
      result$state <- "missing"
      result$summary <- "Expected grouping output is missing."
    } else if (is.finite(output_mtime) && is.finite(input_mtime) && output_mtime < input_mtime) {
      result$state <- "partial"
      result$summary <- if (step_id == "groups") {
        "Validation groups table exists, but it is older than the current metadata or mask measurements."
      } else {
        "Inspected validation table exists, but it is older than the current groups table or manual review manifest."
      }
      result$details <- c(
        sprintf("Output timestamp: %s", format(as.POSIXct(output_mtime, origin = '1970-01-01', tz = Sys.timezone()), "%Y-%m-%d %H:%M:%S")),
        sprintf("Newest relevant upstream timestamp: %s", format(as.POSIXct(input_mtime, origin = '1970-01-01', tz = Sys.timezone()), "%Y-%m-%d %H:%M:%S")),
        if (step_id == "groups") {
          "Rerun 'Create Validation Groups' so this table matches the current metadata and mask-area outputs."
        } else {
        "Rerun '2.A.5.1. Write Back Manual Labels' so the inspected CSV matches the current manual review manifest."
        }
      )
    } else if (is.finite(expected) && expected > 0 && found < expected) {
      result$state <- "partial"
      result$summary <- "Grouping output looks incomplete."
      result$details <- c(sprintf("The grouping table has %s rows while the mask summary table has %s rows.", found, expected))
    } else {
      result$state <- "complete"
      result$summary <- "Grouping output looks complete."
    }
    return(result)
  }

  if (step_id == "manual") {
    mr <- manual_review_integrity(settings)
    result$existing_outputs <- c(result$existing_outputs, if (mr$has_manifest) mr$manifest_path else character())
    result$expected_count <- mr$expected_count
    result$found_count <- mr$found_count
    result$found_total_count <- mr$found_total_count
    result$dropouts <- mr$dropouts
    result$extras <- mr$extras
    if (!mr$has_manifest || mr$found_total_count == 0) {
      result$state <- "missing"
      result$summary <- "No saved manual review manifest is present yet."
      result$details <- c(
        sprintf("The manual review manifest will be saved at %s.", mr$manifest_path),
        "Start labeling images in the Manual Inspection tab to create saved intermediate review data."
      )
    } else if (mr$found_count < mr$expected_count) {
      result$state <- "partial"
      result$summary <- "Manual inspection is partially saved, but some grouped images are still unlabeled."
      result$details <- c(
        sprintf("Saved labels currently cover %s of %s grouped images.", mr$found_count, mr$expected_count),
        sprintf("The saved manifest is %s.", mr$manifest_path)
      )
    } else {
      result$state <- "complete"
      result$summary <- "Manual inspection manifest looks complete."
      result$details <- c(
        sprintf("Saved labels currently cover all %s grouped images.", mr$expected_count),
        sprintf("The saved manifest is %s.", mr$manifest_path)
      )
    }
    return(result)
  }

  if (step_id == "roi_extract") {
    expected_names <- sub("\\.tif$", "", segmentation_mask_names(p$out_seg, settings), ignore.case = TRUE)
    roi_root <- file.path(p$out_opt, settings$OUT_ROI_DIR_NAME %||% "ROIs")
    found_names <- roi_payload_names(roi_root)
    payload_count <- count_roi_zip_bundles(roi_root)
    roi_payload_mtime <- newest_roi_payload_mtime(roi_root)
    seg_mtime <- path_mtime_num(p$out_seg)
    cmp <- summarize_name_dropout(expected_names, found_names)
    expected <- cmp$expected_count
    found <- cmp$found_count
    result$expected_count <- expected
    result$found_count <- found
    result$found_total_count <- cmp$found_total_count
    result$dropouts <- cmp$dropouts
    result$extras <- cmp$extras
    if (found == 0) {
      result$state <- "missing"
      result$summary <- "No usable ROI files detected."
      if (dir.exists(roi_root) && length(list.dirs(roi_root, recursive = FALSE, full.names = TRUE))) {
        result$details <- c(result$details, "ROI folders exist, but none of them contains a .roi file or an _ROIs.zip bundle.")
      }
    } else if (is.finite(roi_payload_mtime) && is.finite(seg_mtime) && roi_payload_mtime < seg_mtime) {
      result$state <- "partial"
      result$summary <- "ROI extraction outputs exist, but they are older than the current segmentation masks."
      result$details <- c(
        sprintf("Newest ROI payload timestamp: %s", format(as.POSIXct(roi_payload_mtime, origin = '1970-01-01', tz = Sys.timezone()), "%Y-%m-%d %H:%M:%S")),
        sprintf("Segmentation folder timestamp: %s", format(as.POSIXct(seg_mtime, origin = '1970-01-01', tz = Sys.timezone()), "%Y-%m-%d %H:%M:%S")),
        "Rerun '2.A.5.2. Extract ROIs and Skeleton Summaries' so the ROI payloads match the current segmentation outputs."
      )
    } else if (expected > 0 && found < expected) {
      result$state <- "partial"
      result$summary <- "ROI extraction looks incomplete."
      result$details <- c(sprintf("Detected usable ROI payloads for %s segmentation images, while %s segmentation images are present. A total of %s ROI zip bundle(s) were found.", found, expected, payload_count))
      if (length(result$dropouts)) {
        result$details <- c(result$details, sprintf("%s segmentation image(s) do not yet have any .roi or _ROIs.zip payload.", length(result$dropouts)))
      }
    } else {
      result$state <- "complete"
      result$summary <- "ROI extraction output looks complete."
      result$details <- c(result$details, sprintf("Detected %s ROI zip bundle(s) across %s segmentation images.", payload_count, found))
      if (length(result$extras)) {
        result$details <- c(result$details, sprintf("%s extra ROI payload folder(s) are present beyond the current segmentation image set.", length(result$extras)))
      }
    }
    return(result)
  }

  if (step_id == "package_rois") {
    expected <- safe_selected_manual_row_count(p$inspected)
    package_summary <- safe_optrois_summary_counts(p$out_opt)
    found <- if (is.finite(package_summary$rows)) package_summary$rows else safe_rds_image_record_count(p$opt_rds)
    roi_object_count <- package_summary$roi_count
    roi_root <- file.path(p$out_opt, settings$OUT_ROI_DIR_NAME %||% "ROIs")
    roi_payload_dirs <- count_roi_payload_dirs(roi_root)
    roi_payload_files <- count_roi_zip_bundles(roi_root)
    opt_mtime <- path_mtime_num(p$opt_rds)
    summary_mtime <- package_summary$mtime
    inspected_mtime <- path_mtime_num(p$inspected)
    roi_payload_mtime <- newest_roi_payload_mtime(roi_root)
    stale_against_upstream <-
      (is.finite(opt_mtime) && is.finite(inspected_mtime) && opt_mtime < inspected_mtime) ||
      (is.finite(opt_mtime) && is.finite(roi_payload_mtime) && opt_mtime < roi_payload_mtime) ||
      (is.finite(summary_mtime) && is.finite(inspected_mtime) && summary_mtime < inspected_mtime) ||
      (is.finite(summary_mtime) && is.finite(roi_payload_mtime) && summary_mtime < roi_payload_mtime)
    result$expected_count <- expected
    result$found_count <- found
    result$found_total_count <- found

    if (is.finite(expected) && expected <= 0) {
      result$state <- "missing"
      result$summary <- "No manually selected images are available for ROI packaging."
      result$details <- c(
        "At least one image must be marked as choice or noise before packaging can create an optimization-ready RDS.",
        "Images marked not sure are intentionally skipped."
      )
    } else if ((!is.finite(found) || found <= 0) && roi_payload_dirs > 0) {
      result$state <- "partial"
      result$summary <- "ROI payloads are present, but the packaged ROI object is still empty."
      result$details <- c(
        sprintf("Detected %s ROI payload directories and %s ROI zip bundle(s) under %s.", roi_payload_dirs, roi_payload_files, roi_root),
        "Rerun '2.A.5.3. Package ROIs for Optimization' so the optimization-ready RDS is rebuilt from the current ROI outputs."
      )
    } else if (stale_against_upstream) {
      result$state <- "partial"
      result$summary <- "Packaged ROI object exists, but it is older than the current inspected labels or ROI outputs."
      result$details <- c(
        sprintf("Packaged RDS timestamp: %s", if (is.finite(opt_mtime)) format(as.POSIXct(opt_mtime, origin = '1970-01-01', tz = Sys.timezone()), "%Y-%m-%d %H:%M:%S") else "missing"),
        sprintf("Packaged summary timestamp: %s", if (is.finite(summary_mtime)) format(as.POSIXct(summary_mtime, origin = '1970-01-01', tz = Sys.timezone()), "%Y-%m-%d %H:%M:%S") else "missing"),
        sprintf("Inspected CSV timestamp: %s", if (is.finite(inspected_mtime)) format(as.POSIXct(inspected_mtime, origin = '1970-01-01', tz = Sys.timezone()), "%Y-%m-%d %H:%M:%S") else "missing"),
        sprintf("Newest ROI payload timestamp: %s", if (is.finite(roi_payload_mtime)) format(as.POSIXct(roi_payload_mtime, origin = '1970-01-01', tz = Sys.timezone()), "%Y-%m-%d %H:%M:%S") else "missing"),
        "Rerun '2.A.5.3. Package ROIs for Optimization' so the packaged object matches the newer upstream outputs."
      )
    } else if (!is.finite(found) || found <= 0) {
      result$state <- "missing"
      result$summary <- "Packaged ROI object is missing or empty."
    } else if (is.finite(expected) && expected > 0 && found < expected) {
      result$state <- "partial"
      result$summary <- "Packaged ROI object looks incomplete."
      result$details <- c(sprintf("The packaged ROI object contains %s selected image record(s), while the inspected table has %s manually selected image(s).", found, expected))
    } else {
      result$state <- "complete"
      result$summary <- "Packaged ROI object looks complete."
      if (is.finite(roi_object_count)) {
        result$details <- c(result$details, sprintf("Packaged %s selected image record(s) containing about %s ROI object(s).", found, roi_object_count))
      } else {
        result$details <- c(result$details, sprintf("Packaged %s selected image record(s). ROI object count is not summarized yet.", found))
      }
    }
    return(result)
  }

  if (step_id %in% c("optimize", "combine", "viz_roi", "viz_pca", "generalize", "meta_pca", "final_plots")) {
    if (length(existing_outputs)) {
      result$state <- "complete"
      result$summary <- "At least one expected output is already present."
    } else {
      result$state <- "missing"
      result$summary <- "No expected output is detected yet."
    }
    return(result)
  }

  if (length(existing_outputs)) {
    result$state <- "complete"
    result$summary <- "Existing output detected."
  } else {
    result$state <- "missing"
    result$summary <- "No expected output is detected yet."
  }
  result
}

typical_step_duration <- function(history_df, step_id, runtime) {
  if (!nrow(history_df)) return(NA_real_)
  subset <- history_df[history_df$step_id == step_id & history_df$runtime == runtime & history_df$status == "completed" & is.finite(history_df$duration_seconds), , drop = FALSE]
  if (!nrow(subset)) return(NA_real_)
  stats::median(as.numeric(subset$duration_seconds), na.rm = TRUE)
}

predict_step_duration <- function(history_df, step_id, runtime, machine_signature, cpu_cores, task_size) {
  subset <- history_df[
    history_df$step_id == step_id &
      history_df$runtime == runtime &
      history_df$status == "completed" &
      is.finite(history_df$duration_seconds),
    ,
    drop = FALSE
  ]
  if (!nrow(subset)) {
    return(list(predicted = NA_real_, method = "no_history"))
  }

  same_machine <- subset[subset$machine_signature == machine_signature, , drop = FALSE]
  if (nrow(same_machine) >= 3) {
    subset <- same_machine
  }

  if (is.finite(task_size) && !all(is.na(subset$task_size))) {
    subset <- subset[is.finite(subset$task_size) & subset$task_size > 0, , drop = FALSE]
  }

  if (nrow(subset) >= 4 && is.finite(task_size) && length(unique(subset$task_size)) >= 3) {
    fit <- tryCatch(
      stats::lm(duration_seconds ~ task_size + cpu_cores, data = subset),
      error = function(e) NULL
    )
    if (!is.null(fit)) {
      newdata <- data.frame(task_size = task_size, cpu_cores = cpu_cores)
      pred <- tryCatch(as.numeric(stats::predict(fit, newdata = newdata)), error = function(e) NA_real_)
      if (is.finite(pred) && pred > 0) {
        return(list(predicted = pred, method = "linear_regression"))
      }
    }
  }

  if (is.finite(task_size) && "seconds_per_unit" %in% names(subset)) {
    rate_subset <- subset[is.finite(subset$seconds_per_unit) & subset$seconds_per_unit > 0, , drop = FALSE]
    if (nrow(rate_subset) >= 2) {
      rate <- stats::median(rate_subset$seconds_per_unit, na.rm = TRUE)
      if (is.finite(rate) && rate > 0) {
        return(list(predicted = rate * task_size, method = "median_seconds_per_unit"))
      }
    }
  }

  if (is.finite(task_size) && nrow(subset) >= 2 && all(is.finite(subset$task_size)) && all(subset$task_size > 0)) {
    ratio <- stats::median(subset$duration_seconds / subset$task_size, na.rm = TRUE)
    if (is.finite(ratio) && ratio > 0) {
      return(list(predicted = ratio * task_size, method = "median_per_task_unit"))
    }
  }

  if (nrow(subset) >= 1) {
    return(list(predicted = stats::median(subset$duration_seconds, na.rm = TRUE), method = "median_duration"))
  }

  list(predicted = NA_real_, method = "no_history")
}

new_run_id <- function(step_id) {
  paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", gsub("[^A-Za-z0-9]+", "_", step_id))
}

build_process_command <- function(command, runtime) {
  if (runtime == "wsl") {
    list(exe = "wsl", args = c("bash", "-lc", sprintf("cd %s && %s", bash_quote(to_wsl_path(project_root)), command)))
  } else {
    list(exe = "bash", args = c("-lc", sprintf("cd %s && %s", bash_quote(project_root), command)))
  }
}

step_specs <- function(settings, input) {
  p <- build_paths(settings)
  runtime_choice <- input$runtime_choice %||% "posix"
  root_arg <- bash_quote(runtime_root_arg(runtime_choice))
  optimization_scoring_mode <- input$optimization_scoring_mode_inline %||%
    settings$OPTIMIZATION_SCORING_MODE %||%
    "continuity_aware"
  list(
    list(id = "rename", phase = "Setup", title = "2.A.1.0. Rename Images", manual = FALSE,
         help_key = "step_setup",
         desc = "Create unique hard-linked image names in PRE_renamed so later outputs stay unambiguous.",
         inputs = "Nested raw image folders", outputs = p$pre_renamed,
         command = sprintf("bash ./0.Rename.sh %s", bash_quote(runtime_path_arg(input$rename_search_root %||% ".", runtime_choice)))),
    list(id = "img_list", phase = "Setup", title = "2.A.1.1. Build Image List", manual = FALSE,
         help_key = "step_setup",
         desc = "Write OUT_img_input1.txt so the whole pipeline uses one exact image universe.",
         inputs = p$pre_renamed, outputs = p$image_list,
         command = sprintf("bash ./1.get_img_list.sh %s", bash_quote(runtime_path_arg(input$image_list_source %||% p$pre_renamed, runtime_choice)))),
    list(id = "fiji", phase = "Setup", title = "2.A.1.2. Run Fiji Preprocessing", manual = FALSE,
         help_key = "step_setup",
         desc = "Create RG-averaged images for stable downstream segmentation.",
         inputs = p$image_list, outputs = p$out_clear,
         command = "bash ./2.RunFIJI_clean_bckgnd.sh"),
    list(id = "ilastik", phase = "Setup", title = "2.A.1.3. Run Ilastik Segmentation", manual = FALSE,
         help_key = "step_setup",
         desc = "Create renormalized neurite masks from the Fiji outputs.",
         inputs = p$out_clear, outputs = p$out_seg,
         command = "bash ./3.Run_ilastik_model.sh"),
    list(id = "quantify", phase = "Setup", title = "2.A.1.4. Quantify Mask Areas", manual = FALSE,
         help_key = "step_setup",
         desc = "Measure neurite, cell-body, and background fractions from the masks.",
         inputs = p$out_seg, outputs = p$mask_percentages,
         command = "bash ./4.GetArea%Fiji.sh"),
    list(id = "groups", phase = "Validation with Test Images", title = "2.A.3. Create Validation Groups", manual = FALSE,
         help_key = "step_validation",
         desc = "Create the four validation quadrants that the user will inspect manually.",
         inputs = paste(p$metadata, "and", p$mask_percentages), outputs = p$groups,
         command = "PIPELINE_NONINTERACTIVE=1 bash ./5.SelectValidationImgGroups.sh --yes"),
    list(id = "manual", phase = "Validation with Test Images", title = "2.A.4. Manual Inspection", manual = TRUE,
         help_key = "step_validation",
         desc = "Review the grouped images in the app and save manual labels into the manifest used by the write-back step.",
         inputs = file.path(p$out_opt, "<group>/"), outputs = manual_review_manifest_path(settings)),
    list(id = "writeback", phase = "Validation with Test Images", title = "2.A.5.1. Write Back Manual Labels", manual = FALSE,
          help_key = "step_validation",
          desc = "Write the manual choice/noise decisions back into the optimization tables.",
          inputs = manual_review_manifest_path(settings), outputs = p$inspected,
          command = "PIPELINE_NONINTERACTIVE=1 bash ./6.SelectedValidationImgWriteSee.sh --yes"),
    list(id = "roi_extract", phase = "Validation with Test Images", title = "2.A.5.2. Extract ROIs and Skeleton Summaries", manual = FALSE,
         help_key = "step_validation",
         desc = "Export ROI objects and skeleton summaries from the segmentation masks.",
         inputs = p$out_seg, outputs = file.path(p$out_opt, settings$OUT_ROI_DIR_NAME %||% "ROIs"),
         command = "bash ./7.RunOptimization.sh"),
    list(id = "package_rois", phase = "Validation with Test Images", title = "2.A.5.3. Package ROIs for Optimization", manual = FALSE,
         help_key = "step_validation",
         desc = "Bundle ROI geometry and metadata into an optimization-ready RDS object.",
         inputs = p$inspected, outputs = p$opt_rds,
         command = sprintf("Rscript ./0.ReadROIdata-to-optimize.R %s", root_arg)),
    list(id = "optimize", phase = "Validation with Test Images", title = "2.A.6. Optimize Neurite Thresholds", manual = FALSE,
         help_key = "step_validation",
         desc = "Find validated cutoff outputs that separate neurite biology from confluence and noise.",
         inputs = p$opt_rds, outputs = p$param_results,
         command = sprintf("OPTIMIZATION_SCORING_MODE=%s Rscript ./1.OptimizeNeuriteParams-MAJOR_OPTIMIZE.R %s", bash_quote(optimization_scoring_mode), root_arg)),
    list(id = "generalize", phase = "Generalization with New Images", title = "3.3. Legacy Skeleton-RDS Analysis", manual = FALSE,
         help_key = "step_generalization",
         desc = "Optional legacy backend analysis for an already-created generalized skeleton RDS. The current app workflow uses tab 3.2 instead.",
         inputs = "Validated cutoff outputs plus new image-derived summaries", outputs = p$generalized,
         command = sprintf("Rscript ./2.1.Generalize_image_threshold_skeleton_rule.R %s", root_arg)),
    list(id = "combine", phase = "Visualization and Interpretation", title = "4.1. Build Combined ROI + Skeleton Analysis Object", manual = FALSE,
         help_key = "step_interpretation",
         desc = "Prepare the combined analysis object used by the interpretation scripts.",
         inputs = p$opt_rds, outputs = file.path(p$out_opt, "allROIs_analysis+skeletons.rds"),
         command = sprintf("Rscript ./2.analyze-combine-ROIs-skeletons.R %s", root_arg)),
    list(id = "viz_roi", phase = "Visualization and Interpretation", title = "4.2. Run Representative ROI Visualization", manual = FALSE,
         help_key = "step_interpretation",
         desc = "Create representative ROI before/after panels to visually inspect cutoff behavior.",
         inputs = "Combined ROI analysis outputs", outputs = file.path(p$out_opt, "visualizations"),
         command = sprintf("Rscript ./3.analyze-visualize-check.R %s", root_arg)),
    list(id = "viz_pca", phase = "Visualization and Interpretation", title = "4.3. Run ROI Visualization Plus PCA", manual = FALSE,
         help_key = "step_interpretation",
         desc = "Add class-comparison and PCA-style interpretation on top of the ROI visual outputs.",
         inputs = "Combined ROI analysis outputs", outputs = file.path(p$out_opt, "class_visualizations"),
         command = sprintf("Rscript ./3.analysis-check-visualize-readiness-on-test-images2.R %s", root_arg)),
    list(id = "meta_pca", phase = "Visualization and Interpretation", title = "4.4. Run Metadata and Image-Feature PCA", manual = FALSE,
         help_key = "step_interpretation",
         desc = "Interpret the generalized outputs at image level and inspect metadata-driven structure.",
         inputs = p$generalized, outputs = p$pca_rds,
         command = sprintf("Rscript ./3.1.analyzed-generalized-skeleton-data-PCA+ALMOST_WORKING.R %s", root_arg)),
    list(id = "final_plots", phase = "Visualization and Interpretation", title = "4.5. Run Final Biological Reporting Plots", manual = FALSE,
         help_key = "step_interpretation",
         desc = "Create the final polished reporting figures for treatment and neurite effects.",
         inputs = p$generalized, outputs = p$final_dir,
         command = sprintf("Rscript ./4.1.Skeleton-cutoff-ploting-improved-polished+BG.R %s", root_arg))
  )
}

step_card_ui <- function(step) {
  tags$div(
    class = if (isTRUE(step$manual)) "step-card step-manual" else "step-card",
    tags$div(class = "step-phase", step$phase),
    label_with_help(step$title, step$help_key),
    tags$p(step$desc),
    tags$p(tags$strong("Inputs: "), tags$span(class = "path-block", step$inputs)),
    tags$p(tags$strong("Expected outputs: "), tags$span(class = "path-block", step$outputs)),
    if (isTRUE(step$manual)) {
      tags$div(class = "manual-note", "This is a real manual checkpoint. Review the grouped images in the app, save labels into the manual review manifest, then continue.")
      } else {
        actionButton(paste0("run_", step$id), "Run this step", class = "run-button")
      },
      uiOutput(paste0("status_", step$id)),
      uiOutput(paste0("inline_readiness_", step$id)),
      tags$details(
        class = "step-details",
        tags$summary(
          tags$span(class = "step-details-icon", "\u25b6"),
          tags$span(class = "step-details-text", "Readiness and Outputs"),
          tags$span(class = "step-details-hint", "Click to expand")
      ),
      uiOutput(paste0("step_summary_", step$id))
    )
  )
}

modal_top_close <- function() {
  tags$button(
    type = "button",
    class = "modal-top-close",
    `data-dismiss` = "modal",
    `data-bs-dismiss` = "modal",
    `aria-label` = "Close",
    HTML("&times;")
  )
}

workflow_phase_order <- function() {
  c(
    "Setup",
    "Validation with Test Images",
    "Generalization with New Images",
    "Visualization and Interpretation"
  )
}

steps_for_phase <- function(steps, phase_name) {
  Filter(function(x) identical(x$phase, phase_name), steps)
}

find_step_by_id <- function(steps, step_id) {
  matches <- Filter(function(x) identical(x$id, step_id), steps)
  if (!length(matches)) return(NULL)
  matches[[1]]
}

required_upstream_step_ids <- function(step_id) {
  switch(
    step_id,
    img_list = c("rename"),
    fiji = c("rename", "img_list"),
    ilastik = c("rename", "img_list", "fiji"),
    quantify = c("rename", "img_list", "fiji", "ilastik"),
    groups = c("rename", "img_list", "fiji", "ilastik", "quantify"),
    manual = c("groups"),
    writeback = c("groups", "manual"),
    roi_extract = c("ilastik"),
    package_rois = c("writeback", "roi_extract"),
    optimize = c("writeback", "roi_extract", "package_rois"),
    generalize = c("optimize"),
    combine = c("generalize"),
    viz_roi = c("generalize", "combine"),
    viz_pca = c("generalize"),
    meta_pca = c("generalize"),
    final_plots = c("generalize"),
    character()
  )
}

upstream_step_messages <- function(step_id, settings, steps = NULL) {
  if (is.null(steps)) steps <- step_specs(settings, list())
  needed_ids <- required_upstream_step_ids(step_id)
  if (!length(needed_ids)) return(character())
  msgs <- character()
  for (needed_id in needed_ids) {
    upstream_step <- find_step_by_id(steps, needed_id)
    if (is.null(upstream_step)) next
    if (identical(needed_id, "manual")) next
    integrity <- step_integrity_check(upstream_step, settings)
    if (!identical(integrity$state, "complete")) {
      msgs <- c(
        msgs,
        sprintf(
          "Missing upstream step: %s (%s module). Current status: %s",
          upstream_step$title,
          upstream_step$phase,
          integrity$summary
        )
      )
    }
  }
  unique(msgs)
}

setup_module_step_ids <- function() {
  c("rename", "img_list", "fiji", "ilastik", "quantify")
}

status_for_output <- function(path) {
  paths <- unique(unlist(path))
  paths <- paths[nzchar(paths)]
  if (!length(paths)) {
    return(list(
      detected = 0L,
      total = 0L,
      label = "No output path defined yet",
      class = "status-neutral",
      icon = "○"
    ))
  }
  detected <- vapply(paths, function(x) dir.exists(x) || file.exists(x), logical(1))
  n_detected <- sum(detected)
  total <- length(paths)
  if (n_detected == total) {
    list(
      detected = n_detected,
      total = total,
      label = if (total == 1) "Output ready" else "All outputs ready",
      class = "status-good",
      icon = "✓"
    )
  } else if (n_detected > 0) {
    list(
      detected = n_detected,
      total = total,
      label = "Partially ready",
      class = "status-warn",
      icon = "◐"
    )
  } else {
    list(
      detected = n_detected,
      total = total,
      label = "Not ready yet",
      class = "status-missing",
      icon = "○"
    )
  }
}

status_for_step <- function(step, settings) {
  integrity <- step_integrity_check(step, settings)
  if (is.finite(integrity$expected_count) && integrity$expected_count > 0) {
    detected <- as.integer(integrity$found_count %||% 0)
    total <- as.integer(integrity$expected_count)
    is_package_step <- identical(step$id %||% "", "package_rois")
    if (detected >= total) {
      return(list(
        detected = detected,
        total = total,
        label = if (is_package_step) "Selected images packaged" else "Images ready",
        class = "status-good",
        icon = "\u2713",
        unit_label = if (is_package_step) "selected images packaged" else "images completed"
      ))
    }
    if (detected > 0) {
      return(list(
        detected = detected,
        total = total,
        label = if (is_package_step) "Packaging incomplete" else "Images incomplete",
        class = "status-partial",
        icon = "\u25d4",
        unit_label = if (is_package_step) "selected images packaged" else "images completed"
      ))
    }
    return(list(
      detected = detected,
      total = total,
      label = if (is_package_step) "Selected images not packaged yet" else "Images not ready yet",
      class = "status-missing",
      icon = "\u25cb",
      unit_label = if (is_package_step) "selected images packaged" else "images completed"
    ))
  }

  out <- status_for_output(step$outputs)
  out$unit_label <- "outputs detected"
  out
}

supports_dropout_retry <- function(step_id, integrity) {
  step_id %in% c("rename", "img_list", "fiji", "ilastik") && length(integrity$dropouts) > 0
}

step_needs_live_refresh <- function(step_id, runner) {
  isTRUE(runner$active) && identical(runner$step$id %||% "", step_id)
}

validation_post_manual_step_ids <- function() {
  c("writeback", "roi_extract", "package_rois")
}

step_needs_module_refresh <- function(step_id, runner, validation_runner) {
  if (step_needs_live_refresh(step_id, runner)) return(TRUE)
  if (isTRUE(validation_runner$active) && step_id %in% validation_post_manual_step_ids()) return(TRUE)
  FALSE
}

step_prerequisite_issues <- function(step, settings) {
  p <- build_paths(settings)
  step_id <- step$id
  issues <- character()

  if (step_id == "quantify") {
    if (!dir.exists(p$out_seg)) {
      issues <- c(issues, sprintf("Segmentation folder is missing: %s", p$out_seg))
      if (!dir.exists(p$input_master) || !file.exists(p$ilastik_project)) {
        issues <- c(issues, input_setup_guidance_text(settings))
      }
    }
    if (!identical(tolower(settings$SEG_LABEL_MAPPING_CONFIRMED %||% "false"), "true")) {
      issues <- c(issues, "Segmentation label mapping has not been confirmed yet. Use 2.A.2. Check Segmentation Label Mapping before quantifying mask areas.")
    }
  }

  if (step_id == "groups") {
    if (!file.exists(p$metadata)) {
      issues <- c(issues, sprintf("Sample metadata file is missing: %s", p$metadata))
      issues <- c(issues, input_setup_guidance_text(settings))
    }
    if (!file.exists(p$mask_percentages)) {
      issues <- c(issues, sprintf("Mask percentages file is missing: %s", p$mask_percentages))
    }
    if (!identical(tolower(settings$SEG_LABEL_MAPPING_CONFIRMED %||% "false"), "true")) {
      issues <- c(issues, "Segmentation label mapping has not been confirmed yet. Use 2.A.2. Check Segmentation Label Mapping before creating validation groups.")
    }
    confirmed_at <- suppressWarnings(as.POSIXct(settings$SEG_LABEL_MAPPING_CONFIRMED_AT %||% "", tz = Sys.timezone()))
    mask_mtime <- if (file.exists(p$mask_percentages)) file.info(p$mask_percentages)$mtime else as.POSIXct(NA)
    if (!is.na(confirmed_at) && !is.na(mask_mtime) && mask_mtime < confirmed_at) {
      issues <- c(issues, "Mask percentages are older than the confirmed segmentation label mapping. Rerun 2.A.1.4. Quantify Mask Areas before creating validation groups.")
    }
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      issues <- c(issues, "R package 'patchwork' is not installed in the R runtime that is running the app.")
    }
  }

  if (step_id == "manual") {
    if (!file.exists(p$groups)) {
      issues <- c(issues, sprintf("Validation groups table is missing: %s", p$groups))
    }
  }

  if (step_id %in% c("rename", "img_list", "fiji", "ilastik")) {
    if (!dir.exists(p$input_master)) {
      issues <- c(issues, sprintf("Master input image directory is missing: %s", p$input_master))
      issues <- c(issues, input_setup_guidance_text(settings))
    } else {
      permission_diagnostics <- input_permission_diagnostics(p$input_master)
      permission_guidance <- format_input_permission_guidance(p$input_master, permission_diagnostics)
      if (length(permission_guidance)) {
        issues <- c(issues, permission_guidance)
      }
    }
    if (dir.exists(p$input_master) && step_id == "rename") {
      image_files <- list.files(p$input_master, pattern = "\\.(tif|tiff|png|jpg|jpeg)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      image_files <- image_files[!grepl("(_RGavg|_mask)", basename(image_files), ignore.case = TRUE)]
      if (!length(image_files)) {
        issues <- c(issues, sprintf("No raw image files were found under the effective master input directory: %s", p$input_master))
        issues <- c(issues, input_setup_guidance_text(settings))
      }
    }
    if (step_id == "ilastik" && !file.exists(p$ilastik_project)) {
      issues <- c(issues, sprintf("Ilastik project file is missing: %s", p$ilastik_project))
      issues <- c(issues, input_setup_guidance_text(settings))
    }
  }

  if (step_id == "writeback") {
    if (!file.exists(p$groups)) {
      issues <- c(issues, sprintf("Validation groups table is missing: %s", p$groups))
    }
    manifest_path <- manual_review_manifest_path(settings)
    if (!file.exists(manifest_path) && !file.exists(p$inspected)) {
      issues <- c(issues, sprintf("Manual review manifest is missing: %s", manifest_path))
    }
  }

  if (step_id == "package_rois") {
    if (!file.exists(p$inspected)) {
      issues <- c(issues, sprintf("Inspected validation CSV is missing: %s", p$inspected))
    }
  }

  if (step_id == "optimize") {
    roi_integrity <- step_integrity_check(list(id = "roi_extract", outputs = file.path(p$out_opt, settings$OUT_ROI_DIR_NAME %||% "ROIs")), settings)
    if (!identical(roi_integrity$state, "complete")) {
      roi_reports <- upstream_required_report_files(step_id, settings)
      roi_reports <- roi_reports[file.exists(roi_reports)]
      issues <- c(issues, sprintf(
        "ROI extraction is not complete yet: %s%s",
        roi_integrity$summary,
          if (length(roi_reports)) paste(" Read the ROI extraction reports first:", paste(roi_reports, collapse = " ; ")) else ""
        ))
      }
    package_integrity <- step_integrity_check(list(id = "package_rois", outputs = p$opt_rds), settings)
    if (!identical(package_integrity$state, "complete")) {
      issues <- c(
        issues,
        sprintf(
          "The packaged ROI object is not ready for optimization: %s Rerun '2.A.5.3. Package ROIs for Optimization' first.",
          package_integrity$summary
        )
      )
    }
    if (!file.exists(p$opt_rds)) {
      issues <- c(issues, sprintf("Packaged ROI RDS is missing: %s", p$opt_rds))
    } else {
      info <- file.info(p$opt_rds)
      if (is.finite(info$size) && info$size < 256) {
        issues <- c(issues, sprintf("Packaged ROI RDS looks empty or stale (%s bytes). Rerun '2.A.5.3. Package ROIs for Optimization' first.", info$size))
      }
    }
    summary_csv <- file.path(p$out_opt, "optROIs_summary.csv")
    if (file.exists(summary_csv)) {
      summary_rows <- safe_csv_row_count(summary_csv)
      if (is.finite(summary_rows) && summary_rows <= 0) {
        issues <- c(issues, sprintf("ROI package summary is empty: %s. This usually means step 7 created no usable ROI payloads.", summary_csv))
      }
    }
  }

  if (step_id == "generalize") {
    if (!file.exists(p$generalized)) {
      issues <- c(
        issues,
        paste0(
          "Legacy backend generalization input is missing: ", p$generalized,
          ". This backend script is an interpretation/aggregation script that reads an already filtered generalized skeleton RDS; it does not create that RDS. ",
          "For the current app workflow, use tab 3.2 to apply selected cutoff profile(s) to the available ROI tables and save the generalized analysis table for tab 4."
        )
      )
    }
    if (!file.exists(p$inspected)) {
      issues <- c(issues, sprintf("Inspected metadata/validation table is missing: %s", p$inspected))
    }
  }

  unique(issues)
}

latest_step_history <- function(history_df, step_id) {
  subset <- history_df[history_df$step_id == step_id, , drop = FALSE]
  if (!nrow(subset)) return(NULL)
  subset <- subset[order(subset$started_at, decreasing = TRUE), , drop = FALSE]
  subset[1, , drop = FALSE]
}

detect_failure_explanation <- function(step, last_run, settings, output_text = NULL) {
  if (is.null(last_run)) return(NULL)
  if (!identical(last_run$status %||% "", "failed")) return(NULL)

  p <- build_paths(settings)

  if ((step$id %||% "") %in% c("rename", "img_list", "fiji", "ilastik") && dir.exists(p$input_master)) {
    permission_diagnostics <- input_permission_diagnostics(p$input_master)
    if (length(permission_diagnostics$untraversable_dirs %||% character()) ||
        length(permission_diagnostics$unreadable_files %||% character())) {
      return(list(
        reason = "This step failed because some input folders/files are visible but cannot be entered/read by the runtime.",
        suggestion = "Click the browser button 'Repair Input Folder Permissions' in this failure panel or in 1. Configuration, then rerun 2.A.1.0. Rename Images. This usually happens after unzipping the Zenodo archive on Linux/WSL when nested directories are missing the execute/search permission bit.",
        level = "warn",
        type = "input_permission"
      ))
    }
  }

  upstream_issues <- upstream_step_messages(step$id, settings)
  if (length(upstream_issues)) {
    return(list(
      reason = "This step is failing while one or more required upstream steps are still incomplete.",
      suggestion = paste(
        "Complete these upstream steps first:",
        paste(upstream_issues, collapse = " | ")
      ),
      level = "missing"
    ))
  }

  log_path <- last_run$log_file %||% ""
  log_lines <- if (!is.null(output_text)) {
    unlist(strsplit(as.character(output_text %||% ""), "\n", fixed = TRUE))
  } else if (nzchar(log_path) && file.exists(log_path)) {
    tryCatch(readLines(log_path, warn = FALSE), error = function(e) character())
  } else {
    character()
  }
  joined <- paste(log_lines, collapse = "\n")

  make_result <- function(reason, suggestion, level = "missing", type = NULL) {
    list(reason = reason, suggestion = suggestion, level = level, type = type)
  }

  if (identical(step$id %||% "", "rename") && grepl("Permission denied|Input permission problem detected|cannot be traversed|inaccessible", joined, ignore.case = TRUE)) {
    return(make_result(
      "This step failed because some raw-image folders are visible but cannot be entered by the runtime.",
      "Open 1. Configuration and click 'Check and Repair Input Folder Permissions', or use the repair button if it is shown in the failure dialog. After repair, rerun 2.A.1.0. Rename Images. This usually happens after unzipping the Zenodo archive on Linux/WSL when nested directories are missing the execute/search permission bit.",
      level = "warn",
      type = "input_permission"
    ))
  }

  pkg_hit <- regmatches(joined, regexpr("there is no package called[^\\n]+", joined, ignore.case = TRUE))
  if (length(pkg_hit) && nzchar(pkg_hit[[1]])) {
    pkg_name <- sub(".*called[[:space:]]+[‘'`\"]?([^’'`\"\\s]+).*", "\\1", pkg_hit[[1]])
    return(make_result(
      sprintf("This step failed because the R package `%s` is not installed in the runtime that executed the pipeline.", pkg_name),
      sprintf("Install it in that same R environment, for example with `install.packages('%s')`, then rerun the step.", pkg_name)
    ))
  }

  if (grepl("executable not found or not executable", joined, ignore.case = TRUE)) {
    bad_path <- regmatches(joined, regexpr("(?<=at: ).*", joined, perl = TRUE))
    tool_name <- if (grepl("ilastik", joined, ignore.case = TRUE)) "ilastik" else if (grepl("fiji|imagej", joined, ignore.case = TRUE)) "Fiji/ImageJ" else "the external tool"
    return(make_result(
      sprintf("This step failed because %s could not be launched from the configured path.", tool_name),
      sprintf("Check the executable path in Settings and runtime, confirm that the file exists and is executable, then rerun the step.%s",
              if (length(bad_path) && nzchar(bad_path[[1]])) paste(" Reported path:", bad_path[[1]]) else "")
    ))
  }

  if (grepl("project file not found", joined, ignore.case = TRUE) || grepl("Sample metadata file not found", joined, ignore.case = TRUE) ||
      grepl("Input directory not found", joined, ignore.case = TRUE) || grepl("Expected CSV not found", joined, ignore.case = TRUE) ||
      grepl("Mask percentages file not found", joined, ignore.case = TRUE) ||
      grepl("Missing prerequisite for generalization", joined, ignore.case = TRUE) ||
      grepl("skeletons_filtered_generalized\\.rds", joined, ignore.case = TRUE)) {
    missing_line <- grep("not found", log_lines, value = TRUE, ignore.case = TRUE)
    if (!length(missing_line)) {
      missing_line <- grep("Missing prerequisite|skeletons_filtered_generalized\\.rds", log_lines, value = TRUE, ignore.case = TRUE)
    }
    missing_line <- missing_line[nzchar(missing_line)]
    if (grepl("skeletons_filtered_generalized\\.rds", joined, ignore.case = TRUE)) {
      return(make_result(
        "This legacy backend step failed because the filtered generalized skeleton RDS is missing.",
        paste(c(
          "This script reads `OUT_generalized_filtering/skeletons_filtered_generalized.rds`; it does not create it.",
          "In the current app workflow, use tab 3.2 to select cutoff model(s), build the generalized model-feature table, and save it for tab 4.",
          utils::head(missing_line, 2)
        ), collapse = " ")
      ))
    }
    return(make_result(
      "This step failed because one of its required input files or directories is missing.",
      paste(c(
        "Check the path mentioned below, make sure the required input really exists, and then rerun the step.",
        utils::head(missing_line, 2),
        input_setup_guidance_text(read_settings_file(settings_path))
      ), collapse = " ")
    ))
  }

  if (grepl("Could not find X11 library", joined, ignore.case = TRUE) || grepl("Could not connect to existing ImageJ instance", joined, ignore.case = TRUE)) {
    return(make_result(
      "This step failed while starting Fiji/ImageJ in GUI-capable mode.",
      "For WSL users, make sure GUI support is enabled. Also close stale Fiji/ImageJ sessions and rerun the step."
    ))
  }

  if (grepl("Connection refused", joined, ignore.case = TRUE) && grepl("ImageJ", joined, ignore.case = TRUE)) {
    return(make_result(
      "This step failed because Fiji/ImageJ tried to reconnect to a stale previous session.",
      "Close old Fiji windows, remove stale ImageJ stub files if needed, and rerun the step."
    ))
  }

  if (grepl("ROI extraction completed without producing any \\.roi or _ROIs\\.zip outputs", joined, ignore.case = TRUE) ||
      grepl("Total ROIs found:[[:space:]]*0", joined, ignore.case = TRUE)) {
    return(make_result(
      "This step failed because the current segmentation masks did not produce any extractable neurite ROI objects.",
      "Check the segmentation outputs before rerunning. In the current run, the masks appear to contain no neurite-class objects for ROI extraction, so Fiji created folders but no usable .roi or _ROIs.zip files."
    ))
  }

  if (grepl("No ROI rows were produced for optimization", joined, ignore.case = TRUE)) {
    roi_reports <- upstream_required_report_files(step$id, settings)
    roi_reports <- roi_reports[file.exists(roi_reports)]
    return(make_result(
      "This step failed because the packaged ROI object is empty and contains no usable ROI entries for optimization.",
      paste0(
        "Go back to '2.A.5.2. Extract ROIs and Skeleton Summaries' and inspect the ROI-generation reports first.",
        if (length(roi_reports)) paste(" Report files:", paste(roi_reports, collapse = " ; ")) else "",
        " Then rerun '2.A.5.3. Package ROIs for Optimization' before trying the optimization step again."
      )
    ))
  }

  if (grepl("Permission denied|Access is denied", joined, ignore.case = TRUE)) {
    return(make_result(
      "This step failed because the runtime could not access a needed file or command.",
      "If this happened after unzipping the Zenodo archive on Linux/WSL, click 'Repair Input Folder Permissions' in this failure panel or open 1. Configuration and use the same repair button. The raw-image folders may be missing the directory execute/search bit. After repair, rerun the setup step. Otherwise, check file permissions, executable permissions, and whether the selected runtime can reach the configured paths.",
      type = "input_permission"
    ))
  }

  if (grepl("command not found", joined, ignore.case = TRUE)) {
    return(make_result(
      "This step failed because a required command-line tool is not installed or not visible in the selected runtime.",
      "Install the missing tool in the same runtime that is executing the pipeline, or adjust the configured command path."
    ))
  }

  if (grepl("\\[INFO\\][[:space:]]+Overlap n = 0", joined, ignore.case = TRUE)) {
    return(make_result(
      "This step failed because the metadata image names and the measured mask image names do not overlap after name normalization.",
      "Check that the metadata CSV `image_name` values match the image universe used in the pipeline. In practice, compare the metadata filenames with the names in `mask_area_percentages.csv`, and if needed adjust the naming logic in `CombineFiles_GetValGroups.R`."
    ))
  }

  if (grepl("Metadata filename mismatch detected", joined, ignore.case = TRUE)) {
    return(make_result(
      "This step failed because the metadata CSV filenames do not match the filenames in the measured mask table.",
      "Open the saved log to see example unmatched filenames from both sides, then update the metadata CSV so its `image_name` column uses the same renamed filenames as the pipeline outputs."
    ))
  }

  if (grepl("Could not find 4 groups of size", joined, ignore.case = TRUE)) {
    return(make_result(
      "This step failed because the current validation-group settings are too strict for the available data after filtering and matching.",
      "Open `CombineFiles_GetValGroups.R` and reduce `target_n`, or relax the quantile limits and thresholds. Also check whether the metadata-to-mask overlap is smaller than expected."
    ))
  }

  if (length(log_lines)) {
    tail_lines <- utils::tail(log_lines[nzchar(trimws(log_lines))], 3)
    return(make_result(
      "This step failed, but the exact cause does not match one of the known installation or dependency patterns yet.",
      paste("Check the saved log for details. Recent lines:", paste(tail_lines, collapse = " | ")),
      level = "warn"
    ))
  }

  make_result(
    "This step failed, but the app could not read a detailed log explanation.",
    "Open the saved log for this run and check the configured inputs, packages, and external tool paths."
  )
}

is_input_permission_failure_info <- function(failure_info) {
  if (is.null(failure_info)) return(FALSE)
  if (identical(failure_info$type %||% "", "input_permission")) return(TRUE)
  grepl(
    "permission|cannot be entered|cannot access|runtime could not access|folders/files are visible",
    paste(failure_info$reason %||% "", failure_info$suggestion %||% ""),
    ignore.case = TRUE
  )
}

preview_file_head <- function(path, n = 6) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("csv", "tsv")) {
    sep <- if (ext == "tsv") "\t" else ","
    df <- tryCatch(utils::read.table(path, sep = sep, header = TRUE, nrows = n, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
    if (is.null(df)) return(tags$p("Preview unavailable for this table file."))
    return(renderTable(df, striped = TRUE, bordered = TRUE, spacing = "xs"))
  }
  if (ext %in% c("txt", "log", "md")) {
    lines <- tryCatch(utils::head(readLines(path, warn = FALSE), n), error = function(e) character())
    return(tags$pre(paste(lines, collapse = "\n")))
  }
  tags$p("Interactive preview is not available for this file type. You can inspect it from the shown path.")
}

step_report_files <- function(step_id, settings) {
  p <- build_paths(settings)
  if (identical(step_id, "roi_extract")) {
    return(c(
      file.path(p$out_opt, "roi_extraction_detailed_log.txt"),
      file.path(p$out_opt, "roi_extraction_diagnostics.csv")
    ))
  }
  if (identical(step_id, "optimize")) {
    return(c(
      file.path(p$out_opt, "optimization_detailed_report.txt"),
      file.path(p$out_opt, "optimization_input_diagnostics.csv")
    ))
  }
  character()
}

upstream_required_report_files <- function(step_id, settings) {
  if (identical(step_id, "optimize")) {
    return(step_report_files("roi_extract", settings))
  }
  character()
}

build_step_summary_ui <- function(step, history_df) {
  outputs <- unique(unlist(step$outputs))
  outputs <- outputs[nzchar(outputs)]
  valid_outputs <- outputs[file.exists(outputs) | dir.exists(outputs)]
  last_run <- latest_step_history(history_df, step$id)
  current_settings <- read_settings_file(settings_path)
  readiness <- status_for_step(step, current_settings)
  integrity <- step_integrity_check(step, current_settings)
  prereq_issues <- step_prerequisite_issues(step, current_settings)
  upstream_issues <- upstream_step_messages(step$id, current_settings)
  failure_info <- detect_failure_explanation(step, last_run, current_settings)
  upstream_report_paths <- upstream_required_report_files(step$id, current_settings)
  upstream_report_paths <- upstream_report_paths[file.exists(upstream_report_paths)]
  stage_report_paths <- step_report_files(step$id, current_settings)
  stage_report_paths <- stage_report_paths[file.exists(stage_report_paths)]
  run_block <- if (is.null(last_run)) {
    tags$div(
      class = "step-summary-box step-summary-neutral",
      tags$div(class = "step-summary-title", "Run history"),
      tags$p(tags$strong("Last recorded run: "), "No recorded run yet.")
    )
  } else {
    tags$div(
      class = paste("step-summary-box", if ((last_run$status %||% "") == "completed") "step-summary-good" else "step-summary-missing"),
      tags$div(class = "step-summary-title", "Run history"),
      tags$p(tags$strong("Last recorded run: "), last_run$started_at %||% ""),
      tags$p(tags$strong("Recorded status: "), last_run$status %||% "unknown"),
      tags$p(tags$strong("Duration: "), if (is.finite(last_run$duration_seconds)) format_seconds(last_run$duration_seconds) else "unknown")
    )
  }

  outputs_block <- if (!length(outputs)) {
    tags$p("No output path is defined for this step.")
  } else {
    failure_block <- if (!is.null(failure_info)) {
      tags$div(
        class = paste("step-summary-box", if (identical(failure_info$level %||% "missing", "warn")) "step-summary-warn" else "step-summary-missing"),
        tags$div(class = "step-summary-title", "Why the last run failed"),
        tags$p(failure_info$reason),
        tags$p(tags$strong("Suggested fix: "), failure_info$suggestion),
        if (is_input_permission_failure_info(failure_info)) {
          tags$button(
            type = "button",
            class = "btn btn-warning btn-lg",
            onclick = "Shiny.setInputValue('repair_input_permissions_from_summary_btn', Math.random(), {priority: 'event'});",
            "Repair Input Folder Permissions"
          )
        }
      )
    } else {
      NULL
    }
    prereq_block <- if (length(prereq_issues)) {
      tags$div(
        class = "step-summary-box step-summary-missing",
        tags$div(class = "step-summary-title", "Missing prerequisites"),
        tags$p("This step cannot run successfully until the following required inputs exist:"),
        tags$ul(lapply(prereq_issues, function(x) tags$li(tags$span(class = "path-block", x))))
      )
    } else {
      NULL
    }
    upstream_block <- if (length(upstream_issues)) {
      tags$div(
        class = "step-summary-box step-summary-warn",
        tags$div(class = "step-summary-title", "Upstream workflow reminder"),
        tags$p("This step may fail or produce incomplete results if the following earlier workflow steps were not completed yet:"),
        tags$ul(lapply(upstream_issues, function(x) tags$li(tags$span(class = "path-block", x))))
      )
    } else {
      NULL
    }
    count_block <- if (is.finite(integrity$expected_count) || is.finite(integrity$found_count)) {
      is_package_step <- identical(step$id %||% "", "package_rois")
      tags$div(
        class = paste("step-summary-box", if (identical(integrity$state, "partial")) "step-summary-warn" else if (identical(integrity$state, "complete")) "step-summary-good" else "step-summary-neutral"),
        tags$div(class = "step-summary-title", if (is_package_step) "Selected image packaging check" else "Image continuity check"),
        if (is.finite(integrity$expected_count)) tags$p(tags$strong(if (is_package_step) "Manually selected images expected: " else "Expected images from current input: "), integrity$expected_count),
        if (is.finite(integrity$found_count)) tags$p(tags$strong(if (is_package_step) "Packaged selected image records: " else "Produced images: "), integrity$found_count),
        if (is.finite(integrity$found_total_count) && is.finite(integrity$found_count) && integrity$found_total_count > integrity$found_count) {
          tags$p(tags$strong("Extra output files currently present: "), integrity$found_total_count - integrity$found_count)
        },
        if (length(integrity$dropouts)) {
          preview_dropouts <- utils::head(integrity$dropouts, 10)
          tagList(
            tags$p(class = "warning-text", tags$strong(sprintf("Warning: %s image dropout(s) detected.", length(integrity$dropouts)))),
            tags$p("Missing or not-yet-generated items:"),
            tags$ul(lapply(preview_dropouts, function(x) tags$li(tags$span(class = "path-block", x)))),
            if (length(integrity$dropouts) > length(preview_dropouts)) {
              tags$p(sprintf("... and %s more dropout item(s).", length(integrity$dropouts) - length(preview_dropouts)))
            }
          )
        },
        if (length(integrity$extras)) {
          preview_extras <- utils::head(integrity$extras, 10)
          tagList(
            tags$p(class = "warning-text", tags$strong(sprintf("Note: %s extra output item(s) are present from outside the current expected set.", length(integrity$extras)))),
            tags$p("Examples of extra items currently in the output location:"),
            tags$ul(lapply(preview_extras, function(x) tags$li(tags$span(class = "path-block", x)))),
            if (length(integrity$extras) > length(preview_extras)) {
              tags$p(sprintf("... and %s more extra item(s).", length(integrity$extras) - length(preview_extras)))
            }
          )
        },
        if (identical(integrity$state, "partial") && supports_dropout_retry(step$id, integrity)) {
          actionButton(
            paste0("retry_dropouts_", gsub("[^A-Za-z0-9]+", "_", step$id)),
            "Retry only missing images",
            class = "btn-primary"
          )
        }
      )
    } else {
      NULL
    }
    tags$div(
      failure_block,
      prereq_block,
      upstream_block,
      count_block,
      {
        if (length(upstream_report_paths)) {
          tags$div(
            class = "step-summary-box step-summary-neutral",
            tags$div(class = "step-summary-title", "Required upstream ROI extraction reports"),
            tags$p("These upstream ROI-generation reports should be read first, before interpreting any optimization-stage output."),
            tags$ul(lapply(upstream_report_paths, function(path) tags$li(tags$span(class = "path-block", path))))
          )
        }
      },
      {
        if (length(stage_report_paths)) {
          tags$div(
            class = "step-summary-box step-summary-neutral",
            tags$div(class = "step-summary-title", "Stage-specific reports"),
            tags$p("These reports are generated specifically for this stage."),
            tags$ul(lapply(stage_report_paths, function(path) tags$li(tags$span(class = "path-block", path))))
          )
        }
      },
      tags$div(
        class = paste("step-summary-box", if (identical(integrity$state, "partial")) "step-summary-warn" else readiness$class),
        tags$div(class = "step-summary-title", "Output detection summary"),
        tags$p(tags$strong("Integrity check: "), integrity$summary),
        if (length(integrity$details)) tags$ul(lapply(integrity$details, tags$li)),
        tags$ul(
          lapply(outputs, function(path) {
            tags$li(tags$span(class = "path-block", if (file.exists(path) || dir.exists(path)) paste("Detected:", path) else paste("Not detected yet:", path)))
          })
        )
      )
    )
  }

  preview_block <- if (!length(valid_outputs)) {
    tags$p("No generated output is available yet for preview.")
  } else {
    preview_target <- valid_outputs[1]
    if (dir.exists(preview_target)) {
      entries <- list.files(preview_target, full.names = TRUE)
      entries <- entries[order(file.info(entries)$mtime, decreasing = TRUE)]
      sample_entries <- utils::head(entries, 8)
      tags$div(
        class = "step-summary-box step-summary-neutral",
        tags$div(class = "step-summary-title", "Generated output preview"),
        tags$p(tags$strong("Folder: "), tags$span(class = "path-block", preview_target)),
        tags$p("Recent contents:"),
        tags$ul(lapply(sample_entries, function(x) tags$li(basename(x))))
      )
    } else {
      ext <- tolower(tools::file_ext(preview_target))
      preview_ui <- if (ext %in% c("csv", "tsv")) {
        tableOutput(paste0("preview_", gsub("[^A-Za-z0-9]+", "_", step$id)))
      } else if (ext %in% c("txt", "log", "md")) {
        tags$pre(paste(tryCatch(utils::head(readLines(preview_target, warn = FALSE), 6), error = function(e) character()), collapse = "\n"))
      } else {
        tags$p("Interactive preview is not available for this file type. The path above can still be inspected directly.")
      }
      tags$div(
        class = "step-summary-box step-summary-neutral",
        tags$div(class = "step-summary-title", "Generated output preview"),
        tags$p(tags$strong("File: "), tags$span(class = "path-block", preview_target)),
        preview_ui
      )
    }
  }

  report_preview_block <- {
    preview_candidates <- c(upstream_report_paths, stage_report_paths)
    if (!length(preview_candidates)) {
      NULL
    } else {
      report_target <- preview_candidates[1]
      ext <- tolower(tools::file_ext(report_target))
      preview_ui <- if (ext %in% c("csv", "tsv")) {
        tableOutput(paste0("report_preview_", gsub("[^A-Za-z0-9]+", "_", step$id)))
      } else {
        tags$pre(paste(tryCatch(utils::head(readLines(report_target, warn = FALSE), 10), error = function(e) character()), collapse = "\n"))
      }
      tags$div(
        class = "step-summary-box step-summary-neutral",
        tags$div(class = "step-summary-title", if (length(upstream_report_paths)) "Required upstream ROI report preview" else "Stage report preview"),
        tags$p(tags$strong("File: "), tags$span(class = "path-block", report_target)),
        preview_ui
      )
    }
  }

  tags$div(run_block, outputs_block, report_preview_block, preview_block)
}

build_step_inline_readiness_ui <- function(step, history_df) {
  current_settings <- read_settings_file(settings_path)
  integrity <- step_integrity_check(step, current_settings)
  last_run <- latest_step_history(history_df, step$id)

  box_class <- switch(
    integrity$state %||% "missing",
    complete = "step-summary-good",
    partial = "step-summary-warn",
    missing = "step-summary-missing",
    "step-summary-neutral"
  )

  headline <- switch(
    integrity$state %||% "missing",
    complete = integrity$summary %||% "Outputs look complete.",
    partial = integrity$summary %||% "Outputs look partial or stale.",
    missing = integrity$summary %||% "Outputs are not ready yet.",
    integrity$summary %||% "No readiness summary available yet."
  )

  subline <- if (is.finite(integrity$expected_count) && integrity$expected_count > 0) {
    sprintf(
      "%s/%s items matched to current input%s",
      as.integer(integrity$found_count %||% 0),
      as.integer(integrity$expected_count),
      if (length(integrity$dropouts)) sprintf("; %s dropout(s) still detected", length(integrity$dropouts)) else ""
    )
  } else if (!is.null(last_run)) {
    sprintf(
      "Last run status: %s%s",
      last_run$status %||% "unknown",
      if (is.finite(last_run$duration_seconds)) paste0("; duration ", format_seconds(last_run$duration_seconds)) else ""
    )
  } else {
    "No recorded run yet."
  }

  tags$div(
    class = paste("step-summary-box", box_class),
    style = "margin-top:10px;",
    tags$div(class = "step-summary-title", "Quick readiness check"),
    tags$p(headline),
    tags$p(subline)
  )
}

ui <- fluidPage(
  tags$head(
    tags$title("Neurite Analysis in Phase Contrast Images"),
    tags$style(HTML("
      body { background:#f6f2eb; color:#182028; font-family:'Segoe UI', Arial, sans-serif; }
      .hero, .panel-card { background:#fffdfa; border:1px solid #e7ddcf; border-radius:18px; padding:18px; margin:16px 0; }
      .hero { background:linear-gradient(135deg,#fbf1d7 0%,#f2dcc1 50%,#d8e6dd 100%); }
      .decision-box { background:#eef5ef; border-left:6px solid #3f7d58; padding:12px 14px; border-radius:10px; margin-bottom:14px; }
      .step-grid { display:grid; grid-template-columns:repeat(auto-fit,minmax(300px,1fr)); gap:16px; }
      .step-card { background:#fff; border:1px solid #e8dfd3; border-radius:16px; padding:14px; }
      .step-manual { background:#fff8ef; border-color:#e3c7a1; }
      .step-phase { display:inline-block; padding:4px 10px; background:#173f35; color:#fff; border-radius:999px; font-size:12px; margin-bottom:8px; }
      .manual-note { background:#fef0d9; border-radius:10px; padding:10px; }
      .step-details { margin-top:10px; }
      .step-details { margin-top:10px; border:1px solid #d8d0c5; border-radius:12px; background:#fcfaf6; overflow:hidden; }
      .step-details summary {
        cursor:pointer; font-weight:700; color:#173f35; list-style:none;
        display:flex; align-items:center; gap:10px; justify-content:space-between;
        padding:12px 14px; background:#f3ece2; border-bottom:1px solid #e2d7c7;
      }
      .step-details summary::-webkit-details-marker { display:none; }
      .step-details-icon {
        display:inline-flex; align-items:center; justify-content:center; width:22px; height:22px;
        border-radius:999px; background:#173f35; color:#fff; font-size:11px; flex:0 0 auto;
        transition:transform 0.2s ease;
      }
      .step-details[open] .step-details-icon { transform:rotate(90deg); }
      .step-details-text { flex:1 1 auto; }
      .step-details-hint { font-size:12px; font-weight:600; color:#6b5f53; }
      .step-details > :not(summary) { padding:0 12px 12px 12px; }
      .step-summary-box { background:#faf6ef; border:1px solid #eadfce; border-radius:10px; padding:10px; margin-top:8px; overflow-wrap:anywhere; word-break:break-word; width:100%; box-sizing:border-box; min-width:0; }
      .step-summary-title { font-weight:700; margin-bottom:6px; overflow-wrap:anywhere; }
      .step-summary-good { background:#eef8f0; border-color:#bfd9c6; }
      .step-summary-warn { background:#fff6df; border-color:#ecd18f; }
      .step-summary-missing { background:#faecea; border-color:#dfbfbc; }
      .step-summary-neutral { background:#f7f4ef; border-color:#eadfce; }
      .step-status-pill { display:flex; align-items:center; gap:8px; border-radius:999px; padding:8px 12px; margin-top:10px; font-weight:700; width:100%; max-width:100%; overflow-wrap:anywhere; word-break:break-word; box-sizing:border-box; min-width:0; }
      .step-status-icon { display:inline-flex; align-items:center; justify-content:center; width:22px; height:22px; border-radius:999px; font-size:14px; font-weight:700; flex:0 0 auto; }
      .status-good { background:#e6f4ea; color:#1f6b3a; border:1px solid #b9dec4; }
      .status-good .step-status-icon { background:#1f6b3a; color:#fff; }
      .status-partial { background:#e6f1f5; color:#1d5f78; border:1px solid #b7d3de; }
      .status-partial .step-status-icon { background:#1d5f78; color:#fff; }
      .status-warn { background:#fff3d6; color:#8a5a00; border:1px solid #e8c97a; }
      .status-warn .step-status-icon { background:#8a5a00; color:#fff; }
      .status-missing { background:#f5e9e7; color:#9b2226; border:1px solid #e1b7b5; }
      .status-missing .step-status-icon { background:#9b2226; color:#fff; }
      .status-neutral { background:#eceff2; color:#4f5c66; border:1px solid #d4dbe2; }
      .status-neutral .step-status-icon { background:#4f5c66; color:#fff; }
      .path-block, .step-summary-box pre, .step-summary-box li, .step-summary-box p, .step-summary-box strong { overflow-wrap:anywhere; word-break:break-word; }
      .table-wrap-panel { overflow-x:auto; }
      .table-wrap-panel table { table-layout:auto; width:max-content; min-width:100%; }
      .table-wrap-panel td, .table-wrap-panel th { white-space:nowrap !important; overflow-wrap:normal; word-break:normal; max-width:280px; overflow:hidden; text-overflow:ellipsis; }
      .publication-preview-panel { margin-bottom:18px; overflow:hidden; }
      .publication-preview-frame {
        background:#fff; border:1px solid #d8d0c5; border-radius:12px; padding:10px;
        width:100%; max-height:700px; overflow:auto; display:block; clear:both;
      }
      .publication-preview-frame .shiny-image-output,
      .publication-preview-frame .shiny-bound-output {
        width:100% !important; height:auto !important; min-height:0 !important; display:block;
      }
      .publication-preview-frame img {
        display:block; max-width:100% !important; height:auto !important; margin:0 auto;
      }
      .publication-follow-panel { margin-top:18px; clear:both; position:relative; z-index:1; }
      .profile-comparison-cards { display:grid; grid-template-columns:repeat(auto-fit,minmax(260px,1fr)); gap:12px; margin-top:10px; }
      .profile-comparison-card { background:#fffdfa; border:1px solid #d7c9b8; border-radius:14px; padding:12px; box-shadow:0 4px 14px rgba(24,32,40,0.06); }
      .profile-comparison-card h5 { margin:0 0 8px 0; color:#173f35; overflow-wrap:anywhere; }
      .profile-metric-grid { display:grid; grid-template-columns:1fr auto; gap:5px 10px; font-size:13px; }
      .profile-metric-grid span:nth-child(odd) { color:#4f5c66; }
      .profile-metric-grid span:nth-child(even) { font-weight:700; color:#182028; text-align:right; }
      .metric-info-box { background:#eef5ef; border:1px solid #c9ddcf; border-radius:10px; padding:10px 12px; margin:8px 0 12px 0; font-size:13px; }
      .metric-info-box strong { color:#173f35; }
      .metric-info-box p { margin:4px 0; }
      .plot-scroll-box { width:100%; overflow-x:auto; overflow-y:hidden; padding-bottom:6px; }
      .warning-text { color:#9b2226; }
      .run-button, .save-button, .apply-button { background:#9b2226 !important; color:#fff !important; border:none !important; }
      .module-run-button {
        background:#173f35 !important; color:#fff !important; border:none !important;
        font-size:16px !important; font-weight:700 !important; padding:12px 18px !important;
        width:100%; margin:10px 0 14px 0;
      }
      .log-box { background:#10171d; color:#e8f0f6; padding:14px; border-radius:12px; min-height:320px; white-space:pre-wrap; font-family:Consolas, monospace; }
      .progress-wrap { margin:10px 0 8px 0; }
      .progress-outer { background:#e5ddd2; border-radius:999px; overflow:hidden; height:18px; }
      .progress-inner { background:linear-gradient(90deg,#173f35,#9b2226); height:18px; transition:width 0.4s ease; }
      .progress-label { font-size:13px; margin-top:6px; color:#4f5c66; }
      .manual-inspection-layout { display:grid; grid-template-columns: minmax(280px, 360px) minmax(420px, 1fr) minmax(260px, 340px); gap:16px; align-items:start; }
      .manual-panel { background:#faf6ef; border:1px solid #eadfce; border-radius:12px; padding:14px; }
      .validation-plot-panel { max-width:100%; overflow:hidden; }
      .validation-plot-frame {
        background:#fff; border:1px solid #d8d0c5; border-radius:12px; padding:10px;
        max-width:100%; height:420px; overflow:auto; display:flex; align-items:flex-start; justify-content:center;
      }
      .validation-plot-frame-large {
        background:#fff; border:1px solid #d8d0c5; border-radius:12px; padding:10px;
        max-width:100%; height:72vh; overflow:auto; display:flex; align-items:flex-start; justify-content:center;
      }
      .validation-plot-modal.modal { padding-left:0 !important; padding-right:0 !important; }
      .modal-dialog.validation-plot-modal,
      .validation-plot-modal .modal-dialog {
        width:100vw !important; max-width:none !important; height:100vh !important;
        margin:0 !important; padding:0 !important;
      }
      .modal-dialog.validation-plot-modal .modal-content,
      .validation-plot-modal .modal-content {
        width:100vw !important; height:100vh !important; border-radius:0 !important;
      }
      .modal-dialog.validation-plot-modal .modal-body,
      .validation-plot-modal .modal-body { height:calc(100vh - 120px) !important; overflow:hidden; }
      .modal-dialog.validation-plot-modal .validation-plot-frame-large,
      .validation-plot-modal .validation-plot-frame-large { height:calc(100vh - 230px) !important; }
      .validation-plot-zoomed { transform-origin: top center; transition: transform 0.12s ease; }
      .validation-plot-frame .shiny-image-output,
      .validation-plot-frame-large .shiny-image-output { width:900px !important; height:auto !important; }
      .validation-plot-frame img,
      .validation-plot-frame-large img { width:900px !important; height:auto !important; max-width:none !important; display:block; margin:0 auto; }
      .modal-top-close {
        position:absolute; top:10px; right:14px; z-index:2000;
        width:34px; height:34px; border-radius:999px; border:1px solid #d8d0c5;
        background:#fffdfa; color:#173f35; font-size:24px; line-height:28px;
        display:flex; align-items:center; justify-content:center; cursor:pointer;
        box-shadow:0 2px 8px rgba(0,0,0,0.12);
      }
      .modal-top-close:hover { background:#9b2226; color:#fff; border-color:#9b2226; }
      .modal-content { position:relative; }
      .manual-image-frame { background:#fff; border:1px solid #d8d0c5; border-radius:12px; overflow:auto; max-height:720px; min-height:420px; padding:10px; text-align:center; }
      .manual-image-frame img { display:block; margin:0 auto; max-width:none; }
      .manual-list-box { max-height:240px; overflow:auto; border:1px solid #e3d7c8; border-radius:10px; padding:8px; background:#fff; }
      .manual-current-label { font-weight:700; margin:8px 0; }
      .manual-action-row { display:flex; flex-wrap:wrap; gap:8px; margin:10px 0; }
      .manual-action-row .btn { flex:1 1 120px; }
      .setting-row { margin-bottom: 10px; }
      .setting-label-row { display:flex; align-items:center; gap:8px; margin-bottom: 4px; }
      .setting-label { margin:0; font-weight:600; }
      .help-dot {
        display:inline-flex; align-items:center; justify-content:center;
        width:20px; height:20px; border-radius:999px; text-decoration:none !important;
        background:#173f35; color:#fff !important; font-weight:700; font-size:12px;
      }
      .help-dot:hover { background:#9b2226; color:#fff !important; }
    "))
  ),
  div(
    class = "hero",
    h1("Neurite Analysis in Phase Contrast Images"),
    p("This app runs the active backend scripts from the browser, explains the workflow in beginner-friendly language, and keeps the real decision logic visible: first check for a saved validated cutoff profile, then choose either fresh validation or profile reuse, then continue with generalization and the Visualization and Interpretation module.")
  ),
  div(
    class = "panel-card",
    h2("About"),
    tabsetPanel(
      id = "about_tabs",
      tabPanel(
        "1. Algorithmic Logic",
        div(
          class = "decision-box",
          h3("Core Workflow Logic"),
          tags$p("The app is organized around one central decision: either create a new validated neurite cutoff model from test images, or reuse a previously saved cutoff profile. Once a trusted profile exists, it can be applied to the full image set without repeating manual validation every time."),
          tags$ol(
            tags$li(tags$strong("Configure the project."), " Confirm the project root, metadata CSV, raw image master directory, output directory, Fiji, ilastik, R, and runtime mode. The default project root ", tags$code("."), " means the folder where the app files live."),
            tags$li(tags$strong("Set up and preprocess images."), " The setup chain renames raw images into unique hard-linked names, writes the exact image universe into ", tags$code("OUT_img_input1.txt"), ", creates Fiji R/G-average images, runs ilastik segmentation, verifies label mapping, and quantifies mask areas."),
            tags$li(tags$strong("Decide validation strategy."), " If no trusted cutoff profile exists, run fresh validation. If one exists, choose whether to reuse it or intentionally create a new one."),
            tags$li(tags$strong("Validate new cutoffs when needed."), " The app creates high/low neurite/confluence validation groups, supports manual inspection in-browser, writes labels back, extracts ROIs and skeleton summaries with Fiji, packages ROI data, and optimizes cutoffs."),
            tags$li(tags$strong("Review cutoff models."), " The app compares saved models, checks trust/artefact indicators, visualizes retained versus candidate ROIs, and supports classic, continuity-aware, topology-aware, and ensemble-style optimization logic."),
            tags$li(tags$strong("Apply the selected model to all images."), " The production generalization step applies validated cutoff profiles to all ROI folders and writes analysis-ready feature tables under ", tags$code("pipeline_outputs/OUT_generalized_analysis/"), "."),
            tags$li(tags$strong("Interpret biological results."), " Tab 4 provides training-image audit, PCA/separation, feature screening, grouped dot/bar plots, covariate correlation checks, adjusted ANCOVA-style modeling, and publication figure/table candidate management."),
            tags$li(tags$strong("Export reproducible outputs."), " Tab 5 builds a report archive with settings, scripts, logs, command history, software/package versions, selected profiles, key outputs, publication candidates, and recreation notes.")
          )
        ),
        div(
          class = "manual-panel",
          h3("Backend Script Hierarchy"),
          tags$p("The frontend does not hide the backend. Each app module corresponds to active Bash, Fiji/ImageJ macro, ilastik, or R scripts in the project root."),
          tags$ul(
            tags$li(tags$strong("Setup and preprocessing: "), tags$code("0.Rename.sh"), " -> ", tags$code("1.get_img_list.sh"), " -> ", tags$code("2.RunFIJI_clean_bckgnd.sh"), " / ", tags$code("Macro_1-clear_bckgnd.ijm"), " -> ", tags$code("3.Run_ilastik_model.sh"), " -> ", tags$code("4.GetArea%Fiji.sh"), " / ", tags$code("Macro_2-get_mask_area_percentages.ijm"), "."),
            tags$li(tags$strong("Validation grouping: "), tags$code("5.SelectValidationImgGroups.sh"), " runs ", tags$code("CombineFiles_GetValGroups.R"), " to merge metadata with mask-area outputs and create the four validation quadrants."),
            tags$li(tags$strong("Manual label writeback: "), tags$code("6.SelectedValidationImgWriteSee.sh"), " runs ", tags$code("CombinedFiles_after_inspection.R"), " and updates inspected choice/noise labels."),
            tags$li(tags$strong("ROI and skeleton extraction: "), tags$code("7.RunOptimization.sh"), " runs ", tags$code("saveROIs.ijm"), " to export ROI payloads and skeleton summaries from segmentation masks."),
            tags$li(tags$strong("Optimization: "), tags$code("0.ReadROIdata-to-optimize.R"), " packages selected ROI data, then ", tags$code("1.OptimizeNeuriteParams-MAJOR_OPTIMIZE.R"), " scores cutoff settings and writes reusable optimization outputs."),
            tags$li(tags$strong("Production application: "), tags$code("3.Apply_cutoff_profile_to_all_images.R"), " applies saved cutoff profiles to all available ROI folders and writes the generalized model-feature table used by Tab 4."),
            tags$li(tags$strong("Legacy/supporting interpretation scripts: "), tags$code("2.1.Generalize_image_threshold_skeleton_rule.R"), ", ", tags$code("3.1.analyzed-generalized-skeleton-data-PCA+ALMOST_WORKING.R"), ", and ", tags$code("4.1.Skeleton-cutoff-ploting-improved-polished+BG.R"), " remain available for reproducibility and comparison.")
          )
        ),
        div(
          class = "manual-panel",
          h3("What This App Controls"),
          tags$ul(
            tags$li("Configuration, software checks, and path selection for Windows/WSL/Linux-style use."),
            tags$li("The exact image universe through ", tags$code("OUT_img_input1.txt"), ", so later steps process the intended images rather than whatever happens to be in a folder."),
            tags$li("Dropout checks and partial-output diagnostics, including the option to continue, retry supported dropouts, or regenerate when appropriate."),
            tags$li("Segmentation label mapping for background, cell bodies, and neurites, so mask-area quantification uses the actual ilastik output labels."),
            tags$li("Manual validation inspection, choice/noise/not-sure bookkeeping, and saved validation cutoff-profile creation."),
            tags$li("All-image production application of selected cutoff profiles and downstream biological visualization/statistical interpretation."),
            tags$li("Publication plot/table candidate saving, style editing, frozen metadata snapshots, high-quality downloads, and reproducible report archives.")
          )
        ),
        div(
          class = "manual-panel",
          h3("Where Data and Outputs Belong"),
          tags$ul(
            tags$li(tags$strong("Scripts and app code: "), "project root, committed to GitHub."),
            tags$li(tags$strong("Raw input data and downloaded Zenodo data: "), tags$code("pipeline_inputs/"), " or an external folder selected in Configuration. Large files are ignored by Git."),
            tags$li(tags$strong("Generated outputs: "), tags$code("pipeline_outputs/"), " so outputs can be cleared without deleting scripts."),
            tags$li(tags$strong("Reusable cutoff profiles, run logs, publication candidates, and caches: "), tags$code("cache/"), " locally ignored by Git unless deliberately archived in a report.")
          )
        ),
        tags$p(tags$strong("Interactive help page: "), "keep ", tags$code("frontend/index.html"), " synchronized with this app whenever workflow logic, script choices, or user explanations change.")
      ),
      tabPanel(
        "2. Software Requirements",
        div(
          class = "decision-box",
          h3("Required Software Stack"),
          tags$p("The app is a browser interface, but the analysis is performed by the backend scientific tools. A complete setup needs R/Rscript, Bash, Fiji/ImageJ, ilastik, ImageMagick, and Java. On Windows, the most reproducible path is usually to run the backend through WSL/Linux while opening the Shiny app in a browser.")
        ),
        tags$div(
          class = "step-grid",
          div(class = "manual-panel", label_with_help("WSL for Windows users", "install_wsl"), p("Recommended for Windows users when Fiji, ilastik, Bash, and R are installed in the Linux/WSL environment. If you provide Windows executables while running inside WSL, path and GUI behavior can become fragile; prefer one coherent runtime environment.")),
          div(class = "manual-panel", label_with_help("R and Rscript", "install_r"), p("Required for the Shiny app, grouping, manual-label writeback, ROI packaging, cutoff optimization, production generalization, PCA, feature screening, covariate checks, ANCOVA, and report generation.")),
          div(class = "manual-panel", label_with_help("Fiji / ImageJ", "install_fiji"), p("Required for preprocessing raw color images into R/G-average intensity images, quantifying mask areas through ImageJ macros, and extracting ROIs/skeleton summaries. GUI-capable Fiji mode is preferred where possible because some headless Fiji/plugin combinations can differ.")),
          div(class = "manual-panel", label_with_help("ilastik", "install_ilastik"), p("Required for pixel-classification segmentation. The app expects a trained project file compatible with the preprocessed images and classes for background, cell bodies, and neurites. The default Linux fallback searches under the ilastik subfolder and the downloaded Zenodo input structure.")),
          div(class = "manual-panel", label_with_help("ImageMagick / convert", "install_imagemagick"), p("Used by the ilastik runner for mask renormalization in workflows that need conversion from raw ilastik masks into the label encoding expected downstream.")),
          div(class = "manual-panel", label_with_help("Required R packages", "install_r_packages"), p("Install packages in the same R environment that runs both the app and command-line R scripts. The app includes detected package-version tables to support reproducibility. Deprecated optional packages are avoided where possible.")),
          div(class = "manual-panel", label_with_help("Input data and ilastik model", "input_data_setup"), p("For the published example dataset, download the Zenodo archive and unzip it inside pipeline_inputs/. The app can detect raw images, metadata, ilastik model files, and ilastik training-image folders there while keeping those large files ignored by Git.")),
          div(class = "manual-panel", label_with_help("Metadata CSV", "metadata_file"), p("A metadata CSV is required for validation grouping and biological interpretation. It should include an image_name field that can be matched to the path-flattened renamed image names, plus experimental variables such as treatment, concentration, day, confluence-related fields, or other study factors."))
        ),
        div(
          class = "manual-panel",
          h3("Runtime Notes for Windows, WSL, and Linux"),
          tags$ul(
            tags$li("Native Linux users can usually run Bash, Rscript, Fiji, and ilastik directly from the same filesystem."),
            tags$li("Windows users with WSL should decide whether the backend is WSL/Linux-based or Windows-native. Mixing Windows executables into a WSL-run backend can fail because paths, GUI forwarding, and library discovery differ."),
            tags$li("For Fiji in WSL, GUI support should be enabled when possible. The app help text intentionally explains this because GUI and headless Fiji can sometimes produce different ImageJ macro/plugin behavior."),
            tags$li("The Configuration tab can detect or accept executable paths, but every backend script should use settings-derived paths rather than hard-coded user-specific absolute paths.")
          )
        )
      ),
      tabPanel(
        "3. Reproducibility",
        div(
          class = "decision-box",
          h3("How the App Makes Runs Reproducible"),
          tags$p("Reproducibility is handled at several levels: GitHub stores the code and documentation, Zenodo stores large raw data/model artifacts, the app records settings and command history, validation profiles preserve cutoff decisions, and report archives bundle the analysis context.")
        ),
        div(
          class = "manual-panel",
          h3("Saved Reproducibility Artifacts"),
          tags$ul(
            tags$li(tags$strong("Settings: "), tags$code("config/pipeline_settings.env"), " is local and ignored by Git; ", tags$code("config/pipeline_settings.example.env"), " is committed as a template."),
            tags$li(tags$strong("Run logs and command history: "), "stored under ", tags$code("cache/run_logs/"), " and command-history cache so failures, elapsed time, and backend commands can be reviewed."),
            tags$li(tags$strong("Validation cutoff profiles: "), tags$code("cache/validation_profiles/"), " contains saved cutoff models with optimization outputs and metadata for reuse."),
            tags$li(tags$strong("Production generalized analysis table: "), tags$code("pipeline_outputs/OUT_generalized_analysis/generalized_model_feature_table.csv"), " and ", tags$code(".rds"), " are the main Tab 4 handoff."),
            tags$li(tags$strong("Publication candidates: "), "saved figures and tables include source PNG/CSV, style metadata, input snapshots, frozen human-readable metadata text, and reproducibility scaffolds."),
            tags$li(tags$strong("Full report archive: "), "Tab 5 can export a report with settings, scripts/macros, software versions, package versions, logs, selected profiles, output tables, plots, and recreation instructions.")
          )
        ),
        div(
          class = "manual-panel",
          h3("Repository and Dataset Separation"),
          tags$p("The GitHub repository is intentionally lightweight. Raw microscopy images, ilastik training data, trained model files, generated masks, ROI archives, RDS outputs, logs, and report archives are ignored by Git. The published dataset should be retrieved from Zenodo and placed under ", tags$code("pipeline_inputs/"), " as described in the repository README."),
          tags$p("This separation keeps the code reviewable and keeps large scientific data in an archival data repository where it can receive a DOI.")
        ),
        div(class = "manual-panel",
            label_with_help("Software versions for reproducibility", "software_versions"),
            p("These tables document the current environment before you share the project or rerun it elsewhere. Include these details in reports when possible."),
            tableOutput("software_versions_table"),
            br(),
            label_with_help("R package versions detected from project scripts", "package_versions"),
            tableOutput("package_versions_table"))
      )
    )
  ),
  div(
    class = "panel-card",
    h2("Workplace"),
    tabsetPanel(
      id = "workplace_tabs",
      tabPanel(
        "1. Configuration",
        div(
          class = "manual-panel step-summary-warn",
          h3("If Zenodo Images Are Missing or Rename Images Fails"),
          tags$p("After directly unzipping the Zenodo dataset on Linux/WSL, some nested folders can be visible but not enterable. That makes the app find only part of the images and can make 2.A.1.0. Rename Images fail with a generic access error."),
          tags$p("Use this browser button first. It repairs only the selected input image tree under the current pipeline inputs; it does not touch scripts, Git files, cache, or generated outputs."),
          actionButton("repair_input_permissions_top_btn", "Repair Input Folder Permissions", class = "btn btn-warning btn-lg"),
          actionButton("refresh_input_permission_status_top_btn", "Recheck Input Folder", class = "btn btn-default"),
          tags$p(class = "helper-text", "After the repair finishes, rerun 2.A.1.0. Rename Images. If there is no permission problem, the button is harmless and simply confirms the input tree is accessible.")
        ),
        uiOutput("input_permission_callout_ui"),
        fluidRow(
          column(6,
                 div(class = "manual-panel",
                     label_with_help("Shared settings", "project_root"),
                     p("Read these settings in setup order: first confirm where the project is, then choose the metadata CSV, then choose which raw-image directory should actually be scanned, and then where generated outputs should be stored. In the base-directory field, . means here, namely the current project folder, and that default is usually the right choice."),
                     setting_row_with_browse("Project root", textInput("project_root_input", NULL, value = "."), "project_root", "browse_project_root"),
                     setting_row_with_browse("Select metadata CSV file", textInput("metadata_file_input", NULL, value = ""), "metadata_file", "browse_metadata_file"),
                     setting_row_with_browse("Master input directory for recursive image scan", textInput("input_master_input", NULL, value = "pipeline_inputs"), "input_master", "browse_input_master"),
                     setting_row_with_browse("Central output workspace", textInput("output_root_input", NULL, value = "pipeline_outputs"), "output_root", "browse_output_root"),
                     setting_row("PRE_renamed folder name", textInput("pre_renamed_input", NULL, value = ""), "pre_renamed"),
                     setting_row("Image list filename", textInput("image_list_name_input", NULL, value = ""), "image_list_name"),
                     setting_row("Optimization output folder", textInput("out_optimize_input", NULL, value = ""), "out_optimize"),
                     setting_row("Fiji executable", textInput("fiji_bin_input", NULL, value = ""), "fiji_bin"),
                     setting_row("ilastik executable", textInput("ilastik_bin_input", NULL, value = ""), "ilastik_bin"),
                     setting_row("ilastik project file", textInput("ilastik_project_input", NULL, value = ""), "ilastik_project"),
                     setting_row_with_browse("Validation profile folder", textInput("validation_dir_input", NULL, value = ""), "validation_dir", "browse_validation_dir"),
                     actionButton("save_settings_btn", "Save settings", class = "save-button"))
          ),
          column(6,
                 div(class = "manual-panel",
                     label_with_help("Runtime and input helpers", "runtime_choice"),
                     div(class = "decision-box", "If you are using WSL for Fiji, make sure GUI support is enabled. GUI-capable Fiji mode is preferred because some headless Fiji runs can produce different outputs."),
                     setting_row("Run the backend in", selectInput("runtime_choice", NULL, choices = detect_runtime_choices()), "runtime_choice"),
                     setting_row_with_browse("Search root for 0.Rename.sh", textInput("rename_search_root", NULL, value = "."), "rename_search_root", "browse_rename_search_root"),
                     setting_row_with_browse("Folder to scan for 1.get_img_list.sh", textInput("image_list_source", NULL, value = file.path("pipeline_outputs", "PRE_renamed")), "image_list_source", "browse_image_list_source"),
                     uiOutput("input_permission_status_ui"),
                     actionButton("repair_input_permissions_btn", "Check and Repair Input Folder Permissions", class = "btn btn-warning"),
                     tags$p(class = "helper-text", "Use this after unzipping the Zenodo archive if the app finds too few images or reports Permission denied. It repairs only the selected input image tree."),
                     actionButton("refresh_checks_btn", "Refresh checks"),
                     br(), br(),
                    label_with_help("Runtime summary", "runtime_table"),
                    tableOutput("runtime_table"),
                    br(),
                    div(class = "decision-box", tags$strong("App version marker: "), tags$code(app_version_marker)))
          )
        ),
        br(),
        div(class = "decision-box", "Configuration is only for selecting paths, metadata, runtime, and executable settings. The setup workflow cards live under 2.A.1. Setup and Preprocessing.")
      ),
      tabPanel(
        "2. Validate New / Use Prior Cutoffs Model",
        uiOutput("validation_workspace_ui")
      ),
      tabPanel(
        "3. Analyze Test Images",
        uiOutput("generalization_phase_ui")
      ),
      tabPanel(
        "4. Visualization and Interpretation",
        uiOutput("visualization_phase_ui")
      ),
      tabPanel(
        "5. Reproducible Report",
        div(
          class = "decision-box",
          "Create a full downloadable analysis report and an accompanying archive folder. The report documents settings, software versions, command history, selected cutoff profiles, key outputs, plots, scripts, and enough metadata to recreate the workflow later.",
          tags$div(
            class = "step-summary-box step-summary-neutral",
            tags$strong("What is included by default"),
            tags$p("The default archive includes compact reproducibility attachments: configuration, active scripts/macros, run logs, command history, saved profile metadata, key CSV/RDS outputs, and generated plots. Large image folders and ROI payload folders are optional so the report does not accidentally become enormous.")
          ),
          fluidRow(
            column(
              4,
              div(
                class = "manual-panel",
                label_with_help("Report archive settings", "output_status"),
                textInput("report_name_input", "Report name", value = paste0("neurite_analysis_report_", format(Sys.Date(), "%Y%m%d"))),
                uiOutput("report_profile_selector_ui"),
                checkboxGroupInput(
                  "report_include_sections",
                  "Include attachment groups",
                  choices = c(
                    "Configuration and environment" = "config",
                    "Backend scripts and macros" = "scripts",
                    "Run logs and command history" = "logs",
                    "Validation/cutoff profile metadata" = "profiles",
                    "Key output tables and RDS files" = "tables",
                    "Generated plots and reports" = "plots"
                  ),
                  selected = c("config", "scripts", "logs", "profiles", "tables", "plots")
                ),
                checkboxInput("report_include_large_outputs", "Also include large image/ROI folders", value = FALSE),
                tags$p(class = "warning-text", "Large folders may include hundreds of TIFF/ROI files and can make the archive very large."),
                actionButton("generate_report_archive_btn", "Generate Report Archive", class = "run-button"),
                br(), br(),
                downloadButton("download_report_archive_btn", "Download Latest Report Archive")
              )
            ),
            column(
              8,
              uiOutput("report_archive_status_ui"),
              div(class = "manual-panel table-wrap-panel", tags$h4("Report Contents Preview"), tableOutput("report_manifest_preview_table")),
              div(class = "manual-panel", tags$h4("Recreation Notes"), htmlOutput("report_recreation_notes_ui"))
            )
          )
        )
      )
    )
  ),
  div(
    class = "panel-card",
    h2("Run Monitor"),
    tabsetPanel(
      id = "monitor_tabs",
      tabPanel(
        "1. Live Log",
        actionButton("clear_log_btn", "Clear Live Log"),
        br(), br(),
        div(class = "log-box", verbatimTextOutput("log_output", placeholder = TRUE)),
        br(),
        downloadButton("download_live_log_btn", "Download Current Live Log"),
        br(), br(),
        label_with_help("Command history", "execution_log"),
        tableOutput("history_table"),
        br(),
        downloadButton("download_history_btn", "Download command history CSV"),
        br(), br(),
        selectInput("saved_log_choice", "Saved run log", choices = c("No saved logs yet" = "")),
        downloadButton("download_saved_log_btn", "Download selected log")
      ),
      tabPanel(
        "2. Current Run Monitor",
        label_with_help("Current Run Monitor", "execution_log"),
        uiOutput("current_run_ui"),
        br(),
        label_with_help("Timing estimate logic", "execution_log"),
        verbatimTextOutput("timing_explanation"),
        br(),
        label_with_help("Key output checks", "output_status"),
        tableOutput("output_status_table")
      )
    )
  )
)

server <- function(input, output, session) {
  tick <- reactiveVal(0)
  log_lines <- reactiveVal(character())
  recorded_plots <- reactiveValues()
  publication_tick <- reactiveVal(0)
  publication_figure_dir <- file.path(project_root, "cache", "publication_figures")
  publication_table_dir <- file.path(project_root, "cache", "publication_tables")
  publication_preview_dir <- file.path(tempdir(), paste0("pcna_publication_preview_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample.int(1000000, 1)))
  dir.create(publication_preview_dir, recursive = TRUE, showWarnings = FALSE)
  publication_preview_cache <- reactiveValues(key = "", path = "")
  session$onSessionEnded(function() {
    unlink(publication_preview_dir, recursive = TRUE, force = TRUE)
  })

  remember_plot <- function(id) {
    try(recorded_plots[[id]] <- grDevices::recordPlot(), silent = TRUE)
    invisible(NULL)
  }

  install_plot_download <- function(id, title = id, width = 3000, height = 2200, res = 300) {
    output[[paste0("download_", id)]] <- downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9._-]+", "_", title), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
      content = function(file) {
        grDevices::png(file, width = width, height = height, res = res)
        on.exit(grDevices::dev.off(), add = TRUE)
        plot_obj <- recorded_plots[[id]]
        if (!is.null(plot_obj)) {
          grDevices::replayPlot(plot_obj)
        } else {
          plot.new()
          text(0.5, 0.5, "Open or refresh this plot in the app before downloading it.")
        }
      }
    )
  }

  publication_plot_choices <- c(
    "PCA score plot" = "dimred_pca_plot",
    "PCA scree plot" = "dimred_scree_plot",
    "PCA separation plot" = "dimred_separation_plot",
    "Contrast feature ranking" = "contrast_variable_plot",
    "Grouped dot and bar plot" = "generalization_grouped_plot",
    "Facet split helper plot" = "generalization_facet_helper_plot",
    "Covariate correlation heatmap" = "covariate_correlation_heatmap",
    "Cutoff profile effectiveness" = "cutoff_profile_effectiveness_plot",
    "Four-group neurite implication plot" = "cutoff_group_dot_plot",
    "Cutoff score overview" = "cutoff_review_score_plot",
    "Cutoff retention overview" = "cutoff_review_retention_plot"
  )

  publication_plot_label <- function(id) {
    names(publication_plot_choices)[match(id, publication_plot_choices)] %||% id
  }

  plot_display_label <- function(x, width = 28, multiline = TRUE) {
    x <- as.character(x %||% "")
    out <- gsub("_", " ", x, fixed = TRUE)
    out <- gsub("\\.", " ", out)
    out <- gsub("pct", "%", out, ignore.case = TRUE)
    out <- gsub("percent", "%", out, ignore.case = TRUE)
    out <- gsub("\\broi\\b", "ROI", out, ignore.case = TRUE)
    out <- gsub("\\bpca\\b", "PCA", out, ignore.case = TRUE)
    out <- gsub("\\bpc([0-9]+)\\b", "PC\\1", out, ignore.case = TRUE)
    out <- gsub("\\bfbs\\b", "FBS", out, ignore.case = TRUE)
    out <- gsub("\\bsh sy5y\\b", "SH-SY5Y", out, ignore.case = TRUE)
    out <- gsub("\\bfirst differentiating agent\\b", "Differentiating agent", out, ignore.case = TRUE)
    out <- gsub("\\bmicromolar\\b", "uM", out, ignore.case = TRUE)
    out <- gsub("\\s+", " ", trimws(out))
    out <- vapply(out, function(label) {
      if (!nzchar(label) || !multiline || nchar(label) <= width) return(label)
      paste(strwrap(label, width = width), collapse = "\n")
    }, character(1))
    unname(out)
  }

  plot_display_labels <- function(x, width = 28, multiline = TRUE) {
    vapply(as.character(x), plot_display_label, character(1), width = width, multiline = multiline)
  }

  publication_style_defaults <- function() {
    list(
      title = "",
      subtitle = "",
      x_label = "",
      y_label = "",
      x_tick_labels = "",
      y_tick_labels = "",
      legend_text = "",
      caption = "",
      font_family = "Arial",
      auto_crop_original_text = "true",
      hide_original_title = "true",
      hide_original_x_axis = "true",
      hide_original_y_axis = "true",
      hide_original_x_ticks = "true",
      hide_original_y_ticks = "true",
      source_crop_top = "0",
      source_crop_bottom = "0",
      source_crop_left = "0",
      source_crop_right = "0",
      title_size = "18",
      subtitle_size = "12",
      x_axis_size = "12",
      y_axis_size = "12",
      x_tick_size = "9",
      y_tick_size = "9",
      x_axis_angle = "0",
      y_axis_angle = "90",
      x_tick_angle = "0",
      y_tick_angle = "0",
      x_axis_distance = "0.035",
      y_axis_distance = "0.035",
      x_tick_distance = "0.075",
      y_tick_distance = "0.075",
      title_distance = "0.035",
      subtitle_distance = "0.065",
      caption_distance = "0.060",
      plot_margin = "0.025",
      plot_margin_top = "0",
      plot_margin_right = "0",
      plot_margin_bottom = "0",
      plot_margin_left = "0",
      legend_size = "9",
      legend_x_nudge = "0",
      legend_y_nudge = "0",
      legend_symbol_type = "none",
      legend_colors = "",
      caption_size = "10",
      caption_x_nudge = "0",
      caption_y_nudge = "0",
      italicize_title = "false",
      italicize_subtitle = "false",
      italicize_x_axis = "false",
      italicize_y_axis = "false",
      italicize_x_ticks = "false",
      italicize_y_ticks = "false",
      bold_title = "true",
      bold_subtitle = "false",
      bold_x_axis = "false",
      bold_y_axis = "false",
      bold_x_ticks = "false",
      bold_y_ticks = "false",
      notes = ""
    )
  }

  publication_snapshot_inputs <- function() {
    vals <- reactiveValuesToList(input, all.names = TRUE)
    keep <- grep("^(tab4_|dimred_|contrast_|generalization_|covariate_|ancova_|cutoff_)", names(vals), value = TRUE)
    vals <- vals[keep]
    rows <- lapply(names(vals), function(nm) {
      value <- vals[[nm]]
      value <- if (length(value) > 1) paste(value, collapse = " | ") else as.character(value %||% "")
      data.frame(input_id = nm, value = value, stringsAsFactors = FALSE)
    })
    if (length(rows)) do.call(rbind, rows) else data.frame(input_id = character(), value = character())
  }

  publication_read_input_snapshot <- function(fig_dir) {
    snapshot_path <- file.path(fig_dir %||% "", "input_snapshot.csv")
    if (!nzchar(fig_dir %||% "") || !file.exists(snapshot_path)) {
      return(data.frame(input_id = character(), value = character(), stringsAsFactors = FALSE))
    }
    snapshot <- tryCatch(
      utils::read.csv(snapshot_path, stringsAsFactors = FALSE, check.names = FALSE),
      error = function(e) data.frame(input_id = character(), value = character(), stringsAsFactors = FALSE)
    )
    if (!is.data.frame(snapshot) || !all(c("input_id", "value") %in% names(snapshot))) {
      return(data.frame(input_id = character(), value = character(), stringsAsFactors = FALSE))
    }
    snapshot
  }

  publication_snapshot_tokens <- function(value) {
    value <- as.character(value %||% "")
    if (!nzchar(value)) return(character())
    tokens <- trimws(unlist(strsplit(value, "\\s*\\|\\s*", perl = TRUE)))
    tokens[nzchar(tokens)]
  }

  publication_restore_inputs_from_snapshot <- function(snapshot) {
    if (!is.data.frame(snapshot) || !nrow(snapshot) || !all(c("input_id", "value") %in% names(snapshot))) return(0L)
    restored <- 0L
    for (i in seq_len(nrow(snapshot))) {
      input_id <- as.character(snapshot$input_id[[i]] %||% "")
      value <- as.character(snapshot$value[[i]] %||% "")
      if (!nzchar(input_id)) next
      tokens <- publication_snapshot_tokens(value)
      bool_value <- tolower(value) %in% c("true", "false")
      numeric_tokens <- suppressWarnings(as.numeric(tokens))
      is_numeric_tokens <- length(tokens) > 0 && all(!is.na(numeric_tokens))
      is_range_input <- grepl("(_range|_bounds|_limit|_limits)$|numeric_range", input_id)
      is_multi_input <- grepl("(features|loading_pcs|covariates|corr_vars|filter_values|filter_numeric_values|profiles|models)$", input_id)
      is_numeric_input <- grepl("(max_rows|random_seed|size|distance|nudge|angle|margin|crop|width|height|alpha|point|line|text)$", input_id)

      tryCatch({
        if (bool_value) {
          updateCheckboxInput(session, input_id, value = identical(tolower(value), "true"))
        } else if (is_multi_input) {
          updateSelectizeInput(session, input_id, selected = tokens)
          updateCheckboxGroupInput(session, input_id, selected = tokens)
          updateSelectInput(session, input_id, selected = tokens)
        } else if (is_range_input && is_numeric_tokens && length(numeric_tokens) >= 2) {
          updateSliderInput(session, input_id, value = range(numeric_tokens[seq_len(2)], na.rm = TRUE))
          updateTextInput(session, input_id, value = value)
        } else if (is_numeric_input && is_numeric_tokens && length(numeric_tokens) == 1) {
          updateNumericInput(session, input_id, value = numeric_tokens[[1]])
        } else {
          selected_value <- if (length(tokens) > 1) tokens else value
          updateSelectInput(session, input_id, selected = selected_value)
          updateSelectizeInput(session, input_id, selected = selected_value)
          updateTextInput(session, input_id, value = value)
          updateTextAreaInput(session, input_id, value = value)
        }
        restored <<- restored + 1L
      }, error = function(e) {
        NULL
      })
    }
    restored
  }

  publication_metadata_value <- function(x, max_chars = 700) {
    x <- as.character(x %||% "")
    x[is.na(x)] <- ""
    x <- paste(x, collapse = " | ")
    x <- gsub("[\r\n]+", " ", x)
    if (nchar(x) > max_chars) paste0(substr(x, 1, max_chars), " ...") else x
  }

  publication_snapshot_lines <- function(snapshot, ids = NULL, pattern = NULL, title = NULL) {
    if (!is.data.frame(snapshot) || !nrow(snapshot) || !all(c("input_id", "value") %in% names(snapshot))) return(character())
    keep <- rep(TRUE, nrow(snapshot))
    if (!is.null(ids)) keep <- snapshot$input_id %in% ids
    if (!is.null(pattern)) keep <- keep & grepl(pattern, snapshot$input_id, ignore.case = TRUE)
    rows <- snapshot[keep, , drop = FALSE]
    if (!nrow(rows)) return(character())
    c(
      if (nzchar(title %||% "")) c("", paste0("## ", title)) else character(),
      sprintf("- %s: %s", rows$input_id, vapply(rows$value, publication_metadata_value, character(1)))
    )
  }

  publication_context_metadata <- function(snapshot, style = NULL, object_id = "", table_control_id = NULL) {
    snapshot <- if (is.data.frame(snapshot)) snapshot else data.frame(input_id = character(), value = character())
    rows <- list()
    add_rows <- function(section, df, key_col = "input_id", value_col = "value", key_prefix = "") {
      if (!is.data.frame(df) || !nrow(df) || !all(c(key_col, value_col) %in% names(df))) return(invisible(NULL))
      rows[[length(rows) + 1L]] <<- data.frame(
        section = section,
        key = paste0(key_prefix, df[[key_col]]),
        value = vapply(df[[value_col]], publication_metadata_value, character(1)),
        stringsAsFactors = FALSE
      )
      invisible(NULL)
    }
    add_manual_row <- function(section, key, value) {
      rows[[length(rows) + 1L]] <<- data.frame(
        section = section,
        key = key,
        value = publication_metadata_value(value),
        stringsAsFactors = FALSE
      )
    }

    add_manual_row("data_subset", "tab4_rows_before_filters", tryCatch(nrow(tab4_source_table()), error = function(e) NA_integer_))
    add_manual_row("data_subset", "tab4_rows_after_filters", tryCatch(nrow(tab4_filtered_table()), error = function(e) NA_integer_))
    add_manual_row("data_subset", "tab4_unique_images_after_filters", tryCatch({
      d <- tab4_filtered_table()
      if ("image_name" %in% names(d)) length(unique(d$image_name)) else NA_integer_
    }, error = function(e) NA_integer_))

    if (nrow(snapshot)) {
      shared <- snapshot[grepl("^tab4_", snapshot$input_id), , drop = FALSE]
      add_rows("shared_tab4_filtering_and_subsampling", shared)

      plot_aesthetics <- snapshot[grepl("(color|colour|palette|group|facet|split|contrast|model|profile|feature|outcome|variable|metric|normaliz|pc_|loading|covariate|ancova)", snapshot$input_id, ignore.case = TRUE), , drop = FALSE]
      add_rows("plot_data_mapping_and_aesthetics", plot_aesthetics)

      if (nzchar(table_control_id %||% "")) {
        table_ids <- paste0(table_control_id, c("_filter_col", "_filter_text", "_filter_min", "_filter_max", "_sort_col", "_sort_dir"))
        table_controls <- snapshot[snapshot$input_id %in% table_ids, , drop = FALSE]
        add_rows("saved_table_filtering_and_ordering", table_controls)
      }
    }

    if (!is.null(style)) {
      style_df <- if (is.data.frame(style)) style else data.frame(key = names(style), value = unlist(style, use.names = FALSE), stringsAsFactors = FALSE)
      if (is.data.frame(style_df) && nrow(style_df) && all(c("key", "value") %in% names(style_df))) {
        add_rows("publication_style_and_overlay_aesthetics", style_df, key_col = "key", value_col = "value", key_prefix = "style_")
      }
    }

    if (!length(rows)) return(data.frame(section = character(), key = character(), value = character()))
    do.call(rbind, rows)
  }

  write_key_value_metadata <- function(path, base_metadata, context_metadata = NULL) {
    base_metadata <- if (is.data.frame(base_metadata)) base_metadata else data.frame()
    if (!nrow(base_metadata)) base_metadata <- data.frame(key = character(), value = character())
    if (!"section" %in% names(base_metadata)) base_metadata$section <- "candidate"
    base_metadata <- base_metadata[, c("section", "key", "value"), drop = FALSE]
    context_metadata <- if (is.data.frame(context_metadata) && nrow(context_metadata)) context_metadata[, c("section", "key", "value"), drop = FALSE] else data.frame(section = character(), key = character(), value = character())
    utils::write.csv(rbind(base_metadata, context_metadata), path, row.names = FALSE)
  }

  publication_metadata_category_label <- function(section) {
    section <- as.character(section %||% "")
    labels <- c(
      candidate = "1. Saved figure identity",
      figure = "1. Saved figure identity",
      data_subset = "2. Data subset used",
      shared_tab4_filtering_and_subsampling = "3. Shared Tab 4 filters and subsampling",
      plot_data_mapping_and_aesthetics = "4. Plot data mapping and visual encoding",
      publication_style_and_overlay_aesthetics = "5. Publication style saved with candidate",
      current_saved_style = "6. Current edited publication style",
      saved_input_snapshot = "7. Complete raw input snapshot",
      saved_table_filtering_and_ordering = "8. Saved table filtering and ordering"
    )
    out <- labels[section]
    out[is.na(out)] <- gsub("_", " ", section[is.na(out)])
    unname(out)
  }

  publication_metadata_setting_label <- function(key) {
    key <- as.character(key %||% "")
    original <- key
    key <- sub("^style_", "", key)
    key <- sub("^tab4_", "", key)
    key <- sub("^dimred_", "PCA: ", key)
    key <- sub("^generalization_", "Grouped plot: ", key)
    key <- sub("^ancova_", "ANCOVA: ", key)
    key <- sub("^contrast_", "Contrast: ", key)
    key <- sub("_table_(filter|sort)_", " table \\1 ", key)
    key <- gsub("_", " ", key)
    key <- gsub("\\bpc\\b", "PC", key, ignore.case = TRUE)
    key <- gsub("\\bpca\\b", "PCA", key, ignore.case = TRUE)
    key <- gsub("\\broi\\b", "ROI", key, ignore.case = TRUE)
    key <- gsub("\\bfbs\\b", "FBS", key, ignore.case = TRUE)
    key <- tools::toTitleCase(key)
    key <- gsub("Pc", "PC", key, fixed = TRUE)
    key <- gsub("Pca", "PCA", key, fixed = TRUE)
    key <- gsub("Roi", "ROI", key, fixed = TRUE)
    key <- gsub("Fbs", "FBS", key, fixed = TRUE)
    key <- gsub(" Id$", " ID", key)
    special <- c(
      figure_id = "Saved Figure Folder ID",
      plot_id = "Internal Plot ID",
      plot_label = "Plot Name Shown in the App",
      created_at = "Date and Time Saved",
      source_tab = "App Tab Where the Plot Was Saved",
      app_project_root = "Project Root Used by the App",
      tab4_rows_before_filters = "Rows Before Shared Tab 4 Filters",
      tab4_rows_after_filters = "Rows After Shared Tab 4 Filters",
      tab4_unique_images_after_filters = "Unique Images After Filters",
      tab4_include_cutoff_training = "Include Cutoff-Optimization Training Images",
      tab4_include_ilastik_training = "Include Ilastik Training Images",
      tab4_max_rows = "Maximum Rows Used After Filtering",
      tab4_random_seed = "Random Seed for Subsampling",
      tab4_filter_column = "Filter 1 Variable",
      tab4_filter_column_2 = "Filter 2 Variable",
      tab4_filter_column_3 = "Filter 3 Variable",
      tab4_filter_column_4 = "Filter 4 Variable",
      tab4_filter_column_5 = "Filter 5 Variable",
      tab4_filter_column_6 = "Filter 6 Variable",
      tab4_filter_column_7 = "Filter 7 Variable",
      tab4_filter_column_8 = "Filter 8 Variable",
      tab4_filter_numeric_range = "Filter 1 Numeric Range",
      tab4_filter_exact_mode = "Filter 1 Exact-Value Rule",
      tab4_filter_numeric_values = "Filter 1 Exact Numeric Values",
      dimred_color_by = "PCA Color Variable",
      dimred_pc_x = "PCA X-Axis Component",
      dimred_pc_y = "PCA Y-Axis Component",
      dimred_features = "Numeric Variables Included in PCA",
      dimred_loading_pcs = "PCA Loading Dimensions Shown"
    )
    hit <- special[original]
    ifelse(!is.na(hit), unname(hit), key)
  }

  publication_metadata_why <- function(section, key) {
    section <- as.character(section %||% "")
    key <- as.character(key %||% "")
    if (section %in% c("candidate", "figure")) return("Identifies exactly which app plot was saved and when.")
    if (section == "data_subset") return("Shows how many rows/images entered the plot after the active filters.")
    if (section == "shared_tab4_filtering_and_subsampling") return("Captures the global Tab 4 filters so the same data subset can be recreated.")
    if (section == "plot_data_mapping_and_aesthetics") return("Captures which variables, PCs, groups, colors, or metrics defined the plotted data.")
    if (section == "publication_style_and_overlay_aesthetics") return("Captures the initial publication styling saved with this candidate.")
    if (section == "current_saved_style") return("Captures the most recent style settings at the moment this metadata CSV was downloaded.")
    if (section == "saved_input_snapshot") return("Raw app input value retained for complete reproducibility.")
    if (grepl("filter|sort", key, ignore.case = TRUE)) return("Records filtering or ordering choices that change what appears in the table or plot.")
    "Reproducibility metadata saved by the app."
  }

  publication_metadata_repro_note <- function(section, key, value) {
    value <- publication_metadata_value(value)
    if (!nzchar(value)) return("Blank means this option was not used or left at the app default.")
    if (grepl("\\|", value)) return("Multiple values are separated by | exactly as selected in the app.")
    if (grepl("filter", key, ignore.case = TRUE)) return("Use this together with the neighboring filter rows in the same category.")
    if (grepl("^style_", key)) return("Use with source_plot.png and style_metadata.csv to recreate the edited export.")
    if (grepl("pc_", key, ignore.case = TRUE)) return("Defines which principal component axis was drawn.")
    "Use this value exactly when recreating the plot."
  }

  publication_human_readable_metadata <- function(df) {
    df <- if (is.data.frame(df)) df else data.frame(section = character(), key = character(), value = character())
    if (!nrow(df)) return(data.frame(
      Category = character(),
      Setting = character(),
      Value = character(),
      Details = character(),
      Why_it_matters = character(),
      Reproducibility_note = character(),
      Technical_keys = character(),
      stringsAsFactors = FALSE
    ))
    if (!"section" %in% names(df)) df$section <- "candidate"
    if (!"key" %in% names(df)) df$key <- ""
    if (!"value" %in% names(df)) df$value <- ""
    df <- df[, c("section", "key", "value"), drop = FALSE]

    make_row <- function(section, key, value, setting = NULL, details = "") {
      data.frame(
        Category = publication_metadata_category_label(section),
        Setting = setting %||% publication_metadata_setting_label(key),
        Value = publication_metadata_value(value),
        Details = details,
        Why_it_matters = publication_metadata_why(section, key),
        Reproducibility_note = publication_metadata_repro_note(section, key, value),
        Technical_keys = key,
        stringsAsFactors = FALSE
      )
    }

    filter_rows <- df[df$section == "shared_tab4_filtering_and_subsampling" & grepl("^tab4_filter_", df$key), , drop = FALSE]
    regular <- df[!(df$section == "shared_tab4_filtering_and_subsampling" & grepl("^tab4_filter_", df$key)), , drop = FALSE]
    out <- data.frame(
      Category = publication_metadata_category_label(df$section),
      Setting = publication_metadata_setting_label(df$key),
      Value = vapply(df$value, publication_metadata_value, character(1)),
      Details = "",
      Why_it_matters = mapply(publication_metadata_why, df$section, df$key, USE.NAMES = FALSE),
      Reproducibility_note = mapply(publication_metadata_repro_note, df$section, df$key, df$value, USE.NAMES = FALSE),
      Technical_keys = df$key,
      stringsAsFactors = FALSE
    )
    if (nrow(regular) != nrow(df)) {
      out <- data.frame(
        Category = publication_metadata_category_label(regular$section),
        Setting = publication_metadata_setting_label(regular$key),
        Value = vapply(regular$value, publication_metadata_value, character(1)),
        Details = "",
        Why_it_matters = mapply(publication_metadata_why, regular$section, regular$key, USE.NAMES = FALSE),
        Reproducibility_note = mapply(publication_metadata_repro_note, regular$section, regular$key, regular$value, USE.NAMES = FALSE),
        Technical_keys = regular$key,
        stringsAsFactors = FALSE
      )
      get_filter_value <- function(idx, stem) {
        suffix <- if (idx == 1L) "" else paste0("_", idx)
        hit <- filter_rows$value[filter_rows$key == paste0("tab4_", stem, suffix)]
        if (length(hit)) publication_metadata_value(hit[[1]]) else ""
      }
      filter_indices <- unique(c(
        1L,
        suppressWarnings(as.integer(sub("^tab4_filter_column_?", "", filter_rows$key[grepl("^tab4_filter_column", filter_rows$key)])))
      ))
      filter_indices[is.na(filter_indices)] <- 1L
      filter_indices <- sort(unique(filter_indices))
      filter_summary <- lapply(filter_indices, function(idx) {
        variable <- get_filter_value(idx, "filter_column")
        if (!nzchar(variable)) return(NULL)
        range <- get_filter_value(idx, "filter_numeric_range")
        exact_mode <- get_filter_value(idx, "filter_exact_mode")
        numeric_values <- get_filter_value(idx, "filter_numeric_values")
        categorical_values <- get_filter_value(idx, "filter_values")
        parts <- c(
          if (nzchar(range)) paste("numeric range:", range) else character(),
          if (nzchar(exact_mode) && !identical(tolower(exact_mode), "none")) paste("exact-value rule:", exact_mode) else character(),
          if (nzchar(numeric_values)) paste("numeric values:", numeric_values) else character(),
          if (nzchar(categorical_values)) paste("selected values:", categorical_values) else character()
        )
        suffix <- if (idx == 1L) "" else paste0("_", idx)
        keys <- paste(filter_rows$key[filter_rows$key %in% paste0("tab4_", c("filter_column", "filter_numeric_range", "filter_exact_mode", "filter_numeric_values", "filter_values"), suffix)], collapse = " | ")
        make_row(
          "shared_tab4_filtering_and_subsampling",
          keys,
          variable,
          setting = paste0("Filter ", idx, ": ", variable),
          details = if (length(parts)) paste(parts, collapse = "; ") else "Filter variable selected, but no range/value restriction was active."
        )
      })
      filter_summary <- do.call(rbind, Filter(Negate(is.null), filter_summary))
      if (is.data.frame(filter_summary) && nrow(filter_summary)) {
        hits <- which(out$Category == publication_metadata_category_label("shared_tab4_filtering_and_subsampling"))
        insert_at <- if (length(hits)) hits[[1]] else nrow(out) + 1L
        before <- if (insert_at > 1L) out[seq_len(insert_at - 1L), , drop = FALSE] else out[0, , drop = FALSE]
        after <- if (insert_at <= nrow(out)) out[seq(from = insert_at, to = nrow(out)), , drop = FALSE] else out[0, , drop = FALSE]
        out <- rbind(before, filter_summary, after)
      }
    }
    row.names(out) <- NULL
    out
  }

  write_publication_metadata_text <- function(candidate_dir, kind, object_id, object_label, metadata, snapshot, style = NULL, rows = NULL, columns = NULL) {
    if (!dir.exists(candidate_dir)) dir.create(candidate_dir, recursive = TRUE, showWarnings = FALSE)
    snapshot <- if (is.data.frame(snapshot)) snapshot else data.frame(input_id = character(), value = character())
    metadata <- if (is.data.frame(metadata)) metadata else data.frame(key = character(), value = character())
    meta_lines <- if (nrow(metadata) && all(c("key", "value") %in% names(metadata))) {
      sprintf("- %s: %s", metadata$key, vapply(metadata$value, publication_metadata_value, character(1)))
    } else {
      "- No structured metadata table was available."
    }
    style_lines <- character()
    if (!is.null(style)) {
      style_df <- if (is.data.frame(style)) style else data.frame(key = names(style), value = unlist(style, use.names = FALSE), stringsAsFactors = FALSE)
      if (nrow(style_df) && all(c("key", "value") %in% names(style_df))) {
        style_lines <- c("", "## Initial Publication Style Metadata", sprintf("- %s: %s", style_df$key, vapply(style_df$value, publication_metadata_value, character(1))))
      }
    }
    table_control_ids <- if (identical(kind, "table")) {
      paste0(object_id, c("_filter_col", "_filter_text", "_filter_min", "_filter_max", "_sort_col", "_sort_dir"))
    } else {
      character()
    }
    lines <- c(
      "Publication Candidate Metadata",
      "==============================",
      "",
      "This file is frozen at the moment this plot/table was saved as publication-relevant. Later app filters, color choices, grouping choices, or style edits do not change this text file.",
      "",
      "## Candidate",
      sprintf("- Type: %s", kind),
      sprintf("- ID: %s", object_id),
      sprintf("- Label: %s", object_label),
      sprintf("- Saved at: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("- Project root: %s", project_root),
      if (!is.null(rows)) sprintf("- Saved rows: %s", rows) else character(),
      if (!is.null(columns)) sprintf("- Saved columns: %s", columns) else character(),
      "",
      "## Structured Candidate Metadata",
      meta_lines,
      publication_snapshot_lines(snapshot, pattern = "^tab4_", title = "Shared Tab 4 Filters, Subsampling, and Training-Image Toggles"),
      publication_snapshot_lines(snapshot, pattern = "(color|colour|palette|group|facet|split|contrast|model|profile|feature|outcome|variable|metric|normaliz|pc_|loading|covariate|ancova)", title = "Plot/Table Variables, Color Coding, Grouping, and Model Choices"),
      publication_snapshot_lines(snapshot, ids = table_control_ids, title = "Table Filter and Ordering Applied at Save Time"),
      publication_snapshot_lines(snapshot, title = "Complete Captured App Input Snapshot"),
      style_lines,
      "",
      "## Files Saved in This Candidate Folder",
      if (identical(kind, "figure")) c("- plot_data_snapshot.rds", "- plot_recipe.rds", "- style_metadata.csv", "- original_style_metadata.csv", "- input_snapshot.csv", "- figure_metadata.csv", "- plot_recipe.Rmd", "- publication_metadata.txt", "- source_plot.png and recorded_plot.rds only for PNG fallback candidates") else c("- table.csv", "- table_metadata.csv", "- input_snapshot.csv", "- publication_metadata.txt"),
      "",
      "## Interpretation Note",
      "Use this text file together with input_snapshot.csv and the saved CSV/RDS/PNG files to recreate the exact data filtering and visual encoding context for this publication candidate."
    )
    writeLines(lines, file.path(candidate_dir, "publication_metadata.txt"), useBytes = TRUE)
    invisible(file.path(candidate_dir, "publication_metadata.txt"))
  }

  publication_candidate_dirs <- function() {
    publication_tick()
    if (!dir.exists(publication_figure_dir)) return(character())
    dirs <- list.dirs(publication_figure_dir, recursive = FALSE, full.names = TRUE)
    dirs[file.exists(file.path(dirs, "figure_metadata.csv"))]
  }

  publication_candidates_table <- function() {
    dirs <- publication_candidate_dirs()
    if (!length(dirs)) {
      return(data.frame(Message = "No publication figure candidates saved yet. Open a meaningful Tab 4 plot and click a Save as Publication Candidate button."))
    }
    rows <- lapply(dirs, function(d) {
      meta <- tryCatch(utils::read.csv(file.path(d, "figure_metadata.csv"), stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      style <- tryCatch(utils::read.csv(file.path(d, "style_metadata.csv"), stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      getv <- function(df, key, default = "") {
        if (!nrow(df) || !all(c("key", "value") %in% names(df))) return(default)
        hit <- df$value[df$key == key]
        if (length(hit)) hit[[1]] else default
      }
      data.frame(
        figure_id = basename(d),
        plot_id = getv(meta, "plot_id"),
        plot_label = getv(meta, "plot_label"),
        edit_mode = if (publication_recipe_supported(d)) "recipe-backed" else "PNG fallback",
        title = getv(style, "title", getv(meta, "plot_label")),
        created_at = getv(meta, "created_at"),
        folder = normalizePath(d, winslash = "/", mustWork = FALSE),
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows)
    out[order(out$created_at, decreasing = TRUE), , drop = FALSE]
  }

  publication_recipe_supported_plot_id <- function(plot_id) {
    plot_id %in% c("dimred_pca_plot", "dimred_scree_plot", "dimred_separation_plot", "contrast_variable_plot", "generalization_grouped_plot", "covariate_correlation_heatmap")
  }

  publication_payload_recipe_ready <- function(plot_id, payload) {
    if (!publication_recipe_supported_plot_id(plot_id) || !is.list(payload)) return(FALSE)
    if (plot_id %in% c("dimred_pca_plot", "dimred_scree_plot", "dimred_separation_plot")) {
      return(isTRUE(payload$ok) && is.data.frame(payload$scores) && nrow(payload$scores) > 0)
    }
    if (identical(plot_id, "contrast_variable_plot")) {
      r <- payload$ranking %||% data.frame()
      return(is.data.frame(r) && nrow(r) > 0)
    }
    if (identical(plot_id, "generalization_grouped_plot")) {
      d <- payload$filtered_table %||% data.frame()
      return(is.data.frame(d) && nrow(d) > 0)
    }
    if (identical(plot_id, "covariate_correlation_heatmap")) {
      res <- payload$result %||% list()
      return(isTRUE(res$ok) && is.matrix(res$matrix) && nrow(res$matrix) > 0)
    }
    FALSE
  }

  save_publication_candidate <- function(plot_id) {
    payload <- publication_plot_data_snapshot(plot_id)
    recipe_ready <- publication_payload_recipe_ready(plot_id, payload)
    plot_obj <- recorded_plots[[plot_id]]
    if (!recipe_ready && is.null(plot_obj)) {
      showNotification("This plot type still needs the plot to be opened/refreshed before saving because no recipe-backed data snapshot is available yet.", type = "warning", duration = 8)
      return(invisible(FALSE))
    }
    dir.create(publication_figure_dir, recursive = TRUE, showWarnings = FALSE)
    fig_id <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", sanitize_profile_name(publication_plot_label(plot_id)))
    fig_dir <- file.path(publication_figure_dir, fig_id)
    dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)
    if (!recipe_ready) {
      saveRDS(plot_obj, file.path(fig_dir, "recorded_plot.rds"))
      grDevices::png(file.path(fig_dir, "source_plot.png"), width = 3000, height = 2200, res = 300)
      on.exit(try(grDevices::dev.off(), silent = TRUE), add = TRUE)
      grDevices::replayPlot(plot_obj)
      grDevices::dev.off()
    }
    meta <- data.frame(
      key = c("figure_id", "plot_id", "plot_label", "created_at", "source_tab", "app_project_root", "saved_as"),
      value = c(fig_id, plot_id, publication_plot_label(plot_id), format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "4. Visualization and Interpretation", project_root, if (recipe_ready) "recipe_data_style_first" else "png_recorded_plot_fallback"),
      stringsAsFactors = FALSE
    )
    defaults <- publication_inferred_plot_text(plot_id)
    style <- data.frame(key = names(defaults), value = unlist(defaults, use.names = FALSE), stringsAsFactors = FALSE)
    utils::write.csv(style, file.path(fig_dir, "style_metadata.csv"), row.names = FALSE)
    write_publication_original_style(fig_dir, defaults)
    snapshot <- publication_snapshot_inputs()
    context <- publication_context_metadata(snapshot, style = style, object_id = plot_id)
    write_key_value_metadata(file.path(fig_dir, "figure_metadata.csv"), meta, context)
    utils::write.csv(snapshot, file.path(fig_dir, "input_snapshot.csv"), row.names = FALSE)
    write_publication_reproducibility_files(fig_dir, plot_id, publication_plot_label(plot_id), payload = payload, recipe_ready = recipe_ready)
    write_publication_metadata_text(
      candidate_dir = fig_dir,
      kind = "figure",
      object_id = plot_id,
      object_label = publication_plot_label(plot_id),
      metadata = meta,
      snapshot = snapshot,
      style = style
    )
    publication_tick(publication_tick() + 1)
    try(updateSelectInput(session, "publication_figure_choice", choices = publication_candidate_choices(), selected = fig_id), silent = TRUE)
    showNotification(sprintf("Saved publication figure candidate as %s: %s", if (recipe_ready) "recipe-backed data/style" else "PNG fallback", publication_plot_label(plot_id)), type = "message")
    invisible(TRUE)
  }

  publication_candidate_choices <- reactive({
    tab <- publication_candidates_table()
    if (!nrow(tab) || "Message" %in% names(tab)) return(c("No saved publication figures" = ""))
    stats::setNames(tab$figure_id, paste(tab$created_at, tab$plot_label, sep = " - "))
  })

  publication_candidate_delete_choices <- reactive({
    tab <- publication_candidates_table()
    if (!nrow(tab) || "Message" %in% names(tab)) return(character())
    labels <- paste(tab$created_at, tab$plot_label, tab$figure_id, sep = " - ")
    stats::setNames(tab$figure_id, labels)
  })

  publication_candidate_dir_for_id <- function(fig_id) {
    fig_id <- basename(as.character(fig_id %||% ""))
    if (!nzchar(fig_id)) return("")
    path <- file.path(publication_figure_dir, fig_id)
    if (!dir.exists(path)) return("")
    root <- normalizePath(publication_figure_dir, winslash = "/", mustWork = FALSE)
    candidate <- normalizePath(path, winslash = "/", mustWork = TRUE)
    if (!identical(dirname(candidate), root)) return("")
    candidate
  }

  publication_table_dirs <- function() {
    publication_tick()
    if (!dir.exists(publication_table_dir)) return(character())
    dirs <- list.dirs(publication_table_dir, recursive = FALSE, full.names = TRUE)
    dirs[file.exists(file.path(dirs, "table_metadata.csv"))]
  }

  publication_tables_table <- function() {
    dirs <- publication_table_dirs()
    if (!length(dirs)) {
      return(data.frame(Message = "No publication table candidates saved yet. Open a useful table and click Save Table as Publication Candidate."))
    }
    rows <- lapply(dirs, function(d) {
      meta <- tryCatch(utils::read.csv(file.path(d, "table_metadata.csv"), stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      getv <- function(key, default = "") {
        if (!nrow(meta) || !all(c("key", "value") %in% names(meta))) return(default)
        hit <- meta$value[meta$key == key]
        if (length(hit)) hit[[1]] else default
      }
      table_path <- file.path(d, "table.csv")
      data.frame(
        table_id = basename(d),
        source_table_id = getv("source_table_id"),
        table_label = getv("table_label"),
        rows = getv("rows"),
        columns = getv("columns"),
        created_at = getv("created_at"),
        folder = normalizePath(d, winslash = "/", mustWork = FALSE),
        csv = normalizePath(table_path, winslash = "/", mustWork = FALSE),
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows)
    out[order(out$created_at, decreasing = TRUE), , drop = FALSE]
  }

  publication_enrich_table_for_export <- function(df, table_id, table_label) {
    if (!is.data.frame(df) || !nrow(df)) return(df)
    out <- df
    label_text <- publication_metadata_value(table_label %||% table_id, max_chars = 2000)
    subset_text <- ""
    if (grepl("^(generalization_|dimred_|contrast_|covariate_|ancova_|tab4_)", table_id %||% "", ignore.case = TRUE)) {
      subset_text <- publication_metadata_value(paste(tab4_active_filter_summary_lines(), collapse = " || "), max_chars = 4000)
    }
    if (identical(table_id, "generalization_group_stats_table")) {
      stat_text <- publication_metadata_value(generalization_group_stats_table_title(), max_chars = 2000)
      out <- cbind(
        Statistical_test_summary = rep(stat_text, nrow(out)),
        Applied_subset_rules = rep(subset_text, nrow(out)),
        Saved_table_title = rep(label_text, nrow(out)),
        out,
        stringsAsFactors = FALSE
      )
    } else if (nzchar(subset_text)) {
      out <- cbind(
        Applied_subset_rules = rep(subset_text, nrow(out)),
        Saved_table_title = rep(label_text, nrow(out)),
        out,
        stringsAsFactors = FALSE
      )
    } else {
      out <- cbind(
        Saved_table_title = rep(label_text, nrow(out)),
        out,
        stringsAsFactors = FALSE
      )
    }
    out
  }

  save_publication_table_candidate <- function(table_id, table_label, df) {
    if (!is.data.frame(df) || !nrow(df)) {
      showNotification("This table has no rows to save as a publication candidate.", type = "warning", duration = 8)
      return(invisible(FALSE))
    }
    dir.create(publication_table_dir, recursive = TRUE, showWarnings = FALSE)
    safe_label <- sanitize_profile_name(table_label %||% table_id)
    candidate_id <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", safe_label)
    candidate_dir <- file.path(publication_table_dir, candidate_id)
    dir.create(candidate_dir, recursive = TRUE, showWarnings = FALSE)
    export_df <- publication_enrich_table_for_export(df, table_id, table_label)
    utils::write.csv(export_df, file.path(candidate_dir, "table.csv"), row.names = FALSE, na = "")
    meta <- data.frame(
      key = c("table_id", "source_table_id", "table_label", "created_at", "rows", "columns", "app_project_root"),
      value = c(candidate_id, table_id, table_label, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), nrow(export_df), ncol(export_df), project_root),
      stringsAsFactors = FALSE
    )
    if (identical(table_id, "generalization_group_stats_table")) {
      meta <- rbind(
        meta,
        data.frame(
          key = c("statistical_test_summary", "applied_subset_rules"),
          value = c(
            publication_metadata_value(generalization_group_stats_table_title(), max_chars = 2000),
            publication_metadata_value(paste(tab4_active_filter_summary_lines(), collapse = " || "), max_chars = 4000)
          ),
          stringsAsFactors = FALSE
        )
      )
    } else if (grepl("^(generalization_|dimred_|contrast_|covariate_|ancova_|tab4_)", table_id %||% "", ignore.case = TRUE)) {
      meta <- rbind(
        meta,
        data.frame(
          key = "applied_subset_rules",
          value = publication_metadata_value(paste(tab4_active_filter_summary_lines(), collapse = " || "), max_chars = 4000),
          stringsAsFactors = FALSE
        )
      )
    }
    snapshot <- publication_snapshot_inputs()
    context <- publication_context_metadata(snapshot, table_control_id = table_id)
    write_key_value_metadata(file.path(candidate_dir, "table_metadata.csv"), meta, context)
    utils::write.csv(snapshot, file.path(candidate_dir, "input_snapshot.csv"), row.names = FALSE)
    write_publication_metadata_text(
      candidate_dir = candidate_dir,
      kind = "table",
      object_id = table_id,
      object_label = table_label,
      metadata = meta,
      snapshot = snapshot,
      rows = nrow(export_df),
      columns = ncol(export_df)
    )
    publication_tick(publication_tick() + 1)
    showNotification(sprintf("Saved publication table candidate: %s", table_label), type = "message")
    invisible(TRUE)
  }

  publication_table_choices <- reactive({
    tab <- publication_tables_table()
    if (!nrow(tab) || "Message" %in% names(tab)) return(c("No saved publication tables" = ""))
    stats::setNames(tab$table_id, paste(tab$created_at, tab$table_label, sep = " - "))
  })

  selected_publication_table_dir <- reactive({
    table_id <- input$publication_table_choice %||% ""
    if (!nzchar(table_id)) return("")
    path <- file.path(publication_table_dir, table_id)
    if (dir.exists(path)) path else ""
  })

  selected_publication_dir <- reactive({
    fig_id <- input$publication_figure_choice %||% ""
    if (!nzchar(fig_id)) return("")
    path <- file.path(publication_figure_dir, fig_id)
    if (dir.exists(path)) path else ""
  })

  publication_candidate_plot_id <- function(fig_dir) {
    meta <- tryCatch(utils::read.csv(file.path(fig_dir, "figure_metadata.csv"), stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    if (!nrow(meta) || !all(c("key", "value") %in% names(meta))) return("")
    meta$value[meta$key == "plot_id"][[1]] %||% ""
  }

  migrate_publication_style_for_ticks <- function(style, fig_dir) {
    plot_id <- publication_candidate_plot_id(fig_dir)
    if (!nzchar(plot_id)) return(style)
    x_title <- trimws(style$x_label %||% "")
    y_title <- trimws(style$y_label %||% "")
    if (identical(plot_id, "generalization_grouped_plot") && nzchar(x_title) && !nzchar(style$x_tick_labels %||% "")) {
      style$x_tick_labels <- x_title
      style$x_label <- ""
    }
    if (identical(plot_id, "contrast_variable_plot") && nzchar(y_title) && grepl("feature|variable", y_title, ignore.case = TRUE) && !nzchar(style$y_tick_labels %||% "")) {
      style$y_tick_labels <- y_title
      style$y_label <- ""
    }
    if (plot_id %in% c("cutoff_profile_effectiveness_plot", "cutoff_group_dot_plot", "cutoff_review_retention_plot") && nzchar(x_title) && !nzchar(style$x_tick_labels %||% "")) {
      style$x_tick_labels <- x_title
      style$x_label <- ""
    }
    style
  }

  read_publication_style <- function(fig_dir) {
    defaults <- publication_style_defaults()
    style_path <- file.path(fig_dir, "style_metadata.csv")
    if (file.exists(style_path)) {
      df <- tryCatch(utils::read.csv(style_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      if (nrow(df) && all(c("key", "value") %in% names(df))) {
        for (k in intersect(names(defaults), df$key)) defaults[[k]] <- df$value[df$key == k][[1]]
      }
    }
    migrate_publication_style_for_ticks(defaults, fig_dir)
  }

  write_publication_style <- function(fig_dir, style) {
    df <- data.frame(key = names(style), value = unlist(style, use.names = FALSE), stringsAsFactors = FALSE)
    utils::write.csv(df, file.path(fig_dir, "style_metadata.csv"), row.names = FALSE)
  }

  read_publication_original_style <- function(fig_dir) {
    original_path <- file.path(fig_dir, "original_style_metadata.csv")
    if (file.exists(original_path)) {
      df <- tryCatch(utils::read.csv(original_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      defaults <- publication_style_defaults()
      if (nrow(df) && all(c("key", "value") %in% names(df))) {
        for (k in intersect(names(defaults), df$key)) defaults[[k]] <- df$value[df$key == k][[1]]
        return(defaults)
      }
    }
    read_publication_style(fig_dir)
  }

  write_publication_original_style <- function(fig_dir, style) {
    df <- data.frame(key = names(style), value = unlist(style, use.names = FALSE), stringsAsFactors = FALSE)
    utils::write.csv(df, file.path(fig_dir, "original_style_metadata.csv"), row.names = FALSE)
  }

  publication_style_changed <- function(style, original, key) {
    current_value <- trimws(as.character(style[[key]] %||% ""))
    original_value <- trimws(as.character(original[[key]] %||% ""))
    nzchar(current_value) && !identical(current_value, original_value)
  }

  publication_element_changed <- function(style, original, text_key, style_keys = character()) {
    text_value <- trimws(as.character(style[[text_key]] %||% ""))
    if (!nzchar(text_value)) return(FALSE)
    if (publication_style_changed(style, original, text_key)) return(TRUE)
    any(vapply(style_keys, function(key) !identical(as.character(style[[key]] %||% ""), as.character(original[[key]] %||% "")), logical(1)))
  }

  style_num <- function(style, key, default) {
    value <- suppressWarnings(as.numeric(style[[key]] %||% default))
    if (length(value) != 1 || !is.finite(value)) default else value
  }

  style_bool <- function(style, key, default = FALSE) {
    value <- tolower(as.character(style[[key]] %||% if (default) "true" else "false"))
    value %in% c("true", "1", "yes", "y")
  }

  style_font <- function(style, italic_key, bold_key) {
    italic <- style_bool(style, italic_key, FALSE)
    bold <- style_bool(style, bold_key, FALSE)
    if (italic && bold) 4 else if (italic) 3 else if (bold) 2 else 1
  }

  split_tick_labels <- function(x) {
    x <- trimws(as.character(x %||% ""))
    if (!nzchar(x)) return(character())
    vals <- unlist(strsplit(x, "\\s*[|,;]\\s*"), use.names = FALSE)
    vals[nzchar(vals)]
  }

  join_tick_labels <- function(x, max_n = 20) {
    x <- as.character(x %||% character())
    x <- x[nzchar(x)]
    if (!length(x)) return("")
    paste(head(x, max_n), collapse = " | ")
  }

  crop_publication_image <- function(img, top = 0, bottom = 0, left = 0, right = 0) {
    if (is.null(img) || length(dim(img)) < 2) return(img)
    nr <- dim(img)[1]
    nc <- dim(img)[2]
    top <- max(0, min(0.45, top))
    bottom <- max(0, min(0.45, bottom))
    left <- max(0, min(0.45, left))
    right <- max(0, min(0.45, right))
    r1 <- min(nr, max(1, floor(nr * top) + 1))
    r2 <- max(r1, min(nr, ceiling(nr * (1 - bottom))))
    c1 <- min(nc, max(1, floor(nc * left) + 1))
    c2 <- max(c1, min(nc, ceiling(nc * (1 - right))))
    img[r1:r2, c1:c2, , drop = FALSE]
  }

  publication_inferred_plot_text <- function(plot_id) {
    style <- publication_style_defaults()
    style$title <- publication_plot_label(plot_id)
    if (identical(plot_id, "dimred_pca_plot")) {
      res <- tryCatch(dimred_result(), error = function(e) NULL)
      axes <- if (!is.null(res) && isTRUE(res$ok)) choose_pca_axes(res$scores, input$dimred_pc_x %||% "PC1", input$dimred_pc_y %||% "PC2") else c("PC1", "PC2")
      style$title <- paste("PCA colored by", plot_display_label(input$dimred_color_by %||% "selected feature", width = 50, multiline = FALSE))
      style$x_label <- axes[[1]]
      style$y_label <- axes[[2]]
      if (!is.null(res) && isTRUE(res$ok) && any(is.finite(res$scores$.color_numeric))) {
        rng <- range(res$scores$.color_numeric, na.rm = TRUE)
        style$legend_text <- paste(plot_display_label(input$dimred_color_by %||% "Color scale", width = 40, multiline = FALSE), sprintf("low %.3g", rng[[1]]), sprintf("high %.3g", rng[[2]]), sep = " | ")
        style$legend_symbol_type <- "dots"
        style$legend_colors <- paste(grDevices::hcl.colors(2, "Viridis"), collapse = " | ")
      }
    } else if (identical(plot_id, "dimred_scree_plot")) {
      style$title <- "PCA Scree Plot"
      style$x_label <- "Principal component"
      style$y_label <- "Variance explained (%)"
    } else if (identical(plot_id, "dimred_separation_plot")) {
      style$title <- paste("PCA separation by", plot_display_label(input$dimred_separation_by %||% "selected feature", width = 50, multiline = FALSE))
      style$x_label <- input$dimred_pc_x %||% "PC1"
      style$y_label <- input$dimred_pc_y %||% "PC2"
    } else if (identical(plot_id, "contrast_variable_plot")) {
      style$title <- "Variables Best Separating the Selected Contrast"
      style$x_label <- "|Cohen's d|"
      style$y_label <- ""
      r <- tryCatch(contrast_variable_ranking(), error = function(e) data.frame())
      if (nrow(r) && "Feature" %in% names(r)) {
        style$y_tick_labels <- join_tick_labels(plot_display_labels(rev(head(r$Feature, 12)), width = 32), max_n = 12)
      }
    } else if (identical(plot_id, "generalization_grouped_plot")) {
      var <- input$generalization_plot_variable %||% "Selected variable"
      normalize_by <- input$generalization_plot_normalize_by %||% ""
      group <- input$generalization_plot_group %||% "Group"
      style$title <- if (nzchar(normalize_by)) paste(plot_display_label(var, width = 42, multiline = FALSE), "normalized by", plot_display_label(normalize_by, width = 42, multiline = FALSE)) else plot_display_label(var, width = 60, multiline = FALSE)
      style$x_label <- ""
      style$y_label <- if (nzchar(normalize_by)) paste(plot_display_label(var, width = 28, multiline = FALSE), "/", plot_display_label(normalize_by, width = 28, multiline = FALSE)) else plot_display_label(var, width = 36)
      df <- tryCatch(tab4_filtered_table(), error = function(e) data.frame())
      group2 <- input$generalization_plot_group2 %||% ""
      if (nrow(df) && group %in% names(df)) {
        grouping_cols <- c(group, if (nzchar(group2) && group2 %in% names(df)) group2 else character())
        groups <- if (length(grouping_cols) > 1) ordered_interaction_factor(df, grouping_cols) else ordered_factor_for_plot(df[[group]])
        style$x_tick_labels <- join_tick_labels(plot_display_labels(gsub(" \\| ", " | ", levels(groups)), width = 24), max_n = 30)
      }
    } else if (identical(plot_id, "generalization_facet_helper_plot")) {
      split_by <- input$generalization_plot_facet_by %||% "selected secondary variable"
      style$title <- paste("Facet split preview:", split_by)
      style$x_label <- plot_display_label(split_by, width = 36)
      style$y_label <- "Rows"
    } else if (identical(plot_id, "covariate_correlation_heatmap")) {
      style$title <- "Covariate Correlation Heatmap"
      style$x_label <- "Covariates/features"
      style$y_label <- "Covariates/features"
    } else if (identical(plot_id, "cutoff_profile_effectiveness_plot")) {
      style$title <- "Overall Effectiveness Comparison"
      style$x_label <- ""
      style$x_tick_labels <- "Cutoff profiles"
      style$y_label <- "Effectiveness metric"
    } else if (identical(plot_id, "cutoff_group_dot_plot")) {
      style$title <- "Four-Group Neurite Implications"
      style$x_label <- ""
      style$x_tick_labels <- "Low N Low C | Low N High C | High N Low C | High N High C"
      style$y_label <- input$cutoff_group_metric %||% "Selected neurite metric"
    } else if (identical(plot_id, "cutoff_review_score_plot")) {
      style$title <- "Optimization Score Distribution"
      style$x_label <- "Score"
      style$y_label <- "Frequency"
    } else if (identical(plot_id, "cutoff_review_retention_plot")) {
      style$title <- "ROI Retention and Label Balance"
      style$x_label <- ""
      style$x_tick_labels <- "Metric"
      style$y_label <- "Value"
    }
    style
  }

  publication_plot_data_snapshot <- function(plot_id) {
    tryCatch({
      if (identical(plot_id, "dimred_pca_plot") || identical(plot_id, "dimred_scree_plot") || identical(plot_id, "dimred_separation_plot")) {
        res <- dimred_result()
        return(list(kind = "pca", ok = isTRUE(res$ok), scores = res$scores %||% data.frame(), features = res$features %||% character(), variance_explained = res$var_exp %||% numeric()))
      }
      if (identical(plot_id, "contrast_variable_plot")) return(list(kind = "contrast", ranking = contrast_variable_ranking()))
      if (identical(plot_id, "generalization_grouped_plot") || identical(plot_id, "generalization_facet_helper_plot")) {
        return(list(kind = "grouped_plot", filtered_table = tab4_filtered_table(), facet_split = tryCatch(build_generalization_facet_split(tab4_filtered_table()), error = function(e) list(ok = FALSE, message = conditionMessage(e)))))
      }
      if (identical(plot_id, "covariate_correlation_heatmap")) return(list(kind = "covariate_correlation", result = covariate_correlation_result()))
      if (grepl("^cutoff_", plot_id)) return(list(kind = "cutoff_review", cutoff_review = cutoff_review_data()))
      list(kind = "recorded_plot_only", message = "No structured data snapshot is available for this plot type yet.")
    }, error = function(e) {
      list(kind = "snapshot_error", message = conditionMessage(e))
    })
  }

  write_publication_reproducibility_files <- function(fig_dir, plot_id, plot_label, payload = NULL, recipe_ready = NULL) {
    payload <- payload %||% publication_plot_data_snapshot(plot_id)
    recipe_ready <- isTRUE(recipe_ready %||% publication_payload_recipe_ready(plot_id, payload))
    saveRDS(payload, file.path(fig_dir, "plot_data_snapshot.rds"))
    recipe <- list(
      plot_id = plot_id,
      plot_label = plot_label,
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      source_png = if (recipe_ready) "" else "source_plot.png",
      recorded_plot = if (recipe_ready) "" else "recorded_plot.rds",
      data_snapshot = "plot_data_snapshot.rds",
      input_snapshot = "input_snapshot.csv",
      filter_and_control_snapshot = "input_snapshot.csv",
      style_metadata = "style_metadata.csv",
      original_style_metadata = "original_style_metadata.csv",
      renderer = if (recipe_ready) "recipe_data_style_first" else "png_recorded_plot_fallback",
      recipe_supported = publication_recipe_supported_plot_id(plot_id),
      recipe_ready = recipe_ready,
      packages = c("base R graphics", "grDevices", "graphics", "stats", "shiny")
    )
    saveRDS(recipe, file.path(fig_dir, "plot_recipe.rds"))
    recipe_lines <- c(
      "---",
      sprintf("title: \"Publication candidate: %s\"", gsub("\"", "'", plot_label)),
      "output: html_document",
      "---",
      "",
      "```{r setup}",
      "knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)",
      "library(grDevices)",
      "library(graphics)",
      "library(stats)",
      "fig_dir <- dirname(knitr::current_input())",
      "recipe <- readRDS(file.path(fig_dir, 'plot_recipe.rds'))",
      "plot_data <- readRDS(file.path(fig_dir, 'plot_data_snapshot.rds'))",
      "style <- read.csv(file.path(fig_dir, 'style_metadata.csv'), stringsAsFactors = FALSE, check.names = FALSE)",
      "original_style <- read.csv(file.path(fig_dir, 'original_style_metadata.csv'), stringsAsFactors = FALSE, check.names = FALSE)",
      "input_snapshot <- read.csv(file.path(fig_dir, 'input_snapshot.csv'), stringsAsFactors = FALSE, check.names = FALSE)",
      "```",
      "",
      "## Saved Source Figure / Recipe Preview",
      "",
      if (recipe_ready) {
      "This candidate was saved as data + recipe + style first. The Shiny publication editor regenerates the plot from plot_data_snapshot.rds and plot_recipe.rds; PNG files are generated only for preview/export."
      } else {
        "This candidate uses the legacy PNG fallback. It stores the source PNG and recorded base-R plot because a recipe-backed data snapshot was not available for this plot type."
      },
      "",
      "The shared tab-4 filters, selected model/profile, grouping variables, color mapping, and plot-control inputs used when this candidate was saved are stored in input_snapshot.csv. In the Shiny publication editor, use the restore button beside the candidate selector to reload those controls after restarting the app.",
      "",
      if (recipe_ready) {
        c(
          "```{r data-first-note}",
          "cat('Recipe-backed candidate: render from plot_data_snapshot.rds + plot_recipe.rds + style_metadata.csv in the Shiny editor.')",
          "```"
        )
      } else {
        c(
          "```{r source-figure, out.width='100%'}",
          "knitr::include_graphics(file.path(fig_dir, 'source_plot.png'))",
          "```"
        )
      },
      "",
      "## Style Metadata",
      "",
      "```{r style-metadata}",
      "style",
      "```",
      "",
      "## Data Snapshot Structure",
      "",
      "```{r data-snapshot}",
      "str(plot_data, max.level = 2)",
      "```",
      "",
      "## Input Snapshot",
      "",
      "```{r input-snapshot}",
      "input_snapshot",
      "```"
    )
    writeLines(recipe_lines, file.path(fig_dir, "plot_recipe.Rmd"), useBytes = TRUE)
    invisible(TRUE)
  }

  publication_recipe_supported <- function(fig_dir) {
    recipe_path <- file.path(fig_dir, "plot_recipe.rds")
    data_path <- file.path(fig_dir, "plot_data_snapshot.rds")
    if (!file.exists(recipe_path) || !file.exists(data_path)) return(FALSE)
    recipe <- tryCatch(readRDS(recipe_path), error = function(e) NULL)
    plot_id <- recipe$plot_id %||% publication_candidate_plot_id(fig_dir)
    publication_recipe_supported_plot_id(plot_id) && !identical(recipe$recipe_ready, FALSE)
  }

  snapshot_value <- function(snapshot, input_id, default = "") {
    if (!is.data.frame(snapshot) || !all(c("input_id", "value") %in% names(snapshot))) return(default)
    hit <- snapshot$value[snapshot$input_id == input_id]
    if (length(hit) && nzchar(as.character(hit[[1]]))) as.character(hit[[1]]) else default
  }

  recipe_axis_label <- function(style, key, default) {
    val <- trimws(style[[key]] %||% "")
    if (nzchar(val)) val else default
  }

  recipe_tick_labels <- function(style, key, defaults) {
    custom <- split_tick_labels(style[[key]] %||% "")
    if (length(custom) == length(defaults)) custom else defaults
  }

  build_generalization_facet_split_from_snapshot <- function(df, snapshot) {
    facet_by <- snapshot_value(snapshot, "generalization_plot_facet_by", "")
    if (!nrow(df) || !nzchar(facet_by) || !facet_by %in% names(df)) {
      return(list(ok = FALSE, message = "No faceting variable selected.", facet = factor(rep("All rows", nrow(df))), cutoffs = numeric(), counts = data.frame()))
    }
    facet_mode <- snapshot_value(snapshot, "generalization_plot_facet_mode", "quantile")
    if (vector_is_numeric_like(df[[facet_by]]) && identical(facet_mode, "quantile")) {
      vals <- suppressWarnings(as.numeric(df[[facet_by]]))
      probs_txt <- snapshot_value(snapshot, "generalization_plot_quantiles", "0.25,0.5,0.75")
      probs <- suppressWarnings(as.numeric(strsplit(probs_txt, "[,;|]")[[1]]))
      probs <- probs[is.finite(probs) & probs > 0 & probs < 1]
      if (!length(probs)) probs <- c(0.25, 0.5, 0.75)
      cutoffs <- unique(stats::quantile(vals, probs = probs, na.rm = TRUE, names = FALSE, type = 7))
      cutoffs <- cutoffs[is.finite(cutoffs)]
      if (!length(cutoffs)) {
        return(list(ok = FALSE, message = "Selected numeric faceting variable does not have enough finite variation for quantile bins.", facet = factor(rep("All rows", nrow(df))), cutoffs = cutoffs, counts = data.frame()))
      }
      bins <- cut(vals, breaks = c(-Inf, cutoffs, Inf), include.lowest = TRUE, dig.lab = 8)
      counts <- as.data.frame(table(Facet = bins, useNA = "ifany"), stringsAsFactors = FALSE)
      names(counts) <- c("Facet_bin", "Rows")
      return(list(ok = TRUE, message = "", facet = bins, cutoffs = cutoffs, counts = counts, variable = facet_by, numeric = TRUE, values = vals))
    }
    raw_values <- as.character(df[[facet_by]])
    categorical_mode <- snapshot_value(snapshot, "generalization_plot_categorical_facet_mode", "all")
    selected_value <- snapshot_value(snapshot, "generalization_plot_facet_value", "")
    if (identical(categorical_mode, "selected_only")) {
      selected_label <- if (nzchar(selected_value)) selected_value else "Selected value"
      facet <- factor(ifelse(raw_values == selected_value, selected_label, NA_character_), levels = selected_label)
      counts <- data.frame(
        Facet_bin = c(selected_label, "Excluded: other values"),
        Rows = c(sum(raw_values == selected_value, na.rm = TRUE), sum(raw_values != selected_value, na.rm = TRUE)),
        stringsAsFactors = FALSE
      )
      return(list(ok = any(!is.na(facet)), message = "", facet = facet, cutoffs = numeric(), counts = counts, variable = facet_by, numeric = FALSE, values = raw_values))
    }
    if (identical(categorical_mode, "selected_vs_rest")) {
      selected_label <- if (nzchar(selected_value)) selected_value else "Selected value"
      other_label <- "All other values"
      facet <- factor(ifelse(raw_values == selected_value, selected_label, other_label), levels = c(selected_label, other_label))
      counts <- as.data.frame(table(Facet = facet, useNA = "ifany"), stringsAsFactors = FALSE)
      names(counts) <- c("Facet_bin", "Rows")
      return(list(ok = TRUE, message = "", facet = facet, cutoffs = numeric(), counts = counts, variable = facet_by, numeric = FALSE, values = raw_values))
    }
    facet <- ordered_factor_for_plot(raw_values)
    counts <- as.data.frame(table(Facet = facet, useNA = "ifany"), stringsAsFactors = FALSE)
    names(counts) <- c("Facet_bin", "Rows")
    list(ok = TRUE, message = "", facet = facet, cutoffs = numeric(), counts = counts, variable = facet_by, numeric = FALSE, values = raw_values)
  }

  render_recipe_dimred_plot <- function(plot_id, payload, style, snapshot) {
    if (!is.list(payload) || !isTRUE(payload$ok) || !is.data.frame(payload$scores) || !nrow(payload$scores)) return(FALSE)
    scores <- payload$scores
    var_exp <- payload$variance_explained %||% numeric()
    pc_x <- snapshot_value(snapshot, "dimred_pc_x", "PC1")
    pc_y <- snapshot_value(snapshot, "dimred_pc_y", "PC2")
    axes <- choose_pca_axes(scores, pc_x, pc_y)
    pc_x <- axes[[1]]
    pc_y <- axes[[2]]
    pc_x_i <- suppressWarnings(as.integer(sub("^PC", "", pc_x)))
    pc_y_i <- suppressWarnings(as.integer(sub("^PC", "", pc_y)))
    if (identical(plot_id, "dimred_scree_plot")) {
      var_pct <- 100 * var_exp
      pcs <- paste0("PC", seq_along(var_pct))
      op <- par(mar = c(4.5, 4.8, 2.8, 1), family = style$font_family %||% "Arial")
      on.exit(par(op), add = TRUE)
      centers <- barplot(var_pct, names.arg = pcs, col = "#8fb8de", border = "white",
                         ylab = recipe_axis_label(style, "y_label", "Variance explained (%)"),
                         xlab = recipe_axis_label(style, "x_label", "Principal component"),
                         main = style$title %||% "PCA Scree Plot",
                         cex.axis = style_num(style, "y_tick_size", 9) / 9,
                         cex.names = style_num(style, "x_tick_size", 9) / 9,
                         cex.lab = style_num(style, "x_axis_size", 12) / 12,
                         cex.main = style_num(style, "title_size", 18) / 18)
      lines(centers, cumsum(var_pct), type = "b", pch = 19, col = "#173f35", lwd = 2)
      legend("topright", legend = c("Individual PC", "Cumulative"), fill = c("#8fb8de", NA), border = c("white", NA), lty = c(NA, 1), pch = c(NA, 19), col = c("#8fb8de", "#173f35"), bty = "n", cex = style_num(style, "legend_size", 9) / 11)
      return(TRUE)
    }
    if (identical(plot_id, "dimred_pca_plot")) {
      numeric_color <- any(is.finite(scores$.color_numeric))
      if (numeric_color) {
        point_cols <- continuous_palette(scores$.color_numeric)
      } else {
        groups <- droplevels(scores$.color)
        palette_cols <- grDevices::hcl.colors(length(levels(groups)), "Dark 3")
        point_cols <- palette_cols[as.integer(groups)]
      }
      op <- par(mar = c(4.8, 4.8, 3.0, if (numeric_color) 5.8 else 1.5), family = style$font_family %||% "Arial")
      on.exit(par(op), add = TRUE)
      color_title <- plot_display_label(snapshot_value(snapshot, "dimred_color_by", "selected feature"), width = 50, multiline = FALSE)
      xlab <- recipe_axis_label(style, "x_label", sprintf("%s (%.1f%%)", pc_x, 100 * var_exp[[pc_x_i]]))
      ylab <- recipe_axis_label(style, "y_label", sprintf("%s (%.1f%%)", pc_y, 100 * var_exp[[pc_y_i]]))
      plot(scores[[pc_x]], scores[[pc_y]], pch = 19, col = point_cols, xlab = xlab, ylab = ylab,
           main = style$title %||% paste("PCA colored by", color_title),
           cex.lab = style_num(style, "x_axis_size", 12) / 12,
           cex.axis = style_num(style, "x_tick_size", 9) / 9,
           cex.main = style_num(style, "title_size", 18) / 18)
      if (numeric_color) {
        draw_continuous_palette_legend(color_title, scores$.color_numeric)
      } else {
        legend("topright", legend = levels(groups), col = palette_cols, pch = 19, bty = "n", cex = style_num(style, "legend_size", 9) / 11)
      }
      return(TRUE)
    }
    if (identical(plot_id, "dimred_separation_plot")) {
      numeric_sep <- any(is.finite(scores$.separation_numeric))
      op <- par(mar = c(4.8, 4.8, 3.0, if (numeric_sep) 5.8 else 1.5), family = style$font_family %||% "Arial")
      on.exit(par(op), add = TRUE)
      sep_title <- plot_display_label(snapshot_value(snapshot, "dimred_separation_by", "selected feature"), width = 50, multiline = FALSE)
      if (numeric_sep) {
        point_cols <- continuous_palette(scores$.separation_numeric)
        plot(scores[[pc_x]], scores[[pc_y]], pch = 19, col = point_cols,
             xlab = recipe_axis_label(style, "x_label", pc_x),
             ylab = recipe_axis_label(style, "y_label", pc_y),
             main = style$title %||% paste("PCA separation by", sep_title),
             cex.lab = style_num(style, "x_axis_size", 12) / 12,
             cex.axis = style_num(style, "x_tick_size", 9) / 9,
             cex.main = style_num(style, "title_size", 18) / 18)
        draw_continuous_palette_legend(sep_title, scores$.separation_numeric)
      } else {
        groups <- droplevels(scores$.separation_group)
        palette_cols <- grDevices::hcl.colors(length(levels(groups)), "Dark 3")
        plot(scores[[pc_x]], scores[[pc_y]], pch = 19, col = palette_cols[as.integer(groups)],
             xlab = recipe_axis_label(style, "x_label", pc_x),
             ylab = recipe_axis_label(style, "y_label", pc_y),
             main = style$title %||% paste("PCA separation by", sep_title),
             cex.lab = style_num(style, "x_axis_size", 12) / 12,
             cex.axis = style_num(style, "x_tick_size", 9) / 9,
             cex.main = style_num(style, "title_size", 18) / 18)
        centers <- aggregate(scores[, c(pc_x, pc_y), drop = FALSE], list(group = groups), mean, na.rm = TRUE)
        points(centers[[pc_x]], centers[[pc_y]], pch = 23, bg = "#fbf1d7", col = "#173f35", cex = 1.7)
        text(centers[[pc_x]], centers[[pc_y]], labels = plot_display_labels(centers$group, width = 18), pos = 3, cex = 0.75)
        legend("topright", legend = levels(groups), col = palette_cols, pch = 19, bty = "n", cex = style_num(style, "legend_size", 9) / 11)
      }
      return(TRUE)
    }
    FALSE
  }

  render_recipe_contrast_plot <- function(payload, style) {
    r <- payload$ranking %||% data.frame()
    if (!is.data.frame(r) || !nrow(r) || !"Abs_effect_size" %in% names(r)) return(FALSE)
    r <- r[is.finite(r$Abs_effect_size), , drop = FALSE]
    if (!nrow(r)) return(FALSE)
    r <- head(r, 12)
    labels <- recipe_tick_labels(style, "y_tick_labels", plot_display_labels(rev(r$Feature), width = 26))
    label_width <- max(12, min(24, max(nchar(gsub("\n", "", labels)), na.rm = TRUE) * 0.28))
    op <- par(mar = c(5, label_width, 3, 1), family = style$font_family %||% "Arial")
    on.exit(par(op), add = TRUE)
    barplot(rev(r$Abs_effect_size), names.arg = labels, horiz = TRUE, las = 1,
            cex.names = style_num(style, "y_tick_size", 9) / 12,
            col = "#8fb8de", border = "white",
            main = style$title %||% "Variables Best Separating the Selected Contrast",
            xlab = recipe_axis_label(style, "x_label", "|Cohen's d|"),
            cex.lab = style_num(style, "x_axis_size", 12) / 12,
            cex.axis = style_num(style, "x_tick_size", 9) / 9,
            cex.main = style_num(style, "title_size", 18) / 18)
    TRUE
  }

  render_recipe_covariate_heatmap <- function(payload, style) {
    res <- payload$result %||% list()
    mat <- res$matrix
    if (!is.matrix(mat) || !nrow(mat)) return(FALSE)
    display_colnames <- plot_display_labels(colnames(mat), width = 18)
    display_rownames <- plot_display_labels(rownames(mat), width = 22)
    label_max <- max(nchar(c(display_rownames, display_colnames)), na.rm = TRUE)
    label_cex <- max(0.42, min(0.75, style_num(style, "x_tick_size", 9) / max(label_max, 12)))
    op <- par(mar = c(max(10, min(18, 4 + 0.22 * label_max)), max(10, min(22, 4 + 0.24 * label_max)), 4, 6), xpd = NA, family = style$font_family %||% "Arial")
    on.exit(par(op), add = TRUE)
    ord <- stats::hclust(stats::as.dist(1 - abs(mat)))$order
    mat <- mat[ord, ord, drop = FALSE]
    display_colnames <- display_colnames[ord]
    display_rownames <- display_rownames[ord]
    cols <- grDevices::colorRampPalette(c("#2b6cb0", "#f7f2df", "#b8322a"))(101)
    image(seq_len(nrow(mat)), seq_len(ncol(mat)), t(mat[nrow(mat):1, ]), col = cols, zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "", main = style$title %||% "Covariate Correlation Heatmap")
    axis(1, at = seq_len(ncol(mat)), labels = FALSE)
    axis(2, at = seq_len(nrow(mat)), labels = FALSE)
    text(seq_len(ncol(mat)), par("usr")[3] - 0.045 * diff(par("usr")[3:4]), labels = display_colnames, srt = style_num(style, "x_tick_angle", 45), adj = 1, cex = label_cex)
    text(par("usr")[1] - 0.035 * diff(par("usr")[1:2]), seq_len(nrow(mat)), labels = rev(display_rownames), adj = 1, cex = label_cex)
    legend("topright", inset = c(-0.14, 0), legend = c("-1", "0", "1"), fill = c("#2b6cb0", "#f7f2df", "#b8322a"), title = "r", bty = "n", cex = style_num(style, "legend_size", 9) / 11)
    box()
    TRUE
  }

  render_recipe_grouped_plot <- function(payload, style, snapshot) {
    df <- payload$filtered_table %||% data.frame()
    if (!is.data.frame(df) || !nrow(df)) return(FALSE)
    var <- snapshot_value(snapshot, "generalization_plot_variable", "")
    normalize_by <- snapshot_value(snapshot, "generalization_plot_normalize_by", "")
    group <- snapshot_value(snapshot, "generalization_plot_group", "")
    group2 <- snapshot_value(snapshot, "generalization_plot_group2", "")
    color_by <- snapshot_value(snapshot, "generalization_plot_color_by", "__group__")
    bar_summary_metric <- snapshot_value(snapshot, "generalization_plot_bar_summary", "mean")
    bar_summary_label <- generalization_bar_summary_label(bar_summary_metric)
    y_max <- parse_generalization_plot_ymax(snapshot_value(snapshot, "generalization_plot_ymax", ""))
    palette_min <- parse_generalization_palette_limit(snapshot_value(snapshot, "generalization_plot_palette_min", ""))
    palette_max <- parse_generalization_palette_limit(snapshot_value(snapshot, "generalization_plot_palette_max", ""))
    palette_limits <- if (is.finite(palette_min) && is.finite(palette_max) && palette_max > palette_min) c(palette_min, palette_max) else NULL
    if (!nzchar(var) || !var %in% names(df) || !nzchar(group) || !group %in% names(df)) return(FALSE)
    df$.y <- suppressWarnings(as.numeric(df[[var]]))
    y_label <- recipe_axis_label(style, "y_label", plot_display_label(var, width = 36))
    plot_title_var <- plot_display_label(var, width = 60, multiline = FALSE)
    if (nzchar(normalize_by) && normalize_by %in% names(df) && !identical(normalize_by, var)) {
      denom <- suppressWarnings(as.numeric(df[[normalize_by]]))
      invalid <- !is.finite(denom) | denom == 0
      df$.y[invalid] <- NA_real_
      df$.y[!invalid] <- df$.y[!invalid] / denom[!invalid]
      y_label <- recipe_axis_label(style, "y_label", paste(plot_display_label(var, width = 28, multiline = FALSE), "/", plot_display_label(normalize_by, width = 28, multiline = FALSE)))
      plot_title_var <- paste(plot_display_label(var, width = 42, multiline = FALSE), "normalized by", plot_display_label(normalize_by, width = 42, multiline = FALSE))
    }
    grouping_cols <- c(group, if (nzchar(group2) && group2 %in% names(df)) group2 else character())
    df$.group <- if (length(grouping_cols) > 1) ordered_interaction_factor(df, grouping_cols) else ordered_factor_for_plot(df[[group]])
    df <- df[is.finite(df$.y) & !is.na(df$.group), , drop = FALSE]
    if (!nrow(df)) return(FALSE)
    facet_split <- build_generalization_facet_split_from_snapshot(df, snapshot)
    apply_facets <- identical(tolower(snapshot_value(snapshot, "generalization_plot_apply_facets", "FALSE")), "true") && isTRUE(facet_split$ok)
    if (apply_facets) {
      df$.facet <- facet_split$facet
      df <- df[!is.na(df$.facet), , drop = FALSE]
      if (!nrow(df)) return(FALSE)
    } else {
      df$.facet <- factor("All rows")
    }
    levels_x <- levels(df$.group)
    display_levels_x <- recipe_tick_labels(style, "x_tick_labels", plot_display_labels(gsub(" \\| ", " | ", levels_x), width = 22))
    color_source <- if (identical(color_by, "__group__") || !color_by %in% names(df)) group else color_by
    color_is_numeric <- color_source %in% names(df) && vector_is_numeric_like(df[[color_source]])
    facet_levels <- levels(droplevels(df$.facet))
    if (!length(facet_levels)) facet_levels <- unique(as.character(df$.facet))
    n_panels <- length(facet_levels)
    op <- par(
      mfrow = c(n_panels, 1),
      mar = c(if (n_panels > 1) 5.5 else 8, 5.2, 3.2, if (color_is_numeric) 8 else 3),
      family = style$font_family %||% "Arial"
    )
    on.exit(par(op), add = TRUE)
    ylim <- range(c(0, df$.y), na.rm = TRUE)
    if (is.finite(y_max)) {
      ylim <- c(0, y_max)
    } else if (!is.finite(diff(ylim)) || diff(ylim) == 0) {
      ylim <- ylim + c(-0.5, 0.5)
    }
    xlim <- c(0.5, length(levels_x) + 0.5)
    for (facet_label in facet_levels) {
      panel_df <- df[as.character(df$.facet) == facet_label, , drop = FALSE]
      if (!nrow(panel_df)) next
      x <- as.numeric(panel_df$.group)
      set.seed(42)
      jitter_x <- x + runif(length(x), -0.18, 0.18)
      if (color_is_numeric) {
        point_cols <- continuous_palette(panel_df[[color_source]], limits = palette_limits)
        bar_color_values <- tapply(
          suppressWarnings(as.numeric(panel_df[[color_source]])),
          panel_df$.group,
          function(v) generalization_bar_summary_value(v, bar_summary_metric)
        )
        bar_cols <- continuous_palette(bar_color_values, limits = palette_limits)
        names(bar_cols) <- names(bar_color_values)
      } else {
        color_factor <- if (color_source %in% names(panel_df)) ordered_factor_for_plot(panel_df[[color_source]]) else panel_df$.group
        pal <- grDevices::hcl.colors(length(levels(color_factor)), "Dark 3")
        point_cols <- pal[as.integer(color_factor)]
        bar_group <- tapply(as.character(color_factor), panel_df$.group, function(v) names(sort(table(v), decreasing = TRUE))[[1]])
        bar_cols <- pal[match(bar_group, levels(color_factor))]
        names(bar_cols) <- names(bar_group)
      }
      main_title <- if (apply_facets) {
        paste(plot_title_var, "|", plot_display_label(facet_split$variable, width = 32, multiline = FALSE), "=", facet_label)
      } else {
        style$title %||% paste(plot_title_var, "by", plot_display_label(group, width = 32, multiline = FALSE))
      }
      plot(
        jitter_x, panel_df$.y,
        xaxt = "n",
        xlab = if (n_panels > 1) "" else recipe_axis_label(style, "x_label", ""),
        ylab = y_label,
        pch = 19,
        col = point_cols,
        xlim = xlim,
        ylim = ylim,
        main = main_title,
        cex.lab = style_num(style, "x_axis_size", 12) / 12,
        cex.axis = style_num(style, "x_tick_size", 9) / 9,
        cex.main = style_num(style, "title_size", 18) / 18
      )
      means <- tapply(panel_df$.y, panel_df$.group, function(v) generalization_bar_summary_value(v, bar_summary_metric))
      bar_centers <- seq_along(levels_x)
      draw_idx <- which(is.finite(means[levels_x]))
      bar_cols_full <- rep("#8fb8de", length(levels_x))
      names(bar_cols_full) <- levels_x
      matching_bar_cols <- intersect(names(bar_cols), levels_x)
      bar_cols_full[matching_bar_cols] <- bar_cols[matching_bar_cols]
      rect(bar_centers[draw_idx] - 0.28, 0, bar_centers[draw_idx] + 0.28, means[levels_x][draw_idx], col = grDevices::adjustcolor(bar_cols_full[draw_idx], alpha.f = 0.38), border = "#173f35")
      points(jitter_x, panel_df$.y, pch = 19, col = point_cols)
      points(bar_centers, means[levels_x], pch = 23, bg = "#fbf1d7", col = "#173f35", cex = 1.4)
      axis(1, at = seq_along(levels_x), labels = display_levels_x, las = style_num(style, "x_tick_angle", 2), cex.axis = if (n_panels > 1) style_num(style, "x_tick_size", 9) / 14 else style_num(style, "x_tick_size", 9) / 12)
      if (!apply_facets || identical(facet_label, facet_levels[[1]])) {
        if (color_is_numeric) {
          draw_continuous_palette_legend(plot_display_label(color_source, width = 28, multiline = FALSE), panel_df[[color_source]], limits = palette_limits)
          legend("topright", legend = bar_summary_label, pch = 23, pt.bg = "#fbf1d7", col = "#173f35", bty = "n", cex = style_num(style, "legend_size", 9) / 11)
        } else if (exists("color_factor") && length(levels(color_factor)) <= 8) {
          legend("topright", legend = c(levels(color_factor), bar_summary_label), pch = c(rep(19, length(levels(color_factor))), 23), pt.bg = c(rep(NA, length(levels(color_factor))), "#fbf1d7"), col = c(pal, "#173f35"), bty = "n", cex = style_num(style, "legend_size", 9) / 12)
        }
      }
    }
    TRUE
  }

  render_publication_recipe_plot <- function(fig_dir, style) {
    recipe_path <- file.path(fig_dir, "plot_recipe.rds")
    data_path <- file.path(fig_dir, "plot_data_snapshot.rds")
    if (!file.exists(recipe_path) || !file.exists(data_path)) return(FALSE)
    recipe <- tryCatch(readRDS(recipe_path), error = function(e) NULL)
    payload <- tryCatch(readRDS(data_path), error = function(e) NULL)
    snapshot <- tryCatch(utils::read.csv(file.path(fig_dir, "input_snapshot.csv"), stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    plot_id <- recipe$plot_id %||% publication_candidate_plot_id(fig_dir)
    if (!nzchar(plot_id)) return(FALSE)
    if (plot_id %in% c("dimred_pca_plot", "dimred_scree_plot", "dimred_separation_plot")) return(render_recipe_dimred_plot(plot_id, payload, style, snapshot))
    if (identical(plot_id, "contrast_variable_plot")) return(render_recipe_contrast_plot(payload, style))
    if (identical(plot_id, "covariate_correlation_heatmap")) return(render_recipe_covariate_heatmap(payload, style))
    if (identical(plot_id, "generalization_grouped_plot")) return(render_recipe_grouped_plot(payload, style, snapshot))
    FALSE
  }

  render_publication_recipe_plot_file <- function(fig_dir, file, width = 3600, height = 2600, res = 300, style_override = NULL) {
    style <- style_override %||% read_publication_style(fig_dir)
    if (!publication_recipe_supported(fig_dir)) return(FALSE)
    diagnostic <- ""
    ok <- tryCatch({
      grDevices::png(file, width = width, height = height, res = res, family = style$font_family %||% "Arial")
      on.exit(grDevices::dev.off(), add = TRUE)
      rendered <- render_publication_recipe_plot(fig_dir, style)
      if (!isTRUE(rendered)) {
        diagnostic <<- "Recipe-backed rendering returned FALSE. The saved recipe/data snapshot exists, but the plot builder could not use it."
        plot.new()
        text(0.5, 0.58, "Recipe-backed preview could not be rendered.", cex = 1.1)
        text(0.5, 0.45, "The candidate is saved as data + recipe + style, not as a PNG.", cex = 0.85)
        text(0.5, 0.35, "Try saving a fresh candidate from the original plot tab, or inspect plot_recipe.rds / plot_data_snapshot.rds.", cex = 0.75)
      }
      isTRUE(rendered)
    }, error = function(e) {
      try(grDevices::dev.off(), silent = TRUE)
      diagnostic <<- conditionMessage(e)
      try({
        grDevices::png(file, width = width, height = height, res = res, family = style$font_family %||% "Arial")
        on.exit(grDevices::dev.off(), add = TRUE)
        plot.new()
        text(0.5, 0.62, "Recipe-backed preview failed.", cex = 1.1)
        wrapped <- paste(strwrap(diagnostic, width = 90), collapse = "\n")
        text(0.5, 0.45, wrapped, cex = 0.75)
      }, silent = TRUE)
      FALSE
    })
    diag_path <- file.path(fig_dir, "last_recipe_render_status.txt")
    status_lines <- c(
      sprintf("rendered: %s", isTRUE(ok)),
      sprintf("checked_at: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("diagnostic: %s", diagnostic %||% "")
    )
    try(writeLines(status_lines, diag_path, useBytes = TRUE), silent = TRUE)
    file.exists(file) && isTRUE(file.info(file)$size > 0)
  }

  render_publication_figure_file <- function(fig_dir, file, width = 3600, height = 2600, res = 300, style_override = NULL) {
    style <- style_override %||% read_publication_style(fig_dir)
    if (publication_recipe_supported(fig_dir)) {
      render_publication_recipe_plot_file(fig_dir, file, width = width, height = height, res = res, style_override = style)
      return(invisible(TRUE))
    }
    original_style <- read_publication_original_style(fig_dir)
    plot_path <- file.path(fig_dir, "recorded_plot.rds")
    plot_obj <- if (file.exists(plot_path)) readRDS(plot_path) else NULL
    source_png <- file.path(fig_dir, "source_plot.png")
    source_img <- NULL
    if (file.exists(source_png) && requireNamespace("png", quietly = TRUE)) {
      source_img <- tryCatch(png::readPNG(source_png), error = function(e) NULL)
    }
    grDevices::png(file, width = width, height = height, res = res, family = style$font_family %||% "Arial")
    on.exit(grDevices::dev.off(), add = TRUE)
    par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
    plot.new()
    plot.window(xlim = c(0, 1), ylim = c(0, 1))

    has_title <- publication_element_changed(style, original_style, "title", c("title_size", "title_distance", "italicize_title", "bold_title"))
    has_subtitle <- publication_element_changed(style, original_style, "subtitle", c("subtitle_size", "subtitle_distance", "italicize_subtitle", "bold_subtitle"))
    has_x <- publication_element_changed(style, original_style, "x_label", c("x_axis_size", "x_axis_angle", "x_axis_distance", "italicize_x_axis", "bold_x_axis"))
    has_y <- publication_element_changed(style, original_style, "y_label", c("y_axis_size", "y_axis_angle", "y_axis_distance", "italicize_y_axis", "bold_y_axis"))
    has_x_ticks <- publication_element_changed(style, original_style, "x_tick_labels", c("x_tick_size", "x_tick_angle", "x_tick_distance", "italicize_x_ticks", "bold_x_ticks"))
    has_y_ticks <- publication_element_changed(style, original_style, "y_tick_labels", c("y_tick_size", "y_tick_angle", "y_tick_distance", "italicize_y_ticks", "bold_y_ticks"))
    has_legend <- publication_element_changed(style, original_style, "legend_text", c("legend_size", "legend_x_nudge", "legend_y_nudge", "legend_symbol_type", "legend_colors"))
    has_caption <- publication_element_changed(style, original_style, "caption", c("caption_size", "caption_distance", "caption_x_nudge", "caption_y_nudge"))
    auto_crop <- style_bool(style, "auto_crop_original_text", TRUE)
    crop_top <- style_num(style, "source_crop_top", 0)
    crop_bottom <- style_num(style, "source_crop_bottom", 0)
    crop_left <- style_num(style, "source_crop_left", 0)
    crop_right <- style_num(style, "source_crop_right", 0)
    if (auto_crop) {
      if ((has_title || has_subtitle) && style_bool(style, "hide_original_title", TRUE)) crop_top <- max(crop_top, 0.145)
      if (has_x && style_bool(style, "hide_original_x_axis", TRUE)) crop_bottom <- max(crop_bottom, 0.135)
      if (has_x_ticks && style_bool(style, "hide_original_x_ticks", TRUE)) crop_bottom <- max(crop_bottom, 0.215)
      if (has_caption) crop_bottom <- max(crop_bottom, 0.145)
      if (has_y && style_bool(style, "hide_original_y_axis", TRUE)) crop_left <- max(crop_left, 0.135)
      if (has_y_ticks && style_bool(style, "hide_original_y_ticks", TRUE)) crop_left <- max(crop_left, 0.205)
    }
    source_img <- crop_publication_image(source_img, top = crop_top, bottom = crop_bottom, left = crop_left, right = crop_right)
    base_margin <- max(0, min(0.15, style_num(style, "plot_margin", 0.025)))
    extra_top_margin <- max(0, min(0.25, style_num(style, "plot_margin_top", 0)))
    extra_right_margin <- max(0, min(0.25, style_num(style, "plot_margin_right", 0)))
    extra_bottom_margin <- max(0, min(0.25, style_num(style, "plot_margin_bottom", 0)))
    extra_left_margin <- max(0, min(0.25, style_num(style, "plot_margin_left", 0)))
    top_margin <- base_margin + extra_top_margin + if (has_title) 0.085 else 0
    top_margin <- top_margin + if (has_subtitle) 0.050 else 0
    bottom_margin <- base_margin + extra_bottom_margin + if (has_x) 0.080 else 0
    bottom_margin <- bottom_margin + if (has_x_ticks) 0.050 else 0
    bottom_margin <- bottom_margin + if (has_caption) 0.080 else 0
    left_margin <- base_margin + extra_left_margin + if (has_y) 0.080 else 0
    left_margin <- left_margin + if (has_y_ticks) 0.055 else 0
    right_margin <- base_margin + extra_right_margin
    x0 <- min(0.48, left_margin)
    x1 <- max(0.52, 1 - right_margin)
    y0 <- min(0.48, bottom_margin)
    y1 <- max(0.52, 1 - top_margin)

    if (!is.null(source_img)) {
      rasterImage(source_img, x0, y0, x1, y1)
    } else if (!is.null(plot_obj)) {
      grDevices::replayPlot(plot_obj)
    } else {
      rect(x0, y0, x1, y1, border = "#cccccc")
      text((x0 + x1) / 2, (y0 + y1) / 2, "Recorded plot is missing.")
    }

    if (has_title) {
      y <- min(0.985, y1 + style_num(style, "title_distance", 0.035))
      text(0.5, y, style$title %||% "", cex = style_num(style, "title_size", 18) / 12, font = style_font(style, "italicize_title", "bold_title"))
    }
    if (has_subtitle) {
      y <- min(0.965, y1 + style_num(style, "subtitle_distance", 0.065))
      text(0.5, y, style$subtitle %||% "", cex = style_num(style, "subtitle_size", 12) / 12, font = style_font(style, "italicize_subtitle", "bold_subtitle"))
    }
    if (has_x) {
      y <- max(0.015, y0 - style_num(style, "x_axis_distance", 0.035))
      text(0.5, y, style$x_label %||% "", cex = style_num(style, "x_axis_size", 12) / 12, srt = style_num(style, "x_axis_angle", 0), font = style_font(style, "italicize_x_axis", "bold_x_axis"))
    }
    if (has_x_ticks) {
      tick_labels <- split_tick_labels(style$x_tick_labels)
      if (length(tick_labels)) {
        xs <- seq(x0, x1, length.out = length(tick_labels))
        y <- max(0.015, y0 - style_num(style, "x_tick_distance", 0.075))
        text(xs, y, tick_labels, cex = style_num(style, "x_tick_size", 9) / 12, srt = style_num(style, "x_tick_angle", 0), font = style_font(style, "italicize_x_ticks", "bold_x_ticks"))
      }
    }
    if (has_y) {
      x <- max(0.015, x0 - style_num(style, "y_axis_distance", 0.035))
      text(x, (y0 + y1) / 2, style$y_label %||% "", cex = style_num(style, "y_axis_size", 12) / 12, srt = style_num(style, "y_axis_angle", 90), font = style_font(style, "italicize_y_axis", "bold_y_axis"))
    }
    if (has_y_ticks) {
      tick_labels <- split_tick_labels(style$y_tick_labels)
      if (length(tick_labels)) {
        ys <- seq(y0, y1, length.out = length(tick_labels))
        x <- max(0.015, x0 - style_num(style, "y_tick_distance", 0.075))
        text(x, ys, tick_labels, cex = style_num(style, "y_tick_size", 9) / 12, srt = style_num(style, "y_tick_angle", 0), font = style_font(style, "italicize_y_ticks", "bold_y_ticks"), adj = c(1, 0.5))
      }
    }
    if (has_legend) {
      legend_lines <- unlist(strsplit(gsub("\\s*[|;]\\s*", "\n", style$legend_text %||% ""), "\n", fixed = TRUE), use.names = FALSE)
      legend_lines <- legend_lines[nzchar(trimws(legend_lines))]
      if (length(legend_lines)) {
        symbol_type <- style$legend_symbol_type %||% "none"
        x <- min(0.985, max(0.015, x1 - 0.20 + style_num(style, "legend_x_nudge", 0)))
        y <- min(0.985, max(0.015, y1 - 0.025 + style_num(style, "legend_y_nudge", 0)))
        line_height <- 0.025 * max(0.7, style_num(style, "legend_size", 9) / 9)
        ys <- y - seq(0, by = line_height, length.out = length(legend_lines))
        cex <- style_num(style, "legend_size", 9) / 12
        if (symbol_type %in% c("dots", "squares")) {
          cols <- parse_publication_legend_colors(style$legend_colors, length(legend_lines))
          pch <- if (identical(symbol_type, "squares")) 15 else 19
          points(rep(x, length(ys)), ys - 0.006, pch = pch, col = cols, cex = max(0.7, cex))
          text(x + 0.020, ys, legend_lines, cex = cex, adj = c(0, 1))
        } else {
          text(x, ys, legend_lines, cex = cex, adj = c(0, 1))
        }
      }
    }
    if (has_caption) {
      x <- min(0.98, max(0.02, 0.5 + style_num(style, "caption_x_nudge", 0)))
      y <- min(0.985, max(0.015, y0 - style_num(style, "caption_distance", 0.060) + style_num(style, "caption_y_nudge", 0)))
      text(x, y, style$caption %||% "", cex = style_num(style, "caption_size", 10) / 12)
    }
  }

  render_publication_figure_file_safe <- function(fig_dir, file, width = 3600, height = 2600, res = 300, style_override = NULL) {
    ok <- tryCatch({
      render_publication_figure_file(fig_dir, file, width = width, height = height, res = res, style_override = style_override)
      file.exists(file) && isTRUE(file.info(file)$size > 0)
    }, error = function(e) {
      structure(FALSE, message = conditionMessage(e))
    })
    if (isTRUE(ok)) return(TRUE)

    source_png <- file.path(fig_dir, "source_plot.png")
    if (file.exists(source_png) && isTRUE(file.info(source_png)$size > 0)) {
      file.copy(source_png, file, overwrite = TRUE)
      return(TRUE)
    }

    msg <- attr(ok, "message") %||% "The recorded plot and source PNG could not be rendered."
    grDevices::png(file, width = width, height = height, res = res)
    on.exit(grDevices::dev.off(), add = TRUE)
    plot.new()
    text(0.5, 0.62, "Publication preview could not be rendered.", cex = 1.3, font = 2)
    text(0.5, 0.48, paste("Reason:", msg), cex = 0.85)
    text(0.5, 0.36, "Try re-saving the plot candidate from its original Tab 4 panel.", cex = 0.85)
    TRUE
  }

  filterable_table_controls <- function(id, save_publication = TRUE) {
    tagList(
      uiOutput(paste0(id, "_filter_ui")),
      downloadButton(paste0("download_", id, "_csv"), "Download Filtered CSV"),
      if (isTRUE(save_publication)) actionButton(paste0("save_pub_table_", id), "Save Table as Publication Candidate", class = "btn btn-default")
    )
  }

  resolve_filterable_table_title <- function(title) {
    out <- if (is.function(title)) title() else title
    out <- as.character(out %||% "")
    if (!length(out) || !nzchar(out[[1]])) "table" else out[[1]]
  }

  apply_table_value_filter <- function(id, df) {
    if (!is.data.frame(df) || !nrow(df)) return(df)
    col <- input[[paste0(id, "_filter_col")]] %||% ""
    if (nzchar(col) && col %in% names(df)) {
      vals <- df[[col]]
      if (vector_is_numeric_like(vals)) {
        nums <- numeric_like_values(vals)
        min_val <- suppressWarnings(as.numeric(input[[paste0(id, "_filter_min")]]))
        max_val <- suppressWarnings(as.numeric(input[[paste0(id, "_filter_max")]]))
        if (length(min_val) && is.finite(min_val)) df <- df[is.finite(nums) & nums >= min_val, , drop = FALSE]
        nums <- numeric_like_values(df[[col]])
        if (length(max_val) && is.finite(max_val)) df <- df[is.finite(nums) & nums <= max_val, , drop = FALSE]
      }
      text <- trimws(input[[paste0(id, "_filter_text")]] %||% "")
      if (nzchar(text) && nrow(df)) {
        df <- df[grepl(text, as.character(df[[col]]), ignore.case = TRUE, fixed = TRUE), , drop = FALSE]
      }
    }
    sort_col <- input[[paste0(id, "_sort_col")]] %||% ""
    sort_dir <- input[[paste0(id, "_sort_dir")]] %||% "desc"
    if (nzchar(sort_col) && sort_col %in% names(df) && nrow(df)) {
      sort_vals <- df[[sort_col]]
      if (vector_is_numeric_like(sort_vals)) {
        sort_key <- numeric_like_values(sort_vals)
      } else {
        sort_key <- tolower(as.character(sort_vals))
      }
      ord <- order(sort_key, decreasing = identical(sort_dir, "desc"), na.last = TRUE)
      df <- df[ord, , drop = FALSE]
    }
    df
  }

  install_filterable_table <- function(id, data_fun, title = id) {
    output[[paste0(id, "_filter_ui")]] <- renderUI({
      df <- data_fun()
      if (!is.data.frame(df) || !ncol(df)) return(tags$p(class = "warning-text", "No table columns available for filtering yet."))
      cols <- names(df)
      selected <- input[[paste0(id, "_filter_col")]] %||% ""
      if (!selected %in% cols) selected <- ""
      sort_selected <- isolate(input[[paste0(id, "_sort_col")]] %||% "")
      if (!sort_selected %in% cols) sort_selected <- ""
      sort_dir <- isolate(input[[paste0(id, "_sort_dir")]] %||% "desc")
      if (!sort_dir %in% c("asc", "desc")) sort_dir <- "desc"
      numeric_ui <- NULL
      if (nzchar(selected) && selected %in% cols && vector_is_numeric_like(df[[selected]])) {
        vals <- numeric_like_values(df[[selected]])
        vals <- vals[is.finite(vals)]
        if (length(vals)) {
          numeric_ui <- fluidRow(
            column(6, numericInput(paste0(id, "_filter_min"), "Min value", value = min(vals), min = min(vals), max = max(vals))),
            column(6, numericInput(paste0(id, "_filter_max"), "Max value", value = max(vals), min = min(vals), max = max(vals)))
          )
        }
      }
      tagList(
        tags$div(
          class = "metric-info-box",
          tags$strong("Table filter"),
          tags$p("Choose a column, optionally type a value to search, and use min/max bounds for numeric columns. You can also rank/order the table by any column. The CSV download uses the filtered and sorted rows.")
        ),
        selectInput(paste0(id, "_filter_col"), "Filter table by column", choices = c("No filter" = "", cols), selected = selected),
        textInput(paste0(id, "_filter_text"), "Contains text/value", value = isolate(input[[paste0(id, "_filter_text")]] %||% "")),
        numeric_ui,
        fluidRow(
          column(7, selectInput(paste0(id, "_sort_col"), "Order/rank table by column", choices = c("No ordering" = "", cols), selected = sort_selected)),
          column(5, radioButtons(paste0(id, "_sort_dir"), "Order direction", choices = c("Highest/Z-A first" = "desc", "Lowest/A-Z first" = "asc"), selected = sort_dir, inline = FALSE))
        )
      )
    })
    output[[paste0("download_", id, "_csv")]] <- downloadHandler(
      filename = function() paste0(gsub("[^A-Za-z0-9._-]+", "_", resolve_filterable_table_title(title)), "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
      content = function(file) {
        utils::write.csv(apply_table_value_filter(id, data_fun()), file, row.names = FALSE, na = "")
      }
    )
    observeEvent(input[[paste0("save_pub_table_", id)]], {
      save_publication_table_candidate(id, resolve_filterable_table_title(title), apply_table_value_filter(id, data_fun()))
    }, ignoreInit = TRUE)
  }

  report_archive_path <- reactiveVal("")
  report_manifest_df <- reactiveVal(data.frame())

  html_escape <- function(x) {
    x <- as.character(x %||% "")
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x
  }

  html_table <- function(df, max_rows = 25) {
    if (!is.data.frame(df) || !nrow(df)) return("<p class='muted'>No rows available.</p>")
    shown <- head(df, max_rows)
    headers <- paste(sprintf("<th>%s</th>", html_escape(names(shown))), collapse = "")
    rows <- apply(shown, 1, function(row) paste(sprintf("<td>%s</td>", html_escape(row)), collapse = ""))
    more <- if (nrow(df) > nrow(shown)) sprintf("<p class='muted'>Showing %s of %s rows.</p>", nrow(shown), nrow(df)) else ""
    paste0("<table><thead><tr>", headers, "</tr></thead><tbody>",
           paste(sprintf("<tr>%s</tr>", rows), collapse = "\n"),
           "</tbody></table>", more)
  }

  report_output_status_df <- function(s) {
    p <- build_paths(s)
    data.frame(
      artifact = c(
        "Managed input workspace", "Central output workspace", "Renamed image folder", "Image list", "Mask percentages CSV", "Validation groups CSV",
        "Inspected validation CSV", "Packaged ROI RDS", "Optimization results CSV", "Generalized skeleton RDS", "Generalized model-feature CSV",
        "Metadata PCA RDS", "Final reporting folder"
      ),
      exists = c(
        dir.exists(p$input_workspace), dir.exists(p$output_root), dir.exists(p$pre_renamed), file.exists(p$image_list), file.exists(p$mask_percentages),
        file.exists(p$groups), file.exists(p$inspected), file.exists(p$opt_rds),
        file.exists(p$param_results), file.exists(p$generalized),
        file.exists(file.path(p$output_root, "OUT_generalized_analysis", "generalized_model_feature_table.csv")),
        file.exists(p$pca_rds), dir.exists(p$final_dir)
      ),
      path = c(
        p$input_workspace, p$output_root, p$pre_renamed, p$image_list, p$mask_percentages, p$groups, p$inspected,
        p$opt_rds, p$param_results, p$generalized,
        file.path(p$output_root, "OUT_generalized_analysis", "generalized_model_feature_table.csv"),
        p$pca_rds, p$final_dir
      ),
      stringsAsFactors = FALSE
    )
  }

  copy_report_file <- function(src, dest_root, rel_dir, manifest_rows, role = "attachment") {
    if (!nzchar(src %||% "") || !file.exists(src) || dir.exists(src)) return(manifest_rows)
    target_dir <- file.path(dest_root, rel_dir)
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
    safe_name <- gsub("[^A-Za-z0-9._-]+", "_", basename(src))
    dest <- file.path(target_dir, safe_name)
    ok <- file.copy(src, dest, overwrite = TRUE)
    manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
      role = role,
      source_path = normalizePath(src, winslash = "/", mustWork = FALSE),
      archive_path = normalizePath(dest, winslash = "/", mustWork = FALSE),
      bytes = if (isTRUE(ok) && file.exists(dest)) file.info(dest)$size else NA_real_,
      stringsAsFactors = FALSE
    )
    manifest_rows
  }

  copy_report_dir <- function(src_dir, dest_root, rel_dir, manifest_rows, role = "large_folder") {
    if (!nzchar(src_dir %||% "") || !dir.exists(src_dir)) return(manifest_rows)
    target_dir <- file.path(dest_root, rel_dir, basename(src_dir))
    dir.create(dirname(target_dir), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(src_dir, dirname(target_dir), recursive = TRUE, overwrite = TRUE)
    manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
      role = role,
      source_path = normalizePath(src_dir, winslash = "/", mustWork = FALSE),
      archive_path = normalizePath(target_dir, winslash = "/", mustWork = FALSE),
      bytes = if (dir.exists(target_dir)) sum(file.info(list.files(target_dir, recursive = TRUE, full.names = TRUE))$size, na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
    manifest_rows
  }

  write_report_csv <- function(df, dest_root, rel_path, manifest_rows, role = "generated_table") {
    target <- file.path(dest_root, rel_path)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(df, target, row.names = FALSE, na = "")
    manifest_rows[[length(manifest_rows) + 1L]] <- data.frame(
      role = role,
      source_path = "generated by app",
      archive_path = normalizePath(target, winslash = "/", mustWork = FALSE),
      bytes = file.info(target)$size,
      stringsAsFactors = FALSE
    )
    manifest_rows
  }

  make_report_archive <- function() {
    s <- settings()
    p <- build_paths(s)
    include <- input$report_include_sections %||% character()
    report_name <- sanitize_profile_name(input$report_name_input %||% paste0("neurite_analysis_report_", format(Sys.Date(), "%Y%m%d")))
    timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
    report_root <- file.path(project_root, "cache", "analysis_reports", paste0(report_name, "_", timestamp))
    dir.create(report_root, recursive = TRUE, showWarnings = FALSE)
    manifest_rows <- list()

    hist <- history_df()
    output_status <- report_output_status_df(s)
    software <- current_software_versions(s)
    packages <- current_package_versions(project_root)
    profiles <- list_validation_profiles(s)
    steps <- do.call(rbind, lapply(step_specs(s, input), function(step) {
      data.frame(phase = step$phase, title = step$title, inputs = as.character(step$inputs), outputs = as.character(step$outputs), command = step$command %||% "", stringsAsFactors = FALSE)
    }))

    manifest_rows <- write_report_csv(output_status, report_root, file.path("attachments", "tables", "output_status.csv"), manifest_rows)
    manifest_rows <- write_report_csv(software, report_root, file.path("attachments", "environment", "software_versions.csv"), manifest_rows)
    manifest_rows <- write_report_csv(packages, report_root, file.path("attachments", "environment", "r_package_versions.csv"), manifest_rows)
    manifest_rows <- write_report_csv(steps, report_root, file.path("attachments", "workflow", "step_commands.csv"), manifest_rows)
    manifest_rows <- write_report_csv(hist, report_root, file.path("attachments", "logs", "command_history.csv"), manifest_rows)
    manifest_rows <- write_report_csv(profiles, report_root, file.path("attachments", "profiles", "detected_validation_profiles.csv"), manifest_rows)

    if ("config" %in% include) {
      manifest_rows <- copy_report_file(settings_path, report_root, file.path("attachments", "config"), manifest_rows, "configuration")
      manifest_rows <- copy_report_file(p$metadata, report_root, file.path("attachments", "config"), manifest_rows, "metadata_input")
    }

    if ("scripts" %in% include) {
      script_files <- c(
        list.files(project_root, pattern = "\\.(R|r|sh|ijm)$", full.names = TRUE, recursive = FALSE),
        list.files(file.path(project_root, "launchers"), pattern = "\\.(sh|bat|ps1)$", full.names = TRUE, recursive = TRUE),
        list.files(file.path(project_root, "docs"), pattern = "\\.md$", full.names = TRUE, recursive = TRUE),
        file.path(project_root, "README.md"),
        file.path(project_root, "START_HERE.txt"),
        file.path(project_root, "app.R"),
        file.path(project_root, "frontend", "index.html")
      )
      script_files <- unique(script_files[file.exists(script_files) & !dir.exists(script_files)])
      for (f in script_files) manifest_rows <- copy_report_file(f, report_root, file.path("attachments", "scripts"), manifest_rows, "script_or_macro")
    }

    if ("logs" %in% include) {
      log_files <- c(run_history_file, list.files(run_log_dir, pattern = "\\.(log|txt|csv)$", full.names = TRUE, recursive = TRUE))
      log_files <- unique(log_files[file.exists(log_files) & !dir.exists(log_files)])
      for (f in log_files) manifest_rows <- copy_report_file(f, report_root, file.path("attachments", "logs"), manifest_rows, "run_log")
    }

    if ("profiles" %in% include) {
      selected_profiles <- input$report_profile_choices %||% character()
      if (length(selected_profiles)) {
        for (profile_name in selected_profiles) {
          profile_dir <- validation_profile_dir(profile_name, s)
          if (nzchar(profile_dir %||% "") && dir.exists(profile_dir)) {
            files <- list.files(profile_dir, full.names = TRUE, recursive = TRUE)
            files <- files[file.exists(files) & !dir.exists(files)]
            for (f in files) manifest_rows <- copy_report_file(f, report_root, file.path("attachments", "profiles", sanitize_profile_name(profile_name)), manifest_rows, "validation_profile")
          }
        }
      }
    }

    if ("tables" %in% include) {
      key_files <- c(
        p$image_list, p$mask_percentages, p$groups, p$inspected, manual_review_manifest_path(s),
        p$param_results, p$filtered_rois, p$opt_rds,
        file.path(p$out_opt, "optimization_detailed_report.txt"),
        file.path(p$out_opt, "optimization_input_diagnostics.csv"),
        file.path(p$out_opt, "optimization_model_metadata.csv"),
        file.path(p$out_opt, "roi_extraction_detailed_log.txt"),
        file.path(p$out_opt, "roi_extraction_diagnostics.csv"),
        file.path(p$output_root, "OUT_generalized_analysis", "generalized_model_feature_table.csv"),
        file.path(p$output_root, "OUT_generalized_analysis", "generalized_model_feature_table.rds")
      )
      key_files <- unique(key_files[file.exists(key_files) & !dir.exists(key_files)])
      for (f in key_files) manifest_rows <- copy_report_file(f, report_root, file.path("attachments", "tables"), manifest_rows, "analysis_output")
    }

    if ("plots" %in% include) {
      plot_roots <- unique(c(p$out_opt, file.path(p$output_root, "OUT_generalized_analysis"), file.path(p$output_root, "OUT_metadata_PCA"), p$final_dir))
      plot_files <- unlist(lapply(plot_roots[dir.exists(plot_roots)], function(root) {
        list.files(root, pattern = "\\.(png|jpg|jpeg|pdf|svg)$", full.names = TRUE, recursive = TRUE)
      }), use.names = FALSE)
      plot_files <- unique(plot_files[file.exists(plot_files) & !dir.exists(plot_files)])
      for (f in plot_files) manifest_rows <- copy_report_file(f, report_root, file.path("attachments", "plots"), manifest_rows, "plot_or_visual_report")
      if (dir.exists(publication_figure_dir)) {
        manifest_rows <- copy_report_dir(publication_figure_dir, report_root, file.path("attachments", "publication_figures"), manifest_rows, "publication_figure_workspace")
      }
      pub_figures <- publication_candidates_table()
      if (is.data.frame(pub_figures) && nrow(pub_figures) && !"Message" %in% names(pub_figures)) {
        manifest_rows <- write_report_csv(pub_figures, report_root, file.path("attachments", "publication_figures", "publication_figure_candidates.csv"), manifest_rows, "publication_figure_table")
      }
      if (dir.exists(publication_table_dir)) {
        manifest_rows <- copy_report_dir(publication_table_dir, report_root, file.path("attachments", "publication_tables"), manifest_rows, "publication_table_workspace")
      }
      pub_tables <- publication_tables_table()
      if (is.data.frame(pub_tables) && nrow(pub_tables) && !"Message" %in% names(pub_tables)) {
        manifest_rows <- write_report_csv(pub_tables, report_root, file.path("attachments", "publication_tables", "publication_table_candidates.csv"), manifest_rows, "publication_table_index")
      }
    }

    if (isTRUE(input$report_include_large_outputs)) {
      large_dirs <- c(p$pre_renamed, p$out_clear, p$out_seg, file.path(p$out_opt, s$OUT_ROI_DIR_NAME %||% "ROIs"))
      for (d in large_dirs) manifest_rows <- copy_report_dir(d, report_root, file.path("attachments", "large_outputs"), manifest_rows, "large_output_folder")
    }

    manifest <- if (length(manifest_rows)) do.call(rbind, manifest_rows) else data.frame(role = character(), source_path = character(), archive_path = character(), bytes = numeric(), stringsAsFactors = FALSE)
    manifest_path <- file.path(report_root, "attachments", "archive_manifest.csv")
    dir.create(dirname(manifest_path), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(manifest, manifest_path, row.names = FALSE, na = "")
    manifest <- rbind(
      manifest,
      data.frame(
        role = "archive_manifest",
        source_path = "generated by app",
        archive_path = normalizePath(manifest_path, winslash = "/", mustWork = FALSE),
        bytes = file.info(manifest_path)$size,
        stringsAsFactors = FALSE
      )
    )

    recreate <- paste(
      "Recreation outline:",
      "1. Install the software and R packages listed in attachments/environment.",
      "2. Restore or inspect config/pipeline_settings.env and the metadata CSV from attachments/config.",
      "3. Review attachments/workflow/step_commands.csv for the exact app-controlled step order and backend commands.",
      "4. Review attachments/logs/command_history.csv and run_logs for exact timestamps, statuses, and live backend output.",
      "5. If reusing a validated cutoff model, restore the selected validation profile folder from attachments/profiles.",
      "6. Review attachments/publication_figures and attachments/publication_tables for saved publication candidates, editable metadata, input snapshots, and export-ready files.",
      "7. If large outputs were not included, rerun the setup/generalization steps from the original raw images using the same settings and scripts.",
      sep = "\n"
    )
    writeLines(recreate, file.path(report_root, "RECREATE_ANALYSIS.txt"), useBytes = TRUE)

    report_html <- file.path(report_root, "report.html")
    html <- paste0(
      "<!doctype html><html><head><meta charset='utf-8'><title>", html_escape(report_name), "</title>",
      "<style>body{font-family:Georgia,serif;line-height:1.45;margin:32px;color:#1f2a2e}h1,h2{font-family:Arial,sans-serif}table{border-collapse:collapse;width:100%;font-size:13px;margin:12px 0}th,td{border:1px solid #d7dee2;padding:6px;vertical-align:top;word-break:break-word}th{background:#eef4f5}.muted{color:#667}.box{background:#f7faf9;border:1px solid #dbe6e3;border-radius:10px;padding:14px;margin:14px 0}code{background:#eef4f5;padding:2px 4px;border-radius:4px}</style></head><body>",
      "<h1>Neurite Analysis in Phase Contrast Images: Reproducible Report</h1>",
      "<div class='box'><p><strong>Generated:</strong> ", html_escape(format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")), "</p>",
      "<p><strong>Project root:</strong> ", html_escape(project_root), "</p>",
      "<p><strong>Report archive folder:</strong> ", html_escape(report_root), "</p></div>",
      "<h2>How to Recreate the Analysis</h2><pre>", html_escape(recreate), "</pre>",
      "<h2>Method Logic and Step Comments</h2>",
      "<div class='box'><p>The analysis is organized as a reproducible chain: raw image discovery and safe renaming, Fiji background preprocessing, ilastik pixel classification, Fiji mask-area quantification, validation-group selection, manual inspection, ROI/skeleton extraction, cutoff optimization, production application of selected cutoff profiles, and final statistical/visual interpretation.</p>",
      "<p>Each backend action is recorded in <code>attachments/workflow/step_commands.csv</code> and <code>attachments/logs/command_history.csv</code>. The copied scripts and macros in <code>attachments/scripts</code> are the executable implementation used by the app-controlled workflow.</p>",
      "<p>Saved publication figures and tables are treated as reproducible analysis objects. Figure candidates store the recorded plot, rendered PNG, editable style metadata, and app-input snapshot. Table candidates store the filtered/sorted CSV, table metadata, and app-input snapshot.</p>",
      "<p>The statistical interpretation guide is copied with the project documentation when scripts/docs are included in the report archive. It explains the image-by-profile row unit, PCA assumptions, contrast screening, grouped plots, covariate correlation checks, and adjusted ANCOVA logic.</p></div>",
      "<h2>Workflow Steps and Commands</h2>", html_table(steps, 80),
      "<h2>Output Readiness Snapshot</h2>", html_table(output_status, 80),
      "<h2>Software Versions</h2>", html_table(software, 80),
      "<h2>R Package Versions</h2>", html_table(packages, 120),
      "<h2>Validation Profiles</h2>", html_table(profiles, 80),
      "<h2>Publication Figure Candidates</h2>", html_table(publication_candidates_table(), 80),
      "<h2>Publication Table Candidates</h2>", html_table(publication_tables_table(), 80),
      "<h2>Recent Command History</h2>", html_table(utils::tail(hist, 80), 80),
      "<h2>Archive Manifest</h2>", html_table(manifest, 200),
      "</body></html>"
    )
    writeLines(html, report_html, useBytes = TRUE)

    archive_base <- paste0(report_root, ".tar.gz")
    old_wd <- getwd()
    on.exit(setwd(old_wd), add = TRUE)
    setwd(dirname(report_root))
    utils::tar(archive_base, files = basename(report_root), compression = "gzip", tar = "internal")
    list(root = normalizePath(report_root, winslash = "/", mustWork = FALSE), archive = normalizePath(archive_base, winslash = "/", mustWork = FALSE), manifest = manifest)
  }

  invisible(lapply(
    c(
      "segmentation_label_histogram",
      "cutoff_profile_effectiveness_plot",
      "cutoff_group_dot_plot",
      "cutoff_visual_roi_overlay_plot",
      "cutoff_review_score_plot",
      "cutoff_review_retention_plot",
      "generalization_grouped_plot",
      "generalization_facet_helper_plot",
      "covariate_correlation_heatmap",
      "dimred_pca_plot",
      "dimred_scree_plot",
      "dimred_separation_plot",
      "contrast_variable_plot"
    ),
    function(id) install_plot_download(id, id)
  ))

  about_tab_selected <- reactiveVal("1. Algorithmic Logic")
  workplace_tab_selected <- reactiveVal("1. Configuration")
  monitor_tab_selected <- reactiveVal("1. Live Log")
  validation_mode_selected <- reactiveVal("2.A. Validate New Cutoffs")
  validation_phase_selected <- reactiveVal("2.A.1. Setup and Preprocessing")
  cutoff_review_tab_selected <- reactiveVal("2.C.1. Cutoff Model Inspection")
  generalization_tab_selected <- reactiveVal("3.1. Metadata Inspection")
  runner <- reactiveValues(
    active = FALSE,
    process = NULL,
    step = NULL,
    runtime = NULL,
    command = NULL,
    started_at = NULL,
    log_file = NULL,
    run_id = NULL,
    last_size = 0,
    last_growth_at = NULL,
    task_size = NULL,
    missing_process_warned = FALSE
  )
  setup_runner <- reactiveValues(
    active = FALSE,
    remaining = character(),
    runtime = NULL
  )
  validation_runner <- reactiveValues(
    active = FALSE,
    remaining = character(),
    runtime = NULL,
    last_message = NULL,
    last_update = NULL
  )
  dir_browser <- reactiveValues(
    target = NULL,
    current = project_root,
    entries = character(),
    files = character(),
    breadcrumbs = character(),
    places = available_root_places(),
    mode = "dir",
    pattern = NULL
  )
  pending_step <- reactiveVal(NULL)
  pending_manual_apply <- reactiveVal(FALSE)
  manual_review_tick <- reactiveVal(0)
  manual_mark_busy <- reactiveVal(FALSE)
  label_mapping_tick <- reactiveVal(0)
  max_dir_buttons <- 150L
  max_file_buttons <- 150L
  all_step_ids <- c("rename", "img_list", "fiji", "ilastik", "quantify", "groups", "manual", "writeback", "roi_extract", "package_rois", "optimize", "generalize", "combine", "viz_roi", "viz_pca", "meta_pca", "final_plots")

  append_log <- function(...) {
    line <- paste0(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " | ", paste(..., collapse = " "))
    log_lines(c(log_lines(), line))
  }

  current_input_permission_diagnostics <- reactive({
    tick()
    p <- build_paths(settings())
    input_permission_diagnostics(p$input_master)
  })

  repair_input_permissions_now <- function() {
    p <- build_paths(settings())
    if (!dir.exists(p$input_master)) {
      showNotification(sprintf("Input directory does not exist: %s", p$input_master), type = "error", duration = 8)
      return(invisible(FALSE))
    }
    removeModal()
    cmd <- sprintf(
      "bash ./launchers/repair_input_permissions.sh %s",
      bash_quote(runtime_path_arg(p$input_master, input$runtime_choice %||% "local"))
    )
    withProgress(message = "Repairing input folder permissions", value = 0.2, {
      append_log("Input permission repair:", sprintf("Repairing %s", p$input_master))
      res <- run_runtime_command(cmd, input$runtime_choice %||% "local")
      append_log("Command:", res$command)
      if (nzchar(res$output)) append_log(res$output)
      append_log("Exit status:", res$status)
      tick(tick() + 1L)
      if (identical(as.integer(res$status), 0L)) {
        remaining <- input_permission_diagnostics(p$input_master)
        remaining_count <- length(remaining$untraversable_dirs %||% character()) + length(remaining$unreadable_files %||% character())
        if (remaining_count > 0) {
          showNotification("The repair ran, but some input folders/files still look inaccessible. Check the live log and the permission panel.", type = "warning", duration = 10)
          append_log("Input permission repair:", sprintf("Completed, but %s inaccessible item(s) are still detected.", remaining_count))
        } else {
          showNotification("Input folder permissions were repaired. Rerun the setup step or refresh checks.", type = "message", duration = 8)
          append_log("Input permission repair:", "Completed successfully; no inaccessible input folders/files remain detectable.")
        }
        TRUE
      } else {
        showNotification("Input permission repair failed. Check the live log for details.", type = "error", duration = 10)
        append_log("Input permission repair:", "Failed.")
        FALSE
      }
    })
  }

  restore_current_tabs <- function() {
    try(updateTabsetPanel(session, "about_tabs", selected = about_tab_selected()), silent = TRUE)
    try(updateTabsetPanel(session, "workplace_tabs", selected = workplace_tab_selected()), silent = TRUE)
    try(updateTabsetPanel(session, "monitor_tabs", selected = monitor_tab_selected()), silent = TRUE)
    try(updateTabsetPanel(session, "validation_mode_tabs", selected = validation_mode_selected()), silent = TRUE)
    try(updateTabsetPanel(session, "validation_phase_tabs", selected = validation_phase_selected()), silent = TRUE)
    try(updateTabsetPanel(session, "cutoff_review_tabs", selected = cutoff_review_tab_selected()), silent = TRUE)
    try(updateTabsetPanel(session, "generalization_tabs", selected = generalization_tab_selected()), silent = TRUE)
    invisible(NULL)
  }

  select_workspace_tabs <- function(workplace = NULL, validation_mode = NULL, validation_phase = NULL, cutoff_review = NULL, generalization = NULL) {
    if (!is.null(workplace) && nzchar(workplace)) {
      workplace_tab_selected(workplace)
      try(updateTabsetPanel(session, "workplace_tabs", selected = workplace), silent = TRUE)
    }
    if (!is.null(validation_mode) && nzchar(validation_mode)) {
      validation_mode_selected(validation_mode)
      try(updateTabsetPanel(session, "validation_mode_tabs", selected = validation_mode), silent = TRUE)
    }
    if (!is.null(validation_phase) && nzchar(validation_phase)) {
      validation_phase_selected(validation_phase)
      try(updateTabsetPanel(session, "validation_phase_tabs", selected = validation_phase), silent = TRUE)
    }
    if (!is.null(cutoff_review) && nzchar(cutoff_review)) {
      cutoff_review_tab_selected(cutoff_review)
      try(updateTabsetPanel(session, "cutoff_review_tabs", selected = cutoff_review), silent = TRUE)
    }
    if (!is.null(generalization) && nzchar(generalization)) {
      generalization_tab_selected(generalization)
      try(updateTabsetPanel(session, "generalization_tabs", selected = generalization), silent = TRUE)
    }
    invisible(NULL)
  }

  history_df <- reactive({
    tick()
    read_run_history()
  })

  current_steps <- reactive({
    step_specs(settings(), input)
  })

  safe_steps_for_ui <- function() {
    tryCatch(
      current_steps(),
      error = function(e) {
        runtime_choice <- isolate(input$runtime_choice %||% "posix")
        step_specs(settings(), list(runtime_choice = runtime_choice))
      }
    )
  }

  current_validation_group_dir <- reactive({
    groups <- validation_group_dirs(settings())
    sel <- input$manual_group_choice %||% names(groups)[1]
    groups[[sel]] %||% unname(groups[[1]])
  })

  current_validation_images <- reactive({
    validation_group_images(current_validation_group_dir())
  })

  current_validation_index <- reactive({
    imgs <- current_validation_images()
    if (!length(imgs)) return(0L)
    idx <- suppressWarnings(as.integer(input$manual_image_index %||% 1L))
    idx <- max(1L, min(length(imgs), idx))
    idx
  })

  current_validation_image <- reactive({
    imgs <- current_validation_images()
    idx <- current_validation_index()
    if (!length(imgs) || idx <= 0L) return(NULL)
    imgs[[idx]]
  })

  current_manual_view <- reactive({
    img <- current_validation_image()
    if (is.null(img)) return(NULL)
    manual_view_image_source(img)
  })

  current_label_mapping_settings <- reactive({
    s <- settings()
    s$SEG_LABEL_NEURITES <- as.character(input$seg_neurites_value %||% s$SEG_LABEL_NEURITES %||% "3")
    s$SEG_LABEL_CELL_BODIES <- as.character(input$seg_cell_bodies_value %||% s$SEG_LABEL_CELL_BODIES %||% "1")
    s$SEG_LABEL_BACKGROUND <- as.character(input$seg_background_value %||% s$SEG_LABEL_BACKGROUND %||% "255")
    s
  })

  current_segmentation_label_mask <- reactive({
    label_mapping_tick()
    files <- segmentation_mask_files(settings())
    if (!length(files)) return("")
    selected <- input$segmentation_label_mask_choice %||% ""
    if (nzchar(selected) && selected %in% files) return(selected)
    files[[1]]
  })

  step_complete_without_dropouts <- function(step_id) {
    step <- find_step_by_id(current_steps(), step_id)
    if (is.null(step)) return(FALSE)
    integrity <- step_integrity_check(step, settings())
    identical(integrity$state, "complete") && !length(integrity$dropouts)
  }

  start_setup_module_run <- function(runtime_choice) {
    ordered_ids <- setup_module_step_ids()
    first_needed <- which(!vapply(ordered_ids, step_complete_without_dropouts, logical(1)))[1]
    if (is.na(first_needed)) {
      showNotification("Setup already looks complete and has no detected dropouts.", type = "message")
      append_log("Setup module:", "All setup steps already look complete, so no new run was started.")
      return(invisible(NULL))
    }
    setup_runner$active <- TRUE
    setup_runner$remaining <- ordered_ids[first_needed:length(ordered_ids)]
    setup_runner$runtime <- runtime_choice
    first_step <- find_step_by_id(current_steps(), setup_runner$remaining[[1]])
    if (is.null(first_step)) {
      setup_runner$active <- FALSE
      setup_runner$remaining <- character()
      setup_runner$runtime <- NULL
      showNotification("The setup module could not resolve its first runnable step.", type = "error")
      return(invisible(NULL))
    }
    append_log("Setup module:", sprintf("Starting chained setup run from %s.", first_step$title))
    run_step_now(first_step, runtime_choice)
    invisible(NULL)
  }

  start_validation_post_manual_run <- function(runtime_choice) {
    ordered_ids <- c("writeback", "roi_extract", "package_rois")
    validation_runner$active <- TRUE
    validation_runner$remaining <- ordered_ids
    validation_runner$runtime <- runtime_choice
    validation_runner$last_message <- "Preparing post-manual validation chain."
    validation_runner$last_update <- Sys.time()
    first_step <- find_step_by_id(current_steps(), ordered_ids[[1]])
    if (is.null(first_step)) {
      validation_runner$active <- FALSE
      validation_runner$remaining <- character()
      validation_runner$runtime <- NULL
      validation_runner$last_message <- NULL
      validation_runner$last_update <- NULL
      showNotification("The post-manual validation chain could not resolve its first step.", type = "error")
      return(invisible(NULL))
    }
    append_log("Validation chain:", sprintf("Starting post-manual chain from %s.", first_step$title))
    validation_runner$last_message <- sprintf("Running %s.", first_step$title)
    validation_runner$last_update <- Sys.time()
    run_step_now(first_step, runtime_choice)
    invisible(NULL)
  }

  save_profile_now <- function(profile_name, runtime_choice) {
    req(nzchar(profile_name))
    original_profile_name <- sanitize_profile_name(profile_name)
    profile_name <- suggest_unique_profile_name(settings(), original_profile_name)
    if (!identical(profile_name, original_profile_name)) {
      append_log("Save profile:", sprintf("Profile name '%s' already exists. Saving as '%s' instead.", original_profile_name, profile_name))
      showNotification(sprintf("Profile name already exists. Saving as '%s' instead.", profile_name), type = "warning", duration = 8)
      try(updateTextInput(session, "opt_profile_name_inline", value = suggest_unique_profile_name(settings(), profile_name)), silent = TRUE)
      try(updateTextInput(session, "new_profile_name", value = suggest_unique_profile_name(settings(), profile_name)), silent = TRUE)
    }
    cmd <- sprintf("bash ./launchers/save_optimization_profile.sh %s", bash_quote(profile_name))
    withProgress(message = "Saving validated cutoff profile", value = 0.1, {
      started <- Sys.time()
      res <- run_runtime_command(cmd, runtime_choice)
      append_log("Save profile:", profile_name)
      append_log("Command:", res$command)
      if (nzchar(res$output)) append_log(res$output)
      append_run_history(list(
        run_id = new_run_id("save_profile"),
        category = "profile_action",
        step_id = "save_profile",
        title = paste("Save validation profile:", profile_name),
        runtime = runtime_choice,
        machine_signature = machine_info()$machine_signature,
        cpu_cores = machine_info()$cpu_cores,
        task_size = NA_real_,
        task_size_label = "profile_save",
        seconds_per_unit = NA_real_,
        command = cmd,
        started_at = format(started, "%Y-%m-%d %H:%M:%S"),
        finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        duration_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
        status = if (res$status == 0) "completed" else "failed",
        exit_status = as.integer(res$status),
        log_file = ""
      ))
      tick(tick() + 1)
      if (res$status == 0) {
        next_suggestion <- suggest_unique_profile_name(settings(), paste0(format(Sys.Date()), "_validated_cutoffs"))
        try(updateTextInput(session, "opt_profile_name_inline", value = next_suggestion), silent = TRUE)
        try(updateTextInput(session, "new_profile_name", value = next_suggestion), silent = TRUE)
        showNotification("Optimization profile saved.", type = "message")
      } else {
        showNotification("Profile save failed. Check the log.", type = "error")
      }
    })
  }

  stop_setup_module_run <- function(reason = NULL, type = "warning") {
    if (!isTRUE(setup_runner$active)) return(invisible(NULL))
    if (nzchar(reason %||% "")) {
      append_log("Setup module:", reason)
      showNotification(reason, type = type, duration = 8)
      restore_current_tabs()
    }
    setup_runner$active <- FALSE
    setup_runner$remaining <- character()
    setup_runner$runtime <- NULL
    invisible(NULL)
  }

  stop_validation_post_manual_run <- function(reason = NULL, type = "warning") {
    if (!isTRUE(validation_runner$active)) return(invisible(NULL))
    if (nzchar(reason %||% "")) {
      append_log("Validation chain:", reason)
      showNotification(reason, type = type, duration = 8)
      restore_current_tabs()
    }
    validation_runner$active <- FALSE
    validation_runner$remaining <- character()
    validation_runner$runtime <- NULL
    validation_runner$last_message <- NULL
    validation_runner$last_update <- NULL
    invisible(NULL)
  }

  handle_success_followup <- function(completed_step_id, runtime_choice) {
    if (identical(completed_step_id, "groups")) {
      select_workspace_tabs(
        workplace = "2. Validate New / Use Prior Cutoffs Model",
        validation_mode = "2.A. Validate New Cutoffs",
        validation_phase = "2.A.4. Manual Inspection"
      )
      append_log("Validation flow:", "Validation groups are ready. Switched to Manual Inspection.")
      showNotification("Validation groups are ready. Continue in the Manual Inspection subtab.", type = "message", duration = 8)
    }

    if (identical(completed_step_id, "optimize")) {
      profile_name <- input$opt_profile_name_inline %||% ""
      if (nzchar(profile_name)) {
        append_log("Optimization:", sprintf("Optimization completed. Saving optimization profile '%s'.", profile_name))
        save_profile_now(profile_name, runtime_choice)
        select_workspace_tabs(
          workplace = "2. Validate New / Use Prior Cutoffs Model",
          validation_mode = "2.C. Review Obtained/Selected Cutoffs",
          cutoff_review = "2.C.1. Cutoff Model Inspection"
        )
      }
    }

    if (isTRUE(setup_runner$active)) {
      if (!step_complete_without_dropouts(completed_step_id)) {
        stop_setup_module_run(
          sprintf("The setup module stopped after %s because the step does not look fully complete yet.", completed_step_id),
          type = "warning"
        )
        return(invisible(NULL))
      }

      remaining <- setup_runner$remaining %||% character()
      if (length(remaining) && identical(remaining[[1]], completed_step_id)) {
        remaining <- remaining[-1]
      }

      while (length(remaining) && step_complete_without_dropouts(remaining[[1]])) {
        skipped_step <- find_step_by_id(current_steps(), remaining[[1]])
        append_log("Setup module:", sprintf("Skipping %s because it already looks complete.", skipped_step$title))
        remaining <- remaining[-1]
      }

      setup_runner$remaining <- remaining

      if (!length(remaining)) {
        stop_setup_module_run("The full setup module finished successfully.", type = "message")
        return(invisible(NULL))
      }

      next_step <- find_step_by_id(current_steps(), remaining[[1]])
      if (is.null(next_step)) {
        stop_setup_module_run("The setup module could not resolve the next step to run.", type = "error")
        return(invisible(NULL))
      }

      append_log("Setup module:", sprintf("%s completed successfully, so %s will start next.", completed_step_id, next_step$title))
      showNotification(
        sprintf("%s completed. Starting %s automatically.", completed_step_id, next_step$title),
        type = "message",
        duration = 6
      )
      run_step_now(next_step, setup_runner$runtime %||% runtime_choice)
      return(invisible(NULL))
    }

    if (isTRUE(validation_runner$active)) {
      if (!step_complete_without_dropouts(completed_step_id)) {
        stop_validation_post_manual_run(
          sprintf("The post-manual validation chain stopped after %s because the step does not look fully complete yet.", completed_step_id),
          type = "warning"
        )
        return(invisible(NULL))
      }

      remaining <- validation_runner$remaining %||% character()
      if (length(remaining) && identical(remaining[[1]], completed_step_id)) {
        remaining <- remaining[-1]
      }
      validation_runner$remaining <- remaining
      if (!length(remaining)) {
        stop_validation_post_manual_run("Manual annotations were written back and the ROI packaging chain finished successfully.", type = "message")
        select_workspace_tabs(
          workplace = "2. Validate New / Use Prior Cutoffs Model",
          validation_mode = "2.A. Validate New Cutoffs",
          validation_phase = "2.A.6. Threshold Optimization"
        )
        showNotification("You can now run threshold optimization and generate an optimization profile.", type = "message", duration = 8)
        return(invisible(NULL))
      }
      next_step <- find_step_by_id(current_steps(), remaining[[1]])
      if (is.null(next_step)) {
        stop_validation_post_manual_run("The post-manual validation chain could not resolve the next step.", type = "error")
        return(invisible(NULL))
      }
      append_log("Validation chain:", sprintf("%s completed successfully, so %s will start next.", completed_step_id, next_step$title))
      validation_runner$last_message <- sprintf("%s completed. Starting %s.", completed_step_id, next_step$title)
      validation_runner$last_update <- Sys.time()
      showNotification(
        sprintf("%s completed. Starting %s automatically.", completed_step_id, next_step$title),
        type = "message",
        duration = 6
      )
      run_step_now(next_step, validation_runner$runtime %||% runtime_choice)
      return(invisible(NULL))
    }

    invisible(NULL)
  }

  finalize_run <- function(status_label, exit_status = NA_integer_) {
    if (is.null(runner$step)) return(invisible(NULL))
    completed_step <- runner$step
    completed_runtime <- runner$runtime
    finished_at <- Sys.time()
    duration_seconds <- as.numeric(difftime(finished_at, runner$started_at, units = "secs"))
    m <- machine_info()
    task <- task_size_for_step(runner$step$id, settings())
    append_run_history(list(
      run_id = runner$run_id,
      category = "pipeline_step",
      step_id = runner$step$id,
      title = runner$step$title,
      runtime = runner$runtime,
      machine_signature = m$machine_signature,
      cpu_cores = m$cpu_cores,
      task_size = task$value,
      task_size_label = task$label,
      seconds_per_unit = seconds_per_unit_value(duration_seconds, task$value),
      command = runner$command,
      started_at = format(runner$started_at, "%Y-%m-%d %H:%M:%S"),
      finished_at = format(finished_at, "%Y-%m-%d %H:%M:%S"),
      duration_seconds = duration_seconds,
      status = status_label,
      exit_status = as.integer(exit_status %||% NA_integer_),
      log_file = runner$log_file
    ))
    tick(tick() + 1)
    runner$active <- FALSE
    runner$process <- NULL
    runner$step <- NULL
    runner$runtime <- NULL
    runner$command <- NULL
    runner$started_at <- NULL
    runner$last_size <- 0
    runner$last_growth_at <- NULL
    runner$task_size <- NULL
    runner$missing_process_warned <- FALSE
    if (identical(status_label, "completed")) {
      handle_success_followup(completed_step$id, completed_runtime)
    } else if (isTRUE(setup_runner$active)) {
      stop_setup_module_run(sprintf("The setup module stopped because %s failed.", completed_step$title), type = "error")
    } else if (isTRUE(validation_runner$active)) {
      stop_validation_post_manual_run(sprintf("The post-manual validation chain stopped because %s failed.", completed_step$title), type = "error")
    }
  }

  prerequisite_next_step_ui <- function(current_step, prereq_issues) {
    stale_mask_percentages <- any(grepl("Mask percentages are older than the confirmed segmentation label mapping", prereq_issues, fixed = TRUE))
    input_permission_issue <- any(grepl("not accessible|untraversable|Permission denied|execute/search", prereq_issues, ignore.case = TRUE))
    tagList(
      tags$p("This step is blocked until the following required inputs or confirmations are available:"),
      tags$ul(lapply(prereq_issues, function(x) tags$li(tags$span(class = "path-block", x)))),
      if (stale_mask_percentages) tags$div(
        class = "step-summary-box step-summary-warn",
        tags$div(class = "step-summary-title", "What to do now"),
        tags$p("Your segmentation label mapping was changed or confirmed after the current mask-area table was created. The old table can still contain the old neurite/cell-body label counts, so the validation grouping plot would be misleading."),
        tags$p("Run 2.A.1.4. Quantify Mask Areas now. After that finishes successfully, rerun 2.A.3. Create Validation Groups.")
      ),
      if (input_permission_issue) tags$div(
        class = "step-summary-box step-summary-warn",
        tags$div(class = "step-summary-title", "What to do now"),
        tags$p("The app can repair the common direct-unzip permission problem for the selected input image tree. Click the repair button below, then rerun the blocked setup step.")
      )
    )
  }

  prerequisite_modal_footer <- function(current_step, prereq_issues) {
    stale_mask_percentages <- any(grepl("Mask percentages are older than the confirmed segmentation label mapping", prereq_issues, fixed = TRUE))
    input_permission_issue <- any(grepl("not accessible|untraversable|Permission denied|execute/search", prereq_issues, ignore.case = TRUE))
    if (stale_mask_percentages) {
      return(tagList(
        actionButton("run_quantify_from_prereq_btn", "Run 2.A.1.4. Quantify Mask Areas Now", class = "btn-primary"),
        modalButton("Close")
      ))
    }
    if (input_permission_issue) {
      return(tagList(
        actionButton("repair_input_permissions_from_modal_btn", "Repair Input Folder Permissions", class = "btn-warning"),
        modalButton("Close")
      ))
    }
    NULL
  }

  show_prerequisite_modal <- function(current_step, prereq_issues) {
    showModal(modalDialog(
      title = paste("Missing prerequisites for", current_step$title),
      easyClose = TRUE,
      footer = prerequisite_modal_footer(current_step, prereq_issues),
      modal_top_close(),
      prerequisite_next_step_ui(current_step, prereq_issues)
    ))
  }

  show_input_permission_failure_modal <- function(reason = NULL, suggestion = NULL) {
    p <- build_paths(settings())
    showModal(modalDialog(
      title = "Input Folder Permission Problem",
      easyClose = TRUE,
      footer = tagList(
        actionButton("repair_input_permissions_from_modal_btn", "Repair Input Folder Permissions", class = "btn-warning"),
        modalButton("Close")
      ),
      modal_top_close(),
      tags$div(
        class = "step-summary-box step-summary-warn",
        tags$div(class = "step-summary-title", "Why the step stopped"),
        tags$p(reason %||% "Some raw-image folders are visible but cannot be entered by the runtime, so recursive image discovery can be incomplete."),
        tags$p(suggestion %||% "Click the repair button below. The app will repair only the selected input image tree, then you can rerun Rename Images."),
        tags$p(tags$strong("Selected input image tree: "), tags$span(class = "path-block", p$input_master))
      )
    ))
  }

  run_step_now <- function(current_step, runtime_choice) {
    prereq_issues <- step_prerequisite_issues(current_step, settings())
    if (length(prereq_issues)) {
      append_log("Blocked step:", current_step$title)
      append_log(paste(prereq_issues, collapse = "\n"))
      show_prerequisite_modal(current_step, prereq_issues)
      if (isTRUE(setup_runner$active)) {
        stop_setup_module_run(sprintf("The setup module stopped before %s because a required prerequisite is missing.", current_step$title), type = "warning")
      } else if (isTRUE(validation_runner$active)) {
        stop_validation_post_manual_run(sprintf("The post-manual validation chain stopped before %s because a required prerequisite is missing.", current_step$title), type = "warning")
      }
      return(invisible(FALSE))
    }
    if (has_processx) {
      started <- start_async_run(current_step, runtime_choice)
      if (started) {
        showNotification(paste("Started:", current_step$title), type = "message")
      }
    } else {
      withProgress(message = paste("Running", current_step$title), value = 0.1, {
        res <- run_runtime_command(current_step$command, runtime_choice)
        append_log("Run step:", current_step$title)
        append_log("Command:", res$command)
        if (nzchar(res$output)) append_log(res$output)
        append_log("Exit status:", res$status)
        append_run_history(list(
          run_id = new_run_id(current_step$id),
          category = "pipeline_step",
          step_id = current_step$id,
          title = current_step$title,
          runtime = runtime_choice,
          machine_signature = machine_info()$machine_signature,
          cpu_cores = machine_info()$cpu_cores,
          task_size = task_size_for_step(current_step$id, settings())$value,
          task_size_label = task_size_for_step(current_step$id, settings())$label,
          seconds_per_unit = NA_real_,
          command = current_step$command,
          started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          duration_seconds = NA_real_,
          status = if (res$status == 0) "completed" else "failed",
          exit_status = as.integer(res$status),
          log_file = ""
        ))
        tick(tick() + 1)
        if (res$status == 0) {
          showNotification(paste("Completed:", current_step$title), type = "message")
          handle_success_followup(current_step$id, runtime_choice)
        } else {
          failure_info <- detect_failure_explanation(current_step, data.frame(status = "failed", log_file = "", stringsAsFactors = FALSE), settings(), output_text = res$output)
          showNotification(if (!is.null(failure_info)) failure_info$reason else paste("Failed:", current_step$title), type = "error", duration = 10)
        if (is_input_permission_failure_info(failure_info)) {
            show_input_permission_failure_modal(failure_info$reason, failure_info$suggestion)
          }
          restore_current_tabs()
          if (isTRUE(setup_runner$active)) {
            stop_setup_module_run(sprintf("The setup module stopped because %s failed.", current_step$title), type = "error")
          } else if (isTRUE(validation_runner$active)) {
            stop_validation_post_manual_run(sprintf("The post-manual validation chain stopped because %s failed.", current_step$title), type = "error")
          }
        }
      })
    }
  }

  regenerate_step_outputs <- function(current_step) {
    outputs <- unique(unlist(current_step$outputs))
    outputs <- outputs[nzchar(outputs)]
    for (path in outputs) delete_path_safely(path)
    append_log("Removed existing outputs before fresh regeneration for step:", current_step$title)
    tick(tick() + 1)
  }

  build_dropout_retry_step <- function(current_step, integrity, runtime_choice) {
    p <- build_paths(settings())
    retry_root <- file.path(run_log_dir, "dropout_retry")
    dir.create(retry_root, recursive = TRUE, showWarnings = FALSE)
    retry_tag <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", current_step$id)

    if (current_step$id == "rename") {
      input_root <- build_paths(settings())$input_master
      source_files <- if (dir.exists(input_root)) {
        source_files <- list.files(input_root, pattern = "\\.(tif|tiff|png|jpg|jpeg)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
        source_files <- normalizePath(source_files, winslash = "/", mustWork = FALSE)
        source_files <- source_files[!grepl("/(ilastik|software_reference|example_expected_outputs|training_images_raw|training_images_preprocessed)/", source_files, ignore.case = TRUE)]
        source_files[!grepl("(_RGavg|_mask|_mask_renorm)\\.", basename(source_files), ignore.case = TRUE)]
      } else {
        character()
      }
      expected_map <- stats::setNames(source_files, rename_expected_names(source_files, input_root))
      matched <- unname(expected_map[integrity$dropouts])
      matched <- matched[file.exists(matched)]
      if (!length(matched)) return(NULL)
      path_file <- file.path(retry_root, paste0(retry_tag, "_paths.txt"))
      writeLines(normalizePath(matched, winslash = "/", mustWork = FALSE), path_file, useBytes = TRUE)
      cmd <- sprintf(
        "PIPELINE_PATHS_FILE=%s bash ./0.Rename.sh %s",
        bash_quote(runtime_path_arg(path_file, runtime_choice)),
        bash_quote(runtime_path_arg(input_root, runtime_choice))
      )
      return(modifyList(current_step, list(
        title = paste(current_step$title, "(dropouts only)"),
        command = cmd
      )))
    }

    if (current_step$id == "img_list") {
      all_files <- list.files(p$pre_renamed, recursive = TRUE, full.names = TRUE)
      matched <- all_files[basename(all_files) %in% integrity$dropouts]
      if (!length(matched)) return(NULL)
      path_file <- file.path(retry_root, paste0(retry_tag, "_paths.txt"))
      writeLines(normalizePath(matched, winslash = "/", mustWork = FALSE), path_file, useBytes = TRUE)
      cmd <- sprintf(
        "PIPELINE_APPEND_ONLY=1 PIPELINE_PATHS_FILE=%s bash ./1.get_img_list.sh",
        bash_quote(runtime_path_arg(path_file, runtime_choice))
      )
      return(modifyList(current_step, list(
        title = paste(current_step$title, "(dropouts only)"),
        command = cmd
      )))
    }

    if (current_step$id == "fiji") {
      image_paths <- safe_read_lines(p$image_list)
      image_paths <- image_paths[basename(image_paths) %in% sub("_RGavg\\.tif$", ".tif", integrity$dropouts, ignore.case = TRUE)]
      if (!length(image_paths)) return(NULL)
      list_file <- file.path(retry_root, paste0(retry_tag, "_image_list.txt"))
      writeLines(image_paths, list_file, useBytes = TRUE)
      cmd <- sprintf(
        "PIPELINE_IMAGE_LIST_FILE=%s PIPELINE_OUT_CLEAR_DIR=%s bash ./2.RunFIJI_clean_bckgnd.sh",
        bash_quote(runtime_path_arg(list_file, runtime_choice)),
        bash_quote(runtime_path_arg(p$out_clear, runtime_choice))
      )
      return(modifyList(current_step, list(
        title = paste(current_step$title, "(dropouts only)"),
        command = cmd
      )))
    }

    if (current_step$id == "ilastik") {
      source_files <- integrity$dropouts
      source_files <- sub("_mask_renorm\\.tif$", ".tif", source_files, ignore.case = TRUE)
      source_files <- sub("_mask\\.tif$", ".tif", source_files, ignore.case = TRUE)
      source_files <- file.path(p$out_clear, source_files)
      source_files <- source_files[file.exists(source_files)]
      if (!length(source_files)) return(NULL)
      temp_input_dir <- file.path(retry_root, paste0(retry_tag, "_input"))
      dir.create(temp_input_dir, recursive = TRUE, showWarnings = FALSE)
      invisible(file.copy(source_files, temp_input_dir, overwrite = TRUE))
      cmd <- sprintf(
        "PIPELINE_INPUT_DIR_OVERRIDE=%s PIPELINE_OUTPUT_DIR_OVERRIDE=%s bash ./3.Run_ilastik_model.sh",
        bash_quote(runtime_path_arg(temp_input_dir, runtime_choice)),
        bash_quote(runtime_path_arg(p$out_seg, runtime_choice))
      )
      return(modifyList(current_step, list(
        title = paste(current_step$title, "(dropouts only)"),
        command = cmd
      )))
    }

    NULL
  }

  start_async_run <- function(step, runtime) {
    if (runner$active) {
      showNotification("Another step is already running. Please wait for it to finish.", type = "warning")
      return(FALSE)
    }
    ensure_run_log_store()
    run_id <- new_run_id(step$id)
    log_file <- file.path(run_log_dir, paste0(run_id, ".log"))
    cmd_info <- build_process_command(step$command, runtime)
    process <- processx::process$new(
      cmd_info$exe,
      cmd_info$args,
      stdout = log_file,
      stderr = log_file,
      cleanup = TRUE
    )
    runner$active <- TRUE
    runner$process <- process
    runner$step <- step
    runner$runtime <- runtime
    runner$command <- step$command
    runner$started_at <- Sys.time()
    runner$log_file <- log_file
    runner$run_id <- run_id
    runner$last_size <- 0
    runner$last_growth_at <- Sys.time()
    runner$task_size <- task_size_for_step(step$id, settings())
    runner$missing_process_warned <- FALSE
    append_log("Run step:", step$title)
    append_log("Command:", step$command)
    append_log("Live log file:", log_file)
    TRUE
  }

  log_has_completion_marker <- function(log_file) {
    if (is.null(log_file) || !file.exists(log_file)) return(FALSE)
    tail_text <- tryCatch(paste(utils::tail(readLines(log_file, warn = FALSE), 80), collapse = "\n"), error = function(e) "")
    if (!nzchar(tail_text)) return(FALSE)
    any(grepl(
      "\\[INFO\\][[:space:]]+Done\\.|Done\\. Results saved|All done\\.|Batch Analysis Complete|Batch macro execution finished|ROI extraction created|Saved CSV with indicators|Saved inspected plot",
      tail_text,
      ignore.case = TRUE
    ))
  }

  reconcile_completed_runner <- function(reason = "completion marker") {
    if (!isTRUE(runner$active) || is.null(runner$step)) return(FALSE)
    integrity <- step_integrity_check(runner$step, settings())
    if (!identical(integrity$state, "complete")) return(FALSE)
    append_log("Monitor recovery:", sprintf("Detected %s and complete outputs for %s; finalizing the run.", reason, runner$step$title))
    if (!is.null(runner$process)) {
      try(if (runner$process$is_alive()) runner$process$kill(), silent = TRUE)
    }
    showNotification(paste("Recovered completed run:", runner$step$title), type = "message", duration = 8)
    finalize_run("completed", 0L)
    TRUE
  }

  advance_validation_chain_from_outputs <- function(reason = "output readiness check") {
    if (!isTRUE(validation_runner$active) || isTRUE(runner$active)) return(FALSE)
    remaining <- validation_runner$remaining %||% character()
    if (!length(remaining)) {
      stop_validation_post_manual_run("Manual annotations were written back and the ROI packaging chain finished successfully.", type = "message")
      return(TRUE)
    }

    advanced <- FALSE
    while (length(remaining)) {
      next_id <- remaining[[1]]
      if (!step_complete_without_dropouts(next_id)) break
      completed_step <- find_step_by_id(current_steps(), next_id)
      completed_title <- if (!is.null(completed_step)) completed_step$title else next_id
      append_log("Validation chain recovery:", sprintf("%s already looks complete during %s.", completed_title, reason))
      remaining <- remaining[-1]
      validation_runner$remaining <- remaining
      advanced <- TRUE
    }

    if (!length(remaining)) {
      stop_validation_post_manual_run("Manual annotations were written back and the ROI packaging chain finished successfully.", type = "message")
      select_workspace_tabs(
        workplace = "2. Validate New / Use Prior Cutoffs Model",
        validation_mode = "2.A. Validate New Cutoffs",
        validation_phase = "2.A.6. Threshold Optimization"
      )
      showNotification("2.A.5 is complete. You can now run threshold optimization.", type = "message", duration = 8)
      return(TRUE)
    }

    next_step <- find_step_by_id(current_steps(), remaining[[1]])
    if (is.null(next_step)) {
      stop_validation_post_manual_run("The post-manual validation chain could not resolve the next step.", type = "error")
      return(TRUE)
    }

    if (advanced) {
      validation_runner$last_message <- sprintf("Continuing recovered chain with %s.", next_step$title)
      validation_runner$last_update <- Sys.time()
      append_log("Validation chain recovery:", sprintf("Continuing with %s.", next_step$title))
      run_step_now(next_step, validation_runner$runtime %||% input$runtime_choice)
      return(TRUE)
    }

    FALSE
  }

  settings <- reactive({
    tick()
    read_settings_file(settings_path)
  })

  browseable_dirs <- function(path) {
    if (!dir.exists(path)) return(character())
    dirs <- list.dirs(path, recursive = FALSE, full.names = TRUE)
    dirs[order(basename(dirs))]
  }

  open_dir_browser <- function(target_id, start_path = project_root) {
    dir_browser$target <- target_id
    dir_browser$mode <- "dir"
    dir_browser$pattern <- NULL
    start_resolved <- normalize_project_path(start_path %||% ".", project_root)
    if (!dir.exists(start_resolved)) {
      parent_candidate <- dirname(start_resolved)
      start_resolved <- if (dir.exists(parent_candidate)) parent_candidate else project_root
    }
    dir_browser$current <- start_resolved
    dir_browser$places <- available_root_places()
    showModal(modalDialog(
      title = "Browse folders",
      easyClose = TRUE,
      footer = tagList(
        actionButton("dir_browser_up", "Up one level"),
        actionButton("dir_browser_use_current", "Use this folder")
      ),
      modal_top_close(),
      uiOutput("dir_browser_ui")
    ))
  }

  open_file_browser <- function(target_id, start_path = project_root, pattern = "\\.(csv|tsv|txt)$") {
    dir_browser$target <- target_id
    dir_browser$mode <- "file"
    dir_browser$pattern <- pattern
    start_resolved <- normalize_project_path(start_path %||% ".", project_root)
    if (!dir.exists(start_resolved)) {
      parent_candidate <- dirname(start_resolved)
      start_resolved <- if (dir.exists(parent_candidate)) parent_candidate else project_root
    }
    dir_browser$current <- start_resolved
    dir_browser$places <- available_root_places()
    showModal(modalDialog(
      title = "Browse files",
      easyClose = TRUE,
      footer = tagList(
        actionButton("dir_browser_up", "Up one level")
      ),
      modal_top_close(),
      uiOutput("dir_browser_ui")
    ))
  }

  observe({
    s <- settings()
    p <- build_paths(s)
    updateTextInput(session, "project_root_input", value = s$PROJECT_ROOT %||% ".")
    updateTextInput(session, "output_root_input", value = s$OUTPUT_ROOT_DIR_NAME %||% "pipeline_outputs")
    updateTextInput(session, "input_master_input", value = relative_to_project(effective_input_master_dir(s)))
    updateTextInput(session, "pre_renamed_input", value = s$PRE_RENAMED_DIR_NAME %||% "PRE_renamed")
    updateTextInput(session, "image_list_name_input", value = s$IMAGE_INPUT_LIST_NAME %||% "OUT_img_input1.txt")
    updateTextInput(session, "out_optimize_input", value = s$OUT_OPTIMIZE_DIR_NAME %||% "OUT_optimize")
    updateTextInput(session, "metadata_file_input", value = relative_to_project(effective_metadata_file(s)))
    updateTextInput(session, "fiji_bin_input", value = s$FIJI_BIN %||% "")
    updateTextInput(session, "ilastik_bin_input", value = s$ILASTIK_BIN %||% "")
    updateTextInput(session, "ilastik_project_input", value = relative_to_project(effective_ilastik_project_file(s)))
    updateTextInput(session, "validation_dir_input", value = if (nzchar(s$VALIDATION_PROFILE_DIR_NAME %||% "")) relative_to_project(normalize_project_path(s$VALIDATION_PROFILE_DIR_NAME)) else "cache/validation_profiles")
    updateTextInput(session, "rename_search_root", value = relative_to_project(effective_input_master_dir(s)))
    updateTextInput(session, "image_list_source", value = relative_to_project(p$pre_renamed))
    updateSelectInput(session, "seg_neurites_value", selected = s$SEG_LABEL_NEURITES %||% "3")
    updateSelectInput(session, "seg_cell_bodies_value", selected = s$SEG_LABEL_CELL_BODIES %||% "1")
    updateSelectInput(session, "seg_background_value", selected = s$SEG_LABEL_BACKGROUND %||% "255")
  })

  observeEvent(input$save_settings_btn, {
    current <- settings()
    output_root_value <- input$output_root_input %||% "pipeline_outputs"
    validation_dir_value <- input$validation_dir_input %||% "cache/validation_profiles"
    new_values <- modifyList(current, list(
      PROJECT_ROOT = if (normalize_project_path(input$project_root_input %||% ".", project_root) == project_root) "." else input$project_root_input,
      OUTPUT_ROOT_DIR_NAME = relative_to_project(normalize_project_path(output_root_value, project_root)),
      INPUT_WORKSPACE_DIR_NAME = current$INPUT_WORKSPACE_DIR_NAME %||% "pipeline_inputs",
      INPUT_MASTER_DIR = if (normalize_project_path(input$input_master_input %||% ".", project_root) == project_root) "." else relative_to_project(normalize_project_path(input$input_master_input %||% ".", project_root)),
      PRE_RENAMED_DIR_NAME = input$pre_renamed_input,
      IMAGE_INPUT_LIST_NAME = input$image_list_name_input,
      OUT_OPTIMIZE_DIR_NAME = input$out_optimize_input,
      SAMPLE_METADATA_FILE = relative_to_project(normalize_project_path(input$metadata_file_input %||% "pipeline_inputs/IN_sample_metadata.csv", project_root)),
      FIJI_BIN = input$fiji_bin_input,
      ILASTIK_BIN = input$ilastik_bin_input,
      ILASTIK_PROJECT_FILE = if (nzchar(input$ilastik_project_input %||% "")) relative_to_project(normalize_project_path(input$ilastik_project_input, project_root)) else "",
      SEG_LABEL_NEURITES = as.character(input$seg_neurites_value %||% current$SEG_LABEL_NEURITES %||% "3"),
      SEG_LABEL_CELL_BODIES = as.character(input$seg_cell_bodies_value %||% current$SEG_LABEL_CELL_BODIES %||% "1"),
      SEG_LABEL_BACKGROUND = as.character(input$seg_background_value %||% current$SEG_LABEL_BACKGROUND %||% "255"),
      OPTIMIZATION_SCORING_MODE = input$optimization_scoring_mode_inline %||% current$OPTIMIZATION_SCORING_MODE %||% "continuity_aware",
      VALIDATION_PROFILE_DIR_NAME = relative_to_project(normalize_project_path(validation_dir_value, project_root))
    ))
    old_labels <- as.character(c(
      current$SEG_LABEL_NEURITES %||% "3",
      current$SEG_LABEL_CELL_BODIES %||% "1",
      current$SEG_LABEL_BACKGROUND %||% "255"
    ))
    new_labels <- as.character(c(
      new_values$SEG_LABEL_NEURITES %||% "3",
      new_values$SEG_LABEL_CELL_BODIES %||% "1",
      new_values$SEG_LABEL_BACKGROUND %||% "255"
    ))
    if (!identical(old_labels, new_labels)) {
      new_values$SEG_LABEL_MAPPING_CONFIRMED <- "false"
      new_values$SEG_LABEL_MAPPING_CONFIRMED_AT <- ""
    }
    write_settings_file(settings_path, new_values)
    dir.create(normalize_project_path(new_values$INPUT_WORKSPACE_DIR_NAME %||% "pipeline_inputs", project_root), recursive = TRUE, showWarnings = FALSE)
    tick(tick() + 1)
    append_log("Saved shared settings.")
    showNotification("Settings saved.", type = "message")
  })

  output$input_permission_status_ui <- renderUI({
    p <- build_paths(settings())
    diagnostics <- current_input_permission_diagnostics()
    bad_dirs <- diagnostics$untraversable_dirs %||% character()
    bad_files <- diagnostics$unreadable_files %||% character()
    image_count <- count_raw_image_files(p$input_master)
    if (length(bad_dirs) || length(bad_files)) {
      return(tags$div(
        class = "step-summary-box step-summary-warn",
        tags$div(class = "step-summary-title", "Input permission warning"),
        tags$p(sprintf("The app can see %s raw image file(s), but some folders/files under the selected input tree are not accessible.", image_count)),
        if (length(bad_dirs)) tags$p(tags$strong("Example folder: "), tags$span(class = "path-block", bad_dirs[[1]])),
        if (length(bad_files)) tags$p(tags$strong("Example file: "), tags$span(class = "path-block", bad_files[[1]])),
        tags$p("Click the repair button below to make the selected input image tree readable and traversable.")
      ))
    }
    tags$div(
      class = "step-summary-box step-summary-good",
      tags$div(class = "step-summary-title", "Input permission check"),
      tags$p(sprintf("No inaccessible input folders/files detected. Current raw-image count estimate: %s.", image_count))
    )
  })

  output$input_permission_callout_ui <- renderUI({
    p <- build_paths(settings())
    diagnostics <- current_input_permission_diagnostics()
    bad_dirs <- diagnostics$untraversable_dirs %||% character()
    bad_files <- diagnostics$unreadable_files %||% character()
    image_count <- count_raw_image_files(p$input_master)
    issue_detected <- length(bad_dirs) || length(bad_files)
    tags$div(
      class = paste("manual-panel", if (issue_detected) "step-summary-warn" else "step-summary-neutral"),
      tags$h3("Zenodo/Input Folder Permission Check"),
      tags$p("Use this first if the app reports too few images, the Rename Images step fails with Permission denied, or the Zenodo archive was unzipped on Linux/WSL."),
      tags$p(tags$strong("Effective raw-image folder: "), tags$span(class = "path-block", p$input_master)),
      tags$p(tags$strong("Current accessible raw-image count estimate: "), image_count),
      if (issue_detected) tags$div(
        tags$p(class = "warning-text", tags$strong("Problem detected: "), "Some input folders/files are visible but cannot be entered/read. Recursive image discovery may therefore be incomplete."),
        if (length(bad_dirs)) tags$p(tags$strong("Example blocked folder: "), tags$span(class = "path-block", bad_dirs[[1]])),
        if (length(bad_files)) tags$p(tags$strong("Example blocked file: "), tags$span(class = "path-block", bad_files[[1]])),
        tags$p("Click the repair button below. The app will repair only this input image tree, then you can rerun 2.A.1.0. Rename Images.")
      ) else tags$p("No inaccessible input folders/files are currently detected by the app."),
      actionButton(
        "repair_input_permissions_callout_btn",
        if (issue_detected) "Repair Input Folder Permissions Now" else "Recheck/Repair Input Folder Permissions",
        class = if (issue_detected) "btn btn-warning btn-lg" else "btn btn-default"
      ),
      tags$p(class = "helper-text", "This browser button performs the permission repair; terminal commands are only a fallback for advanced troubleshooting.")
    )
  })

  observeEvent(input$repair_input_permissions_btn, {
    repair_input_permissions_now()
  }, ignoreInit = TRUE)

  observeEvent(input$repair_input_permissions_top_btn, {
    repair_input_permissions_now()
  }, ignoreInit = TRUE)

  observeEvent(input$refresh_input_permission_status_top_btn, {
    tick(tick() + 1L)
    showNotification("Input permission status refreshed.", type = "message", duration = 5)
  }, ignoreInit = TRUE)

  observeEvent(input$repair_input_permissions_callout_btn, {
    repair_input_permissions_now()
  }, ignoreInit = TRUE)

  observeEvent(input$repair_input_permissions_from_modal_btn, {
    repair_input_permissions_now()
  }, ignoreInit = TRUE)

  observeEvent(input$repair_input_permissions_from_summary_btn, {
    repair_input_permissions_now()
  }, ignoreInit = TRUE)

  persist_optimization_scoring_mode <- function(selected_mode = NULL, announce = FALSE) {
    current <- settings()
    selected_mode <- selected_mode %||% input$optimization_scoring_mode_inline %||% current$OPTIMIZATION_SCORING_MODE %||% "continuity_aware"
    if (!identical(current$OPTIMIZATION_SCORING_MODE %||% "continuity_aware", selected_mode)) {
      write_settings_file(settings_path, modifyList(current, list(OPTIMIZATION_SCORING_MODE = selected_mode)))
      if (isTRUE(announce)) append_log("Optimization scoring mode:", selected_mode)
      tick(tick() + 1L)
    }
    invisible(selected_mode)
  }

  observeEvent(input$optimization_scoring_mode_inline, {
    persist_optimization_scoring_mode(input$optimization_scoring_mode_inline, announce = FALSE)
  }, ignoreInit = TRUE)

  output$segmentation_label_mask_selector_ui <- renderUI({
    files <- segmentation_mask_files(settings())
    if (!length(files)) {
      return(tags$div(class = "step-summary-box step-summary-missing", "No segmentation masks were found yet. Run 2.A.1.3. Run Ilastik Segmentation first."))
    }
    selected <- current_segmentation_label_mask()
    selectInput(
      "segmentation_label_mask_choice",
      "Segmentation Mask to Inspect",
      choices = stats::setNames(files, basename(files)),
      selected = selected
    )
  })

  observeEvent(input$segmentation_label_random_btn, {
    files <- segmentation_mask_files(settings())
    if (!length(files)) {
      showNotification("No segmentation masks are available yet.", type = "warning")
      return()
    }
    updateSelectInput(session, "segmentation_label_mask_choice", selected = sample(files, 1))
    label_mapping_tick(label_mapping_tick() + 1)
  }, ignoreInit = TRUE)

  observeEvent(input$refresh_label_mapping_btn, {
    label_mapping_tick(label_mapping_tick() + 1)
    showNotification("Segmentation label check refreshed.", type = "message")
  }, ignoreInit = TRUE)

  output$segmentation_label_peak_table <- renderTable({
    path <- current_segmentation_label_mask()
    if (!nzchar(path) || !file.exists(path)) return(data.frame(message = "No mask selected."))
    info <- mask_histogram_info(path)
    if (!isTRUE(info$ok)) return(data.frame(message = info$message))
    data.frame(
      pixel_value = info$peaks,
      pixel_count = info$counts[info$peaks + 1L],
      percent = round(100 * info$counts[info$peaks + 1L] / sum(info$counts), 4),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$segmentation_label_histogram <- renderPlot({
    on.exit(remember_plot("segmentation_label_histogram"), add = TRUE)
    path <- current_segmentation_label_mask()
    req(nzchar(path), file.exists(path))
    info <- mask_histogram_info(path)
    validate(need(isTRUE(info$ok), info$message))
    counts <- info$counts
    plot(
      0:255,
      counts,
      type = "h",
      xlab = "Pixel value",
      ylab = "Pixel count",
      main = "Segmentation Mask Pixel Histogram",
      col = ifelse(0:255 %in% info$peaks, "#173f35", "#d8d0c5")
    )
    labels <- seg_label_values(current_label_mapping_settings())
    abline(v = labels[["neurites"]], col = "red", lwd = 2)
    abline(v = labels[["cell_bodies"]], col = "blue", lwd = 2)
    abline(v = labels[["background"]], col = "goldenrod", lwd = 2)
    legend(
      "topright",
      legend = c("Neurites", "Cell bodies", "Background"),
      col = c("red", "blue", "goldenrod"),
      lwd = 2,
      bty = "n"
    )
  }, height = 260)

  output$segmentation_label_status_ui <- renderUI({
    path <- current_segmentation_label_mask()
    if (!nzchar(path) || !file.exists(path)) return(NULL)
    info <- mask_histogram_info(path)
    if (!isTRUE(info$ok)) {
      return(tags$div(class = "step-summary-box step-summary-missing", tags$div(class = "step-summary-title", "Label Mapping Check"), info$message))
    }
    status <- label_mapping_status(info$peaks, current_label_mapping_settings())
    confirmed <- identical(tolower(settings()$SEG_LABEL_MAPPING_CONFIRMED %||% "false"), "true")
    tags$div(
      class = paste("step-summary-box", status$class),
      tags$div(class = "step-summary-title", "Label Mapping Check"),
      tags$p(status$message),
      tags$p(sprintf("Confirmed in settings: %s", if (confirmed) paste("yes", settings()$SEG_LABEL_MAPPING_CONFIRMED_AT %||% "") else "no"))
    )
  })

  output$segmentation_label_assignment_ui <- renderUI({
    path <- current_segmentation_label_mask()
    s <- settings()
    default_choices <- as.character(sort(unique(c(0, 1, 3, 7, 255))))
    choices <- default_choices
    if (nzchar(path) && file.exists(path)) {
      info <- mask_histogram_info(path)
      if (isTRUE(info$ok) && length(info$peaks)) choices <- as.character(info$peaks)
    }
    tags$div(
      class = "manual-panel",
      label_with_help("Assign Detected Peaks to Classes", "segmentation_label_mapping"),
      tags$p("Choose which detected pixel value corresponds to each biological class. The colored preview uses these assignments immediately. If the defaults already look correct, still save them here once: that records that you checked the channels and allows the app to rebuild downstream mask percentages safely."),
      selectInput("seg_neurites_value", "Neurites label value (red)", choices = choices, selected = as.character(s$SEG_LABEL_NEURITES %||% "3")),
      selectInput("seg_cell_bodies_value", "Cell bodies label value (blue)", choices = choices, selected = as.character(s$SEG_LABEL_CELL_BODIES %||% "1")),
      selectInput("seg_background_value", "Background label value (yellow)", choices = choices, selected = as.character(s$SEG_LABEL_BACKGROUND %||% "255")),
      actionButton("confirm_seg_label_mapping_btn", "Use These Labels for Quantification", class = "module-run-button")
    )
  })

  output$segmentation_label_preview <- renderImage({
    path <- current_segmentation_label_mask()
    req(nzchar(path), file.exists(path))
    preview <- write_label_mapping_preview(path, current_label_mapping_settings())
    validate(need(isTRUE(preview$ok), preview$message))
    list(src = preview$path, contentType = "image/png", alt = "Colored segmentation label preview")
  }, deleteFile = FALSE)

  observeEvent(input$confirm_seg_label_mapping_btn, {
    path <- current_segmentation_label_mask()
    if (!nzchar(path) || !file.exists(path)) {
      showNotification("Choose a segmentation mask before confirming label mapping.", type = "error")
      return()
    }
    info <- mask_histogram_info(path)
    if (!isTRUE(info$ok)) {
      showNotification(info$message, type = "error", duration = 10)
      return()
    }
    labels <- seg_label_values(current_label_mapping_settings())
    if (length(info$peaks) != 3L) {
      showNotification(sprintf("Cannot confirm yet: expected exactly 3 mask peaks, but detected %s.", length(info$peaks)), type = "error", duration = 10)
      return()
    }
    if (length(unique(as.integer(labels))) != 3L || any(!as.integer(labels) %in% info$peaks)) {
      showNotification("Cannot confirm yet: assign neurites, cell bodies, and background to three different detected peak values.", type = "error", duration = 10)
      return()
    }
    current <- settings()
    new_values <- modifyList(current, list(
      SEG_LABEL_NEURITES = as.character(labels[["neurites"]]),
      SEG_LABEL_CELL_BODIES = as.character(labels[["cell_bodies"]]),
      SEG_LABEL_BACKGROUND = as.character(labels[["background"]]),
      SEG_LABEL_MAPPING_CONFIRMED = "true",
      SEG_LABEL_MAPPING_CONFIRMED_AT = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    ))
    write_settings_file(settings_path, new_values)
    tick(tick() + 1)
    showModal(modalDialog(
      title = "Label Mapping Saved",
      easyClose = TRUE,
      modal_top_close(),
      tags$p("The label mapping was saved. Because mask percentages are generated from these labels, the safe next step is to rerun the area quantification table before creating validation groups."),
      tags$p(tags$strong("Next step: "), "click the button below to run 2.A.1.4. Quantify Mask Areas now, then rerun 2.A.3. Create Validation Groups."),
      footer = tagList(
        actionButton("run_quantify_from_prereq_btn", "Run 2.A.1.4. Quantify Mask Areas Now", class = "btn-primary"),
        modalButton("Close")
      )
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$refresh_checks_btn, tick(tick() + 1))

  observe({
    groups <- validation_group_map()
    updateSelectInput(
      session,
      "manual_group_choice",
      choices = stats::setNames(names(groups), unname(groups)),
      selected = input$manual_group_choice %||% names(groups)[1]
    )
  })

  observeEvent(input$manual_group_choice, {
    updateNumericInput(session, "manual_image_index", value = 1)
  }, ignoreInit = TRUE)

  observeEvent(input$manual_prev_image, {
    idx <- current_validation_index()
    if (idx > 1L) updateNumericInput(session, "manual_image_index", value = idx - 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$manual_next_image, {
    idx <- current_validation_index()
    imgs <- current_validation_images()
    if (idx < length(imgs)) updateNumericInput(session, "manual_image_index", value = idx + 1L)
  }, ignoreInit = TRUE)

  observeEvent(input$manual_mark_choice, {
    if (isTRUE(manual_mark_busy())) return()
    manual_mark_busy(TRUE)
    on.exit(manual_mark_busy(FALSE), add = TRUE)
    img <- current_validation_image()
    req(img)
    res <- set_manual_label(current_validation_group_dir(), img, "choice")
    if (isTRUE(res$ok)) {
      manual_review_tick(manual_review_tick() + 1)
      showNotification("Marked as choice.", type = "message")
      imgs <- current_validation_images()
      idx <- current_validation_index()
      if (idx < length(imgs)) updateNumericInput(session, "manual_image_index", value = idx + 1L)
    } else {
      showNotification(res$message %||% "Could not save this choice label.", type = "error", duration = 8)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$manual_mark_noise, {
    if (isTRUE(manual_mark_busy())) return()
    manual_mark_busy(TRUE)
    on.exit(manual_mark_busy(FALSE), add = TRUE)
    img <- current_validation_image()
    req(img)
    res <- set_manual_label(current_validation_group_dir(), img, "noise")
    if (isTRUE(res$ok)) {
      manual_review_tick(manual_review_tick() + 1)
      showNotification("Marked as noise.", type = "message")
      imgs <- current_validation_images()
      idx <- current_validation_index()
      if (idx < length(imgs)) updateNumericInput(session, "manual_image_index", value = idx + 1L)
    } else {
      showNotification(res$message %||% "Could not save this noise label.", type = "error", duration = 8)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$manual_mark_not_sure, {
    if (isTRUE(manual_mark_busy())) return()
    manual_mark_busy(TRUE)
    on.exit(manual_mark_busy(FALSE), add = TRUE)
    img <- current_validation_image()
    req(img)
    res <- set_manual_label(current_validation_group_dir(), img, "not_sure")
    if (isTRUE(res$ok)) {
      manual_review_tick(manual_review_tick() + 1)
      showNotification("Marked as not so certain.", type = "message")
      imgs <- current_validation_images()
      idx <- current_validation_index()
      if (idx < length(imgs)) updateNumericInput(session, "manual_image_index", value = idx + 1L)
    } else {
      showNotification(res$message %||% "Could not save this not-sure marker.", type = "error", duration = 8)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$manual_clear_label, {
    if (isTRUE(manual_mark_busy())) return()
    manual_mark_busy(TRUE)
    on.exit(manual_mark_busy(FALSE), add = TRUE)
    img <- current_validation_image()
    req(img)
    res <- set_manual_label(current_validation_group_dir(), img, "clear")
    if (isTRUE(res$ok)) {
      manual_review_tick(manual_review_tick() + 1)
      showNotification("Cleared manual label for this image.", type = "message")
    } else {
      showNotification(res$message %||% "Could not clear this manual label.", type = "error", duration = 8)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$manual_finish_and_continue, {
    pending_manual_apply(TRUE)
    showModal(modalDialog(
      title = "Recap manual decisions before applying them to the table",
      easyClose = TRUE,
      size = "l",
      modal_top_close(),
      tags$p("Review the current manual categorization summary below. If you want to change any decision, close this recap and return to the Manual Inspection tab. Nothing will be written back into the CSV until you confirm."),
      tableOutput("manual_recap_table"),
      footer = tagList(
        modalButton("Go back and revise labels"),
        actionButton("manual_confirm_apply_btn", "Confirm recap and continue", class = "btn-primary")
      )
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$manual_confirm_apply_btn, {
    removeModal()
    if (isTRUE(runner$active)) {
      showNotification("Another step is already running. Please wait for it to finish.", type = "warning")
      pending_manual_apply(FALSE)
      return()
    }
    pending_manual_apply(FALSE)
    select_workspace_tabs(
      workplace = "2. Validate New / Use Prior Cutoffs Model",
      validation_mode = "2.A. Validate New Cutoffs",
      validation_phase = "2.A.5. Apply Manual Labels and Package ROIs"
    )
    start_validation_post_manual_run(input$runtime_choice)
  }, ignoreInit = TRUE)

  observeEvent(input$run_post_manual_module_btn, {
    if (isTRUE(runner$active)) {
      showNotification("Another step is already running. Please wait for it to finish.", type = "warning")
      return()
    }
    select_workspace_tabs(
      workplace = "2. Validate New / Use Prior Cutoffs Model",
      validation_mode = "2.A. Validate New Cutoffs",
      validation_phase = "2.A.5. Apply Manual Labels and Package ROIs"
    )
    start_validation_post_manual_run(input$runtime_choice)
  }, ignoreInit = TRUE)

  observeEvent(input$run_setup_module_btn, {
    if (isTRUE(runner$active)) {
      showNotification("Another step is already running. Please wait for it to finish.", type = "warning")
      return()
    }
    start_setup_module_run(input$runtime_choice)
  }, ignoreInit = TRUE)

  all_help <- c(settings_help, app_help)

  for (help_id in names(all_help)) {
    local({
      id <- help_id
      observeEvent(input[[paste0("help_", id)]], {
        item <- all_help[[id]]
        showModal(modalDialog(
          title = item$title,
          easyClose = TRUE,
          footer = NULL,
          modal_top_close(),
          tags$p(item$body)
        ))
      }, ignoreInit = TRUE)
    })
  }

  output$report_profile_selector_ui <- renderUI({
    prof <- list_validation_profiles(settings())
    if (!nrow(prof)) {
      return(tags$p(class = "warning-text", "No saved validation profiles were detected yet. The report can still include current working outputs and logs."))
    }
    choices <- stats::setNames(prof$profile_name, paste0(prof$profile_name, " (", prof$source, ifelse(nzchar(prof$created_at), paste0(", ", prof$created_at), ""), ")"))
    selectizeInput(
      "report_profile_choices",
      "Saved cutoff profiles to attach",
      choices = choices,
      selected = head(prof$profile_name, 3),
      multiple = TRUE,
      options = list(plugins = list("remove_button"), placeholder = "Choose saved profiles to include")
    )
  })

  observeEvent(input$generate_report_archive_btn, {
    withProgress(message = "Creating reproducible report archive", value = 0.1, {
      incProgress(0.2, detail = "Collecting settings, logs, outputs, and scripts")
      res <- tryCatch(make_report_archive(), error = function(e) list(error = conditionMessage(e)))
      if (!is.null(res$error)) {
        showNotification(paste("Report generation failed:", res$error), type = "error", duration = 12)
        return()
      }
      incProgress(0.8, detail = "Archive written")
      report_archive_path(res$archive)
      report_manifest_df(res$manifest)
      append_log("Report archive:", res$archive)
      showNotification("Reproducible report archive created.", type = "message")
    })
  }, ignoreInit = TRUE)

  output$report_archive_status_ui <- renderUI({
    archive <- report_archive_path()
    manifest <- report_manifest_df()
    if (!nzchar(archive %||% "") || !file.exists(archive)) {
      return(tags$div(
        class = "step-summary-box step-summary-neutral",
        tags$strong("No report archive generated yet"),
        tags$p("Click Generate Report Archive. The app will create a timestamped folder under cache/analysis_reports and a downloadable .tar.gz archive.")
      ))
    }
    total_bytes <- if (nrow(manifest) && "bytes" %in% names(manifest)) sum(suppressWarnings(as.numeric(manifest$bytes)), na.rm = TRUE) else file.info(archive)$size
    tags$div(
      class = "step-summary-box step-summary-good",
      tags$strong("Report archive ready"),
      tags$p(tags$strong("Archive: "), tags$span(class = "path-block", archive)),
      tags$p(tags$strong("Approximate attached bytes: "), format(total_bytes, big.mark = ",")),
      tags$p("Download the archive and keep it together with the original raw image source if large image outputs were not included.")
    )
  })

  output$report_manifest_preview_table <- renderTable({
    manifest <- report_manifest_df()
    if (!is.data.frame(manifest) || !nrow(manifest)) {
      return(data.frame(Message = "Generate a report archive to preview its contents."))
    }
    out <- manifest[, intersect(c("role", "source_path", "archive_path", "bytes"), names(manifest)), drop = FALSE]
    head(out, 40)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$report_recreation_notes_ui <- renderUI({
    tags$div(
      class = "step-summary-box step-summary-neutral",
      tags$p("The HTML report is stored inside the archive as report.html. The plain-text recreation guide is stored as RECREATE_ANALYSIS.txt."),
      tags$p("For full recreation, the most important attachments are: settings/config, software and package versions, step_commands.csv, command_history.csv, validation profile metadata, and the copied active scripts/macros."),
      tags$p("If you leave large outputs unchecked, the archive documents how to recreate them rather than duplicating every image, mask, and ROI file.")
    )
  })

  output$download_report_archive_btn <- downloadHandler(
    filename = function() {
      archive <- report_archive_path()
      if (nzchar(archive %||% "") && file.exists(archive)) basename(archive) else paste0("neurite_analysis_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".tar.gz")
    },
    content = function(file) {
      archive <- report_archive_path()
      if (!nzchar(archive %||% "") || !file.exists(archive)) {
        res <- make_report_archive()
        archive <- res$archive
        report_archive_path(archive)
        report_manifest_df(res$manifest)
      }
      file.copy(archive, file, overwrite = TRUE)
    }
  )

  output$dir_browser_ui <- renderUI({
    current <- dir_browser$current %||% project_root
    dirs <- browseable_dirs(current)
    files <- if (identical(dir_browser$mode, "file")) browseable_files(current, dir_browser$pattern) else character()
    current_norm <- normalizePath(current, winslash = "/", mustWork = FALSE)
    crumb_paths <- current_norm
    crumb_labels <- basename(current_norm)
    while (length(crumb_paths) < 30L) {
      parent <- dirname(crumb_paths[[1]])
      if (identical(parent, crumb_paths[[1]])) break
      crumb_paths <- c(parent, crumb_paths)
      crumb_labels <- c(if (.Platform$OS.type == "windows" && grepl("^[A-Za-z]:/$", parent)) parent else basename(parent), crumb_labels)
      if (length(crumb_paths) >= 30L) break
    }
    if (!length(crumb_labels) || !nzchar(crumb_labels[[1]])) crumb_labels[[1]] <- if (.Platform$OS.type == "windows") crumb_paths[[1]] else "/"
    dir_browser$entries <- dirs
    dir_browser$files <- files
    dir_browser$breadcrumbs <- crumb_paths
    tagList(
      tags$p(tags$strong("Current folder: "), current),
      tags$p(if (identical(dir_browser$mode, "file")) "Use the quick places, breadcrumbs, folder list, or file list below. You do not need to paste a full path first." else "Use the quick places, breadcrumbs, or folder list below. You do not need to paste a full path first."),
      tags$div(
        style = "display:flex; flex-wrap:wrap; gap:6px; margin-bottom:10px;",
        lapply(seq_along(dir_browser$places), function(i) {
          nm <- names(dir_browser$places)[[i]]
          actionButton(
            paste0("dir_browser_place_", i),
            nm,
            class = "btn btn-default btn-sm"
          )
        })
      ),
      tags$div(
        style = "display:flex; flex-wrap:wrap; gap:6px; margin-bottom:10px;",
        lapply(seq_along(crumb_labels), function(i) {
          actionButton(
            paste0("dir_browser_crumb_", i),
            crumb_labels[[i]],
            class = "btn btn-default btn-sm"
          )
        })
      ),
      textInput(
        "dir_browser_manual_path",
        "Path",
        value = current
      ),
      tags$div(
        style = "display:flex; gap:8px; margin-bottom:10px;",
        actionButton("dir_browser_go_manual", "Go to path"),
        actionButton("dir_browser_refresh", "Refresh folder")
      ),
      tags$div(
        style = "border:1px solid #d9d0c3; border-radius:10px; max-height:320px; overflow:auto; padding:8px; background:#fff;",
        tagList(
          if (!length(dirs)) {
            tags$p(style = "margin:4px 0;", "No subfolders found here.")
          } else {
            lapply(seq_along(dirs), function(i) {
              tags$div(
                style = "display:flex; align-items:center; justify-content:space-between; gap:10px; padding:6px 4px; border-bottom:1px solid #eee6da;",
                tags$span(paste0("[Folder] ", basename(dirs[[i]]))),
                actionButton(
                  paste0("dir_browser_open_", i),
                  "Open",
                  class = "btn btn-default btn-sm"
                )
              )
            })
          },
          if (identical(dir_browser$mode, "file")) {
            if (!length(files)) {
              tags$p(style = "margin:10px 0 4px 0;", "No matching files found here.")
            } else {
              lapply(seq_along(files), function(i) {
                tags$div(
                  style = "display:flex; align-items:center; justify-content:space-between; gap:10px; padding:6px 4px; border-bottom:1px solid #eee6da;",
                  tags$span(paste0("[File] ", basename(files[[i]]))),
                  actionButton(
                    paste0("dir_browser_choose_file_", i),
                    "Choose",
                    class = "btn btn-default btn-sm"
                  )
                )
              })
            }
          }
        )
      )
    )
  })

  observeEvent(input$browse_rename_search_root, {
    open_dir_browser("rename_search_root", input$rename_search_root %||% ".")
  }, ignoreInit = TRUE)

  observeEvent(input$browse_image_list_source, {
    open_dir_browser("image_list_source", input$image_list_source %||% file.path(settings()$OUTPUT_ROOT_DIR_NAME %||% "pipeline_outputs", settings()$PRE_RENAMED_DIR_NAME %||% "PRE_renamed"))
  }, ignoreInit = TRUE)

  observeEvent(input$browse_project_root, {
    open_dir_browser("project_root_input", input$project_root_input %||% ".")
  }, ignoreInit = TRUE)

  observeEvent(input$browse_output_root, {
    open_dir_browser("output_root_input", input$output_root_input %||% "pipeline_outputs")
  }, ignoreInit = TRUE)

  observeEvent(input$browse_input_master, {
    open_dir_browser("input_master_input", input$input_master_input %||% ".")
  }, ignoreInit = TRUE)

  observeEvent(input$browse_metadata_file, {
    metadata_value <- input$metadata_file_input %||% ""
    if (nzchar(metadata_value) && file.exists(normalize_project_path(metadata_value, project_root))) {
      start_from <- dirname(normalize_project_path(metadata_value, project_root))
    } else {
      start_from <- normalize_project_path(settings()$INPUT_WORKSPACE_DIR_NAME %||% "pipeline_inputs", project_root)
    }
    open_file_browser("metadata_file_input", start_from, "\\.csv$")
  }, ignoreInit = TRUE)

  observeEvent(input$browse_validation_dir, {
    open_dir_browser("validation_dir_input", input$validation_dir_input %||% "cache/validation_profiles")
  }, ignoreInit = TRUE)

  observeEvent(input$dir_browser_up, {
    current <- dir_browser$current %||% project_root
    parent <- dirname(current)
    if (!dir.exists(parent)) parent <- current
    dir_browser$current <- parent
  }, ignoreInit = TRUE)

  observeEvent(input$dir_browser_go_manual, {
    target <- input$dir_browser_manual_path %||% "."
    resolved <- normalize_project_path(target, project_root)
    if (!dir.exists(resolved)) {
      showNotification("That folder does not exist.", type = "error")
      return()
    }
    dir_browser$current <- resolved
  }, ignoreInit = TRUE)

  observeEvent(input$dir_browser_refresh, {
    dir_browser$current <- normalizePath(dir_browser$current %||% project_root, winslash = "/", mustWork = FALSE)
  }, ignoreInit = TRUE)

  observeEvent(input$dir_browser_use_current, {
    req(dir_browser$target)
    updateTextInput(session, dir_browser$target, value = relative_to_project(dir_browser$current))
    removeModal()
  }, ignoreInit = TRUE)

  for (i in seq_len(max_dir_buttons)) {
    local({
      idx <- i
      observeEvent(input[[paste0("dir_browser_open_", idx)]], {
        entries <- dir_browser$entries %||% character()
        if (length(entries) < idx) return()
        dir_browser$current <- entries[[idx]]
      }, ignoreInit = TRUE)
    })
  }

  for (i in seq_len(max_file_buttons)) {
    local({
      idx <- i
      observeEvent(input[[paste0("dir_browser_choose_file_", idx)]], {
        files <- dir_browser$files %||% character()
        if (length(files) < idx) return()
        req(dir_browser$target)
        updateTextInput(session, dir_browser$target, value = relative_to_project(files[[idx]]))
        removeModal()
      }, ignoreInit = TRUE)
    })
  }

  for (i in seq_len(30L)) {
    local({
      idx <- i
      observeEvent(input[[paste0("dir_browser_crumb_", idx)]], {
        crumbs <- dir_browser$breadcrumbs %||% character()
        if (length(crumbs) < idx) return()
        dir_browser$current <- crumbs[[idx]]
      }, ignoreInit = TRUE)
    })
  }

  for (i in seq_len(30L)) {
    local({
      idx <- i
      observeEvent(input[[paste0("dir_browser_place_", idx)]], {
        places <- dir_browser$places %||% list()
        if (length(places) < idx) return()
        dir_browser$current <- normalizePath(unname(places[[idx]]), winslash = "/", mustWork = FALSE)
      }, ignoreInit = TRUE)
    })
  }

  observeEvent(input$partial_continue_btn, {
    ps <- pending_step()
    req(ps)
    removeModal()
    run_step_now(ps$step, ps$runtime)
    pending_step(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$partial_regenerate_btn, {
    ps <- pending_step()
    req(ps)
    removeModal()
    regenerate_step_outputs(ps$step)
    run_step_now(ps$step, ps$runtime)
    pending_step(NULL)
  }, ignoreInit = TRUE)

  observeEvent(input$partial_retry_btn, {
    ps <- pending_step()
    req(ps)
    removeModal()
    retry_step <- build_dropout_retry_step(ps$step, ps$integrity, ps$runtime)
    if (is.null(retry_step)) {
      showNotification("This step does not currently support retrying only the missing items.", type = "warning")
    } else {
      run_step_now(retry_step, ps$runtime)
    }
    pending_step(NULL)
  }, ignoreInit = TRUE)

  profiles_df <- reactive({
    tick()
    list_validation_profiles(settings())
  })

  observe({
    invalidateLater(1000, session)
    if (!has_processx) return()

    if (isTRUE(validation_runner$active) && !isTRUE(runner$active)) {
      advance_validation_chain_from_outputs("background chain recovery")
    }

    if (!runner$active) return()

    if (is.null(runner$process)) {
      if (!reconcile_completed_runner("a missing process handle with complete outputs")) {
        if (!isTRUE(runner$missing_process_warned)) {
          append_log("Monitor warning:", "A run is marked active but no subprocess handle is available. Check the current step outputs or restart the app if this persists.")
          runner$missing_process_warned <- TRUE
        }
      }
      return()
    }

    current_size <- if (!is.null(runner$log_file) && file.exists(runner$log_file)) file.info(runner$log_file)$size %||% 0 else 0
    if (is.finite(current_size) && current_size > runner$last_size) {
      runner$last_size <- current_size
      runner$last_growth_at <- Sys.time()
    }

    if (log_has_completion_marker(runner$log_file) && reconcile_completed_runner("a completion marker in the live log")) {
      return()
    }

    process_alive <- tryCatch(runner$process$is_alive(), error = function(e) FALSE)
    if (!process_alive) {
      exit_status <- tryCatch(runner$process$get_exit_status(), error = function(e) NA_integer_)
      if (isTRUE(exit_status == 0)) {
        append_log("Completed:", runner$step$title)
        append_log("Exit status:", exit_status)
        showNotification(paste("Completed:", runner$step$title), type = "message")
        finalize_run("completed", exit_status)
      } else {
        append_log("Failed:", runner$step$title)
        append_log("Exit status:", exit_status)
        failure_info <- detect_failure_explanation(runner$step, data.frame(status = "failed", log_file = runner$log_file, stringsAsFactors = FALSE), settings())
        failure_msg <- if (!is.null(failure_info)) failure_info$reason else paste("Failed:", runner$step$title, "- check the log.")
        showNotification(failure_msg, type = "error", duration = 10)
        if (is_input_permission_failure_info(failure_info)) {
          show_input_permission_failure_modal(failure_info$reason, failure_info$suggestion)
        }
        restore_current_tabs()
        finalize_run("failed", exit_status)
      }
    }
  })

  observe({
    prof <- profiles_df()
    choices <- if (nrow(prof)) setNames(prof$profile_name, prof$profile_name) else c("No profiles detected" = "")
    current_choice <- input$profile_choice %||% ""
    selected_choice <- if (nzchar(current_choice) && current_choice %in% unname(choices)) current_choice else choices[[1]]
    updateSelectInput(session, "profile_choice", choices = choices, selected = selected_choice)
  })

  observe({
    profiles_df()
    current_name <- input$opt_profile_name_inline %||% ""
    suggested_name <- suggest_unique_profile_name(settings())
    if (!nzchar(current_name) || profile_name_exists(current_name, settings())) {
      updateTextInput(session, "opt_profile_name_inline", value = suggested_name)
    }
  })

  output$decision_logic_text <- renderText({
    if (identical(input$validation_mode_tabs %||% "2.A. Validate New Cutoffs", "2.B. Skip Validation and Use Prior Cutoffs Model")) {
      "Reuse a trusted saved cutoff profile, then continue directly with generalization and the Visualization and Interpretation module."
    } else {
      "Run the full path: Validation with Test Images, then Generalization with New Images, then Visualization and Interpretation."
    }
  })

  output$runtime_table <- renderTable({
    s <- settings()
    p <- build_paths(s)
    data.frame(
      check = c("Current session is Windows", "WSL command visible", "Bash visible", "Rscript visible", "Managed input workspace", "Central output workspace", "Fiji path", "ilastik path"),
      status = c(
        if (.Platform$OS.type == "windows") "yes" else "no",
        if (nzchar(Sys.which("wsl"))) "yes" else "no",
        if (nzchar(Sys.which("bash"))) "yes" else "no",
        if (nzchar(Sys.which("Rscript"))) "yes" else "no",
        p$input_workspace,
        p$output_root,
        s$FIJI_BIN %||% "",
        s$ILASTIK_BIN %||% ""
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$software_versions_table <- renderTable({
    current_software_versions(settings())
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$package_versions_table <- renderTable({
    current_package_versions(project_root)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$profile_table <- renderTable({
    prof <- profiles_df()
    if (!nrow(prof)) return(data.frame(message = "No saved validation profiles were detected yet."))
    prof
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$profile_picker_ui <- renderUI({
    prof <- profiles_df()
    if (!nrow(prof)) {
      return(tags$div(
        class = "step-status-pill not-ready",
        tags$span(class = "step-status-icon", "○"),
        tags$div(
          tags$strong("No saved profiles detected"),
          tags$br(),
          "Run fresh validation and save an optimization profile first, or check the configured validation profile folder."
        )
      ))
    }
    choices <- setNames(prof$profile_name, paste0(prof$profile_name, ifelse(nzchar(prof$created_at), paste0(" | ", prof$created_at), "")))
    selected <- input$profile_choice %||% choices[[1]]
    if (!selected %in% prof$profile_name) selected <- choices[[1]]
    tagList(
      selectInput("profile_choice", "Saved profile to load", choices = choices, selected = selected),
      tags$p("Loading a profile copies its saved cutoff outputs back into the active working output folder, so downstream analysis uses those selected cutoffs."),
      actionButton("apply_profile_btn", "Load Selected Cutoff Profile", class = "apply-button")
    )
  })

  output$selected_profile_details_ui <- renderUI({
    prof <- profiles_df()
    selected <- input$profile_choice %||% ""
    if (!nrow(prof) || !nzchar(selected)) {
      return(tags$p("Select a profile on the left to see details here."))
    }
    row <- prof[prof$profile_name == selected, , drop = FALSE]
    if (!nrow(row)) {
      return(tags$p("The selected profile is not currently in the detected profile list. Click refresh/save settings if the profile folder changed."))
    }
    profile_root <- if (identical(row$source[[1]], basename(normalize_project_path(settings()$VALIDATION_PROFILE_DIR_NAME %||% "cache/validation_profiles")))) {
      normalize_project_path(settings()$VALIDATION_PROFILE_DIR_NAME %||% "cache/validation_profiles")
    } else {
      normalize_project_path(file.path("cache", row$source[[1]]))
    }
    profile_dir <- file.path(profile_root, selected)
    metadata_file <- file.path(profile_dir, "profile_metadata.tsv")
    manifest_file <- file.path(profile_dir, "profile_file_manifest.tsv")
    report_file <- file.path(profile_dir, "profile_recreation_report.txt")
    tags$div(
      class = "decision-box",
      tags$strong("Selected profile: "), tags$span(class = "path-block", selected),
      tags$br(),
      tags$strong("Created: "), row$created_at[[1]] %||% "",
      tags$br(),
      tags$strong("Folder: "), tags$span(class = "path-block", profile_dir),
      tags$br(),
      tags$strong("Metadata bundle: "),
      if (file.exists(metadata_file) && file.exists(manifest_file)) "available" else "not found for this profile",
      tags$br(),
      tags$strong("Recreation report: "),
      if (file.exists(report_file)) "available" else "not found for this profile"
    )
  })

  output$history_table <- renderTable({
    hist <- history_df()
    history_table_view(hist, n = 20)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  observe({
    hist <- history_df()
    choices <- if (nrow(hist)) {
      setNames(hist$log_file, paste(hist$started_at, "|", hist$title, "|", hist$status))
    } else {
      c("No saved logs yet" = "")
    }
    updateSelectInput(session, "saved_log_choice", choices = choices, selected = choices[[1]])
  })

  output$current_run_ui <- renderUI({
    invalidateLater(1000, session)

    if (!has_processx) {
      return(tags$div(
        class = "decision-box",
        tags$strong("Live background monitoring is limited."),
        tags$p("The app could not find the processx package, so it can still run commands and save history, but it cannot provide the full live elapsed-time and quiet-output monitoring layer. Installing processx will enable the richer monitor.")
      ))
    }

    if ((!runner$active || is.null(runner$step)) && isTRUE(validation_runner$active)) {
      remaining <- validation_runner$remaining %||% character()
      return(tags$div(
        class = "decision-box",
        tags$strong("2.A.5 post-manual chain is active between backend steps."),
        tags$p(validation_runner$last_message %||% "The app is checking whether the next 2.A.5 substep should start."),
        tags$p(tags$strong("Remaining substeps: "), if (length(remaining)) paste(remaining, collapse = " -> ") else "none"),
        tags$p(tags$strong("Last chain update: "), if (!is.null(validation_runner$last_update)) format(validation_runner$last_update, "%Y-%m-%d %H:%M:%S") else "not recorded"),
        tags$p("If this does not advance within a few seconds, click the recovery button below. It will inspect the expected outputs and continue only if the current substep already looks complete."),
        actionButton("advance_validation_chain_btn", "Recheck 2.A.5 Chain and Continue", class = "btn btn-primary")
      ))
    }

    if (!runner$active || is.null(runner$step)) {
      return(tags$div(
        class = "decision-box",
        tags$strong("No step is currently running."),
        tags$p("When you start a step, this panel will show the current command, elapsed time, expected typical duration based on previous runs, and whether the log still appears active.")
      ))
    }

    hist <- history_df()
    started_at <- runner$started_at
    if (is.null(started_at) || is.na(started_at)) {
      run_stamp <- sub("_.*$", "", runner$run_id %||% "")
      parsed_stamp <- suppressWarnings(as.POSIXct(run_stamp, format = "%Y%m%d%H%M%S", tz = Sys.timezone()))
      if (!is.na(parsed_stamp)) started_at <- parsed_stamp
    }
    elapsed <- if (!is.null(started_at) && !is.na(started_at)) {
      as.numeric(difftime(Sys.time(), started_at, units = "secs"))
    } else {
      NA_real_
    }
    m <- machine_info()
    task <- runner$task_size %||% task_size_for_step(runner$step$id, settings())
    pred <- predict_step_duration(hist, runner$step$id, runner$runtime, m$machine_signature, m$cpu_cores, task$value)
    typical <- pred$predicted
    quiet_for <- if (!is.null(runner$last_growth_at)) as.numeric(difftime(Sys.time(), runner$last_growth_at, units = "secs")) else NA_real_
    width_pct <- if (is.finite(typical) && is.finite(elapsed)) max(5, min(95, round((elapsed / typical) * 100))) else if (is.finite(elapsed)) max(10, min(85, round(15 + elapsed / 6))) else 10
    assessment <- if (is.na(quiet_for) || quiet_for < 20) {
      "The log is still receiving output, so the process appears active."
    } else if (is.finite(typical) && elapsed > (typical * 1.5) && quiet_for > 60) {
      "This step is longer and quieter than typical. It may still be working, but it deserves attention in case it is stuck."
    } else if (quiet_for > 60) {
      "This step is currently quiet. Some tools produce sparse output, so quiet periods are not always a crash."
    } else {
      "This step is still within a normal-looking range."
    }

    tags$div(
      tags$h4(runner$step$title),
      tags$p(tags$strong("Current command: "), runner$command),
      tags$p(tags$strong("Elapsed time: "), if (is.finite(elapsed)) format_seconds(elapsed) else "unknown"),
      tags$p(tags$strong("Predicted typical duration on this machine and task size: "), if (is.finite(typical)) format_seconds(typical) else "No history yet"),
      tags$p(tags$strong("Task size metric: "), paste(task$label, "=", if (is.finite(task$value)) format(round(task$value, 2), scientific = FALSE) else "unknown")),
      tags$p(tags$strong("Quiet for: "), format_seconds(quiet_for)),
      tags$div(class = "progress-wrap",
               tags$div(class = "progress-outer",
                        tags$div(class = "progress-inner", style = sprintf("width:%s%%;", width_pct))),
               tags$div(class = "progress-label", assessment)),
      actionButton("reconcile_current_run_btn", "Recheck and Finalize if Outputs Are Complete", class = "btn btn-default")
    )
  })

  observeEvent(input$reconcile_current_run_btn, {
    append_log("Monitor recovery button:", "User requested a recheck/finalize pass.")
    if (!isTRUE(runner$active) || is.null(runner$step)) {
      latest_logs <- tryCatch(
        list.files(run_log_dir, pattern = "\\.log$", full.names = TRUE),
        error = function(e) character()
      )
      latest_logs <- latest_logs[file.exists(latest_logs)]
      if (length(latest_logs)) {
        latest_logs <- latest_logs[order(file.info(latest_logs)$mtime, decreasing = TRUE)]
        latest_log <- latest_logs[[1]]
        latest_tail <- tryCatch(paste(utils::tail(readLines(latest_log, warn = FALSE), 8), collapse = "\n"), error = function(e) "")
        append_log("Monitor recovery button:", sprintf("No active in-memory runner. Latest saved log is %s.", latest_log))
        if (nzchar(latest_tail)) append_log("Latest saved log tail:\n", latest_tail)
      } else {
        append_log("Monitor recovery button:", "No active in-memory runner and no saved run logs were found.")
      }
      tick(tick() + 1)
      showNotification("No active run is currently being monitored. Refreshed readiness checks and printed the latest saved log tail.", type = "message", duration = 8)
      return()
    }
    if (!reconcile_completed_runner("manual monitor recheck")) {
      integrity <- step_integrity_check(runner$step, settings())
      append_log(
        "Monitor recovery button:",
        sprintf("Current step still does not look complete: %s", integrity$summary %||% "no summary available")
      )
      showNotification("The current step does not yet look complete based on its expected outputs.", type = "warning", duration = 8)
    }
  }, ignoreInit = TRUE)

  observeEvent(input$advance_validation_chain_btn, {
    append_log("2.A.5 recovery button:", "User requested a post-manual chain recheck.")
    if (!isTRUE(validation_runner$active)) {
      append_log("2.A.5 recovery button:", "The post-manual chain is not active in app memory.")
      tick(tick() + 1)
      showNotification("The 2.A.5 chain is not currently active.", type = "message")
      return()
    }
    if (!advance_validation_chain_from_outputs("manual 2.A.5 chain recheck")) {
      remaining <- validation_runner$remaining %||% character()
      next_id <- if (length(remaining)) remaining[[1]] else ""
      next_step <- find_step_by_id(current_steps(), next_id)
      next_title <- if (!is.null(next_step)) next_step$title else next_id
      showNotification(
        sprintf("2.A.5 is waiting at %s. Its expected outputs are not complete yet.", next_title),
        type = "warning",
        duration = 8
      )
    }
  }, ignoreInit = TRUE)

  output$timing_explanation <- renderText({
    hist <- history_df()
    if (!runner$active || is.null(runner$step)) {
      return(paste(
        "Timing logic:",
        "- The app stores completed command durations in cache/run_logs/command_history.csv.",
        "- For each step, it records the runtime, machine signature, CPU core count, a task-size metric, and when possible a seconds-per-unit rate.",
        "- For batch-style steps this metric can be images, measured rows, ROI objects, or skeleton/object counts, depending on the stage.",
        "- When enough history exists, the estimate uses a simple linear model of duration against task size and CPU cores for the same step and runtime.",
        "- Otherwise it falls back to a median seconds-per-unit estimate, then a per-task-unit median, and finally a plain median successful duration.",
        sep = "\n"
      ))
    }
    m <- machine_info()
    task <- runner$task_size %||% task_size_for_step(runner$step$id, settings())
    pred <- predict_step_duration(hist, runner$step$id, runner$runtime, m$machine_signature, m$cpu_cores, task$value)
    method_text <- switch(
      pred$method,
      linear_regression = "Simple linear regression on past successful runs of the same step, using task size and CPU cores.",
      median_seconds_per_unit = "Median seconds per analyzed unit from past successful runs of the same step.",
      median_per_task_unit = "Median duration per task unit from past successful runs of the same step.",
      median_duration = "Median successful duration from past runs of the same step.",
      no_history = "No relevant history yet, so the app cannot estimate a typical duration.",
      "Fallback heuristic."
    )
    paste(
      "Timing logic for the current run:",
      paste("- Method:", method_text),
      paste("- Machine signature:", m$machine_signature),
      paste("- CPU cores seen by R:", m$cpu_cores %||% "unknown"),
      paste("- Task-size metric:", task$label),
      paste("- Task-size value:", if (is.finite(task$value)) format(round(task$value, 2), scientific = FALSE) else "unknown"),
      "- The progress bar is therefore an informed estimate, not a guaranteed completion percentage.",
      "- Quiet log periods are tracked separately so the app can warn when a run is much quieter and longer than typical.",
      sep = "\n"
    )
  })

  output$output_status_table <- renderTable({
    p <- build_paths(settings())
    data.frame(
      artifact = c(
        "Managed input workspace", "Central output workspace", "Renamed image folder", "Image list", "Mask percentages CSV", "Validation groups CSV",
        "Inspected validation CSV", "Packaged ROI RDS", "Optimization results CSV",
        "Generalized skeleton RDS", "Metadata PCA RDS", "Final reporting folder"
      ),
      exists = c(
        dir.exists(p$input_workspace), dir.exists(p$output_root), dir.exists(p$pre_renamed), file.exists(p$image_list), file.exists(p$mask_percentages),
        file.exists(p$groups), file.exists(p$inspected), file.exists(p$opt_rds),
        file.exists(p$param_results), file.exists(p$generalized),
        file.exists(p$pca_rds), dir.exists(p$final_dir)
      ),
      path = c(
        p$input_workspace, p$output_root, p$pre_renamed, p$image_list, p$mask_percentages, p$groups, p$inspected,
        p$opt_rds, p$param_results, p$generalized, p$pca_rds, p$final_dir
      ),
      stringsAsFactors = FALSE
    )
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  observeEvent(input$apply_profile_btn, {
    req(nzchar(input$profile_choice))
    cmd <- sprintf("bash ./launchers/apply_optimization_profile.sh %s", bash_quote(input$profile_choice))
    withProgress(message = "Applying saved validation profile", value = 0.1, {
      started <- Sys.time()
      res <- run_runtime_command(cmd, input$runtime_choice)
      append_log("Apply profile:", input$profile_choice)
      append_log("Command:", res$command)
      if (nzchar(res$output)) append_log(res$output)
      append_run_history(list(
        run_id = new_run_id("apply_profile"),
        category = "profile_action",
        step_id = "apply_profile",
        title = paste("Apply validation profile:", input$profile_choice),
        runtime = input$runtime_choice,
        machine_signature = machine_info()$machine_signature,
        cpu_cores = machine_info()$cpu_cores,
        task_size = NA_real_,
        task_size_label = "profile_restore",
        seconds_per_unit = NA_real_,
        command = cmd,
        started_at = format(started, "%Y-%m-%d %H:%M:%S"),
        finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        duration_seconds = as.numeric(difftime(Sys.time(), started, units = "secs")),
        status = if (res$status == 0) "completed" else "failed",
        exit_status = as.integer(res$status),
        log_file = ""
      ))
      tick(tick() + 1)
      if (res$status == 0) showNotification("Profile applied.", type = "message") else showNotification("Profile application failed. Check the log.", type = "error")
    })
  }, ignoreInit = TRUE)

  observeEvent(input$save_profile_btn, {
    save_profile_now(input$new_profile_name, input$runtime_choice)
  })

  observeEvent(input$save_profile_inline_btn, {
    updateTextInput(session, "new_profile_name", value = input$opt_profile_name_inline %||% paste0(format(Sys.Date()), "_validated_cutoffs"))
    save_profile_now(input$opt_profile_name_inline, input$runtime_choice)
  }, ignoreInit = TRUE)

  observeEvent(input$run_optimize_and_profile_btn, {
    if (isTRUE(runner$active)) {
      showNotification("Another step is already running. Please wait for it to finish.", type = "warning")
      return()
    }
    optimize_step <- find_step_by_id(current_steps(), "optimize")
    req(optimize_step)
    run_step_now(optimize_step, input$runtime_choice)
  }, ignoreInit = TRUE)

  observeEvent(input$run_quantify_from_prereq_btn, {
    removeModal()
    if (isTRUE(runner$active)) {
      showNotification("Another step is already running. Please wait for it to finish.", type = "warning")
      return()
    }
    quantify_step <- find_step_by_id(current_steps(), "quantify")
    if (is.null(quantify_step)) {
      showNotification("The app could not find the 2.A.1.4. Quantify Mask Areas step.", type = "error")
      return()
    }
    append_log("Navigation:", "Keeping the current validation tab open while 2.A.1.4. Quantify Mask Areas runs.")
    run_step_now(quantify_step, input$runtime_choice)
  }, ignoreInit = TRUE)

  output$setup_phase_ui <- renderUI({
    tryCatch(
      {
        steps <- current_steps()
        phase_steps <- steps_for_phase(steps, "Setup")
        div(
          class = "manual-panel",
          actionButton(
            "run_setup_module_btn",
            "Run full setup module (0 to 4)",
            class = "module-run-button"
          ),
          div(class = "step-grid", lapply(phase_steps, step_card_ui))
        )
      },
      error = function(e) {
        div(class = "step-summary-box step-summary-missing", tags$strong("Setup panel render error: "), conditionMessage(e))
      }
    )
  })

  output$validation_mode_logic_text <- renderText({
    if (identical(input$validation_mode_tabs %||% "2.A. Validate New Cutoffs", "2.B. Skip Validation and Use Prior Cutoffs Model")) {
      "Active mode: use a prior cutoffs model. New-cutoff validation is inactive until you switch back to the validation tab."
    } else {
      "Active mode: Validate New Cutoffs. Prior-model reuse is inactive until you switch to the reuse tab."
    }
  })

  observeEvent(input$validation_mode_tabs, {
    if (nzchar(input$validation_mode_tabs %||% "")) validation_mode_selected(input$validation_mode_tabs)
  }, ignoreInit = TRUE)

  observeEvent(input$validation_phase_tabs, {
    if (nzchar(input$validation_phase_tabs %||% "")) validation_phase_selected(input$validation_phase_tabs)
  }, ignoreInit = TRUE)

  observeEvent(input$cutoff_review_tabs, {
    if (nzchar(input$cutoff_review_tabs %||% "")) cutoff_review_tab_selected(input$cutoff_review_tabs)
  }, ignoreInit = TRUE)

  observeEvent(input$generalization_tabs, {
    if (nzchar(input$generalization_tabs %||% "")) generalization_tab_selected(input$generalization_tabs)
  }, ignoreInit = TRUE)

  observeEvent(input$about_tabs, {
    if (nzchar(input$about_tabs %||% "")) about_tab_selected(input$about_tabs)
  }, ignoreInit = TRUE)

  observeEvent(input$workplace_tabs, {
    if (nzchar(input$workplace_tabs %||% "")) workplace_tab_selected(input$workplace_tabs)
  }, ignoreInit = TRUE)

  observeEvent(input$monitor_tabs, {
    if (nzchar(input$monitor_tabs %||% "")) monitor_tab_selected(input$monitor_tabs)
  }, ignoreInit = TRUE)

  output$validation_groups_plot_ui <- renderUI({
    s <- settings()
    p <- build_paths(s)
    plot_path <- file.path(p$out_opt, "quadrant_selection_report.jpg")
    confirmed_at <- suppressWarnings(as.POSIXct(s$SEG_LABEL_MAPPING_CONFIRMED_AT %||% "", tz = Sys.timezone()))
    mask_mtime <- if (file.exists(p$mask_percentages)) file.info(p$mask_percentages)$mtime else as.POSIXct(NA)
    plot_mtime <- if (file.exists(plot_path)) file.info(plot_path)$mtime else as.POSIXct(NA)
    groups_mtime <- if (file.exists(p$groups)) file.info(p$groups)$mtime else as.POSIXct(NA)
    stale_reasons <- character()
    if (!identical(tolower(s$SEG_LABEL_MAPPING_CONFIRMED %||% "false"), "true")) {
      stale_reasons <- c(stale_reasons, "The segmentation label mapping has not been confirmed yet.")
    }
    if (!is.na(confirmed_at) && !is.na(mask_mtime) && mask_mtime < confirmed_at) {
      stale_reasons <- c(stale_reasons, "The mask percentages table is older than the confirmed label mapping, so neurites_percent may still reflect the old label mapping.")
    }
    if (!is.na(confirmed_at) && !is.na(groups_mtime) && groups_mtime < confirmed_at) {
      stale_reasons <- c(stale_reasons, "The validation groups table is older than the confirmed label mapping.")
    }
    if (!is.na(confirmed_at) && !is.na(plot_mtime) && plot_mtime < confirmed_at) {
      stale_reasons <- c(stale_reasons, "The validation grouping plot is older than the confirmed label mapping.")
    }
    if (file.exists(p$mask_percentages)) {
      mask_df <- tryCatch(utils::read.csv(p$mask_percentages, check.names = FALSE), error = function(e) NULL)
      if (!is.null(mask_df) && "neurites_percent" %in% names(mask_df)) {
        neur_vals <- suppressWarnings(as.numeric(mask_df$neurites_percent))
        neur_vals <- neur_vals[is.finite(neur_vals)]
        if (length(neur_vals) && length(unique(neur_vals)) <= 1L) {
          stale_reasons <- c(stale_reasons, sprintf("The current mask percentages table has a degenerate neurites_percent column: %s unique value(s), with range %s to %s.", length(unique(neur_vals)), signif(min(neur_vals), 6), signif(max(neur_vals), 6)))
        }
      }
    }
    if (length(stale_reasons)) {
      return(tags$div(
        class = "step-summary-box step-summary-warn",
        tags$div(class = "step-summary-title", "Validation Grouping Plot Needs Regeneration"),
        tags$p("This plot should not be interpreted yet. It appears to be stale or based on a degenerate neurite summary table."),
        tags$ul(lapply(stale_reasons, tags$li)),
        tags$p("Next step: rerun 2.A.1.4. Quantify Mask Areas, then rerun 2.A.3. Create Validation Groups. The neurites histogram should then be based on the confirmed neurite label.")
      ))
    }
    if (!file.exists(plot_path)) return(NULL)
    tags$div(
      class = "manual-panel validation-plot-panel",
      tags$h4("Validation Grouping Plot"),
      tags$p("Compact preview of the generated four-panel threshold and subgroup report. Use the larger view if you need to inspect labels or cutoffs in detail."),
      tags$div(
        class = "validation-plot-frame",
        imageOutput("validation_groups_plot", width = "900px", height = "auto")
      ),
      div(
        class = "manual-action-row",
        actionButton("show_validation_groups_plot_btn", "Show Larger Plot"),
        downloadButton("download_validation_groups_plot_btn", "Download Plot")
      ),
      tags$p(class = "path-block", tags$strong("Report file: "), plot_path)
    )
  })

  output$validation_groups_plot <- renderImage({
    plot_path <- file.path(build_paths(settings())$out_opt, "quadrant_selection_report.jpg")
    req(file.exists(plot_path))
    list(
      src = plot_path,
      contentType = "image/jpeg",
      alt = "Validation grouping plot"
    )
  }, deleteFile = FALSE)

  output$validation_groups_plot_large <- renderImage({
    plot_path <- file.path(build_paths(settings())$out_opt, "quadrant_selection_report.jpg")
    req(file.exists(plot_path))
    list(
      src = plot_path,
      contentType = "image/jpeg",
      alt = "Validation grouping plot"
    )
  }, deleteFile = FALSE)

  output$download_validation_groups_plot_btn <- downloadHandler(
    filename = function() "quadrant_selection_report.jpg",
    content = function(file) {
      plot_path <- file.path(build_paths(settings())$out_opt, "quadrant_selection_report.jpg")
      req(file.exists(plot_path))
      file.copy(plot_path, file, overwrite = TRUE)
    }
  )

  observeEvent(input$show_validation_groups_plot_btn, {
    showModal(modalDialog(
      title = "Validation Grouping Plot",
      easyClose = TRUE,
      footer = NULL,
      class = "validation-plot-modal",
      modal_top_close(),
      tags$style(HTML("
        body.modal-open .modal { padding-left:0 !important; padding-right:0 !important; }
        body.modal-open .modal .modal-dialog,
        body.modal-open .modal-dialog {
          width:100vw !important; max-width:none !important; height:100vh !important;
          margin:0 !important; padding:0 !important;
        }
        body.modal-open .modal .modal-content,
        body.modal-open .modal-dialog .modal-content {
          width:100vw !important; height:100vh !important; border-radius:0 !important;
        }
        body.modal-open .modal .modal-body,
        body.modal-open .modal-dialog .modal-body {
          height:calc(100vh - 120px) !important; overflow:hidden !important;
        }
        body.modal-open .validation-plot-frame-large {
          height:calc(100vh - 230px) !important;
        }
      ")),
      tags$script(HTML("
        setTimeout(function() {
          var modal = $('.modal:visible').last();
          modal.css({ 'padding-left': '0', 'padding-right': '0' });
          modal.find('.modal-dialog').css({
            'width': '100vw',
            'max-width': 'none',
            'height': '100vh',
            'margin': '0',
            'padding': '0'
          });
          modal.find('.modal-content').css({
            'width': '100vw',
            'height': '100vh',
            'border-radius': '0'
          });
          modal.find('.modal-body').css({
            'height': 'calc(100vh - 120px)',
            'overflow': 'hidden'
          });
          modal.find('.validation-plot-frame-large').css({
            'height': 'calc(100vh - 230px)'
          });
        }, 50);
      ")),
      tags$p("Larger report preview. If the text is still too dense, use Download Plot and open the file in an external image viewer."),
      sliderInput("validation_groups_plot_zoom_large", "Plot Zoom", min = 50, max = 300, value = 120, step = 10, post = "%"),
      tags$div(
        class = "validation-plot-frame-large",
        uiOutput("validation_groups_plot_large_ui")
      )
    ))
  }, ignoreInit = TRUE)

  output$validation_groups_plot_large_ui <- renderUI({
    zoom <- suppressWarnings(as.numeric(input$validation_groups_plot_zoom_large %||% 120))
    if (!is.finite(zoom)) zoom <- 120
    tags$div(
      class = "validation-plot-zoomed",
      style = sprintf("transform: scale(%s);", zoom / 100),
      imageOutput("validation_groups_plot_large", width = "900px", height = "auto")
    )
  })

  output$validation_workspace_ui <- renderUI({
    tryCatch(local({
      steps <- safe_steps_for_ui()
      phase_steps <- steps_for_phase(steps, "Validation with Test Images")
      groups_step <- find_step_by_id(phase_steps, "groups")
      post_manual_steps <- Filter(function(x) x$id %in% c("writeback", "roi_extract", "package_rois"), phase_steps)
      optimize_step <- find_step_by_id(phase_steps, "optimize")
      mode_selected <- input$validation_mode_tabs %||% isolate(validation_mode_selected())
      if (!mode_selected %in% c("2.A. Validate New Cutoffs", "2.B. Skip Validation and Use Prior Cutoffs Model", "2.C. Review Obtained/Selected Cutoffs")) {
        mode_selected <- "2.A. Validate New Cutoffs"
      }
      phase_selected <- isolate(validation_phase_selected())
      if (!phase_selected %in% c("2.A.1. Setup and Preprocessing", "2.A.2. Check Segmentation Label Mapping", "2.A.3. Create Validation Groups", "2.A.4. Manual Inspection", "2.A.5. Apply Manual Labels and Package ROIs", "2.A.6. Threshold Optimization")) {
        phase_selected <- "2.A.1. Setup and Preprocessing"
      }
      cutoff_selected <- isolate(cutoff_review_tab_selected())
      if (!cutoff_selected %in% c("2.C.1. Cutoff Model Inspection", "2.C.2. Image and ROI Overlay Visualization")) {
        cutoff_selected <- "2.C.1. Cutoff Model Inspection"
      }
      tabsetPanel(
        id = "validation_mode_tabs",
        selected = mode_selected,
      tabPanel(
        "2.A. Validate New Cutoffs",
        if (!identical(mode_selected, "2.A. Validate New Cutoffs")) {
          div(
            class = "step-summary-box step-summary-neutral",
            tags$strong("Validation workflow is lazy-loaded."),
            tags$p("Open tab 2.A when you want to run fresh validation. Keeping these readiness cards asleep makes saved-profile browsing faster.")
          )
        } else tagList(
        div(class = "decision-box", textOutput("validation_mode_logic_text")),
        tabsetPanel(
          id = "validation_phase_tabs",
          selected = phase_selected,
          tabPanel(
            "2.A.1. Setup and Preprocessing",
            div(
              class = "decision-box",
              "These are the required preparation steps for a new validation run. Use the large button for a full sequential run through 2.A.1.0 to 2.A.1.4, or use the individual cards below when you want to run only one setup substep. Individual setup cards do not auto-chain; only the large setup button does.",
              uiOutput("setup_phase_ui")
            )
          ),
          tabPanel(
            "2.A.2. Check Segmentation Label Mapping",
            div(
              class = "decision-box",
              "Inspect one segmentation mask before validation grouping. The app checks which pixel values are present, colors the configured classes, and saves the confirmed mapping so downstream Fiji/R steps count the correct neurites, cell bodies, and background labels.",
              fluidRow(
                column(
                  4,
                  div(
                    class = "manual-panel",
                    label_with_help("Segmentation Mask Selector", "segmentation_label_mapping"),
                    uiOutput("segmentation_label_mask_selector_ui"),
                    div(
                      class = "manual-action-row",
                      actionButton("segmentation_label_random_btn", "Change Image Randomly"),
                      actionButton("refresh_label_mapping_btn", "Refresh Check")
                    ),
                    uiOutput("segmentation_label_status_ui"),
                    br(),
                    uiOutput("segmentation_label_assignment_ui")
                  )
                ),
                column(
                  4,
                  div(
                    class = "manual-panel",
                    tags$h4("Pixel Histogram and Detected Peaks"),
                    plotOutput("segmentation_label_histogram", height = "260px"),
                    downloadButton("download_segmentation_label_histogram", "Download High-Quality PNG"),
                    tableOutput("segmentation_label_peak_table")
                  )
                ),
                column(
                  4,
                  div(
                    class = "manual-panel",
                    tags$h4("Colored Label Preview"),
                    tags$p("Legend: red = neurites, blue = cell bodies, yellow = background. If the colors are biologically exchanged, reassign the detected peak values on the left and confirm."),
                    tags$div(class = "manual-image-frame", imageOutput("segmentation_label_preview", width = "100%", height = "auto"))
                  )
                )
              )
            )
          ),
          tabPanel(
            "2.A.3. Create Validation Groups",
            div(
              class = "decision-box",
              "This validation stage creates the rough quadrant-based validation groups from the mask summary table and metadata. Run it after confirming the segmentation label mapping and rerunning mask-area quantification if needed. After this step succeeds, the app takes you to the Manual Inspection tab.",
              if (!is.null(groups_step)) div(class = "step-grid", step_card_ui(groups_step)),
              uiOutput("validation_groups_plot_ui")
            )
          ),
          tabPanel(
            "2.A.4. Manual Inspection",
            div(
              class = "decision-box",
              "Review each grouped image here, zoom in as needed, and label it as choice, noise, or not so certain. The app saves those decisions into a manual review manifest while you work, so the next write-back step can use those saved manual decisions.",
              div(
                class = "manual-inspection-layout",
                div(
                  class = "manual-panel",
                  label_with_help("Manual Inspection Workflow", "step_validation"),
                  selectInput("manual_group_choice", "Validation group", choices = stats::setNames(names(validation_group_map()), unname(validation_group_map()))),
                  numericInput("manual_image_index", "Image number in this group", value = 1, min = 1, step = 1),
                  sliderInput("manual_zoom_percent", "Zoom", min = 25, max = 300, value = 100, step = 25, post = "%"),
                  div(
                    class = "manual-action-row",
                    actionButton("manual_prev_image", "Previous"),
                    actionButton("manual_next_image", "Next")
                  ),
                  div(
                    class = "manual-action-row",
                    actionButton("manual_mark_choice", "Mark as choice", class = "btn-success"),
                    actionButton("manual_mark_noise", "Mark as noise", class = "btn-danger"),
                    actionButton("manual_mark_not_sure", "Mark as not so certain", class = "btn-warning"),
                    actionButton("manual_clear_label", "Clear label", class = "btn-default")
                  ),
                  actionButton("manual_finish_and_continue", "Finish manual annotation and continue", class = "module-run-button"),
                  uiOutput("manual_current_image_info")
                ),
                div(
                  class = "manual-panel",
                  uiOutput("manual_image_note_ui"),
                  tags$div(class = "manual-image-frame", imageOutput("manual_image_view", width = "100%", height = "auto"))
                ),
                div(
                  class = "manual-panel",
                  tags$h4("Manual categorization side list"),
                  uiOutput("manual_side_summary")
                )
              )
            )
          ),
          tabPanel(
            "2.A.5. Apply Manual Labels and Package ROIs",
            div(
              class = "decision-box",
              "After you confirm the recap from Manual Inspection, the app can run this sequence automatically: write back manual labels, extract ROIs and skeleton summaries, and package ROIs for optimization. You can also run the three substeps one by one below.",
              actionButton("run_post_manual_module_btn", "Run writeback + ROI extraction + ROI packaging", class = "module-run-button"),
              div(class = "step-grid", lapply(post_manual_steps, step_card_ui))
            )
          ),
          tabPanel(
            "2.A.6. Threshold Optimization",
            div(
              class = "decision-box",
              "This is the final part of the validation branch. After the writeback and ROI packaging steps are done, run the optimization step here to create the validated neurite-threshold outputs and then save them as an optimization profile for later reuse.",
              div(
                class = "manual-panel",
                tags$h4("Optimization controls"),
                p("Choose how the cutoff model should score candidate ROI thresholds before running optimization."),
                selectInput(
                  "optimization_scoring_mode_inline",
                  "Cutoff optimization mode",
                  choices = c(
                    "Continuity-aware (recommended for long, interrupted neurites)" = "continuity_aware",
                    "Topology-aware continuity (experimental; crossing neurites vs mesh noise)" = "topology_aware_continuity",
                    "Ensemble union (continuity OR topology, balanced; recommended comparison)" = "ensemble_union_continuity_topology",
                    "Classic ROI cutoff (legacy area/circularity/aspect scoring)" = "classic_roi_cutoff"
                  ),
                  selected = (settings()$OPTIMIZATION_SCORING_MODE %||% "continuity_aware")
                ),
                div(
                  class = "step-summary-box step-summary-ready",
                  tags$strong("Mode guide: "),
                  "Continuity-aware optimization rewards image-level neurite continuity, long retained skeleton length, and lower fragmentation. Topology-aware continuity adds extra penalties for sponge-like mesh geometry while still rewarding dominant long paths. Ensemble union combines both families and is useful when continuity and topology modes each recover different correct neurites. Classic ROI cutoff keeps the earlier ROI-level scoring behavior and is useful for comparison or legacy reproducibility."
                ),
                p("Use the run button in the step card below. After a successful optimization run, the app automatically saves the outputs as a reusable optimization profile using the name below."),
                textInput(
                  "opt_profile_name_inline",
                  "Optimization profile name to save after a successful run",
                  value = tryCatch(
                    suggest_unique_profile_name(settings()),
                    error = function(e) paste0(format(Sys.Date()), "_validated_cutoffs")
                  )
                )
              ),
              if (!is.null(optimize_step)) div(class = "step-grid", step_card_ui(optimize_step))
            )
          )
        )
        )
      ),
      tabPanel(
        "2.B. Skip Validation and Use Prior Cutoffs Model",
        div(class = "decision-box", textOutput("validation_mode_logic_text")),
        fluidRow(
          column(5,
                 div(class = "manual-panel",
                     label_with_help("Use a Saved Validation Profile", "workflow_strategy"),
                     p("Use this tab only when you want to skip new validation and restore a previously saved cutoffs model into the working outputs."),
                     uiOutput("profile_picker_ui"))
          ),
          column(7,
                 div(class = "manual-panel",
                     label_with_help("Detected profiles", "profile_table"),
                     uiOutput("selected_profile_details_ui"),
                     br(),
                     tableOutput("profile_table"))
          )
        )
      ),
      tabPanel(
        "2.C. Review Obtained/Selected Cutoffs",
        if (!identical(mode_selected, "2.C. Review Obtained/Selected Cutoffs")) {
          div(
            class = "step-summary-box step-summary-neutral",
            tags$strong("Cutoff review is lazy-loaded."),
            tags$p("Open tab 2.C when you want to inspect cutoff models. Keeping this panel asleep makes tab 2.B load quickly when you only need to restore a saved profile.")
          )
        } else div(
          class = "decision-box",
          label_with_help("Review Obtained or Selected Cutoffs", "cutoff_review"),
          tags$p("Use this panel after running a fresh optimization or after restoring a saved profile. It does not run backend scripts; it reads the current cutoff-related outputs and highlights whether the result looks usable, fragile, or potentially meaningless."),
          tabsetPanel(
            id = "cutoff_review_tabs",
            selected = cutoff_selected,
            tabPanel(
              "2.C.1. Cutoff Model Inspection",
              br(),
              actionButton("refresh_cutoff_review_btn", "Refresh Cutoff Review", class = "apply-button"),
              br(), br(),
              fluidRow(
                column(
                  4,
                  div(
                    class = "manual-panel",
                    tags$h4("Applied Cutoff Profile"),
                    uiOutput("cutoff_review_active_profile_ui")
                  )
                ),
                column(
                  8,
                  div(
                    class = "manual-panel table-wrap-panel",
                    tags$h4("Compare Cutoff Profiles Side by Side"),
                    uiOutput("cutoff_profile_compare_picker_ui"),
                    tags$p("Choose two or more profiles to compare their effectiveness, retention, and practical cutoff behavior."),
                    tags$div(
                      class = "metric-info-box",
                    tags$p(tags$strong("Net biological utility: "), "a readable composite: neurite separation, retention, dominant paths, and orientation coherence minus confluence coupling, fragmentation, and mesh risk."),
                    tags$p(tags$strong("Recovery proxy: "), "how permissive the model is for biologically relevant ROIs, using retention plus length/path recovery."),
                    tags$p(tags$strong("Specificity risk: "), "a false-positive-risk proxy; higher values mean more confluence coupling, fragmentation, or mesh-like retained structure."),
                    tags$p(tags$strong("Score: "), "the optimizer's internal objective; compare mainly within the same scoring mode.")
                    ),
                    uiOutput("cutoff_profile_compare_table")
                  )
                )
              ),
              fluidRow(
                column(
                  6,
                  div(
                    class = "manual-panel",
                    tags$h4("Overall Effectiveness Comparison"),
                    tags$div(
                      class = "metric-info-box",
                      tags$p("Dot size reflects each metric value within that metric row. Prefer high net utility and recovery with low specificity risk.")
                    ),
                    tags$div(class = "plot-scroll-box", plotOutput("cutoff_profile_effectiveness_plot", height = "380px", width = "100%")),
                    downloadButton("download_cutoff_profile_effectiveness_plot", "Download High-Quality PNG")
                  )
                ),
                column(
                  6,
                  div(
                    class = "manual-panel",
                    tags$h4("Four-Group Neurite Implications"),
                    tags$div(
                      class = "metric-info-box",
                      tags$p("Each dot is an ROI. Red dots are candidates and green dots are retained by the selected model. Green points are drawn last so they remain visible. The diamond marks the retained median for each validation group.")
                    ),
                    uiOutput("cutoff_group_profile_picker_ui"),
                    uiOutput("cutoff_group_metric_picker_ui"),
                    tags$div(class = "plot-scroll-box", uiOutput("cutoff_group_dot_plot_ui")),
                    downloadButton("download_cutoff_group_dot_plot", "Download High-Quality PNG")
                  )
                )
              ),
              uiOutput("cutoff_review_overall_ui"),
              fluidRow(
                column(
                  5,
                  div(
                    class = "manual-panel",
                    tags$h4("Trust and Artefact Checks"),
                    filterable_table_controls("cutoff_review_checks_table"),
                    tableOutput("cutoff_review_checks_table")
                  ),
                  div(
                    class = "manual-panel",
                    tags$h4("Best Cutoff Summary"),
                    filterable_table_controls("cutoff_review_best_table"),
                    tableOutput("cutoff_review_best_table")
                  )
                ),
                column(
                  7,
                  div(
                    class = "manual-panel",
                    tags$h4("Optimization Score Overview"),
                    plotOutput("cutoff_review_score_plot", height = "300px"),
                    downloadButton("download_cutoff_review_score_plot", "Download High-Quality PNG")
                  ),
                  div(
                    class = "manual-panel",
                    tags$h4("ROI Retention and Label Balance"),
                    plotOutput("cutoff_review_retention_plot", height = "260px"),
                    downloadButton("download_cutoff_review_retention_plot", "Download High-Quality PNG")
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  div(
                    class = "manual-panel",
                    tags$h4("Top Parameter Sets"),
                    filterable_table_controls("cutoff_review_top_table"),
                    tableOutput("cutoff_review_top_table")
                  )
                )
              ),
              fluidRow(
                column(
                  12,
                  div(
                    class = "manual-panel table-wrap-panel",
                    tags$h4("Output Files and Cache"),
                    filterable_table_controls("cutoff_review_files_table"),
                    tableOutput("cutoff_review_files_table")
                  )
                )
              )
            ),
            tabPanel(
              "2.C.2. Image and ROI Overlay Visualization",
              br(),
              div(
                class = "manual-panel",
                tags$h4("Visual Validation: Mask and Retained ROI Overlay"),
                tags$p("Use this to inspect what the selected cutoff profile really keeps. The mask layer can be toggled; retained ROIs are drawn on top when the ROI geometry is available."),
                fluidRow(
                  column(3, uiOutput("cutoff_visual_profile_picker_ui")),
                  column(3, uiOutput("cutoff_visual_group_picker_ui")),
                  column(4, uiOutput("cutoff_visual_image_picker_ui")),
                  column(2, checkboxInput("cutoff_visual_show_mask", "Show mask layer", value = FALSE))
                ),
                fluidRow(
                  column(3, checkboxInput("cutoff_visual_show_all_rois", "Show all ROIs in red", value = TRUE)),
                  column(3, checkboxInput("cutoff_visual_show_retained", "Show retained ROIs", value = TRUE)),
                  column(3, actionButton("cutoff_visual_refresh_btn", "Refresh ROI Preview", class = "apply-button")),
                  column(3, uiOutput("cutoff_visual_summary_ui"))
                ),
                plotOutput("cutoff_visual_roi_overlay_plot", height = "620px"),
                downloadButton("download_cutoff_visual_roi_overlay_plot", "Download High-Quality PNG")
              )
            )
          )
        )
      )
      )
    }), error = function(e) {
      error_message <- conditionMessage(e)
      if (!nzchar(error_message)) {
        error_message <- "A required Shiny value was not ready while building one validation subpanel. The app kept your backend outputs on disk; refresh the tab or restart the app if this persists."
      }
      tagList(
        div(
          class = "step-summary-box step-summary-missing",
          tags$strong("Validation workspace render warning: "),
          error_message,
          tags$p("The app is showing a lightweight recovery layout below so you can still inspect or apply cutoff profiles instead of losing the whole workplace.")
        ),
        tabsetPanel(
          id = "validation_mode_tabs",
          selected = "2.C. Review Obtained/Selected Cutoffs",
          tabPanel(
            "2.A. Validate New Cutoffs",
            div(
              class = "decision-box",
              tags$strong("Validation controls are in recovery mode."),
              tags$p("Restarting the Shiny app should restore the full validation workflow. Your backend output files and saved profiles are not deleted by this warning.")
            )
          ),
          tabPanel(
            "2.B. Skip Validation and Use Prior Cutoffs Model",
            fluidRow(
              column(5, div(class = "manual-panel", label_with_help("Use a Saved Validation Profile", "workflow_strategy"), uiOutput("profile_picker_ui"))),
              column(7, div(class = "manual-panel table-wrap-panel", label_with_help("Detected profiles", "profile_table"), tableOutput("profile_table")))
            )
          ),
          tabPanel(
            "2.C. Review Obtained/Selected Cutoffs",
            div(
              class = "decision-box",
              label_with_help("Review Obtained or Selected Cutoffs", "cutoff_review"),
              tags$p("Recovery view: inspect the currently saved or selected cutoff model outputs."),
              tabsetPanel(
                id = "cutoff_review_tabs",
                selected = "2.C.1. Cutoff Model Inspection",
                tabPanel(
                  "2.C.1. Cutoff Model Inspection",
                  br(),
                  actionButton("refresh_cutoff_review_btn", "Refresh Cutoff Review", class = "apply-button"),
                  br(), br(),
                  fluidRow(
                    column(4, div(class = "manual-panel", tags$h4("Applied Cutoff Profile"), uiOutput("cutoff_review_active_profile_ui"))),
                    column(8, div(class = "manual-panel table-wrap-panel", tags$h4("Compare Cutoff Profiles Side by Side"), uiOutput("cutoff_profile_compare_picker_ui"), uiOutput("cutoff_profile_compare_table")))
                  ),
                  fluidRow(
                    column(6, div(class = "manual-panel", tags$h4("Overall Effectiveness Comparison"), tags$div(class = "plot-scroll-box", plotOutput("cutoff_profile_effectiveness_plot", height = "380px", width = "100%")))),
                    column(6, div(class = "manual-panel", tags$h4("Four-Group Neurite Implications"), uiOutput("cutoff_group_profile_picker_ui"), uiOutput("cutoff_group_metric_picker_ui"), tags$div(class = "plot-scroll-box", uiOutput("cutoff_group_dot_plot_ui"))))
                  ),
                  uiOutput("cutoff_review_overall_ui"),
                  fluidRow(
                    column(5, div(class = "manual-panel", tags$h4("Trust and Artefact Checks"), tableOutput("cutoff_review_checks_table")), div(class = "manual-panel", tags$h4("Best Cutoff Summary"), tableOutput("cutoff_review_best_table"))),
                    column(7, div(class = "manual-panel", tags$h4("Optimization Score Overview"), plotOutput("cutoff_review_score_plot", height = "300px")), div(class = "manual-panel", tags$h4("ROI Retention and Label Balance"), plotOutput("cutoff_review_retention_plot", height = "260px")))
                  ),
                  fluidRow(column(12, div(class = "manual-panel table-wrap-panel", tags$h4("Output Files and Cache"), tableOutput("cutoff_review_files_table"))))
                ),
                tabPanel(
                  "2.C.2. Image and ROI Overlay Visualization",
                  br(),
                  div(
                    class = "manual-panel",
                    tags$h4("Visual Validation: Mask and Retained ROI Overlay"),
                    fluidRow(
                      column(3, uiOutput("cutoff_visual_profile_picker_ui")),
                      column(3, uiOutput("cutoff_visual_group_picker_ui")),
                      column(4, uiOutput("cutoff_visual_image_picker_ui")),
                      column(2, checkboxInput("cutoff_visual_show_mask", "Show mask layer", value = FALSE))
                    ),
                    fluidRow(
                      column(3, checkboxInput("cutoff_visual_show_all_rois", "Show all ROIs in red", value = TRUE)),
                      column(3, checkboxInput("cutoff_visual_show_retained", "Show retained ROIs", value = TRUE)),
                      column(3, actionButton("cutoff_visual_refresh_btn", "Refresh ROI Preview", class = "apply-button")),
                      column(3, uiOutput("cutoff_visual_summary_ui"))
                    ),
                    plotOutput("cutoff_visual_roi_overlay_plot", height = "620px")
                  )
                )
              )
            )
          )
        )
      )
    })
  })

  cutoff_profile_choices <- reactive({
    prof <- profiles_df()
    choices <- c("Current working outputs" = "__current__")
    if (nrow(prof)) {
      choices <- c(choices, setNames(prof$profile_name, paste0(prof$profile_name, " (", prof$source, ")")))
    }
    choices
  })

  selected_compare_profiles <- reactive({
    selected <- input$cutoff_compare_profiles
    if (is.null(selected) || !length(selected)) selected <- c("__current__")
    unique(selected)
  })

  cutoff_tab_active <- function(tab_name) {
    identical(input$validation_mode_tabs %||% validation_mode_selected(), "2.C. Review Obtained/Selected Cutoffs") &&
      identical(input$cutoff_review_tabs %||% cutoff_review_tab_selected(), tab_name)
  }

  output$cutoff_review_active_profile_table <- renderTable({
    s <- settings()
    active <- s$ACTIVE_OPTIMIZATION_PROFILE %||% s$ACTIVE_VALIDATION_PROFILE %||% ""
    d <- cutoff_review_data()
    manifest <- d$manifest
    out <- data.frame(
      Field = c("Working source", "Configured active profile", "Optimization rows", "Filtered ROI rows"),
      Value = c(
        "Current pipeline_outputs/OUT_optimize folder",
        if (nzchar(active)) active else "No saved profile marked active",
        if (nrow(d$params)) as.character(nrow(d$params)) else "missing",
        if (nrow(d$filtered)) as.character(nrow(d$filtered)) else "missing"
      ),
      stringsAsFactors = FALSE
    )
    if (nrow(manifest)) {
      out <- rbind(out, data.frame(
        Field = c("ROI checkpoint created", "ROI objects in checkpoint"),
        Value = c(manifest$created_at[[1]] %||% "", manifest$expected_roi_objects[[1]] %||% ""),
        stringsAsFactors = FALSE
      ))
    }
    out
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$cutoff_review_active_profile_ui <- renderUI({
    tags$div(
      tags$p("This is the cutoff model currently being reviewed and applied by the working output folder."),
      tableOutput("cutoff_review_active_profile_table")
    )
  })

  output$cutoff_profile_compare_picker_ui <- renderUI({
    choices <- cutoff_profile_choices()
    selected <- intersect(isolate(input$cutoff_compare_profiles %||% "__current__"), unname(choices))
    if (!length(selected)) selected <- "__current__"
    selectInput(
      "cutoff_compare_profiles",
      "Profiles to compare",
      choices = choices,
      selected = selected,
      multiple = TRUE
    )
  })

  cutoff_profile_compare_data <- reactive({
    input$refresh_cutoff_review_btn
    ids <- selected_compare_profiles()
    rows <- lapply(ids, function(id) {
      tryCatch(
        cutoff_source_summary(id, settings()),
        error = function(e) data.frame(
          Profile = id,
          Source = "Profile read error",
          Score = NA_real_,
          Error = conditionMessage(e),
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
      )
    })
    rows <- Filter(function(x) is.data.frame(x) && nrow(x), rows)
    if (!length(rows)) return(data.frame(Message = "No profiles selected for comparison."))
    all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
    rows <- lapply(rows, function(x) {
      missing <- setdiff(all_names, names(x))
      for (nm in missing) x[[nm]] <- NA
      x[, all_names, drop = FALSE]
    })
    out <- do.call(rbind, rows)
    row.names(out) <- NULL
    out
  })

  output$cutoff_profile_compare_table <- renderUI({
    d <- cutoff_profile_compare_data()
    if (!nrow(d)) return(tags$p("No profiles selected for comparison."))
    fmt <- function(x, digits = 3) {
      x <- suppressWarnings(as.numeric(x))
      ifelse(is.finite(x), format(round(x, digits), trim = TRUE), "NA")
    }
    tagList(
      tags$div(
        class = "profile-comparison-cards",
        lapply(seq_len(nrow(d)), function(i) {
          row <- d[i, , drop = FALSE]
          tags$div(
            class = "profile-comparison-card",
            tags$h5(row$Profile %||% "Profile"),
            tags$p(tags$strong(row$Optimization_Mode %||% ""), tags$br(), tags$small(row$Scoring_Mode %||% "")),
            tags$div(
              class = "profile-metric-grid",
              tags$span("Net utility"), tags$span(fmt(row$Net_Biological_Utility)),
              tags$span("Recovery proxy"), tags$span(fmt(row$Recovery_Proxy)),
              tags$span("Specificity risk"), tags$span(fmt(row$Specificity_Risk)),
              tags$span("Score"), tags$span(fmt(row$Score)),
              tags$span("Retention"), tags$span(paste0(fmt(row$Retention_Pct, 2), "%")),
              tags$span("Length retention"), tags$span(fmt(row$Length_Weighted_Retention)),
              tags$span("Dominant path"), tags$span(fmt(row$Dominant_Path_Ratio)),
              tags$span("Mesh penalty"), tags$span(fmt(row$Mesh_Penalty)),
              tags$span("Retained / raw ROIs"), tags$span(paste0(row$Retained_ROIs %||% "NA", " / ", row$Raw_ROIs %||% "NA"))
            )
          )
        })
      ),
      tags$details(
        class = "step-details",
        tags$summary(tags$span(class = "step-details-icon", "\u25b6"), tags$span(class = "step-details-text", "Show full comparison table")),
        tags$div(
          class = "table-wrap-panel",
          {
            keep <- intersect(c("Profile", "Source", "Optimization_Mode", "Scoring_Mode", "Net_Biological_Utility", "Recovery_Proxy", "Specificity_Risk", "Score", "Retention_Pct", "Length_Weighted_Retention", "Fragmentation_Penalty", "Topology_Score", "Dominant_Path_Ratio", "Mesh_Penalty", "Orientation_Coherence", "Retained_ROIs", "Raw_ROIs", "Neurite_Separation", "Confluence_Separation", "Signal_Noise_Ratio", "Created"), names(d))
            full <- d[, keep, drop = FALSE]
            tags$table(
              class = "table table-striped table-bordered table-condensed",
              tags$thead(tags$tr(lapply(names(full), tags$th))),
              tags$tbody(lapply(seq_len(nrow(full)), function(r) {
                tags$tr(lapply(full[r, , drop = TRUE], function(v) tags$td(as.character(v %||% ""))))
              }))
            )
          }
        )
      )
    )
  })

  output$cutoff_profile_effectiveness_plot <- renderPlot({
    on.exit(remember_plot("cutoff_profile_effectiveness_plot"), add = TRUE)
    d <- cutoff_profile_compare_data()
    if (!nrow(d)) {
      plot.new(); text(0.5, 0.5, "Select profiles to compare."); return()
    }
    metrics <- c("Net_Biological_Utility", "Recovery_Proxy", "Specificity_Risk", "Neurite_Separation", "Retention_Pct", "Signal_Noise_Ratio")
    metric_labels <- c(
      Net_Biological_Utility = "Net utility",
      Recovery_Proxy = "Recovery",
      Specificity_Risk = "Specificity risk",
      Neurite_Separation = "Neurite sep.",
      Retention_Pct = "Retention %",
      Signal_Noise_Ratio = "Signal/noise"
    )
    long <- do.call(rbind, lapply(metrics, function(m) {
      data.frame(Profile = d$Profile, Metric = metric_labels[[m]], Value = suppressWarnings(as.numeric(d[[m]])), stringsAsFactors = FALSE)
    }))
    long <- long[is.finite(long$Value), , drop = FALSE]
    if (!nrow(long)) {
      plot.new(); text(0.5, 0.5, "No finite effectiveness values available."); return()
    }
    short_profile_names <- gsub("^([0-9]{4}-[0-9]{2}-[0-9]{2})_validated_cutoffs", "\\1", unique(long$Profile))
    short_profile_names <- gsub("_", "\n", short_profile_names)
    op <- par(mar = c(8.5, 6.2, 2.5, 1), xpd = FALSE)
    on.exit(par(op), add = TRUE)
    profile_names <- unique(long$Profile)
    metric_names <- unique(long$Metric)
    plot(NA, xlim = c(0.5, length(profile_names) + 0.5), ylim = c(0.5, length(metric_names) + 0.5),
         xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = "Profile Effectiveness Values")
    axis(1, at = seq_along(profile_names), labels = short_profile_names, las = 2, cex.axis = 0.72)
    axis(2, at = seq_along(metric_names), labels = metric_names, las = 1, cex.axis = 0.9)
    cols <- c("#173f35", "#9b2226", "#1d5f78", "#8a5a00")
    for (i in seq_len(nrow(long))) {
      x <- match(long$Profile[i], profile_names)
      y <- match(long$Metric[i], metric_names)
      vals <- long$Value[long$Metric == long$Metric[i]]
      scaled <- if (length(unique(vals)) > 1) (long$Value[i] - min(vals)) / (max(vals) - min(vals)) else 0.6
      points(x, y, pch = 19, cex = 1.2 + 2.2 * scaled, col = cols[((y - 1) %% length(cols)) + 1])
      text(x, y + 0.18, labels = format(round(long$Value[i], 3), trim = TRUE), cex = 0.72)
    }
    grid()
  })

  cutoff_review_data <- reactive({
    input$refresh_cutoff_review_btn
    s <- settings()
    p <- build_paths(s)
    read_csv_safe <- function(path) {
      if (!file.exists(path) || dir.exists(path)) return(data.frame())
      tryCatch(read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    }
    files <- data.frame(
      output = c(
        "parameter_optimization_results.csv",
        "filtered_rois_best_params.csv",
        "roi_analysis_raw.csv",
        "roi_analysis_raw.rds",
        "roi_analysis_raw_manifest.csv",
        "optimization_input_diagnostics.csv",
        "optimization_model_metadata.csv"
      ),
      path = c(
        p$param_results,
        p$filtered_rois,
        file.path(p$out_opt, "roi_analysis_raw.csv"),
        file.path(p$out_opt, "roi_analysis_raw.rds"),
        file.path(p$out_opt, "roi_analysis_raw_manifest.csv"),
        file.path(p$out_opt, "optimization_input_diagnostics.csv"),
        file.path(p$out_opt, "optimization_model_metadata.csv")
      ),
      stringsAsFactors = FALSE
    )
    files$exists <- file.exists(files$path)
    files$bytes <- ifelse(files$exists, file.info(files$path)$size, NA_real_)
    files$modified <- ifelse(files$exists, as.character(file.info(files$path)$mtime), "")

    list(
      paths = p,
      files = files,
      params = read_csv_safe(p$param_results),
      filtered = read_csv_safe(p$filtered_rois),
      raw = read_csv_safe(file.path(p$out_opt, "roi_analysis_raw.csv")),
      manifest = read_csv_safe(file.path(p$out_opt, "roi_analysis_raw_manifest.csv")),
      diagnostics = read_csv_safe(file.path(p$out_opt, "optimization_input_diagnostics.csv")),
      model_metadata = read_csv_safe(file.path(p$out_opt, "optimization_model_metadata.csv"))
    )
  })

  profile_roi_tables <- reactive({
    src <- cutoff_source_paths(input$cutoff_visual_profile %||% "__current__", settings())
    list(
      source = src,
      raw = read_csv_safe_file(src$raw),
      filtered = read_csv_safe_file(src$filtered)
    )
  })

  group_label_for_roi_row <- function(df) {
    if (!nrow(df)) return(character())
    high_neur <- if ("is_high_neurites" %in% names(df)) tolower(as.character(df$is_high_neurites)) %in% c("true", "t", "1", "yes") else rep(FALSE, nrow(df))
    low_neur <- if ("is_low_neurites" %in% names(df)) tolower(as.character(df$is_low_neurites)) %in% c("true", "t", "1", "yes") else rep(FALSE, nrow(df))
    high_conf <- if ("is_high_confluence" %in% names(df)) tolower(as.character(df$is_high_confluence)) %in% c("true", "t", "1", "yes") else rep(FALSE, nrow(df))
    low_conf <- if ("is_low_confluence" %in% names(df)) tolower(as.character(df$is_low_confluence)) %in% c("true", "t", "1", "yes") else rep(FALSE, nrow(df))
    ifelse(high_neur & low_conf, "High neurites / Low confluence",
      ifelse(low_neur & high_conf, "Low neurites / High confluence",
        ifelse(high_neur & high_conf, "High neurites / High confluence",
          ifelse(low_neur & low_conf, "Low neurites / Low confluence", "Unassigned"))))
  }

  confluence_column_for_roi_table <- function(df) {
    if (!nrow(df)) return(NA_character_)
    candidates <- names(df)[grepl("confluence", names(df), ignore.case = TRUE)]
    if (length(candidates)) return(candidates[[1]])
    candidates <- names(df)[grepl("background", names(df), ignore.case = TRUE)]
    if (length(candidates)) return(candidates[[1]])
    NA_character_
  }

  image_level_group_metric <- function(raw, filtered, metric) {
    if (!nrow(raw) || !"image_name" %in% names(raw) || !"skeleton_length" %in% names(raw)) return(data.frame())
    raw$.retained <- FALSE
    if (nrow(filtered) && all(c("image_name", "roi_name") %in% names(filtered)) && all(c("image_name", "roi_name") %in% names(raw))) {
      retained_key <- paste(filtered$image_name, filtered$roi_name, sep = "||")
      raw$.retained <- paste(raw$image_name, raw$roi_name, sep = "||") %in% retained_key
    }
    raw$.group <- group_label_for_roi_row(raw)
    raw$skeleton_length_num <- suppressWarnings(as.numeric(raw$skeleton_length))
    conf_col <- confluence_column_for_roi_table(raw)
    raw$.confluence <- NA_real_
    if (!is.na(conf_col) && nzchar(conf_col)) {
      raw$.confluence <- suppressWarnings(as.numeric(raw[[conf_col]]))
      if (grepl("background", conf_col, ignore.case = TRUE)) raw$.confluence <- 100 - raw$.confluence
    }
    one_summary <- function(df, retained_state) {
      rows <- lapply(split(df, df$image_name), function(x) {
        lengths <- x$skeleton_length_num[is.finite(x$skeleton_length_num)]
        if (!length(lengths)) return(NULL)
        confluence <- suppressWarnings(as.numeric(stats::median(x$.confluence, na.rm = TRUE)))
        if (!is.finite(confluence) || confluence <= 0) confluence <- NA_real_
        value <- switch(
          metric,
          image_total_skeleton_length = sum(lengths, na.rm = TRUE),
          image_mean_skeleton_length = mean(lengths, na.rm = TRUE),
          image_median_skeleton_length = stats::median(lengths, na.rm = TRUE),
          image_total_skeleton_length_per_confluence = sum(lengths, na.rm = TRUE) / confluence,
          image_median_skeleton_length_per_confluence = stats::median(lengths, na.rm = TRUE) / confluence,
          stats::median(lengths, na.rm = TRUE)
        )
        data.frame(
          image_name = x$image_name[[1]],
          roi_name = "",
          .group = x$.group[[1]],
          .value = value,
          .retained = retained_state,
          .plot_level = "Image summary",
          .n_rois = length(lengths),
          .confluence = confluence,
          stringsAsFactors = FALSE
        )
      })
      rows <- Filter(Negate(is.null), rows)
      if (!length(rows)) data.frame() else do.call(rbind, rows)
    }
    all_summary <- one_summary(raw, FALSE)
    retained_summary <- one_summary(raw[raw$.retained, , drop = FALSE], TRUE)
    rbind(all_summary, retained_summary)
  }

  cutoff_group_metric_label <- function(metric) {
    switch(
      metric,
      skeleton_length = "ROI-level skeleton length",
      image_total_skeleton_length = "Image-level total retained skeleton length",
      image_mean_skeleton_length = "Image-level mean retained skeleton length",
      image_median_skeleton_length = "Image-level median retained skeleton length",
      image_total_skeleton_length_per_confluence = "Image-level total retained skeleton length / image confluence",
      image_median_skeleton_length_per_confluence = "Image-level median retained skeleton length / image confluence",
      area = "ROI-level area",
      circularity = "ROI-level circularity",
      aspect_ratio = "ROI-level aspect ratio",
      metric
    )
  }

  output$cutoff_group_profile_picker_ui <- renderUI({
    choices <- cutoff_profile_choices()
    selected <- input$cutoff_group_profile %||% input$cutoff_visual_profile %||% "__current__"
    if (!selected %in% unname(choices)) selected <- "__current__"
    selectInput("cutoff_group_profile", "Model/profile shown in four-group plot", choices = choices, selected = selected)
  })

  output$cutoff_group_metric_picker_ui <- renderUI({
    selectInput(
      "cutoff_group_metric",
      "Metric to compare across the four validation groups",
      choices = c(
        "ROI-level skeleton length (individual ROI)" = "skeleton_length",
        "Image-level total retained skeleton length" = "image_total_skeleton_length",
        "Image-level mean retained skeleton length" = "image_mean_skeleton_length",
        "Image-level median retained skeleton length" = "image_median_skeleton_length",
        "Image-level total retained skeleton length / confluence" = "image_total_skeleton_length_per_confluence",
        "Image-level median retained skeleton length / confluence" = "image_median_skeleton_length_per_confluence",
        "ROI-level area" = "area",
        "ROI-level circularity" = "circularity",
        "ROI-level aspect ratio" = "aspect_ratio"
      ),
      selected = "image_median_skeleton_length"
    )
  })

  cutoff_group_plot_data <- reactive({
    src <- cutoff_source_paths(input$cutoff_group_profile %||% "__current__", settings())
    raw <- read_csv_safe_file(src$raw)
    filtered <- read_csv_safe_file(src$filtered)
    metric <- input$cutoff_group_metric %||% "skeleton_length"
    image_level_metric <- startsWith(metric, "image_")
    if (!nrow(raw)) return(list(ok = FALSE, message = "No ROI table available.", src = src, metric = metric))
    if (image_level_metric) {
      plot_df <- image_level_group_metric(raw, filtered, metric)
      if (!nrow(plot_df)) return(list(ok = FALSE, message = "No image-level summary values could be computed. Check that skeleton_length and image_name columns exist.", src = src, metric = metric))
    } else {
      if (!metric %in% names(raw)) return(list(ok = FALSE, message = "No ROI table or selected metric available.", src = src, metric = metric))
    raw$.retained <- FALSE
    if (nrow(filtered) && all(c("image_name", "roi_name") %in% names(filtered)) && all(c("image_name", "roi_name") %in% names(raw))) {
      retained_key <- paste(filtered$image_name, filtered$roi_name, sep = "||")
      raw$.retained <- paste(raw$image_name, raw$roi_name, sep = "||") %in% retained_key
    }
    raw$.group <- group_label_for_roi_row(raw)
    raw$.value <- suppressWarnings(as.numeric(raw[[metric]]))
    plot_df <- raw[is.finite(raw$.value) & raw$.group != "Unassigned", , drop = FALSE]
      plot_df$.plot_level <- "ROI"
      plot_df$.n_rois <- 1L
      plot_df$.confluence <- NA_real_
    }
    if (!nrow(plot_df)) return(list(ok = FALSE, message = "No finite group-level ROI values available.", src = src, metric = metric))
    groups <- c("Low neurites / Low confluence", "Low neurites / High confluence", "High neurites / Low confluence", "High neurites / High confluence")
    group_labels <- c("Low N\nLow C", "Low N\nHigh C", "High N\nLow C", "High N\nHigh C")
    plot_df$.group <- factor(plot_df$.group, levels = groups)
    set.seed(42)
    plot_df$.x <- as.numeric(plot_df$.group) + runif(nrow(plot_df), -0.18, 0.18)
    retained_values <- plot_df$.value[plot_df$.retained & is.finite(plot_df$.value)]
    scale_from_retained <- image_level_metric && length(retained_values) >= 1L
    y_range <- range(if (scale_from_retained) retained_values else plot_df$.value, na.rm = TRUE)
    if (all(is.finite(y_range)) && diff(y_range) == 0) {
      padding <- max(abs(y_range[[1]]) * 0.08, 1)
      y_range <- y_range + c(-padding, padding)
    } else if (all(is.finite(y_range))) {
      padding <- diff(y_range) * 0.08
      y_range <- y_range + c(-padding, padding)
    } else {
      y_range <- NULL
    }
    list(ok = TRUE, plot_df = plot_df, groups = groups, group_labels = group_labels, src = src, metric = metric, metric_label = cutoff_group_metric_label(metric), y_range = y_range, scale_from_retained = scale_from_retained)
  })

  output$cutoff_group_dot_plot_ui <- renderUI({
    if (requireNamespace("plotly", quietly = TRUE)) {
      plotly::plotlyOutput("cutoff_group_dot_plot_interactive", height = "420px", width = "100%")
    } else {
      tagList(
        tags$p(class = "warning-text", "Install the R package 'plotly' to enable hover tooltips in this plot. Showing static fallback."),
        plotOutput("cutoff_group_dot_plot", height = "380px", width = "100%")
      )
    }
  })

  output$cutoff_group_dot_plot <- renderPlot({
    on.exit(remember_plot("cutoff_group_dot_plot"), add = TRUE)
    pd <- cutoff_group_plot_data()
    if (!isTRUE(pd$ok)) {
      plot.new(); text(0.5, 0.5, pd$message); return()
    }
    plot_df <- pd$plot_df
    groups <- pd$groups
    group_labels <- pd$group_labels
    metric <- pd$metric
    metric_label <- pd$metric_label
    src <- pd$src
    subtitle <- if (isTRUE(pd$scale_from_retained)) " (y-axis scaled to retained summaries)" else ""
    op <- par(mar = c(6.8, 4.6, 2.5, 1), xpd = FALSE)
    on.exit(par(op), add = TRUE)
    plot(
      plot_df$.x,
      plot_df$.value,
      xaxt = "n",
      xlab = "",
      ylab = metric_label,
      pch = 19,
      col = "#b8322a28",
      ylim = pd$y_range,
      main = paste0(metric_label, " by Validation Group", subtitle, ": ", src$label)
    )
    axis(1, at = seq_along(groups), labels = group_labels, las = 1, cex.axis = 0.84)
    retained_df <- plot_df[plot_df$.retained, , drop = FALSE]
    if (nrow(retained_df)) {
      points(retained_df$.x, retained_df$.value, pch = 19, cex = 1.15, col = "#00c853DD")
      points(retained_df$.x, retained_df$.value, pch = 1, cex = 1.25, col = "#003b16")
    }
    for (i in seq_along(groups)) {
      vals <- plot_df$.value[plot_df$.group == groups[i] & plot_df$.retained]
      if (length(vals)) {
        points(i - 0.06, median(vals, na.rm = TRUE), pch = 23, cex = 1.8, bg = "#00ff40", col = "#173f35")
        points(i + 0.06, mean(vals, na.rm = TRUE), pch = 24, cex = 1.7, bg = "#4cc9f0", col = "#173f35")
      }
    }
    legend("topright", legend = c("All candidates", "Retained", "Retained median", "Retained mean"), pch = c(19, 19, 23, 24), col = c("#b8322a", "#00c853", "#173f35", "#173f35"), pt.bg = c(NA, NA, "#00ff40", "#4cc9f0"), bty = "n", cex = 0.82)
  })

  if (requireNamespace("plotly", quietly = TRUE)) {
    output$cutoff_group_dot_plot_interactive <- plotly::renderPlotly({
      pd <- cutoff_group_plot_data()
      if (!isTRUE(pd$ok)) {
        return(plotly::plot_ly() %>% plotly::layout(title = pd$message))
      }
      plot_df <- pd$plot_df
      metric <- pd$metric
      metric_label <- pd$metric_label
      src <- pd$src
      plot_df$.status <- ifelse(plot_df$.retained, "Retained ROI", "All candidate ROIs")
      plot_df$.hover <- paste0(
        "Profile: ", src$label,
        "<br>Image: ", plot_df$image_name %||% "",
        "<br>ROI: ", plot_df$roi_name %||% "",
        "<br>Group: ", as.character(plot_df$.group),
        "<br>Level: ", plot_df$.plot_level %||% "",
        "<br>Status: ", plot_df$.status,
        "<br>", metric_label, ": ", signif(plot_df$.value, 5),
        "<br>ROIs summarized: ", plot_df$.n_rois %||% "",
        "<br>Image confluence: ", ifelse(is.finite(plot_df$.confluence), signif(plot_df$.confluence, 5), "NA")
      )
      med <- do.call(rbind, lapply(pd$groups, function(g) {
        vals <- plot_df$.value[as.character(plot_df$.group) == g & plot_df$.retained]
        if (!length(vals)) return(NULL)
        data.frame(.group = g, .x = match(g, pd$groups) - 0.06, .value = median(vals, na.rm = TRUE), .hover = paste0("Retained median<br>Group: ", g, "<br>", metric_label, ": ", signif(median(vals, na.rm = TRUE), 5)), stringsAsFactors = FALSE)
      }))
      avg <- do.call(rbind, lapply(pd$groups, function(g) {
        vals <- plot_df$.value[as.character(plot_df$.group) == g & plot_df$.retained]
        if (!length(vals)) return(NULL)
        data.frame(.group = g, .x = match(g, pd$groups) + 0.06, .value = mean(vals, na.rm = TRUE), .hover = paste0("Retained mean<br>Group: ", g, "<br>", metric_label, ": ", signif(mean(vals, na.rm = TRUE), 5)), stringsAsFactors = FALSE)
      }))
      p <- plotly::plot_ly()
      candidates <- plot_df[!plot_df$.retained, , drop = FALSE]
      retained <- plot_df[plot_df$.retained, , drop = FALSE]
      if (nrow(candidates)) {
        p <- plotly::add_markers(p, data = candidates, x = ~.x, y = ~.value, text = ~.hover, hoverinfo = "text", name = "All ROI candidates", marker = list(color = "rgba(184,50,42,0.25)", size = 6))
      }
      if (nrow(retained)) {
        p <- plotly::add_markers(p, data = retained, x = ~.x, y = ~.value, text = ~.hover, hoverinfo = "text", name = "Retained ROI", marker = list(color = "rgba(0,200,83,0.9)", size = 8, line = list(color = "#003b16", width = 1.2)))
      }
      if (is.data.frame(med) && nrow(med)) {
        p <- plotly::add_markers(p, data = med, x = ~.x, y = ~.value, text = ~.hover, hoverinfo = "text", name = "Retained median", marker = list(color = "#00ff40", symbol = "diamond", size = 13, line = list(color = "#173f35", width = 1.5)))
      }
      if (is.data.frame(avg) && nrow(avg)) {
        p <- plotly::add_markers(p, data = avg, x = ~.x, y = ~.value, text = ~.hover, hoverinfo = "text", name = "Retained mean", marker = list(color = "#4cc9f0", symbol = "triangle-up", size = 13, line = list(color = "#173f35", width = 1.5)))
      }
      p <- plotly::layout(
        p = p,
        title = paste0(metric_label, " by Validation Group", if (isTRUE(pd$scale_from_retained)) " (y-axis scaled to retained summaries)" else "", ": ", src$label),
        xaxis = list(title = "", tickmode = "array", tickvals = seq_along(pd$groups), ticktext = c("Low N<br>Low C", "Low N<br>High C", "High N<br>Low C", "High N<br>High C")),
        yaxis = if (!is.null(pd$y_range)) list(title = metric_label, range = pd$y_range) else list(title = metric_label),
        legend = list(orientation = "h", x = 0, y = -0.2)
      )
      p
    })
  }

  output$cutoff_visual_profile_picker_ui <- renderUI({
    choices <- cutoff_profile_choices()
    selected <- input$cutoff_visual_profile %||% "__current__"
    if (!selected %in% unname(choices)) selected <- "__current__"
    selectInput("cutoff_visual_profile", "Profile for ROI overlay", choices = choices, selected = selected)
  })

  output$cutoff_visual_group_picker_ui <- renderUI({
    tables <- profile_roi_tables()
    raw <- tables$raw
    if (!nrow(raw)) {
      return(selectInput("cutoff_visual_group", "Validation group", choices = c("No ROI rows available" = "")))
    }
    raw$.group <- group_label_for_roi_row(raw)
    groups <- sort(unique(raw$.group[nzchar(raw$.group)]))
    groups <- groups[groups != "Unassigned"]
    choices <- c("All groups" = "__all__", setNames(groups, groups))
    selected <- input$cutoff_visual_group %||% "__all__"
    if (!selected %in% unname(choices)) selected <- "__all__"
    selectInput("cutoff_visual_group", "Validation group", choices = choices, selected = selected)
  })

  cutoff_visual_image_choices <- reactive({
    tables <- profile_roi_tables()
    raw <- tables$raw
    if (!nrow(raw) || !"image_name" %in% names(raw)) return(character())
    raw$.group <- group_label_for_roi_row(raw)
    group <- input$cutoff_visual_group %||% "__all__"
    if (!identical(group, "__all__")) raw <- raw[raw$.group == group, , drop = FALSE]
    sort(unique(raw$image_name))
  })

  output$cutoff_visual_image_picker_ui <- renderUI({
    imgs <- cutoff_visual_image_choices()
    if (!length(imgs)) return(selectInput("cutoff_visual_image", "Validation image", choices = c("No images available" = "")))
    selected <- input$cutoff_visual_image %||% imgs[[1]]
    if (!selected %in% imgs) selected <- imgs[[1]]
    selectInput("cutoff_visual_image", "Validation image", choices = setNames(imgs, imgs), selected = selected)
  })

  output$cutoff_visual_summary_ui <- renderUI({
    tables <- profile_roi_tables()
    img <- input$cutoff_visual_image %||% ""
    raw <- tables$raw
    filtered <- tables$filtered
    raw_n <- if (nrow(raw) && nzchar(img)) sum(raw$image_name == img, na.rm = TRUE) else 0L
    filtered_n <- if (nrow(filtered) && nzchar(img)) sum(filtered$image_name == img, na.rm = TRUE) else 0L
    tags$div(
      class = "step-summary-box step-summary-neutral",
      tags$strong("Selected image ROI counts"),
      tags$p(sprintf("Raw ROI rows: %s", raw_n)),
      tags$p(sprintf("Retained ROI rows: %s", filtered_n))
    )
  })

  find_segmentation_mask_for_image <- function(image_name, settings) {
    p <- build_paths(settings)
    if (!dir.exists(p$out_seg) || !nzchar(image_name %||% "")) return(NA_character_)
    files <- list.files(p$out_seg, pattern = "\\.(tif|tiff|png|jpg|jpeg)$", full.names = TRUE, ignore.case = TRUE)
    if (!length(files)) return(NA_character_)
    b <- tools::file_path_sans_ext(basename(files))
    hit <- files[b == paste0(image_name, "_RGavg_mask") | b == paste0(image_name, "_RGavg_mask_renorm") | b == image_name]
    if (length(hit)) return(hit[[1]])
    hit <- files[grepl(image_name, b, fixed = TRUE)]
    if (length(hit)) hit[[1]] else NA_character_
  }

  find_raw_image_for_roi_image <- function(image_name, settings) {
    p <- build_paths(settings)
    search_dirs <- unique(c(p$pre_renamed, p$out_clear, p$input_master))
    search_dirs <- search_dirs[dir.exists(search_dirs)]
    if (!length(search_dirs) || !nzchar(image_name %||% "")) return(NA_character_)
    patterns <- c(
      paste0("^", gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", image_name), "\\.(tif|tiff|png|jpg|jpeg)$"),
      paste0("^", gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", sub("_RGavg_mask(_renorm)?$", "", image_name)), "(_RGavg)?\\.(tif|tiff|png|jpg|jpeg)$")
    )
    files <- unlist(lapply(search_dirs, function(d) {
      list.files(d, pattern = "\\.(tif|tiff|png|jpg|jpeg)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
    }), use.names = FALSE)
    if (!length(files)) return(NA_character_)
    b <- basename(files)
    for (pat in patterns) {
      hit <- files[grepl(pat, b, ignore.case = TRUE, perl = TRUE)]
      if (length(hit)) return(hit[[1]])
    }
    stem <- sub("_RGavg_mask(_renorm)?$", "", image_name)
    hit <- files[grepl(stem, tools::file_path_sans_ext(b), fixed = TRUE)]
    if (length(hit)) hit[[1]] else NA_character_
  }

  load_roi_object_for_visual <- function(source, image_name) {
    if (!file.exists(source$opt_rois_rds) || !nzchar(image_name %||% "")) return(NULL)
    opt <- tryCatch(readRDS(source$opt_rois_rds), error = function(e) NULL)
    if (is.null(opt)) return(NULL)
    if (image_name %in% names(opt)) return(opt[[image_name]])
    hit <- names(opt)[grepl(image_name, names(opt), fixed = TRUE) | grepl(names(opt), image_name, fixed = TRUE)]
    if (length(hit)) opt[[hit[[1]]]] else NULL
  }

  output$cutoff_visual_roi_overlay_plot <- renderPlot({
    on.exit(remember_plot("cutoff_visual_roi_overlay_plot"), add = TRUE)
    input$cutoff_visual_refresh_btn
    if (!cutoff_tab_active("2.C.2. Image and ROI Overlay Visualization")) {
      plot.new(); text(0.5, 0.5, "Open 2.C.2 to render image overlays."); return()
    }
    tables <- profile_roi_tables()
    source <- tables$source
    raw <- tables$raw
    filtered <- tables$filtered
    img <- input$cutoff_visual_image %||% ""
    if (!nzchar(img)) {
      plot.new(); text(0.5, 0.5, "Select an image to preview."); return()
    }
    roi_obj <- load_roi_object_for_visual(source, img)
    if (is.null(roi_obj) || is.null(roi_obj$rois) || !length(roi_obj$rois)) {
      plot.new(); text(0.5, 0.5, "ROI geometry was not available for this image/profile."); return()
    }
    retained_names <- if (nrow(filtered) && all(c("image_name", "roi_name") %in% names(filtered))) {
      filtered$roi_name[filtered$image_name == img]
    } else character()
    retained_names <- unique(retained_names)
    roi_names <- names(roi_obj$rois)
    all_coords <- do.call(rbind, lapply(roi_obj$rois, function(r) if (!is.null(r$coords)) r$coords else NULL))
    if (is.null(all_coords) || !nrow(all_coords)) {
      plot.new(); text(0.5, 0.5, "ROI coordinate data was empty for this image."); return()
    }
    raw_path <- find_raw_image_for_roi_image(img, settings())
    mask_path <- find_segmentation_mask_for_image(img, settings())
    plotted_raster <- FALSE
    width <- max(all_coords[, 1], na.rm = TRUE)
    height <- max(all_coords[, 2], na.rm = TRUE)
    if (!is.na(raw_path) && file.exists(raw_path) && requireNamespace("magick", quietly = TRUE)) {
      raw_img <- tryCatch(magick::image_read(raw_path), error = function(e) NULL)
      if (!is.null(raw_img)) {
        info <- magick::image_info(raw_img)
        width <- max(width, info$width[[1]], na.rm = TRUE)
        height <- max(height, info$height[[1]], na.rm = TRUE)
        op <- par(mar = c(2, 2, 2, 1))
        on.exit(par(op), add = TRUE)
        plot(NA, xlim = c(0, width), ylim = c(height, 0), asp = 1, xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = paste("Retained ROI overlay:", img))
        rasterImage(as.raster(raw_img), 0, height, width, 0)
        plotted_raster <- TRUE
      }
    }
    if (!plotted_raster && isTRUE(input$cutoff_visual_show_mask) && !is.na(mask_path) && file.exists(mask_path) && requireNamespace("magick", quietly = TRUE)) {
      mask <- tryCatch(magick::image_read(mask_path), error = function(e) NULL)
      if (!is.null(mask)) {
        info <- magick::image_info(mask)
        width <- max(width, info$width[[1]], na.rm = TRUE)
        height <- max(height, info$height[[1]], na.rm = TRUE)
        op <- par(mar = c(2, 2, 2, 1))
        on.exit(par(op), add = TRUE)
        plot(NA, xlim = c(0, width), ylim = c(height, 0), asp = 1, xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = paste("Retained ROI overlay:", img))
        rasterImage(as.raster(mask), 0, height, width, 0)
        plotted_raster <- TRUE
      }
    }
    if (!plotted_raster) {
      op <- par(mar = c(2, 2, 2, 1))
      on.exit(par(op), add = TRUE)
      plot(NA, xlim = range(all_coords[, 1], na.rm = TRUE), ylim = rev(range(all_coords[, 2], na.rm = TRUE)), asp = 1, xlab = "", ylab = "", xaxt = "n", yaxt = "n", main = paste("Retained ROI overlay:", img))
    }
    draw_roi <- function(r, col, lwd = 1) {
      coords <- r$coords
      if (is.null(coords) || !nrow(coords)) return()
      polygon(coords[, 1], coords[, 2], border = col, col = NA, lwd = lwd)
    }
    if (plotted_raster && isTRUE(input$cutoff_visual_show_mask) && !is.na(mask_path) && file.exists(mask_path) && requireNamespace("magick", quietly = TRUE)) {
      mask <- tryCatch(magick::image_read(mask_path), error = function(e) NULL)
      if (!is.null(mask)) {
        mask <- tryCatch(magick::image_colorize(mask, opacity = 35, color = "#ffd166"), error = function(e) mask)
        rasterImage(as.raster(mask), 0, height, width, 0)
      }
    }
    if (isTRUE(input$cutoff_visual_show_all_rois)) {
      invisible(lapply(roi_obj$rois, draw_roi, col = "#ff2d2d88", lwd = 0.7))
    }
    if (isTRUE(input$cutoff_visual_show_retained) && length(retained_names)) {
      keep <- intersect(retained_names, roi_names)
      invisible(lapply(roi_obj$rois[keep], draw_roi, col = "#00ff40", lwd = 1.7))
    }
    legend("topright", legend = c("Retained ROI", "All ROI candidates", "Mask layer"), col = c("#00ff40", "#ff2d2d", "#ffd166"), lwd = c(2, 1, 6), bty = "n")
  })

  cutoff_review_checks <- reactive({
    d <- cutoff_review_data()
    params <- d$params
    filtered <- d$filtered
    raw <- d$raw
    manifest <- d$manifest
    diagnostics <- d$diagnostics
    model_metadata <- d$model_metadata
    add_check <- function(check, status, detail) {
      data.frame(Check = check, Status = status, Detail = detail, stringsAsFactors = FALSE)
    }
    checks <- list()

    checks[[length(checks) + 1L]] <- add_check(
      "Optimization result file",
      if (nrow(params)) "OK" else "Missing/empty",
      if (nrow(params)) paste(nrow(params), "parameter rows found") else "Run 2.A.6 or apply a saved profile that contains parameter_optimization_results.csv."
    )
    checks[[length(checks) + 1L]] <- add_check(
      "ROI analysis checkpoint",
      if (nrow(raw) || file.exists(file.path(d$paths$out_opt, "roi_analysis_raw.rds"))) "OK" else "Missing",
      if (nrow(raw)) paste(nrow(raw), "ROI rows available for review/reuse") else "The optimizer can still run, but it may need to rebuild the expensive ROI table."
    )
    checks[[length(checks) + 1L]] <- add_check(
      "Filtered ROI output",
      if (nrow(filtered)) "OK" else "Missing/empty",
      if (nrow(filtered)) paste(nrow(filtered), "filtered ROI rows found") else "No filtered_rois_best_params.csv was found, so the selected cutoffs may not have completed."
    )

    required_param_cols <- c("score", "n_retained", "min_area", "max_area", "min_circularity", "max_circularity", "min_aspect_ratio", "max_aspect_ratio")
    missing_param_cols <- setdiff(required_param_cols, names(params))
    checks[[length(checks) + 1L]] <- add_check(
      "Required optimization columns",
      if (!length(missing_param_cols)) "OK" else "Problem",
      if (!length(missing_param_cols)) "All key cutoff/result columns are present." else paste("Missing:", paste(missing_param_cols, collapse = ", "))
    )

    if (nrow(params) && "score" %in% names(params)) {
      scores <- suppressWarnings(as.numeric(params$score))
      finite_scores <- scores[is.finite(scores)]
      checks[[length(checks) + 1L]] <- add_check(
        "Finite optimization scores",
        if (length(finite_scores) > 1) "OK" else "Problem",
        paste(length(finite_scores), "finite score(s) out of", nrow(params), "parameter rows")
      )
      if (length(finite_scores) >= 2) {
        sorted <- sort(finite_scores, decreasing = TRUE)
        gap <- sorted[1] - sorted[2]
        rel_gap <- gap / (abs(sorted[1]) + 1e-9)
        checks[[length(checks) + 1L]] <- add_check(
          "Best score separation",
          if (rel_gap >= 0.02) "OK" else "Caution",
          sprintf("Best-vs-second score gap is %.4f (%.2f%% of best score). Small gaps mean several cutoffs are similarly plausible.", gap, 100 * rel_gap)
        )
      }
    }

    if (nrow(raw)) {
      raw_signal <- if ("is_signal" %in% names(raw)) sum(as.logical(raw$is_signal), na.rm = TRUE) else NA_integer_
      raw_noise <- if ("is_noise" %in% names(raw)) sum(as.logical(raw$is_noise), na.rm = TRUE) else NA_integer_
      checks[[length(checks) + 1L]] <- add_check(
        "Manual signal/noise labels",
        if (is.finite(raw_signal) && is.finite(raw_noise) && raw_signal > 0 && raw_noise > 0) "OK" else "Caution",
        paste("Raw ROI rows labelled signal:", raw_signal %||% NA, "| noise:", raw_noise %||% NA, ". If one class is absent, signal/noise scoring is weaker.")
      )
      metric_cols <- intersect(c("area", "circularity", "aspect_ratio", "skeleton_length"), names(raw))
      empty_metrics <- metric_cols[vapply(metric_cols, function(x) all(is.na(suppressWarnings(as.numeric(raw[[x]])))), logical(1))]
      constant_metrics <- metric_cols[vapply(metric_cols, function(x) {
        vals <- suppressWarnings(as.numeric(raw[[x]]))
        vals <- vals[is.finite(vals)]
        length(unique(vals)) <= 1
      }, logical(1))]
      checks[[length(checks) + 1L]] <- add_check(
        "Metric fields meaningful",
        if (!length(empty_metrics) && !length(constant_metrics)) "OK" else "Problem",
        if (!length(empty_metrics) && !length(constant_metrics)) "Area/circularity/aspect/skeleton fields contain usable variation." else paste("Empty metrics:", paste(empty_metrics, collapse = ", "), "| constant metrics:", paste(constant_metrics, collapse = ", "))
      )
    }

    if (nrow(raw) && nrow(filtered)) {
      retention <- nrow(filtered) / nrow(raw)
      checks[[length(checks) + 1L]] <- add_check(
        "ROI retention rate",
        if (retention >= 0.01 && retention <= 0.95) "OK" else "Caution",
        sprintf("%.2f%% of ROI rows retained. Very low retention can be over-stringent; very high retention can mean cutoffs are not selective.", 100 * retention)
      )
    }

    if (nrow(manifest)) {
      checks[[length(checks) + 1L]] <- add_check(
        "Checkpoint manifest",
        "OK",
        paste("Created:", manifest$created_at[1] %||% "", "| expected images:", manifest$expected_images[1] %||% "", "| expected ROI objects:", manifest$expected_roi_objects[1] %||% "")
      )
    }
    if (nrow(diagnostics)) {
      checks[[length(checks) + 1L]] <- add_check(
        "Optimizer diagnostics",
        diagnostics$status[1] %||% "Available",
        diagnostics$note[1] %||% "Diagnostics file is present."
      )
    }
    if (nrow(model_metadata)) {
      mode <- if (all(c("key", "value") %in% names(model_metadata))) {
        hit <- model_metadata$value[model_metadata$key == "optimization_mode"]
        if (length(hit)) hit[[1]] else "available"
      } else {
        "available"
      }
      checks[[length(checks) + 1L]] <- add_check(
        "Optimization model metadata",
        "OK",
        paste("Model metadata is present. Optimization mode:", mode)
      )
    } else {
      checks[[length(checks) + 1L]] <- add_check(
        "Optimization model metadata",
        "Caution",
        "optimization_model_metadata.csv is missing. Older profiles may not record whether classic, continuity-aware, or topology-aware scoring was used."
      )
    }

    do.call(rbind, checks)
  })

  output$cutoff_review_overall_ui <- renderUI({
    checks <- cutoff_review_checks()
    if (!nrow(checks)) return(tags$div(class = "step-status-pill not-ready", "No cutoff review data available yet."))
    problems <- sum(checks$Status %in% c("Problem", "Missing", "Missing/empty"), na.rm = TRUE)
    cautions <- sum(checks$Status %in% c("Caution"), na.rm = TRUE)
    cls <- if (problems > 0) "not-ready" else if (cautions > 0) "partial" else "ready"
    icon <- if (problems > 0) "×" else if (cautions > 0) "◔" else "✓"
    label <- if (problems > 0) "Cutoff review found blocking problems" else if (cautions > 0) "Cutoff review found cautions" else "Cutoff outputs look internally consistent"
    tags$div(
      class = paste("step-status-pill", cls),
      tags$span(class = "step-status-icon", icon),
      tags$div(tags$strong(label), tags$br(), sprintf("%d problem(s), %d caution(s), %d total check(s)", problems, cautions, nrow(checks)))
    )
  })

  output$cutoff_review_checks_table <- renderTable({
    apply_table_value_filter("cutoff_review_checks_table", cutoff_review_checks())
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$cutoff_review_files_table <- renderTable({
    d <- cutoff_review_data()
    apply_table_value_filter("cutoff_review_files_table", transform(d$files, bytes = ifelse(is.na(bytes), "", format(bytes, big.mark = ","))))
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  metadata_image_key <- function(x) {
    x <- basename(as.character(x %||% ""))
    x <- sub("\\.(tif|tiff|png|jpg|jpeg)$", "", x, ignore.case = TRUE)
    x <- sub("_RGavg_mask_renorm$", "", x, ignore.case = TRUE)
    x <- sub("_RGavg_mask$", "", x, ignore.case = TRUE)
    x <- sub("_mask_renorm$", "", x, ignore.case = TRUE)
    x <- sub("_mask$", "", x, ignore.case = TRUE)
    x <- sub("_RGavg$", "", x, ignore.case = TRUE)
    tolower(gsub("[^A-Za-z0-9]+", "", x))
  }

  read_metadata_table <- reactive({
    p <- build_paths(settings())
    meta <- read_csv_safe_file(p$metadata)
    if (!nrow(meta)) return(meta)
    image_col <- if ("image_name" %in% names(meta)) {
      "image_name"
    } else {
      candidates <- names(meta)[grepl("image|file|name", names(meta), ignore.case = TRUE)]
      if (length(candidates)) candidates[[1]] else names(meta)[[1]]
    }
    meta$.metadata_image_col <- image_col
    meta$.image_key <- metadata_image_key(meta[[image_col]])
    meta
  })

  read_mask_area_feature_table <- reactive({
    p <- build_paths(settings())
    mask <- read_csv_safe_file(p$mask_percentages)
    if (!nrow(mask)) return(mask)
    image_col <- if ("image_name" %in% names(mask)) {
      "image_name"
    } else {
      candidates <- names(mask)[grepl("image|file|name", names(mask), ignore.case = TRUE)]
      if (length(candidates)) candidates[[1]] else names(mask)[[1]]
    }
    mask$.image_key <- metadata_image_key(mask[[image_col]])
    numeric_copy <- function(column_name) {
      if (column_name %in% names(mask)) suppressWarnings(as.numeric(mask[[column_name]])) else NA_real_
    }
    first_matching_col <- function(candidates) {
      hits <- names(mask)[tolower(names(mask)) %in% candidates]
      if (length(hits)) hits[[1]] else ""
    }
    background_col <- first_matching_col(c("background%", "background_percent", "background"))
    cell_col <- first_matching_col(c("cell_bodies_percent", "cell_body_percent", "cell_percent"))
    neurite_col <- first_matching_col(c("neurites_percent", "neurite_percent"))
    total_col <- first_matching_col(c("total_area_px", "total_area", "area_px"))
    out <- data.frame(
      .image_key = mask$.image_key,
      mask_image_name = as.character(mask[[image_col]]),
      neurites_percent = numeric_copy(neurite_col),
      cell_bodies_percent = numeric_copy(cell_col),
      background_percent = numeric_copy(background_col),
      total_area_px = numeric_copy(total_col),
      stringsAsFactors = FALSE
    )
    out$confluence <- 100 - out$background_percent
    out$image_neurites_percent <- out$neurites_percent
    out$image_cell_bodies_percent <- out$cell_bodies_percent
    out$image_background_percent <- out$background_percent
    out$image_total_area_px <- out$total_area_px
    out$image_confluence_mask_area <- out$confluence
    out <- out[!duplicated(out$.image_key), , drop = FALSE]
    out
  })

  enrich_with_mask_area_features <- function(df) {
    if (!nrow(df)) return(df)
    image_col <- if ("image_name" %in% names(df)) "image_name" else ""
    if (!".image_key" %in% names(df) && nzchar(image_col)) {
      df$.image_key <- metadata_image_key(df[[image_col]])
    }
    mask <- read_mask_area_feature_table()
    if (!nrow(mask)) return(df)
    if (nzchar(image_col)) {
      df$.mask_area_join_key <- metadata_image_key(df[[image_col]])
    } else if (".image_key" %in% names(df)) {
      df$.mask_area_join_key <- metadata_image_key(df$.image_key)
    } else {
      return(df)
    }
    mask$.mask_area_join_key <- mask$.image_key
    mask_feature_cols <- c(
      "mask_image_name",
      "neurites_percent",
      "cell_bodies_percent",
      "background_percent",
      "total_area_px",
      "confluence",
      "image_neurites_percent",
      "image_cell_bodies_percent",
      "image_background_percent",
      "image_total_area_px",
      "image_confluence_mask_area"
    )
    mask_feature_cols <- intersect(mask_feature_cols, names(mask))
    if (!length(mask_feature_cols)) {
      df$.mask_area_join_key <- NULL
      return(df)
    }
    df <- df[, setdiff(names(df), mask_feature_cols), drop = FALSE]
    out <- merge(
      df,
      mask[, c(".mask_area_join_key", mask_feature_cols), drop = FALSE],
      by = ".mask_area_join_key",
      all.x = TRUE,
      sort = FALSE
    )
    out$.mask_area_join_key <- NULL
    confluence_for_composites <- if ("image_confluence" %in% names(out)) {
      suppressWarnings(as.numeric(out$image_confluence))
    } else if ("image_confluence_mask_area" %in% names(out)) {
      suppressWarnings(as.numeric(out$image_confluence_mask_area))
    } else if ("confluence" %in% names(out)) {
      suppressWarnings(as.numeric(out$confluence))
    } else {
      rep(NA_real_, nrow(out))
    }
    invalid_confluence <- !is.finite(confluence_for_composites) | confluence_for_composites <= 0
    confluence_for_composites[invalid_confluence] <- NA_real_
    if ("image_total_skeleton_length" %in% names(out)) {
      out$image_total_skeleton_length_per_confluence <- suppressWarnings(as.numeric(out$image_total_skeleton_length)) / confluence_for_composites
    }
    if ("image_median_skeleton_length" %in% names(out) && !"image_median_skeleton_length_per_confluence" %in% names(out)) {
      out$image_median_skeleton_length_per_confluence <- suppressWarnings(as.numeric(out$image_median_skeleton_length)) / confluence_for_composites
    }
    out
  }

  generalization_feature_choices <- function() {
    c(
      "ROI-level skeleton length: retained mean per image" = "roi_mean_skeleton_length",
      "ROI-level skeleton length: retained median per image" = "roi_median_skeleton_length",
      "ROI-level skeleton length: retained total per image" = "image_total_skeleton_length",
      "Image-level mean retained skeleton length" = "image_mean_skeleton_length",
      "Image-level median retained skeleton length" = "image_median_skeleton_length",
      "Image-level total retained skeleton length / image confluence" = "image_total_skeleton_length_per_confluence",
      "Image-level median retained skeleton length / image confluence" = "image_median_skeleton_length_per_confluence",
      "ROI-level area: retained mean per image" = "roi_mean_area",
      "ROI-level circularity: retained mean per image" = "roi_mean_circularity",
      "ROI-level aspect ratio: retained mean per image" = "roi_mean_aspect_ratio",
      "Retained ROI count" = "retained_roi_count",
      "Retained fraction of candidate ROIs" = "retained_roi_fraction",
      "Image-level neurite area percent" = "neurites_percent",
      "Image-level cell-body area percent" = "cell_bodies_percent",
      "Image-level background area percent" = "background_percent",
      "Image-level confluence from mask area" = "confluence",
      "Image-level total mask area in pixels" = "total_area_px"
    )
  }

  normalize_generalization_profiles <- function(selected_profiles) {
    selected_profiles <- selected_profiles %||% "__current__"
    selected_profiles <- selected_profiles[nzchar(selected_profiles)]
    if (!length(selected_profiles)) "__current__" else selected_profiles
  }

  normalize_generalization_features <- function(selected_features) {
    selected_features <- selected_features %||% c("image_total_skeleton_length", "image_total_skeleton_length_per_confluence", "image_median_skeleton_length", "image_median_skeleton_length_per_confluence")
    selected_features <- intersect(selected_features[nzchar(selected_features)], unname(generalization_feature_choices()))
    if (!length(selected_features)) c("image_total_skeleton_length", "image_total_skeleton_length_per_confluence", "image_median_skeleton_length", "image_median_skeleton_length_per_confluence") else selected_features
  }

  generalization_input_diagnostics <- function(selected_profiles, current_settings) {
    selected_profiles <- normalize_generalization_profiles(selected_profiles)
    rows <- lapply(selected_profiles, function(profile_id) {
      src <- cutoff_source_paths(profile_id, current_settings)
      raw_header <- if (file.exists(src$raw)) tryCatch(names(read.csv(src$raw, nrows = 1, check.names = FALSE)), error = function(e) character()) else character()
      filtered_header <- if (file.exists(src$filtered)) tryCatch(names(read.csv(src$filtered, nrows = 1, check.names = FALSE)), error = function(e) character()) else character()
      data.frame(
        Profile = src$label,
        Raw_table_exists = file.exists(src$raw),
        Raw_table_MB = if (file.exists(src$raw)) round(file.info(src$raw)$size / 1024^2, 2) else NA_real_,
        Raw_has_image_name = "image_name" %in% raw_header,
        Raw_has_roi_name = "roi_name" %in% raw_header,
        Raw_has_skeleton_length = "skeleton_length" %in% raw_header,
        Filtered_table_exists = file.exists(src$filtered),
        Filtered_has_image_name = "image_name" %in% filtered_header,
        Filtered_has_roi_name = "roi_name" %in% filtered_header,
        stringsAsFactors = FALSE
      )
    })
    if (!length(rows)) data.frame() else do.call(rbind, rows)
  }

  summarize_profile_features_for_generalization <- function(profile_id, settings, selected_features) {
    src <- cutoff_source_paths(profile_id, settings)
    raw <- read_csv_safe_file(src$raw)
    filtered <- read_csv_safe_file(src$filtered)
    if (!nrow(raw) || !"image_name" %in% names(raw)) return(data.frame())
    raw$.retained <- FALSE
    if (nrow(filtered) && all(c("image_name", "roi_name") %in% names(filtered)) && all(c("image_name", "roi_name") %in% names(raw))) {
      retained_key <- paste(filtered$image_name, filtered$roi_name, sep = "||")
      raw$.retained <- paste(raw$image_name, raw$roi_name, sep = "||") %in% retained_key
    }
    raw$.image_key <- metadata_image_key(raw$image_name)
    conf_col <- confluence_column_for_roi_table(raw)
    raw$.confluence <- NA_real_
    if (!is.na(conf_col) && nzchar(conf_col)) {
      raw$.confluence <- suppressWarnings(as.numeric(raw[[conf_col]]))
      if (grepl("background", conf_col, ignore.case = TRUE)) raw$.confluence <- 100 - raw$.confluence
    }
    raw$skeleton_length_num <- suppressWarnings(as.numeric(raw$skeleton_length %||% NA_real_))
    raw$area_num <- suppressWarnings(as.numeric(raw$area %||% NA_real_))
    raw$circularity_num <- suppressWarnings(as.numeric(raw$circularity %||% NA_real_))
    raw$aspect_ratio_num <- suppressWarnings(as.numeric(raw$aspect_ratio %||% NA_real_))
    rows <- lapply(split(raw, raw$image_name), function(x) {
      retained <- x[x$.retained, , drop = FALSE]
      lens <- retained$skeleton_length_num[is.finite(retained$skeleton_length_num)]
      all_n <- nrow(x)
      retained_n <- nrow(retained)
      conf <- suppressWarnings(stats::median(x$.confluence, na.rm = TRUE))
      if (!is.finite(conf) || conf <= 0) conf <- NA_real_
      out <- data.frame(
        profile_id = profile_id,
        profile_label = src$label,
        image_name = x$image_name[[1]],
        .image_key = x$.image_key[[1]],
        candidate_roi_count = all_n,
        retained_roi_count = retained_n,
        retained_roi_fraction = if (all_n > 0) retained_n / all_n else NA_real_,
        image_confluence = conf,
        stringsAsFactors = FALSE
      )
      out$roi_mean_skeleton_length <- if (length(lens)) mean(lens, na.rm = TRUE) else NA_real_
      out$roi_median_skeleton_length <- if (length(lens)) stats::median(lens, na.rm = TRUE) else NA_real_
      out$image_total_skeleton_length <- if (length(lens)) sum(lens, na.rm = TRUE) else 0
      out$image_mean_skeleton_length <- out$roi_mean_skeleton_length
      out$image_median_skeleton_length <- out$roi_median_skeleton_length
      out$image_total_skeleton_length_per_confluence <- out$image_total_skeleton_length / conf
      out$image_median_skeleton_length_per_confluence <- out$image_median_skeleton_length / conf
      out$roi_mean_area <- if (nrow(retained)) mean(retained$area_num, na.rm = TRUE) else NA_real_
      out$roi_mean_circularity <- if (nrow(retained)) mean(retained$circularity_num, na.rm = TRUE) else NA_real_
      out$roi_mean_aspect_ratio <- if (nrow(retained)) mean(retained$aspect_ratio_num, na.rm = TRUE) else NA_real_
      keep_cols <- unique(c("profile_id", "profile_label", "image_name", ".image_key", "candidate_roi_count", "retained_roi_count", "retained_roi_fraction", "image_confluence", selected_features))
      out[, intersect(keep_cols, names(out)), drop = FALSE]
    })
    rows <- Filter(Negate(is.null), rows)
    if (!length(rows)) data.frame() else do.call(rbind, rows)
  }

  generalization_analysis_cache <- reactiveVal(data.frame())

  build_generalized_analysis_table <- function(selected_profiles, selected_features, current_settings) {
    selected_profiles <- normalize_generalization_profiles(selected_profiles)
    selected_features <- normalize_generalization_features(selected_features)
    rows <- lapply(selected_profiles, summarize_profile_features_for_generalization, settings = current_settings, selected_features = selected_features)
    rows <- Filter(function(x) is.data.frame(x) && nrow(x), rows)
    if (!length(rows)) return(data.frame())
    feature_df <- do.call(rbind, rows)
    feature_df <- enrich_with_mask_area_features(feature_df)
    meta <- read_metadata_table()
    if (nrow(meta)) {
      feature_df <- merge(feature_df, meta, by = ".image_key", all.x = TRUE, sort = FALSE)
    }
    row.names(feature_df) <- NULL
    feature_df
  }

  generalized_analysis_table <- reactive({
    generalization_analysis_cache()
  })

  observeEvent(list(input$generalization_models, input$generalization_features), {
    generalization_analysis_cache(data.frame())
  }, ignoreInit = TRUE)

  observeEvent(input$build_generalization_analysis_btn, {
    generalization_tab_selected("3.2. Analysis Specification and Export")
    try(updateTabsetPanel(session, "generalization_tabs", selected = "3.2. Analysis Specification and Export"), silent = TRUE)
    selected_profiles <- normalize_generalization_profiles(input$generalization_models)
    selected_features <- normalize_generalization_features(input$generalization_features)
    withProgress(message = "Building generalized model-feature table", value = 0.1, {
      append_log("Tab 3.2:", sprintf("Building generalized table for %s profile(s).", length(selected_profiles)))
      df <- build_generalized_analysis_table(selected_profiles, selected_features, settings())
      generalization_analysis_cache(df)
      append_log("Tab 3.2:", sprintf("Generalized table build finished with %s row(s) and %s column(s).", nrow(df), if (nrow(df)) ncol(df) else 0L))
      tick(tick() + 1)
      if (nrow(df)) {
        showNotification(sprintf("Generalized analysis table built with %s rows.", nrow(df)), type = "message")
      } else {
        showNotification("No generalized rows were built. Check selected profiles and ROI tables.", type = "warning", duration = 8)
      }
    })
    restore_current_tabs()
  })

  observeEvent(input$run_production_generalization_btn, {
    generalization_tab_selected("3.2. Analysis Specification and Export")
    try(updateTabsetPanel(session, "generalization_tabs", selected = "3.2. Analysis Specification and Export"), silent = TRUE)
    selected_profiles <- normalize_generalization_profiles(input$generalization_models)
    runtime_choice <- input$runtime_choice %||% "posix"
    root_arg <- bash_quote(runtime_root_arg(runtime_choice))
    profile_args <- character()
    for (selected_profile in selected_profiles) {
      if (!identical(selected_profile, "__current__")) {
        profile_dir <- validation_profile_dir(selected_profile, settings())
        if (!nzchar(profile_dir %||% "") || !dir.exists(profile_dir)) {
          showNotification(sprintf("Selected cutoff profile folder could not be found: %s", selected_profile), type = "error", duration = 8)
          return()
        }
        profile_args <- c(profile_args, bash_quote(runtime_path_arg(profile_dir, runtime_choice)))
      }
    }
    profile_arg <- if (length(profile_args)) paste0(" ", paste(profile_args, collapse = " ")) else ""
    out <- generalized_analysis_output_paths(settings())
    step <- list(
      id = "production_generalization",
      phase = "Generalization with New Images",
      title = "3.2. Apply Cutoff Model to All Images",
      manual = FALSE,
      desc = "Apply the selected validated cutoff model to every ROI folder from the production image set.",
      inputs = file.path(build_paths(settings())$out_opt, "ROIs"),
      outputs = c(out$csv, out$rds),
      command = sprintf("Rscript ./3.Apply_cutoff_profile_to_all_images.R %s%s", root_arg, profile_arg)
    )
    append_log("Tab 3.2:", sprintf("Applying %s cutoff profile(s) to all ROI folders: %s", length(selected_profiles), paste(selected_profiles, collapse = ", ")))
    run_step_now(step, runtime_choice)
  }, ignoreInit = TRUE)

  saved_generalized_analysis_table <- reactive({
    tick()
    out <- generalized_analysis_output_paths(settings())
    if (file.exists(out$rds)) {
      obj <- tryCatch(readRDS(out$rds), error = function(e) NULL)
      if (is.data.frame(obj)) return(obj)
    }
    read_csv_safe_file(out$csv)
  })

  vector_is_numeric_like <- function(x) {
    if (is.numeric(x)) return(TRUE)
    y <- suppressWarnings(as.numeric(as.character(x)))
    ok <- !is.na(y) | is.na(x) | !nzchar(as.character(x))
    length(y) > 0 && mean(ok, na.rm = TRUE) >= 0.95 && any(is.finite(y))
  }

  ordered_unique_values <- function(x) {
    vals <- unique(as.character(x))
    vals <- vals[nzchar(vals) & !is.na(vals)]
    num <- suppressWarnings(as.numeric(vals))
    if (length(vals) && all(is.finite(num))) {
      vals[order(num)]
    } else {
      sort(vals)
    }
  }

  numeric_like_values <- function(x) {
    suppressWarnings(as.numeric(as.character(x)))
  }

  numeric_choice_labels <- function(x, source_values = NULL) {
    labels <- format(x, digits = 12, trim = TRUE, scientific = FALSE)
    if (!is.null(source_values)) {
      source_values <- source_values[is.finite(source_values)]
      counts <- vapply(x, function(v) sum(source_values == v, na.rm = TRUE), integer(1))
      labels <- sprintf("%s (n=%s)", labels, counts)
    }
    labels
  }

  numeric_value_reminder_ui <- function(unique_vals, source_values = NULL, max_values = 80L) {
    unique_vals <- sort(unique_vals[is.finite(unique_vals)])
    if (!length(unique_vals)) return(NULL)
    shown <- head(unique_vals, max_values)
    value_text <- paste(format(shown, digits = 12, trim = TRUE, scientific = FALSE), collapse = ", ")
    if (length(unique_vals) > length(shown)) {
      value_text <- paste0(value_text, sprintf(", ... and %s more", length(unique_vals) - length(shown)))
    }
    count_text <- ""
    if (!is.null(source_values)) {
      source_values <- source_values[is.finite(source_values)]
      counts <- vapply(shown, function(v) sum(source_values == v, na.rm = TRUE), integer(1))
      count_pairs <- paste(sprintf("%s n=%s", format(shown, digits = 12, trim = TRUE, scientific = FALSE), counts), collapse = "; ")
      if (length(unique_vals) > length(shown)) count_pairs <- paste0(count_pairs, "; ...")
      count_text <- count_pairs
    }
    tags$div(
      class = "metric-info-box",
      tags$strong("Available exact values"),
      tags$p("Use this as a quick reminder when choosing or pasting values into the selector."),
      tags$p(tags$code(style = "white-space:normal; word-break:break-word;", value_text)),
      if (nzchar(count_text)) tags$p(style = "font-size:12px; color:#5b6a70;", count_text)
    )
  }

  ordered_factor_for_plot <- function(x) {
    vals <- ordered_unique_values(x)
    factor(as.character(x), levels = vals)
  }

  ordered_interaction_factor <- function(df, columns, sep = " | ") {
    columns <- columns[nzchar(columns) & columns %in% names(df)]
    if (!length(columns) || !nrow(df)) return(factor(character()))
    parts <- lapply(columns, function(nm) as.character(df[[nm]]))
    labels <- do.call(paste, c(parts, sep = sep))
    level_df <- unique(data.frame(label = labels, df[, columns, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE))
    order_bits <- lapply(columns, function(nm) {
      if (vector_is_numeric_like(level_df[[nm]])) suppressWarnings(as.numeric(level_df[[nm]])) else as.character(level_df[[nm]])
    })
    level_order <- do.call(order, order_bits)
    factor(labels, levels = level_df$label[level_order])
  }

  continuous_palette <- function(x, palette = "Viridis", limits = NULL) {
    x <- suppressWarnings(as.numeric(x))
    cols <- grDevices::hcl.colors(256, palette)
    if (!length(x) || !any(is.finite(x))) return(rep("#8aa0a8", length(x)))
    rng <- if (length(limits) == 2 && all(is.finite(limits))) c(min(limits), max(limits)) else range(x, na.rm = TRUE)
    if (!is.finite(diff(rng)) || diff(rng) == 0) return(rep(cols[[128]], length(x)))
    idx <- round(1 + 255 * (x - rng[[1]]) / diff(rng))
    idx[!is.finite(idx)] <- 1
    cols[pmax(1, pmin(256, idx))]
  }

  draw_continuous_palette_legend <- function(label, values, palette = "Viridis", limits = NULL) {
    values <- suppressWarnings(as.numeric(values))
    values <- values[is.finite(values)]
    if (!length(values) && !(length(limits) == 2 && all(is.finite(limits)))) return(invisible(NULL))
    usr <- par("usr")
    cols <- grDevices::hcl.colors(80, palette)
    x0 <- usr[[2]] + 0.04 * diff(usr[1:2])
    x1 <- usr[[2]] + 0.08 * diff(usr[1:2])
    y0 <- usr[[3]] + 0.12 * diff(usr[3:4])
    y1 <- usr[[4]] - 0.12 * diff(usr[3:4])
    old_xpd <- par(xpd = NA)
    on.exit(par(old_xpd), add = TRUE)
    rasterImage(as.raster(matrix(rev(cols), ncol = 1)), x0, y0, x1, y1)
    rect(x0, y0, x1, y1, border = "#333333")
    rng <- if (length(limits) == 2 && all(is.finite(limits))) c(min(limits), max(limits)) else range(values)
    text(x1 + 0.01 * diff(usr[1:2]), y1, labels = sprintf("high %.3g", rng[[2]]), adj = c(0, 0.5), cex = 0.72)
    text(x1 + 0.01 * diff(usr[1:2]), y0, labels = sprintf("low %.3g", rng[[1]]), adj = c(0, 0.5), cex = 0.72)
    text((x0 + x1) / 2, y1 + 0.08 * diff(usr[3:4]), labels = label, adj = c(0.5, 0.5), cex = 0.72)
    invisible(NULL)
  }

  parse_publication_legend_colors <- function(x, n) {
    if (!is.finite(n) || n <= 0) return(character())
    raw <- unlist(strsplit(gsub("\\s*[|;]\\s*", ",", as.character(x %||% "")), "\\s*,\\s*"), use.names = FALSE)
    raw <- raw[nzchar(trimws(raw))]
    raw <- trimws(raw)
    valid <- vapply(raw, function(col) {
      isTRUE(tryCatch({
        grDevices::col2rgb(col)
        TRUE
      }, error = function(e) FALSE))
    }, logical(1))
    cols <- raw[valid]
    if (length(cols) < n) cols <- c(cols, grDevices::hcl.colors(n - length(cols), "Dark 3"))
    cols[seq_len(n)]
  }

  choose_pca_axes <- function(scores, requested_x = "PC1", requested_y = "PC2") {
    pcs <- names(scores)[grepl("^PC[0-9]+$", names(scores))]
    if (!length(pcs)) return(c("PC1", "PC1"))
    pc_x <- if (requested_x %in% pcs) requested_x else pcs[[1]]
    fallback_y <- setdiff(pcs, pc_x)
    pc_y <- if (requested_y %in% pcs && !identical(requested_y, pc_x)) requested_y else if (length(fallback_y)) fallback_y[[1]] else pc_x
    c(pc_x, pc_y)
  }

  safe_image_key <- function(x) {
    if (exists("metadata_image_key", mode = "function")) {
      return(metadata_image_key(x))
    }
    x <- basename(as.character(x))
    x <- sub("\\.[A-Za-z0-9]+$", "", x)
    x <- sub("_RGavg(_mask(_renorm)?)?$", "", x)
    x <- sub("_mask(_renorm)?$", "", x)
    tolower(trimws(x))
  }

  file_stem <- function(x) {
    tools::file_path_sans_ext(basename(as.character(x)))
  }

  exact_final_stem_match <- function(stem, values) {
    stem <- as.character(stem)
    values <- as.character(values)
    escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", stem)
    grepl(paste0("(^|-)", escaped, "$"), values, ignore.case = FALSE, perl = TRUE)
  }

  likely_ilastik_training_dirs <- function(settings) {
    p <- build_paths(settings)
    ilp <- p$ilastik_project %||% ""
    input_workspace <- p$input_workspace %||% normalize_project_path("pipeline_inputs")
    legacy_root <- file.path(project_root, "Ilastik_model_neurites_sh-sy5y")
    ilp_dir <- if (nzchar(ilp)) dirname(ilp) else ""
    ilp_parent <- if (nzchar(ilp_dir)) dirname(ilp_dir) else ""
    ilp_grandparent <- if (nzchar(ilp_parent)) dirname(ilp_parent) else ""
    path_under <- function(root, ...) {
      if (!nzchar(root %||% "")) return(character())
      file.path(root, ...)
    }

    direct_candidates <- unique(c(
      file.path(legacy_root, "training_set_raw_before preprocessing"),
      file.path(legacy_root, "training_images_raw"),
      file.path(input_workspace, "ilastik", "training_images_raw"),
      file.path(input_workspace, "ilastik", "training_set_raw_before preprocessing"),
      file.path(input_workspace, "ilastik", "training_set_raw"),
      file.path(input_workspace, "ilastik", "training_images"),
      file.path(input_workspace, "ilastik", "raw_training_images"),
      file.path(input_workspace, "ilastik", "model", "training_images_raw"),
      path_under(ilp_dir, "training_images_raw"),
      path_under(ilp_dir, "training_set_raw_before preprocessing"),
      path_under(ilp_dir, "training_set_raw"),
      path_under(ilp_dir, "training_images"),
      path_under(ilp_dir, "raw_training_images"),
      path_under(ilp_parent, "training_images_raw"),
      path_under(ilp_parent, "training_set_raw_before preprocessing"),
      path_under(ilp_parent, "training_set_raw"),
      path_under(ilp_parent, "training_images"),
      path_under(ilp_parent, "raw_training_images"),
      path_under(ilp_grandparent, "ilastik", "training_images_raw"),
      path_under(ilp_grandparent, "ilastik", "training_set_raw_before preprocessing")
    ))

    should_scan_ilp_parent <- nzchar(ilp_parent) &&
      !normalizePath(ilp_parent, winslash = "/", mustWork = FALSE) %in% normalizePath(c(project_root, p$input_master, p$output_root), winslash = "/", mustWork = FALSE) &&
      (grepl("model", basename(ilp_dir), ignore.case = TRUE) || grepl("ilastik", basename(ilp_parent), ignore.case = TRUE))
    seed_roots <- unique(c(legacy_root, file.path(input_workspace, "ilastik"), ilp_dir, if (isTRUE(should_scan_ilp_parent)) ilp_parent else character()))
    seed_roots <- seed_roots[nzchar(seed_roots) & dir.exists(seed_roots)]
    discovered <- character()
    for (root in seed_roots) {
      dirs <- tryCatch(list.dirs(root, recursive = TRUE, full.names = TRUE), error = function(e) character())
      if (!length(dirs)) next
      keep <- grepl("(^|[/\\\\])(training|train|raw).*|training.*raw|raw.*training|training_images_raw|training_set_raw", basename(dirs), ignore.case = TRUE)
      discovered <- c(discovered, dirs[keep])
    }

    direct_ilp_images_dir <- character()
    if (nzchar(ilp_dir) && dir.exists(ilp_dir) && grepl("ilastik|model|training", basename(ilp_dir), ignore.case = TRUE)) {
      direct_images <- list.files(ilp_dir, pattern = "\\.(tif|tiff|png|jpg|jpeg)$", recursive = FALSE, full.names = TRUE, ignore.case = TRUE)
      if (length(direct_images) > 0 && length(direct_images) <= 200) direct_ilp_images_dir <- ilp_dir
    }

    dirs <- c(direct_candidates, discovered, direct_ilp_images_dir)
    dirs <- dirs[nzchar(dirs)]
    dirs <- unique(normalizePath(dirs, winslash = "/", mustWork = FALSE))
    excluded_prefixes <- normalizePath(c(p$output_root, p$pre_renamed, p$out_clear, p$out_seg, p$out_opt), winslash = "/", mustWork = FALSE)
    dirs <- dirs[!vapply(dirs, function(d) any(startsWith(d, paste0(excluded_prefixes, "/")) | identical(d, excluded_prefixes)), logical(1))]
    dirs <- dirs[dir.exists(dirs)]
    if (!length(dirs)) return(data.frame())
    has_images <- vapply(dirs, function(d) {
      length(list.files(d, pattern = "\\.(tif|tiff|png|jpg|jpeg)$", recursive = TRUE, full.names = FALSE, ignore.case = TRUE)) > 0
    }, logical(1))
    dirs <- dirs[has_images]
    if (!length(dirs)) return(data.frame())
    data.frame(
      training_root = dirs,
      detection_reason = ifelse(grepl("training|raw", basename(dirs), ignore.case = TRUE), "named training/raw folder near ilastik project or input workspace", "image files beside selected ilastik project"),
      ilastik_project = normalizePath(ilp, winslash = "/", mustWork = FALSE),
      stringsAsFactors = FALSE
    )
  }

  tab4_cutoff_training_keys <- reactive({
    manifest <- read_manual_review_manifest(settings())
    if (nrow(manifest) && all(c("image_name", "manual_label") %in% names(manifest))) {
      manifest <- manifest[manifest$manual_label %in% c("choice", "noise"), , drop = FALSE]
      if (nrow(manifest)) return(unique(safe_image_key(manifest$image_name)))
    }
    p <- build_paths(settings())
    inspected <- read_csv_safe_file(p$inspected)
    groups <- read_csv_safe_file(p$groups)
    src <- if (nrow(inspected)) inspected else groups
    if (!nrow(src) || !"image_name" %in% names(src)) return(character())
    label_cols <- intersect(c("manual_choise", "manual_choice", "manual_noise"), names(src))
    if (length(label_cols)) {
      keep <- Reduce(`|`, lapply(label_cols, function(nm) suppressWarnings(as.numeric(src[[nm]])) == 1))
      src <- src[keep %in% TRUE, , drop = FALSE]
    } else {
      grp_cols <- grep("^grp_", names(src), value = TRUE)
      if (length(grp_cols)) {
        keep <- Reduce(`|`, lapply(grp_cols, function(nm) suppressWarnings(as.numeric(src[[nm]])) == 1))
        src <- src[keep %in% TRUE, , drop = FALSE]
      }
    }
    unique(safe_image_key(src$image_name))
  })

  tab4_ilastik_training_files <- reactive({
    roots <- likely_ilastik_training_dirs(settings())
    if (!nrow(roots)) return(data.frame())
    rows <- lapply(seq_len(nrow(roots)), function(i) {
      root <- roots$training_root[[i]]
      files <- list.files(root, pattern = "\\.(tif|tiff|png|jpg|jpeg)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (!length(files)) return(NULL)
      info <- file.info(files)
      data.frame(
        training_file = normalizePath(files, winslash = "/", mustWork = FALSE),
        training_name = basename(files),
        training_stem = file_stem(files),
        training_size_bytes = as.numeric(info$size),
        training_root = normalizePath(root, winslash = "/", mustWork = FALSE),
        detection_reason = roots$detection_reason[[i]],
        ilastik_project = roots$ilastik_project[[i]],
        stringsAsFactors = FALSE
      )
    })
    out <- do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
    if (is.null(out) || !nrow(out)) return(data.frame())
    out <- out[!duplicated(out$training_file), , drop = FALSE]
    out[order(out$training_root, out$training_name), , drop = FALSE]
  })

  tab4_ilastik_training_matches <- reactive({
    df <- saved_generalized_analysis_table()
    train <- tab4_ilastik_training_files()
    if (!nrow(df) || !nrow(train) || !"image_name" %in% names(df)) return(data.frame())
    image_names <- unique(as.character(df$image_name))
    image_keys <- safe_image_key(image_names)
    p <- build_paths(settings())
    search_roots <- unique(c(p$pre_renamed, p$input_master, unique(train$training_root)))
    search_roots <- search_roots[dir.exists(search_roots)]
    candidate_files <- character()
    if (length(search_roots)) {
      candidate_files <- unique(unlist(lapply(search_roots, function(root) {
        list.files(root, pattern = "\\.(tif|tiff|png|jpg|jpeg)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      }), use.names = FALSE))
    }
    candidate_info <- if (length(candidate_files)) {
      fi <- file.info(candidate_files)
      data.frame(candidate_file = normalizePath(candidate_files, winslash = "/", mustWork = FALSE), candidate_name = basename(candidate_files), candidate_stem = file_stem(candidate_files), candidate_size_bytes = as.numeric(fi$size), stringsAsFactors = FALSE)
    } else {
      data.frame()
    }
    rows <- lapply(seq_len(nrow(train)), function(i) {
      stem <- train$training_stem[[i]]
      name_hits <- exact_final_stem_match(stem, file_stem(image_names))
      matched_names <- image_names[name_hits]
      size_hits <- character()
      size_match_count <- 0L
      if (nrow(candidate_info)) {
        by_stem <- exact_final_stem_match(stem, candidate_info$candidate_stem)
        by_size <- is.finite(candidate_info$candidate_size_bytes) & candidate_info$candidate_size_bytes == train$training_size_bytes[[i]]
        size_hits <- candidate_info$candidate_name[by_stem & by_size]
        size_match_count <- length(size_hits)
      }
      confidence <- if (length(matched_names) == 1 && size_match_count >= 1) {
        "high: unique name match plus file-size match"
      } else if (length(matched_names) == 1) {
        "medium: unique suffix/name match"
      } else if (length(matched_names) > 1 && size_match_count >= 1) {
        "partial: multiple name matches, file-size support exists"
      } else if (length(matched_names) > 1) {
        "ambiguous: multiple suffix/name matches"
      } else {
        "not found in current analysis table"
      }
      data.frame(
        training_name = train$training_name[[i]],
        training_stem = stem,
        training_size_bytes = train$training_size_bytes[[i]],
        training_root = train$training_root[[i]],
        training_source = train$detection_reason[[i]],
        analysis_name_matches = length(matched_names),
        file_size_matches = size_match_count,
        confidence = confidence,
        matched_analysis_images = paste(head(matched_names, 8), collapse = " | "),
        matched_file_examples = paste(head(size_hits, 5), collapse = " | "),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, rows)
  })

  tab4_ilastik_training_keys <- reactive({
    matches <- tab4_ilastik_training_matches()
    if (!nrow(matches) || !"matched_analysis_images" %in% names(matches)) return(character())
    imgs <- unlist(strsplit(matches$matched_analysis_images, " \\| "), use.names = FALSE)
    imgs <- imgs[nzchar(imgs)]
    unique(safe_image_key(imgs))
  })

  annotate_tab4_training_images <- function(df) {
    if (!nrow(df)) return(df)
    image_col <- if ("image_name" %in% names(df)) "image_name" else ""
    keys <- if (nzchar(image_col)) safe_image_key(df[[image_col]]) else rep("", nrow(df))
    cutoff_keys <- tab4_cutoff_training_keys()
    ilastik_keys <- tab4_ilastik_training_keys()
    df$.is_cutoff_training_image <- keys %in% cutoff_keys
    df$.is_ilastik_training_image <- keys %in% ilastik_keys
    df$.training_image_role <- ifelse(df$.is_cutoff_training_image & df$.is_ilastik_training_image, "cutoff + ilastik training",
      ifelse(df$.is_cutoff_training_image, "cutoff training",
        ifelse(df$.is_ilastik_training_image, "ilastik training", "not training")))
    df
  }

  tab4_source_table <- reactive({
    annotate_tab4_training_images(enrich_with_mask_area_features(saved_generalized_analysis_table()))
  })

  tab4_numeric_filter_control <- function(range_id, discrete_id, min_id, max_id, vals, exact_mode_id) {
    vals <- numeric_like_values(vals)
    vals <- vals[is.finite(vals)]
    if (!length(vals)) return(NULL)
    unique_vals <- sort(unique(vals))
    has_discrete_selector <- length(unique_vals) > 1 && length(unique_vals) <= 100
    compact_discrete_selector <- length(unique_vals) > 1 && length(unique_vals) < 10
    current_range <- isolate(input[[range_id]])
    rng <- range(vals)
    if (!is.finite(diff(rng)) || diff(rng) == 0) rng <- rng + c(-0.5, 0.5)
    if (!(length(current_range) == 2 && all(is.finite(current_range)))) current_range <- rng
    values_in_current_range <- unique_vals
    values_in_current_range <- unique_vals[unique_vals >= min(current_range) & unique_vals <= max(current_range)]
    current_exact_mode <- isolate(input[[exact_mode_id]] %||% if (compact_discrete_selector) "keep" else "off")
    if (!current_exact_mode %in% c("off", "keep", "exclude")) current_exact_mode <- if (compact_discrete_selector) "keep" else "off"
    current_discrete_values <- isolate(input[[discrete_id]])
    if (length(current_discrete_values)) {
      selected_discrete_values <- intersect(as.character(unique_vals), as.character(current_discrete_values))
    } else if (!is.null(current_discrete_values)) {
      selected_discrete_values <- character()
    } else if (compact_discrete_selector) {
      selected_discrete_values <- as.character(values_in_current_range)
    } else {
      selected_discrete_values <- character()
    }
    current_min_value <- suppressWarnings(as.numeric(isolate(input[[min_id]])))
    current_max_value <- suppressWarnings(as.numeric(isolate(input[[max_id]])))
    if (!length(current_min_value) || !is.finite(current_min_value)) current_min_value <- min(current_range)
    if (!length(current_max_value) || !is.finite(current_max_value)) current_max_value <- max(current_range)
    discrete_ui <- if (has_discrete_selector) {
      exact_choices <- setNames(as.character(unique_vals), numeric_choice_labels(unique_vals, vals))
      selector <- if (compact_discrete_selector) {
        checkboxGroupInput(
          discrete_id,
          "Specific numeric values",
          choices = exact_choices,
          selected = selected_discrete_values,
          inline = TRUE
        )
      } else {
        selectizeInput(
          discrete_id,
          "Specific numeric values to keep or exclude",
          choices = exact_choices,
          selected = selected_discrete_values,
          multiple = TRUE,
          options = list(
            plugins = list("remove_button"),
            delimiter = ",",
            placeholder = "Type to search, click values, or paste comma-separated values"
          )
        )
      }
      tagList(
        tags$p(
          class = "metric-info-box",
          if (compact_discrete_selector) {
            "This variable has fewer than 10 unique numeric values, so you can click exact values on/off. Moving the slider automatically refreshes which exact values are checked."
          } else {
            "This variable has many but still manageable unique numeric values. Use the exact-value selector when you need to keep or exclude specific concentrations that do not form one continuous interval."
          }
        ),
        if (!compact_discrete_selector) numeric_value_reminder_ui(unique_vals, vals),
        radioButtons(
          exact_mode_id,
          "Exact numeric value rule",
          choices = c("Do not use exact values" = "off", "Keep selected values" = "keep", "Exclude selected values" = "exclude"),
          selected = current_exact_mode,
          inline = TRUE
        ),
        if (!compact_discrete_selector) tags$p(class = "metric-info-box", tags$strong("How to enter several values: "), "type to search and press Enter/click a value, or paste values separated by commas, for example ", tags$code("0.63, 1.25, 2.5"), ". Selected values appear as removable chips in the box."),
        selector
      )
    } else {
      tagList(
        tags$p(class = "metric-info-box", "This variable has many unique numeric values. Use the slider for broad filtering, then use From/To boxes for exact numeric limits."),
        fluidRow(
          column(6, numericInput(min_id, "From", value = current_min_value, min = min(vals), max = max(vals), step = signif(max(diff(rng) / 100, .Machine$double.eps), 2))),
          column(6, numericInput(max_id, "To", value = current_max_value, min = min(vals), max = max(vals), step = signif(max(diff(rng) / 100, .Machine$double.eps), 2)))
        )
      )
    }
    tagList(
      sliderInput(
        range_id,
        "Keep numeric range",
        min = floor(rng[[1]]),
        max = ceiling(rng[[2]]),
        value = current_range,
        step = signif(max(diff(rng) / 100, .Machine$double.eps), 2)
      ),
      discrete_ui
    )
  }

  tab4_additional_filter_indices <- 2:8
  tab4_combination_filter_indices <- 1:2

  output$tab4_filter_controls_ui <- renderUI({
    df <- tab4_source_table()
    if (!nrow(df)) return(NULL)
    cols <- setdiff(names(df), character())
    selected_col <- input$tab4_filter_column %||% ""
    if (!selected_col %in% cols) selected_col <- ""
    selected_additional_cols <- stats::setNames(
      vapply(tab4_additional_filter_indices, function(i) isolate(input[[paste0("tab4_filter_column_", i)]] %||% ""), character(1)),
      as.character(tab4_additional_filter_indices)
    )
    selected_additional_cols[!selected_additional_cols %in% c("", cols)] <- ""
    max_rows_value <- suppressWarnings(as.integer(isolate(input$tab4_max_rows %||% 0L)))
    random_seed_value <- suppressWarnings(as.integer(isolate(input$tab4_random_seed %||% 42L)))
    if (!length(max_rows_value) || !is.finite(max_rows_value)) max_rows_value <- 0L
    if (!length(random_seed_value) || !is.finite(random_seed_value)) random_seed_value <- 42L
    include_cutoff_training <- isolate(input$tab4_include_cutoff_training)
    include_ilastik_training <- isolate(input$tab4_include_ilastik_training)
    if (is.null(include_cutoff_training)) include_cutoff_training <- TRUE
    if (is.null(include_ilastik_training)) include_ilastik_training <- TRUE
    value_ui <- NULL
    if (nzchar(selected_col) && selected_col %in% names(df)) {
      if (vector_is_numeric_like(df[[selected_col]])) {
        vals <- df[[selected_col]]
        value_ui <- tab4_numeric_filter_control("tab4_filter_numeric_range", "tab4_filter_numeric_values", "tab4_filter_numeric_min", "tab4_filter_numeric_max", vals, "tab4_filter_exact_mode")
      } else {
        vals <- ordered_unique_values(df[[selected_col]])
        selected_values <- isolate(input$tab4_filter_values)
        if (is.null(selected_values)) selected_values <- vals
        selected_values <- intersect(vals, selected_values)
        value_ui <- selectizeInput(
          "tab4_filter_values",
          "Keep selected values",
          choices = vals,
          selected = selected_values,
          multiple = TRUE,
          options = list(plugins = list("remove_button"))
        )
      }
    }
    div(
      class = "manual-panel",
      tags$h4("Shared Tab 4 Subsampling and Filtering"),
      tags$p("These controls affect all tab 4 plots and analyses. Numeric values are sorted numerically and shown with continuous palettes where they are used for color."),
      fluidRow(
        column(4, selectInput("tab4_filter_column", "Filter/subsample by variable", choices = c("No filter" = "", cols), selected = selected_col)),
        column(4, value_ui),
        column(2, numericInput("tab4_max_rows", "Max rows after filter", value = max_rows_value, min = 0, step = 10)),
        column(2, numericInput("tab4_random_seed", "Random seed", value = random_seed_value, min = 1, step = 1))
      ),
      tags$details(
        open = TRUE,
        tags$summary("Additional filters"),
        tags$p(class = "metric-info-box", "Stack multiple filters when you need to subset by treatment, day, concentration, confluence range, model profile, or training-image status at the same time."),
        tagList(lapply(tab4_additional_filter_indices, function(i) {
          fluidRow(
            column(4, selectInput(paste0("tab4_filter_column_", i), sprintf("Additional filter %s", i), choices = c("No filter" = "", cols), selected = selected_additional_cols[[as.character(i)]])),
            column(8, uiOutput(paste0("tab4_filter_value_ui_", i)))
          )
        }))
      ),
      tags$details(
        tags$summary("Specific combination exclusions or keeps"),
        tags$p(class = "metric-info-box", "Use these rules when you need to exclude or keep a specific combination only. Example: exclude concentration 10 only when compound is RosAc, while keeping concentration 10 for RetAc or other compounds."),
        tagList(lapply(tab4_combination_filter_indices, function(i) {
          fluidRow(
            column(2, selectInput(paste0("tab4_combo_mode_", i), sprintf("Rule %s action", i), choices = c("No rule" = "", "Exclude matching rows" = "exclude", "Keep only matching rows" = "keep"), selected = input[[paste0("tab4_combo_mode_", i)]] %||% "")),
            column(2, selectInput(paste0("tab4_combo_column_a_", i), sprintf("Rule %s variable A", i), choices = c("None" = "", cols), selected = input[[paste0("tab4_combo_column_a_", i)]] %||% "")),
            column(3, uiOutput(paste0("tab4_combo_value_ui_a_", i))),
            column(2, selectInput(paste0("tab4_combo_column_b_", i), sprintf("Rule %s variable B", i), choices = c("None" = "", cols), selected = input[[paste0("tab4_combo_column_b_", i)]] %||% "")),
            column(3, uiOutput(paste0("tab4_combo_value_ui_b_", i)))
          )
        }))
      ),
      fluidRow(
        column(6, checkboxInput("tab4_include_cutoff_training", "Include images used for cutoff-model optimization training", value = isTRUE(include_cutoff_training))),
        column(6, checkboxInput("tab4_include_ilastik_training", "Include images used for ilastik training", value = isTRUE(include_ilastik_training)))
      ),
      uiOutput("tab4_filter_status_ui")
    )
  })

  tab4_filter_value_control <- function(index) {
    df <- tab4_source_table()
    col_id <- paste0("tab4_filter_column_", index)
    col <- input[[col_id]] %||% ""
    if (!nrow(df) || !nzchar(col) || !col %in% names(df)) return(NULL)
    if (vector_is_numeric_like(df[[col]])) {
      vals <- df[[col]]
      tab4_numeric_filter_control(
        paste0("tab4_filter_numeric_range_", index),
        paste0("tab4_filter_numeric_values_", index),
        paste0("tab4_filter_numeric_min_", index),
        paste0("tab4_filter_numeric_max_", index),
        vals,
        paste0("tab4_filter_exact_mode_", index)
      )
    } else {
      vals <- ordered_unique_values(df[[col]])
      selected_values <- isolate(input[[paste0("tab4_filter_values_", index)]])
      if (is.null(selected_values)) selected_values <- vals
      selected_values <- intersect(vals, selected_values)
      selectizeInput(
        paste0("tab4_filter_values_", index),
        "Keep selected values",
        choices = vals,
        selected = selected_values,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      )
    }
  }

  for (filter_index in tab4_additional_filter_indices) {
    local({
      i <- filter_index
      output[[paste0("tab4_filter_value_ui_", i)]] <- renderUI(tab4_filter_value_control(i))
    })
  }

  tab4_combo_value_control <- function(index, side = c("a", "b")) {
    side <- match.arg(side)
    df <- tab4_source_table()
    col <- input[[paste0("tab4_combo_column_", side, "_", index)]] %||% ""
    if (!nrow(df) || !nzchar(col) || !col %in% names(df)) return(NULL)
    raw_vals <- df[[col]]
    if (vector_is_numeric_like(raw_vals)) {
      vals <- sort(unique(numeric_like_values(raw_vals)))
      vals <- vals[is.finite(vals)]
      selected <- input[[paste0("tab4_combo_values_", side, "_", index)]] %||% character()
      selected <- intersect(as.character(vals), as.character(selected))
      return(selectizeInput(
        paste0("tab4_combo_values_", side, "_", index),
        sprintf("Values for %s", toupper(side)),
        choices = setNames(as.character(vals), numeric_choice_labels(vals, raw_vals)),
        selected = selected,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ))
    }
    vals <- ordered_unique_values(raw_vals)
    selected <- input[[paste0("tab4_combo_values_", side, "_", index)]] %||% character()
    selected <- intersect(vals, selected)
    selectizeInput(
      paste0("tab4_combo_values_", side, "_", index),
      sprintf("Values for %s", toupper(side)),
      choices = vals,
      selected = selected,
      multiple = TRUE,
      options = list(plugins = list("remove_button"))
    )
  }

  for (combo_index in tab4_combination_filter_indices) {
    local({
      i <- combo_index
      output[[paste0("tab4_combo_value_ui_a_", i)]] <- renderUI(tab4_combo_value_control(i, "a"))
      output[[paste0("tab4_combo_value_ui_b_", i)]] <- renderUI(tab4_combo_value_control(i, "b"))
    })
  }

  tab4_exact_numeric_filter_enabled <- function(df, col) {
    if (!nrow(df) || !nzchar(col %||% "") || !col %in% names(df) || !vector_is_numeric_like(df[[col]])) return(FALSE)
    vals <- numeric_like_values(df[[col]])
    unique_vals <- sort(unique(vals[is.finite(vals)]))
    length(unique_vals) > 1 && length(unique_vals) <= 100
  }

  tab4_default_exact_numeric_mode <- function(df, col) {
    if (!nrow(df) || !nzchar(col %||% "") || !col %in% names(df) || !vector_is_numeric_like(df[[col]])) return("off")
    vals <- numeric_like_values(df[[col]])
    unique_vals <- sort(unique(vals[is.finite(vals)]))
    if (length(unique_vals) > 1 && length(unique_vals) < 10) "keep" else "off"
  }

  sync_tab4_numeric_value_selector <- function(col, range, values_id, min_id, max_id, exact_mode_id) {
    df <- tab4_source_table()
    if (!nrow(df) || !nzchar(col %||% "") || !col %in% names(df) || !vector_is_numeric_like(df[[col]])) return(invisible(NULL))
    vals <- numeric_like_values(df[[col]])
    vals <- vals[is.finite(vals)]
    unique_vals <- sort(unique(vals))
    if (length(unique_vals) <= 1) return(invisible(NULL))
    selected <- unique_vals
    if (length(range) == 2 && all(is.finite(range))) {
      selected <- unique_vals[unique_vals >= min(range) & unique_vals <= max(range)]
    }
    exact_mode <- input[[exact_mode_id]] %||% if (length(unique_vals) < 10) "keep" else "off"
    if (length(unique_vals) <= 100) {
      current_selected <- input[[values_id]]
      selected_for_update <- if (length(unique_vals) < 10 && identical(exact_mode, "keep")) {
        as.character(selected)
      } else {
        intersect(as.character(unique_vals), as.character(current_selected %||% character()))
      }
      if (length(unique_vals) < 10) {
        updateCheckboxGroupInput(
          session,
          values_id,
          choices = setNames(as.character(unique_vals), numeric_choice_labels(unique_vals, vals)),
          selected = selected_for_update
        )
      } else {
        updateSelectizeInput(
          session,
          values_id,
          choices = setNames(as.character(unique_vals), numeric_choice_labels(unique_vals, vals)),
          selected = selected_for_update,
          server = TRUE
        )
      }
    }
    if (length(unique_vals) >= 10 && length(range) == 2 && all(is.finite(range))) {
      updateNumericInput(session, min_id, value = min(range))
      updateNumericInput(session, max_id, value = max(range))
    }
    invisible(NULL)
  }

  observeEvent(
    list(input$tab4_filter_column, input$tab4_filter_numeric_range),
    sync_tab4_numeric_value_selector(input$tab4_filter_column %||% "", input$tab4_filter_numeric_range, "tab4_filter_numeric_values", "tab4_filter_numeric_min", "tab4_filter_numeric_max", "tab4_filter_exact_mode"),
    ignoreInit = TRUE
  )

  for (filter_index in tab4_additional_filter_indices) {
    local({
      i <- filter_index
      observeEvent(
        list(
          input[[paste0("tab4_filter_column_", i)]],
          input[[paste0("tab4_filter_numeric_range_", i)]]
        ),
        sync_tab4_numeric_value_selector(
          input[[paste0("tab4_filter_column_", i)]] %||% "",
          input[[paste0("tab4_filter_numeric_range_", i)]],
          paste0("tab4_filter_numeric_values_", i),
          paste0("tab4_filter_numeric_min_", i),
          paste0("tab4_filter_numeric_max_", i),
          paste0("tab4_filter_exact_mode_", i)
        ),
        ignoreInit = TRUE
      )
    })
  }

  apply_tab4_filter <- function(df, col, numeric_range = NULL, keep_values = NULL, keep_numeric_values = NULL, numeric_min = NULL, numeric_max = NULL, exact_numeric_enabled = FALSE, exact_numeric_mode = "keep") {
    if (!nrow(df) || !nzchar(col %||% "") || !col %in% names(df)) return(df)
    if (vector_is_numeric_like(df[[col]])) {
      vals <- numeric_like_values(df[[col]])
      if (length(numeric_range) == 2 && all(is.finite(numeric_range))) {
        df <- df[is.finite(vals) & vals >= min(numeric_range) & vals <= max(numeric_range), , drop = FALSE]
      }
      if (isTRUE(exact_numeric_enabled)) {
        exact_numeric_mode <- exact_numeric_mode %||% "keep"
        if (!exact_numeric_mode %in% c("off", "keep", "exclude")) exact_numeric_mode <- "keep"
        keep_numeric_values <- suppressWarnings(as.numeric(keep_numeric_values))
        keep_numeric_values <- keep_numeric_values[is.finite(keep_numeric_values)]
        if (!identical(exact_numeric_mode, "off")) {
          vals_after_range <- numeric_like_values(df[[col]])
          if (length(keep_numeric_values)) {
            if (identical(exact_numeric_mode, "exclude")) {
              df <- df[is.finite(vals_after_range) & !(vals_after_range %in% keep_numeric_values), , drop = FALSE]
            } else {
              df <- df[is.finite(vals_after_range) & vals_after_range %in% keep_numeric_values, , drop = FALSE]
            }
          } else if (identical(exact_numeric_mode, "keep")) {
            df <- df[0, , drop = FALSE]
          }
        }
      }
      numeric_min <- suppressWarnings(as.numeric(numeric_min))
    numeric_max <- suppressWarnings(as.numeric(numeric_max))
    if (length(numeric_min) && length(numeric_max) && is.finite(numeric_min) && is.finite(numeric_max)) {
        vals_after_exact <- numeric_like_values(df[[col]])
        df <- df[is.finite(vals_after_exact) & vals_after_exact >= min(numeric_min, numeric_max) & vals_after_exact <= max(numeric_min, numeric_max), , drop = FALSE]
      }
    } else {
      if (length(keep_values)) {
        df <- df[as.character(df[[col]]) %in% keep_values, , drop = FALSE]
      } else {
        df <- df[0, , drop = FALSE]
      }
    }
    df
  }

  tab4_match_selected_values <- function(vec, selected_values) {
    if (!length(selected_values)) return(rep(FALSE, length(vec)))
    selected_chr <- as.character(selected_values)
    if (vector_is_numeric_like(vec)) {
      vals <- numeric_like_values(vec)
      selected_num <- suppressWarnings(as.numeric(selected_chr))
      selected_num <- selected_num[is.finite(selected_num)]
      return(is.finite(vals) & vals %in% selected_num)
    }
    as.character(vec) %in% selected_chr
  }

  apply_tab4_combination_rule <- function(df, mode = "", col_a = "", values_a = NULL, col_b = "", values_b = NULL) {
    if (!nrow(df) || !nzchar(mode %||% "") || !mode %in% c("exclude", "keep")) return(df)
    if (!nzchar(col_a %||% "") || !nzchar(col_b %||% "") || !col_a %in% names(df) || !col_b %in% names(df)) return(df)
    if (!length(values_a) || !length(values_b)) return(df)
    hit <- tab4_match_selected_values(df[[col_a]], values_a) & tab4_match_selected_values(df[[col_b]], values_b)
    if (identical(mode, "exclude")) df[!hit, , drop = FALSE] else df[hit, , drop = FALSE]
  }

  tab4_filtered_table <- reactive({
    df <- tab4_source_table()
    if (!nrow(df)) return(df)
    df <- apply_tab4_filter(
      df,
      input$tab4_filter_column %||% "",
      input$tab4_filter_numeric_range,
      input$tab4_filter_values,
      input$tab4_filter_numeric_values,
      input$tab4_filter_numeric_min,
      input$tab4_filter_numeric_max,
      tab4_exact_numeric_filter_enabled(df, input$tab4_filter_column %||% ""),
      input$tab4_filter_exact_mode %||% tab4_default_exact_numeric_mode(df, input$tab4_filter_column %||% "")
    )
    for (i in tab4_additional_filter_indices) {
      filter_col_i <- input[[paste0("tab4_filter_column_", i)]] %||% ""
      df <- apply_tab4_filter(
        df,
        filter_col_i,
        input[[paste0("tab4_filter_numeric_range_", i)]],
        input[[paste0("tab4_filter_values_", i)]],
        input[[paste0("tab4_filter_numeric_values_", i)]],
        input[[paste0("tab4_filter_numeric_min_", i)]],
        input[[paste0("tab4_filter_numeric_max_", i)]],
        tab4_exact_numeric_filter_enabled(df, filter_col_i),
        input[[paste0("tab4_filter_exact_mode_", i)]] %||% tab4_default_exact_numeric_mode(df, filter_col_i)
      )
    }
    for (i in tab4_combination_filter_indices) {
      df <- apply_tab4_combination_rule(
        df,
        mode = input[[paste0("tab4_combo_mode_", i)]] %||% "",
        col_a = input[[paste0("tab4_combo_column_a_", i)]] %||% "",
        values_a = input[[paste0("tab4_combo_values_a_", i)]],
        col_b = input[[paste0("tab4_combo_column_b_", i)]] %||% "",
        values_b = input[[paste0("tab4_combo_values_b_", i)]]
      )
    }
    if (".is_cutoff_training_image" %in% names(df) && !isTRUE(input$tab4_include_cutoff_training %||% TRUE)) {
      df <- df[!(df$.is_cutoff_training_image %in% TRUE), , drop = FALSE]
    }
    if (".is_ilastik_training_image" %in% names(df) && !isTRUE(input$tab4_include_ilastik_training %||% TRUE)) {
      df <- df[!(df$.is_ilastik_training_image %in% TRUE), , drop = FALSE]
    }
    max_rows <- suppressWarnings(as.integer(input$tab4_max_rows %||% 0L))
    if (is.finite(max_rows) && max_rows > 0 && nrow(df) > max_rows) {
      seed <- suppressWarnings(as.integer(input$tab4_random_seed %||% 42L))
      set.seed(seed)
      df <- df[sample(seq_len(nrow(df)), max_rows), , drop = FALSE]
    }
    df
  })

  describe_tab4_single_filter <- function(df, col, numeric_range = NULL, keep_values = NULL, keep_numeric_values = NULL, numeric_min = NULL, numeric_max = NULL, exact_numeric_enabled = FALSE, exact_numeric_mode = "keep") {
    if (!nrow(df) || !nzchar(col %||% "") || !col %in% names(df)) return("")
    label <- plot_display_label(col, width = 40, multiline = FALSE)
    if (vector_is_numeric_like(df[[col]])) {
      bits <- character()
      if (length(numeric_range) == 2 && all(is.finite(numeric_range))) {
        bits <- c(bits, sprintf("range %.4g to %.4g", min(numeric_range), max(numeric_range)))
      }
      if (isTRUE(exact_numeric_enabled)) {
        exact_numeric_mode <- exact_numeric_mode %||% "keep"
        exact_vals <- suppressWarnings(as.numeric(keep_numeric_values))
        exact_vals <- exact_vals[is.finite(exact_vals)]
        if (!identical(exact_numeric_mode, "off") && length(exact_vals)) {
          shown_vals <- paste(utils::head(sprintf("%.4g", sort(unique(exact_vals))), 8), collapse = ", ")
          if (length(unique(exact_vals)) > 8) shown_vals <- paste0(shown_vals, ", ...")
          bits <- c(bits, sprintf("%s exact values [%s]", if (identical(exact_numeric_mode, "exclude")) "excluding" else "keeping", shown_vals))
        }
      }
      numeric_min <- suppressWarnings(as.numeric(numeric_min))
      numeric_max <- suppressWarnings(as.numeric(numeric_max))
      if (length(numeric_min) && length(numeric_max) && is.finite(numeric_min) && is.finite(numeric_max)) {
        bits <- c(bits, sprintf("bounded from %.4g to %.4g", min(numeric_min, numeric_max), max(numeric_min, numeric_max)))
      }
      if (!length(bits)) return("")
      return(sprintf("%s: %s", label, paste(bits, collapse = "; ")))
    }
    if (!length(keep_values)) return("")
    vals <- unique(as.character(keep_values))
    shown_vals <- paste(utils::head(vals, 8), collapse = ", ")
    if (length(vals) > 8) shown_vals <- paste0(shown_vals, ", ...")
    sprintf("%s: keeping [%s]", label, shown_vals)
  }

  describe_tab4_combination_rule <- function(df, mode = "", col_a = "", values_a = NULL, col_b = "", values_b = NULL) {
    if (!nrow(df) || !nzchar(mode %||% "") || !mode %in% c("exclude", "keep")) return("")
    if (!nzchar(col_a %||% "") || !nzchar(col_b %||% "") || !col_a %in% names(df) || !col_b %in% names(df)) return("")
    if (!length(values_a) || !length(values_b)) return("")
    fmt_vals <- function(v) {
      vals <- unique(as.character(v))
      shown <- paste(utils::head(vals, 6), collapse = ", ")
      if (length(vals) > 6) shown <- paste0(shown, ", ...")
      shown
    }
    action <- if (identical(mode, "exclude")) "excluding rows where" else "keeping only rows where"
    sprintf(
      "Combination rule: %s %s in [%s] AND %s in [%s]",
      action,
      plot_display_label(col_a, width = 30, multiline = FALSE),
      fmt_vals(values_a),
      plot_display_label(col_b, width = 30, multiline = FALSE),
      fmt_vals(values_b)
    )
  }

  tab4_active_filter_summary_lines <- reactive({
    df <- tab4_source_table()
    if (!nrow(df)) return("No saved tab-4 source table is available.")
    lines <- character()
    main_line <- describe_tab4_single_filter(
      df,
      input$tab4_filter_column %||% "",
      input$tab4_filter_numeric_range,
      input$tab4_filter_values,
      input$tab4_filter_numeric_values,
      input$tab4_filter_numeric_min,
      input$tab4_filter_numeric_max,
      tab4_exact_numeric_filter_enabled(df, input$tab4_filter_column %||% ""),
      input$tab4_filter_exact_mode %||% tab4_default_exact_numeric_mode(df, input$tab4_filter_column %||% "")
    )
    if (nzchar(main_line)) lines <- c(lines, main_line)
    for (i in tab4_additional_filter_indices) {
      line_i <- describe_tab4_single_filter(
        df,
        input[[paste0("tab4_filter_column_", i)]] %||% "",
        input[[paste0("tab4_filter_numeric_range_", i)]],
        input[[paste0("tab4_filter_values_", i)]],
        input[[paste0("tab4_filter_numeric_values_", i)]],
        input[[paste0("tab4_filter_numeric_min_", i)]],
        input[[paste0("tab4_filter_numeric_max_", i)]],
        tab4_exact_numeric_filter_enabled(df, input[[paste0("tab4_filter_column_", i)]] %||% ""),
        input[[paste0("tab4_filter_exact_mode_", i)]] %||% tab4_default_exact_numeric_mode(df, input[[paste0("tab4_filter_column_", i)]] %||% "")
      )
      if (nzchar(line_i)) lines <- c(lines, line_i)
    }
    for (i in tab4_combination_filter_indices) {
      combo_line <- describe_tab4_combination_rule(
        df,
        mode = input[[paste0("tab4_combo_mode_", i)]] %||% "",
        col_a = input[[paste0("tab4_combo_column_a_", i)]] %||% "",
        values_a = input[[paste0("tab4_combo_values_a_", i)]],
        col_b = input[[paste0("tab4_combo_column_b_", i)]] %||% "",
        values_b = input[[paste0("tab4_combo_values_b_", i)]]
      )
      if (nzchar(combo_line)) lines <- c(lines, combo_line)
    }
    if (".is_cutoff_training_image" %in% names(df) && !isTRUE(input$tab4_include_cutoff_training %||% TRUE)) {
      lines <- c(lines, "Cutoff-optimization training images excluded.")
    }
    if (".is_ilastik_training_image" %in% names(df) && !isTRUE(input$tab4_include_ilastik_training %||% TRUE)) {
      lines <- c(lines, "Ilastik-training images excluded.")
    }
    max_rows <- suppressWarnings(as.integer(input$tab4_max_rows %||% 0L))
    if (is.finite(max_rows) && max_rows > 0) {
      lines <- c(lines, sprintf("Random subsampling capped at %s row(s) with seed %s.", max_rows, suppressWarnings(as.integer(input$tab4_random_seed %||% 42L))))
    }
    if (!length(lines)) "No additional Tab 4 subset filters were applied beyond keeping the currently available analysis table." else lines
  })

  output$tab4_filter_status_ui <- renderUI({
    full <- tab4_source_table()
    filtered <- tab4_filtered_table()
    cutoff_n <- if (".is_cutoff_training_image" %in% names(full)) sum(full$.is_cutoff_training_image %in% TRUE, na.rm = TRUE) else 0L
    ilastik_n <- if (".is_ilastik_training_image" %in% names(full)) sum(full$.is_ilastik_training_image %in% TRUE, na.rm = TRUE) else 0L
    tags$div(
      class = if (nrow(filtered)) "step-summary-box step-summary-ready" else "step-summary-box step-summary-missing",
      tags$strong("Current tab 4 analysis subset"),
      tags$p(sprintf("Rows currently analyzed: %s / %s", nrow(filtered), nrow(full))),
      tags$p(sprintf("Detected training rows in full table: cutoff optimization = %s; ilastik = %s", cutoff_n, ilastik_n)),
      tags$p("Set max rows to 0 to disable random subsampling.")
    )
  })

  output$tab4_training_audit_summary <- renderUI({
    df <- tab4_source_table()
    train <- tab4_ilastik_training_files()
    matches <- tab4_ilastik_training_matches()
    cutoff_keys <- tab4_cutoff_training_keys()
    unique_images <- if (nrow(df) && "image_name" %in% names(df)) length(unique(df$image_name)) else 0L
    model_rows <- nrow(df)
    profile_count <- if (nrow(df) && "profile_label" %in% names(df)) length(unique(df$profile_label)) else 1L
    cutoff_unique <- if (".is_cutoff_training_image" %in% names(df) && "image_name" %in% names(df)) length(unique(df$image_name[df$.is_cutoff_training_image %in% TRUE])) else 0L
    ilastik_unique <- if (".is_ilastik_training_image" %in% names(df) && "image_name" %in% names(df)) length(unique(df$image_name[df$.is_ilastik_training_image %in% TRUE])) else 0L
    cutoff_rows <- if (".is_cutoff_training_image" %in% names(df)) sum(df$.is_cutoff_training_image %in% TRUE, na.rm = TRUE) else 0L
    ilastik_rows <- if (".is_ilastik_training_image" %in% names(df)) sum(df$.is_ilastik_training_image %in% TRUE, na.rm = TRUE) else 0L
    high_or_medium <- if (nrow(matches)) sum(grepl("high|medium", matches$confidence, ignore.case = TRUE)) else 0L
    tags$div(
      class = "step-summary-box step-summary-neutral",
      tags$strong("Training-image identification summary"),
      tags$p(sprintf("Current tab-4 table contains %s unique image(s) and %s image-model row(s) across %s cutoff profile(s).", unique_images, model_rows, profile_count)),
      tags$p(sprintf("Cutoff optimization training image keys detected from manual choice/noise labels: %s", length(cutoff_keys))),
      tags$p(sprintf("Unique images marked as cutoff-training images: %s; image-model rows marked as cutoff-training rows: %s", cutoff_unique, cutoff_rows)),
      tags$p(sprintf("Raw ilastik training files detected: %s", nrow(train))),
      tags$p(sprintf("Ilastik training files with high/medium analysis-table match confidence: %s / %s", high_or_medium, nrow(matches))),
      tags$p(sprintf("Unique images marked as ilastik-training images: %s; image-model rows marked as ilastik-training rows: %s", ilastik_unique, ilastik_rows)),
      tags$p("Use the shared tab-4 filter above to include or exclude cutoff-training and ilastik-training images before running PCA, contrasts, grouped plots, or ANCOVA.")
    )
  })

  output$tab4_training_role_counts_table <- renderTable({
    df <- tab4_source_table()
    if (!nrow(df) || !".training_image_role" %in% names(df)) return(data.frame(Message = "No tab-4 analysis table is available yet."))
    row_counts <- sort(table(df$.training_image_role, useNA = "ifany"), decreasing = TRUE)
    unique_counts <- tapply(df$image_name, df$.training_image_role, function(x) length(unique(x)))
    out <- data.frame(
      Training_role = names(row_counts),
      Unique_images = as.integer(unique_counts[names(row_counts)]),
      Image_model_rows = as.integer(row_counts),
      stringsAsFactors = FALSE
    )
    apply_table_value_filter("tab4_training_role_counts_table", out)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$tab4_ilastik_training_match_table <- renderTable({
    matches <- tab4_ilastik_training_matches()
    if (!nrow(matches)) return(data.frame(Message = "No ilastik training images were detected or no tab-4 analysis image names are available."))
    apply_table_value_filter("tab4_ilastik_training_match_table", matches)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$tab4_cutoff_training_keys_table <- renderTable({
    keys <- tab4_cutoff_training_keys()
    if (!length(keys)) return(data.frame(Message = "No cutoff optimization training image keys were detected from the inspected validation outputs."))
    apply_table_value_filter("tab4_cutoff_training_keys_table", data.frame(Cutoff_training_image_key = keys, stringsAsFactors = FALSE))
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$metadata_feature_picker_ui <- renderUI({
    meta <- read_metadata_table()
    if (!nrow(meta)) return(tags$p("No metadata CSV is available yet. Check the metadata path in Configuration."))
    choices <- setdiff(names(meta), c(".image_key", ".metadata_image_col"))
    selectInput("metadata_inspect_feature", "Metadata feature to inspect/filter", choices = choices, selected = choices[[1]])
  })

  output$metadata_value_filter_ui <- renderUI({
    meta <- read_metadata_table()
    feature <- input$metadata_inspect_feature %||% ""
    if (!nrow(meta) || !feature %in% names(meta)) return(NULL)
    vals <- sort(unique(as.character(meta[[feature]])))
    vals <- vals[nzchar(vals)]
    selectInput("metadata_inspect_values", "Show only these values", choices = vals, selected = vals, multiple = TRUE)
  })

  output$metadata_combination_picker_ui <- renderUI({
    meta <- read_metadata_table()
    if (!nrow(meta)) return(NULL)
    choices <- setdiff(names(meta), c(".image_key", ".metadata_image_col"))
    selected <- intersect(c(input$metadata_inspect_feature %||% "", "profile_label"), choices)
    selected <- selected[nzchar(selected)]
    selectInput(
      "metadata_combination_features",
      "Metadata feature combination to count/balance",
      choices = choices,
      selected = unique(head(selected, 2)),
      multiple = TRUE
    )
  })

  filtered_metadata_table <- reactive({
    meta <- read_metadata_table()
    feature <- input$metadata_inspect_feature %||% ""
    vals <- input$metadata_inspect_values
    if (nrow(meta) && nzchar(feature) && feature %in% names(meta) && length(vals)) {
      meta <- meta[as.character(meta[[feature]]) %in% vals, , drop = FALSE]
    }
    meta
  })

  output$metadata_inspection_summary <- renderUI({
    meta <- filtered_metadata_table()
    all_meta <- read_metadata_table()
    analysis_df <- generalized_analysis_table()
    saved_df <- saved_generalized_analysis_table()
    available_keys <- unique(c(analysis_df$.image_key %||% character(), saved_df$.image_key %||% character()))
    metadata_keys <- unique(all_meta$.image_key %||% character())
    matched <- intersect(metadata_keys, available_keys)
    unmatched_metadata <- setdiff(metadata_keys, available_keys)
    unmatched_analysis <- setdiff(available_keys, metadata_keys)
    tags$div(
      class = "step-summary-box step-summary-neutral",
      tags$strong("Metadata inspection summary"),
      tags$p(sprintf("Metadata preview/filter rows shown: %s / %s total metadata rows", nrow(meta), nrow(all_meta))),
      tags$p(sprintf("Metadata columns: %s", if (nrow(all_meta)) ncol(all_meta) - 2L else 0L)),
      tags$p(sprintf("Image-name column detected: %s", if (nrow(all_meta)) all_meta$.metadata_image_col[[1]] else "not available")),
      tags$p(sprintf("Metadata image keys matched to current/saved analysis rows: %s", length(matched))),
      tags$p(sprintf("Metadata-only image keys: %s", length(unmatched_metadata))),
      tags$p(sprintf("Analysis-only image keys without metadata: %s", length(unmatched_analysis))),
      tags$p("Note: the table preview below is intentionally limited. Use the diagnostics tables to assess full metadata coverage.")
    )
  })

  output$metadata_inspection_table <- renderTable({
    apply_table_value_filter("metadata_inspection_table", head(filtered_metadata_table(), 50))
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$metadata_unique_values_table <- renderTable({
    meta <- read_metadata_table()
    feature <- input$metadata_inspect_feature %||% ""
    if (!nrow(meta) || !feature %in% names(meta)) return(data.frame(Message = "Select a metadata feature."))
    vals <- as.character(meta[[feature]])
    counts <- sort(table(vals, useNA = "ifany"), decreasing = TRUE)
    if (!length(counts)) return(data.frame(Message = "No values found for this feature."))
    tab <- data.frame(Unique_value = names(counts), Metadata_rows = as.integer(counts), stringsAsFactors = FALSE)
    names(tab) <- c("Unique_value", "Metadata_rows")
    tab$Percent <- round(100 * tab$Metadata_rows / sum(tab$Metadata_rows), 2)
    apply_table_value_filter("metadata_unique_values_table", tab)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  metadata_combination_counts <- reactive({
    meta <- read_metadata_table()
    features <- input$metadata_combination_features %||% character()
    features <- intersect(features, names(meta))
    if (!nrow(meta) || !length(features)) return(data.frame())
    combo <- meta[, features, drop = FALSE]
    for (nm in names(combo)) combo[[nm]] <- as.character(combo[[nm]])
    combo$.combination <- apply(combo, 1, function(x) paste(ifelse(nzchar(x), x, "<blank>"), collapse = " | "))
    counts <- sort(table(combo$.combination, useNA = "ifany"), decreasing = TRUE)
    if (!length(counts)) return(data.frame())
    tab <- data.frame(Metadata_combination = names(counts), Metadata_rows = as.integer(counts), stringsAsFactors = FALSE)
    tab$Percent <- round(100 * tab$Metadata_rows / sum(tab$Metadata_rows), 2)
    expected <- mean(tab$Metadata_rows)
    tab$Balance_ratio_vs_expected <- round(tab$Metadata_rows / expected, 3)
    apply_table_value_filter("metadata_combination_counts_table", tab)
  })

  output$metadata_combination_counts_table <- renderTable({
    tab <- metadata_combination_counts()
    if (!nrow(tab)) return(data.frame(Message = "Select one or more metadata features for combination counts."))
    tab
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$metadata_balance_summary_ui <- renderUI({
    tab <- metadata_combination_counts()
    if (!nrow(tab)) return(tags$div(class = "step-summary-box step-summary-missing", "No metadata combination selected."))
    min_n <- min(tab$Metadata_rows)
    max_n <- max(tab$Metadata_rows)
    ratio <- if (min_n > 0) max_n / min_n else Inf
    cls <- if (is.finite(ratio) && ratio <= 1.5) "step-summary-box step-summary-ready" else if (is.finite(ratio) && ratio <= 3) "step-summary-box step-summary-partial" else "step-summary-box step-summary-missing"
    tags$div(
      class = cls,
      tags$strong("Metadata balance diagnostics"),
      tags$p(sprintf("Combinations detected: %s", nrow(tab))),
      tags$p(sprintf("Smallest group: %s row(s)", min_n)),
      tags$p(sprintf("Largest group: %s row(s)", max_n)),
      tags$p(sprintf("Largest/smallest ratio: %s", if (is.finite(ratio)) round(ratio, 2) else "infinite")),
      tags$p(if (is.finite(ratio) && ratio <= 1.5) "Balance looks good." else if (is.finite(ratio) && ratio <= 3) "Moderate imbalance: interpretation should account for unequal group sizes." else "Strong imbalance: some comparisons may be underpowered or misleading.")
    )
  })

  output$metadata_match_diagnostics_table <- renderTable({
    meta <- read_metadata_table()
    analysis_df <- generalized_analysis_table()
    saved_df <- saved_generalized_analysis_table()
    available_keys <- unique(c(analysis_df$.image_key %||% character(), saved_df$.image_key %||% character()))
    metadata_keys <- unique(meta$.image_key %||% character())
    out <- data.frame(
      Diagnostic = c(
        "Total metadata rows",
        "Unique metadata image keys",
        "Unique current/saved analysis image keys",
        "Matched image keys",
        "Metadata-only image keys",
        "Analysis-only image keys without metadata"
      ),
      Count = c(
        nrow(meta),
        length(metadata_keys),
        length(available_keys),
        length(intersect(metadata_keys, available_keys)),
        length(setdiff(metadata_keys, available_keys)),
        length(setdiff(available_keys, metadata_keys))
      )
    )
    apply_table_value_filter("metadata_match_diagnostics_table", out)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$generalization_model_picker_ui <- renderUI({
    choices <- cutoff_profile_choices()
    selected <- intersect(normalize_generalization_profiles(input$generalization_models), unname(choices))
    if (!length(selected)) selected <- "__current__"
    selectInput("generalization_models", "Cutoff model/profile(s) to apply", choices = choices, selected = selected, multiple = TRUE)
  })

  output$generalization_feature_picker_ui <- renderUI({
    checkboxGroupInput(
      "generalization_features",
      "Features to measure and export",
      choices = generalization_feature_choices(),
      selected = c("image_total_skeleton_length", "image_total_skeleton_length_per_confluence", "image_mean_skeleton_length", "image_median_skeleton_length", "image_median_skeleton_length_per_confluence", "retained_roi_count", "retained_roi_fraction")
    )
  })

  output$generalization_analysis_preview <- renderTable({
    df <- generalized_analysis_table()
    if (!nrow(df)) df <- saved_generalized_analysis_table()
    apply_table_value_filter("generalization_analysis_preview", head(df, 25))
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$generalization_analysis_status <- renderUI({
    df <- generalized_analysis_table()
    saved_df <- saved_generalized_analysis_table()
    diag <- generalization_input_diagnostics(input$generalization_models, settings())
    selected_profiles <- normalize_generalization_profiles(input$generalization_models)
    all_inputs_look_ready <- nrow(diag) && all(diag$Raw_table_exists, diag$Raw_has_image_name, diag$Raw_has_roi_name, diag$Raw_has_skeleton_length)
    tags$div(
      class = if (nrow(saved_df) || nrow(df)) "step-summary-box step-summary-ready" else if (isTRUE(all_inputs_look_ready)) "step-summary-box step-summary-neutral" else "step-summary-box step-summary-missing",
      tags$strong("Generalized feature table"),
      tags$p(sprintf("Preview rows in memory: %s", nrow(df))),
      tags$p(sprintf("Saved production/analysis rows for tab 4: %s", nrow(saved_df))),
      tags$p(sprintf("Saved columns: %s", if (nrow(saved_df)) ncol(saved_df) else if (nrow(df)) ncol(df) else 0L)),
      tags$p(sprintf("Selected profile(s): %s", paste(selected_profiles, collapse = ", "))),
      if (nrow(saved_df)) {
        tags$p("A saved all-image/analysis table is available for tab 4. If this was generated by 'Apply Selected Cutoff Model to All Images', it is the production output.")
      } else if (!nrow(df) && isTRUE(all_inputs_look_ready)) {
        tags$p("Inputs look readable, but no all-image production table is saved yet. Click 'Apply Selected Cutoff Model to All Images' for production, or build the validation-table preview for a quick check.")
      } else if (!nrow(df)) {
        tags$p("No table is built yet, and one or more selected profile inputs appear incomplete. See the diagnostics table below.")
      },
      tags$p("This table is the handoff for tab 4 multivariate analysis, dimensionality reduction, clustering, and contrast-based variable selection.")
    )
  })

  output$generalization_input_diagnostics_table <- renderTable({
    diag <- generalization_input_diagnostics(input$generalization_models, settings())
    if (!nrow(diag)) return(data.frame(Message = "No profile diagnostics available."))
    apply_table_value_filter("generalization_input_diagnostics_table", diag)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  generalized_analysis_output_paths <- function(settings) {
    p <- build_paths(settings)
    out_dir <- file.path(p$output_root, "OUT_generalized_analysis")
    list(
      dir = out_dir,
      csv = file.path(out_dir, "generalized_model_feature_table.csv"),
      rds = file.path(out_dir, "generalized_model_feature_table.rds")
    )
  }

  observeEvent(input$save_generalization_analysis_btn, {
    generalization_tab_selected("3.2. Analysis Specification and Export")
    try(updateTabsetPanel(session, "generalization_tabs", selected = "3.2. Analysis Specification and Export"), silent = TRUE)
    df <- generalized_analysis_table()
    if (!nrow(df)) {
      selected_profiles <- normalize_generalization_profiles(input$generalization_models)
      selected_features <- normalize_generalization_features(input$generalization_features)
      df_holder <- new.env(parent = emptyenv())
      df_holder$value <- df
      withProgress(message = "Building generalized model-feature table before saving", value = 0.1, {
        append_log("Tab 3.2:", "No cached generalized table was available, so building it before save.")
        built_df <- build_generalized_analysis_table(selected_profiles, selected_features, settings())
        generalization_analysis_cache(built_df)
        append_log("Tab 3.2:", sprintf("Generalized table build finished with %s row(s) and %s column(s).", nrow(built_df), if (nrow(built_df)) ncol(built_df) else 0L))
        df_holder$value <- built_df
      })
      df <- df_holder$value
      if (!nrow(df)) {
        showNotification("No generalized analysis table is available to save yet. Check selected profiles and ROI tables.", type = "warning", duration = 8)
        return()
      }
    }
    out <- generalized_analysis_output_paths(settings())
    dir.create(out$dir, recursive = TRUE, showWarnings = FALSE)
    write.csv(df, out$csv, row.names = FALSE)
    saveRDS(df, out$rds)
    showNotification(sprintf("Saved generalized analysis table with %s rows.", nrow(df)), type = "message")
    tick(tick() + 1)
    restore_current_tabs()
  }, ignoreInit = TRUE)

  output$generalization_saved_paths_ui <- renderUI({
    out <- generalized_analysis_output_paths(settings())
    tags$div(
      class = "step-summary-box step-summary-neutral",
      tags$strong("Saved analysis outputs"),
      tags$p(tags$strong("CSV: "), tags$span(class = "path-block", out$csv)),
      tags$p(tags$strong("RDS: "), tags$span(class = "path-block", out$rds)),
      tags$p(if (file.exists(out$csv)) sprintf("CSV last modified: %s", file.info(out$csv)$mtime) else "CSV not saved yet.")
    )
  })

  generalized_numeric_columns <- reactive({
    df <- tab4_source_table()
    if (!nrow(df)) return(character())
    names(df)[vapply(df, vector_is_numeric_like, logical(1))]
  })

  generalized_group_columns <- reactive({
    df <- tab4_source_table()
    if (!nrow(df)) return(character())
    blocked <- c(".image_key")
    cols <- setdiff(names(df), blocked)
    cols[vapply(df[cols], function(x) !is.numeric(x) || length(unique(x[!is.na(x)])) <= 30, logical(1))]
  })

  choose_groupwise_control_level <- function(levels_vec) {
    levels_vec <- as.character(levels_vec %||% character())
    levels_vec <- levels_vec[nzchar(levels_vec)]
    if (!length(levels_vec)) return("")
    preferred <- grep("(^|[^A-Za-z])(control|ctrl|vehicle|untreated|none|baseline)([^A-Za-z]|$)", levels_vec, ignore.case = TRUE)
    if (length(preferred)) return(levels_vec[[preferred[[1]]]])
    levels_vec[[1]]
  }

  significance_stars <- function(p) {
    if (!is.finite(p)) return("")
    if (p < 0.001) return("***")
    if (p < 0.01) return("**")
    if (p < 0.05) return("*")
    "ns"
  }

  selected_generalization_stats_tests <- function() {
    tests <- input$generalization_stats_tests %||% c("welch", "kruskal")
    tests <- intersect(tests, c("welch", "kruskal"))
    if (!length(tests)) tests <- c("welch")
    tests
  }

  selected_generalization_stats_scope <- function(prep = NULL) {
    scope <- input$generalization_stats_scope %||% "within"
    if (!scope %in% c("within", "within_and_across", "across")) scope <- "within"
    prep <- prep %||% tryCatch(generalization_group_plot_data(), error = function(e) NULL)
    if (is.null(prep) || !isTRUE(prep$ok) || !isTRUE(prep$apply_facets)) return("within")
    scope
  }

  parse_generalization_plot_ymax <- function(x) {
    val <- suppressWarnings(as.numeric(x %||% NA))
    if (length(val) && is.finite(val) && val > 0) val[[1]] else NA_real_
  }

  parse_generalization_palette_limit <- function(x) {
    val <- suppressWarnings(as.numeric(x %||% NA))
    if (length(val) && is.finite(val)) val[[1]] else NA_real_
  }

  generalization_bar_summary_value <- function(x, metric = "mean") {
    vals <- suppressWarnings(as.numeric(x))
    vals <- vals[is.finite(vals)]
    if (!length(vals)) return(NA_real_)
    metric <- metric %||% "mean"
    if (identical(metric, "median")) {
      return(stats::median(vals, na.rm = TRUE))
    }
    if (identical(metric, "trimmed_mean")) {
      return(mean(vals, trim = 0.1, na.rm = TRUE))
    }
    mean(vals, na.rm = TRUE)
  }

  generalization_bar_summary_label <- function(metric = "mean") {
    switch(
      metric %||% "mean",
      "median" = "Group median",
      "trimmed_mean" = "Group trimmed mean (10%)",
      "Group mean"
    )
  }

  generalization_group_plot_data <- reactive({
    df <- tab4_filtered_table()
    var <- input$generalization_plot_variable %||% ""
    normalize_by <- input$generalization_plot_normalize_by %||% ""
    group <- input$generalization_plot_group %||% ""
    group2 <- input$generalization_plot_group2 %||% ""
    color_by <- input$generalization_plot_color_by %||% "__group__"
    bar_summary_metric <- input$generalization_plot_bar_summary %||% "mean"
    y_max <- parse_generalization_plot_ymax(input$generalization_plot_ymax)
    palette_min <- parse_generalization_palette_limit(input$generalization_plot_palette_min)
    palette_max <- parse_generalization_palette_limit(input$generalization_plot_palette_max)
    if (!nrow(df) || !var %in% names(df) || !group %in% names(df)) {
      return(list(ok = FALSE, message = "Select a saved/generalized table, variable, and grouping feature."))
    }
    df$.y <- suppressWarnings(as.numeric(df[[var]]))
    y_label <- plot_display_label(var, width = 36)
    plot_title_var <- plot_display_label(var, width = 60, multiline = FALSE)
    if (nzchar(normalize_by) && normalize_by %in% names(df) && !identical(normalize_by, var)) {
      denom <- suppressWarnings(as.numeric(df[[normalize_by]]))
      invalid <- !is.finite(denom) | denom == 0
      df$.y[invalid] <- NA_real_
      df$.y[!invalid] <- df$.y[!invalid] / denom[!invalid]
      y_label <- paste(plot_display_label(var, width = 28, multiline = FALSE), "/", plot_display_label(normalize_by, width = 28, multiline = FALSE))
      plot_title_var <- paste(plot_display_label(var, width = 42, multiline = FALSE), "normalized by", plot_display_label(normalize_by, width = 42, multiline = FALSE))
    }
    grouping_cols <- c(group, if (nzchar(group2) && group2 %in% names(df)) group2 else character())
    df$.group <- if (length(grouping_cols) > 1) {
      ordered_interaction_factor(df, grouping_cols)
    } else {
      ordered_factor_for_plot(df[[group]])
    }
    df <- df[is.finite(df$.y) & !is.na(df$.group), , drop = FALSE]
    if (!nrow(df)) {
      return(list(ok = FALSE, message = "No finite rows are available after applying the current filters, variable choice, and optional normalization."))
    }
    facet_split <- build_generalization_facet_split(df)
    apply_facets <- isTRUE(input$generalization_plot_apply_facets %||% FALSE) && isTRUE(facet_split$ok)
    if (apply_facets) {
      df$.facet <- facet_split$facet
      df <- df[!is.na(df$.facet), , drop = FALSE]
      if (!nrow(df)) {
        return(list(ok = FALSE, message = "No rows remain after applying the selected facet split."))
      }
    } else {
      df$.facet <- factor("All rows")
    }
    levels_x <- levels(droplevels(df$.group))
    if (!length(levels_x)) levels_x <- unique(as.character(df$.group))
    display_levels_x <- plot_display_labels(gsub(" \\| ", " | ", levels_x), width = 22)
    color_source <- if (identical(color_by, "__group__") || !color_by %in% names(df)) group else color_by
    color_source_label <- plot_display_label(color_source, width = 28, multiline = FALSE)
    color_is_numeric <- color_source %in% names(df) && vector_is_numeric_like(df[[color_source]])
    list(
      ok = TRUE,
      df = df,
      var = var,
      normalize_by = normalize_by,
      group = group,
      group2 = group2,
      color_by = color_by,
      bar_summary_metric = bar_summary_metric,
      bar_summary_label = generalization_bar_summary_label(bar_summary_metric),
      y_label = y_label,
      plot_title_var = plot_title_var,
      facet_split = facet_split,
      apply_facets = apply_facets,
      levels_x = levels_x,
      display_levels_x = display_levels_x,
      color_source = color_source,
      color_source_label = color_source_label,
      color_is_numeric = color_is_numeric,
      y_max = y_max,
      palette_min = palette_min,
      palette_max = palette_max
    )
  })

  generalization_group_stats_table_title <- reactive({
    prep <- generalization_group_plot_data()
    if (!isTRUE(prep$ok)) return("Grouped plot statistics")
    mode <- input$generalization_stats_mode %||% "summary"
    alpha <- suppressWarnings(as.numeric(input$generalization_stats_alpha %||% 0.05))
    if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05
    tests <- selected_generalization_stats_tests()
    scope <- selected_generalization_stats_scope(prep)
    test_text <- paste(c(
      if ("welch" %in% tests) "Welch ANOVA + Holm-adjusted Welch t-tests",
      if ("kruskal" %in% tests) "Kruskal-Wallis + Holm-adjusted pairwise Wilcoxon tests"
    ), collapse = " and ")
    facet_note <- if (isTRUE(prep$apply_facets) && isTRUE(prep$facet_split$ok)) {
      scope_note <- switch(
        scope,
        "within" = "computed separately within each facet",
        "within_and_across" = "computed within each facet and also across facet levels for the same plotted group",
        "across" = "focused on across-facet comparisons for the same plotted group",
        "computed separately within each facet"
      )
      paste0(" (", scope_note, " of ", plot_display_label(prep$facet_split$variable, width = 42, multiline = FALSE), ")")
    } else {
      ""
    }
    if (identical(mode, "pairwise")) {
      paste0(test_text, " for ", prep$plot_title_var, " by ", plot_display_label(prep$group, width = 42, multiline = FALSE), " (alpha = ", sprintf("%.3g", alpha), ")", facet_note)
    } else if (identical(mode, "control")) {
      control_level <- input$generalization_stats_control_group %||% choose_groupwise_control_level(prep$levels_x)
      paste0(test_text, " versus control ", shQuote(control_level), " for ", prep$plot_title_var, " by ", plot_display_label(prep$group, width = 42, multiline = FALSE), " (alpha = ", sprintf("%.3g", alpha), ")", facet_note)
    } else {
      paste0("Descriptive grouped summary for ", prep$plot_title_var, " by ", plot_display_label(prep$group, width = 42, multiline = FALSE), " (reference alpha = ", sprintf("%.3g", alpha), ")", facet_note)
    }
  })

  generalization_group_stats_table_data <- reactive({
    prep <- generalization_group_plot_data()
    if (!isTRUE(prep$ok)) return(data.frame(Message = prep$message, stringsAsFactors = FALSE))
    df <- prep$df
    mode <- input$generalization_stats_mode %||% "summary"
    control_level <- input$generalization_stats_control_group %||% choose_groupwise_control_level(prep$levels_x)
    alpha <- suppressWarnings(as.numeric(input$generalization_stats_alpha %||% 0.05))
    if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05
    tests <- selected_generalization_stats_tests()
    scope <- selected_generalization_stats_scope(prep)
    include_within <- !identical(scope, "across")
    include_across <- isTRUE(prep$apply_facets) && (identical(scope, "within_and_across") || identical(scope, "across"))
    mean_col <- paste0("Mean (", prep$plot_title_var, ")")
    sd_col <- paste0("SD (", prep$plot_title_var, ")")
    median_col <- paste0("Median (", prep$plot_title_var, ")")
    iqr_col <- paste0("IQR range (Q1 to Q3) (", prep$plot_title_var, ")")
    format_p_value <- function(x) {
      if (!is.finite(x)) return(NA_character_)
      if (x < 1e-4) return("<0.0001")
      sprintf("%.4g", x)
    }
    summarize_panel <- function(panel_df, facet_label) {
      group_levels <- levels(droplevels(panel_df$.group))
      if (!length(group_levels)) group_levels <- unique(as.character(panel_df$.group))
      summary_rows <- lapply(group_levels, function(g) {
        vals <- panel_df$.y[as.character(panel_df$.group) == g]
        q <- stats::quantile(vals, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE, type = 7)
        q1 <- if (length(q) >= 1 && is.finite(q[[1]])) sprintf("%.4f", q[[1]]) else NA_character_
        q3 <- if (length(q) >= 2 && is.finite(q[[2]])) sprintf("%.4f", q[[2]]) else NA_character_
        data.frame(
          Facet = facet_label,
          Group = g,
          N = sum(is.finite(vals)),
          Mean = mean(vals, na.rm = TRUE),
          SD = stats::sd(vals, na.rm = TRUE),
          Median = stats::median(vals, na.rm = TRUE),
          IQR = if (!is.na(q1) && !is.na(q3)) paste(q1, "to", q3) else NA_character_,
          stringsAsFactors = FALSE
        )
      })
      out <- do.call(rbind, summary_rows)
      out$Mean <- round(out$Mean, 4)
      out$SD <- round(out$SD, 4)
      out$Median <- round(out$Median, 4)
      if (identical(mode, "summary")) {
        out$Alpha_level <- sprintf("%.3g", alpha)
        out$Comparison_scope <- switch(
          scope,
          "within" = "Within-facet group summaries",
          "within_and_across" = "Within-facet group summaries; across-facet same-group comparisons available only in inferential modes",
          "across" = "Across-facet same-group comparisons available only in inferential modes",
          "Within-facet group summaries"
        )
        out$Selected_test_families <- paste(c(
          if ("welch" %in% tests) "Welch",
          if ("kruskal" %in% tests) "Kruskal-Wallis"
        ), collapse = " | ")
        out$Significance_star_summary <- "Descriptive only"
        return(out)
      }
      out$Alpha_level <- sprintf("%.3g", alpha)
      out$Comparison_scope <- switch(
        scope,
        "within" = "Within-facet group comparisons only",
        "within_and_across" = "Within-facet group comparisons plus across-facet same-group comparisons",
        "across" = "Across-facet same-group comparisons only",
        "Within-facet group comparisons only"
      )
      out$Selected_test_families <- paste(c(
        if ("welch" %in% tests) "Welch",
        if ("kruskal" %in% tests) "Kruskal-Wallis"
      ), collapse = " | ")
      fill_test_family <- function(prefix, omnibus_p, pairwise_df) {
        comparison_col <- paste0(prefix, "_comparison")
        p_col <- paste0(prefix, "_adjusted_p_value")
        omnibus_col <- paste0(prefix, "_omnibus_p_value")
        star_col <- paste0(prefix, "_star_summary")
        out[[comparison_col]] <<- ""
        out[[p_col]] <<- ""
        out[[omnibus_col]] <<- format_p_value(omnibus_p)
        out[[star_col]] <<- ""
        if (identical(mode, "control") && !control_level %in% group_levels) {
          out[[comparison_col]] <<- "Selected control group is not present after the current filtering."
          out[[p_col]] <<- NA_character_
          return(invisible(NULL))
        }
        if (identical(mode, "control") && nrow(pairwise_df)) {
          pairwise_df <- pairwise_df[pairwise_df$Group_A == control_level | pairwise_df$Group_B == control_level, , drop = FALSE]
        }
        for (i in seq_len(nrow(out))) {
          g <- out$Group[[i]]
          if (!nrow(pairwise_df)) {
            out[[comparison_col]][[i]] <<- if (identical(mode, "control") && identical(g, control_level)) "Reference group" else "Not enough groups/replicates for post hoc comparison."
            out[[p_col]][[i]] <<- NA_character_
            out[[star_col]][[i]] <<- ""
            next
          }
          hits <- pairwise_df[pairwise_df$Group_A == g | pairwise_df$Group_B == g, , drop = FALSE]
          if (!nrow(hits)) {
            out[[comparison_col]][[i]] <<- if (identical(mode, "control") && identical(g, control_level)) "Reference group" else "No eligible comparison"
            out[[p_col]][[i]] <<- NA_character_
            out[[star_col]][[i]] <<- ""
            next
          }
          other_groups <- ifelse(hits$Group_A == g, hits$Group_B, hits$Group_A)
          out[[comparison_col]][[i]] <<- paste(sprintf("vs %s", other_groups), collapse = "; ")
          out[[p_col]][[i]] <<- paste(vapply(hits$Adjusted_P_value, format_p_value, character(1)), collapse = "; ")
          star_parts <- ifelse(
            hits$Adjusted_P_value < alpha,
            paste0(other_groups, " ", vapply(hits$Adjusted_P_value, significance_stars, character(1))),
            paste0(other_groups, " ns")
          )
          out[[star_col]][[i]] <<- paste(star_parts, collapse = "; ")
        }
        invisible(NULL)
      }
      build_pairwise_df <- function(pairwise_mat) {
        pairwise_rows <- list()
        if (!is.null(pairwise_mat) && nrow(pairwise_mat) && ncol(pairwise_mat)) {
          idx <- 1L
          for (r in rownames(pairwise_mat)) {
            for (c in colnames(pairwise_mat)) {
              p_val <- suppressWarnings(as.numeric(pairwise_mat[r, c]))
              if (is.finite(p_val)) {
                pairwise_rows[[idx]] <- data.frame(Group_A = c, Group_B = r, Adjusted_P_value = p_val, stringsAsFactors = FALSE)
                idx <- idx + 1L
              }
            }
          }
        }
        if (length(pairwise_rows)) do.call(rbind, pairwise_rows) else data.frame()
      }
      if (include_within) {
        if ("welch" %in% tests) {
          welch_omnibus_p <- tryCatch(stats::oneway.test(.y ~ .group, data = panel_df)$p.value, error = function(e) NA_real_)
          welch_pairwise_mat <- tryCatch(
            stats::pairwise.t.test(panel_df$.y, panel_df$.group, p.adjust.method = "holm", pool.sd = FALSE)$p.value,
            error = function(e) NULL
          )
          fill_test_family("Welch", welch_omnibus_p, build_pairwise_df(welch_pairwise_mat))
        }
        if ("kruskal" %in% tests) {
          kruskal_omnibus_p <- tryCatch(stats::kruskal.test(.y ~ .group, data = panel_df)$p.value, error = function(e) NA_real_)
          kruskal_pairwise_mat <- tryCatch(
            stats::pairwise.wilcox.test(panel_df$.y, panel_df$.group, p.adjust.method = "holm", exact = FALSE)$p.value,
            error = function(e) NULL
          )
          fill_test_family("Kruskal", kruskal_omnibus_p, build_pairwise_df(kruskal_pairwise_mat))
        }
      }
      out
    }
    facet_levels <- levels(droplevels(df$.facet))
    if (!length(facet_levels)) facet_levels <- unique(as.character(df$.facet))
    tables <- lapply(facet_levels, function(facet_label) {
      panel_df <- df[as.character(df$.facet) == facet_label, , drop = FALSE]
      if (!nrow(panel_df)) return(NULL)
      summarize_panel(panel_df, facet_label)
    })
    out <- do.call(rbind, Filter(Negate(is.null), tables))
    if (!nrow(out)) return(data.frame(Message = "No grouped statistics are available for the current selection.", stringsAsFactors = FALSE))
    if (!include_within && !identical(mode, "summary")) {
      for (prefix in c("Welch", "Kruskal")) {
        if (prefix == "Welch" && !"welch" %in% tests) next
        if (prefix == "Kruskal" && !"kruskal" %in% tests) next
        out[[paste0(prefix, "_comparison")]] <- "Within-facet comparisons not requested."
        out[[paste0(prefix, "_adjusted_p_value")]] <- NA_character_
        out[[paste0(prefix, "_omnibus_p_value")]] <- NA_character_
        out[[paste0(prefix, "_star_summary")]] <- ""
      }
    }
    if (include_across && !identical(mode, "summary")) {
      build_across_pairwise_df <- function(pairwise_mat) {
        pairwise_rows <- list()
        if (!is.null(pairwise_mat) && nrow(pairwise_mat) && ncol(pairwise_mat)) {
          idx <- 1L
          for (r in rownames(pairwise_mat)) {
            for (c in colnames(pairwise_mat)) {
              p_val <- suppressWarnings(as.numeric(pairwise_mat[r, c]))
              if (is.finite(p_val)) {
                pairwise_rows[[idx]] <- data.frame(Facet_A = c, Facet_B = r, Adjusted_P_value = p_val, stringsAsFactors = FALSE)
                idx <- idx + 1L
              }
            }
          }
        }
        if (length(pairwise_rows)) do.call(rbind, pairwise_rows) else data.frame()
      }
      fill_across_facet_family <- function(prefix, omnibus_fun, pairwise_fun) {
        comparison_col <- paste0(prefix, "_across_facet_comparison")
        p_col <- paste0(prefix, "_across_facet_adjusted_p_value")
        omnibus_col <- paste0(prefix, "_across_facet_omnibus_p_value")
        star_col <- paste0(prefix, "_across_facet_star_summary")
        out[[comparison_col]] <<- ""
        out[[p_col]] <<- ""
        out[[omnibus_col]] <<- ""
        out[[star_col]] <<- ""
        group_levels_all <- unique(as.character(out$Group))
        for (g in group_levels_all) {
          group_df <- df[as.character(df$.group) == g & !is.na(df$.facet), , drop = FALSE]
          group_rows <- which(as.character(out$Group) == g)
          facet_levels_group <- levels(droplevels(group_df$.facet))
          if (!length(facet_levels_group)) facet_levels_group <- unique(as.character(group_df$.facet))
          if (length(facet_levels_group) < 2) {
            out[[comparison_col]][group_rows] <<- "Only one facet level is present for this plotted group."
            out[[p_col]][group_rows] <<- NA_character_
            out[[omnibus_col]][group_rows] <<- NA_character_
            out[[star_col]][group_rows] <<- ""
            next
          }
          omnibus_p <- tryCatch(omnibus_fun(group_df), error = function(e) NA_real_)
          pairwise_df <- tryCatch(build_across_pairwise_df(pairwise_fun(group_df)), error = function(e) data.frame())
          for (row_idx in group_rows) {
            facet_label <- as.character(out$Facet[[row_idx]])
            out[[omnibus_col]][row_idx] <<- format_p_value(omnibus_p)
            if (!nrow(pairwise_df)) {
              out[[comparison_col]][row_idx] <<- "Not enough facet-level replicates for post hoc comparison."
              out[[p_col]][row_idx] <<- NA_character_
              out[[star_col]][row_idx] <<- ""
              next
            }
            hits <- pairwise_df[pairwise_df$Facet_A == facet_label | pairwise_df$Facet_B == facet_label, , drop = FALSE]
            if (!nrow(hits)) {
              out[[comparison_col]][row_idx] <<- "No eligible across-facet comparison"
              out[[p_col]][row_idx] <<- NA_character_
              out[[star_col]][row_idx] <<- ""
              next
            }
            other_facets <- ifelse(hits$Facet_A == facet_label, hits$Facet_B, hits$Facet_A)
            out[[comparison_col]][row_idx] <<- paste(sprintf("same group vs facet %s", other_facets), collapse = "; ")
            out[[p_col]][row_idx] <<- paste(vapply(hits$Adjusted_P_value, format_p_value, character(1)), collapse = "; ")
            star_parts <- ifelse(
              hits$Adjusted_P_value < alpha,
              paste0(other_facets, " ", vapply(hits$Adjusted_P_value, significance_stars, character(1))),
              paste0(other_facets, " ns")
            )
            out[[star_col]][row_idx] <<- paste(star_parts, collapse = "; ")
          }
        }
      }
      if ("welch" %in% tests) {
        fill_across_facet_family(
          "Welch",
          function(group_df) stats::oneway.test(.y ~ .facet, data = group_df)$p.value,
          function(group_df) stats::pairwise.t.test(group_df$.y, group_df$.facet, p.adjust.method = "holm", pool.sd = FALSE)$p.value
        )
      }
      if ("kruskal" %in% tests) {
        fill_across_facet_family(
          "Kruskal",
          function(group_df) stats::kruskal.test(.y ~ .facet, data = group_df)$p.value,
          function(group_df) stats::pairwise.wilcox.test(group_df$.y, group_df$.facet, p.adjust.method = "holm", exact = FALSE)$p.value
        )
      }
    }
    if (!isTRUE(prep$apply_facets)) out$Facet <- NULL
    names(out)[names(out) == "Mean"] <- mean_col
    names(out)[names(out) == "SD"] <- sd_col
    names(out)[names(out) == "Median"] <- median_col
    names(out)[names(out) == "IQR"] <- iqr_col
    readable_names <- c(
      Welch_comparison = "Welch within-facet comparison",
      Welch_adjusted_p_value = "Welch within-facet adjusted p-value",
      Welch_omnibus_p_value = "Welch within-facet omnibus p-value",
      Welch_star_summary = "Welch within-facet star summary",
      Kruskal_comparison = "Kruskal within-facet comparison",
      Kruskal_adjusted_p_value = "Kruskal within-facet adjusted p-value",
      Kruskal_omnibus_p_value = "Kruskal within-facet omnibus p-value",
      Kruskal_star_summary = "Kruskal within-facet star summary",
      Welch_across_facet_comparison = "Welch across-facet same-group comparison",
      Welch_across_facet_adjusted_p_value = "Welch across-facet same-group adjusted p-value",
      Welch_across_facet_omnibus_p_value = "Welch across-facet same-group omnibus p-value",
      Welch_across_facet_star_summary = "Welch across-facet same-group star summary",
      Kruskal_across_facet_comparison = "Kruskal across-facet same-group comparison",
      Kruskal_across_facet_adjusted_p_value = "Kruskal across-facet same-group adjusted p-value",
      Kruskal_across_facet_omnibus_p_value = "Kruskal across-facet same-group omnibus p-value",
      Kruskal_across_facet_star_summary = "Kruskal across-facet same-group star summary"
    )
    present_names <- intersect(names(readable_names), names(out))
    names(out)[match(present_names, names(out))] <- unname(readable_names[present_names])
    out
  })

  output$generalization_plot_controls_ui <- renderUI({
    nums <- generalized_numeric_columns()
    groups <- generalized_group_columns()
    if (!length(nums) || !length(groups)) return(tags$p("No saved generalized feature table is available yet. Finish tab 3 and save the analysis table first."))
    df <- tab4_source_table()
    facet_choices <- c("No faceting" = "", setNames(setdiff(names(df), c(".image_key")), setdiff(names(df), c(".image_key"))))
    selected_var <- input$generalization_plot_variable %||% nums[[1]]
    if (!selected_var %in% nums) selected_var <- nums[[1]]
    normalization_choices <- c("No normalization" = "", setNames(nums, nums))
    selected_norm <- input$generalization_plot_normalize_by %||% ""
    if (!selected_norm %in% c("", nums) || identical(selected_norm, selected_var)) selected_norm <- ""
    selected_group <- input$generalization_plot_group %||% if ("profile_label" %in% groups) "profile_label" else groups[[1]]
    if (!selected_group %in% groups) selected_group <- if ("profile_label" %in% groups) "profile_label" else groups[[1]]
    selected_group2 <- input$generalization_plot_group2 %||% ""
    if (!selected_group2 %in% c("", groups)) selected_group2 <- ""
    selected_color <- input$generalization_plot_color_by %||% "__group__"
    if (!selected_color %in% c("__group__", nums, groups)) selected_color <- "__group__"
    selected_facet <- input$generalization_plot_facet_by %||% ""
    if (!selected_facet %in% c("", names(df))) selected_facet <- ""
    facet_is_numeric <- nzchar(selected_facet) && selected_facet %in% names(df) && vector_is_numeric_like(df[[selected_facet]])
    stats_mode <- input$generalization_stats_mode %||% "control"
    if (!stats_mode %in% c("summary", "pairwise", "control")) stats_mode <- "control"
    stats_scope <- input$generalization_stats_scope %||% "within"
    if (!stats_scope %in% c("within", "within_and_across", "across")) stats_scope <- "within"
    stats_alpha <- suppressWarnings(as.numeric(input$generalization_stats_alpha %||% 0.05))
    if (!is.finite(stats_alpha) || stats_alpha <= 0 || stats_alpha >= 1) stats_alpha <- 0.05
    stats_tests <- input$generalization_stats_tests %||% c("welch", "kruskal")
    stats_tests <- intersect(stats_tests, c("welch", "kruskal"))
    if (!length(stats_tests)) stats_tests <- c("welch")
    prep <- generalization_group_plot_data()
    control_levels <- if (isTRUE(prep$ok) && length(prep$levels_x)) prep$levels_x else character()
    if (!length(control_levels)) control_levels <- c("No eligible plotted groups" = "")
    selected_control <- input$generalization_stats_control_group %||% choose_groupwise_control_level(control_levels)
    if (!selected_control %in% control_levels) selected_control <- choose_groupwise_control_level(control_levels)
    facet_value_ui <- NULL
    if (nzchar(selected_facet) && selected_facet %in% names(df) && !facet_is_numeric) {
      facet_values <- ordered_unique_values(df[[selected_facet]])
      selected_facet_value <- input$generalization_plot_facet_value %||% facet_values[[1]] %||% ""
      if (!selected_facet_value %in% facet_values) selected_facet_value <- facet_values[[1]] %||% ""
      facet_value_ui <- tagList(
        radioButtons(
          "generalization_plot_categorical_facet_mode",
          "Categorical split behavior",
          choices = c(
            "Show all category values as stacked facets" = "all",
            "Show only one selected value" = "selected_only",
            "Selected value versus all other rows" = "selected_vs_rest"
          ),
          selected = input$generalization_plot_categorical_facet_mode %||% "all"
        ),
        selectInput(
          "generalization_plot_facet_value",
          "Selected categorical value",
          choices = facet_values,
          selected = selected_facet_value
        )
      )
    }
    tagList(
      selectInput("generalization_plot_variable", "Variable to plot", choices = nums, selected = selected_var),
      selectInput(
        "generalization_plot_normalize_by",
        "Normalize plotted values by numeric variable",
        choices = normalization_choices,
        selected = selected_norm
      ),
      selectInput("generalization_plot_group", "Group by metadata/model feature", choices = groups, selected = selected_group),
      selectInput("generalization_plot_group2", "Optional second grouping feature", choices = c("None" = "", groups), selected = selected_group2),
      selectInput("generalization_plot_color_by", "Color dots/bars by", choices = c("Same as x-axis group" = "__group__", unique(c(nums, groups))), selected = selected_color),
      selectInput(
        "generalization_plot_bar_summary",
        "Group-level bar summary metric",
        choices = c(
          "Mean (average)" = "mean",
          "Median (more robust to skew/outliers)" = "median",
          "Trimmed mean, 10% (balances robustness and continuity)" = "trimmed_mean"
        ),
        selected = input$generalization_plot_bar_summary %||% "mean"
      ),
      textInput("generalization_plot_ymax", "Optional y-axis maximum tick/value", value = input$generalization_plot_ymax %||% ""),
      if (isTRUE(selected_color != "__group__") &&
          selected_color %in% names(df) &&
          vector_is_numeric_like(df[[selected_color]])) {
        tagList(
          textInput("generalization_plot_palette_min", "Optional numeric palette minimum", value = input$generalization_plot_palette_min %||% ""),
          textInput("generalization_plot_palette_max", "Optional numeric palette maximum", value = input$generalization_plot_palette_max %||% "")
        )
      },
      tags$hr(),
      tags$h5("Optional vertical faceting / stratification"),
      selectInput("generalization_plot_facet_by", "Facet/split by secondary variable", choices = facet_choices, selected = selected_facet),
      radioButtons(
        "generalization_plot_facet_mode",
        "Split mode for numeric variables",
        choices = c("Quantile bins" = "quantile", "Treat as categories" = "categorical"),
        selected = input$generalization_plot_facet_mode %||% "quantile"
      ),
      textInput(
        "generalization_plot_quantiles",
        "Quantile probabilities for numeric split",
        value = input$generalization_plot_quantiles %||% "0, 0.25, 0.5, 0.75, 1"
      ),
      facet_value_ui,
      checkboxInput(
        "generalization_plot_apply_facets",
        "Apply these split rules as vertical stacked facets",
        value = isTRUE(input$generalization_plot_apply_facets %||% FALSE)
      ),
      tags$hr(),
      tags$h5("Statistics table for the grouped plot"),
      radioButtons(
        "generalization_stats_mode",
        "Comparison mode",
        choices = c(
          "Summary only (group N, mean, SD, median, IQR)" = "summary",
          "All groups versus all others" = "pairwise",
          "Every group versus one selected control" = "control"
        ),
        selected = stats_mode
      ),
      radioButtons(
        "generalization_stats_scope",
        "Comparison scope",
        choices = c(
          "Compare plotted groups within each facet only" = "within",
          "Compare plotted groups within each facet and also compare the same plotted group across facet levels" = "within_and_across",
          "Compare the same plotted group across facet levels only" = "across"
        ),
        selected = if (isTRUE(prep$ok) && isTRUE(prep$apply_facets)) stats_scope else "within"
      ),
      numericInput(
        "generalization_stats_alpha",
        "Alpha level for significance calls",
        value = stats_alpha,
        min = 0.0001,
        max = 0.5,
        step = 0.005
      ),
      checkboxGroupInput(
        "generalization_stats_tests",
        "Test families to include as columns",
        choices = c(
          "Welch ANOVA + Welch t-test post hoc (mean-based, unequal variances allowed)" = "welch",
          "Kruskal-Wallis + Wilcoxon post hoc (rank-based, more robust to skew/outliers)" = "kruskal"
        ),
        selected = stats_tests
      ),
      if (identical(stats_mode, "control")) selectInput(
        "generalization_stats_control_group",
        "Control/reference group",
        choices = control_levels,
        selected = selected_control
      )
    )
  })

  parse_quantile_probabilities <- function(text) {
    vals <- suppressWarnings(as.numeric(trimws(unlist(strsplit(text %||% "", "[,;\\s]+")))))
    vals <- vals[is.finite(vals)]
    vals <- sort(unique(vals[vals >= 0 & vals <= 1]))
    if (length(vals) < 2) vals <- c(0, 0.25, 0.5, 0.75, 1)
    if (min(vals) > 0) vals <- c(0, vals)
    if (max(vals) < 1) vals <- c(vals, 1)
    sort(unique(vals))
  }

  build_generalization_facet_split <- function(df) {
    facet_by <- input$generalization_plot_facet_by %||% ""
    if (!nrow(df) || !nzchar(facet_by) || !facet_by %in% names(df)) {
      return(list(ok = FALSE, message = "No faceting variable selected.", facet = factor(rep("All rows", nrow(df))), cutoffs = numeric(), counts = data.frame()))
    }
    facet_mode <- input$generalization_plot_facet_mode %||% "quantile"
    if (vector_is_numeric_like(df[[facet_by]]) && identical(facet_mode, "quantile")) {
      vals <- suppressWarnings(as.numeric(df[[facet_by]]))
      probs <- parse_quantile_probabilities(input$generalization_plot_quantiles)
      cutoffs <- suppressWarnings(as.numeric(stats::quantile(vals, probs = probs, na.rm = TRUE, type = 7)))
      cutoffs <- sort(unique(cutoffs[is.finite(cutoffs)]))
      if (length(cutoffs) < 2) {
        return(list(ok = FALSE, message = "Selected numeric faceting variable does not have enough finite variation for quantile bins.", facet = factor(rep("All rows", nrow(df))), cutoffs = cutoffs, counts = data.frame()))
      }
      bins <- cut(vals, breaks = cutoffs, include.lowest = TRUE, dig.lab = 6)
      counts <- as.data.frame(table(Bin = bins, useNA = "ifany"), stringsAsFactors = FALSE)
      names(counts) <- c("Facet_bin", "Rows")
      return(list(ok = TRUE, message = "", facet = bins, cutoffs = cutoffs, counts = counts, variable = facet_by, numeric = TRUE, values = vals))
    }
    raw_values <- as.character(df[[facet_by]])
    categorical_mode <- input$generalization_plot_categorical_facet_mode %||% "all"
    selected_value <- input$generalization_plot_facet_value %||% ""
    available_values <- ordered_unique_values(raw_values)
    if (!selected_value %in% available_values) selected_value <- available_values[[1]] %||% ""
    if (identical(categorical_mode, "selected_only") && nzchar(selected_value)) {
      selected_label <- paste0("Selected: ", selected_value)
      facet <- factor(ifelse(raw_values == selected_value, selected_label, NA_character_), levels = selected_label)
      counts <- data.frame(
        Facet_bin = c(selected_label, "Excluded: other values"),
        Rows = c(sum(raw_values == selected_value, na.rm = TRUE), sum(raw_values != selected_value | is.na(raw_values), na.rm = TRUE)),
        stringsAsFactors = FALSE
      )
      return(list(ok = any(!is.na(facet)), message = "", facet = facet, cutoffs = numeric(), counts = counts, variable = facet_by, numeric = FALSE, values = raw_values))
    }
    if (identical(categorical_mode, "selected_vs_rest") && nzchar(selected_value)) {
      selected_label <- paste0("Selected: ", selected_value)
      other_label <- "Other values"
      facet <- factor(ifelse(raw_values == selected_value, selected_label, other_label), levels = c(selected_label, other_label))
      counts <- as.data.frame(table(Facet = facet, useNA = "ifany"), stringsAsFactors = FALSE)
      names(counts) <- c("Facet_bin", "Rows")
      return(list(ok = TRUE, message = "", facet = facet, cutoffs = numeric(), counts = counts, variable = facet_by, numeric = FALSE, values = raw_values))
    }
    facet <- ordered_factor_for_plot(raw_values)
    counts <- as.data.frame(table(Facet = facet, useNA = "ifany"), stringsAsFactors = FALSE)
    names(counts) <- c("Facet_bin", "Rows")
    list(ok = TRUE, message = "", facet = facet, cutoffs = numeric(), counts = counts, variable = facet_by, numeric = FALSE, values = raw_values)
  }

  output$generalization_grouped_plot <- renderPlot({
    on.exit(remember_plot("generalization_grouped_plot"), add = TRUE)
    prep <- generalization_group_plot_data()
    if (!isTRUE(prep$ok)) {
      plot.new(); text(0.5, 0.5, prep$message %||% "Grouped plot is not ready."); return()
    }
    df <- prep$df
    y_label <- prep$y_label
    plot_title_var <- prep$plot_title_var
    group <- prep$group
    facet_split <- prep$facet_split
    apply_facets <- prep$apply_facets
    levels_x <- prep$levels_x
    display_levels_x <- prep$display_levels_x
    color_source <- prep$color_source
    color_source_label <- prep$color_source_label
    color_is_numeric <- prep$color_is_numeric
    bar_summary_metric <- prep$bar_summary_metric %||% "mean"
    bar_summary_label <- prep$bar_summary_label %||% generalization_bar_summary_label(bar_summary_metric)
    y_max <- prep$y_max
    palette_limits <- if (is.finite(prep$palette_min) &&
                          is.finite(prep$palette_max) &&
                          prep$palette_max > prep$palette_min) {
      c(prep$palette_min, prep$palette_max)
    } else {
      NULL
    }
    facet_levels <- levels(droplevels(df$.facet))
    if (!length(facet_levels)) facet_levels <- unique(as.character(df$.facet))
    n_panels <- length(facet_levels)
    op <- par(
      mfrow = c(n_panels, 1),
      mar = c(if (n_panels > 1) 5.5 else 8, 5.2, 3.2, if (color_is_numeric) 8 else 3)
    )
    on.exit(par(op), add = TRUE)
    ylim <- range(c(0, df$.y), na.rm = TRUE)
    if (is.finite(y_max)) {
      ylim <- c(0, y_max)
    } else if (!is.finite(diff(ylim)) || diff(ylim) == 0) {
      ylim <- ylim + c(-0.5, 0.5)
    }
    xlim <- c(0.5, length(levels_x) + 0.5)
    for (facet_label in facet_levels) {
      panel_df <- df[as.character(df$.facet) == facet_label, , drop = FALSE]
      if (!nrow(panel_df)) next
      x <- as.numeric(panel_df$.group)
      set.seed(42)
      jitter_x <- x + runif(length(x), -0.18, 0.18)
      if (color_is_numeric) {
        point_cols <- continuous_palette(panel_df[[color_source]], limits = palette_limits)
        bar_color_values <- tapply(
          suppressWarnings(as.numeric(panel_df[[color_source]])),
          panel_df$.group,
          function(v) generalization_bar_summary_value(v, bar_summary_metric)
        )
        bar_cols <- continuous_palette(bar_color_values, limits = palette_limits)
        names(bar_cols) <- names(bar_color_values)
      } else {
        color_factor <- if (color_source %in% names(panel_df)) ordered_factor_for_plot(panel_df[[color_source]]) else panel_df$.group
        pal <- grDevices::hcl.colors(length(levels(color_factor)), "Dark 3")
        point_cols <- pal[as.integer(color_factor)]
        bar_group <- tapply(as.character(color_factor), panel_df$.group, function(v) names(sort(table(v), decreasing = TRUE))[[1]])
        bar_cols <- pal[match(bar_group, levels(color_factor))]
        names(bar_cols) <- names(bar_group)
      }
      main_title <- if (apply_facets) paste(plot_title_var, "|", plot_display_label(facet_split$variable, width = 32, multiline = FALSE), "=", facet_label) else paste(plot_display_label(plot_title_var, width = 52, multiline = FALSE), "by", plot_display_label(group, width = 32, multiline = FALSE))
      plot(jitter_x, panel_df$.y, xaxt = "n", xlab = "", ylab = y_label, pch = 19, col = point_cols, xlim = xlim, ylim = ylim, main = main_title)
      means <- tapply(panel_df$.y, panel_df$.group, function(v) generalization_bar_summary_value(v, bar_summary_metric))
      bar_centers <- seq_along(levels_x)
      draw_idx <- which(is.finite(means[levels_x]))
      bar_cols_full <- rep("#8fb8de", length(levels_x))
      names(bar_cols_full) <- levels_x
      matching_bar_cols <- intersect(names(bar_cols), levels_x)
      bar_cols_full[matching_bar_cols] <- bar_cols[matching_bar_cols]
      rect(
        bar_centers[draw_idx] - 0.28,
        0,
        bar_centers[draw_idx] + 0.28,
        means[levels_x][draw_idx],
        col = grDevices::adjustcolor(bar_cols_full[draw_idx], alpha.f = 0.38),
        border = "#173f35"
      )
      points(jitter_x, panel_df$.y, pch = 19, col = point_cols)
      points(bar_centers, means[levels_x], pch = 23, bg = "#fbf1d7", col = "#173f35", cex = 1.4)
      axis(1, at = seq_along(levels_x), labels = display_levels_x, las = 2, cex.axis = if (n_panels > 1) 0.62 else 0.75)
      if (!apply_facets || identical(facet_label, facet_levels[[1]])) {
        if (color_is_numeric) {
          draw_continuous_palette_legend(color_source_label, panel_df[[color_source]], limits = palette_limits)
          legend("topright", legend = bar_summary_label, pch = 23, pt.bg = "#fbf1d7", col = "#173f35", bty = "n", cex = 0.75)
        } else if (exists("color_factor") && length(levels(color_factor)) <= 8) {
          legend("topright", legend = c(levels(color_factor), bar_summary_label), pch = c(rep(19, length(levels(color_factor))), 23), pt.bg = c(rep(NA, length(levels(color_factor))), "#fbf1d7"), col = c(pal, "#173f35"), bty = "n", cex = 0.7)
        }
      }
    }
  })

  output$generalization_facet_helper_plot <- renderPlot({
    on.exit(remember_plot("generalization_facet_helper_plot"), add = TRUE)
    df <- tab4_filtered_table()
    split <- build_generalization_facet_split(df)
    if (!nzchar(input$generalization_plot_facet_by %||% "")) {
      plot.new(); text(0.5, 0.5, "Select a secondary faceting variable to preview split rules."); return()
    }
    if (!isTRUE(split$ok)) {
      plot.new(); text(0.5, 0.5, split$message); return()
    }
    op <- par(mar = c(4.5, 4.5, 2.5, 1))
    on.exit(par(op), add = TRUE)
    if (isTRUE(split$numeric)) {
      vals <- split$values[is.finite(split$values)]
      hist(vals, breaks = 30, col = "#8fb8de", border = "white", main = paste("Distribution of", split$variable), xlab = split$variable)
      abline(v = split$cutoffs, col = "#9b2226", lwd = 2, lty = 2)
      legend("topright", legend = "Quantile cutoffs", col = "#9b2226", lwd = 2, lty = 2, bty = "n")
    } else {
      counts <- sort(table(split$facet), decreasing = TRUE)
      barplot(counts, las = 2, col = "#8fb8de", border = "white", main = paste("Facet group sizes for", split$variable), ylab = "Rows")
    }
  })

  output$generalization_facet_group_sizes <- renderTable({
    df <- tab4_filtered_table()
    split <- build_generalization_facet_split(df)
    if (!nzchar(input$generalization_plot_facet_by %||% "")) return(data.frame(Message = "No secondary faceting variable selected."))
    if (!isTRUE(split$ok)) return(data.frame(Message = split$message))
    apply_table_value_filter("generalization_facet_group_sizes", split$counts)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$generalization_group_stats_info_ui <- renderUI({
    prep <- generalization_group_plot_data()
    title_text <- generalization_group_stats_table_title()
    if (!isTRUE(prep$ok)) {
      return(tags$div(
        class = "step-summary-box step-summary-missing",
        tags$strong("Grouped-plot statistics"),
        tags$p(prep$message %||% "Grouped-plot statistics are not ready yet.")
      ))
    }
    full_n <- tryCatch(nrow(tab4_source_table()), error = function(e) 0L)
    filtered_n <- tryCatch(nrow(tab4_filtered_table()), error = function(e) 0L)
    unique_images <- tryCatch({
      d <- tab4_filtered_table()
      if (!nrow(d) || !"image_name" %in% names(d)) 0L else length(unique(d$image_name))
    }, error = function(e) 0L)
    mode <- input$generalization_stats_mode %||% "summary"
    alpha <- suppressWarnings(as.numeric(input$generalization_stats_alpha %||% 0.05))
    if (!is.finite(alpha) || alpha <= 0 || alpha >= 1) alpha <- 0.05
    tests <- selected_generalization_stats_tests()
    stats_scope <- selected_generalization_stats_scope(prep)
    group_df <- prep$df
    skew_check <- tryCatch({
      split_vals <- split(group_df$.y, as.character(group_df$.group))
      stats <- lapply(split_vals, function(v) {
        v <- v[is.finite(v)]
        if (length(v) < 5) return(c(n = length(v), mean = NA_real_, median = NA_real_, sd = NA_real_))
        c(n = length(v), mean = mean(v), median = stats::median(v), sd = stats::sd(v))
      })
      stat_df <- as.data.frame(do.call(rbind, stats), stringsAsFactors = FALSE)
      if (!nrow(stat_df)) FALSE else any(
        is.finite(stat_df$mean) &
        is.finite(stat_df$median) &
        is.finite(stat_df$sd) &
        stat_df$median > 0 &
        ((stat_df$mean / stat_df$median) > 1.25 | (stat_df$sd / stat_df$mean) > 0.9),
        na.rm = TRUE
      )
    }, error = function(e) FALSE)
    measure_text <- if (nzchar(prep$normalize_by %||% "")) {
      sprintf("The mean, SD, median, and IQR columns refer to the currently displayed plotted value: %s divided by %s.", plot_display_label(prep$var, width = 42, multiline = FALSE), plot_display_label(prep$normalize_by, width = 42, multiline = FALSE))
    } else {
      sprintf("The mean, SD, median, and IQR columns refer to the currently displayed plotted value: %s.", plot_display_label(prep$var, width = 52, multiline = FALSE))
    }
    scope_text <- if (!isTRUE(prep$apply_facets)) {
      "No vertical facet split is currently applied, so the table only compares the plotted groups in the current filtered dataset."
    } else {
      switch(
        stats_scope,
        "within" = sprintf("A vertical facet split is active on %s. The inferential columns compare plotted groups separately inside each facet panel only.", plot_display_label(prep$facet_split$variable, width = 42, multiline = FALSE)),
        "within_and_across" = sprintf("A vertical facet split is active on %s. The table includes both within-facet group comparisons and across-facet comparisons asking whether the same plotted group changes between facet levels.", plot_display_label(prep$facet_split$variable, width = 42, multiline = FALSE)),
        "across" = sprintf("A vertical facet split is active on %s. The inferential columns focus on whether the same plotted group changes between facet levels; within-facet group comparisons are intentionally left inactive.", plot_display_label(prep$facet_split$variable, width = 42, multiline = FALSE)),
        "The table follows the current facet split."
      )
    }
    explanation <- if (identical(mode, "summary")) {
      "This mode is descriptive only: it summarizes the currently plotted subset by group using N, mean, SD, median, and IQR, without hypothesis testing. Switch to one of the comparison modes if you want multiplicity-aware p-values directly below the grouped plot."
    } else {
      paste(
        c(
          if ("welch" %in% tests) "Welch ANOVA is a mean-based omnibus test that tolerates unequal variances better than classical one-way ANOVA. Its post hoc columns use Holm-adjusted Welch t-tests to compare groups while controlling the family-wise error rate.",
          if ("kruskal" %in% tests) "Kruskal-Wallis is a rank-based omnibus test that is less sensitive to skew and outliers than mean-based tests. Its post hoc columns use Holm-adjusted pairwise Wilcoxon rank-sum tests, which compare the rank distributions between groups rather than the means themselves."
        ),
        collapse = " "
      )
    }
    scope_interpretation <- if (identical(mode, "summary") || !isTRUE(prep$apply_facets)) {
      NULL
    } else {
      switch(
        stats_scope,
        "within" = "Within-facet interpretation: each row is compared against other plotted groups from the same facet panel only.",
        "within_and_across" = "Across-facet interpretation: columns ending in across_facet ask whether this same plotted group differs between the current facet and the other facet levels. The within-facet columns still compare different plotted groups inside each panel.",
        "across" = "Across-facet interpretation: columns ending in across_facet ask whether this same plotted group differs between the current facet and the other facet levels. Because across-facet-only mode is selected, the within-facet comparison columns are placeholders rather than active tests.",
        NULL
      )
    }
    interpretation <- if (identical(mode, "summary")) {
      "Interpretation: compare the group means and SDs visually and use them as context for the grouped plot. No inferential p-value is being claimed in this mode."
    } else {
      if (identical(mode, "control")) {
        "Interpretation: within each selected test family, the omnibus p-value asks whether any grouped difference is present overall, while the adjusted post hoc p-values tell you whether that row’s group differs from the chosen control after multiple-comparison correction. Compare Welch and Kruskal columns side by side: agreement strengthens confidence, while disagreement suggests that skew, outliers, or the distinction between mean-based and rank-based questions may matter."
      } else {
        "Interpretation: within each selected test family, the omnibus p-value asks whether any grouped difference is present overall, while the adjusted post hoc p-values summarize the row’s pairwise comparisons after multiple-comparison correction. Compare Welch and Kruskal columns side by side: agreement strengthens confidence, while disagreement suggests that skew, outliers, or the distinction between mean-based and rank-based questions may matter."
      }
    }
    filter_lines <- tab4_active_filter_summary_lines()
    tags$div(
      class = "step-summary-box step-summary-neutral",
      tags$strong(title_text),
      tags$p(measure_text),
      tags$p(scope_text),
      tags$p(sprintf("Current subset summary: %s of %s tab-4 row(s) remain after filtering, representing %s unique image(s).", filtered_n, full_n, unique_images)),
      tags$p(sprintf("Alpha level used for significance calls and star summaries: %s. The star shorthand follows the adjusted post hoc p-values: * < 0.05, ** < 0.01, *** < 0.001, and ns = not significant.", sprintf("%.3g", alpha))),
      tags$p(sprintf("Selected test families currently shown as columns: %s.", paste(c(if ("welch" %in% tests) "Welch mean-based inference", if ("kruskal" %in% tests) "Kruskal rank-based inference"), collapse = " and "))),
      if (isTRUE(skew_check)) tags$p(class = "warning-text", "Distribution check: at least one displayed group looks strongly right-skewed based on mean-to-median and SD-to-mean heuristics. The added median and IQR columns are therefore especially important. Welch ANOVA and Welch t-tests are often reasonably robust with group sizes in the 10-40 range, but for heavily skewed positive data they should be interpreted as approximate mean-based tests rather than the only possible analysis."),
      tags$p(tags$strong("Applied subset rules:")),
      tags$ul(lapply(filter_lines, tags$li)),
      tags$p(tags$strong("How the statistical test works: "), explanation),
      if (nzchar(scope_interpretation %||% "")) tags$p(tags$strong("How the facet-aware comparisons work: "), scope_interpretation),
      tags$p(tags$strong("How to interpret it: "), interpretation),
      tags$p("This table follows the same current tab-4 filters, selected outcome, normalization rule, grouping rule, and optional facet split as the grouped plot above. For covariate-adjusted inference, continue to tab 4.5.")
    )
  })

  output$generalization_group_stats_table <- renderTable({
    apply_table_value_filter("generalization_group_stats_table", generalization_group_stats_table_data())
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$ancova_controls_ui <- renderUI({
    nums <- generalized_numeric_columns()
    groups <- generalized_group_columns()
    if (!length(nums) || !length(groups)) return(tags$p("No saved generalized feature table is available yet. Finish tab 3 and save the analysis table first."))
    outcome <- input$ancova_outcome %||% nums[[1]]
    if (!outcome %in% nums) outcome <- nums[[1]]
    use_ratio <- isTRUE(input$ancova_use_ratio_outcome %||% FALSE)
    denominator_choices <- setdiff(nums, outcome)
    preferred_denominator <- intersect(c("confluence", "image_confluence", "image_confluence_mask_area"), denominator_choices)
    denominator <- input$ancova_ratio_denominator %||% if (length(preferred_denominator)) preferred_denominator[[1]] else denominator_choices[[1]] %||% ""
    if (!denominator %in% denominator_choices) denominator <- denominator_choices[[1]] %||% ""
    primary_group <- input$ancova_group %||% if ("profile_label" %in% groups) "profile_label" else groups[[1]]
    if (!primary_group %in% groups) primary_group <- if ("profile_label" %in% groups) "profile_label" else groups[[1]]
    df <- tab4_filtered_table()
    blocked_covariates <- c(".image_key", "image_name", "metadata_image_name", "mask_image_name")
    id_like <- names(df)[grepl("(^|_)(id|key|name)$|unique_well_id|well_id", names(df), ignore.case = TRUE)]
    ratio_blocked <- if (use_ratio && nzchar(denominator)) denominator else character()
    covariate_choices <- setdiff(unique(c(groups, nums)), c(outcome, primary_group, ratio_blocked, blocked_covariates, id_like))
    selected_covariates <- intersect(input$ancova_covariates %||% c("confluence", "image_confluence_mask_area", "neurites_percent", "cell_bodies_percent", "retained_roi_count"), covariate_choices)
    contrast_levels <- if (nrow(df) && primary_group %in% names(df)) ordered_unique_values(df[[primary_group]]) else character()
    contrast_levels <- contrast_levels[nzchar(contrast_levels)]
    contrast_a <- input$ancova_contrast_a %||% contrast_levels[[1]] %||% ""
    contrast_b <- input$ancova_contrast_b %||% contrast_levels[[min(2, length(contrast_levels))]] %||% ""
    reference_level <- input$ancova_reference_level %||% contrast_levels[[1]] %||% ""
    tagList(
      tags$div(
        class = "metric-info-box",
        tags$p(tags$strong("Simple rule: "), "do not use the same variable as both the primary contrast and a covariate. The primary contrast is what you are testing; covariates are nuisance or adjustment variables you want to account for."),
        tags$p("This panel hides the selected outcome and primary contrast from the covariate list. If an old/stale selection still contains them, the model removes them before fitting."),
        tags$p("Also be careful with covariates that are mathematical components of the outcome. For example, if the outcome is total skeleton length, adjusting for retained ROI count or mean skeleton length changes the question from total neurite burden to length conditional on object count/size."),
        tags$p("Ratio outcomes answer a different question from covariate adjustment. Outcome/confluence asks whether groups differ in outcome per unit confluence; outcome ~ group + confluence asks whether groups differ after statistical adjustment for confluence.")
      ),
      selectInput("ancova_outcome", "Outcome variable to explain", choices = nums, selected = outcome),
      checkboxInput("ancova_use_ratio_outcome", "Use ratio-normalized outcome", value = use_ratio),
      if (isTRUE(use_ratio)) tagList(
        selectInput("ancova_ratio_denominator", "Denominator for ratio outcome", choices = denominator_choices, selected = denominator),
        tags$p(class = "metric-info-box", tags$strong("Model outcome will be: "), paste(outcome, "/", denominator), ". Rows with missing, non-finite, or zero denominator are excluded from this ANCOVA model. The denominator is removed from covariate choices by default to avoid adjusting for the same quantity twice.")
      ),
      selectInput("ancova_group", "Primary contrast / main factor to test", choices = groups, selected = primary_group),
      checkboxGroupInput(
        "ancova_covariates",
        "Adjustment covariates to keep in the model",
        choices = covariate_choices,
        selected = selected_covariates
      ),
      radioButtons(
        "ancova_contrast_mode",
        "Primary-factor contrast output",
        choices = c(
          "Omnibus only: is any level different?" = "omnibus",
          "Selected two levels" = "selected_two",
          "Each level versus reference/control" = "all_vs_reference",
          "All pairwise level comparisons" = "all_pairwise"
        ),
        selected = input$ancova_contrast_mode %||% "omnibus"
      ),
      if (length(contrast_levels) >= 2) tagList(
        selectInput("ancova_reference_level", "Reference/control level", choices = contrast_levels, selected = reference_level),
        selectInput("ancova_contrast_a", "Selected contrast level A", choices = contrast_levels, selected = contrast_a),
        selectInput("ancova_contrast_b", "Selected contrast level B", choices = contrast_levels, selected = contrast_b)
      ) else tags$p(class = "warning-text", "The selected primary factor has fewer than two observed levels after filtering."),
      actionButton("ancova_use_covariate_recommendations", "Use 4.4 Suggested Covariates", class = "btn-default")
    )
  })

  output$covariate_correlation_controls_ui <- renderUI({
    nums <- generalized_numeric_columns()
    if (length(nums) < 2) return(tags$p("At least two numeric variables are needed for covariate-correlation diagnostics."))
    checkboxGroupInput(
      "covariate_corr_vars",
      "Numeric covariates/features to check for intercorrelation",
      choices = nums,
      selected = intersect(c("image_confluence", "retained_roi_count", "retained_roi_fraction", "image_total_skeleton_length", "image_mean_skeleton_length", "image_median_skeleton_length"), nums)
    )
  })

  covariate_correlation_result <- reactive({
    df <- tab4_filtered_table()
    vars <- intersect(input$covariate_corr_vars %||% character(), names(df))
    vars <- vars[vapply(df[vars], is.numeric, logical(1))]
    if (!nrow(df) || length(vars) < 2) {
      return(list(ok = FALSE, message = "Select at least two numeric covariates/features.", matrix = NULL, pairs = data.frame(), recommendations = data.frame()))
    }
    x <- df[, vars, drop = FALSE]
    for (nm in names(x)) x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
    non_constant <- vapply(x, function(v) stats::sd(v, na.rm = TRUE) > 0, logical(1))
    x <- x[, non_constant, drop = FALSE]
    if (ncol(x) < 2) {
      return(list(ok = FALSE, message = "Fewer than two non-constant numeric covariates remain after filtering.", matrix = NULL, pairs = data.frame(), recommendations = data.frame()))
    }
    mat <- suppressWarnings(stats::cor(x, use = "pairwise.complete.obs"))
    idx <- which(upper.tri(mat), arr.ind = TRUE)
    pairs <- data.frame(
      Variable_1 = rownames(mat)[idx[, 1]],
      Variable_2 = colnames(mat)[idx[, 2]],
      Correlation = mat[idx],
      Abs_correlation = abs(mat[idx]),
      stringsAsFactors = FALSE
    )
    pairs <- pairs[order(pairs$Abs_correlation, decreasing = TRUE), , drop = FALSE]
    high <- pairs[is.finite(pairs$Abs_correlation) & pairs$Abs_correlation >= 0.8, , drop = FALSE]
    correlated_vars <- unique(c(high$Variable_1, high$Variable_2))
    independent_vars <- setdiff(colnames(x), correlated_vars)
    recs <- if (nrow(high)) {
      pair_recs <- do.call(rbind, lapply(seq_len(nrow(high)), function(i) {
        a <- high$Variable_1[[i]]
        b <- high$Variable_2[[i]]
        va <- stats::var(x[[a]], na.rm = TRUE)
        vb <- stats::var(x[[b]], na.rm = TRUE)
        keep <- if (is.finite(va) && is.finite(vb) && va >= vb) a else b
        drop <- if (identical(keep, a)) b else a
        data.frame(
          Highly_correlated_pair = paste(a, "vs", b),
          Abs_correlation = round(high$Abs_correlation[[i]], 3),
          Suggested_retain = keep,
          Consider_excluding_or_not_using_together = drop,
          Rationale = "Pair is strongly correlated; retaining the higher-variance variable is a simple default, but biological interpretability should override this when appropriate.",
          stringsAsFactors = FALSE
        )
      }))
      if (length(independent_vars)) {
        independent_recs <- data.frame(
          Highly_correlated_pair = paste0(independent_vars, " is unique among selected numeric covariates"),
          Abs_correlation = NA_real_,
          Suggested_retain = independent_vars,
          Consider_excluding_or_not_using_together = "",
          Rationale = "No selected numeric covariate has |correlation| >= 0.80 with this variable, so it is retained as an independent adjustment candidate.",
          stringsAsFactors = FALSE
        )
        rbind(pair_recs, independent_recs)
      } else {
        pair_recs
      }
    } else {
      data.frame(
        Highly_correlated_pair = paste0(colnames(x), " is unique among selected numeric covariates"),
        Abs_correlation = NA_real_,
        Suggested_retain = colnames(x),
        Consider_excluding_or_not_using_together = "",
        Rationale = "No selected numeric covariate pairs have |correlation| >= 0.80, so all selected numeric covariates are retained as independent adjustment candidates.",
        stringsAsFactors = FALSE
      )
    }
    list(ok = TRUE, message = "", matrix = mat, pairs = pairs, recommendations = recs)
  })

  recommended_covariates_from_correlation <- reactive({
    res <- covariate_correlation_result()
    selected <- intersect(input$covariate_corr_vars %||% character(), names(tab4_filtered_table()))
    if (!length(selected)) return(character())
    if (!isTRUE(res$ok) || !nrow(res$recommendations) || !"Suggested_retain" %in% names(res$recommendations)) {
      return(selected)
    }
    keep <- unique(as.character(res$recommendations$Suggested_retain))
    keep <- keep[nzchar(keep)]
    drop <- unique(as.character(res$recommendations$Consider_excluding_or_not_using_together))
    drop <- drop[nzchar(drop)]
    unique(c(setdiff(selected, drop), keep))
  })

  output$covariate_correlation_heatmap <- renderPlot({
    on.exit(remember_plot("covariate_correlation_heatmap"), add = TRUE)
    res <- covariate_correlation_result()
    if (!isTRUE(res$ok)) {
      plot.new(); text(0.5, 0.5, res$message); return()
    }
    mat <- res$matrix
    display_colnames <- plot_display_labels(colnames(mat), width = 18)
    display_rownames <- plot_display_labels(rownames(mat), width = 22)
    label_max <- max(nchar(c(display_rownames, display_colnames)), na.rm = TRUE)
    label_cex <- max(0.42, min(0.72, 9 / max(label_max, 12)))
    bottom_margin <- max(10, min(18, 4 + 0.22 * label_max))
    left_margin <- max(10, min(22, 4 + 0.24 * label_max))
    op <- par(mar = c(bottom_margin, left_margin, 4, 6), xpd = NA)
    on.exit(par(op), add = TRUE)
    ord <- stats::hclust(stats::as.dist(1 - abs(mat)))$order
    mat <- mat[ord, ord, drop = FALSE]
    display_colnames <- display_colnames[ord]
    display_rownames <- display_rownames[ord]
    cols <- grDevices::colorRampPalette(c("#2b6cb0", "#f7f2df", "#b8322a"))(101)
    image(seq_len(nrow(mat)), seq_len(ncol(mat)), t(mat[nrow(mat):1, ]), col = cols, zlim = c(-1, 1), axes = FALSE, xlab = "", ylab = "", main = "Covariate Correlation Heatmap")
    axis(1, at = seq_len(ncol(mat)), labels = FALSE)
    axis(2, at = seq_len(nrow(mat)), labels = FALSE)
    text(seq_len(ncol(mat)), par("usr")[3] - 0.045 * diff(par("usr")[3:4]), labels = display_colnames, srt = 45, adj = 1, cex = label_cex)
    text(par("usr")[1] - 0.035 * diff(par("usr")[1:2]), seq_len(nrow(mat)), labels = rev(display_rownames), adj = 1, cex = label_cex)
    legend("topright", inset = c(-0.14, 0), legend = c("-1", "0", "1"), fill = c("#2b6cb0", "#f7f2df", "#b8322a"), title = "r", bty = "n", cex = 0.75)
    box()
  })

  output$covariate_correlation_pairs_table <- renderTable({
    res <- covariate_correlation_result()
    if (!isTRUE(res$ok)) return(data.frame(Message = res$message))
    out <- head(res$pairs, 25)
    out$Correlation <- round(out$Correlation, 3)
    out$Abs_correlation <- round(out$Abs_correlation, 3)
    apply_table_value_filter("covariate_correlation_pairs_table", out)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$covariate_recommendation_table <- renderTable({
    res <- covariate_correlation_result()
    if (!isTRUE(res$ok)) return(data.frame(Message = res$message))
    apply_table_value_filter("covariate_recommendation_table", res$recommendations)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$covariate_next_step_ui <- renderUI({
    suggested <- recommended_covariates_from_correlation()
    if (!length(suggested)) {
      return(tags$div(class = "step-summary-box step-summary-neutral", tags$strong("Next step"), tags$p("Select numeric covariates above to get a directed recommendation before running ANCOVA.")))
    }
    tags$div(
      class = "step-summary-box step-summary-good",
      tags$strong("Suggested next step for 4.5"),
      tags$p("Use the retained covariates below as a starting point in the ANCOVA panel. This is not automatic proof of correctness; it is a safer default that avoids putting strongly overlapping numeric covariates into the same model."),
      tags$p(tags$strong("Suggested covariates: "), paste(suggested, collapse = ", "))
    )
  })

  observeEvent(input$ancova_use_covariate_recommendations, {
    nums <- generalized_numeric_columns()
    groups <- generalized_group_columns()
    df <- tab4_filtered_table()
    outcome <- input$ancova_outcome %||% nums[[1]] %||% ""
    primary_group <- input$ancova_group %||% groups[[1]] %||% ""
    ratio_denominator <- if (isTRUE(input$ancova_use_ratio_outcome %||% FALSE)) input$ancova_ratio_denominator %||% "" else ""
    id_like <- names(df)[grepl("(^|_)(id|key|name)$|unique_well_id|well_id", names(df), ignore.case = TRUE)]
    covariate_choices <- setdiff(unique(c(groups, nums)), c(outcome, primary_group, ratio_denominator, ".image_key", "image_name", "metadata_image_name", "mask_image_name", id_like))
    existing_valid <- intersect(input$ancova_covariates %||% character(), covariate_choices)
    existing_not_checked_in_4_4 <- setdiff(existing_valid, input$covariate_corr_vars %||% character())
    suggested <- intersect(unique(c(recommended_covariates_from_correlation(), existing_not_checked_in_4_4)), covariate_choices)
    updateCheckboxGroupInput(session, "ancova_covariates", choices = covariate_choices, selected = suggested)
    showNotification("Suggested covariates from 4.4 were applied to 4.5, including independent numeric covariates and preserving valid manual/categorical covariates that 4.4 cannot test by correlation.", type = "message")
  }, ignoreInit = TRUE)

  build_ancova_model <- reactive({
    df <- tab4_filtered_table()
    outcome <- input$ancova_outcome %||% ""
    group <- input$ancova_group %||% ""
    use_ratio <- isTRUE(input$ancova_use_ratio_outcome %||% FALSE)
    denominator <- input$ancova_ratio_denominator %||% ""
    covars <- input$ancova_covariates %||% character()
    if (!nzchar(outcome) || !nzchar(group) || identical(outcome, group)) {
      return(list(ok = FALSE, message = "Choose different variables for the outcome and the primary contrast. The same variable cannot be both the thing being explained and the factor used to explain it."))
    }
    if (use_ratio && (!nzchar(denominator) || identical(denominator, outcome) || !denominator %in% names(df))) {
      return(list(ok = FALSE, message = "Choose a valid denominator that is different from the outcome when using a ratio-normalized ANCOVA outcome."))
    }
    excluded_from_covars <- intersect(covars, c(outcome, group, if (use_ratio) denominator else character()))
    id_like <- names(df)[grepl("(^|_)(id|key|name)$|unique_well_id|well_id", names(df), ignore.case = TRUE)]
    covars <- setdiff(covars, c(outcome, group, if (use_ratio) denominator else character(), ".image_key", "image_name", "metadata_image_name", "mask_image_name", id_like))
    terms <- unique(c(group, covars))
    terms <- terms[nzchar(terms) & terms %in% names(df)]
    if (!nrow(df) || !outcome %in% names(df) || !length(terms)) return(list(ok = FALSE, message = "Select outcome and model terms."))
    model_df <- df[, unique(c(outcome, if (use_ratio) denominator else character(), terms)), drop = FALSE]
    model_outcome_name <- outcome
    denominator_invalid_n <- 0L
    if (use_ratio) {
      model_df[[denominator]] <- suppressWarnings(as.numeric(df[[denominator]]))
      numerator <- suppressWarnings(as.numeric(df[[outcome]]))
      denominator_values <- model_df[[denominator]]
      invalid_denominator <- !is.finite(denominator_values) | denominator_values == 0
      denominator_invalid_n <- sum(invalid_denominator, na.rm = TRUE)
      ratio_values <- numerator / denominator_values
      ratio_values[invalid_denominator] <- NA_real_
      model_outcome_name <- paste0(outcome, "_per_", denominator)
      model_df[[model_outcome_name]] <- ratio_values
      model_df[[outcome]] <- NULL
      model_df[[denominator]] <- NULL
    } else {
      model_df[[outcome]] <- suppressWarnings(as.numeric(model_df[[outcome]]))
    }
    dropped_terms <- character()
    imputed_terms <- character()
    for (nm in terms) {
      if (identical(nm, group)) {
        model_df[[nm]] <- ordered_factor_for_plot(model_df[[nm]])
      } else if (vector_is_numeric_like(model_df[[nm]])) {
        v <- suppressWarnings(as.numeric(model_df[[nm]]))
        finite_n <- sum(is.finite(v))
        if (finite_n < 3 || stats::sd(v, na.rm = TRUE) <= 0) {
          dropped_terms <- c(dropped_terms, nm)
          next
        }
        if (any(!is.finite(v))) {
          fill <- stats::median(v[is.finite(v)], na.rm = TRUE)
          v[!is.finite(v)] <- fill
          imputed_terms <- c(imputed_terms, nm)
        }
        model_df[[nm]] <- v
      } else {
        x <- as.character(model_df[[nm]])
        x[!nzchar(x) | is.na(x)] <- "(missing)"
        model_df[[nm]] <- ordered_factor_for_plot(x)
        if (length(unique(model_df[[nm]])) < 2) {
          dropped_terms <- c(dropped_terms, nm)
        }
      }
    }
    if (length(dropped_terms)) {
      terms <- setdiff(terms, dropped_terms)
      model_df <- model_df[, unique(c(outcome, terms)), drop = FALSE]
    }
    model_df <- model_df[is.finite(model_df[[model_outcome_name]]), , drop = FALSE]
    if (nrow(model_df) < max(5, length(terms) + 2)) return(list(ok = FALSE, message = sprintf("Not enough usable rows for ANCOVA after dropping sparse/constant covariates. Rows left: %s. Dropped terms: %s", nrow(model_df), if (length(dropped_terms)) paste(dropped_terms, collapse = ", ") else "none")))
    if (length(unique(model_df[[group]])) < 2) return(list(ok = FALSE, message = "The selected primary factor has fewer than two levels after filtering and complete-case cleanup."))
    form <- stats::as.formula(paste(sprintf("`%s`", model_outcome_name), "~", paste(sprintf("`%s`", terms), collapse = " + ")))
    fit <- tryCatch(stats::lm(form, data = model_df), error = function(e) NULL)
    if (is.null(fit)) return(list(ok = FALSE, message = "ANCOVA model could not be fit."))
    list(ok = TRUE, fit = fit, model_df = model_df, outcome = model_outcome_name, raw_outcome = outcome, use_ratio = use_ratio, ratio_denominator = if (use_ratio) denominator else "", denominator_invalid_n = denominator_invalid_n, group = group, covars = setdiff(covars, dropped_terms), terms = terms, excluded_from_covars = excluded_from_covars, dropped_terms = dropped_terms, imputed_terms = imputed_terms)
  })

  ancova_result_table <- reactive({
    model <- build_ancova_model()
    if (!isTRUE(model$ok)) return(data.frame(Message = model$message))
    fit <- model$fit
    a <- tryCatch(as.data.frame(stats::drop1(fit, test = "F")), error = function(e) data.frame())
    if (!nrow(a)) {
      return(data.frame(Message = "Adjusted term tests could not be calculated for this model. Try fewer or less-overlapping covariates."))
    }
    a$Term <- row.names(a)
    row.names(a) <- NULL
    a <- a[!a$Term %in% "<none>", , drop = FALSE]
    df_resid <- stats::df.residual(fit)
    f_candidates <- intersect(c("F value", "F"), names(a))
    p_candidates <- intersect(c("Pr(>F)", "Pr(>Chi)"), names(a))
    f_col <- if (length(f_candidates)) f_candidates[[1]] else ""
    p_col <- if (length(p_candidates)) p_candidates[[1]] else ""
    if (nzchar(f_col) && "Df" %in% names(a)) {
      f_vals <- suppressWarnings(as.numeric(a[[f_col]]))
      df_vals <- suppressWarnings(as.numeric(a$Df))
      a$Partial_eta_squared <- ifelse(
        is.finite(f_vals) & is.finite(df_vals) & is.finite(df_resid) & df_resid > 0,
        (f_vals * df_vals) / (f_vals * df_vals + df_resid),
        NA_real_
      )
    } else {
      a$Partial_eta_squared <- NA_real_
    }
    a$Normalized_impact_size <- sqrt(pmax(0, a$Partial_eta_squared))
    a$Test_type <- "partial drop-one F test"
    keep <- c("Term", "Test_type", "Df", "Sum of Sq", "RSS", "AIC", f_col, p_col, "Partial_eta_squared", "Normalized_impact_size")
    keep <- unique(keep[nzchar(keep)])
    out <- a[, intersect(keep, names(a)), drop = FALSE]
    if (length(model$excluded_from_covars)) {
      attr(out, "excluded_from_covars") <- model$excluded_from_covars
    }
    out
  })

  most_common_value <- function(x) {
    tab <- sort(table(x, useNA = "no"), decreasing = TRUE)
    if (length(tab)) names(tab)[[1]] else NA_character_
  }

  adjusted_primary_level_predictions <- function(model) {
    fit <- model$fit
    model_df <- model$model_df
    group <- model$group
    outcome <- model$outcome
    levels_group <- levels(model_df[[group]])
    if (length(levels_group) < 2) return(list(ok = FALSE, message = "Primary factor has fewer than two levels."))
    newdata <- model_df[rep(1, length(levels_group)), , drop = FALSE]
    for (nm in setdiff(names(model_df), outcome)) {
      if (identical(nm, group)) {
        newdata[[nm]] <- factor(levels_group, levels = levels(model_df[[nm]]), ordered = is.ordered(model_df[[nm]]))
      } else if (is.numeric(model_df[[nm]])) {
        newdata[[nm]] <- mean(model_df[[nm]], na.rm = TRUE)
      } else {
        mode_val <- most_common_value(model_df[[nm]])
        newdata[[nm]] <- factor(rep(mode_val, length(levels_group)), levels = levels(model_df[[nm]]), ordered = is.ordered(model_df[[nm]]))
      }
    }
    mm <- tryCatch(stats::model.matrix(stats::delete.response(stats::terms(fit)), newdata), error = function(e) NULL)
    if (is.null(mm)) return(list(ok = FALSE, message = "Could not build adjusted prediction matrix for primary-factor contrasts."))
    coefs <- stats::coef(fit)
    keep_coef <- is.finite(coefs) & names(coefs) %in% colnames(mm)
    if (!any(keep_coef)) {
      return(list(ok = FALSE, message = "No estimable model coefficients were available for adjusted primary-factor contrasts. Try removing strongly overlapping covariates."))
    }
    mm <- mm[, names(coefs)[keep_coef], drop = FALSE]
    coefs <- coefs[keep_coef]
    vc <- stats::vcov(fit)
    vc <- vc[names(coefs), names(coefs), drop = FALSE]
    pred <- as.numeric(mm %*% coefs)
    list(ok = TRUE, levels = levels_group, newdata = newdata, mm = mm, vcov = vc, pred = pred)
  }

  primary_contrast_rows <- function(model, mode = "omnibus", reference_level = NULL, contrast_a = NULL, contrast_b = NULL) {
    if (!isTRUE(model$ok)) return(data.frame(Message = model$message))
    if (identical(mode, "omnibus")) {
      return(data.frame(Message = "Omnibus-only mode selected. Read the primary factor row in the ANCOVA contribution table above: it tests whether any level differs after adjustment."))
    }
    pred <- adjusted_primary_level_predictions(model)
    if (!isTRUE(pred$ok)) return(data.frame(Message = pred$message))
    lv <- pred$levels
    make_pair <- function(a, b) {
      ia <- match(a, lv)
      ib <- match(b, lv)
      if (!is.finite(ia) || !is.finite(ib) || ia == ib) return(NULL)
      L <- pred$mm[ia, , drop = FALSE] - pred$mm[ib, , drop = FALSE]
      est <- as.numeric(pred$pred[[ia]] - pred$pred[[ib]])
      se <- sqrt(as.numeric(L %*% pred$vcov %*% t(L)))
      tval <- est / se
      p <- 2 * stats::pt(-abs(tval), df = stats::df.residual(model$fit))
      data.frame(
        Contrast = paste(a, "minus", b),
        Level_A = a,
        Level_B = b,
        Adjusted_mean_A = pred$pred[[ia]],
        Adjusted_mean_B = pred$pred[[ib]],
        Difference_A_minus_B = est,
        SE = se,
        T_value = tval,
        P_value = p,
        stringsAsFactors = FALSE
      )
    }
    pairs <- list()
    if (identical(mode, "selected_two")) {
      pairs <- list(make_pair(contrast_a %||% lv[[1]], contrast_b %||% lv[[min(2, length(lv))]]))
    } else if (identical(mode, "all_vs_reference")) {
      ref <- reference_level %||% lv[[1]]
      if (!ref %in% lv) ref <- lv[[1]]
      pairs <- lapply(setdiff(lv, ref), function(x) make_pair(x, ref))
    } else if (identical(mode, "all_pairwise")) {
      combos <- utils::combn(lv, 2, simplify = FALSE)
      pairs <- lapply(combos, function(x) make_pair(x[[1]], x[[2]]))
    }
    pairs <- Filter(Negate(is.null), pairs)
    if (!length(pairs)) return(data.frame(Message = "No valid primary-factor pairwise contrasts could be built."))
    out <- do.call(rbind, pairs)
    out$P_adjusted_Holm <- stats::p.adjust(out$P_value, method = "holm")
    out
  }

  ancova_primary_contrast_table <- reactive({
    model <- build_ancova_model()
    mode <- input$ancova_contrast_mode %||% "omnibus"
    out <- primary_contrast_rows(
      model,
      mode = mode,
      reference_level = input$ancova_reference_level %||% NULL,
      contrast_a = input$ancova_contrast_a %||% NULL,
      contrast_b = input$ancova_contrast_b %||% NULL
    )
    if ("Message" %in% names(out)) return(out)
    num_cols <- intersect(c("Adjusted_mean_A", "Adjusted_mean_B", "Difference_A_minus_B", "SE", "T_value", "P_value", "P_adjusted_Holm"), names(out))
    out[num_cols] <- lapply(out[num_cols], function(x) round(x, 5))
    out
  })

  output$ancova_model_guidance_ui <- renderUI({
    df <- tab4_filtered_table()
    outcome <- input$ancova_outcome %||% ""
    group <- input$ancova_group %||% ""
    use_ratio <- isTRUE(input$ancova_use_ratio_outcome %||% FALSE)
    denominator <- input$ancova_ratio_denominator %||% ""
    covars_raw <- input$ancova_covariates %||% character()
    illegal <- intersect(covars_raw, c(outcome, group, if (use_ratio) denominator else character()))
    covars <- setdiff(covars_raw, c(outcome, group, if (use_ratio) denominator else character(), ".image_key", "image_name", "metadata_image_name", "mask_image_name"))
    issues <- character()
    if (!nzchar(outcome) || !outcome %in% names(df)) issues <- c(issues, "Select a valid outcome variable.")
    if (!nzchar(group) || !group %in% names(df)) issues <- c(issues, "Select a valid primary contrast.")
    if (identical(outcome, group)) issues <- c(issues, "Outcome and primary contrast are the same variable. Pick a biological readout as outcome and a group/treatment/condition variable as the primary contrast.")
    if (use_ratio && (!nzchar(denominator) || identical(denominator, outcome) || !denominator %in% names(df))) {
      issues <- c(issues, "Ratio outcome is enabled, but the denominator is missing, invalid, or identical to the outcome.")
    }
    if (length(illegal)) issues <- c(issues, sprintf("Removed from covariates because already used as outcome, primary contrast, or ratio denominator: %s.", paste(illegal, collapse = ", ")))
    component_like <- character()
    if (grepl("total.*skeleton|skeleton.*total", outcome, ignore.case = TRUE)) {
      component_like <- intersect(covars, grep("retained_roi_count|mean_skeleton|median_skeleton|roi_.*skeleton", covars, value = TRUE, ignore.case = TRUE))
    }
    if (length(component_like)) {
      issues <- c(issues, sprintf("Potential over-adjustment: %s may be mathematical components or close summaries of the selected outcome. Keep them only if your question is explicitly conditional on those components.", paste(component_like, collapse = ", ")))
    }
    complete_rows <- 0L
    if (nrow(df) && outcome %in% names(df) && group %in% names(df)) {
      model_cols <- unique(c(outcome, group, intersect(covars, names(df))))
      model_df <- df[, model_cols, drop = FALSE]
      if (use_ratio && nzchar(denominator) && denominator %in% names(df) && !identical(denominator, outcome)) {
        numerator <- suppressWarnings(as.numeric(df[[outcome]]))
        denom <- suppressWarnings(as.numeric(df[[denominator]]))
        model_df$.ratio_outcome <- numerator / denom
        model_df$.ratio_outcome[!is.finite(denom) | denom == 0] <- NA_real_
        model_df[[outcome]] <- NULL
        complete_rows <- sum(stats::complete.cases(model_df) & is.finite(model_df$.ratio_outcome))
      } else {
        model_df[[outcome]] <- suppressWarnings(as.numeric(model_df[[outcome]]))
        complete_rows <- sum(stats::complete.cases(model_df) & is.finite(model_df[[outcome]]))
      }
      if (complete_rows < max(5, length(model_cols) + 1)) issues <- c(issues, sprintf("Only %s complete row(s) remain after this model selection; estimates may be unstable.", complete_rows))
    }
    model <- build_ancova_model()
    if (isTRUE(model$ok)) {
      complete_rows <- nrow(model$model_df)
      issues <- issues[!grepl("complete row", issues, ignore.case = TRUE)]
    }
    cls <- if (length(issues)) "step-summary-box step-summary-warn" else "step-summary-box step-summary-good"
    tags$div(
      class = cls,
      tags$strong("Model-readiness guide"),
      tags$p(tags$strong("Interpretation template: "), "Outcome ~ primary contrast + covariates. The primary contrast asks what differs; covariates ask what to adjust for."),
      tags$p(tags$strong("Current model: "), paste(c(if (use_ratio && nzchar(denominator)) paste0(outcome, " / ", denominator) else outcome, "~", group, if (length(covars)) paste("+", paste(covars, collapse = " + ")) else ""), collapse = " ")),
      if (use_ratio && nzchar(denominator)) tags$p(tags$strong("Ratio-outcome interpretation: "), sprintf("This asks whether %s per unit %s differs by %s after the remaining selected covariates.", outcome, denominator, group)),
      tags$p(tags$strong("Complete rows available: "), complete_rows),
      if (isTRUE(model$ok)) tagList(
        if (length(model$dropped_terms)) tags$p(tags$strong("Dropped unusable covariates: "), paste(model$dropped_terms, collapse = ", ")),
        if (length(model$imputed_terms)) tags$p(tags$strong("Median-imputed numeric covariates: "), paste(model$imputed_terms, collapse = ", ")),
        if (isTRUE(model$use_ratio) && is.finite(model$denominator_invalid_n) && model$denominator_invalid_n > 0) tags$p(tags$strong("Rows excluded for invalid ratio denominator before model fitting: "), model$denominator_invalid_n)
      ),
      if (length(issues)) tags$ul(lapply(issues, tags$li)) else tags$p("Specification looks internally consistent. Next interpret the primary contrast row first, then covariate rows as adjustment/context rather than the main biological claim.")
    )
  })

  output$ancova_result_table <- renderTable({
    apply_table_value_filter("ancova_result_table", ancova_result_table())
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$ancova_primary_contrast_table <- renderTable({
    apply_table_value_filter("ancova_primary_contrast_table", ancova_primary_contrast_table())
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$visualization_generalized_table_status <- renderUI({
    df <- saved_generalized_analysis_table()
    out <- generalized_analysis_output_paths(settings())
    cls <- if (nrow(df)) "step-summary-box step-summary-ready" else "step-summary-box step-summary-missing"
    tags$div(
      class = cls,
      tags$strong("Tab 4 input: saved generalized model-feature table"),
      tags$p(sprintf("Rows available: %s", nrow(df))),
      tags$p(sprintf("Columns available: %s", if (nrow(df)) ncol(df) else 0L)),
      tags$p("Important statistical unit note: tab 4 rows are image-by-cutoff-profile rows. If you apply two cutoff profiles to the same image, that image appears twice. Use this intentionally for model comparison, or filter to one profile before biological inference."),
      tags$p(tags$strong("Expected CSV: "), tags$span(class = "path-block", out$csv)),
      tags$p(tags$strong("Expected RDS: "), tags$span(class = "path-block", out$rds)),
      if (!nrow(df)) tags$p("Go to tab 3.2, select model(s) and features, then click 'Save Analysis Table for Tab 4'.")
    )
  })

  output$dimred_controls_ui <- renderUI({
    df <- tab4_source_table()
    nums <- generalized_numeric_columns()
    groups <- setdiff(names(df), ".image_key")
    if (!nrow(df) || length(nums) < 2) return(tags$p("Save the tab-3 generalized feature table with at least two numeric variables first."))
    tagList(
      checkboxGroupInput("dimred_features", "Numeric features for PCA/separation", choices = nums, selected = head(nums, min(8, length(nums)))),
      selectInput("dimred_color_by", "Color points by metadata/model feature", choices = groups, selected = if ("profile_label" %in% groups) "profile_label" else groups[[1]]),
      fluidRow(
        column(6, selectInput("dimred_pc_x", "X-axis PC", choices = paste0("PC", 1:4), selected = "PC1")),
        column(6, selectInput("dimred_pc_y", "Y-axis PC", choices = paste0("PC", 1:4), selected = "PC2"))
      ),
      selectInput("dimred_separation_by", "Assess PCA separation by variable", choices = groups, selected = if ("profile_label" %in% groups) "profile_label" else groups[[1]]),
      checkboxGroupInput("dimred_loading_pcs", "Loading dimensions to show", choices = paste0("PC", 1:4), selected = paste0("PC", 1:4))
    )
  })

  dimred_result <- reactive({
    df <- tab4_filtered_table()
    features <- input$dimred_features %||% character()
    features <- intersect(features, names(df))
    features <- features[vapply(df[features], vector_is_numeric_like, logical(1))]
    if (!nrow(df) || length(features) < 2) return(list(ok = FALSE, message = "Select at least two numeric features."))
    x <- df[, features, drop = FALSE]
    for (nm in names(x)) x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
    finite_counts <- vapply(x, function(v) sum(is.finite(v)), numeric(1))
    enough_values <- finite_counts >= 3
    x <- x[, enough_values, drop = FALSE]
    if (ncol(x) < 2) {
      dropped <- paste(names(finite_counts)[!enough_values], collapse = ", ")
      msg <- "Fewer than two selected features have at least three finite values for PCA."
      if (nzchar(dropped)) msg <- paste(msg, "Dropped sparse feature(s):", dropped)
      return(list(ok = FALSE, message = msg))
    }
    row_has_any_value <- apply(x, 1, function(v) any(is.finite(v)))
    x <- x[row_has_any_value, , drop = FALSE]
    meta <- df[row_has_any_value, , drop = FALSE]
    if (nrow(x) < 3) return(list(ok = FALSE, message = "Not enough rows with any finite selected PCA feature."))
    for (nm in names(x)) {
      v <- x[[nm]]
      fill <- suppressWarnings(stats::median(v[is.finite(v)], na.rm = TRUE))
      if (!is.finite(fill)) fill <- 0
      v[!is.finite(v)] <- fill
      x[[nm]] <- v
    }
    non_constant <- vapply(x, function(v) stats::sd(v, na.rm = TRUE) > 0, logical(1))
    x <- x[, non_constant, drop = FALSE]
    if (ncol(x) < 2) return(list(ok = FALSE, message = "Fewer than two non-constant numeric features are available."))
    pca <- tryCatch(stats::prcomp(x, center = TRUE, scale. = TRUE), error = function(e) NULL)
    if (is.null(pca)) return(list(ok = FALSE, message = "PCA could not be calculated."))
    scores <- as.data.frame(pca$x[, 1:min(4, ncol(pca$x)), drop = FALSE])
    scores$.row_id <- seq_len(nrow(scores))
    color_by <- input$dimred_color_by %||% ""
    if (nzchar(color_by) && color_by %in% names(meta) && vector_is_numeric_like(meta[[color_by]])) {
      scores$.color_numeric <- suppressWarnings(as.numeric(meta[[color_by]]))
      scores$.color <- factor("numeric")
    } else {
      scores$.color_numeric <- NA_real_
      scores$.color <- if (nzchar(color_by) && color_by %in% names(meta)) ordered_factor_for_plot(meta[[color_by]]) else factor("All rows")
    }
    scores$.label <- paste0(meta$image_name %||% row.names(meta), " | ", meta$profile_label %||% "")
    separation_by <- input$dimred_separation_by %||% ""
    if (nzchar(separation_by) && separation_by %in% names(meta)) {
      scores$.separation_raw <- meta[[separation_by]]
      scores$.separation_numeric <- if (vector_is_numeric_like(meta[[separation_by]])) suppressWarnings(as.numeric(meta[[separation_by]])) else NA_real_
      scores$.separation_group <- if (any(is.finite(scores$.separation_numeric))) factor("numeric") else ordered_factor_for_plot(meta[[separation_by]])
    } else {
      scores$.separation_raw <- "All rows"
      scores$.separation_numeric <- NA_real_
      scores$.separation_group <- factor("All rows")
    }
    var_exp <- (pca$sdev^2) / sum(pca$sdev^2)
    dropped_features <- setdiff(features, colnames(x))
    list(ok = TRUE, scores = scores, pca = pca, features = colnames(x), dropped_features = dropped_features, imputed_missing = sum(!stats::complete.cases(df[row_has_any_value, features, drop = FALSE])), var_exp = var_exp, color_by = color_by, separation_by = separation_by)
  })

  output$dimred_pca_plot <- renderPlot({
    on.exit(remember_plot("dimred_pca_plot"), add = TRUE)
    res <- dimred_result()
    if (!isTRUE(res$ok)) {
      plot.new(); text(0.5, 0.5, res$message); return()
    }
    scores <- res$scores
    axes <- choose_pca_axes(scores, input$dimred_pc_x %||% "PC1", input$dimred_pc_y %||% "PC2")
    pc_x <- axes[[1]]
    pc_y <- axes[[2]]
    pc_x_i <- suppressWarnings(as.integer(sub("^PC", "", pc_x)))
    pc_y_i <- suppressWarnings(as.integer(sub("^PC", "", pc_y)))
    numeric_color <- any(is.finite(scores$.color_numeric))
    if (numeric_color) {
      point_cols <- continuous_palette(scores$.color_numeric)
    } else {
      cols <- as.integer(scores$.color)
      palette_cols <- grDevices::hcl.colors(length(levels(scores$.color)), "Dark 3")
      point_cols <- palette_cols[cols]
    }
    op <- par(mar = c(4.8, 4.8, 2.8, if (numeric_color) 5.8 else 1))
    on.exit(par(op), add = TRUE)
    color_title <- plot_display_label(res$color_by %||% "selected feature", width = 50, multiline = FALSE)
    plot(scores[[pc_x]], scores[[pc_y]], pch = 19, col = point_cols, xlab = sprintf("%s (%.1f%%)", pc_x, 100 * res$var_exp[[pc_x_i]]), ylab = sprintf("%s (%.1f%%)", pc_y, 100 * res$var_exp[[pc_y_i]]), main = paste("PCA colored by", color_title))
    if (numeric_color) {
      draw_continuous_palette_legend(color_title, scores$.color_numeric)
    } else {
      legend("topright", legend = levels(scores$.color), col = palette_cols, pch = 19, bty = "n", cex = 0.75)
    }
  })

  pca_separation_metrics <- reactive({
    res <- dimred_result()
    if (!isTRUE(res$ok)) return(data.frame(Metric = "Status", Value = res$message))
    scores <- res$scores
    if (!nrow(scores)) return(data.frame(Metric = "Status", Value = "No PCA scores available."))
    axes <- choose_pca_axes(scores, input$dimred_pc_x %||% "PC1", input$dimred_pc_y %||% "PC2")
    pc_x <- axes[[1]]
    pc_y <- axes[[2]]
    pcs <- scores[, intersect(c(pc_x, pc_y), names(scores)), drop = FALSE]
    if (ncol(pcs) < 2) return(data.frame(Metric = "Status", Value = "PC1/PC2 scores are not available."))
    prep_rows <- data.frame(
      Metric = c("PCA features used", "PCA features dropped", "Rows entering PCA", "Missing rows median-imputed before PCA"),
      Value = c(
        paste(res$features, collapse = ", "),
        if (length(res$dropped_features)) paste(res$dropped_features, collapse = ", ") else "none",
        nrow(scores),
        res$imputed_missing %||% 0
      ),
      stringsAsFactors = FALSE
    )
    if (any(is.finite(scores$.separation_numeric))) {
      y <- scores$.separation_numeric
      keep <- is.finite(y) & stats::complete.cases(pcs)
      if (sum(keep) < 3) return(data.frame(Metric = "Status", Value = "Not enough finite numeric values for separation assessment."))
      model_df <- pcs[keep, , drop = FALSE]
      names(model_df) <- c("PC_x", "PC_y")
      fit <- tryCatch(stats::lm(y[keep] ~ PC_x + PC_y, data = model_df), error = function(e) NULL)
      r2 <- if (!is.null(fit)) summary(fit)$r.squared else NA_real_
      c1 <- suppressWarnings(stats::cor(y[keep], pcs[[1]][keep], use = "complete.obs"))
      c2 <- suppressWarnings(stats::cor(y[keep], pcs[[2]][keep], use = "complete.obs"))
      return(rbind(prep_rows, data.frame(
        Metric = c("Selected variable", "Variable type", "Rows assessed", "Selected PCA plane", "PCA-plane R-squared", paste("Correlation with", pc_x), paste("Correlation with", pc_y)),
        Value = c(res$separation_by, "numeric", sum(keep), paste(pc_x, "vs", pc_y), round(r2, 4), round(c1, 4), round(c2, 4)),
        stringsAsFactors = FALSE
      )))
    }
    grp <- scores$.separation_group
    keep <- !is.na(grp) & stats::complete.cases(pcs)
    pcs <- pcs[keep, , drop = FALSE]
    grp <- droplevels(grp[keep])
    if (nrow(pcs) < 3 || length(levels(grp)) < 2) {
      return(data.frame(Metric = "Status", Value = "Need at least two groups with complete PCA scores."))
    }
    grand <- colMeans(pcs)
    total_ss <- sum(rowSums((sweep(pcs, 2, grand, "-"))^2))
    groups <- levels(grp)
    between_ss <- 0
    within_ss <- 0
    for (g in groups) {
      rows <- grp == g
      center <- colMeans(pcs[rows, , drop = FALSE])
      between_ss <- between_ss + sum(rows) * sum((center - grand)^2)
      within_ss <- within_ss + sum(rowSums((sweep(pcs[rows, , drop = FALSE], 2, center, "-"))^2))
    }
    eta2 <- if (is.finite(total_ss) && total_ss > 0) between_ss / total_ss else NA_real_
    nearest_ok <- NA_real_
    centers <- do.call(rbind, lapply(groups, function(g) colMeans(pcs[grp == g, , drop = FALSE])))
    rownames(centers) <- groups
    if (nrow(centers) >= 2) {
      assigned <- apply(pcs, 1, function(row) {
        d <- rowSums((sweep(centers, 2, row, "-"))^2)
        names(which.min(d))
      })
      nearest_ok <- mean(assigned == as.character(grp))
    }
    rbind(prep_rows, data.frame(
      Metric = c("Selected variable", "Variable type", "Rows assessed", "Selected PCA plane", "Groups", "PCA separation eta-squared", "Nearest-centroid agreement"),
      Value = c(res$separation_by, "categorical/grouped", nrow(pcs), paste(pc_x, "vs", pc_y), length(groups), round(eta2, 4), round(nearest_ok, 4)),
      stringsAsFactors = FALSE
    ))
  })

  output$dimred_separation_plot <- renderPlot({
    on.exit(remember_plot("dimred_separation_plot"), add = TRUE)
    res <- dimred_result()
    if (!isTRUE(res$ok)) {
      plot.new(); text(0.5, 0.5, res$message); return()
    }
    scores <- res$scores
    axes <- choose_pca_axes(scores, input$dimred_pc_x %||% "PC1", input$dimred_pc_y %||% "PC2")
    pc_x <- axes[[1]]
    pc_y <- axes[[2]]
    numeric_separation <- any(is.finite(scores$.separation_numeric))
    op <- par(mar = c(4.8, 4.8, 2.8, if (numeric_separation) 5.8 else 1))
    on.exit(par(op), add = TRUE)
    if (numeric_separation) {
      point_cols <- continuous_palette(scores$.separation_numeric)
      separation_title <- plot_display_label(res$separation_by %||% "Separation variable", width = 50, multiline = FALSE)
      plot(scores[[pc_x]], scores[[pc_y]], pch = 19, col = point_cols, xlab = pc_x, ylab = pc_y, main = paste("PCA separation by", separation_title))
      draw_continuous_palette_legend(separation_title, scores$.separation_numeric)
    } else {
      groups <- droplevels(scores$.separation_group)
      cols <- as.integer(groups)
      palette_cols <- grDevices::hcl.colors(length(levels(groups)), "Dark 3")
      separation_title <- plot_display_label(res$separation_by %||% "Separation variable", width = 50, multiline = FALSE)
      plot(scores[[pc_x]], scores[[pc_y]], pch = 19, col = palette_cols[cols], xlab = pc_x, ylab = pc_y, main = paste("PCA separation by", separation_title))
      centers <- aggregate(scores[, c(pc_x, pc_y), drop = FALSE], list(group = groups), mean, na.rm = TRUE)
      points(centers[[pc_x]], centers[[pc_y]], pch = 23, bg = "#fbf1d7", col = "#173f35", cex = 1.7)
      text(centers[[pc_x]], centers[[pc_y]], labels = centers$group, pos = 3, cex = 0.75)
      legend("topright", legend = levels(groups), col = palette_cols, pch = 19, bty = "n", cex = 0.75)
    }
  })

  output$dimred_separation_metric_table <- renderTable({
    apply_table_value_filter("dimred_separation_metric_table", pca_separation_metrics())
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$dimred_scree_plot <- renderPlot({
    on.exit(remember_plot("dimred_scree_plot"), add = TRUE)
    res <- dimred_result()
    if (!isTRUE(res$ok)) {
      plot.new(); text(0.5, 0.5, res$message); return()
    }
    var_pct <- 100 * res$var_exp
    pcs <- paste0("PC", seq_along(var_pct))
    op <- par(mar = c(4.5, 4.8, 2.5, 1))
    on.exit(par(op), add = TRUE)
    centers <- barplot(var_pct, names.arg = pcs, col = "#8fb8de", border = "white", ylab = "Variance explained (%)", main = "PCA Scree Plot")
    lines(centers, cumsum(var_pct), type = "b", pch = 19, col = "#173f35", lwd = 2)
    legend("topright", legend = c("Individual PC", "Cumulative"), fill = c("#8fb8de", NA), border = c("white", NA), lty = c(NA, 1), pch = c(NA, 19), col = c("#8fb8de", "#173f35"), bty = "n", cex = 0.8)
  })

  output$dimred_feature_loading_table <- renderTable({
    res <- dimred_result()
    if (!isTRUE(res$ok)) return(data.frame(Message = res$message))
    pcs <- input$dimred_loading_pcs %||% paste0("PC", 1:4)
    pcs <- intersect(pcs, colnames(res$pca$rotation))
    if (!length(pcs)) pcs <- colnames(res$pca$rotation)[seq_len(min(4, ncol(res$pca$rotation)))]
    load <- as.data.frame(res$pca$rotation[, pcs, drop = FALSE])
    load$Feature <- row.names(load)
    row.names(load) <- NULL
    first_pc <- pcs[[1]]
    load <- load[order(abs(load[[first_pc]]), decreasing = TRUE), , drop = FALSE]
    apply_table_value_filter("dimred_feature_loading_table", load[, c("Feature", setdiff(names(load), "Feature")), drop = FALSE])
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$contrast_controls_ui <- renderUI({
    nums <- generalized_numeric_columns()
    groups <- generalized_group_columns()
    df <- tab4_source_table()
    if (!nrow(df) || !length(nums) || !length(groups)) return(tags$p("Save the tab-3 generalized feature table first."))
    selected_group <- input$contrast_group %||% groups[[1]]
    tagList(
      selectInput("contrast_group", "Metadata/model grouping feature for contrast", choices = groups, selected = selected_group),
      uiOutput("contrast_level_picker_ui"),
      checkboxGroupInput("contrast_features", "Candidate variables to rank", choices = nums, selected = head(nums, min(10, length(nums))))
    )
  })

  output$contrast_level_picker_ui <- renderUI({
    df <- tab4_filtered_table()
    group <- input$contrast_group %||% ""
    if (!nrow(df) || !group %in% names(df)) return(NULL)
    vals <- ordered_unique_values(df[[group]])
    vals <- vals[nzchar(vals)]
    tagList(
      selectInput("contrast_level_a", "Contrast group A", choices = vals, selected = vals[[1]] %||% ""),
      selectInput("contrast_level_b", "Contrast group B", choices = vals, selected = vals[[min(2, length(vals))]] %||% "")
    )
  })

  contrast_variable_ranking <- reactive({
    df <- tab4_filtered_table()
    group <- input$contrast_group %||% ""
    a <- input$contrast_level_a %||% ""
    b <- input$contrast_level_b %||% ""
    features <- intersect(input$contrast_features %||% character(), names(df))
    if (!nrow(df) || !group %in% names(df) || !nzchar(a) || !nzchar(b) || identical(a, b) || !length(features)) {
      return(data.frame(Message = "Select a grouping feature, two different levels, and variables to rank."))
    }
    rows <- lapply(features, function(f) {
      x <- suppressWarnings(as.numeric(df[[f]]))
      xa <- x[as.character(df[[group]]) == a]
      xb <- x[as.character(df[[group]]) == b]
      xa <- xa[is.finite(xa)]
      xb <- xb[is.finite(xb)]
      if (length(xa) < 2 || length(xb) < 2) {
        return(data.frame(Feature = f, Group_A = a, Group_B = b, N_A = length(xa), N_B = length(xb), Mean_A = NA_real_, Mean_B = NA_real_, Effect_size_Cohen_d = NA_real_, Abs_effect_size = NA_real_, P_value = NA_real_))
      }
      pooled_sd <- sqrt(((length(xa) - 1) * stats::var(xa) + (length(xb) - 1) * stats::var(xb)) / (length(xa) + length(xb) - 2))
      d <- if (is.finite(pooled_sd) && pooled_sd > 0) (mean(xa) - mean(xb)) / pooled_sd else NA_real_
      p <- tryCatch(stats::t.test(xa, xb)$p.value, error = function(e) NA_real_)
      data.frame(Feature = f, Group_A = a, Group_B = b, N_A = length(xa), N_B = length(xb), Mean_A = mean(xa), Mean_B = mean(xb), Effect_size_Cohen_d = d, Abs_effect_size = abs(d), P_value = p)
    })
    out <- do.call(rbind, rows)
    out$P_adjusted_Holm <- stats::p.adjust(out$P_value, method = "holm")
    out[order(out$Abs_effect_size, decreasing = TRUE), , drop = FALSE]
  })

  output$contrast_variable_table <- renderTable({
    apply_table_value_filter("contrast_variable_table", contrast_variable_ranking())
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$contrast_variable_plot <- renderPlot({
    on.exit(remember_plot("contrast_variable_plot"), add = TRUE)
    r <- contrast_variable_ranking()
    if (!nrow(r) || !"Abs_effect_size" %in% names(r)) {
      plot.new(); text(0.5, 0.5, "No contrast ranking available."); return()
    }
    r <- r[is.finite(r$Abs_effect_size), , drop = FALSE]
    if (!nrow(r)) {
      plot.new(); text(0.5, 0.5, "No finite contrast effect sizes available."); return()
    }
    r <- head(r, 12)
    label_width <- max(14, min(28, max(nchar(r$Feature), na.rm = TRUE) * 0.32))
    op <- par(mar = c(5, label_width, 3, 1))
    on.exit(par(op), add = TRUE)
    labels <- plot_display_labels(rev(r$Feature), width = 26)
    barplot(rev(r$Abs_effect_size), names.arg = labels, horiz = TRUE, las = 1, cex.names = 0.72, col = "#8fb8de", border = "white", main = "Variables Best Separating the Selected Contrast", xlab = "|Cohen's d|")
  })

  output$cutoff_review_best_table <- renderTable({
    params <- cutoff_review_data()$params
    if (!nrow(params)) return(data.frame(Message = "No parameter_optimization_results.csv available."))
    if ("score" %in% names(params)) {
      params$score <- suppressWarnings(as.numeric(params$score))
      params <- params[order(params$score, decreasing = TRUE), , drop = FALSE]
    }
    keep <- intersect(c("score", "n_retained", "min_area", "max_area", "min_circularity", "max_circularity", "min_aspect_ratio", "max_aspect_ratio", "neurite_separation", "confluence_separation", "signal_noise_ratio"), names(params))
    out <- params[1, keep, drop = FALSE]
    apply_table_value_filter("cutoff_review_best_table", data.frame(Metric = names(out), Value = unlist(out[1, ], use.names = FALSE), row.names = NULL, check.names = FALSE))
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$cutoff_review_top_table <- renderTable({
    params <- cutoff_review_data()$params
    if (!nrow(params)) return(data.frame(Message = "No optimization result table available."))
    if ("score" %in% names(params)) {
      params$score <- suppressWarnings(as.numeric(params$score))
      params <- params[order(params$score, decreasing = TRUE), , drop = FALSE]
    }
    keep <- intersect(c("score", "n_retained", "min_area", "max_area", "min_circularity", "max_circularity", "min_aspect_ratio", "max_aspect_ratio", "signal_noise_ratio"), names(params))
    apply_table_value_filter("cutoff_review_top_table", head(params[, keep, drop = FALSE], 10))
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  cutoff_review_best_table_data <- function() {
    params <- cutoff_review_data()$params
    if (!nrow(params)) return(data.frame(Message = "No parameter_optimization_results.csv available."))
    if ("score" %in% names(params)) {
      params$score <- suppressWarnings(as.numeric(params$score))
      params <- params[order(params$score, decreasing = TRUE), , drop = FALSE]
    }
    keep <- intersect(c("score", "n_retained", "min_area", "max_area", "min_circularity", "max_circularity", "min_aspect_ratio", "max_aspect_ratio", "neurite_separation", "confluence_separation", "signal_noise_ratio"), names(params))
    out <- params[1, keep, drop = FALSE]
    data.frame(Metric = names(out), Value = unlist(out[1, ], use.names = FALSE), row.names = NULL, check.names = FALSE)
  }

  cutoff_review_top_table_data <- function() {
    params <- cutoff_review_data()$params
    if (!nrow(params)) return(data.frame(Message = "No optimization result table available."))
    if ("score" %in% names(params)) {
      params$score <- suppressWarnings(as.numeric(params$score))
      params <- params[order(params$score, decreasing = TRUE), , drop = FALSE]
    }
    keep <- intersect(c("score", "n_retained", "min_area", "max_area", "min_circularity", "max_circularity", "min_aspect_ratio", "max_aspect_ratio", "signal_noise_ratio"), names(params))
    head(params[, keep, drop = FALSE], 10)
  }

  cutoff_review_files_table_data <- function() {
    d <- cutoff_review_data()
    transform(d$files, bytes = ifelse(is.na(bytes), "", format(bytes, big.mark = ",")))
  }

  install_filterable_table("cutoff_review_checks_table", cutoff_review_checks, "cutoff_review_checks")
  install_filterable_table("cutoff_review_best_table", cutoff_review_best_table_data, "best_cutoff_summary")
  install_filterable_table("cutoff_review_top_table", cutoff_review_top_table_data, "top_parameter_sets")
  install_filterable_table("cutoff_review_files_table", cutoff_review_files_table_data, "cutoff_output_files")
  install_filterable_table("metadata_inspection_table", function() head(filtered_metadata_table(), 50), "metadata_preview_rows")
  install_filterable_table("metadata_unique_values_table", function() {
    meta <- read_metadata_table()
    feature <- input$metadata_inspect_feature %||% ""
    if (!nrow(meta) || !feature %in% names(meta)) return(data.frame(Message = "Select a metadata feature."))
    vals <- as.character(meta[[feature]])
    counts <- sort(table(vals, useNA = "ifany"), decreasing = TRUE)
    if (!length(counts)) return(data.frame(Message = "No values found for this feature."))
    tab <- data.frame(Unique_value = names(counts), Metadata_rows = as.integer(counts), stringsAsFactors = FALSE)
    tab$Percent <- round(100 * tab$Metadata_rows / sum(tab$Metadata_rows), 2)
    tab
  }, "metadata_unique_values")
  install_filterable_table("metadata_combination_counts_table", metadata_combination_counts, "metadata_combination_counts")
  install_filterable_table("metadata_match_diagnostics_table", function() {
    meta <- read_metadata_table()
    analysis_df <- generalized_analysis_table()
    saved_df <- saved_generalized_analysis_table()
    available_keys <- unique(c(analysis_df$.image_key %||% character(), saved_df$.image_key %||% character()))
    metadata_keys <- unique(meta$.image_key %||% character())
    data.frame(
      Diagnostic = c("Total metadata rows", "Unique metadata image keys", "Unique current/saved analysis image keys", "Matched image keys", "Metadata-only image keys", "Analysis-only image keys without metadata"),
      Count = c(nrow(meta), length(metadata_keys), length(available_keys), length(intersect(metadata_keys, available_keys)), length(setdiff(metadata_keys, available_keys)), length(setdiff(available_keys, metadata_keys))),
      stringsAsFactors = FALSE
    )
  }, "metadata_match_diagnostics")
  install_filterable_table("generalization_analysis_preview", function() {
    df <- generalized_analysis_table()
    if (!nrow(df)) df <- saved_generalized_analysis_table()
    head(df, 25)
  }, "generalization_analysis_preview")
  install_filterable_table("generalization_input_diagnostics_table", function() {
    diag <- generalization_input_diagnostics(input$generalization_models, settings())
    if (!nrow(diag)) data.frame(Message = "No profile diagnostics available.") else diag
  }, "generalization_input_diagnostics")
  install_filterable_table("generalization_facet_group_sizes", function() {
    df <- tab4_filtered_table()
    split <- build_generalization_facet_split(df)
    if (!nzchar(input$generalization_plot_facet_by %||% "")) return(data.frame(Message = "No secondary faceting variable selected."))
    if (!isTRUE(split$ok)) return(data.frame(Message = split$message))
    split$counts
  }, "facet_group_sizes")
  install_filterable_table("generalization_group_stats_table", generalization_group_stats_table_data, function() generalization_group_stats_table_title())
  install_filterable_table("tab4_training_role_counts_table", function() {
    df <- tab4_source_table()
    if (!nrow(df) || !".training_image_role" %in% names(df)) return(data.frame(Message = "No tab-4 analysis table is available yet."))
    row_counts <- sort(table(df$.training_image_role, useNA = "ifany"), decreasing = TRUE)
    unique_counts <- tapply(df$image_name, df$.training_image_role, function(x) length(unique(x)))
    data.frame(Training_role = names(row_counts), Unique_images = as.integer(unique_counts[names(row_counts)]), Image_model_rows = as.integer(row_counts), stringsAsFactors = FALSE)
  }, "tab4_training_role_counts")
  install_filterable_table("tab4_ilastik_training_match_table", tab4_ilastik_training_matches, "ilastik_training_match_diagnostics")
  install_filterable_table("tab4_cutoff_training_keys_table", function() {
    keys <- tab4_cutoff_training_keys()
    if (!length(keys)) data.frame(Message = "No cutoff optimization training image keys were detected from the inspected validation outputs.") else data.frame(Cutoff_training_image_key = keys, stringsAsFactors = FALSE)
  }, "cutoff_training_image_keys")
  install_filterable_table("dimred_separation_metric_table", pca_separation_metrics, "pca_separation_metrics")
  install_filterable_table("dimred_feature_loading_table", function() {
    res <- dimred_result()
    if (!isTRUE(res$ok)) return(data.frame(Message = res$message))
    pcs <- input$dimred_loading_pcs %||% paste0("PC", 1:4)
    pcs <- intersect(pcs, colnames(res$pca$rotation))
    if (!length(pcs)) pcs <- colnames(res$pca$rotation)[seq_len(min(4, ncol(res$pca$rotation)))]
    load <- as.data.frame(res$pca$rotation[, pcs, drop = FALSE])
    load$Feature <- row.names(load)
    row.names(load) <- NULL
    first_pc <- pcs[[1]]
    load <- load[order(abs(load[[first_pc]]), decreasing = TRUE), , drop = FALSE]
    load[, c("Feature", setdiff(names(load), "Feature")), drop = FALSE]
  }, "pca_loadings")
  install_filterable_table("contrast_variable_table", contrast_variable_ranking, "contrast_variable_ranking")
  install_filterable_table("covariate_correlation_pairs_table", function() {
    res <- covariate_correlation_result()
    if (!isTRUE(res$ok)) return(data.frame(Message = res$message))
    out <- head(res$pairs, 25)
    out$Correlation <- round(out$Correlation, 3)
    out$Abs_correlation <- round(out$Abs_correlation, 3)
    out
  }, "top_pairwise_covariate_correlations")
  install_filterable_table("covariate_recommendation_table", function() {
    res <- covariate_correlation_result()
    if (!isTRUE(res$ok)) data.frame(Message = res$message) else res$recommendations
  }, "suggested_covariate_retention")
  install_filterable_table("ancova_result_table", ancova_result_table, "ancova_contribution_table")
  install_filterable_table("ancova_primary_contrast_table", ancova_primary_contrast_table, "ancova_adjusted_contrasts")

  output$cutoff_review_score_plot <- renderPlot({
    on.exit(remember_plot("cutoff_review_score_plot"), add = TRUE)
    params <- cutoff_review_data()$params
    if (!nrow(params) || !"score" %in% names(params)) {
      plot.new(); text(0.5, 0.5, "No optimization score table available."); return()
    }
    scores <- suppressWarnings(as.numeric(params$score))
    scores <- scores[is.finite(scores)]
    if (!length(scores)) {
      plot.new(); text(0.5, 0.5, "No finite optimization scores available."); return()
    }
    op <- par(mar = c(4, 4, 2, 1))
    on.exit(par(op), add = TRUE)
    hist(scores, breaks = 30, col = "#8fb8de", border = "white", main = "Optimization Score Distribution", xlab = "Score")
    abline(v = max(scores), col = "#0b5d1e", lwd = 3)
    legend("topright", legend = "Best score", col = "#0b5d1e", lwd = 3, bty = "n")
  })

  output$cutoff_review_retention_plot <- renderPlot({
    on.exit(remember_plot("cutoff_review_retention_plot"), add = TRUE)
    d <- cutoff_review_data()
    raw <- d$raw
    filtered <- d$filtered
    if (!nrow(raw) && !nrow(filtered)) {
      plot.new(); text(0.5, 0.5, "No raw/filtered ROI tables available."); return()
    }
    raw_n <- nrow(raw)
    filtered_n <- nrow(filtered)
    raw_signal <- if (nrow(raw) && "is_signal" %in% names(raw)) sum(as.logical(raw$is_signal), na.rm = TRUE) else NA_real_
    raw_noise <- if (nrow(raw) && "is_noise" %in% names(raw)) sum(as.logical(raw$is_noise), na.rm = TRUE) else NA_real_
    filtered_signal <- if (nrow(filtered) && "is_signal" %in% names(filtered)) sum(as.logical(filtered$is_signal), na.rm = TRUE) else NA_real_
    filtered_noise <- if (nrow(filtered) && "is_noise" %in% names(filtered)) sum(as.logical(filtered$is_noise), na.rm = TRUE) else NA_real_
    vals <- c(raw_n, filtered_n, raw_signal, filtered_signal, raw_noise, filtered_noise)
    names(vals) <- c("Raw all", "Filtered all", "Raw signal", "Filtered signal", "Raw noise", "Filtered noise")
    vals[!is.finite(vals)] <- 0
    op <- par(mar = c(7, 4, 2, 1))
    on.exit(par(op), add = TRUE)
    barplot(vals, las = 2, col = c("#9aa8b2", "#2f855a", "#b6e3b6", "#1f7a3a", "#f2b8b5", "#b8322a"), main = "ROI Retention and Label Balance", ylab = "ROI rows")
  })

  output$generalization_phase_ui <- renderUI({
    steps <- safe_steps_for_ui()
    phase_steps <- steps_for_phase(steps, "Generalization with New Images")
    p <- build_paths(settings())
    saved_out <- generalized_analysis_output_paths(settings())
    saved_rows <- nrow(saved_generalized_analysis_table())
    legacy_rds_exists <- file.exists(p$generalized)
    tabsetPanel(
      id = "generalization_tabs",
      selected = input$generalization_tabs %||% isolate(generalization_tab_selected()),
      tabPanel(
        "3.1. Metadata Inspection",
        div(
          class = "decision-box",
          "Inspect the metadata table before applying models broadly. This helps confirm which image belongs to which treatment, concentration, batch, or other experimental feature.",
          fluidRow(
            column(4, div(class = "manual-panel", uiOutput("metadata_feature_picker_ui"), uiOutput("metadata_value_filter_ui"), uiOutput("metadata_combination_picker_ui"), uiOutput("metadata_inspection_summary"), uiOutput("metadata_balance_summary_ui"))),
            column(
              8,
              div(class = "manual-panel table-wrap-panel", tags$h4("Metadata preview rows"), tags$p("Preview only: shows the first 50 rows after filtering, not the full metadata table."), filterable_table_controls("metadata_inspection_table"), tableOutput("metadata_inspection_table")),
              div(class = "manual-panel table-wrap-panel", tags$h4("Unique values for selected metadata feature"), filterable_table_controls("metadata_unique_values_table"), tableOutput("metadata_unique_values_table")),
              div(class = "manual-panel table-wrap-panel", tags$h4("Metadata combination counts and balance"), filterable_table_controls("metadata_combination_counts_table"), tableOutput("metadata_combination_counts_table")),
              div(class = "manual-panel table-wrap-panel", tags$h4("Metadata-to-analysis matching diagnostics"), filterable_table_controls("metadata_match_diagnostics_table"), tableOutput("metadata_match_diagnostics_table"))
            )
          )
        )
      ),
      tabPanel(
        "3.2. Analysis Specification and Export",
        div(
          class = "decision-box",
          "Select one or more cutoff models and feature definitions. The app builds one analysis-ready table with image/model rows and metadata columns, then saves it for tab 4 multivariate analysis.",
          fluidRow(
            column(4, div(class = "manual-panel", uiOutput("generalization_model_picker_ui"), uiOutput("generalization_feature_picker_ui"), tags$div(class = "step-summary-box step-summary-neutral", tags$strong("Production application"), tags$p("Use this button to apply the selected validated cutoff model to all ROI folders from the full image set. This is the real handoff step for >500 images.")), actionButton("run_production_generalization_btn", "Apply Selected Cutoff Model to All Images", class = "module-run-button"), actionButton("build_generalization_analysis_btn", "Build / Refresh Validation-Table Preview", class = "btn btn-default"), actionButton("save_generalization_analysis_btn", "Save Preview Table for Tab 4", class = "apply-button"), uiOutput("generalization_analysis_status"), tags$h4("Selected Profile Input Diagnostics"), filterable_table_controls("generalization_input_diagnostics_table"), tableOutput("generalization_input_diagnostics_table"), uiOutput("generalization_saved_paths_ui"))),
            column(8, div(class = "manual-panel table-wrap-panel", tags$h4("Analysis table preview"), filterable_table_controls("generalization_analysis_preview"), tableOutput("generalization_analysis_preview")))
          )
        )
      ),
      tabPanel(
        "3.3. Legacy Backend Scripts",
        div(
          class = "decision-box",
          "This panel is optional and only applies to the older skeleton-RDS backend branch. For the current workflow, tab 3.2 is the production generalization/export step; after saving that table, continue directly to tab 4.",
          tags$div(
            class = if (saved_rows > 0) "step-summary-box step-summary-ready" else "step-summary-box step-summary-neutral",
            tags$strong("Current app workflow status"),
            tags$p(sprintf("Saved generalized model-feature rows available for tab 4: %s", saved_rows)),
            tags$p(tags$strong("Current workflow CSV: "), tags$span(class = "path-block", saved_out$csv)),
            tags$p(tags$strong("Current workflow RDS: "), tags$span(class = "path-block", saved_out$rds)),
            if (saved_rows > 0) tags$p("You do not need to run the legacy backend card below. Proceed to tab 4 for dimensionality reduction, contrasts, grouped plots, and ANCOVA.")
          ),
          if (legacy_rds_exists) {
            tagList(
              tags$div(
                class = "step-summary-box step-summary-partial",
                tags$strong("Legacy skeleton-RDS input detected"),
                tags$p("The optional legacy backend card is shown because its expected input exists."),
                tags$p(tags$strong("Legacy input: "), tags$span(class = "path-block", p$generalized))
              ),
              div(class = "step-grid", lapply(phase_steps, step_card_ui))
            )
          } else {
            tags$div(
              class = "step-summary-box step-summary-neutral",
              tags$strong("Legacy backend not available"),
              tags$p("The legacy backend card is hidden because its required input does not exist. This is normal when using the newer tab 3.2 table-based workflow."),
              tags$p(tags$strong("Missing legacy input: "), tags$span(class = "path-block", p$generalized))
            )
          }
        )
      )
    )
  })

  output$visualization_phase_ui <- renderUI({
    steps <- safe_steps_for_ui()
    phase_steps <- steps_for_phase(steps, "Visualization and Interpretation")
    tagList(
      uiOutput("tab4_filter_controls_ui"),
      tabsetPanel(
        id = "visualization_tabs",
        tabPanel(
          "4.0. Training Image Audit",
          div(
            class = "decision-box",
            "Identify images that were used during ilastik training or cutoff-model optimization before interpreting production results.",
            tags$div(
              class = "step-summary-box step-summary-neutral",
              tags$strong("Method explanation"),
              tags$p("Cutoff-training images are detected from the validation grouping and inspected-label tables generated during model optimization. Ilastik-training images are detected from likely training-image folders near the effective .ilp project, the managed pipeline_inputs/ilastik folder, direct Zenodo layouts, or the legacy repository model folder. They are then matched against analysis image names by the distinctive final filename stem, with file-size support when matching source files are available."),
              tags$p("This keeps the audit compatible with several normal scenarios: using the bundled legacy model folder, selecting an external .ilp project with sibling training images, unzipping the Zenodo archive directly in pipeline_inputs, or placing a custom model under pipeline_inputs/ilastik/model."),
              tags$p("This tab is intentionally preliminary: use it to decide whether training images should be included in downstream PCA, contrasts, grouped plots, and ANCOVA. The shared filter above contains toggles to include or exclude both training-image classes.")
            ),
            uiOutput("tab4_training_audit_summary"),
            fluidRow(
              column(4, div(class = "manual-panel table-wrap-panel", tags$h4("Training role counts in tab-4 table"), filterable_table_controls("tab4_training_role_counts_table"), tableOutput("tab4_training_role_counts_table"))),
              column(8, div(class = "manual-panel table-wrap-panel", tags$h4("Ilastik training image match diagnostics"), filterable_table_controls("tab4_ilastik_training_match_table"), tableOutput("tab4_ilastik_training_match_table")))
            ),
            fluidRow(
              column(12, div(class = "manual-panel table-wrap-panel", tags$h4("Cutoff optimization training image keys"), filterable_table_controls("tab4_cutoff_training_keys_table"), tableOutput("tab4_cutoff_training_keys_table")))
            )
          )
        ),
        tabPanel(
          "4.1. Global Structure: PCA and Separation",
          div(
            class = "decision-box",
            "Start interpretation by reducing the saved tab-3 feature table to principal components. Color points by metadata/model features and assess whether PCA space separates according to a selected biological, metadata, or model variable.",
            tags$div(
              class = "step-summary-box step-summary-neutral",
              tags$strong("Method explanation"),
              tags$p("PCA transforms correlated neurite/image features into principal components, ordered by how much total variance they explain. The scree plot shows whether most structure is captured by the first few PCs. Loadings show which original features push each selected PC up or down."),
              tags$p("The separation plot is supervised: it does not invent clusters. It asks whether the PCA coordinates visibly and quantitatively separate according to the selected metadata/model variable.")
            ),
            uiOutput("visualization_generalized_table_status"),
            fluidRow(
              column(4, div(class = "manual-panel", uiOutput("dimred_controls_ui"))),
              column(8, div(class = "manual-panel", plotOutput("dimred_pca_plot", height = "420px"), downloadButton("download_dimred_pca_plot", "Download High-Quality PNG"), actionButton("save_pub_dimred_pca_plot", "Save as Publication Candidate", class = "btn btn-default")))
            ),
            fluidRow(
              column(4, div(class = "manual-panel", plotOutput("dimred_scree_plot", height = "320px"), downloadButton("download_dimred_scree_plot", "Download High-Quality PNG"), actionButton("save_pub_dimred_scree_plot", "Save as Publication Candidate", class = "btn btn-default"))),
              column(
                8,
                div(class = "manual-panel", plotOutput("dimred_separation_plot", height = "420px"), downloadButton("download_dimred_separation_plot", "Download High-Quality PNG"), actionButton("save_pub_dimred_separation_plot", "Save as Publication Candidate", class = "btn btn-default")),
                div(class = "manual-panel table-wrap-panel", tags$h4("PCA Separation Metrics"), filterable_table_controls("dimred_separation_metric_table"), tableOutput("dimred_separation_metric_table"))
              )
            ),
            fluidRow(
              column(12, div(class = "manual-panel table-wrap-panel", tags$h4("PCA Loadings for Selected Dimensions"), filterable_table_controls("dimred_feature_loading_table"), tableOutput("dimred_feature_loading_table")))
            )
          )
        ),
        tabPanel(
          "4.2. Feature Screening by Contrast",
          div(
            class = "decision-box",
            "Screen many ROI-wise and image-wise variables to find which ones most strongly separate a prespecified two-group contrast.",
            tags$div(
              class = "step-summary-box step-summary-neutral",
              tags$strong("Method explanation"),
              tags$p("This panel compares two selected metadata/model groups for each candidate numeric feature. It reports Cohen's d as the standardized difference between group means, the raw t-test p-value, and a Holm-adjusted p-value across the screened features."),
              tags$p("Use this as feature prioritization, not as final proof: strong effects should still be checked for sample balance, metadata confounding, repeated model rows from multiple cutoff profiles, and consistency in the grouped plots.")
            ),
            fluidRow(
              column(4, div(class = "manual-panel", uiOutput("contrast_controls_ui"))),
              column(8, div(class = "manual-panel", plotOutput("contrast_variable_plot", height = "460px"), downloadButton("download_contrast_variable_plot", "Download High-Quality PNG"), actionButton("save_pub_contrast_variable_plot", "Save as Publication Candidate", class = "btn btn-default")))
            ),
            fluidRow(
              column(12, div(class = "manual-panel table-wrap-panel", tags$h4("Contrast variable ranking"), filterable_table_controls("contrast_variable_table"), tableOutput("contrast_variable_table")))
            )
          )
        ),
        tabPanel(
          "4.3. Visual Inspection: Grouped Dot and Bar Plot",
          div(
            class = "decision-box",
            "Visualize the saved tab-3 output. Choose a feature and group it by a metadata feature, model profile, or a combination of two grouping features.",
            tags$div(
              class = "step-summary-box step-summary-neutral",
              tags$strong("Method explanation"),
              tags$p("Dots show individual image/model rows after the shared tab-4 filter. Bars show the mean for each displayed group. This makes it easy to see both the central tendency and whether the result is driven by only a few points."),
              tags$p("Numeric grouping variables are ordered by numeric value, not alphabetically. Color can represent the x-axis group, another metadata feature, or a numeric gradient such as concentration/confluence."),
              tags$p("The optional normalization selector divides the plotted outcome by a selected numeric variable, for example neurite length divided by confluence. Use it as a visual sensitivity check for possible confounding; for formal adjustment, use tab 4.5 ANCOVA.")
            ),
            fluidRow(
              column(4, div(class = "manual-panel", uiOutput("generalization_plot_controls_ui"))),
              column(8, div(class = "manual-panel", plotOutput("generalization_grouped_plot", height = "520px"), downloadButton("download_generalization_grouped_plot", "Download High-Quality PNG"), actionButton("save_pub_generalization_grouped_plot", "Save as Publication Candidate", class = "btn btn-default")))
            ),
            fluidRow(
              column(
                12,
                div(
                  class = "manual-panel table-wrap-panel",
                  uiOutput("generalization_group_stats_info_ui"),
                  filterable_table_controls("generalization_group_stats_table"),
                  tableOutput("generalization_group_stats_table")
                )
              )
            ),
            fluidRow(
              column(
                8,
                div(
                  class = "manual-panel",
                  tags$h4("Facet Split Preview"),
                  tags$p("Preview the selected secondary variable before applying it as stacked facets. Numeric variables show the distribution and quantile cutoff lines; categorical variables show group sizes."),
                  plotOutput("generalization_facet_helper_plot", height = "280px"),
                  downloadButton("download_generalization_facet_helper_plot", "Download High-Quality PNG"),
                  actionButton("save_pub_generalization_facet_helper_plot", "Save as Publication Candidate", class = "btn btn-default")
                )
              ),
              column(
                4,
                div(
                  class = "manual-panel table-wrap-panel",
                  tags$h4("Facet Group Sizes"),
                  filterable_table_controls("generalization_facet_group_sizes"),
                  tableOutput("generalization_facet_group_sizes")
                )
              )
            )
          )
        ),
        tabPanel(
          "4.4. Covariate Correlation Check",
          div(
            class = "decision-box",
            "Before adjusted modeling, inspect whether candidate covariates are strongly intercorrelated. Highly correlated covariates can make model terms unstable and hard to interpret.",
            tags$div(
              class = "step-summary-box step-summary-neutral",
              tags$strong("Method explanation"),
              tags$p("This tab computes pairwise Pearson correlations among selected numeric covariates/features after the shared tab-4 filters are applied. Strong correlations, often |r| >= 0.80, suggest that two variables carry overlapping information."),
              tags$p("The recommendation table gives a simple default suggestion: avoid using highly correlated variables together. The retained-variable suggestion is a heuristic, not a rule; biological interpretability, missingness, and whether the variable is a direct outcome-derived quantity should override it when needed.")
            ),
            fluidRow(
              column(4, div(class = "manual-panel", uiOutput("covariate_correlation_controls_ui"), div(class = "manual-panel table-wrap-panel", tags$h4("Suggested Covariate Retention"), filterable_table_controls("covariate_recommendation_table"), tableOutput("covariate_recommendation_table")))),
              column(8, div(class = "manual-panel", plotOutput("covariate_correlation_heatmap", height = "520px"), downloadButton("download_covariate_correlation_heatmap", "Download High-Quality PNG"), actionButton("save_pub_covariate_correlation_heatmap", "Save as Publication Candidate", class = "btn btn-default")))
            ),
            fluidRow(
              column(12, uiOutput("covariate_next_step_ui"))
            ),
            fluidRow(
              column(12, div(class = "manual-panel table-wrap-panel", tags$h4("Top Pairwise Covariate Correlations"), filterable_table_controls("covariate_correlation_pairs_table"), tableOutput("covariate_correlation_pairs_table")))
            )
          )
        ),
        tabPanel(
          "4.5. Adjusted Effect Modeling / ANCOVA",
          div(
            class = "decision-box",
            "Model one selected outcome while adjusting for selected covariates. Use this after feature screening, visual inspection, and covariate-correlation checks.",
            tags$div(
              class = "step-summary-box step-summary-neutral",
              tags$strong("Method explanation"),
              tags$p("ANCOVA fits a linear model for a selected outcome using one primary group variable plus optional covariates. It helps estimate whether a factor still contributes after accounting for other measured variables."),
              tags$p("You can optionally model a ratio outcome, such as total skeleton length divided by confluence. This is not the same as adding confluence as a covariate: the ratio asks about neurite burden per unit confluence, while covariate adjustment asks about total neurite burden after accounting for confluence."),
              tags$p("The contribution table uses partial drop-one F tests: each term is tested by removing it from the full model while keeping the other selected terms. This is safer for adjusted interpretation than sequential ANOVA sums-of-squares, where term order can change the answer."),
              tags$p("The contrast table below answers the more specific question: which adjusted primary-factor levels differ, using selected two levels, all levels versus a reference/control, or all pairwise comparisons."),
              tags$p("Do not use the same variable as both the primary contrast and a covariate. That asks the model to test and adjust away the same effect at once, which makes interpretation circular or impossible."),
              tags$p("Avoid placing highly mutually correlated covariates in the same model unless there is a strong biological reason. Use tab 4.4 first to identify which covariates are safest to retain, then use the recommendation button in this panel as a starting point."),
              tags$p("Partial eta-squared estimates the fraction of explainable residual variation associated with each term after the rest of the model is accounted for. Normalized impact size is sqrt(partial eta-squared), making contribution magnitudes easier to compare.")
            ),
            fluidRow(
              column(4, div(class = "manual-panel", uiOutput("ancova_controls_ui"))),
              column(
                8,
                uiOutput("ancova_model_guidance_ui"),
                div(class = "manual-panel table-wrap-panel", tags$h4("Adjusted ANCOVA Contribution Table"), filterable_table_controls("ancova_result_table"), tableOutput("ancova_result_table")),
                div(class = "manual-panel table-wrap-panel", tags$h4("Adjusted primary-factor contrast table"), filterable_table_controls("ancova_primary_contrast_table"), tableOutput("ancova_primary_contrast_table"))
              )
            )
          )
        ),
        tabPanel(
          "4.6. Backend Visualization Scripts",
          div(
            class = "decision-box",
            "These are the original backend visualization and interpretation script cards. Use them after the generalized outputs are ready.",
            tags$div(
              class = "step-summary-box step-summary-neutral",
              tags$strong("Method explanation"),
              tags$p("These cards expose the older script-based visualization branch. They are kept for reproducibility and comparison with earlier analyses, while the newer subtabs above use the saved generalized model-feature table directly.")
            ),
            div(class = "step-grid", lapply(phase_steps, step_card_ui))
          )
        ),
        tabPanel(
          "4.7. Publication Figure Review and Export",
          div(
            class = "decision-box",
            "Collect meaningful Tab 4 plots as publication candidates, edit figure-level labels and style notes, and export high-resolution figure files. The saved figure metadata and snapshots are also included in the reproducible report archive.",
            tags$div(
              class = "step-summary-box step-summary-neutral",
              tags$strong("Method and recreation note"),
              tags$p("Each saved candidate stores the recorded plot, a high-resolution source PNG, editable style metadata, and an input snapshot describing the filters, model choices, plotted features, and grouping variables active when the figure was saved."),
              tags$p("Tables can also be saved as publication candidates. The app stores the currently filtered/sorted table as CSV plus metadata and an input snapshot, so exact table contents can be reused in the final report."),
      tags$p("The preview updates live from the fields below, using a lightweight cached preview image so interactive editing stays responsive. It does not overwrite the saved style until you click Save Edited Figure Metadata or download the edited PNG/metadata. This protects earlier figure decisions while still allowing quick experimentation."),
      tags$p("Current behavior: supported figure candidates are regenerated from saved plot data, plot recipe, and editable style settings before export. This means labels, ticks, legends, titles, and margins are edited at the plot level rather than as a PNG overlay. Older or unsupported candidates still use the PNG fallback so previous saved figures remain usable."),
              tags$p("For complete recreation, the report also stores command history, software/tool versions, R package versions, active scripts, selected validation profiles, key output tables, and workflow step comments. This means a future user can identify which data, model, code, settings, and analysis choices produced each figure.")
            ),
            tabsetPanel(
              id = "publication_candidate_tabs",
              tabPanel(
                "4.7.1. Figure Editing",
                fluidRow(
                  column(
                    4,
                    div(
                      class = "manual-panel",
                      uiOutput("publication_candidate_selector_ui"),
                      tags$div(class = "metric-info-box", tags$strong("Editing flow"), tags$p("1. Choose the saved figure. 2. Check whether it is recipe-backed or PNG fallback. 3. Edit canvas margins, titles, axis text, tick/category labels, legend, and caption. 4. Preview/export. For recipe-backed figures these controls regenerate the plot from saved data; for PNG fallback figures they draw around or over the saved image.")),
                  tags$hr(),
                  tags$h5("1. Canvas Space Around the Plot"),
                  fluidRow(
                    column(4, numericInput("publication_plot_margin_input", "Base margin", value = 0.025, min = 0, max = 0.15, step = 0.005)),
                    column(4, numericInput("publication_plot_margin_top_input", "Extra top", value = 0, min = 0, max = 0.25, step = 0.005)),
                    column(4, numericInput("publication_plot_margin_right_input", "Extra right", value = 0, min = 0, max = 0.25, step = 0.005))
                  ),
                  fluidRow(
                    column(6, numericInput("publication_plot_margin_bottom_input", "Extra bottom", value = 0, min = 0, max = 0.25, step = 0.005)),
                    column(6, numericInput("publication_plot_margin_left_input", "Extra left", value = 0, min = 0, max = 0.25, step = 0.005))
                  ),
                  tags$div(class = "metric-info-box", "Start here if long labels are clipped. Use Bottom for angled X labels, Left for long Y labels, Top for titles, and Right when legends or labels need breathing room."),
                  tags$hr(),
                  tags$h5("2. Source Plot Cleanup and Cropping"),
                  checkboxInput("publication_auto_crop_original_text_input", "Automatically crop old embedded title/axis/tick text when replacements are drawn", value = TRUE),
                  checkboxInput("publication_hide_original_title_input", "Hide original embedded title/subtitle when edited title/subtitle is drawn", value = TRUE),
                  checkboxInput("publication_hide_original_x_axis_input", "Hide original embedded X-axis title when edited X-axis title is drawn", value = TRUE),
                  checkboxInput("publication_hide_original_y_axis_input", "Hide original embedded Y-axis title when edited Y-axis title is drawn", value = TRUE),
                  checkboxInput("publication_hide_original_x_ticks_input", "Hide original embedded X tick/category labels when replacement X ticks are drawn", value = TRUE),
                  checkboxInput("publication_hide_original_y_ticks_input", "Hide original embedded Y tick/category labels when replacement Y ticks are drawn", value = TRUE),
                  fluidRow(
                    column(3, numericInput("publication_source_crop_top_input", "Crop top", value = 0, min = 0, max = 0.45, step = 0.005)),
                    column(3, numericInput("publication_source_crop_bottom_input", "Crop bottom", value = 0, min = 0, max = 0.45, step = 0.005)),
                    column(3, numericInput("publication_source_crop_left_input", "Crop left", value = 0, min = 0, max = 0.45, step = 0.005)),
                    column(3, numericInput("publication_source_crop_right_input", "Crop right", value = 0, min = 0, max = 0.45, step = 0.005))
                  ),
                  tags$div(class = "metric-info-box", "Best practice is to save plots after the app has already converted raw variable names into readable display labels. Cropping remains only a fallback for older saved candidates or unusual figures where the original embedded labels are still visible. If duplicated labels appear, keep the relevant hide option enabled or adjust the source crop slightly."),
                  tags$hr(),
                  tags$h5("3. Title, Subtitle, and Global Font"),
                  selectInput("publication_font_family_input", "Font family", choices = c("Arial", "Helvetica", "Times", "serif", "sans", "mono"), selected = "Arial"),
                  textInput("publication_title_input", "Figure title", value = ""),
                  textInput("publication_subtitle_input", "Subtitle", value = ""),
                  fluidRow(
                    column(6, numericInput("publication_title_size_input", "Title size", value = 18, min = 8, max = 48)),
                    column(6, numericInput("publication_title_distance_input", "Title distance from plot", value = 0.035, min = 0, max = 0.20, step = 0.005))
                  ),
                  fluidRow(
                    column(6, numericInput("publication_subtitle_size_input", "Subtitle size", value = 12, min = 6, max = 36)),
                    column(6, numericInput("publication_subtitle_distance_input", "Subtitle distance from plot", value = 0.065, min = 0, max = 0.25, step = 0.005))
                  ),
                  checkboxInput("publication_bold_title_input", "Bold title", value = TRUE),
                  checkboxInput("publication_italic_title_input", "Italic title", value = FALSE),
                  checkboxInput("publication_bold_subtitle_input", "Bold subtitle", value = FALSE),
                  checkboxInput("publication_italic_subtitle_input", "Italic subtitle", value = FALSE),
                  tags$hr(),
                  tags$h5("4. Axis Title Text and Style"),
                  tags$div(class = "metric-info-box", "Axis titles are the centered labels such as 'PC1' or 'Skeleton length'. Category names, group names, feature names, and numeric tick values belong in the tick-label fields below, not here."),
                  textInput("publication_x_label_input", "X-axis title, not tick/category labels", value = ""),
                  fluidRow(
                    column(4, numericInput("publication_x_axis_size_input", "Text size", value = 12, min = 6, max = 36)),
                    column(4, numericInput("publication_x_axis_angle_input", "Angle", value = 0, min = -90, max = 90, step = 5)),
                    column(4, numericInput("publication_x_axis_distance_input", "Distance", value = 0.035, min = 0, max = 0.25, step = 0.005))
                  ),
                  checkboxInput("publication_bold_x_axis_input", "Bold X-axis label", value = FALSE),
                  checkboxInput("publication_italic_x_axis_input", "Italic X-axis label", value = FALSE),
                  textInput("publication_y_label_input", "Y-axis title, not tick/category labels", value = ""),
                  fluidRow(
                    column(4, numericInput("publication_y_axis_size_input", "Text size", value = 12, min = 6, max = 36)),
                    column(4, numericInput("publication_y_axis_angle_input", "Angle", value = 90, min = -90, max = 90, step = 5)),
                    column(4, numericInput("publication_y_axis_distance_input", "Distance", value = 0.035, min = 0, max = 0.25, step = 0.005))
                  ),
                  checkboxInput("publication_bold_y_axis_input", "Bold Y-axis label", value = FALSE),
                  checkboxInput("publication_italic_y_axis_input", "Italic Y-axis label", value = FALSE),
                  tags$hr(),
                  tags$h5("5. Tick and Category Label Text and Style"),
                  textInput("publication_x_tick_labels_input", "X tick/category labels, separated by |, comma, or semicolon", value = ""),
                  fluidRow(
                    column(4, numericInput("publication_x_tick_size_input", "Text size", value = 9, min = 5, max = 28)),
                    column(4, numericInput("publication_x_tick_angle_input", "Angle", value = 0, min = -90, max = 90, step = 5)),
                    column(4, numericInput("publication_x_tick_distance_input", "Distance", value = 0.075, min = 0, max = 0.30, step = 0.005))
                  ),
                  checkboxInput("publication_bold_x_ticks_input", "Bold X tick labels", value = FALSE),
                  checkboxInput("publication_italic_x_ticks_input", "Italic X tick labels", value = FALSE),
                  textInput("publication_y_tick_labels_input", "Y tick/category labels, separated by |, comma, or semicolon", value = ""),
                  fluidRow(
                    column(4, numericInput("publication_y_tick_size_input", "Text size", value = 9, min = 5, max = 28)),
                    column(4, numericInput("publication_y_tick_angle_input", "Angle", value = 0, min = -90, max = 90, step = 5)),
                    column(4, numericInput("publication_y_tick_distance_input", "Distance", value = 0.075, min = 0, max = 0.30, step = 0.005))
                  ),
                  checkboxInput("publication_bold_y_ticks_input", "Bold Y tick labels", value = FALSE),
                  checkboxInput("publication_italic_y_ticks_input", "Italic Y tick labels", value = FALSE),
                  tags$div(class = "metric-info-box", "Tick labels are redrawn evenly along the cropped source plot. Use this for category labels, feature names, group names, or numeric tick values. If old labels remain visible, keep automatic cropping enabled or increase the relevant source crop."),
                  tags$hr(),
                  tags$h5("6. Editable In-Plot Legend Overlay"),
                  textAreaInput("publication_legend_text_input", "Legend text drawn on top of the plot", value = "", rows = 3, placeholder = "Example: Control | Retinoic acid | Rosmarinic acid"),
                  fluidRow(
                    column(4, numericInput("publication_legend_size_input", "Legend text size", value = 9, min = 5, max = 28)),
                    column(4, numericInput("publication_legend_x_nudge_input", "Move left/right", value = 0, min = -0.60, max = 0.20, step = 0.005)),
                    column(4, numericInput("publication_legend_y_nudge_input", "Move down/up", value = 0, min = -0.60, max = 0.20, step = 0.005))
                  ),
                  fluidRow(
                    column(6, selectInput("publication_legend_symbol_type_input", "Legend marker style", choices = c("Text only" = "none", "Colored dots" = "dots", "Colored squares" = "squares"), selected = "none")),
                    column(6, textInput("publication_legend_colors_input", "Marker colors", value = "", placeholder = "#1b9e77 | #d95f02 | #7570b3"))
                  ),
                  tags$div(class = "metric-info-box", "This draws a new editable legend overlay near the upper-right of the plot area. Use |, semicolon, or new lines to separate legend rows. Marker colors accept R color names or hex colors separated by |, comma, or semicolon. For PCA numeric legends, the plot itself now uses a continuous palette scale; this overlay is mainly for final publication polishing. If the old embedded legend remains visible, crop that part of the source image or leave the overlay blank."),
                  tags$hr(),
                  tags$h5("7. Caption Below the Plot"),
                  textAreaInput("publication_caption_input", "Caption text below plot", value = "", rows = 4),
                  fluidRow(
                    column(6, numericInput("publication_caption_size_input", "Caption text size", value = 10, min = 6, max = 28)),
                    column(6, numericInput("publication_caption_distance_input", "Distance below plot", value = 0.060, min = 0, max = 0.25, step = 0.005))
                  ),
                  fluidRow(
                    column(6, numericInput("publication_caption_x_nudge_input", "Move left/right", value = 0, min = -0.45, max = 0.45, step = 0.005)),
                    column(6, numericInput("publication_caption_y_nudge_input", "Move down/up", value = 0, min = -0.30, max = 0.30, step = 0.005))
                  ),
                  tags$div(class = "metric-info-box", "Caption nudge controls move only the below-plot caption. Use the separate in-plot legend overlay controls above for legend text inside the plot panel."),
                  tags$hr(),
                  tags$h5("8. Save and Export"),
                  textAreaInput("publication_notes_input", "Editing notes for later figure polishing", value = "", rows = 3),
                  actionButton("publication_save_style_btn", "Save Edited Figure Metadata", class = "apply-button"),
                  downloadButton("download_publication_png_btn", "Download Edited High-Quality PNG"),
                  downloadButton("download_publication_metadata_btn", "Download Figure Metadata CSV"),
                  tags$hr(),
                  tags$h5("9. Clear Old Saved Figures"),
                  tags$div(class = "metric-info-box", "This only deletes saved publication figure-candidate folders from cache/publication_figures. It does not delete scripts, raw images, pipeline outputs, saved tables, or validation profiles."),
                  uiOutput("publication_delete_selector_ui"),
                  actionButton("publication_select_all_delete_btn", "Select All Figure Candidates", class = "btn btn-default"),
                  actionButton("publication_delete_selected_btn", "Delete Selected Figure Candidates", class = "btn btn-danger")
                )
              ),
              column(
                8,
                div(
                  class = "manual-panel publication-preview-panel",
                  tags$h4("Edited Figure Preview"),
                  div(
                    class = "publication-preview-frame",
                    imageOutput("publication_figure_preview_plot", width = "100%", height = "auto")
                  )
                ),
                div(class = "manual-panel table-wrap-panel publication-follow-panel", tags$h4("Saved Publication Figure Candidates"), filterable_table_controls("publication_candidates_table", save_publication = FALSE), tableOutput("publication_candidates_table")),
                div(class = "manual-panel table-wrap-panel", tags$h4("Saved Input Snapshot for Selected Figure"), tableOutput("publication_input_snapshot_table"))
              )
            )
          ),
          tabPanel(
            "4.7.2. Table Candidates",
            div(
              class = "manual-panel",
              tags$h4("Publication Table Workflow"),
              tags$p("Save table candidates from the table panels throughout Tab 4 by using their Save Table as Publication Candidate buttons. This subtab is only for reviewing, filtering, downloading, and confirming saved table candidates."),
              tags$p("Each saved table candidate includes the filtered/sorted CSV, table metadata, input snapshot, and a frozen publication_metadata.txt file describing the filters and analysis choices active when the table was saved.")
            ),
            fluidRow(
              column(
                6,
                div(class = "manual-panel table-wrap-panel", tags$h4("Saved Publication Table Candidates"), filterable_table_controls("publication_tables_table", save_publication = FALSE), tableOutput("publication_tables_table"))
              ),
              column(
                6,
                div(class = "manual-panel table-wrap-panel", tags$h4("Selected Publication Table Preview"), uiOutput("publication_table_selector_ui"), downloadButton("download_publication_table_btn", "Download Selected Table CSV"), tableOutput("publication_table_preview"))
              )
            )
          )
        )
      )
    )
    )
    )
  })

  lapply(unname(publication_plot_choices), function(plot_id) {
    local({
      id <- plot_id
      observeEvent(input[[paste0("save_pub_", id)]], {
        save_publication_candidate(id)
      }, ignoreInit = TRUE)
    })
  })

  output$publication_candidate_selector_ui <- renderUI({
    choices <- publication_candidate_choices()
    current <- input$publication_figure_choice %||% ""
    if (!current %in% unname(choices)) current <- unname(choices)[[1]] %||% ""
    current_dir <- publication_candidate_dir_for_id(current)
    mode <- if (nzchar(current_dir) && publication_recipe_supported(current_dir)) {
      tags$div(class = "step-summary-box step-summary-good", tags$strong("Editing mode: recipe-backed"), tags$p("This figure can be regenerated from saved data plus style settings before export. Axis labels, ticks, title, legend text, and plot text are edited at the plot level rather than as PNG overlays."))
    } else if (nzchar(current)) {
      tags$div(class = "step-summary-box step-summary-neutral", tags$strong("Editing mode: PNG fallback"), tags$p("This older or unsupported figure is edited as a saved image with optional overlay/crop controls. Save a fresh candidate from the source plot if you want recipe-backed editing."))
    } else {
      NULL
    }
    tagList(
      selectInput("publication_figure_choice", "Saved publication figure", choices = choices, selected = current),
      actionButton(
        "publication_restore_snapshot_btn",
        "Restore Saved Filters and Plot Controls",
        class = "btn btn-default"
      ),
      tags$p(
        class = "helper-text",
        "Reloads the tab-4 filters, model/profile choices, grouping variables, color mapping, and plot-control inputs saved with this candidate. Use this after restarting the app when you want to continue editing from the exact saved recipe context."
      ),
      mode
    )
  })

  output$publication_delete_selector_ui <- renderUI({
    choices <- publication_candidate_delete_choices()
    if (!length(choices)) {
      return(tags$p("No saved publication figure candidates are available to delete."))
    }
    selectizeInput(
      "publication_figures_to_delete",
      "Figure candidates to delete",
      choices = choices,
      selected = character(),
      multiple = TRUE,
      options = list(placeholder = "Choose old figure candidates to remove from the cache")
    )
  })

  observeEvent(input$publication_figure_choice, {
    fig_dir <- selected_publication_dir()
    if (!nzchar(fig_dir)) return()
    style <- read_publication_style(fig_dir)
    updateTextInput(session, "publication_title_input", value = style$title %||% "")
    updateTextInput(session, "publication_subtitle_input", value = style$subtitle %||% "")
    updateTextInput(session, "publication_x_label_input", value = style$x_label %||% "")
    updateTextInput(session, "publication_y_label_input", value = style$y_label %||% "")
    updateTextInput(session, "publication_x_tick_labels_input", value = style$x_tick_labels %||% "")
    updateTextInput(session, "publication_y_tick_labels_input", value = style$y_tick_labels %||% "")
    updateTextAreaInput(session, "publication_legend_text_input", value = style$legend_text %||% "")
    updateTextAreaInput(session, "publication_caption_input", value = style$caption %||% "")
    updateSelectInput(session, "publication_font_family_input", selected = style$font_family %||% "Arial")
    updateCheckboxInput(session, "publication_auto_crop_original_text_input", value = identical(tolower(style$auto_crop_original_text %||% "true"), "true"))
    updateCheckboxInput(session, "publication_hide_original_title_input", value = identical(tolower(style$hide_original_title %||% "true"), "true"))
    updateCheckboxInput(session, "publication_hide_original_x_axis_input", value = identical(tolower(style$hide_original_x_axis %||% "true"), "true"))
    updateCheckboxInput(session, "publication_hide_original_y_axis_input", value = identical(tolower(style$hide_original_y_axis %||% "true"), "true"))
    updateCheckboxInput(session, "publication_hide_original_x_ticks_input", value = identical(tolower(style$hide_original_x_ticks %||% "true"), "true"))
    updateCheckboxInput(session, "publication_hide_original_y_ticks_input", value = identical(tolower(style$hide_original_y_ticks %||% "true"), "true"))
    updateNumericInput(session, "publication_source_crop_top_input", value = suppressWarnings(as.numeric(style$source_crop_top %||% "0")))
    updateNumericInput(session, "publication_source_crop_bottom_input", value = suppressWarnings(as.numeric(style$source_crop_bottom %||% "0")))
    updateNumericInput(session, "publication_source_crop_left_input", value = suppressWarnings(as.numeric(style$source_crop_left %||% "0")))
    updateNumericInput(session, "publication_source_crop_right_input", value = suppressWarnings(as.numeric(style$source_crop_right %||% "0")))
    updateNumericInput(session, "publication_title_size_input", value = suppressWarnings(as.numeric(style$title_size %||% "18")))
    updateNumericInput(session, "publication_subtitle_size_input", value = suppressWarnings(as.numeric(style$subtitle_size %||% "12")))
    updateNumericInput(session, "publication_x_axis_size_input", value = suppressWarnings(as.numeric(style$x_axis_size %||% style$axis_size %||% "12")))
    updateNumericInput(session, "publication_y_axis_size_input", value = suppressWarnings(as.numeric(style$y_axis_size %||% style$axis_size %||% "12")))
    updateNumericInput(session, "publication_x_tick_size_input", value = suppressWarnings(as.numeric(style$x_tick_size %||% "9")))
    updateNumericInput(session, "publication_y_tick_size_input", value = suppressWarnings(as.numeric(style$y_tick_size %||% "9")))
    updateNumericInput(session, "publication_x_axis_angle_input", value = suppressWarnings(as.numeric(style$x_axis_angle %||% "0")))
    updateNumericInput(session, "publication_y_axis_angle_input", value = suppressWarnings(as.numeric(style$y_axis_angle %||% "90")))
    updateNumericInput(session, "publication_x_tick_angle_input", value = suppressWarnings(as.numeric(style$x_tick_angle %||% "0")))
    updateNumericInput(session, "publication_y_tick_angle_input", value = suppressWarnings(as.numeric(style$y_tick_angle %||% "0")))
    updateNumericInput(session, "publication_x_axis_distance_input", value = suppressWarnings(as.numeric(style$x_axis_distance %||% "0.035")))
    updateNumericInput(session, "publication_y_axis_distance_input", value = suppressWarnings(as.numeric(style$y_axis_distance %||% "0.035")))
    updateNumericInput(session, "publication_x_tick_distance_input", value = suppressWarnings(as.numeric(style$x_tick_distance %||% "0.075")))
    updateNumericInput(session, "publication_y_tick_distance_input", value = suppressWarnings(as.numeric(style$y_tick_distance %||% "0.075")))
    updateNumericInput(session, "publication_title_distance_input", value = suppressWarnings(as.numeric(style$title_distance %||% "0.035")))
    updateNumericInput(session, "publication_subtitle_distance_input", value = suppressWarnings(as.numeric(style$subtitle_distance %||% "0.065")))
    updateNumericInput(session, "publication_caption_distance_input", value = suppressWarnings(as.numeric(style$caption_distance %||% "0.060")))
    updateNumericInput(session, "publication_plot_margin_input", value = suppressWarnings(as.numeric(style$plot_margin %||% "0.025")))
    updateNumericInput(session, "publication_plot_margin_top_input", value = suppressWarnings(as.numeric(style$plot_margin_top %||% "0")))
    updateNumericInput(session, "publication_plot_margin_right_input", value = suppressWarnings(as.numeric(style$plot_margin_right %||% "0")))
    updateNumericInput(session, "publication_plot_margin_bottom_input", value = suppressWarnings(as.numeric(style$plot_margin_bottom %||% "0")))
    updateNumericInput(session, "publication_plot_margin_left_input", value = suppressWarnings(as.numeric(style$plot_margin_left %||% "0")))
    updateNumericInput(session, "publication_legend_size_input", value = suppressWarnings(as.numeric(style$legend_size %||% "9")))
    updateNumericInput(session, "publication_legend_x_nudge_input", value = suppressWarnings(as.numeric(style$legend_x_nudge %||% "0")))
    updateNumericInput(session, "publication_legend_y_nudge_input", value = suppressWarnings(as.numeric(style$legend_y_nudge %||% "0")))
    updateSelectInput(session, "publication_legend_symbol_type_input", selected = style$legend_symbol_type %||% "none")
    updateTextInput(session, "publication_legend_colors_input", value = style$legend_colors %||% "")
    updateNumericInput(session, "publication_caption_size_input", value = suppressWarnings(as.numeric(style$caption_size %||% "10")))
    updateNumericInput(session, "publication_caption_x_nudge_input", value = suppressWarnings(as.numeric(style$caption_x_nudge %||% "0")))
    updateNumericInput(session, "publication_caption_y_nudge_input", value = suppressWarnings(as.numeric(style$caption_y_nudge %||% "0")))
    updateCheckboxInput(session, "publication_italic_title_input", value = identical(tolower(style$italicize_title %||% "false"), "true"))
    updateCheckboxInput(session, "publication_italic_subtitle_input", value = identical(tolower(style$italicize_subtitle %||% "false"), "true"))
    updateCheckboxInput(session, "publication_italic_x_axis_input", value = identical(tolower(style$italicize_x_axis %||% style$italicize_axis_labels %||% "false"), "true"))
    updateCheckboxInput(session, "publication_italic_y_axis_input", value = identical(tolower(style$italicize_y_axis %||% style$italicize_axis_labels %||% "false"), "true"))
    updateCheckboxInput(session, "publication_italic_x_ticks_input", value = identical(tolower(style$italicize_x_ticks %||% "false"), "true"))
    updateCheckboxInput(session, "publication_italic_y_ticks_input", value = identical(tolower(style$italicize_y_ticks %||% "false"), "true"))
    updateCheckboxInput(session, "publication_bold_title_input", value = identical(tolower(style$bold_title %||% "true"), "true"))
    updateCheckboxInput(session, "publication_bold_subtitle_input", value = identical(tolower(style$bold_subtitle %||% "false"), "true"))
    updateCheckboxInput(session, "publication_bold_x_axis_input", value = identical(tolower(style$bold_x_axis %||% "false"), "true"))
    updateCheckboxInput(session, "publication_bold_y_axis_input", value = identical(tolower(style$bold_y_axis %||% "false"), "true"))
    updateCheckboxInput(session, "publication_bold_x_ticks_input", value = identical(tolower(style$bold_x_ticks %||% "false"), "true"))
    updateCheckboxInput(session, "publication_bold_y_ticks_input", value = identical(tolower(style$bold_y_ticks %||% "false"), "true"))
    updateTextAreaInput(session, "publication_notes_input", value = style$notes %||% "")
  }, ignoreInit = FALSE)

  observeEvent(input$publication_restore_snapshot_btn, {
    fig_dir <- selected_publication_dir()
    if (!nzchar(fig_dir)) {
      showNotification("No saved publication figure candidate is selected.", type = "warning")
      return()
    }
    snapshot <- publication_read_input_snapshot(fig_dir)
    if (!nrow(snapshot)) {
      showNotification("This saved candidate does not have an input_snapshot.csv file to restore from.", type = "warning")
      return()
    }
    restored <- publication_restore_inputs_from_snapshot(snapshot)
    publication_tick(publication_tick() + 1L)
    showNotification(
      sprintf("Restored %s saved filter/control value(s) from this publication recipe.", restored),
      type = "message",
      duration = 6
    )
  }, ignoreInit = TRUE)

  current_publication_style_from_inputs <- function() {
    list(
      title = input$publication_title_input %||% "",
      subtitle = input$publication_subtitle_input %||% "",
      x_label = input$publication_x_label_input %||% "",
      y_label = input$publication_y_label_input %||% "",
      x_tick_labels = input$publication_x_tick_labels_input %||% "",
      y_tick_labels = input$publication_y_tick_labels_input %||% "",
      legend_text = input$publication_legend_text_input %||% "",
      caption = input$publication_caption_input %||% "",
      font_family = input$publication_font_family_input %||% "Arial",
      auto_crop_original_text = if (isTRUE(input$publication_auto_crop_original_text_input)) "true" else "false",
      hide_original_title = if (isTRUE(input$publication_hide_original_title_input)) "true" else "false",
      hide_original_x_axis = if (isTRUE(input$publication_hide_original_x_axis_input)) "true" else "false",
      hide_original_y_axis = if (isTRUE(input$publication_hide_original_y_axis_input)) "true" else "false",
      hide_original_x_ticks = if (isTRUE(input$publication_hide_original_x_ticks_input)) "true" else "false",
      hide_original_y_ticks = if (isTRUE(input$publication_hide_original_y_ticks_input)) "true" else "false",
      source_crop_top = as.character(input$publication_source_crop_top_input %||% 0),
      source_crop_bottom = as.character(input$publication_source_crop_bottom_input %||% 0),
      source_crop_left = as.character(input$publication_source_crop_left_input %||% 0),
      source_crop_right = as.character(input$publication_source_crop_right_input %||% 0),
      title_size = as.character(input$publication_title_size_input %||% 18),
      subtitle_size = as.character(input$publication_subtitle_size_input %||% 12),
      x_axis_size = as.character(input$publication_x_axis_size_input %||% 12),
      y_axis_size = as.character(input$publication_y_axis_size_input %||% 12),
      x_tick_size = as.character(input$publication_x_tick_size_input %||% 9),
      y_tick_size = as.character(input$publication_y_tick_size_input %||% 9),
      x_axis_angle = as.character(input$publication_x_axis_angle_input %||% 0),
      y_axis_angle = as.character(input$publication_y_axis_angle_input %||% 90),
      x_tick_angle = as.character(input$publication_x_tick_angle_input %||% 0),
      y_tick_angle = as.character(input$publication_y_tick_angle_input %||% 0),
      x_axis_distance = as.character(input$publication_x_axis_distance_input %||% 0.035),
      y_axis_distance = as.character(input$publication_y_axis_distance_input %||% 0.035),
      x_tick_distance = as.character(input$publication_x_tick_distance_input %||% 0.075),
      y_tick_distance = as.character(input$publication_y_tick_distance_input %||% 0.075),
      title_distance = as.character(input$publication_title_distance_input %||% 0.035),
      subtitle_distance = as.character(input$publication_subtitle_distance_input %||% 0.065),
      caption_distance = as.character(input$publication_caption_distance_input %||% 0.060),
      plot_margin = as.character(input$publication_plot_margin_input %||% 0.025),
      plot_margin_top = as.character(input$publication_plot_margin_top_input %||% 0),
      plot_margin_right = as.character(input$publication_plot_margin_right_input %||% 0),
      plot_margin_bottom = as.character(input$publication_plot_margin_bottom_input %||% 0),
      plot_margin_left = as.character(input$publication_plot_margin_left_input %||% 0),
      legend_size = as.character(input$publication_legend_size_input %||% 9),
      legend_x_nudge = as.character(input$publication_legend_x_nudge_input %||% 0),
      legend_y_nudge = as.character(input$publication_legend_y_nudge_input %||% 0),
      legend_symbol_type = input$publication_legend_symbol_type_input %||% "none",
      legend_colors = input$publication_legend_colors_input %||% "",
      caption_size = as.character(input$publication_caption_size_input %||% 10),
      caption_x_nudge = as.character(input$publication_caption_x_nudge_input %||% 0),
      caption_y_nudge = as.character(input$publication_caption_y_nudge_input %||% 0),
      italicize_title = if (isTRUE(input$publication_italic_title_input)) "true" else "false",
      italicize_subtitle = if (isTRUE(input$publication_italic_subtitle_input)) "true" else "false",
      italicize_x_axis = if (isTRUE(input$publication_italic_x_axis_input)) "true" else "false",
      italicize_y_axis = if (isTRUE(input$publication_italic_y_axis_input)) "true" else "false",
      italicize_x_ticks = if (isTRUE(input$publication_italic_x_ticks_input)) "true" else "false",
      italicize_y_ticks = if (isTRUE(input$publication_italic_y_ticks_input)) "true" else "false",
      bold_title = if (isTRUE(input$publication_bold_title_input)) "true" else "false",
      bold_subtitle = if (isTRUE(input$publication_bold_subtitle_input)) "true" else "false",
      bold_x_axis = if (isTRUE(input$publication_bold_x_axis_input)) "true" else "false",
      bold_y_axis = if (isTRUE(input$publication_bold_y_axis_input)) "true" else "false",
      bold_x_ticks = if (isTRUE(input$publication_bold_x_ticks_input)) "true" else "false",
      bold_y_ticks = if (isTRUE(input$publication_bold_y_ticks_input)) "true" else "false",
      notes = input$publication_notes_input %||% ""
    )
  }

  publication_preview_style <- shiny::debounce(reactive({
    list(
      title = input$publication_title_input %||% "",
      subtitle = input$publication_subtitle_input %||% "",
      x_label = input$publication_x_label_input %||% "",
      y_label = input$publication_y_label_input %||% "",
      x_tick_labels = input$publication_x_tick_labels_input %||% "",
      y_tick_labels = input$publication_y_tick_labels_input %||% "",
      legend_text = input$publication_legend_text_input %||% "",
      caption = input$publication_caption_input %||% "",
      font_family = input$publication_font_family_input %||% "Arial",
      auto_crop_original_text = if (isTRUE(input$publication_auto_crop_original_text_input)) "true" else "false",
      hide_original_title = if (isTRUE(input$publication_hide_original_title_input)) "true" else "false",
      hide_original_x_axis = if (isTRUE(input$publication_hide_original_x_axis_input)) "true" else "false",
      hide_original_y_axis = if (isTRUE(input$publication_hide_original_y_axis_input)) "true" else "false",
      hide_original_x_ticks = if (isTRUE(input$publication_hide_original_x_ticks_input)) "true" else "false",
      hide_original_y_ticks = if (isTRUE(input$publication_hide_original_y_ticks_input)) "true" else "false",
      source_crop_top = as.character(input$publication_source_crop_top_input %||% 0),
      source_crop_bottom = as.character(input$publication_source_crop_bottom_input %||% 0),
      source_crop_left = as.character(input$publication_source_crop_left_input %||% 0),
      source_crop_right = as.character(input$publication_source_crop_right_input %||% 0),
      title_size = as.character(input$publication_title_size_input %||% 18),
      subtitle_size = as.character(input$publication_subtitle_size_input %||% 12),
      x_axis_size = as.character(input$publication_x_axis_size_input %||% 12),
      y_axis_size = as.character(input$publication_y_axis_size_input %||% 12),
      x_tick_size = as.character(input$publication_x_tick_size_input %||% 9),
      y_tick_size = as.character(input$publication_y_tick_size_input %||% 9),
      x_axis_angle = as.character(input$publication_x_axis_angle_input %||% 0),
      y_axis_angle = as.character(input$publication_y_axis_angle_input %||% 90),
      x_tick_angle = as.character(input$publication_x_tick_angle_input %||% 0),
      y_tick_angle = as.character(input$publication_y_tick_angle_input %||% 0),
      x_axis_distance = as.character(input$publication_x_axis_distance_input %||% 0.035),
      y_axis_distance = as.character(input$publication_y_axis_distance_input %||% 0.035),
      x_tick_distance = as.character(input$publication_x_tick_distance_input %||% 0.075),
      y_tick_distance = as.character(input$publication_y_tick_distance_input %||% 0.075),
      title_distance = as.character(input$publication_title_distance_input %||% 0.035),
      subtitle_distance = as.character(input$publication_subtitle_distance_input %||% 0.065),
      caption_distance = as.character(input$publication_caption_distance_input %||% 0.060),
      plot_margin = as.character(input$publication_plot_margin_input %||% 0.025),
      plot_margin_top = as.character(input$publication_plot_margin_top_input %||% 0),
      plot_margin_right = as.character(input$publication_plot_margin_right_input %||% 0),
      plot_margin_bottom = as.character(input$publication_plot_margin_bottom_input %||% 0),
      plot_margin_left = as.character(input$publication_plot_margin_left_input %||% 0),
      legend_size = as.character(input$publication_legend_size_input %||% 9),
      legend_x_nudge = as.character(input$publication_legend_x_nudge_input %||% 0),
      legend_y_nudge = as.character(input$publication_legend_y_nudge_input %||% 0),
      legend_symbol_type = input$publication_legend_symbol_type_input %||% "none",
      legend_colors = input$publication_legend_colors_input %||% "",
      caption_size = as.character(input$publication_caption_size_input %||% 10),
      caption_x_nudge = as.character(input$publication_caption_x_nudge_input %||% 0),
      caption_y_nudge = as.character(input$publication_caption_y_nudge_input %||% 0),
      italicize_title = if (isTRUE(input$publication_italic_title_input)) "true" else "false",
      italicize_subtitle = if (isTRUE(input$publication_italic_subtitle_input)) "true" else "false",
      italicize_x_axis = if (isTRUE(input$publication_italic_x_axis_input)) "true" else "false",
      italicize_y_axis = if (isTRUE(input$publication_italic_y_axis_input)) "true" else "false",
      italicize_x_ticks = if (isTRUE(input$publication_italic_x_ticks_input)) "true" else "false",
      italicize_y_ticks = if (isTRUE(input$publication_italic_y_ticks_input)) "true" else "false",
      bold_title = if (isTRUE(input$publication_bold_title_input)) "true" else "false",
      bold_subtitle = if (isTRUE(input$publication_bold_subtitle_input)) "true" else "false",
      bold_x_axis = if (isTRUE(input$publication_bold_x_axis_input)) "true" else "false",
      bold_y_axis = if (isTRUE(input$publication_bold_y_axis_input)) "true" else "false",
      bold_x_ticks = if (isTRUE(input$publication_bold_x_ticks_input)) "true" else "false",
      bold_y_ticks = if (isTRUE(input$publication_bold_y_ticks_input)) "true" else "false"
    )
  }), millis = 450)

  publication_preview_key <- function(fig_dir, style) {
    tracked_files <- c("source_plot.png", "recorded_plot.rds", "style_metadata.csv", "plot_recipe.rds", "plot_data_snapshot.rds", "input_snapshot.csv")
    file_bits <- vapply(tracked_files, function(file_name) {
      path <- file.path(fig_dir, file_name)
      if (!file.exists(path)) return(paste(file_name, "missing", sep = ":"))
      info <- file.info(path)
      paste(file_name, as.numeric(info$mtime), info$size, sep = ":")
    }, character(1))
    paste(c(normalizePath(fig_dir, winslash = "/", mustWork = FALSE), unlist(style, use.names = TRUE), file_bits), collapse = "\n")
  }

  make_publication_placeholder_png <- function(message, file) {
    grDevices::png(file, width = 1200, height = 720, res = 120)
    on.exit(grDevices::dev.off(), add = TRUE)
    plot.new()
    text(0.5, 0.56, message, cex = 1.15)
    text(0.5, 0.42, "Saved figure candidates will appear here after selection.", cex = 0.85)
  }

  observeEvent(input$publication_save_style_btn, {
    fig_dir <- selected_publication_dir()
    if (!nzchar(fig_dir)) {
      showNotification("Select a saved publication figure first.", type = "warning")
      return()
    }
    style_list <- current_publication_style_from_inputs()
    write_publication_style(fig_dir, style_list)
    meta_path <- file.path(fig_dir, "figure_metadata.csv")
    meta <- tryCatch(utils::read.csv(meta_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
    if (is.data.frame(meta) && nrow(meta) && all(c("key", "value") %in% names(meta))) {
      base_meta <- meta[!grepl("^(shared_tab4_filtering_and_subsampling|plot_data_mapping_and_aesthetics|publication_style_and_overlay_aesthetics|data_subset)$", meta$section %||% ""), c("key", "value"), drop = FALSE]
      snapshot <- tryCatch(utils::read.csv(file.path(fig_dir, "input_snapshot.csv"), stringsAsFactors = FALSE, check.names = FALSE), error = function(e) publication_snapshot_inputs())
      style_df <- data.frame(key = names(style_list), value = unlist(style_list, use.names = FALSE), stringsAsFactors = FALSE)
      context <- publication_context_metadata(snapshot, style = style_df, object_id = publication_candidate_plot_id(fig_dir))
      write_key_value_metadata(meta_path, base_meta, context)
    }
    publication_tick(publication_tick() + 1)
    showNotification("Publication figure metadata saved.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$publication_select_all_delete_btn, {
    choices <- publication_candidate_delete_choices()
    if (!length(choices)) {
      showNotification("No saved publication figure candidates are available to select.", type = "warning")
      return()
    }
    updateSelectizeInput(session, "publication_figures_to_delete", selected = unname(choices))
  }, ignoreInit = TRUE)

  observeEvent(input$publication_delete_selected_btn, {
    selected <- input$publication_figures_to_delete %||% character()
    selected <- unique(selected[nzchar(selected)])
    if (!length(selected)) {
      showNotification("Choose one or more saved publication figure candidates to delete first.", type = "warning")
      return()
    }
    showModal(modalDialog(
      title = "Delete saved publication figure candidates?",
      tags$p(sprintf("This will delete %s saved publication figure candidate folder(s) from cache/publication_figures.", length(selected))),
      tags$p("This does not delete raw images, scripts, pipeline outputs, saved publication tables, validation profiles, or reports."),
      tags$ul(lapply(utils::head(selected, 8), tags$li)),
      if (length(selected) > 8) tags$p(sprintf("... and %s more.", length(selected) - 8)),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("publication_confirm_delete_selected_btn", "Delete Selected", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  }, ignoreInit = TRUE)

  observeEvent(input$publication_confirm_delete_selected_btn, {
    selected <- input$publication_figures_to_delete %||% character()
    selected <- unique(selected[nzchar(selected)])
    removeModal()
    if (!length(selected)) {
      showNotification("No publication figure candidates were selected for deletion.", type = "warning")
      return()
    }
    paths <- vapply(selected, publication_candidate_dir_for_id, character(1), USE.NAMES = FALSE)
    paths <- paths[nzchar(paths)]
    if (!length(paths)) {
      showNotification("No valid saved publication figure candidate folders were found for the current selection.", type = "warning", duration = 8)
      return()
    }
    deleted <- 0L
    for (path in paths) {
      before_exists <- dir.exists(path)
      if (before_exists) {
        delete_path_safely(path)
        if (!dir.exists(path)) deleted <- deleted + 1L
      }
    }
    publication_preview_cache$key <- ""
    publication_preview_cache$path <- ""
    publication_tick(publication_tick() + 1)
    choices <- publication_candidate_choices()
    next_selected <- unname(choices)[[1]] %||% ""
    try(updateSelectInput(session, "publication_figure_choice", choices = choices, selected = next_selected), silent = TRUE)
    try(updateSelectizeInput(session, "publication_figures_to_delete", choices = publication_candidate_delete_choices(), selected = character()), silent = TRUE)
    showNotification(sprintf("Deleted %s saved publication figure candidate(s).", deleted), type = "message", duration = 8)
  }, ignoreInit = TRUE)

  output$publication_figure_preview_plot <- renderImage({
    publication_tick()
    fig_dir <- selected_publication_dir()
    style <- publication_preview_style()
    if (!nzchar(fig_dir)) {
      tmp <- file.path(publication_preview_dir, "no_figure_selected.png")
      if (!file.exists(tmp)) make_publication_placeholder_png("No saved publication figure selected.", tmp)
      return(list(src = tmp, contentType = "image/png", alt = "No saved publication figure selected", width = "100%", height = "auto"))
    }
    key <- publication_preview_key(fig_dir, style)
    if (identical(publication_preview_cache$key, key) && nzchar(publication_preview_cache$path) && file.exists(publication_preview_cache$path)) {
      return(list(src = publication_preview_cache$path, contentType = "image/png", alt = paste("Publication figure preview", basename(fig_dir)), width = "100%", height = "auto"))
    }
    tmp <- file.path(publication_preview_dir, paste0("preview_", format(Sys.time(), "%Y%m%d%H%M%OS3"), "_", sample.int(1000000, 1), ".png"))
    render_publication_figure_file_safe(fig_dir, tmp, width = 1500, height = 1050, res = 120, style_override = style)
    old_path <- publication_preview_cache$path
    publication_preview_cache$key <- key
    publication_preview_cache$path <- tmp
    if (nzchar(old_path) && !identical(old_path, tmp) && dirname(old_path) == publication_preview_dir) {
      unlink(old_path, force = TRUE)
    }
    list(src = tmp, contentType = "image/png", alt = paste("Publication figure preview", basename(fig_dir)), width = "100%", height = "auto")
  }, deleteFile = FALSE)

  output$publication_input_snapshot_table <- renderTable({
    fig_dir <- selected_publication_dir()
    if (!nzchar(fig_dir)) return(data.frame(Message = "Select a saved publication figure."))
    path <- file.path(fig_dir, "input_snapshot.csv")
    if (!file.exists(path)) return(data.frame(Message = "No input snapshot was saved for this figure."))
    head(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), 30)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$publication_table_selector_ui <- renderUI({
    choices <- publication_table_choices()
    current <- input$publication_table_choice %||% ""
    if (!current %in% unname(choices)) current <- unname(choices)[[1]] %||% ""
    selectInput("publication_table_choice", "Saved publication table", choices = choices, selected = current)
  })

  output$publication_tables_table <- renderTable({
    apply_table_value_filter("publication_tables_table", publication_tables_table())
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$publication_table_preview <- renderTable({
    table_dir <- selected_publication_table_dir()
    if (!nzchar(table_dir)) return(data.frame(Message = "Select a saved publication table."))
    path <- file.path(table_dir, "table.csv")
    if (!file.exists(path)) return(data.frame(Message = "Saved table CSV is missing."))
    head(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), 30)
  }, striped = TRUE, bordered = TRUE, spacing = "xs")

  output$download_publication_table_btn <- downloadHandler(
    filename = function() {
      table_dir <- selected_publication_table_dir()
      table_id <- if (nzchar(table_dir)) basename(table_dir) else "publication_table"
      paste0(table_id, ".csv")
    },
    content = function(file) {
      table_dir <- selected_publication_table_dir()
      validate(need(nzchar(table_dir), "Select a saved publication table first."))
      src <- file.path(table_dir, "table.csv")
      validate(need(file.exists(src), "Saved table CSV is missing."))
      file.copy(src, file, overwrite = TRUE)
    }
  )

  output$download_publication_png_btn <- downloadHandler(
    filename = function() {
      fig_dir <- selected_publication_dir()
      fig_id <- if (nzchar(fig_dir)) basename(fig_dir) else "publication_figure"
      paste0(fig_id, "_edited_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      fig_dir <- selected_publication_dir()
      validate(need(nzchar(fig_dir), "Select a saved publication figure first."))
      write_publication_style(fig_dir, current_publication_style_from_inputs())
      render_publication_figure_file_safe(fig_dir, file, width = 4200, height = 3000, res = 300)
    }
  )

  output$download_publication_metadata_btn <- downloadHandler(
    filename = function() {
      fig_dir <- selected_publication_dir()
      fig_id <- if (nzchar(fig_dir)) basename(fig_dir) else "publication_figure"
      paste0(fig_id, "_metadata.csv")
    },
    content = function(file) {
      fig_dir <- selected_publication_dir()
      validate(need(nzchar(fig_dir), "Select a saved publication figure first."))
      write_publication_style(fig_dir, current_publication_style_from_inputs())
      meta <- tryCatch(utils::read.csv(file.path(fig_dir, "figure_metadata.csv"), stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      style <- tryCatch(utils::read.csv(file.path(fig_dir, "style_metadata.csv"), stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      snapshot <- tryCatch(utils::read.csv(file.path(fig_dir, "input_snapshot.csv"), stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
      if (is.data.frame(meta) && nrow(meta) && !"section" %in% names(meta)) meta$section <- "figure"
      if (is.data.frame(meta) && nrow(meta)) meta <- meta[, c("section", "key", "value"), drop = FALSE]
      if (!is.data.frame(meta) || !nrow(meta)) meta <- data.frame(section = character(), key = character(), value = character())
      style_out <- if (is.data.frame(style) && nrow(style) && all(c("key", "value") %in% names(style))) {
        data.frame(section = "current_saved_style", key = paste0("style_", style$key), value = style$value, stringsAsFactors = FALSE)
      } else {
        data.frame(section = character(), key = character(), value = character())
      }
      snapshot_out <- if (is.data.frame(snapshot) && nrow(snapshot) && all(c("input_id", "value") %in% names(snapshot))) {
        data.frame(section = "saved_input_snapshot", key = snapshot$input_id, value = snapshot$value, stringsAsFactors = FALSE)
      } else {
        data.frame(section = character(), key = character(), value = character())
      }
      out <- rbind(meta, style_out, snapshot_out)
      utils::write.csv(publication_human_readable_metadata(out), file, row.names = FALSE)
    }
  )

  install_filterable_table("publication_candidates_table", publication_candidates_table, "publication_figure_candidates")
  install_filterable_table("publication_tables_table", publication_tables_table, "publication_table_candidates")

  try(outputOptions(output, "validation_workspace_ui", suspendWhenHidden = FALSE), silent = TRUE)
  try(outputOptions(output, "generalization_phase_ui", suspendWhenHidden = FALSE), silent = TRUE)
  try(outputOptions(output, "visualization_phase_ui", suspendWhenHidden = FALSE), silent = TRUE)

  output$manual_current_image_info <- renderUI({
    img <- current_validation_image()
    group_dir <- current_validation_group_dir()
    labels <- manual_labels_for_group(group_dir, settings())
    if (is.null(img)) {
      return(tags$p("No image is currently available in this group."))
    }
    img_name <- basename(img)
    current_label <- if (img_name %in% labels$choice) {
      "choice"
    } else if (img_name %in% labels$noise) {
      "noise"
    } else if (img_name %in% labels$not_sure) {
      "not so certain"
    } else {
      "not reviewed yet"
    }
    tags$div(
      tags$p(tags$strong("Current image: "), tags$span(class = "path-block", img_name)),
      tags$p(tags$strong("Group folder: "), tags$span(class = "path-block", group_dir)),
      tags$p(tags$strong("Images in this group: "), length(current_validation_images())),
      tags$p(class = "manual-current-label", tags$strong("Current label: "), current_label)
    )
  })

  output$manual_image_note_ui <- renderUI({
    img <- current_validation_image()
    if (is.null(img)) {
      return(tags$p("No image selected for preview."))
    }
    view <- current_manual_view()
    if (nzchar(view$note %||% "")) {
      return(tagList(
        tags$p(class = "warning-text", view$note),
        tags$p(tags$strong("Image file: "), tags$span(class = "path-block", basename(img)))
      ))
    }
    NULL
  })

  output$manual_image_view <- renderImage({
    img <- current_validation_image()
    req(img)
    zoom <- suppressWarnings(as.integer(input$manual_zoom_percent %||% 100L))
    view <- current_manual_view()
    req(!is.null(view))
    req(file.exists(view$src))
    list(
      src = view$src,
      contentType = view$content_type,
      width = paste0(zoom, "%"),
      alt = basename(img)
    )
  }, deleteFile = FALSE)

  output$manual_side_summary <- renderUI({
    manual_review_tick()
    group_dir <- current_validation_group_dir()
    labels <- manual_labels_for_group(group_dir, settings())
    overall <- manual_label_summary_all_groups(settings())
    current_group_id <- basename(group_dir)
    current_row <- overall[overall$group_id == current_group_id, , drop = FALSE]
    counts <- c(
      choice = length(labels$choice),
      noise = length(labels$noise),
      not_sure = length(labels$not_sure)
    )
    tagList(
      tags$h5("Overall recap across all groups"),
      tags$table(
        class = "table table-condensed table-striped",
        tags$thead(
          tags$tr(
            tags$th("Group"),
            tags$th("Choice"),
            tags$th("Noise"),
            tags$th("Not sure"),
            tags$th("Unlabeled")
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(overall)), function(i) {
            row <- overall[i, , drop = FALSE]
            tags$tr(
              tags$td(row$group_label[[1]]),
              tags$td(as.integer(row$choice[[1]])),
              tags$td(as.integer(row$noise[[1]])),
              tags$td(as.integer(row$not_sure[[1]])),
              tags$td(as.integer(row$unlabeled[[1]]))
            )
          })
        )
      ),
      if (nrow(current_row)) tags$p(tags$strong("Current group recap: "), current_row$group_label[[1]]),
      tags$p(tags$strong("Choice: "), counts[["choice"]]),
      tags$div(class = "manual-list-box", if (length(labels$choice)) tags$ul(lapply(labels$choice, function(x) tags$li(tags$span(class = "path-block", x)))) else tags$p("No images marked as choice yet.")),
      tags$p(tags$strong("Noise: "), counts[["noise"]]),
      tags$div(class = "manual-list-box", if (length(labels$noise)) tags$ul(lapply(labels$noise, function(x) tags$li(tags$span(class = "path-block", x)))) else tags$p("No images marked as noise yet.")),
      tags$p(tags$strong("Not so certain: "), counts[["not_sure"]]),
      tags$div(class = "manual-list-box", if (length(labels$not_sure)) tags$ul(lapply(labels$not_sure, function(x) tags$li(tags$span(class = "path-block", x)))) else tags$p("No images marked as not so certain yet."))
    )
  })

  output$manual_recap_table <- renderTable({
    manual_label_summary_all_groups(settings())
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  step_card_is_visible <- function(step_id) {
    if (isTRUE(runner$active) && !is.null(runner$step) && identical(runner$step$id, step_id)) return(TRUE)
    if (isTRUE(validation_runner$active) && step_id %in% validation_post_manual_step_ids()) return(TRUE)

    workplace <- input$workplace_tabs %||% workplace_tab_selected()
    if (identical(workplace, "2. Validate New / Use Prior Cutoffs Model")) {
      mode <- input$validation_mode_tabs %||% validation_mode_selected()
      if (!identical(mode, "2.A. Validate New Cutoffs")) return(FALSE)
      phase <- input$validation_phase_tabs %||% validation_phase_selected()
      if (identical(phase, "2.A.1. Setup and Preprocessing")) return(step_id %in% setup_module_step_ids())
      if (identical(phase, "2.A.3. Create Validation Groups")) return(identical(step_id, "groups"))
      if (identical(phase, "2.A.5. Apply Manual Labels and Package ROIs")) return(step_id %in% c("writeback", "roi_extract", "package_rois"))
      if (identical(phase, "2.A.6. Threshold Optimization")) return(identical(step_id, "optimize"))
      return(FALSE)
    }

    current_step <- find_step_by_id(current_steps(), step_id)
    if (is.null(current_step)) return(FALSE)
    if (identical(workplace, "3. Analyze Test Images")) return(identical(current_step$phase, "Generalization with New Images"))
    if (identical(workplace, "4. Visualization and Interpretation")) return(identical(current_step$phase, "Visualization and Interpretation"))
    FALSE
  }

  observe({
    for (step_id in all_step_ids) {
      local({
        id <- step_id
        output[[paste0("status_", id)]] <- renderUI({
          if (!step_card_is_visible(id)) return(NULL)
          if (step_needs_module_refresh(id, runner, validation_runner)) invalidateLater(2000, session)
          current_step <- find_step_by_id(current_steps(), id)
          if (is.null(current_step)) return(NULL)
          st <- status_for_step(current_step, settings())
          tags$div(
            class = paste("step-status-pill", st$class),
            tags$span(class = "step-status-icon", st$icon),
            tags$div(
              tags$div(st$label),
              tags$div(style = "font-weight:500; font-size:12px;", sprintf("%s/%s %s", st$detected, st$total, st$unit_label %||% "outputs detected"))
              )
            )
          })
        output[[paste0("inline_readiness_", id)]] <- renderUI({
          if (!step_card_is_visible(id)) return(NULL)
          if (step_needs_module_refresh(id, runner, validation_runner)) invalidateLater(2000, session)
          current_step <- find_step_by_id(current_steps(), id)
          if (is.null(current_step)) return(NULL)
          build_step_inline_readiness_ui(current_step, history_df())
        })
        output[[paste0("step_summary_", id)]] <- renderUI({
          if (!step_card_is_visible(id)) return(NULL)
          if (step_needs_module_refresh(id, runner, validation_runner)) invalidateLater(2000, session)
          current_step <- find_step_by_id(current_steps(), id)
          if (is.null(current_step)) return(NULL)
          build_step_summary_ui(current_step, history_df())
        })
        output[[paste0("preview_", gsub("[^A-Za-z0-9]+", "_", id))]] <- renderTable({
          if (!step_card_is_visible(id)) return(NULL)
          if (step_needs_module_refresh(id, runner, validation_runner)) invalidateLater(2000, session)
          current_step <- find_step_by_id(current_steps(), id)
          if (is.null(current_step)) return(NULL)
          outputs <- unique(unlist(current_step$outputs))
          outputs <- outputs[nzchar(outputs)]
          valid_outputs <- outputs[file.exists(outputs) & !dir.exists(outputs)]
          if (!length(valid_outputs)) return(NULL)
          preview_target <- valid_outputs[1]
          ext <- tolower(tools::file_ext(preview_target))
          if (!ext %in% c("csv", "tsv")) return(NULL)
          tryCatch(
            if (ext == "tsv") utils::read.table(preview_target, sep = "\t", header = TRUE, nrows = 6, stringsAsFactors = FALSE, check.names = FALSE)
            else utils::read.csv(preview_target, nrows = 6, stringsAsFactors = FALSE, check.names = FALSE),
            error = function(e) data.frame(message = "Preview unavailable for this table file.")
          )
        }, striped = TRUE, bordered = TRUE, spacing = "xs")
        output[[paste0("report_preview_", gsub("[^A-Za-z0-9]+", "_", id))]] <- renderTable({
          if (!step_card_is_visible(id)) return(NULL)
          if (step_needs_module_refresh(id, runner, validation_runner)) invalidateLater(2000, session)
          current_step <- find_step_by_id(current_steps(), id)
          if (is.null(current_step)) return(NULL)
          report_paths <- step_report_files(current_step$id, settings())
          report_paths <- report_paths[file.exists(report_paths) & !dir.exists(report_paths)]
          if (!length(report_paths)) return(NULL)
          report_target <- report_paths[1]
          ext <- tolower(tools::file_ext(report_target))
          if (!ext %in% c("csv", "tsv")) return(NULL)
          tryCatch(
            if (ext == "tsv") utils::read.table(report_target, sep = "\t", header = TRUE, nrows = 8, stringsAsFactors = FALSE, check.names = FALSE)
            else utils::read.csv(report_target, nrows = 8, stringsAsFactors = FALSE, check.names = FALSE),
            error = function(e) data.frame(message = "Preview unavailable for this report file.")
          )
        }, striped = TRUE, bordered = TRUE, spacing = "xs")
      })
    }
  })

  for (step_id in setdiff(all_step_ids, "manual")) {
    local({
      id <- step_id
      observeEvent(input[[paste0("run_", id)]], {
        if (identical(id, "optimize")) {
          persist_optimization_scoring_mode(announce = TRUE)
        }
        current_step <- Filter(function(x) identical(x$id, id), step_specs(settings(), input))[[1]]
        prereq_issues <- step_prerequisite_issues(current_step, settings())
        if (length(prereq_issues)) {
          show_prerequisite_modal(current_step, prereq_issues)
          return()
        }
        integrity <- step_integrity_check(current_step, settings())
        if (identical(integrity$state, "partial")) {
          pending_step(list(step = current_step, runtime = input$runtime_choice, integrity = integrity))
          showModal(modalDialog(
            title = paste("Partial output detected for", current_step$title),
            easyClose = TRUE,
            modal_top_close(),
            tags$p(integrity$summary),
            if (length(integrity$details)) tags$ul(lapply(integrity$details, tags$li)),
            if (length(integrity$dropouts)) tags$p(sprintf("%s dropout item(s) are currently detected.", length(integrity$dropouts))),
            tags$p("Choose whether to continue from the current partial state, retry only the missing items when supported, or delete this step's outputs and regenerate them fresh."),
            footer = tagList(
              actionButton("partial_continue_btn", "Continue with existing partial outputs"),
              if (supports_dropout_retry(current_step$id, integrity)) actionButton("partial_retry_btn", "Retry only the detected dropouts", class = "btn-primary"),
              actionButton("partial_regenerate_btn", "Delete outputs and regenerate fresh", class = "btn-danger")
            )
          ))
        } else {
          run_step_now(current_step, input$runtime_choice)
        }
      }, ignoreInit = TRUE)

      observeEvent(input[[paste0("retry_dropouts_", gsub("[^A-Za-z0-9]+", "_", id))]], {
        current_step <- Filter(function(x) identical(x$id, id), step_specs(settings(), input))[[1]]
        integrity <- step_integrity_check(current_step, settings())
        retry_step <- build_dropout_retry_step(current_step, integrity, input$runtime_choice)
        if (is.null(retry_step)) {
          showNotification("This step does not currently support retrying only the missing items.", type = "warning")
        } else {
          run_step_now(retry_step, input$runtime_choice)
        }
      }, ignoreInit = TRUE)
    })
  }

  observeEvent(input$clear_log_btn, log_lines(character()))

  output$log_output <- renderText({
    invalidateLater(1000, session)
    if (runner$active && !is.null(runner$log_file) && file.exists(runner$log_file)) {
      file_text <- tryCatch(paste(utils::tail(readLines(runner$log_file, warn = FALSE), 200), collapse = "\n"), error = function(e) "")
      prefix <- paste(log_lines(), collapse = "\n")
      trimws(paste(prefix, file_text, sep = if (nzchar(prefix) && nzchar(file_text)) "\n" else ""))
    } else {
      paste(log_lines(), collapse = "\n")
    }
  })

  output$download_history_btn <- downloadHandler(
    filename = function() paste0("command_history_", format(Sys.Date()), ".csv"),
    content = function(file) {
      hist <- history_df()
      write.csv(hist, file, row.names = FALSE)
    }
  )

  output$download_live_log_btn <- downloadHandler(
    filename = function() paste0("current_run_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt"),
    content = function(file) {
      if (runner$active && !is.null(runner$log_file) && file.exists(runner$log_file)) {
        file.copy(runner$log_file, file, overwrite = TRUE)
      } else {
        writeLines(log_lines(), file)
      }
    }
  )

  output$download_saved_log_btn <- downloadHandler(
    filename = function() {
      paste0("saved_run_log_", format(Sys.Date()), ".txt")
    },
    content = function(file) {
      req(nzchar(input$saved_log_choice))
      file.copy(input$saved_log_choice, file, overwrite = TRUE)
    }
  )
}

app <- shinyApp(ui, server)

if (identical(environment(), globalenv()) && !interactive()) {
  runApp(app, host = "127.0.0.1", port = 3838, launch.browser = TRUE)
}
