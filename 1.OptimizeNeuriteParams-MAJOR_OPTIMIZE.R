# ROI Parameter Optimization for Neurite Skeleton Analysis
# Continuation script - assumes optROIs is already loaded

library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(RImageJROI)

args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)
source(file.path(base_dir, "launchers", "pipeline_paths.R"))
ctx <- pipeline_context(base_dir)
dir.create(ctx$paths$out_opt, recursive = TRUE, showWarnings = FALSE)

report_txt <- file.path(ctx$paths$out_opt, "optimization_detailed_report.txt")
report_csv <- file.path(ctx$paths$out_opt, "optimization_input_diagnostics.csv")
if (file.exists(report_txt)) file.remove(report_txt)

report_line <- function(...) {
  msg <- paste(..., collapse = "")
  cat(msg, "\n")
  write(msg, file = report_txt, append = TRUE)
  flush.console()
}

stage_line <- function(stage, status, detail = "") {
  suffix <- if (nzchar(detail)) paste0(" - ", detail) else ""
  report_line(sprintf("[OPTIMIZATION %s] %s%s", status, stage, suffix))
}

format_elapsed <- function(seconds) {
  seconds <- as.numeric(seconds)
  if (!is.finite(seconds)) return("unknown")
  if (seconds < 60) return(sprintf("%.1fs", seconds))
  sprintf("%dm %.1fs", floor(seconds / 60), seconds %% 60)
}

write_input_diag <- function(status, note, extra = list()) {
  roi_root <- file.path(ctx$paths$out_opt, "ROIs")
  inspected_csv_path <- ctx$paths$optim_groups_inspected %||% file.path(ctx$paths$out_opt, "mask_area_percentages+optim_groups+inspected.csv")
  opt_summary_path <- file.path(ctx$paths$out_opt, "optROIs_summary.csv")
  opt_summary <- if (file.exists(opt_summary_path)) {
    tryCatch(read.csv(opt_summary_path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  } else {
    data.frame()
  }
  roi_zip_count <- if (dir.exists(roi_root)) {
    roi_dirs <- list.dirs(roi_root, recursive = FALSE, full.names = TRUE)
    candidate_zips <- file.path(roi_dirs, paste0(basename(roi_dirs), ".tif_ROIs.zip"))
    sum(file.exists(candidate_zips))
  } else {
    0L
  }
  roi_file_count <- if (nrow(opt_summary) && "roi_count" %in% names(opt_summary)) {
    sum(suppressWarnings(as.numeric(opt_summary$roi_count)), na.rm = TRUE)
  } else {
    NA_real_
  }
  base <- data.frame(
    status = status,
    note = note,
    opt_rds_path = ctx$paths$opt_rois_rds,
    opt_rds_exists = file.exists(ctx$paths$opt_rois_rds),
    opt_rds_bytes = if (file.exists(ctx$paths$opt_rois_rds)) file.info(ctx$paths$opt_rois_rds)$size else NA_real_,
    roi_root = roi_root,
    roi_file_count = roi_file_count,
    roi_zip_count = roi_zip_count,
    inspected_csv = inspected_csv_path,
    inspected_exists = file.exists(inspected_csv_path),
    inspected_rows = if (file.exists(inspected_csv_path)) nrow(tryCatch(read.csv(inspected_csv_path, stringsAsFactors = FALSE), error = function(e) data.frame())) else 0L,
    stringsAsFactors = FALSE
  )
  for (nm in names(extra)) base[[nm]] <- extra[[nm]]
  write.csv(base, report_csv, row.names = FALSE, na = "")
}

report_line("=== OPTIMIZATION SCRIPT REPORT ===")
report_line("Base directory: ", base_dir)
report_line("Packaged ROI RDS path: ", ctx$paths$opt_rois_rds)
report_line("Optimization report text: ", report_txt)
report_line("Optimization input diagnostics CSV: ", report_csv)
stage_line("2.A.6. Optimize Neurite Thresholds", "START", "loading packaged ROI data and preparing checkpoint logic")

# Load optROIs if not already in environment
if (!exists("optROIs")) {
  optROIs <- readRDS(ctx$paths$opt_rois_rds)
}

# Function to extract ROI properties
extract_roi_properties <- function(roi_data) {
  properties <- list(
    area = NA,
    perimeter = NA,
    circularity = NA,
    aspect_ratio = NA,
    solidity = NA,
    mean_width = NA
  )
  
  tryCatch({
    # Get coordinates
    if (!is.null(roi_data$coords)) {
      coords <- roi_data$coords
      n_points <- nrow(coords)
      
      # Calculate area (shoelace formula)
      if (n_points >= 3) {
        x <- coords[, 1]
        y <- coords[, 2]
        area <- 0.5 * abs(sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y))
        properties$area <- area
        
        # Calculate perimeter
        dx <- diff(c(x, x[1]))
        dy <- diff(c(y, y[1]))
        perimeter <- sum(sqrt(dx^2 + dy^2))
        properties$perimeter <- perimeter
        
        # Calculate circularity: 4π * area / perimeter²
        if (perimeter > 0) {
          properties$circularity <- (4 * pi * area) / (perimeter^2)
        }
        
        # Calculate bounding box for aspect ratio
        width <- max(x) - min(x)
        height <- max(y) - min(y)
        if (height > 0) {
          properties$aspect_ratio <- width / height
        }
        properties$mean_width <- mean(c(width, height))
        
        # Estimate solidity (area / convex hull area)
        # Simplified: use bounding box as approximation
        if (width > 0 && height > 0) {
          bbox_area <- width * height
          properties$solidity <- area / bbox_area
        }
      }
    }
  }, error = function(e) {
    warning("Error extracting ROI properties: ", e$message)
  })
  
  return(properties)
}

# Function to calculate skeleton metrics for an ROI
calculate_skeleton_metrics <- function(roi_data) {
  metrics <- list(
    total_length = 0,
    branch_points = 0,
    endpoints = 0,
    avg_branch_length = 0,
    path_straightness = NA_real_,
    orientation_coherence = NA_real_
  )
  
  tryCatch({
    if (!is.null(roi_data$coords)) {
      coords <- roi_data$coords
      n_points <- nrow(coords)
      
      # Simple skeleton approximation: total path length
      if (n_points >= 2) {
        dx <- diff(coords[, 1])
        dy <- diff(coords[, 2])
        segment_lengths <- sqrt(dx^2 + dy^2)
        metrics$total_length <- sum(segment_lengths)
        metrics$endpoints <- 2  # Start and end
        metrics$avg_branch_length <- mean(segment_lengths)
        displacement <- sqrt((coords[n_points, 1] - coords[1, 1])^2 + (coords[n_points, 2] - coords[1, 2])^2)
        metrics$path_straightness <- displacement / (metrics$total_length + 1)
        angles <- atan2(dy, dx)
        if (length(angles)) {
          metrics$orientation_coherence <- sqrt(mean(cos(2 * angles), na.rm = TRUE)^2 + mean(sin(2 * angles), na.rm = TRUE)^2)
        }
      }
    }
  }, error = function(e) {
    warning("Error calculating skeleton metrics: ", e$message)
  })
  
  return(metrics)
}

add_topology_proxy_metrics <- function(df) {
  numeric_cols <- c(
    "area", "perimeter", "circularity", "aspect_ratio", "solidity", "mean_width",
    "skeleton_length", "branch_points", "endpoints", "avg_branch_length",
    "path_straightness", "orientation_coherence"
  )
  for (col in numeric_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_real_
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  df$path_straightness <- ifelse(
    is.finite(df$path_straightness),
    pmin(1, pmax(0, df$path_straightness)),
    pmin(1, pmax(0, df$aspect_ratio / (df$aspect_ratio + 1)))
  )
  df$orientation_coherence <- ifelse(
    is.finite(df$orientation_coherence),
    pmin(1, pmax(0, df$orientation_coherence)),
    df$path_straightness
  )
  df$branch_density <- df$branch_points / (df$skeleton_length + 1)
  df$endpoint_density <- df$endpoints / (df$skeleton_length + 1)
  df$area_length_ratio <- df$area / (df$skeleton_length + 1)
  df$perimeter_length_ratio <- df$perimeter / (df$skeleton_length + 1)
  df$dominant_path_ratio <- pmin(
    1,
    pmax(
      0,
      0.45 * df$path_straightness +
        0.35 * df$orientation_coherence +
        0.20 * pmin(1, df$skeleton_length / (df$perimeter + 1))
    )
  )
  df$meshness_score <- log1p(pmax(0, df$area_length_ratio)) +
    log1p(pmax(0, df$perimeter_length_ratio)) +
    log1p(pmax(0, df$branch_density) * 100) +
    log1p(pmax(0, df$endpoint_density - 0.1) * 100) +
    0.35 * pmax(0, df$circularity %||% 0) +
    0.25 * pmax(0, df$solidity %||% 0) -
    0.50 * pmax(0, df$dominant_path_ratio) -
    0.25 * log1p(pmax(0, df$aspect_ratio))
  df$meshness_score[!is.finite(df$meshness_score)] <- NA_real_
  df
}

total_images_to_process <- length(optROIs)
expected_total_rois <- sum(vapply(optROIs, function(x) length(x$rois), numeric(1)), na.rm = TRUE)
roi_analysis_csv <- file.path(ctx$paths$out_opt, "roi_analysis_raw.csv")
roi_analysis_rds <- file.path(ctx$paths$out_opt, "roi_analysis_raw.rds")
roi_analysis_manifest <- file.path(ctx$paths$out_opt, "roi_analysis_raw_manifest.csv")

opt_info <- file.info(ctx$paths$opt_rois_rds)
opt_rds_size <- if (file.exists(ctx$paths$opt_rois_rds)) opt_info$size else NA_real_
opt_rds_mtime <- if (file.exists(ctx$paths$opt_rois_rds)) as.numeric(opt_info$mtime) else NA_real_

write_roi_analysis_cache <- function(roi_analysis) {
  report_line("Saving reusable ROI analysis checkpoint before grid optimization...")
  write_csv(roi_analysis, roi_analysis_csv)
  saveRDS(roi_analysis, roi_analysis_rds)
  write.csv(
    data.frame(
      status = "ready",
      created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      opt_rds_path = ctx$paths$opt_rois_rds,
      opt_rds_size = opt_rds_size,
      opt_rds_mtime = opt_rds_mtime,
      expected_images = total_images_to_process,
      expected_roi_objects = expected_total_rois,
      roi_analysis_rows = nrow(roi_analysis),
      signal_rows = sum(roi_analysis$is_signal),
      noise_rows = sum(roi_analysis$is_noise),
      stringsAsFactors = FALSE
    ),
    roi_analysis_manifest,
    row.names = FALSE,
    na = ""
  )
  report_line("Reusable ROI analysis checkpoint saved to: ", roi_analysis_csv)
}

read_roi_analysis_cache <- function() {
  if (!file.exists(roi_analysis_manifest)) return(NULL)
  if (!file.exists(roi_analysis_rds) && !file.exists(roi_analysis_csv)) return(NULL)
  manifest <- tryCatch(read.csv(roi_analysis_manifest, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) data.frame())
  if (!nrow(manifest)) return(NULL)
  manifest <- manifest[1, , drop = FALSE]
  manifest_path <- normalizePath(manifest$opt_rds_path %||% "", winslash = "/", mustWork = FALSE)
  current_path <- normalizePath(ctx$paths$opt_rois_rds, winslash = "/", mustWork = FALSE)
  manifest_size <- suppressWarnings(as.numeric(manifest$opt_rds_size))
  manifest_mtime <- suppressWarnings(as.numeric(manifest$opt_rds_mtime))
  manifest_images <- suppressWarnings(as.numeric(manifest$expected_images))
  manifest_rois <- suppressWarnings(as.numeric(manifest$expected_roi_objects))
  path_matches <- identical(tolower(manifest_path), tolower(current_path))
  size_matches <- is.finite(manifest_size) && is.finite(opt_rds_size) && manifest_size == opt_rds_size
  images_match <- is.finite(manifest_images) && manifest_images == total_images_to_process
  rois_match <- is.finite(manifest_rois) && manifest_rois == expected_total_rois
  same_source <- isTRUE(
    path_matches &&
      size_matches &&
      images_match &&
      rois_match
  )
  if (!same_source) {
    report_line("Existing ROI analysis checkpoint was found but does not match the current optROIs_data.rds, so it will be rebuilt.")
    report_line(
      sprintf(
        "Checkpoint compatibility details: path_matches=%s, size_matches=%s, images_match=%s, rois_match=%s.",
        path_matches,
        size_matches,
        images_match,
        rois_match
      )
    )
    return(NULL)
  }
  if (is.finite(manifest_mtime) && is.finite(opt_rds_mtime) && abs(manifest_mtime - opt_rds_mtime) >= 1) {
    report_line(
      sprintf(
        "Existing ROI analysis checkpoint matches by path, file size, image count, and ROI count. The optROIs_data.rds timestamp changed by %.1f seconds, but this is treated as safe because the ROI universe still matches.",
        abs(manifest_mtime - opt_rds_mtime)
      )
    )
  }
  cached <- if (file.exists(roi_analysis_rds)) {
    tryCatch(readRDS(roi_analysis_rds), error = function(e) NULL)
  } else {
    NULL
  }
  if (is.null(cached) && file.exists(roi_analysis_csv)) {
    cached <- tryCatch(read_csv(roi_analysis_csv, show_col_types = FALSE), error = function(e) NULL)
  }
  required_cols <- c("image_name", "roi_name", "area", "circularity", "aspect_ratio", "skeleton_length", "is_signal", "is_noise")
  if (is.null(cached) || !nrow(cached) || !all(required_cols %in% names(cached))) {
    report_line("Existing ROI analysis checkpoint is incomplete or unreadable, so it will be rebuilt.")
    return(NULL)
  }
  cached
}

roi_analysis <- read_roi_analysis_cache()

if (!is.null(roi_analysis)) {
  stage_line("ROI table checkpoint", "COMPLETE", "valid cached ROI analysis found")
  report_line(
    sprintf(
      "Reusing saved ROI analysis checkpoint: %d rows from %d images. Skipping the expensive per-image ROI table rebuild.",
      nrow(roi_analysis),
      length(unique(roi_analysis$image_name))
    )
  )
} else {
  # Extract all ROI data with properties and skeleton metrics
  stage_line("ROI table construction", "START", "extracting ROI geometry metrics from optROIs_data.rds")
  report_line("Extracting ROI properties and skeleton metrics...")
  overall_roi_start <- Sys.time()
  cumulative_rois_seen <- 0L
  report_line(
    sprintf(
      "ROI extraction progress will be reported per image: %d images, %d ROI objects expected.",
      total_images_to_process,
      expected_total_rois
    )
  )
  roi_analysis <- data.frame(
    image_name = character(),
    roi_name = character(),
    area = numeric(),
    perimeter = numeric(),
    circularity = numeric(),
    aspect_ratio = numeric(),
    solidity = numeric(),
    mean_width = numeric(),
    skeleton_length = numeric(),
    branch_points = numeric(),
    endpoints = numeric(),
    avg_branch_length = numeric(),
    path_straightness = numeric(),
    orientation_coherence = numeric(),
    is_high_neurites = logical(),
    is_low_neurites = logical(),
    is_high_confluence = logical(),
    is_low_confluence = logical(),
    is_signal = logical(),
    is_noise = logical(),
    stringsAsFactors = FALSE
  )

  for (image_index in seq_along(names(optROIs))) {
    img_name <- names(optROIs)[[image_index]]
    image_start <- Sys.time()
    img_data <- optROIs[[img_name]]
    metadata <- img_data$metadata
    image_roi_count <- length(img_data$rois)
    
    # Determine group classifications
    is_high_neurites <- metadata$grp_high_neurites_low_confluence == 1 || 
      metadata$grp_high_neurites_high_confluence == 1
    is_low_neurites <- metadata$grp_low_neurites_low_confluence == 1 || 
      metadata$grp_low_neurites_high_confluence == 1
    is_high_confluence <- metadata$grp_low_neurites_high_confluence == 1 || 
      metadata$grp_high_neurites_high_confluence == 1
    is_low_confluence <- metadata$grp_high_neurites_low_confluence == 1 || 
      metadata$grp_low_neurites_low_confluence == 1
    is_signal <- metadata$manual_choise == 1
    is_noise <- metadata$manual_noise == 1
    
    # Process each ROI
    if (length(img_data$rois) > 0) {
      for (roi_name in names(img_data$rois)) {
        roi_data <- img_data$rois[[roi_name]]
        
        # Extract properties
        props <- extract_roi_properties(roi_data)
        skel_metrics <- calculate_skeleton_metrics(roi_data)
        
        # Create row
        roi_row <- data.frame(
          image_name = img_name,
          roi_name = roi_name,
          area = props$area,
          perimeter = props$perimeter,
          circularity = props$circularity,
          aspect_ratio = props$aspect_ratio,
          solidity = props$solidity,
          mean_width = props$mean_width,
          skeleton_length = skel_metrics$total_length,
          branch_points = skel_metrics$branch_points,
          endpoints = skel_metrics$endpoints,
          avg_branch_length = skel_metrics$avg_branch_length,
          path_straightness = skel_metrics$path_straightness,
          orientation_coherence = skel_metrics$orientation_coherence,
          is_high_neurites = is_high_neurites,
          is_low_neurites = is_low_neurites,
          is_high_confluence = is_high_confluence,
          is_low_confluence = is_low_confluence,
          is_signal = is_signal,
          is_noise = is_noise,
          stringsAsFactors = FALSE
        )
        
        roi_analysis <- rbind(roi_analysis, roi_row)
      }
    }
    
    image_elapsed <- as.numeric(difftime(Sys.time(), image_start, units = "secs"))
    cumulative_rois_seen <- cumulative_rois_seen + image_roi_count
    cumulative_elapsed <- as.numeric(difftime(Sys.time(), overall_roi_start, units = "secs"))
    image_rate <- if (image_elapsed > 0) image_roi_count / image_elapsed else NA_real_
    cumulative_rate <- if (cumulative_elapsed > 0) cumulative_rois_seen / cumulative_elapsed else NA_real_
    remaining_rois <- max(0, expected_total_rois - cumulative_rois_seen)
    roi_eta <- if (is.finite(cumulative_rate) && cumulative_rate > 0) remaining_rois / cumulative_rate else NA_real_
    report_line(
      sprintf(
        "  Image added to optimization table %d/%d: %s - %d ROIs in %s%s | cumulative %d/%d ROIs in %s%s | ROI-table ETA %s",
        image_index,
        total_images_to_process,
        img_name,
        image_roi_count,
        format_elapsed(image_elapsed),
        if (is.finite(image_rate)) sprintf(" (%.1f ROIs/sec)", image_rate) else "",
        cumulative_rois_seen,
        expected_total_rois,
        format_elapsed(cumulative_elapsed),
        if (is.finite(cumulative_rate)) sprintf(" (%.1f ROIs/sec overall)", cumulative_rate) else "",
        format_elapsed(roi_eta)
      )
    )
  }
  stage_line("ROI table construction", "COMPLETE", sprintf("%d images and %d ROI objects processed", total_images_to_process, expected_total_rois))
}

if (!nrow(roi_analysis)) {
  write_input_diag(
    "failed_no_roi_rows",
    "No ROI rows were produced for optimization. The packaged ROI object contains no readable ROI entries, or the ROI extraction stage created empty ROI payloads.",
    list(optroi_object_count = length(optROIs))
  )
  report_line("ERROR: No ROI rows were produced for optimization.")
  stop(
    paste(
      "No ROI rows were produced for optimization.",
      "This usually means the packaged ROI object contains no readable ROI entries for the selected images,",
      "or the ROI extraction step created empty ROI sets.",
      "Check the output of '7. Extract ROIs and skeleton summaries' and the packaged optROIs RDS before rerunning."
    )
  )
}

# Remove rows with NA values in key metrics
roi_analysis <- roi_analysis %>%
  filter(!is.na(area) & !is.na(circularity) & !is.na(skeleton_length)) %>%
  add_topology_proxy_metrics()

roi_analysis$.roi_row_id <- seq_len(nrow(roi_analysis))

if (!nrow(roi_analysis)) {
  write_input_diag(
    "failed_no_complete_metric_rows",
    "ROI rows were present, but none survived the key-metric completeness filter for area, circularity, and skeleton_length.",
    list(optroi_object_count = length(optROIs))
  )
  report_line("ERROR: ROI rows existed, but none had complete area/circularity/skeleton_length values.")
  stop(
    paste(
      "ROI rows were found, but none had complete area/circularity/skeleton_length values after filtering.",
      "Check whether the ROI property extraction logic matches the actual ROI object structure produced by the previous steps."
    )
  )
}

write_roi_analysis_cache(roi_analysis)
stage_line("ROI table checkpoint", "COMPLETE", "checkpoint is saved and can be reused on compatible reruns")

report_line("")
report_line("Total ROIs analyzed: ", nrow(roi_analysis))
report_line("  Signal (manual_choice): ", sum(roi_analysis$is_signal))
report_line("  Noise (manual_noise): ", sum(roi_analysis$is_noise))
has_manual_noise <- sum(roi_analysis$is_noise, na.rm = TRUE) > 0
optimization_mode <- Sys.getenv(
  "OPTIMIZATION_SCORING_MODE",
  unset = ctx$settings$OPTIMIZATION_SCORING_MODE %||% "continuity_aware"
)
optimization_mode <- trimws(optimization_mode)
allowed_optimization_modes <- c("continuity_aware", "topology_aware_continuity", "ensemble_union_continuity_topology", "classic_roi_cutoff")
if (!optimization_mode %in% allowed_optimization_modes) {
  report_line(
    "WARNING: Unknown OPTIMIZATION_SCORING_MODE='", optimization_mode,
    "'. Falling back to continuity_aware."
  )
  optimization_mode <- "continuity_aware"
}
scoring_mode_label <- paste0(
  optimization_mode,
  if (has_manual_noise) "_signal_noise_supervised" else "_fallback_no_manual_noise"
)
if (identical(optimization_mode, "continuity_aware")) {
  if (has_manual_noise) {
    stage_line("Optimization scoring mode", "START", "continuity-aware scoring enabled; manual noise examples are available, so signal/noise rejection will contribute to the score")
  } else {
    stage_line("Optimization scoring mode", "START", "continuity-aware scoring enabled; no manual noise examples found, so fallback scoring excludes signal/noise reward")
    report_line("WARNING: No manual_noise ROI rows were found. The optimizer will use continuity-aware fallback scoring based on image-level neurite continuity, confluence robustness, length-weighted retention, and fragmentation control. Add manually labelled noise images later for true noise-rejection learning.")
  }
} else if (identical(optimization_mode, "topology_aware_continuity")) {
  if (has_manual_noise) {
    stage_line("Optimization scoring mode", "START", "topology-aware continuity scoring enabled; manual noise examples are available, so mesh-like ROI rejection will contribute to the score")
  } else {
    stage_line("Optimization scoring mode", "START", "topology-aware continuity scoring enabled; no manual noise examples found, so fallback scoring uses dominant-path rewards and mesh penalties")
    report_line("WARNING: No manual_noise ROI rows were found. Topology-aware mode will still penalize sponge-like mesh geometry using dominant-path ratio, meshness, branch/endpoint density, and orientation coherence.")
  }
} else if (identical(optimization_mode, "ensemble_union_continuity_topology")) {
  if (has_manual_noise) {
    stage_line("Optimization scoring mode", "START", "ensemble union scoring enabled; continuity and topology rewards are combined while manual noise examples penalize false-positive-prone settings")
  } else {
    stage_line("Optimization scoring mode", "START", "ensemble union scoring enabled; no manual noise examples found, so fallback scoring combines continuity, dominant-path topology, and anti-mesh penalties")
    report_line("WARNING: No manual_noise ROI rows were found. Ensemble mode will still avoid obvious mesh-like candidates by using topology penalties, but explicit false-positive learning will improve if noise examples are added.")
  }
} else {
  if (has_manual_noise) {
    stage_line("Optimization scoring mode", "START", "classic ROI cutoff scoring enabled; manual noise examples are available, so signal/noise rejection will contribute to the score")
  } else {
    stage_line("Optimization scoring mode", "START", "classic ROI cutoff scoring enabled; no manual noise examples found, so fallback scoring excludes signal/noise reward")
    report_line("WARNING: No manual_noise ROI rows were found. Classic mode will score by ROI-level neurite separation, confluence penalty, and retention behavior only.")
  }
}
stage_line("Input diagnostics", "START", "writing optimization readiness diagnostics")
report_line("Writing lightweight optimization input diagnostics...")
write_input_diag(
  "ready_for_optimization",
  "ROI analysis table was created successfully and optimization can proceed.",
  list(
    optroi_object_count = length(optROIs),
    roi_analysis_rows = nrow(roi_analysis),
    signal_rows = sum(roi_analysis$is_signal),
    noise_rows = sum(roi_analysis$is_noise),
    optimization_mode = optimization_mode,
    scoring_mode = scoring_mode_label
  )
)
stage_line("Input diagnostics", "COMPLETE", "optimization_input_diagnostics.csv updated")

report_line("")

# Define parameter grid for optimization
stage_line("Parameter grid setup", "START", "building candidate cutoff combinations")
param_grid <- expand.grid(
  min_area = c(100, 200, 350, 500, 750, 1000),
  max_area = c(5000, 10000, 15000, 20000, Inf),
  min_circularity = c(0, 0.1, 0.2, 0.3),
  max_circularity = c(0.7, 0.8, 0.9, 1.0),
  min_aspect_ratio = c(0.5, 1.0, 1.5, 2.0),
  max_aspect_ratio = c(3, 5, 10, Inf),
  stringsAsFactors = FALSE
)

# Reduce grid size for computational efficiency (sample if too large)
if (nrow(param_grid) > 500) {
  set.seed(42)
  param_grid <- param_grid[sample(1:nrow(param_grid), 500), ]
}

stage_line("Parameter grid setup", "COMPLETE", sprintf("%d parameter combinations ready", nrow(param_grid)))
report_line("Testing ", nrow(param_grid), " parameter combinations...")
report_line("")
stage_line("Grid optimization", "START", sprintf("testing %d parameter combinations", nrow(param_grid)))
report_line("Starting grid optimization over ", nrow(param_grid), " parameter combinations...")
report_line("Grid progress will be reported at regular checkpoints with the best score found so far.")

long_roi_threshold <- suppressWarnings(as.numeric(stats::quantile(roi_analysis$skeleton_length, 0.90, na.rm = TRUE)))
short_roi_threshold <- suppressWarnings(as.numeric(stats::quantile(roi_analysis$skeleton_length, 0.25, na.rm = TRUE)))
if (!is.finite(long_roi_threshold)) long_roi_threshold <- 100
if (!is.finite(short_roi_threshold)) short_roi_threshold <- 10
if (identical(optimization_mode, "continuity_aware")) {
  report_line(sprintf("Continuity-aware thresholds: long ROI >= %.2f skeleton units; short ROI < %.2f skeleton units.", long_roi_threshold, short_roi_threshold))
} else if (identical(optimization_mode, "topology_aware_continuity")) {
  report_line(sprintf("Topology-aware thresholds: long ROI >= %.2f skeleton units; short ROI < %.2f skeleton units. Mesh-like topology penalties will also be used.", long_roi_threshold, short_roi_threshold))
} else if (identical(optimization_mode, "ensemble_union_continuity_topology")) {
  report_line(sprintf("Ensemble union thresholds: long ROI >= %.2f skeleton units; short ROI < %.2f skeleton units. Continuity and topology rewards will be combined with anti-mesh penalties and a higher retention target.", long_roi_threshold, short_roi_threshold))
} else {
  report_line("Classic ROI cutoff mode selected; continuity thresholds will be recorded for metadata but not used in scoring.")
}

safe_norm_sep <- function(a, b) {
  if (!is.finite(a) || !is.finite(b)) return(NA_real_)
  abs(a - b) / (mean(c(abs(a), abs(b)), na.rm = TRUE) + 1)
}

safe_mean <- function(x) {
  x <- x[is.finite(x)]
  if (!length(x)) return(NA_real_)
  mean(x)
}

top_k_sum <- function(x, k = 5L) {
  x <- sort(x[is.finite(x)], decreasing = TRUE)
  if (!length(x)) return(0)
  sum(utils::head(x, k))
}

aggregate_image_continuity_metrics <- function(filtered_data, all_data) {
  if (!nrow(filtered_data)) return(data.frame())
  all_lengths <- all_data %>%
    group_by(image_name) %>%
    summarise(all_skeleton_length = sum(skeleton_length, na.rm = TRUE), .groups = "drop")
  filtered_data %>%
    group_by(
      image_name,
      is_high_neurites,
      is_low_neurites,
      is_high_confluence,
      is_low_confluence,
      is_signal,
      is_noise
    ) %>%
    summarise(
      retained_roi_count = n(),
      total_skeleton_length = sum(skeleton_length, na.rm = TRUE),
      max_skeleton_length = max(skeleton_length, na.rm = TRUE),
      top5_skeleton_length_sum = top_k_sum(skeleton_length, 5L),
      long_roi_count = sum(skeleton_length >= long_roi_threshold, na.rm = TRUE),
      short_fragment_fraction = mean(skeleton_length < short_roi_threshold, na.rm = TRUE),
      mean_skeleton_length = mean(skeleton_length, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(all_lengths, by = "image_name") %>%
    mutate(
      length_weighted_retention = total_skeleton_length / (all_skeleton_length + 1),
      fragmentation_index = retained_roi_count / (total_skeleton_length + 1),
      continuity_score = log1p(total_skeleton_length) +
        log1p(top5_skeleton_length_sum) +
        log1p(max_skeleton_length) +
        log1p(long_roi_count) -
        log1p(fragmentation_index * 1000) -
        short_fragment_fraction
    )
}

aggregate_image_topology_metrics <- function(filtered_data, all_data) {
  if (!nrow(filtered_data)) return(data.frame())
  all_lengths <- all_data %>%
    group_by(image_name) %>%
    summarise(all_skeleton_length = sum(skeleton_length, na.rm = TRUE), .groups = "drop")
  filtered_data %>%
    group_by(
      image_name,
      is_high_neurites,
      is_low_neurites,
      is_high_confluence,
      is_low_confluence,
      is_signal,
      is_noise
    ) %>%
    summarise(
      retained_roi_count = n(),
      total_skeleton_length = sum(skeleton_length, na.rm = TRUE),
      max_skeleton_length = max(skeleton_length, na.rm = TRUE),
      top5_skeleton_length_sum = top_k_sum(skeleton_length, 5L),
      long_roi_count = sum(skeleton_length >= long_roi_threshold, na.rm = TRUE),
      short_fragment_fraction = mean(skeleton_length < short_roi_threshold, na.rm = TRUE),
      dominant_path_length = sum(skeleton_length * dominant_path_ratio, na.rm = TRUE),
      dominant_path_ratio = weighted.mean(dominant_path_ratio, pmax(skeleton_length, 1), na.rm = TRUE),
      mesh_penalty = weighted.mean(meshness_score, pmax(skeleton_length, 1), na.rm = TRUE),
      junction_density_penalty = weighted.mean(branch_density + endpoint_density, pmax(skeleton_length, 1), na.rm = TRUE),
      orientation_coherence = weighted.mean(orientation_coherence, pmax(skeleton_length, 1), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(all_lengths, by = "image_name") %>%
    mutate(
      length_weighted_retention = total_skeleton_length / (all_skeleton_length + 1),
      fragmentation_index = retained_roi_count / (total_skeleton_length + 1),
      topology_score = log1p(dominant_path_length) +
        log1p(top5_skeleton_length_sum) +
        log1p(max_skeleton_length) +
        log1p(long_roi_count) +
        dominant_path_ratio +
        orientation_coherence -
        log1p(fragmentation_index * 1000) -
        mesh_penalty -
        log1p(junction_density_penalty * 100) -
        short_fragment_fraction
    )
}

empty_eval_result <- function(n_retained, data_n) {
  list(
    score = -Inf,
    n_retained = n_retained,
    neurite_separation = NA_real_,
    confluence_separation = NA_real_,
    signal_noise_ratio = NA_real_,
    retention_rate = n_retained / data_n,
    retention_penalty = NA_real_,
    optimization_mode = optimization_mode,
    scoring_mode = scoring_mode_label,
    continuity_score = NA_real_,
    continuity_neurite_separation = NA_real_,
    total_length_separation = NA_real_,
    top5_length_separation = NA_real_,
    max_length_separation = NA_real_,
    length_weighted_retention = NA_real_,
    fragmentation_penalty = NA_real_,
    short_fragment_fraction = NA_real_,
    topology_score = NA_real_,
    dominant_path_ratio = NA_real_,
    mesh_penalty = NA_real_,
    junction_density_penalty = NA_real_,
    orientation_coherence = NA_real_,
    high_neurites_mean = NA_real_,
    low_neurites_mean = NA_real_,
    high_conf_mean = NA_real_,
    low_conf_mean = NA_real_
  )
}

evaluate_parameters_classic <- function(params, data) {
  filtered_data <- data %>%
    filter(
      area >= params$min_area & area <= params$max_area,
      circularity >= params$min_circularity & circularity <= params$max_circularity,
      aspect_ratio >= params$min_aspect_ratio & aspect_ratio <= params$max_aspect_ratio
    )

  n_retained <- nrow(filtered_data)
  if (n_retained < 10) return(empty_eval_result(n_retained, nrow(data)))

  signal_data <- filtered_data %>% filter(is_signal)
  noise_data <- filtered_data %>% filter(is_noise)
  if (nrow(signal_data) < 5) return(empty_eval_result(n_retained, nrow(data)))

  high_neurites_mean <- safe_mean(signal_data$skeleton_length[signal_data$is_high_neurites])
  low_neurites_mean <- safe_mean(signal_data$skeleton_length[signal_data$is_low_neurites])
  high_conf_mean <- safe_mean(signal_data$skeleton_length[signal_data$is_high_confluence])
  low_conf_mean <- safe_mean(signal_data$skeleton_length[signal_data$is_low_confluence])

  neurite_separation <- safe_norm_sep(high_neurites_mean, low_neurites_mean)
  confluence_separation <- safe_norm_sep(high_conf_mean, low_conf_mean)

  signal_mean <- safe_mean(signal_data$skeleton_length)
  if (has_manual_noise && nrow(noise_data) > 0) {
    noise_mean <- safe_mean(noise_data$skeleton_length)
    signal_noise_ratio <- signal_mean / (noise_mean + 1)
  } else {
    signal_noise_ratio <- NA_real_
  }

  retention_rate <- n_retained / nrow(data)
  retention_penalty <- abs(retention_rate - 0.35)

  if (is.finite(neurite_separation) && is.finite(confluence_separation)) {
    score <- (2.0 * neurite_separation) -
      (1.0 * confluence_separation) -
      (0.6 * retention_penalty)
    if (has_manual_noise && is.finite(signal_noise_ratio)) {
      score <- score + log1p(signal_noise_ratio)
    }
  } else {
    score <- -Inf
  }

  list(
    score = score,
    n_retained = n_retained,
    neurite_separation = neurite_separation,
    confluence_separation = confluence_separation,
    signal_noise_ratio = signal_noise_ratio,
    retention_rate = retention_rate,
    retention_penalty = retention_penalty,
    optimization_mode = optimization_mode,
    scoring_mode = scoring_mode_label,
    continuity_score = NA_real_,
    continuity_neurite_separation = NA_real_,
    total_length_separation = NA_real_,
    top5_length_separation = NA_real_,
    max_length_separation = NA_real_,
    length_weighted_retention = NA_real_,
    fragmentation_penalty = NA_real_,
    short_fragment_fraction = NA_real_,
    topology_score = NA_real_,
    dominant_path_ratio = NA_real_,
    mesh_penalty = NA_real_,
    junction_density_penalty = NA_real_,
    orientation_coherence = NA_real_,
    high_neurites_mean = high_neurites_mean,
    low_neurites_mean = low_neurites_mean,
    high_conf_mean = high_conf_mean,
    low_conf_mean = low_conf_mean
  )
}

# Function to evaluate a parameter set
evaluate_parameters_continuity <- function(params, data) {
  filtered_data <- data %>%
    filter(
      area >= params$min_area & area <= params$max_area,
      circularity >= params$min_circularity & circularity <= params$max_circularity,
      aspect_ratio >= params$min_aspect_ratio & aspect_ratio <= params$max_aspect_ratio
    )

  n_retained <- nrow(filtered_data)
  if (n_retained < 10) return(empty_eval_result(n_retained, nrow(data)))

  signal_data <- filtered_data %>% filter(is_signal)
  noise_data <- filtered_data %>% filter(is_noise)
  if (nrow(signal_data) < 5) return(empty_eval_result(n_retained, nrow(data)))

  image_metrics <- aggregate_image_continuity_metrics(filtered_data, data)
  signal_images <- image_metrics %>% filter(is_signal)
  noise_images <- image_metrics %>% filter(is_noise)
  if (nrow(signal_images) < 2) return(empty_eval_result(n_retained, nrow(data)))

  high_neurites_mean <- safe_mean(signal_images$continuity_score[signal_images$is_high_neurites])
  low_neurites_mean <- safe_mean(signal_images$continuity_score[signal_images$is_low_neurites])
  high_conf_mean <- safe_mean(signal_images$continuity_score[signal_images$is_high_confluence])
  low_conf_mean <- safe_mean(signal_images$continuity_score[signal_images$is_low_confluence])

  continuity_neurite_separation <- safe_norm_sep(high_neurites_mean, low_neurites_mean)
  confluence_separation <- safe_norm_sep(high_conf_mean, low_conf_mean)

  total_length_separation <- safe_norm_sep(
    safe_mean(signal_images$total_skeleton_length[signal_images$is_high_neurites]),
    safe_mean(signal_images$total_skeleton_length[signal_images$is_low_neurites])
  )
  top5_length_separation <- safe_norm_sep(
    safe_mean(signal_images$top5_skeleton_length_sum[signal_images$is_high_neurites]),
    safe_mean(signal_images$top5_skeleton_length_sum[signal_images$is_low_neurites])
  )
  max_length_separation <- safe_norm_sep(
    safe_mean(signal_images$max_skeleton_length[signal_images$is_high_neurites]),
    safe_mean(signal_images$max_skeleton_length[signal_images$is_low_neurites])
  )
  length_weighted_retention <- safe_mean(signal_images$length_weighted_retention)
  fragmentation_penalty <- safe_mean(signal_images$fragmentation_index)
  short_fragment_fraction <- safe_mean(signal_images$short_fragment_fraction)
  continuity_score <- safe_mean(signal_images$continuity_score)

  signal_mean_continuity <- safe_mean(signal_images$continuity_score)
  if (has_manual_noise && nrow(noise_images) > 0) {
    noise_mean_continuity <- safe_mean(noise_images$continuity_score)
    signal_noise_ratio <- signal_mean_continuity / (noise_mean_continuity + 1)
  } else {
    signal_noise_ratio <- NA_real_
  }

  retention_rate <- n_retained / nrow(data)
  retention_penalty <- abs(length_weighted_retention - 0.45)
  if (!is.finite(retention_penalty)) retention_penalty <- abs(retention_rate - 0.35)

  if (is.finite(continuity_neurite_separation) && is.finite(confluence_separation)) {
    score <- (2.0 * continuity_neurite_separation) +
      (1.5 * (total_length_separation %||% 0)) +
      (1.0 * (top5_length_separation %||% 0)) +
      (0.7 * (max_length_separation %||% 0)) +
      (0.8 * log1p(length_weighted_retention %||% 0)) -
      (1.0 * confluence_separation) -
      (0.8 * log1p((fragmentation_penalty %||% 0) * 1000)) -
      (0.8 * (short_fragment_fraction %||% 0)) -
      (0.6 * retention_penalty)
    if (has_manual_noise && is.finite(signal_noise_ratio)) {
      score <- score + log1p(signal_noise_ratio)
    }
  } else {
    score <- -Inf
  }

  list(
    score = score,
    n_retained = n_retained,
    neurite_separation = continuity_neurite_separation,
    confluence_separation = confluence_separation,
    signal_noise_ratio = signal_noise_ratio,
    retention_rate = retention_rate,
    retention_penalty = retention_penalty,
    optimization_mode = optimization_mode,
    scoring_mode = scoring_mode_label,
    continuity_score = continuity_score,
    continuity_neurite_separation = continuity_neurite_separation,
    total_length_separation = total_length_separation,
    top5_length_separation = top5_length_separation,
    max_length_separation = max_length_separation,
    length_weighted_retention = length_weighted_retention,
    fragmentation_penalty = fragmentation_penalty,
    short_fragment_fraction = short_fragment_fraction,
    topology_score = NA_real_,
    dominant_path_ratio = NA_real_,
    mesh_penalty = NA_real_,
    junction_density_penalty = NA_real_,
    orientation_coherence = NA_real_,
    high_neurites_mean = high_neurites_mean,
    low_neurites_mean = low_neurites_mean,
    high_conf_mean = high_conf_mean,
    low_conf_mean = low_conf_mean
  )
}

evaluate_parameters_topology <- function(params, data) {
  filtered_data <- data %>%
    filter(
      area >= params$min_area & area <= params$max_area,
      circularity >= params$min_circularity & circularity <= params$max_circularity,
      aspect_ratio >= params$min_aspect_ratio & aspect_ratio <= params$max_aspect_ratio
    )

  n_retained <- nrow(filtered_data)
  if (n_retained < 10) return(empty_eval_result(n_retained, nrow(data)))

  signal_data <- filtered_data %>% filter(is_signal)
  noise_data <- filtered_data %>% filter(is_noise)
  if (nrow(signal_data) < 5) return(empty_eval_result(n_retained, nrow(data)))

  image_metrics <- aggregate_image_topology_metrics(filtered_data, data)
  signal_images <- image_metrics %>% filter(is_signal)
  noise_images <- image_metrics %>% filter(is_noise)
  if (nrow(signal_images) < 2) return(empty_eval_result(n_retained, nrow(data)))

  high_neurites_mean <- safe_mean(signal_images$topology_score[signal_images$is_high_neurites])
  low_neurites_mean <- safe_mean(signal_images$topology_score[signal_images$is_low_neurites])
  high_conf_mean <- safe_mean(signal_images$topology_score[signal_images$is_high_confluence])
  low_conf_mean <- safe_mean(signal_images$topology_score[signal_images$is_low_confluence])

  topology_neurite_separation <- safe_norm_sep(high_neurites_mean, low_neurites_mean)
  confluence_separation <- safe_norm_sep(high_conf_mean, low_conf_mean)

  total_length_separation <- safe_norm_sep(
    safe_mean(signal_images$total_skeleton_length[signal_images$is_high_neurites]),
    safe_mean(signal_images$total_skeleton_length[signal_images$is_low_neurites])
  )
  dominant_path_separation <- safe_norm_sep(
    safe_mean(signal_images$dominant_path_length[signal_images$is_high_neurites]),
    safe_mean(signal_images$dominant_path_length[signal_images$is_low_neurites])
  )
  max_length_separation <- safe_norm_sep(
    safe_mean(signal_images$max_skeleton_length[signal_images$is_high_neurites]),
    safe_mean(signal_images$max_skeleton_length[signal_images$is_low_neurites])
  )

  topology_score <- safe_mean(signal_images$topology_score)
  dominant_path_ratio <- safe_mean(signal_images$dominant_path_ratio)
  mesh_penalty <- safe_mean(signal_images$mesh_penalty)
  junction_density_penalty <- safe_mean(signal_images$junction_density_penalty)
  orientation_coherence <- safe_mean(signal_images$orientation_coherence)
  length_weighted_retention <- safe_mean(signal_images$length_weighted_retention)
  fragmentation_penalty <- safe_mean(signal_images$fragmentation_index)
  short_fragment_fraction <- safe_mean(signal_images$short_fragment_fraction)

  signal_mean_topology <- safe_mean(signal_images$topology_score)
  if (has_manual_noise && nrow(noise_images) > 0) {
    noise_mean_topology <- safe_mean(noise_images$topology_score)
    signal_noise_ratio <- signal_mean_topology / (noise_mean_topology + 1)
  } else {
    signal_noise_ratio <- NA_real_
  }

  retention_rate <- n_retained / nrow(data)
  retention_penalty <- abs(length_weighted_retention - 0.45)
  if (!is.finite(retention_penalty)) retention_penalty <- abs(retention_rate - 0.35)

  if (is.finite(topology_neurite_separation) && is.finite(confluence_separation)) {
    score <- (2.0 * topology_neurite_separation) +
      (1.4 * (dominant_path_separation %||% 0)) +
      (1.0 * (total_length_separation %||% 0)) +
      (0.6 * (max_length_separation %||% 0)) +
      (1.0 * log1p(dominant_path_ratio %||% 0)) +
      (0.7 * log1p(length_weighted_retention %||% 0)) +
      (0.5 * (orientation_coherence %||% 0)) -
      (1.0 * confluence_separation) -
      (1.2 * log1p(mesh_penalty %||% 0)) -
      (0.9 * log1p((junction_density_penalty %||% 0) * 100)) -
      (0.8 * log1p((fragmentation_penalty %||% 0) * 1000)) -
      (0.8 * (short_fragment_fraction %||% 0)) -
      (0.6 * retention_penalty)
    if (has_manual_noise && is.finite(signal_noise_ratio)) {
      score <- score + log1p(signal_noise_ratio)
    }
  } else {
    score <- -Inf
  }

  list(
    score = score,
    n_retained = n_retained,
    neurite_separation = topology_neurite_separation,
    confluence_separation = confluence_separation,
    signal_noise_ratio = signal_noise_ratio,
    retention_rate = retention_rate,
    retention_penalty = retention_penalty,
    optimization_mode = optimization_mode,
    scoring_mode = scoring_mode_label,
    continuity_score = topology_score,
    continuity_neurite_separation = topology_neurite_separation,
    total_length_separation = total_length_separation,
    top5_length_separation = dominant_path_separation,
    max_length_separation = max_length_separation,
    length_weighted_retention = length_weighted_retention,
    fragmentation_penalty = fragmentation_penalty,
    short_fragment_fraction = short_fragment_fraction,
    topology_score = topology_score,
    dominant_path_ratio = dominant_path_ratio,
    mesh_penalty = mesh_penalty,
    junction_density_penalty = junction_density_penalty,
    orientation_coherence = orientation_coherence,
    high_neurites_mean = high_neurites_mean,
    low_neurites_mean = low_neurites_mean,
    high_conf_mean = high_conf_mean,
    low_conf_mean = low_conf_mean
  )
}

evaluate_parameters_ensemble <- function(params, data) {
  filtered_data <- data %>%
    filter(
      area >= params$min_area & area <= params$max_area,
      circularity >= params$min_circularity & circularity <= params$max_circularity,
      aspect_ratio >= params$min_aspect_ratio & aspect_ratio <= params$max_aspect_ratio
    )

  n_retained <- nrow(filtered_data)
  if (n_retained < 10) return(empty_eval_result(n_retained, nrow(data)))

  signal_data <- filtered_data %>% filter(is_signal)
  if (nrow(signal_data) < 5) return(empty_eval_result(n_retained, nrow(data)))

  continuity_metrics <- aggregate_image_continuity_metrics(filtered_data, data)
  topology_metrics <- aggregate_image_topology_metrics(filtered_data, data)
  image_metrics <- continuity_metrics %>%
    select(
      image_name,
      is_signal,
      is_noise,
      is_high_neurites,
      is_low_neurites,
      is_high_confluence,
      is_low_confluence,
      continuity_score,
      continuity_total_skeleton_length = total_skeleton_length,
      continuity_top5_skeleton_length_sum = top5_skeleton_length_sum,
      continuity_max_skeleton_length = max_skeleton_length,
      continuity_length_weighted_retention = length_weighted_retention,
      continuity_fragmentation_index = fragmentation_index,
      continuity_short_fragment_fraction = short_fragment_fraction
    ) %>%
    left_join(
      topology_metrics %>%
        select(
          image_name,
          topology_score,
          topology_total_skeleton_length = total_skeleton_length,
          dominant_path_length,
          dominant_path_ratio,
          mesh_penalty,
          junction_density_penalty,
          orientation_coherence,
          topology_length_weighted_retention = length_weighted_retention,
          topology_fragmentation_index = fragmentation_index,
          topology_short_fragment_fraction = short_fragment_fraction
        ),
      by = "image_name"
    )

  signal_images <- image_metrics %>% filter(is_signal)
  noise_images <- image_metrics %>% filter(is_noise)
  if (nrow(signal_images) < 2) return(empty_eval_result(n_retained, nrow(data)))

  ensemble_value <- function(df) {
    log1p(pmax(0, df$continuity_score)) +
      log1p(pmax(0, df$topology_score)) +
      log1p(pmax(0, df$continuity_top5_skeleton_length_sum)) +
      log1p(pmax(0, df$dominant_path_length)) +
      pmax(0, df$dominant_path_ratio) +
      pmax(0, df$orientation_coherence) -
      log1p(pmax(0, df$mesh_penalty)) -
      log1p(pmax(0, df$junction_density_penalty) * 100)
  }

  signal_images$ensemble_score <- ensemble_value(signal_images)
  if (nrow(noise_images)) noise_images$ensemble_score <- ensemble_value(noise_images)

  high_neurites_mean <- safe_mean(signal_images$ensemble_score[signal_images$is_high_neurites])
  low_neurites_mean <- safe_mean(signal_images$ensemble_score[signal_images$is_low_neurites])
  high_conf_mean <- safe_mean(signal_images$ensemble_score[signal_images$is_high_confluence])
  low_conf_mean <- safe_mean(signal_images$ensemble_score[signal_images$is_low_confluence])

  ensemble_neurite_separation <- safe_norm_sep(high_neurites_mean, low_neurites_mean)
  confluence_separation <- safe_norm_sep(high_conf_mean, low_conf_mean)
  total_length_separation <- safe_norm_sep(
    safe_mean(signal_images$continuity_total_skeleton_length[signal_images$is_high_neurites]),
    safe_mean(signal_images$continuity_total_skeleton_length[signal_images$is_low_neurites])
  )
  top5_length_separation <- safe_norm_sep(
    safe_mean(signal_images$continuity_top5_skeleton_length_sum[signal_images$is_high_neurites]),
    safe_mean(signal_images$continuity_top5_skeleton_length_sum[signal_images$is_low_neurites])
  )
  dominant_path_separation <- safe_norm_sep(
    safe_mean(signal_images$dominant_path_length[signal_images$is_high_neurites]),
    safe_mean(signal_images$dominant_path_length[signal_images$is_low_neurites])
  )
  max_length_separation <- safe_norm_sep(
    safe_mean(signal_images$continuity_max_skeleton_length[signal_images$is_high_neurites]),
    safe_mean(signal_images$continuity_max_skeleton_length[signal_images$is_low_neurites])
  )

  continuity_score <- safe_mean(signal_images$continuity_score)
  topology_score <- safe_mean(signal_images$topology_score)
  dominant_path_ratio <- safe_mean(signal_images$dominant_path_ratio)
  mesh_penalty <- safe_mean(signal_images$mesh_penalty)
  junction_density_penalty <- safe_mean(signal_images$junction_density_penalty)
  orientation_coherence <- safe_mean(signal_images$orientation_coherence)
  length_weighted_retention <- safe_mean(c(signal_images$continuity_length_weighted_retention, signal_images$topology_length_weighted_retention))
  fragmentation_penalty <- safe_mean(c(signal_images$continuity_fragmentation_index, signal_images$topology_fragmentation_index))
  short_fragment_fraction <- safe_mean(c(signal_images$continuity_short_fragment_fraction, signal_images$topology_short_fragment_fraction))

  if (has_manual_noise && nrow(noise_images) > 0) {
    signal_noise_ratio <- safe_mean(signal_images$ensemble_score) / (safe_mean(noise_images$ensemble_score) + 1)
  } else {
    signal_noise_ratio <- NA_real_
  }

  retention_rate <- n_retained / nrow(data)
  retention_penalty <- abs(length_weighted_retention - 0.60)
  if (!is.finite(retention_penalty)) retention_penalty <- abs(retention_rate - 0.45)

  if (is.finite(ensemble_neurite_separation) && is.finite(confluence_separation)) {
    score <- (2.2 * ensemble_neurite_separation) +
      (1.4 * (total_length_separation %||% 0)) +
      (1.2 * (top5_length_separation %||% 0)) +
      (1.2 * (dominant_path_separation %||% 0)) +
      (0.7 * (max_length_separation %||% 0)) +
      (0.8 * log1p(length_weighted_retention %||% 0)) +
      (0.7 * log1p(dominant_path_ratio %||% 0)) +
      (0.4 * (orientation_coherence %||% 0)) -
      (1.0 * confluence_separation) -
      (0.9 * log1p(mesh_penalty %||% 0)) -
      (0.7 * log1p((junction_density_penalty %||% 0) * 100)) -
      (0.7 * log1p((fragmentation_penalty %||% 0) * 1000)) -
      (0.6 * (short_fragment_fraction %||% 0)) -
      (0.5 * retention_penalty)
    if (has_manual_noise && is.finite(signal_noise_ratio)) {
      score <- score + 0.9 * log1p(signal_noise_ratio)
    }
  } else {
    score <- -Inf
  }

  list(
    score = score,
    n_retained = n_retained,
    neurite_separation = ensemble_neurite_separation,
    confluence_separation = confluence_separation,
    signal_noise_ratio = signal_noise_ratio,
    retention_rate = retention_rate,
    retention_penalty = retention_penalty,
    optimization_mode = optimization_mode,
    scoring_mode = scoring_mode_label,
    continuity_score = continuity_score,
    continuity_neurite_separation = ensemble_neurite_separation,
    total_length_separation = total_length_separation,
    top5_length_separation = top5_length_separation,
    max_length_separation = max_length_separation,
    length_weighted_retention = length_weighted_retention,
    fragmentation_penalty = fragmentation_penalty,
    short_fragment_fraction = short_fragment_fraction,
    topology_score = topology_score,
    dominant_path_ratio = dominant_path_ratio,
    mesh_penalty = mesh_penalty,
    junction_density_penalty = junction_density_penalty,
    orientation_coherence = orientation_coherence,
    high_neurites_mean = high_neurites_mean,
    low_neurites_mean = low_neurites_mean,
    high_conf_mean = high_conf_mean,
    low_conf_mean = low_conf_mean
  )
}

evaluate_parameters <- if (identical(optimization_mode, "classic_roi_cutoff")) {
  evaluate_parameters_classic
} else if (identical(optimization_mode, "topology_aware_continuity")) {
  evaluate_parameters_topology
} else if (identical(optimization_mode, "ensemble_union_continuity_topology")) {
  evaluate_parameters_ensemble
} else {
  evaluate_parameters_continuity
}

filter_rois_by_params <- function(data, params) {
  data %>%
    filter(
      area >= params$min_area & area <= params$max_area,
      circularity >= params$min_circularity & circularity <= params$max_circularity,
      aspect_ratio >= params$min_aspect_ratio & aspect_ratio <= params$max_aspect_ratio
    )
}

score_parameter_grid_for_union_component <- function(eval_fun, label) {
  stage_line("Ensemble union component", "START", paste("scoring", label, "component for true union output"))
  rows <- vector("list", nrow(param_grid))
  for (i in seq_len(nrow(param_grid))) {
    params <- param_grid[i, ]
    result <- eval_fun(params, roi_analysis)
    rows[[i]] <- data.frame(
      component = label,
      param_index = i,
      min_area = params$min_area,
      max_area = params$max_area,
      min_circularity = params$min_circularity,
      max_circularity = params$max_circularity,
      min_aspect_ratio = params$min_aspect_ratio,
      max_aspect_ratio = params$max_aspect_ratio,
      score = result$score,
      n_retained = result$n_retained,
      neurite_separation = result$neurite_separation,
      confluence_separation = result$confluence_separation,
      signal_noise_ratio = result$signal_noise_ratio,
      length_weighted_retention = result$length_weighted_retention,
      fragmentation_penalty = result$fragmentation_penalty,
      topology_score = result$topology_score,
      dominant_path_ratio = result$dominant_path_ratio,
      mesh_penalty = result$mesh_penalty,
      orientation_coherence = result$orientation_coherence,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows) %>% filter(is.finite(score)) %>% arrange(desc(score))
  if (nrow(out)) {
    stage_line("Ensemble union component", "COMPLETE", sprintf("%s best score %.4f at parameter set %d", label, out$score[[1]], out$param_index[[1]]))
  } else {
    stage_line("Ensemble union component", "WARNING", paste("no finite", label, "component score found"))
  }
  out
}

# Optimize parameters
optimization_results <- list()
grid_start <- Sys.time()
grid_report_every <- max(1L, ceiling(nrow(param_grid) / 20))
best_score_so_far <- -Inf
best_index_so_far <- NA_integer_

pb <- txtProgressBar(min = 0, max = nrow(param_grid), style = 3)
for (i in 1:nrow(param_grid)) {
  iter_start <- Sys.time()
  params <- param_grid[i, ]
  results <- evaluate_parameters(params, roi_analysis)
  
  # Combine into single data frame row
  result_row <- data.frame(
    min_area = params$min_area,
    max_area = params$max_area,
    min_circularity = params$min_circularity,
    max_circularity = params$max_circularity,
    min_aspect_ratio = params$min_aspect_ratio,
    max_aspect_ratio = params$max_aspect_ratio,
    score = results$score,
    n_retained = results$n_retained,
    neurite_separation = results$neurite_separation,
    confluence_separation = results$confluence_separation,
    signal_noise_ratio = results$signal_noise_ratio,
    retention_rate = results$retention_rate,
    retention_penalty = results$retention_penalty,
    optimization_mode = results$optimization_mode,
    scoring_mode = results$scoring_mode,
    continuity_score = results$continuity_score,
    continuity_neurite_separation = results$continuity_neurite_separation,
    total_length_separation = results$total_length_separation,
    top5_length_separation = results$top5_length_separation,
    max_length_separation = results$max_length_separation,
    length_weighted_retention = results$length_weighted_retention,
    fragmentation_penalty = results$fragmentation_penalty,
    short_fragment_fraction = results$short_fragment_fraction,
    topology_score = results$topology_score,
    dominant_path_ratio = results$dominant_path_ratio,
    mesh_penalty = results$mesh_penalty,
    junction_density_penalty = results$junction_density_penalty,
    orientation_coherence = results$orientation_coherence,
    high_neurites_mean = results$high_neurites_mean,
    low_neurites_mean = results$low_neurites_mean,
    high_conf_mean = results$high_conf_mean,
    low_conf_mean = results$low_conf_mean,
    stringsAsFactors = FALSE
  )
  
  optimization_results[[i]] <- result_row
  if (is.finite(result_row$score) && result_row$score > best_score_so_far) {
    best_score_so_far <- result_row$score
    best_index_so_far <- i
  }
  
  setTxtProgressBar(pb, i)
  if (i == 1L || i == nrow(param_grid) || i %% grid_report_every == 0L) {
    elapsed <- as.numeric(difftime(Sys.time(), grid_start, units = "secs"))
    iter_elapsed <- as.numeric(difftime(Sys.time(), iter_start, units = "secs"))
    rate <- if (elapsed > 0) i / elapsed else NA_real_
    remaining <- if (is.finite(rate) && rate > 0) (nrow(param_grid) - i) / rate else NA_real_
    report_line(
      sprintf(
        "  Grid progress: %d/%d parameter sets tested (%.1f%%) | last %.2fs | elapsed %s | ETA %s | best score so far %s%s",
        i,
        nrow(param_grid),
        100 * i / nrow(param_grid),
        iter_elapsed,
        format_elapsed(elapsed),
        format_elapsed(remaining),
        if (is.finite(best_score_so_far)) sprintf("%.4f", best_score_so_far) else "none yet",
        if (!is.na(best_index_so_far)) sprintf(" at set %d", best_index_so_far) else ""
      )
    )
  }
}
close(pb)
grid_elapsed <- as.numeric(difftime(Sys.time(), grid_start, units = "secs"))
stage_line("Grid optimization", "COMPLETE", paste0("finished in ", format_elapsed(grid_elapsed)))
report_line("Finished grid optimization in ", format_elapsed(grid_elapsed), ".")

# Combine all results into data frame
stage_line("Optimization result table", "START", "combining, filtering, and sorting parameter scores")
optimization_results <- do.call(rbind, optimization_results)

# Sort by score
optimization_results <- optimization_results %>%
  arrange(desc(score)) %>%
  filter(is.finite(score))

# Save optimization results
write_csv(optimization_results, file.path(ctx$paths$out_opt, "parameter_optimization_results.csv"))
report_line("")
report_line("Optimization results saved to: ", file.path(ctx$paths$out_opt, "parameter_optimization_results.csv"))
stage_line("Optimization result table", "COMPLETE", sprintf("%d finite-scoring parameter sets saved", nrow(optimization_results)))

# Display top 10 parameter combinations
report_line("")
report_line("=== TOP 10 PARAMETER COMBINATIONS ===")
report_line("")
top_params <- head(optimization_results, 10)
print(top_params)

# Best parameters
stage_line("Best parameter selection", "START", "selecting the highest scoring cutoff profile")
best_params <- optimization_results[1, ]
report_line("")
report_line("=== BEST PARAMETERS ===")
report_line("Score: ", round(best_params$score, 4))
report_line("Min Area: ", best_params$min_area)
report_line("Max Area: ", best_params$max_area)
report_line("Min Circularity: ", best_params$min_circularity)
report_line("Max Circularity: ", best_params$max_circularity)
report_line("Min Aspect Ratio: ", best_params$min_aspect_ratio)
report_line("Max Aspect Ratio: ", best_params$max_aspect_ratio)
report_line("")
report_line("Performance Metrics:")
report_line("  ROIs Retained: ", best_params$n_retained)
report_line("  Neurite Separation: ", round(best_params$neurite_separation, 4))
report_line("  Confluence Separation: ", round(best_params$confluence_separation, 4))
report_line("  Signal/Noise Ratio: ", if (is.na(best_params$signal_noise_ratio)) "not used in fallback mode" else round(best_params$signal_noise_ratio, 4))
report_line("  Retention Rate: ", round(100 * best_params$retention_rate, 2), "%")
report_line("  Optimization Mode: ", best_params$optimization_mode)
report_line("  Scoring Mode: ", best_params$scoring_mode)
report_line("  Continuity Score: ", round(best_params$continuity_score, 4))
report_line("  Continuity Neurite Separation: ", round(best_params$continuity_neurite_separation, 4))
report_line("  Total Length Separation: ", round(best_params$total_length_separation, 4))
report_line("  Top-5 Length Separation: ", round(best_params$top5_length_separation, 4))
report_line("  Max Length Separation: ", round(best_params$max_length_separation, 4))
report_line("  Length-Weighted Retention: ", round(best_params$length_weighted_retention, 4))
report_line("  Fragmentation Penalty: ", round(best_params$fragmentation_penalty, 6))
report_line("  Short Fragment Fraction: ", round(best_params$short_fragment_fraction, 4))
report_line("  Topology Score: ", round(best_params$topology_score, 4))
report_line("  Dominant Path Ratio: ", round(best_params$dominant_path_ratio, 4))
report_line("  Mesh Penalty: ", round(best_params$mesh_penalty, 4))
report_line("  Junction Density Penalty: ", round(best_params$junction_density_penalty, 6))
report_line("  Orientation Coherence: ", round(best_params$orientation_coherence, 4))
report_line("  High Neurites Mean Skeleton: ", round(best_params$high_neurites_mean, 2))
report_line("  Low Neurites Mean Skeleton: ", round(best_params$low_neurites_mean, 2))
stage_line("Best parameter selection", "COMPLETE", paste0("best score ", round(best_params$score, 4)))

optimization_model_metadata <- data.frame(
  key = c(
    "optimization_mode",
    "scoring_mode",
    "long_roi_threshold",
    "short_roi_threshold",
    "best_score",
    "best_min_area",
    "best_max_area",
    "best_min_circularity",
    "best_max_circularity",
    "best_min_aspect_ratio",
    "best_max_aspect_ratio",
    "best_continuity_score",
    "best_continuity_neurite_separation",
    "best_total_length_separation",
    "best_top5_length_separation",
    "best_max_length_separation",
    "best_length_weighted_retention",
    "best_fragmentation_penalty",
    "best_short_fragment_fraction",
    "best_topology_score",
    "best_dominant_path_ratio",
    "best_mesh_penalty",
    "best_junction_density_penalty",
    "best_orientation_coherence",
    "has_manual_noise",
    "optimizer_script"
  ),
  value = c(
    optimization_mode,
    best_params$scoring_mode,
    long_roi_threshold,
    short_roi_threshold,
    best_params$score,
    best_params$min_area,
    best_params$max_area,
    best_params$min_circularity,
    best_params$max_circularity,
    best_params$min_aspect_ratio,
    best_params$max_aspect_ratio,
    best_params$continuity_score,
    best_params$continuity_neurite_separation,
    best_params$total_length_separation,
    best_params$top5_length_separation,
    best_params$max_length_separation,
    best_params$length_weighted_retention,
    best_params$fragmentation_penalty,
    best_params$short_fragment_fraction,
    best_params$topology_score,
    best_params$dominant_path_ratio,
    best_params$mesh_penalty,
    best_params$junction_density_penalty,
    best_params$orientation_coherence,
    has_manual_noise,
    "1.OptimizeNeuriteParams-MAJOR_OPTIMIZE.R"
  ),
  stringsAsFactors = FALSE
)
write_csv(optimization_model_metadata, file.path(ctx$paths$out_opt, "optimization_model_metadata.csv"))
report_line("Optimization model metadata saved to: ", file.path(ctx$paths$out_opt, "optimization_model_metadata.csv"))

write_input_diag(
  "completed",
  paste0(optimization_mode, " optimization completed and selected the best cutoff profile."),
  list(
    optroi_object_count = length(optROIs),
    roi_analysis_rows = nrow(roi_analysis),
    signal_rows = sum(roi_analysis$is_signal),
    noise_rows = sum(roi_analysis$is_noise),
    optimization_mode = optimization_mode,
    scoring_mode = best_params$scoring_mode,
    best_score = best_params$score,
    best_length_weighted_retention = best_params$length_weighted_retention,
    best_fragmentation_penalty = best_params$fragmentation_penalty,
    best_short_fragment_fraction = best_params$short_fragment_fraction,
    best_topology_score = best_params$topology_score,
    best_dominant_path_ratio = best_params$dominant_path_ratio,
    best_mesh_penalty = best_params$mesh_penalty,
    best_junction_density_penalty = best_params$junction_density_penalty,
    best_orientation_coherence = best_params$orientation_coherence
  )
)

# Visualization: Score vs parameters
stage_line("Optimization plots", "START", "saving score and separation plots")
p1 <- ggplot(optimization_results %>% head(50), 
             aes(x = min_area, y = score, color = min_circularity)) +
  geom_point(size = 3) +
  labs(title = "Optimization Score vs Min Area",
       x = "Minimum Area", y = "Score", color = "Min Circularity") +
  theme_minimal()

ggsave(file.path(ctx$paths$out_opt, "optimization_score_vs_area.png"), p1, width = 10, height = 6)

p2 <- ggplot(optimization_results %>% head(50), 
             aes(x = neurite_separation, y = signal_noise_ratio, 
                 size = score, color = confluence_separation)) +
  geom_point(alpha = 0.6) +
  labs(title = "Neurite Separation vs Signal/Noise Ratio",
       x = "Neurite Separation", y = "Signal/Noise Ratio",
       size = "Score", color = "Confluence\nSeparation") +
  theme_minimal()

ggsave(file.path(ctx$paths$out_opt, "optimization_scatter.png"), p2, width = 10, height = 6)

report_line("")
report_line("Visualization plots saved to ", ctx$paths$out_opt)
stage_line("Optimization plots", "COMPLETE", "optimization_score_vs_area.png and optimization_scatter.png saved")

# Apply best parameters and save filtered dataset
stage_line("Best cutoff application", "START", "filtering ROI table with selected parameters")
if (identical(optimization_mode, "ensemble_union_continuity_topology")) {
  report_line("Ensemble union mode: building final retained ROI table as continuity-aware UNION topology-aware selections.")
  continuity_component_results <- score_parameter_grid_for_union_component(evaluate_parameters_continuity, "continuity_aware")
  topology_component_results <- score_parameter_grid_for_union_component(evaluate_parameters_topology, "topology_aware_continuity")

  if (nrow(continuity_component_results) && nrow(topology_component_results)) {
    continuity_best_params <- continuity_component_results[1, ]
    topology_best_params <- topology_component_results[1, ]
    continuity_rois <- filter_rois_by_params(roi_analysis, continuity_best_params)
    topology_rois <- filter_rois_by_params(roi_analysis, topology_best_params)
    continuity_rois$ensemble_component <- "continuity_aware"
    topology_rois$ensemble_component <- "topology_aware_continuity"
    union_source <- rbind(continuity_rois, topology_rois)
    union_membership <- aggregate(
      ensemble_component ~ .roi_row_id,
      data = union_source[, c(".roi_row_id", "ensemble_component"), drop = FALSE],
      FUN = function(x) paste(sort(unique(x)), collapse = "+")
    )
    best_filtered_rois <- roi_analysis[roi_analysis$.roi_row_id %in% union_membership$.roi_row_id, , drop = FALSE] %>%
      left_join(union_membership, by = ".roi_row_id")
    write_csv(
      rbind(continuity_component_results[1, ], topology_component_results[1, ]),
      file.path(ctx$paths$out_opt, "ensemble_union_component_best_params.csv")
    )
    report_line("Ensemble union component parameters saved to: ", file.path(ctx$paths$out_opt, "ensemble_union_component_best_params.csv"))
    report_line("  Continuity component retained: ", nrow(continuity_rois), " ROI rows")
    report_line("  Topology component retained: ", nrow(topology_rois), " ROI rows")
    report_line("  Union retained: ", nrow(best_filtered_rois), " unique ROI rows")
  } else {
    report_line("WARNING: Ensemble union component scoring did not produce both finite component models. Falling back to the ensemble-best single cutoff window.")
    best_filtered_rois <- filter_rois_by_params(roi_analysis, best_params)
    best_filtered_rois$ensemble_component <- "ensemble_single_window_fallback"
  }
} else {
  best_filtered_rois <- filter_rois_by_params(roi_analysis, best_params)
}

write_csv(best_filtered_rois, file.path(ctx$paths$out_opt, "filtered_rois_best_params.csv"))
report_line("")
report_line("Filtered ROIs with best parameters saved to: ", file.path(ctx$paths$out_opt, "filtered_rois_best_params.csv"))
stage_line("Best cutoff application", "COMPLETE", sprintf("%d of %d ROI rows retained", nrow(best_filtered_rois), nrow(roi_analysis)))

# Summary statistics
stage_line("Filtered output summary", "START", "summarizing retained signal ROIs by neurite group")
report_line("")
report_line("=== FILTERED DATA SUMMARY ===")
report_line("Original ROIs: ", nrow(roi_analysis))
report_line("Filtered ROIs: ", nrow(best_filtered_rois))
report_line("Retention rate: ", round(100 * nrow(best_filtered_rois) / nrow(roi_analysis), 2), "%")

summary_by_group <- best_filtered_rois %>%
  filter(is_signal) %>%
  group_by(is_high_neurites, is_low_neurites) %>%
  summarise(
    n = n(),
    mean_skel_length = mean(skeleton_length, na.rm = TRUE),
    sd_skel_length = sd(skeleton_length, na.rm = TRUE),
    .groups = "drop"
  )

report_line("")
report_line("Skeleton Length by Neurite Group (Signal only):")
print(summary_by_group)
stage_line("Filtered output summary", "COMPLETE", "signal-group summary printed")

report_line("")
stage_line("2.A.6. Optimize Neurite Thresholds", "COMPLETE", "all optimization substages finished")
report_line("=== OPTIMIZATION COMPLETE ===")
