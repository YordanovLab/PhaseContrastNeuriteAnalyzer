#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(RImageJROI)
})

args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[[1]] else getwd()
profile_dirs <- if (length(args) >= 2) args[-1] else character()
setwd(base_dir)

source(file.path(base_dir, "launchers", "pipeline_paths.R"))
ctx <- pipeline_context(base_dir)

out_dir <- file.path(ctx$paths$output_root, "OUT_generalized_analysis")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

report_file <- file.path(out_dir, "apply_cutoff_profile_report.txt")
if (file.exists(report_file)) invisible(file.remove(report_file))

report_line <- function(...) {
  msg <- paste(..., collapse = "")
  cat(msg, "\n")
  write(msg, file = report_file, append = TRUE)
  flush.console()
}

format_elapsed <- function(seconds) {
  seconds <- as.numeric(seconds)
  if (!is.finite(seconds)) return("unknown")
  if (seconds < 60) return(sprintf("%.1fs", seconds))
  sprintf("%dm %.1fs", floor(seconds / 60), seconds %% 60)
}

normalize_image_name <- function(x) {
  x <- sub("\\.(tif|tiff|png|jpg|jpeg)$", "", basename(x), ignore.case = TRUE)
  x <- sub("_RGavg_mask_renorm$", "", x)
  x <- sub("_RGavg_mask$", "", x)
  x
}

read_mask_area_features <- function(mask_path) {
  if (!file.exists(mask_path)) return(data.frame())
  mask <- tryCatch(read_csv(mask_path, show_col_types = FALSE), error = function(e) data.frame())
  if (!nrow(mask)) return(mask)
  names(mask) <- make.unique(ifelse(nzchar(names(mask)), names(mask), "unnamed_mask_column"))
  image_col <- if ("image_name" %in% names(mask)) {
    "image_name"
  } else {
    candidates <- names(mask)[grepl("image|file|name", names(mask), ignore.case = TRUE)]
    if (length(candidates)) candidates[[1]] else names(mask)[[1]]
  }
  numeric_copy <- function(column_name) {
    if (nzchar(column_name) && column_name %in% names(mask)) suppressWarnings(as.numeric(mask[[column_name]])) else NA_real_
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
    .image_key = normalize_image_name(mask[[image_col]]),
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
  out[!duplicated(out$.image_key), , drop = FALSE]
}

extract_roi_properties <- function(roi_data) {
  properties <- list(area = NA_real_, perimeter = NA_real_, circularity = NA_real_, aspect_ratio = NA_real_, solidity = NA_real_, mean_width = NA_real_)
  tryCatch({
    if (!is.null(roi_data$coords)) {
      coords <- roi_data$coords
      n_points <- nrow(coords)
      if (n_points >= 3) {
        x <- coords[, 1]
        y <- coords[, 2]
        area <- 0.5 * abs(sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y))
        properties$area <- area
        dx <- diff(c(x, x[1]))
        dy <- diff(c(y, y[1]))
        perimeter <- sum(sqrt(dx^2 + dy^2))
        properties$perimeter <- perimeter
        if (perimeter > 0) properties$circularity <- (4 * pi * area) / (perimeter^2)
        width <- max(x) - min(x)
        height <- max(y) - min(y)
        if (height > 0) properties$aspect_ratio <- width / height
        properties$mean_width <- mean(c(width, height))
        if (width > 0 && height > 0) properties$solidity <- area / (width * height)
      }
    }
  }, error = function(e) {
    warning("Error extracting ROI properties: ", e$message)
  })
  properties
}

calculate_skeleton_metrics <- function(roi_data) {
  metrics <- list(total_length = 0, branch_points = 0, endpoints = 0, avg_branch_length = 0, path_straightness = NA_real_, orientation_coherence = NA_real_)
  tryCatch({
    if (!is.null(roi_data$coords)) {
      coords <- roi_data$coords
      n_points <- nrow(coords)
      if (n_points >= 2) {
        dx <- diff(coords[, 1])
        dy <- diff(coords[, 2])
        segment_lengths <- sqrt(dx^2 + dy^2)
        metrics$total_length <- sum(segment_lengths)
        metrics$endpoints <- 2
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
  metrics
}

add_topology_proxy_metrics <- function(df) {
  numeric_cols <- c("area", "perimeter", "circularity", "aspect_ratio", "solidity", "mean_width", "skeleton_length", "branch_points", "endpoints", "avg_branch_length", "path_straightness", "orientation_coherence")
  for (col in numeric_cols) {
    if (!col %in% names(df)) df[[col]] <- NA_real_
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  df$path_straightness <- ifelse(is.finite(df$path_straightness), pmin(1, pmax(0, df$path_straightness)), pmin(1, pmax(0, df$aspect_ratio / (df$aspect_ratio + 1))))
  df$orientation_coherence <- ifelse(is.finite(df$orientation_coherence), pmin(1, pmax(0, df$orientation_coherence)), df$path_straightness)
  df$branch_density <- df$branch_points / (df$skeleton_length + 1)
  df$endpoint_density <- df$endpoints / (df$skeleton_length + 1)
  df$area_length_ratio <- df$area / (df$skeleton_length + 1)
  df$perimeter_length_ratio <- df$perimeter / (df$skeleton_length + 1)
  df$dominant_path_ratio <- pmin(1, pmax(0, 0.45 * df$path_straightness + 0.35 * df$orientation_coherence + 0.20 * pmin(1, df$skeleton_length / (df$perimeter + 1))))
  df$meshness_score <- log1p(pmax(0, df$area_length_ratio)) +
    log1p(pmax(0, df$perimeter_length_ratio)) +
    log1p(pmax(0, df$branch_density) * 100) +
    log1p(pmax(0, df$endpoint_density - 0.1) * 100) +
    0.35 * pmax(0, df$circularity) +
    0.25 * pmax(0, df$solidity) -
    0.50 * pmax(0, df$dominant_path_ratio) -
    0.25 * log1p(pmax(0, df$aspect_ratio))
  df$meshness_score[!is.finite(df$meshness_score)] <- NA_real_
  df
}

read_csv_if_exists <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return(data.frame())
  tryCatch(read_csv(path, show_col_types = FALSE), error = function(e) data.frame())
}

if (!length(profile_dirs)) profile_dirs <- ""

profile_root_for_arg <- function(profile_dir) {
  if (nzchar(profile_dir)) profile_dir else ctx$paths$out_opt
}

profile_label_for_arg <- function(profile_dir) {
  if (nzchar(profile_dir)) basename(profile_dir) else "current_working_outputs"
}

metadata_value <- function(model_metadata, key, fallback = "") {
  if (!nrow(model_metadata) || !all(c("key", "value") %in% names(model_metadata))) return(fallback)
  hit <- model_metadata$value[model_metadata$key == key]
  if (length(hit) && nzchar(hit[[1]])) hit[[1]] else fallback
}

read_profile_spec <- function(profile_dir) {
  profile_root <- profile_root_for_arg(profile_dir)
  profile_label <- profile_label_for_arg(profile_dir)
  params_path <- file.path(profile_root, "parameter_optimization_results.csv")
  component_path <- file.path(profile_root, "ensemble_union_component_best_params.csv")
  metadata_path <- file.path(profile_root, "optimization_model_metadata.csv")
  params <- read_csv_if_exists(params_path)
  components <- read_csv_if_exists(component_path)
  model_metadata <- read_csv_if_exists(metadata_path)
  if (!nrow(params)) {
    stop("No parameter_optimization_results.csv found for selected cutoff profile: ", params_path)
  }
  if ("score" %in% names(params)) {
    params$score <- suppressWarnings(as.numeric(params$score))
    params <- params[order(params$score, decreasing = TRUE), , drop = FALSE]
  }
  best_params <- params[1, , drop = FALSE]
  scoring_mode <- metadata_value(model_metadata, "scoring_mode", if ("scoring_mode" %in% names(best_params)) best_params$scoring_mode[[1]] else "")
  optimization_mode <- metadata_value(model_metadata, "optimization_mode", if ("optimization_mode" %in% names(best_params)) best_params$optimization_mode[[1]] else "")
  list(
    root = profile_root,
    label = profile_label,
    params = params,
    components = components,
    metadata = model_metadata,
    best_params = best_params,
    scoring_mode = scoring_mode,
    optimization_mode = optimization_mode
  )
}

profile_specs <- lapply(profile_dirs, read_profile_spec)

filter_by_params <- function(df, p) {
  required <- c("min_area", "max_area", "min_circularity", "max_circularity", "min_aspect_ratio", "max_aspect_ratio")
  if (!all(required %in% names(p))) return(rep(FALSE, nrow(df)))
  min_area <- suppressWarnings(as.numeric(p$min_area[[1]]))
  max_area <- suppressWarnings(as.numeric(p$max_area[[1]]))
  min_circularity <- suppressWarnings(as.numeric(p$min_circularity[[1]]))
  max_circularity <- suppressWarnings(as.numeric(p$max_circularity[[1]]))
  min_aspect_ratio <- suppressWarnings(as.numeric(p$min_aspect_ratio[[1]]))
  max_aspect_ratio <- suppressWarnings(as.numeric(p$max_aspect_ratio[[1]]))
  if (!is.finite(max_area)) max_area <- Inf
  if (!is.finite(max_aspect_ratio)) max_aspect_ratio <- Inf
  df$area >= min_area & df$area <= max_area &
    df$circularity >= min_circularity & df$circularity <= max_circularity &
    df$aspect_ratio >= min_aspect_ratio & df$aspect_ratio <= max_aspect_ratio
}

apply_model <- function(df, spec) {
  if (grepl("ensemble_union", spec$scoring_mode, ignore.case = TRUE) && nrow(spec$components)) {
    keep <- rep(FALSE, nrow(df))
    component_labels <- rep("", nrow(df))
    for (i in seq_len(nrow(spec$components))) {
      hit <- filter_by_params(df, spec$components[i, , drop = FALSE])
      keep <- keep | hit
      component_name <- if ("component" %in% names(spec$components)) spec$components$component[[i]] else paste0("component_", i)
      component_labels[hit] <- ifelse(nzchar(component_labels[hit]), paste(component_labels[hit], component_name, sep = "|"), component_name)
    }
    list(keep = keep, component = component_labels)
  } else {
    keep <- filter_by_params(df, spec$best_params)
    list(keep = keep, component = ifelse(keep, spec$scoring_mode, ""))
  }
}

roi_root <- ctx$paths$roi_root
if (!dir.exists(roi_root)) stop("ROI root folder is missing: ", roi_root)
roi_dirs <- list.dirs(roi_root, recursive = FALSE, full.names = TRUE)
roi_dirs <- roi_dirs[dir.exists(roi_dirs)]
if (!length(roi_dirs)) stop("No ROI image folders found under: ", roi_root)
roi_file_counts <- vapply(roi_dirs, function(d) length(list.files(d, pattern = "\\.roi$", full.names = FALSE)), numeric(1))

production_raw_csv <- file.path(out_dir, "all_images_roi_analysis_raw.csv")
production_raw_rds <- file.path(out_dir, "all_images_roi_analysis_raw.rds")
production_manifest <- file.path(out_dir, "all_images_roi_analysis_manifest.csv")
filtered_csv <- file.path(out_dir, "all_images_filtered_rois.csv")
features_csv <- file.path(out_dir, "generalized_model_feature_table.csv")
features_rds <- file.path(out_dir, "generalized_model_feature_table.rds")

current_manifest <- data.frame(
  roi_root = normalizePath(roi_root, winslash = "/", mustWork = FALSE),
  roi_folder_count = length(roi_dirs),
  roi_folder_signature = paste(sort(basename(roi_dirs)), collapse = "|"),
  stringsAsFactors = FALSE
)

use_cache <- FALSE
roi_analysis <- NULL
if (file.exists(production_raw_rds) && file.exists(production_manifest)) {
  old_manifest <- tryCatch(read.csv(production_manifest, stringsAsFactors = FALSE), error = function(e) data.frame())
  if (nrow(old_manifest) && identical(old_manifest$roi_folder_count[[1]], current_manifest$roi_folder_count[[1]]) &&
      identical(old_manifest$roi_folder_signature[[1]], current_manifest$roi_folder_signature[[1]])) {
    roi_analysis <- tryCatch(readRDS(production_raw_rds), error = function(e) NULL)
    use_cache <- is.data.frame(roi_analysis) && nrow(roi_analysis)
  }
}

report_line("=== APPLY CUTOFF PROFILE TO ALL IMAGES ===")
report_line("Base directory: ", base_dir)
report_line("ROI root: ", roi_root)
report_line("ROI image folders: ", length(roi_dirs))
report_line("Profiles selected: ", paste(vapply(profile_specs, `[[`, character(1), "label"), collapse = ", "))
report_line("Scoring modes: ", paste(vapply(profile_specs, `[[`, character(1), "scoring_mode"), collapse = ", "))
report_line("Optimization modes: ", paste(vapply(profile_specs, `[[`, character(1), "optimization_mode"), collapse = ", "))
report_line("Output directory: ", out_dir)

if (use_cache) {
  report_line("[PRODUCTION APPLY] Reusing cached all-image ROI analysis table: ", nrow(roi_analysis), " ROI rows")
} else {
  report_line("[PRODUCTION APPLY] Building all-image ROI analysis table from ROI folders")
  all_rows <- list()
  start_time <- Sys.time()
  cumulative_rois <- 0L
  for (i in seq_along(roi_dirs)) {
    image_start <- Sys.time()
    roi_dir <- roi_dirs[[i]]
    roi_files <- list.files(roi_dir, pattern = "\\.roi$", full.names = TRUE)
    image_name <- normalize_image_name(basename(roi_dir))
    image_rows <- vector("list", length(roi_files))
    if (length(roi_files)) {
      for (j in seq_along(roi_files)) {
        roi_file <- roi_files[[j]]
        roi_data <- tryCatch(read.ijroi(roi_file), error = function(e) NULL)
        if (is.null(roi_data)) next
        props <- extract_roi_properties(roi_data)
        skel <- calculate_skeleton_metrics(roi_data)
        image_rows[[j]] <- data.frame(
          image_name = image_name,
          roi_name = basename(roi_file),
          area = props$area,
          perimeter = props$perimeter,
          circularity = props$circularity,
          aspect_ratio = props$aspect_ratio,
          solidity = props$solidity,
          mean_width = props$mean_width,
          skeleton_length = skel$total_length,
          branch_points = skel$branch_points,
          endpoints = skel$endpoints,
          avg_branch_length = skel$avg_branch_length,
          path_straightness = skel$path_straightness,
          orientation_coherence = skel$orientation_coherence,
          stringsAsFactors = FALSE
        )
      }
    }
    image_rows <- Filter(Negate(is.null), image_rows)
    if (length(image_rows)) {
      all_rows[[length(all_rows) + 1]] <- bind_rows(image_rows)
    }
    cumulative_rois <- cumulative_rois + length(roi_files)
    elapsed <- as.numeric(difftime(Sys.time(), image_start, units = "secs"))
    total_elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
    rate <- if (total_elapsed > 0) cumulative_rois / total_elapsed else NA_real_
    remaining_rois <- if (i < length(roi_dirs)) sum(roi_file_counts[(i + 1):length(roi_dirs)], na.rm = TRUE) else 0
    eta <- if (is.finite(rate) && rate > 0) {
      remaining_rois / rate
    } else {
      NA_real_
    }
    report_line(sprintf(
      "  Production image %d/%d: %s - %d ROI files in %s | cumulative %d ROIs | ETA %s",
      i, length(roi_dirs), image_name, length(roi_files), format_elapsed(elapsed), cumulative_rois, format_elapsed(eta)
    ))
  }
  roi_analysis <- if (length(all_rows)) bind_rows(all_rows) else data.frame()
  roi_analysis <- roi_analysis %>%
    filter(!is.na(area), !is.na(circularity), !is.na(skeleton_length)) %>%
    add_topology_proxy_metrics()
  roi_analysis$.roi_row_id <- seq_len(nrow(roi_analysis))
  write_csv(roi_analysis, production_raw_csv)
  saveRDS(roi_analysis, production_raw_rds)
  current_manifest$created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  current_manifest$roi_analysis_rows <- nrow(roi_analysis)
  write.csv(current_manifest, production_manifest, row.names = FALSE)
  report_line("[PRODUCTION APPLY] Saved all-image ROI analysis cache: ", production_raw_csv)
}

if (!nrow(roi_analysis)) stop("No all-image ROI rows were available after reading ROI folders.")

all_filtered <- list()
all_image_features <- list()

for (spec in profile_specs) {
  report_line("[PRODUCTION APPLY] Applying profile: ", spec$label)
  model_result <- apply_model(roi_analysis, spec)
  retained_by_model <- model_result$keep
  retained_component <- model_result$component

  filtered <- roi_analysis[retained_by_model, , drop = FALSE]
  if (nrow(filtered)) {
    filtered$retained_by_model <- TRUE
    filtered$retained_component <- retained_component[retained_by_model]
    filtered$profile_id <- spec$label
    filtered$scoring_mode <- spec$scoring_mode
    filtered$optimization_mode <- spec$optimization_mode
    all_filtered[[length(all_filtered) + 1]] <- filtered
  }

  tmp <- roi_analysis
  tmp$retained_by_model <- retained_by_model
  tmp$retained_component <- retained_component

  image_features_one <- tmp %>%
    group_by(image_name) %>%
    summarise(
      profile_id = .env$spec$label,
      profile_label = .env$spec$label,
      scoring_mode = .env$spec$scoring_mode,
      optimization_mode = .env$spec$optimization_mode,
      candidate_roi_count = n(),
      retained_roi_count = sum(retained_by_model, na.rm = TRUE),
      retained_roi_fraction = retained_roi_count / candidate_roi_count,
      image_total_skeleton_length = sum(skeleton_length[retained_by_model], na.rm = TRUE),
      image_mean_skeleton_length = mean(skeleton_length[retained_by_model], na.rm = TRUE),
      image_median_skeleton_length = median(skeleton_length[retained_by_model], na.rm = TRUE),
      roi_mean_skeleton_length = image_mean_skeleton_length,
      roi_median_skeleton_length = image_median_skeleton_length,
      roi_mean_area = mean(area[retained_by_model], na.rm = TRUE),
      roi_mean_circularity = mean(circularity[retained_by_model], na.rm = TRUE),
      roi_mean_aspect_ratio = mean(aspect_ratio[retained_by_model], na.rm = TRUE),
      mean_dominant_path_ratio = mean(dominant_path_ratio[retained_by_model], na.rm = TRUE),
      mean_meshness_score = mean(meshness_score[retained_by_model], na.rm = TRUE),
      .groups = "drop"
    )
  all_image_features[[length(all_image_features) + 1]] <- image_features_one
  report_line("[PRODUCTION APPLY] Profile ", spec$label, " retained ROI rows: ", sum(retained_by_model, na.rm = TRUE), " / ", nrow(roi_analysis))
}

filtered <- if (length(all_filtered)) bind_rows(all_filtered) else data.frame()
image_features <- bind_rows(all_image_features)
write_csv(filtered, filtered_csv)

mask_features <- read_mask_area_features(ctx$paths$mask_percentages)
if (nrow(mask_features)) {
  image_features$.image_key <- normalize_image_name(image_features$image_name)
  add_cols <- setdiff(names(mask_features), names(image_features))
  if (length(add_cols)) {
    image_features <- left_join(image_features, mask_features[, c(".image_key", add_cols), drop = FALSE], by = ".image_key")
    report_line("[PRODUCTION APPLY] Joined image-level mask area features from: ", ctx$paths$mask_percentages)
  }
} else {
  report_line("[PRODUCTION APPLY] No mask-area summary table was available to join: ", ctx$paths$mask_percentages)
}

confluence_for_composites <- if ("image_confluence" %in% names(image_features)) {
  suppressWarnings(as.numeric(image_features$image_confluence))
} else if ("image_confluence_mask_area" %in% names(image_features)) {
  suppressWarnings(as.numeric(image_features$image_confluence_mask_area))
} else if ("confluence" %in% names(image_features)) {
  suppressWarnings(as.numeric(image_features$confluence))
} else {
  rep(NA_real_, nrow(image_features))
}
confluence_for_composites[!is.finite(confluence_for_composites) | confluence_for_composites <= 0] <- NA_real_
if ("image_total_skeleton_length" %in% names(image_features)) {
  image_features$image_total_skeleton_length_per_confluence <- suppressWarnings(as.numeric(image_features$image_total_skeleton_length)) / confluence_for_composites
}
if ("image_median_skeleton_length" %in% names(image_features)) {
  image_features$image_median_skeleton_length_per_confluence <- suppressWarnings(as.numeric(image_features$image_median_skeleton_length)) / confluence_for_composites
}

metadata_path <- ctx$paths$metadata
if (file.exists(metadata_path)) {
  metadata <- tryCatch(read_csv(metadata_path, show_col_types = FALSE), error = function(e) data.frame())
  if (nrow(metadata)) {
    names(metadata) <- make.unique(ifelse(nzchar(names(metadata)), names(metadata), "unnamed_metadata_column"))
    image_col <- if ("image_name" %in% names(metadata)) "image_name" else {
      candidates <- names(metadata)[grepl("image|file|name", names(metadata), ignore.case = TRUE)]
      if (length(candidates)) candidates[[1]] else names(metadata)[[1]]
    }
    metadata$metadata_image_name <- metadata[[image_col]]
    metadata$.image_key <- normalize_image_name(metadata[[image_col]])
    metadata <- metadata[, setdiff(names(metadata), "image_name"), drop = FALSE]
    image_features$.image_key <- normalize_image_name(image_features$image_name)
    image_features <- left_join(image_features, metadata, by = ".image_key")
  }
}

if (!"image_name" %in% names(image_features) && "image_name.x" %in% names(image_features)) {
  image_features$image_name <- image_features$image_name.x
}

write_csv(image_features, features_csv)
saveRDS(image_features, features_rds)

report_line("[PRODUCTION APPLY] Retained ROI rows across all selected profiles: ", nrow(filtered), " / ", nrow(roi_analysis), " base ROI rows")
report_line("[PRODUCTION APPLY] Saved all-image filtered ROIs: ", filtered_csv)
report_line("[PRODUCTION APPLY] Saved image-level feature table: ", features_csv)
report_line("[PRODUCTION APPLY] Done.")
