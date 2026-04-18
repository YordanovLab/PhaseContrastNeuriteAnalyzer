# =============================================================
# Optimized ROI + Skeleton Analysis (uses internal allROIs metadata)
# =============================================================

suppressPackageStartupMessages({
  library(RImageJROI)
  library(dplyr)
  library(magick)
  library(ggplot2)
  library(readr)
  library(parallel)
})

# ========= SETTINGS =========
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)
source(file.path(base_dir, "launchers", "pipeline_paths.R"))
ctx <- pipeline_context(base_dir)
dir.create(ctx$paths$out_opt, recursive = TRUE, showWarnings = FALSE)

CHUNK_SIZE     <- 20
CHECKPOINT_DIR <- file.path(ctx$paths$out_opt, "checkpoints")
FINAL_OUTPUT   <- ctx$paths$all_rois_analysis_skeletons

detected   <- max(1, parallel::detectCores() - 1)
N_CORES    <- min(detected, 50)
USE_FORKS  <- .Platform$OS.type != "windows"  # TRUE on Ubuntu

cat(sprintf("Using %d workers (%s)\n",
            N_CORES, if (USE_FORKS) "mclapply (forks)" else "PSOCK cluster"))

if (!dir.exists(CHECKPOINT_DIR)) dir.create(CHECKPOINT_DIR, recursive = TRUE)

# ========= LOAD DATA =========
cat("=== LOADING allROIs ===\n")
all_rois_source <- if (file.exists(ctx$paths$all_rois_data)) ctx$paths$all_rois_data else ctx$paths$opt_rois_rds
allROIs <- readRDS(all_rois_source)
cat("Loaded", length(allROIs), "image entries\n\n")

# ========= FUNCTIONS =========

# Fast ROI geometric features
extract_roi_properties <- function(roi_data) {
  properties <- list(
    area = NA_real_, perimeter = NA_real_, circularity = NA_real_,
    aspect_ratio = NA_real_, solidity = NA_real_, mean_width = NA_real_
  )
  if (is.null(roi_data$coords)) return(properties)
  coords <- roi_data$coords
  n_points <- nrow(coords); if (n_points < 3) return(properties)
  
  x <- coords[,1]; y <- coords[,2]
  x_shift <- c(x[-1], x[1]); y_shift <- c(y[-1], y[1])
  
  area <- 0.5 * abs(sum(x * y_shift - x_shift * y))
  properties$area <- area
  
  dx <- x_shift - x; dy <- y_shift - y
  perimeter <- sum(sqrt(dx^2 + dy^2))
  properties$perimeter <- perimeter
  
  if (perimeter > 0) properties$circularity <- (4 * pi * area) / (perimeter^2)
  
  x_range <- range(x); y_range <- range(y)
  width <- x_range[2] - x_range[1]; height <- y_range[2] - y_range[1]
  if (height > 0) properties$aspect_ratio <- width / height
  properties$mean_width <- (width + height) / 2
  
  if (width > 0 && height > 0) properties$solidity <- area / (width * height)
  
  properties
}

# Simplified skeleton metrics
calculate_skeleton_metrics <- function(roi_data) {
  metrics <- list(total_length = 0, branch_points = 0, endpoints = 0, avg_branch_length = 0)
  if (is.null(roi_data$coords)) return(metrics)
  coords <- roi_data$coords
  n_points <- nrow(coords); if (n_points < 2) return(metrics)
  
  dx <- diff(coords[,1]); dy <- diff(coords[,2])
  seg_len <- sqrt(dx^2 + dy^2)
  
  metrics$total_length <- sum(seg_len)
  metrics$endpoints <- 2
  metrics$avg_branch_length <- mean(seg_len)
  metrics
}

# Process one image (metadata from allROIs$...$metadata)
process_single_image <- function(img_key, img_data) {
  metadata <- img_data$metadata
  img_name <- metadata$image_name
  
  roi_results <- NULL
  if (length(img_data$rois) > 0) {
    roi_names <- names(img_data$rois)
    n_rois <- length(roi_names)
    
    areas <- numeric(n_rois); perimeters <- numeric(n_rois); circularities <- numeric(n_rois)
    aspect_ratios <- numeric(n_rois); solidities <- numeric(n_rois); mean_widths <- numeric(n_rois)
    skeleton_lengths <- numeric(n_rois); branch_points <- integer(n_rois)
    endpoints <- integer(n_rois); avg_branch_lengths <- numeric(n_rois)
    
    for (i in seq_along(roi_names)) {
      roi_obj <- img_data$rois[[roi_names[i]]]
      props <- extract_roi_properties(roi_obj)
      skel  <- calculate_skeleton_metrics(roi_obj)
      
      areas[i] <- props$area; perimeters[i] <- props$perimeter
      circularities[i] <- props$circularity; aspect_ratios[i] <- props$aspect_ratio
      solidities[i] <- props$solidity; mean_widths[i] <- props$mean_width
      skeleton_lengths[i] <- skel$total_length; branch_points[i] <- skel$branch_points
      endpoints[i] <- skel$endpoints; avg_branch_lengths[i] <- skel$avg_branch_length
    }
    
    # Flatten metadata and attach to ROI metrics
    meta_df <- as.data.frame(t(unlist(metadata)), stringsAsFactors = FALSE)
    meta_df$image_key <- img_key
    meta_repeated <- meta_df[rep(1, n_rois), , drop = FALSE]
    
    roi_df <- data.frame(
      roi_name = roi_names,
      area = areas,
      perimeter = perimeters,
      circularity = circularities,
      aspect_ratio = aspect_ratios,
      solidity = solidities,
      mean_width = mean_widths,
      skeleton_length = skeleton_lengths,
      branch_points = branch_points,
      endpoints = endpoints,
      avg_branch_length = avg_branch_lengths,
      stringsAsFactors = FALSE
    )
    
    roi_results <- cbind(meta_repeated, roi_df)
  }
  
  roi_results
}

# ========= CHECKPOINT RESUME =========
cat("=== CHECKING FOR EXISTING CHECKPOINTS ===\n")
checkpoint_files <- list.files(CHECKPOINT_DIR, pattern = "^chunk_\\d+\\.rds$", full.names = TRUE)

if (length(checkpoint_files) > 0) {
  chunk_nums <- as.integer(gsub(".*chunk_(\\d+)\\.rds", "\\1", checkpoint_files))
  max_chunk <- max(chunk_nums)
  
  existing_data <- lapply(checkpoint_files, readRDS)
  existing_df <- do.call(rbind, existing_data)
  processed_images <- unique(existing_df$image_key)
  
  cat(sprintf("Found %d existing checkpoints\n", length(checkpoint_files)))
  cat(sprintf("Already processed: %d images\n", length(processed_images)))
  cat(sprintf("Starting from chunk: %d\n\n", max_chunk + 1))
  
  remaining_keys <- setdiff(names(allROIs), processed_images)
  start_chunk <- max_chunk + 1
} else {
  cat("No existing checkpoints found. Starting fresh.\n\n")
  existing_df <- NULL
  remaining_keys <- names(allROIs)
  start_chunk <- 1
}

# ========= PROCESSING =========
cat("=== PROCESSING ROIs IN PARALLEL ===\n")
cat(sprintf("Images to process: %d\n", length(remaining_keys)))
cat(sprintf("Chunk size: %d images\n", CHUNK_SIZE))
cat(sprintf("Workers: %d (%s)\n\n", N_CORES, if (USE_FORKS) "forks" else "PSOCK"))

n_images <- length(remaining_keys)
n_chunks <- ceiling(n_images / CHUNK_SIZE)

start_time <- Sys.time()

for (chunk_idx in seq_len(n_chunks)) {
  chunk_num <- start_chunk + chunk_idx - 1
  chunk_start <- (chunk_idx - 1) * CHUNK_SIZE + 1
  chunk_end   <- min(chunk_idx * CHUNK_SIZE, n_images)
  chunk_keys  <- remaining_keys[chunk_start:chunk_end]
  
  cat(sprintf("Processing chunk %d/%d (images %d–%d)...\n",
              chunk_num, start_chunk + n_chunks - 1, chunk_start, chunk_end))
  chunk_start_time <- Sys.time()
  
  chunk_data <- lapply(chunk_keys, function(key) list(key = key, data = allROIs[[key]]))
  
  chunk_results <- parallel::mclapply(
    chunk_data,
    function(item) process_single_image(item$key, item$data),
    mc.cores = N_CORES
  )
  
  chunk_results <- chunk_results[!vapply(chunk_results, is.null, logical(1))]
  if (length(chunk_results) > 0) {
    chunk_df <- do.call(rbind, chunk_results)
    checkpoint_file <- file.path(CHECKPOINT_DIR, sprintf("chunk_%04d.rds", chunk_num))
    saveRDS(chunk_df, checkpoint_file)
    
    chunk_time <- as.numeric(difftime(Sys.time(), chunk_start_time, units = "secs"))
    cat(sprintf("  Chunk completed in %.1f seconds | %d ROIs | Saved %s\n\n",
                chunk_time, nrow(chunk_df), checkpoint_file))
  } else {
    cat("  Chunk completed but contained no ROIs.\n\n")
  }
}

elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
cat(sprintf("All chunks processed in %.1f minutes\n\n", elapsed_time))

# ========= COMBINE ALL CHUNKS =========
cat("=== COMBINING ALL CHUNKS ===\n")
all_checkpoint_files <- list.files(CHECKPOINT_DIR, pattern = "^chunk_\\d+\\.rds$", full.names = TRUE)
cat(sprintf("Found %d checkpoint files to combine\n", length(all_checkpoint_files)))

all_chunks <- lapply(all_checkpoint_files, readRDS)
roi_analysis_full <- do.call(rbind, all_chunks)

cat("\n=== SUMMARY ===\n")
cat("Total ROIs analyzed:", nrow(roi_analysis_full), "\n")

saveRDS(roi_analysis_full, FINAL_OUTPUT)
write_csv(roi_analysis_full, file.path(ctx$paths$out_opt, "allROIs_analysis+skeletons.csv"))
saveRDS(roi_analysis_full, ctx$paths$all_rois_analysis)
write_csv(roi_analysis_full, file.path(ctx$paths$out_opt, "allROIs_analysis.csv"))
cat("\nSaved combined results to:\n")
cat("  -", FINAL_OUTPUT, "\n")
cat("  -", file.path(ctx$paths$out_opt, "allROIs_analysis+skeletons.csv"), "\n")
cat("  -", ctx$paths$all_rois_analysis, "\n")
cat("  -", file.path(ctx$paths$out_opt, "allROIs_analysis.csv"), "\n\n")

# ========= CLEANUP CHECKPOINTS =========
cat("=== CLEANING UP CHECKPOINTS ===\n")
response <- Sys.getenv("PIPELINE_DELETE_CHECKPOINTS", unset = "no")
if (tolower(trimws(response)) == "yes") {
  unlink(all_checkpoint_files)
  cat(sprintf("Deleted %d checkpoint files\n", length(all_checkpoint_files)))
} else {
  cat("Checkpoints preserved in:", CHECKPOINT_DIR, "\n")
}




# ========= VISUALIZATION =========
roi_to_xy <- function(roi_obj) {
  if (!is.null(roi_obj$coords)) {
    xy <- as.data.frame(roi_obj$coords); colnames(xy) <- c("x","y"); return(xy)
  }
  if (!is.null(roi_obj$x) && !is.null(roi_obj$y)) return(data.frame(x = roi_obj$x, y = roi_obj$y))
  if (!is.null(roi_obj$left) && !is.null(roi_obj$top) && !is.null(roi_obj$right) && !is.null(roi_obj$bottom)) {
    return(data.frame(
      x = c(roi_obj$left, roi_obj$right, roi_obj$right, roi_obj$left, roi_obj$left),
      y = c(roi_obj$top,  roi_obj$top,   roi_obj$bottom, roi_obj$bottom, roi_obj$top)
    ))
  }
  NULL
}

visualize_rois_before_after <- function(
    image_key,
    cutoff_params = list(
      min_area = 1, max_circularity = 0.10, min_aspect_ratio = 2.0,
      min_skeleton_length = 3, min_mean_width = 1
    ),
    line_width = 2,
    save_output = TRUE,
    output_dir = file.path(ctx$paths$out_opt, "visualizations")
) {
  if (!image_key %in% names(allROIs)) stop("Image key '", image_key, "' not found in allROIs")
  if (save_output && !dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  img_name <- allROIs[[image_key]]$metadata$image_name
  base_name <- sub("(_RGavg_mask_renorm)?\\.(tif|png|jpg|jpeg)$", "", img_name, ignore.case = TRUE)
  
  clear_bcgnd <- ctx$paths$out_clear
  img_files <- list.files(clear_bcgnd, pattern = paste0("^", gsub("[^[:alnum:]]", ".", base_name)),
                          full.names = TRUE, ignore.case = TRUE)
  if (length(img_files) == 0) stop("No image found in preprocessing output folder for: ", image_key)
  img_path <- img_files[1]
  
  roi_data_subset <- roi_analysis_full %>% filter(image_key == !!image_key)
  if (nrow(roi_data_subset) == 0) stop("No ROI data found for: ", image_key)
  
  roi_data_filtered <- roi_data_subset %>%
    filter(
      area >= cutoff_params$min_area,
      circularity <= cutoff_params$max_circularity,
      aspect_ratio >= cutoff_params$min_aspect_ratio,
      skeleton_length >= cutoff_params$min_skeleton_length,
      mean_width >= cutoff_params$min_mean_width
    )
  
  all_rois <- allROIs[[image_key]]$rois
  kept_roi_names <- roi_data_filtered$roi_name
  removed_roi_names <- setdiff(roi_data_subset$roi_name, kept_roi_names)
  
  img <- image_read(img_path)
  info <- image_info(img); W <- info$width; H <- info$height
  
  # BEFORE
  img_before <- image_draw(img)
  for (roi_name in roi_data_subset$roi_name) {
    if (roi_name %in% names(all_rois)) {
      xy <- roi_to_xy(all_rois[[roi_name]])
      if (!is.null(xy) && nrow(xy) >= 2) lines(x = xy$x, y = H - xy$y, col = "blue", lwd = line_width)
    }
  }
  dev.off()
  
  # AFTER
  img_after <- image_draw(img)
  for (roi_name in removed_roi_names) {
    if (roi_name %in% names(all_rois)) {
      xy <- roi_to_xy(all_rois[[roi_name]])
      if (!is.null(xy) && nrow(xy) >= 2) lines(x = xy$x, y = H - xy$y, col = "red", lwd = line_width)
    }
  }
  for (roi_name in kept_roi_names) {
    if (roi_name %in% names(all_rois)) {
      xy <- roi_to_xy(all_rois[[roi_name]])
      if (!is.null(xy) && nrow(xy) >= 2) lines(x = xy$x, y = H - xy$y, col = "green", lwd = line_width)
    }
  }
  dev.off()
  
  img_before <- image_annotate(img_before,
                               sprintf("BEFORE: %d ROIs", nrow(roi_data_subset)),
                               size = 30, color = "white", location = "+10+10", boxcolor = "black")
  img_after <- image_annotate(img_after,
                              sprintf("AFTER: %d kept (green), %d removed (red)",
                                      nrow(roi_data_filtered),
                                      nrow(roi_data_subset) - nrow(roi_data_filtered)),
                              size = 30, color = "white", location = "+10+10", boxcolor = "black")
  
  img_combined <- image_append(c(img_before, img_after), stack = FALSE)
  
  if (save_output) {
    out_file <- file.path(output_dir, paste0(image_key, "_before_after.png"))
    image_write(img_combined, out_file)
    cat("Saved visualization to:", out_file, "\n")
  }
  
  print(img_combined)
  
  list(
    image_key = image_key,
    total_rois = nrow(roi_data_subset),
    kept_rois = nrow(roi_data_filtered),
    removed_rois = nrow(roi_data_subset) - nrow(roi_data_filtered),
    retention_rate = nrow(roi_data_filtered) / nrow(roi_data_subset)
  )
}

# ========= EXAMPLE USAGE =========
cat("\n=== EXAMPLE VISUALIZATION ===\n")
optimal_params <- list(
  min_area = 1.0, max_circularity = 0.10, min_aspect_ratio = 2.0,
  min_skeleton_length = 3.0, min_mean_width = 1.0
)

example_images <- roi_analysis_full %>%
  group_by(image_key) %>%
  summarise(n_rois = n(), .groups = "drop") %>%
  filter(n_rois > 100 & n_rois < 5000) %>%
  arrange(desc(n_rois)) %>%
  head(3)

if (nrow(example_images) > 0) {
  cat("\nTo visualize an image, use:\n")
  cat('visualize_rois_before_after(\n')
  cat('  image_key = "', example_images$image_key[1], '",\n', sep = "")
  cat('  cutoff_params = optimal_params\n')
  cat(')\n\n')
}

cat("=== ANALYSIS COMPLETE ===\n")
cat(sprintf("Total processing time: %.1f minutes\n", elapsed_time))
cat(sprintf("Average speed: %.1f images/minute\n",
            length(remaining_keys) / max(elapsed_time, .Machine$double.eps)))
