# Batch Visualization Script - FIXED VERSION
# Fixes: cache exhaustion, inverted ROIs, slow processing, better image selection

library(magick)
library(dplyr)
library(readr)
library(parallel)

args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)
source(file.path(base_dir, "launchers", "pipeline_paths.R"))
ctx <- pipeline_context(base_dir)
dir.create(ctx$paths$out_opt, recursive = TRUE, showWarnings = FALSE)

# Configuration
N_CORES <- min(50, detectCores() - 2)  # Use up to 10 cores for visualization
cat(sprintf("Using %d CPU cores for parallel processing\n\n", N_CORES))

# Load data
cat("Loading data...\n")
all_rois_source <- if (file.exists(ctx$paths$all_rois_data)) ctx$paths$all_rois_data else ctx$paths$opt_rois_rds
allROIs <- readRDS(all_rois_source)
roi_analysis_full <- readRDS(ctx$paths$all_rois_analysis)
cat("Data loaded successfully\n\n")

# ===== OPTIMIZED VISUALIZATION FUNCTION WITH CACHE FIX =====
roi_to_xy <- function(roi_obj) {
  if (!is.null(roi_obj$coords)) {
    xy <- as.data.frame(roi_obj$coords)
    colnames(xy) <- c("x", "y")
    return(xy)
  }
  if (!is.null(roi_obj$x) && !is.null(roi_obj$y)) {
    return(data.frame(x = roi_obj$x, y = roi_obj$y))
  }
  if (!is.null(roi_obj$left) && !is.null(roi_obj$top) &&
      !is.null(roi_obj$right) && !is.null(roi_obj$bottom)) {
    return(data.frame(
      x = c(roi_obj$left, roi_obj$right, roi_obj$right, roi_obj$left, roi_obj$left),
      y = c(roi_obj$top, roi_obj$top, roi_obj$bottom, roi_obj$bottom, roi_obj$top)
    ))
  }
  NULL
}

visualize_rois_before_after <- function(
    image_key,
    cutoff_params = list(
      min_area = 1,
      max_circularity = 0.10,
      min_aspect_ratio = 2.0,
      min_skeleton_length = 3,
      min_mean_width = 1
    ),
    line_width = 1,
    save_output = TRUE,
    output_dir = file.path(ctx$paths$out_opt, "visualizations")
) {
  
  if (!image_key %in% names(allROIs)) {
    stop("Image key '", image_key, "' not found in allROIs")
  }
  
  if (save_output && !dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Get image path
  img_name <- allROIs[[image_key]]$metadata$image_name
  base_name <- sub("(_RGavg_mask_renorm)?\\.(tif|png|jpg|jpeg)$", "", img_name, ignore.case = TRUE)
  
  clear_bcgnd <- ctx$paths$out_clear
  img_files <- list.files(clear_bcgnd, pattern = paste0("^", gsub("[^[:alnum:]]", ".", base_name)), 
                          full.names = TRUE, ignore.case = TRUE)
  
  if (length(img_files) == 0) {
    warning("No image found in preprocessing output folder for: ", image_key)
    return(NULL)
  }
  img_path <- img_files[1]
  
  # Get ROIs for this image
  roi_data_subset <- roi_analysis_full %>%
    filter(image_key == !!image_key)
  
  if (nrow(roi_data_subset) == 0) {
    warning("No ROI data found for: ", image_key)
    return(NULL)
  }
  
  # Apply filters
  roi_data_filtered <- roi_data_subset %>%
    filter(
      area >= cutoff_params$min_area,
      circularity <= cutoff_params$max_circularity,
      aspect_ratio >= cutoff_params$min_aspect_ratio,
      skeleton_length >= cutoff_params$min_skeleton_length,
      mean_width >= cutoff_params$min_mean_width
    )
  
  # Get ROI objects
  all_rois <- allROIs[[image_key]]$rois
  kept_roi_names <- roi_data_filtered$roi_name
  removed_roi_names <- setdiff(roi_data_subset$roi_name, kept_roi_names)
  
  # === FIX 1: Read image with proper cache management ===
  # Scale down large images to prevent cache exhaustion
  img <- tryCatch({
    img_raw <- image_read(img_path)
    info <- image_info(img_raw)
    
    # If image is very large, scale it down
    max_dimension <- 4000
    if (info$width > max_dimension || info$height > max_dimension) {
      scale_factor <- max_dimension / max(info$width, info$height)
      img_raw <- image_scale(img_raw, geometry_size_percent(scale_factor * 100))
      cat(sprintf("  Scaled down large image (was %dx%d)\n", info$width, info$height))
      info <- image_info(img_raw)  # Update info
    }
    img_raw
  }, error = function(e) {
    stop("Failed to read image: ", e$message)
  })
  
  W <- info$width
  H <- info$height
  
  # === FIX 2: Correct Y-coordinate (ImageJ uses top-left origin, R graphics uses bottom-left) ===
  # ImageJ ROIs have Y increasing downward, so we DON'T need to flip
  # The issue was inverting when we shouldn't have
  
  # Create "BEFORE" visualization (all ROIs in blue)
  img_before <- img
  img_before <- image_draw(img_before)
  for (roi_name in roi_data_subset$roi_name) {
    if (roi_name %in% names(all_rois)) {
      roi_obj <- all_rois[[roi_name]]
      xy <- roi_to_xy(roi_obj)
      if (!is.null(xy) && nrow(xy) >= 2) {
        # Draw ROIs directly with their coordinates
        lines(x = xy$x, y = xy$y, col = "blue", lwd = line_width)
      }
    }
  }
  dev.off()
  
  # Create "AFTER" visualization
  img_after <- img
  img_after <- image_draw(img_after)
  
  # Draw removed ROIs in red
  for (roi_name in removed_roi_names) {
    if (roi_name %in% names(all_rois)) {
      roi_obj <- all_rois[[roi_name]]
      xy <- roi_to_xy(roi_obj)
      if (!is.null(xy) && nrow(xy) >= 2) {
        lines(x = xy$x, y = xy$y, col = "red", lwd = line_width)
      }
    }
  }
  
  # Draw kept ROIs in green
  for (roi_name in kept_roi_names) {
    if (roi_name %in% names(all_rois)) {
      roi_obj <- all_rois[[roi_name]]
      xy <- roi_to_xy(roi_obj)
      if (!is.null(xy) && nrow(xy) >= 2) {
        lines(x = xy$x, y = xy$y, col = "green", lwd = line_width)
      }
    }
  }
  dev.off()
  
  # Add annotations
  img_before <- image_annotate(img_before, 
                               sprintf("BEFORE: %d ROIs", nrow(roi_data_subset)),
                               size = 30, color = "white", location = "+10+10",
                               boxcolor = "black")
  
  img_after <- image_annotate(img_after,
                              sprintf("AFTER: %d kept (green), %d removed (red)",
                                      nrow(roi_data_filtered), 
                                      nrow(roi_data_subset) - nrow(roi_data_filtered)),
                              size = 30, color = "white", location = "+10+10",
                              boxcolor = "black")
  
  # Combine side by side
  img_combined <- image_append(c(img_before, img_after), stack = FALSE)
  
  # Save
  if (save_output) {
    params_label <- sprintf("area%g_circ%.2f_asp%.1f", 
                            cutoff_params$min_area,
                            cutoff_params$max_circularity,
                            cutoff_params$min_aspect_ratio)
    out_file <- file.path(output_dir, paste0(image_key, "_", params_label, ".png"))
    image_write(img_combined, out_file, format = "png")
  }
  
  # Clean up to free memory
  rm(img, img_before, img_after, img_combined)
  gc(verbose = FALSE)
  
  # Return statistics
  list(
    image_key = image_key,
    total_rois = nrow(roi_data_subset),
    kept_rois = nrow(roi_data_filtered),
    removed_rois = nrow(roi_data_subset) - nrow(roi_data_filtered),
    retention_rate = nrow(roi_data_filtered) / nrow(roi_data_subset)
  )
}

# ===== IMPROVED IMAGE SELECTION =====
cat("=== SELECTING REPRESENTATIVE IMAGES ===\n\n")

# Get images with metadata classification
images_with_metadata <- roi_analysis_full %>%
  group_by(image_key, image_name) %>%
  summarise(
    n_rois = n(),
    is_signal = first(is_signal[!is.na(is_signal)]),
    is_noise = first(is_noise[!is.na(is_noise)]),
    is_high_neurites = first(is_high_neurites[!is.na(is_high_neurites)]),
    is_low_neurites = first(is_low_neurites[!is.na(is_low_neurites)]),
    is_high_confluence = first(is_high_confluence[!is.na(is_high_confluence)]),
    is_low_confluence = first(is_low_confluence[!is.na(is_low_confluence)]),
    mean_area = mean(area, na.rm = TRUE),
    mean_skeleton = mean(skeleton_length, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(is_signal) | !is.na(is_noise))

# Select test images from different categories
test_images <- list()

# Category 1: Signal images with high neurites, low confluence
high_neurites_low_conf <- images_with_metadata %>%
  filter(is_signal == TRUE, is_high_neurites == TRUE, is_low_confluence == TRUE) %>%
  filter(n_rois > 500, n_rois < 5000) %>%
  arrange(desc(n_rois)) %>%
  head(2)
if (nrow(high_neurites_low_conf) > 0) {
  test_images$high_neurites_low_conf <- high_neurites_low_conf
  cat("Selected", nrow(high_neurites_low_conf), "HIGH NEURITES + LOW CONFLUENCE (signal) images\n")
}

# Category 2: Signal images with low neurites, high confluence
low_neurites_high_conf <- images_with_metadata %>%
  filter(is_signal == TRUE, is_low_neurites == TRUE, is_high_confluence == TRUE) %>%
  filter(n_rois > 500, n_rois < 5000) %>%
  arrange(desc(n_rois)) %>%
  head(2)
if (nrow(low_neurites_high_conf) > 0) {
  test_images$low_neurites_high_conf <- low_neurites_high_conf
  cat("Selected", nrow(low_neurites_high_conf), "LOW NEURITES + HIGH CONFLUENCE (signal) images\n")
}

# Category 3: Noise images with low confluence
noise_low_conf <- images_with_metadata %>%
  filter(is_noise == TRUE, is_low_confluence == TRUE) %>%
  filter(n_rois > 500, n_rois < 5000) %>%
  arrange(desc(n_rois)) %>%
  head(2)
if (nrow(noise_low_conf) > 0) {
  test_images$noise_low_conf <- noise_low_conf
  cat("Selected", nrow(noise_low_conf), "NOISE + LOW CONFLUENCE images\n")
}

# Category 4: Noise images with high neurites (if they exist)
noise_high_neurites <- images_with_metadata %>%
  filter(is_noise == TRUE, is_high_neurites == TRUE) %>%
  filter(n_rois > 500, n_rois < 5000) %>%
  arrange(desc(n_rois)) %>%
  head(2)
if (nrow(noise_high_neurites) > 0) {
  test_images$noise_high_neurites <- noise_high_neurites
  cat("Selected", nrow(noise_high_neurites), "NOISE + HIGH NEURITES images\n")
}

# Combine all test images
all_test_images <- do.call(rbind, test_images)
if (is.null(all_test_images) || nrow(all_test_images) == 0) {
  # Fallback: just select some images with good ROI counts
  all_test_images <- images_with_metadata %>%
    filter(n_rois > 500, n_rois < 5000) %>%
    arrange(desc(n_rois)) %>%
    head(5)
}

cat("\nTotal test images:", nrow(all_test_images), "\n\n")

# ===== DEFINE PARAMETER SETS =====
param_sets <- list(
  optimal = list(
    name = "Optimal (Very Strict)",
    params = list(min_area = 1, max_circularity = 0.10, 
                  min_aspect_ratio = 2.0, min_skeleton_length = 3, min_mean_width = 1)
  ),
  moderate = list(
    name = "Moderate",
    params = list(min_area = 10, max_circularity = 0.30, 
                  min_aspect_ratio = 1.5, min_skeleton_length = 10, min_mean_width = 2)
  ),
  relaxed = list(
    name = "Relaxed",
    params = list(min_area = 50, max_circularity = 0.50, 
                  min_aspect_ratio = 1.0, min_skeleton_length = 20, min_mean_width = 3)
  )
)

# ===== PARALLEL BATCH PROCESSING =====
cat("=== PROCESSING VISUALIZATIONS IN PARALLEL ===\n\n")

# Create all combinations of images and parameter sets
visualization_jobs <- expand.grid(
  img_idx = 1:nrow(all_test_images),
  ps_name = names(param_sets),
  stringsAsFactors = FALSE
)

cat(sprintf("Total visualizations to create: %d\n", nrow(visualization_jobs)))
cat(sprintf("Using %d parallel workers\n\n", N_CORES))

# Parallel processing function
process_visualization <- function(job_idx, jobs, test_imgs, params) {
  job <- jobs[job_idx, ]
  img_key <- test_imgs$image_key[job$img_idx]
  img_name <- test_imgs$image_name[job$img_idx]
  ps <- params[[job$ps_name]]
  
  result <- tryCatch({
    visualize_rois_before_after(
      image_key = img_key,
      cutoff_params = ps$params,
      line_width = 1,
      save_output = TRUE,
    output_dir = file.path(ctx$paths$out_opt, "visualizations", job$ps_name)
    )
  }, error = function(e) {
    list(
      image_key = img_key,
      error = e$message,
      total_rois = NA,
      kept_rois = NA,
      removed_rois = NA,
      retention_rate = NA
    )
  })
  
  if (!is.null(result)) {
    result$image_name <- img_name
    result$parameter_set <- ps$name
    result$category <- NA
    
    # Determine category
    img_info <- test_imgs[test_imgs$image_key == img_key, ]
    if (nrow(img_info) > 0) {
      if (!is.na(img_info$is_signal) && img_info$is_signal == TRUE) {
        if (!is.na(img_info$is_high_neurites) && img_info$is_high_neurites == TRUE) {
          result$category <- "Signal: High Neurites"
        } else if (!is.na(img_info$is_low_neurites) && img_info$is_low_neurites == TRUE) {
          result$category <- "Signal: Low Neurites"
        } else {
          result$category <- "Signal: Other"
        }
      } else if (!is.na(img_info$is_noise) && img_info$is_noise == TRUE) {
        result$category <- "Noise"
      }
    }
  }
  
  return(result)
}

# Run parallel processing
cl <- makeCluster(N_CORES)
clusterExport(cl, c("visualize_rois_before_after", "roi_to_xy", "allROIs", 
                    "roi_analysis_full", "all_test_images", "param_sets"),
              envir = environment())
clusterEvalQ(cl, {
  library(magick)
  library(dplyr)
})

start_time <- Sys.time()
results_list <- parLapply(cl, 1:nrow(visualization_jobs), process_visualization,
                          jobs = visualization_jobs,
                          test_imgs = all_test_images,
                          params = param_sets)
stopCluster(cl)

elapsed_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
cat(sprintf("\nAll visualizations completed in %.1f seconds\n\n", elapsed_time))

# Combine results
comparison_results <- do.call(rbind, lapply(results_list, function(x) {
  if (!is.null(x) && !is.null(x$image_key)) {
    as.data.frame(x, stringsAsFactors = FALSE)
  }
}))

# Remove failed results
comparison_results <- comparison_results %>%
  filter(!is.na(total_rois))

# ===== SAVE AND VISUALIZE RESULTS =====
write_csv(comparison_results, file.path(ctx$paths$out_opt, "parameter_comparison_results.csv"))
cat("Saved results to:", file.path(ctx$paths$out_opt, "parameter_comparison_results.csv"), "\n\n")

# Create summary
summary_table <- comparison_results %>%
  group_by(parameter_set, category) %>%
  summarise(
    n_images = n(),
    mean_retention = mean(retention_rate, na.rm = TRUE) * 100,
    sd_retention = sd(retention_rate, na.rm = TRUE) * 100,
    mean_kept = mean(kept_rois, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(category, desc(mean_retention))

cat("=== SUMMARY BY CATEGORY ===\n")
print(summary_table)
write_csv(summary_table, file.path(ctx$paths$out_opt, "parameter_comparison_summary.csv"))

# Create comparison plots
library(ggplot2)

if (nrow(comparison_results) > 0) {
  
  # Plot 1: Retention rates by parameter set and category
  p1 <- ggplot(comparison_results, 
               aes(x = parameter_set, y = retention_rate * 100, 
                   fill = category)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5, size = 2) +
    labs(title = "ROI Retention Rates by Parameter Set and Image Category",
         x = "Parameter Set", y = "Retention Rate (%)",
         fill = "Image Category") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
ggsave(file.path(ctx$paths$out_opt, "parameter_comparison_by_category.png"),
         p1, width = 12, height = 6)
  
  # Plot 2: Number of kept ROIs
  p2 <- ggplot(comparison_results, 
               aes(x = parameter_set, y = kept_rois, fill = category)) +
    geom_boxplot() +
    labs(title = "Number of Kept ROIs by Parameter Set",
         x = "Parameter Set", y = "Number of Kept ROIs",
         fill = "Image Category") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
ggsave(file.path(ctx$paths$out_opt, "parameter_comparison_kept_rois.png"),
         p2, width = 12, height = 6)
  
  cat("\nSaved comparison plots\n")
}

# ===== CREATE IMAGE INDEX =====
cat("\n=== CREATING COMPREHENSIVE IMAGE INDEX ===\n")
image_index <- roi_analysis_full %>%
  group_by(image_key, image_name) %>%
  summarise(
    n_rois = n(),
    is_signal = first(is_signal[!is.na(is_signal)]),
    is_noise = first(is_noise[!is.na(is_noise)]),
    is_high_neurites = first(is_high_neurites[!is.na(is_high_neurites)]),
    is_low_neurites = first(is_low_neurites[!is.na(is_low_neurites)]),
    is_high_confluence = first(is_high_confluence[!is.na(is_high_confluence)]),
    is_low_confluence = first(is_low_confluence[!is.na(is_low_confluence)]),
    mean_area = mean(area, na.rm = TRUE),
    mean_circularity = mean(circularity, na.rm = TRUE),
    mean_aspect_ratio = mean(aspect_ratio, na.rm = TRUE),
    mean_skeleton_length = mean(skeleton_length, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_rois))

write_csv(image_index, file.path(ctx$paths$out_opt, "image_index.csv"))
cat("Image index saved to:", file.path(ctx$paths$out_opt, "image_index.csv"), "\n")

cat("\n=== BATCH VISUALIZATION COMPLETE ===\n\n")
cat("Outputs:\n")
cat("  - Visualizations in", file.path(ctx$paths$out_opt, "visualizations", "[optimal|moderate|relaxed]"), "\n")
cat("  - parameter_comparison_results.csv\n")
cat("  - parameter_comparison_summary.csv\n")
cat("  - parameter_comparison_by_category.png\n")
cat("  - parameter_comparison_kept_rois.png\n")
cat("  - image_index.csv\n\n")

cat("=== QUICK VISUALIZATION EXAMPLES ===\n\n")
cat("To visualize a specific image:\n\n")
cat('visualize_rois_before_after(\n')
cat('  image_key = "', all_test_images$image_key[1], '",\n', sep = "")
cat('  cutoff_params = param_sets$optimal$params\n')
cat(')\n\n')

cat("Top signal images (high neurites):\n")
print(head(image_index %>% 
             filter(!is.na(is_signal), is_signal == TRUE, 
                    !is.na(is_high_neurites), is_high_neurites == TRUE) %>%
             select(image_key, n_rois, mean_skeleton_length), 5))

cat("\nTop noise images:\n")
print(head(image_index %>% 
             filter(!is.na(is_noise), is_noise == TRUE) %>%
             select(image_key, n_rois, mean_skeleton_length), 5))
