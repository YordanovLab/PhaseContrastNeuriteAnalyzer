# Batch Visualization Script - Test multiple cutoff parameters
# Requires: allROIs_analysis.rds already created

library(magick)
library(dplyr)
library(readr)

args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)

# Load data
allROIs <- readRDS("allROIs_data.rds")
roi_analysis_full <- readRDS("allROIs_analysis.rds")

# Source visualization function (or copy from main script)
source_visualization_function <- function() {
  # roi_to_xy function
  roi_to_xy <<- function(roi_obj) {
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
  
  # Main visualization function
  visualize_rois_before_after <<- function(
    image_key,
    cutoff_params = list(
      min_area = 1,
      max_circularity = 0.10,
      min_aspect_ratio = 2.0,
      min_skeleton_length = 3,
      min_mean_width = 1
    ),
    line_width = 2,
    save_output = TRUE,
    output_dir = "OUT_optimize/visualizations"
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
    
    clear_bcgnd <- "OUT_clear_bcgnd"
    img_files <- list.files(clear_bcgnd, pattern = paste0("^", gsub("[^[:alnum:]]", ".", base_name)), 
                            full.names = TRUE, ignore.case = TRUE)
    
    if (length(img_files) == 0) {
      warning("No image found in OUT_clear_bcgnd for: ", image_key)
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
    
    # Read image
    img <- image_read(img_path)
    info <- image_info(img)
    W <- info$width
    H <- info$height
    
    # Create "BEFORE" visualization (all ROIs in blue)
    img_before <- img
    img_before <- image_draw(img_before)
    for (roi_name in roi_data_subset$roi_name) {
      if (roi_name %in% names(all_rois)) {
        roi_obj <- all_rois[[roi_name]]
        xy <- roi_to_xy(roi_obj)
        if (!is.null(xy) && nrow(xy) >= 2) {
          lines(x = xy$x, y = H - xy$y, col = "blue", lwd = line_width)
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
          lines(x = xy$x, y = H - xy$y, col = "red", lwd = line_width)
        }
      }
    }
    
    # Draw kept ROIs in green
    for (roi_name in kept_roi_names) {
      if (roi_name %in% names(all_rois)) {
        roi_obj <- all_rois[[roi_name]]
        xy <- roi_to_xy(roi_obj)
        if (!is.null(xy) && nrow(xy) >= 2) {
          lines(x = xy$x, y = H - xy$y, col = "green", lwd = line_width)
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
    
    # Combine
    img_combined <- image_append(c(img_before, img_after), stack = FALSE)
    
    # Save
    if (save_output) {
      params_label <- sprintf("area%g_circ%g_asp%g", 
                              cutoff_params$min_area,
                              cutoff_params$max_circularity,
                              cutoff_params$min_aspect_ratio)
      out_file <- file.path(output_dir, paste0(image_key, "_", params_label, ".png"))
      image_write(img_combined, out_file)
    }
    
    # Return statistics
    list(
      image_key = image_key,
      total_rois = nrow(roi_data_subset),
      kept_rois = nrow(roi_data_filtered),
      removed_rois = nrow(roi_data_subset) - nrow(roi_data_filtered),
      retention_rate = nrow(roi_data_filtered) / nrow(roi_data_subset)
    )
  }
}

# Initialize functions
source_visualization_function()

# ===== TEST MULTIPLE PARAMETER SETS =====
cat("=== TESTING MULTIPLE CUTOFF PARAMETERS ===\n\n")

# Define parameter sets to test
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
  ),
  minimal = list(
    name = "Minimal Filtering",
    params = list(min_area = 100, max_circularity = 0.70, 
                  min_aspect_ratio = 0.5, min_skeleton_length = 50, min_mean_width = 5)
  )
)

# Select representative images
test_images <- roi_analysis_full %>%
  group_by(image_key) %>%
  summarise(
    n_rois = n(),
    has_metadata = !all(is.na(is_signal)),
    .groups = "drop"
  ) %>%
  filter(n_rois > 500 & n_rois < 3000, has_metadata) %>%
  arrange(desc(n_rois)) %>%
  head(3)

if (nrow(test_images) == 0) {
  test_images <- roi_analysis_full %>%
    group_by(image_key) %>%
    summarise(n_rois = n(), .groups = "drop") %>%
    filter(n_rois > 100) %>%
    arrange(desc(n_rois)) %>%
    head(3)
}

cat("Selected", nrow(test_images), "images for testing\n\n")

# Create comparison table
comparison_results <- data.frame()

for (img_idx in seq_len(nrow(test_images))) {
  img_key <- test_images$image_key[img_idx]
  cat(sprintf("=== IMAGE %d/%d: %s ===\n", img_idx, nrow(test_images), img_key))
  
  for (ps_name in names(param_sets)) {
    ps <- param_sets[[ps_name]]
    cat(sprintf("  Testing: %s\n", ps$name))
    
    result <- tryCatch({
      visualize_rois_before_after(
        image_key = img_key,
        cutoff_params = ps$params,
        line_width = 1,
        save_output = TRUE,
        output_dir = file.path("OUT_optimize/visualizations", ps_name)
      )
    }, error = function(e) {
      cat("    Error:", e$message, "\n")
      NULL
    })
    
    if (!is.null(result)) {
      cat(sprintf("    Retention: %.1f%% (%d/%d ROIs)\n", 
                  result$retention_rate * 100,
                  result$kept_rois,
                  result$total_rois))
      
      comparison_results <- rbind(comparison_results, data.frame(
        image_key = img_key,
        parameter_set = ps$name,
        total_rois = result$total_rois,
        kept_rois = result$kept_rois,
        retention_rate = result$retention_rate,
        stringsAsFactors = FALSE
      ))
    }
  }
  cat("\n")
}

# Save comparison results
write_csv(comparison_results, "OUT_optimize/parameter_comparison_results.csv")

# Create summary visualization
if (nrow(comparison_results) > 0) {
  library(ggplot2)
  
  p_comparison <- ggplot(comparison_results, 
                         aes(x = parameter_set, y = retention_rate * 100, 
                             fill = parameter_set)) +
    geom_boxplot() +
    geom_jitter(width = 0.2, alpha = 0.5) +
    labs(title = "ROI Retention Rates Across Different Cutoff Parameters",
         x = "Parameter Set", y = "Retention Rate (%)",
         fill = "Parameter Set") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  ggsave("OUT_optimize/parameter_comparison_retention.png", 
         p_comparison, width = 10, height = 6)
  
  cat("\nSaved comparison plot to: OUT_optimize/parameter_comparison_retention.png\n")
}

# Print summary table
cat("\n=== SUMMARY TABLE ===\n")
if (nrow(comparison_results) > 0) {
  summary_table <- comparison_results %>%
    group_by(parameter_set) %>%
    summarise(
      mean_retention = mean(retention_rate) * 100,
      sd_retention = sd(retention_rate) * 100,
      mean_kept = mean(kept_rois),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_retention))
  
  print(summary_table)
  write_csv(summary_table, "OUT_optimize/parameter_comparison_summary.csv")
}

cat("\n=== BATCH VISUALIZATION COMPLETE ===\n")
cat("\nOutputs saved to:\n")
cat("  - OUT_optimize/visualizations/optimal/\n")
cat("  - OUT_optimize/visualizations/moderate/\n")
cat("  - OUT_optimize/visualizations/relaxed/\n")
cat("  - OUT_optimize/visualizations/minimal/\n")
cat("  - OUT_optimize/parameter_comparison_results.csv\n")
cat("  - OUT_optimize/parameter_comparison_summary.csv\n")
cat("  - OUT_optimize/parameter_comparison_retention.png\n\n")

# ===== QUICK VISUALIZATION FUNCTION =====
cat("=== QUICK ACCESS FUNCTION ===\n")
cat("\nTo quickly visualize any image with custom parameters:\n\n")
cat('visualize_rois_before_after(\n')
cat('  image_key = "your_image_key_here",\n')
cat('  cutoff_params = list(\n')
cat('    min_area = 10,\n')
cat('    max_circularity = 0.3,\n')
cat('    min_aspect_ratio = 1.5,\n')
cat('    min_skeleton_length = 10,\n')
cat('    min_mean_width = 2\n')
cat('  ),\n')
cat('  line_width = 2,\n')
cat('  save_output = TRUE,\n')
cat('  output_dir = "OUT_optimize/visualizations/custom"\n')
cat(')\n\n')

# ===== CREATE INDEX OF AVAILABLE IMAGES ===
cat("Creating index of available images...\n")
image_index <- roi_analysis_full %>%
  group_by(image_key, image_name) %>%
  summarise(
    n_rois = n(),
    has_signal_data = any(!is.na(is_signal)),
    is_signal = ifelse(any(!is.na(is_signal)), first(is_signal[!is.na(is_signal)]), NA),
    is_noise = ifelse(any(!is.na(is_noise)), first(is_noise[!is.na(is_noise)]), NA),
    mean_area = mean(area, na.rm = TRUE),
    mean_circularity = mean(circularity, na.rm = TRUE),
    mean_aspect_ratio = mean(aspect_ratio, na.rm = TRUE),
    mean_skeleton_length = mean(skeleton_length, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_rois))

write_csv(image_index, "OUT_optimize/image_index.csv")
cat("Image index saved to: OUT_optimize/image_index.csv\n")
cat("Total images available:", nrow(image_index), "\n\n")

cat("Top 10 images by ROI count:\n")
print(head(image_index %>% select(image_key, n_rois, has_signal_data), 10))
