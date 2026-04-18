# Comprehensive ROI Analysis with Before/After Visualization
# Analyzes all ROIs from allROIs_data.rds and applies optimal filtering

library(RImageJROI)
library(dplyr)
library(magick)
library(ggplot2)
library(readr)

# Set working directory
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)

# Load data
cat("=== LOADING DATA ===\n")
allROIs <- readRDS("allROIs_data.rds")
metadata <- read_csv("OUT_optimize/mask_area_percentages+optim_groups+inspected.csv", 
                     show_col_types = FALSE)

cat("Loaded", length(allROIs), "image entries\n")
cat("Metadata rows:", nrow(metadata), "\n\n")

# ===== FUNCTION DEFINITIONS =====

# Extract ROI properties
extract_roi_properties <- function(roi_data) {
  properties <- list(
    area = NA_real_,
    perimeter = NA_real_,
    circularity = NA_real_,
    aspect_ratio = NA_real_,
    solidity = NA_real_,
    mean_width = NA_real_
  )
  
  tryCatch({
    if (!is.null(roi_data$coords)) {
      coords <- roi_data$coords
      n_points <- nrow(coords)
      
      if (n_points >= 3) {
        x <- coords[, 1]
        y <- coords[, 2]
        
        # Area (shoelace formula)
        area <- 0.5 * abs(sum(x * c(y[-1], y[1]) - c(x[-1], x[1]) * y))
        properties$area <- area
        
        # Perimeter
        dx <- diff(c(x, x[1]))
        dy <- diff(c(y, y[1]))
        perimeter <- sum(sqrt(dx^2 + dy^2))
        properties$perimeter <- perimeter
        
        # Circularity: 4π * area / perimeter²
        if (perimeter > 0) {
          properties$circularity <- (4 * pi * area) / (perimeter^2)
        }
        
        # Bounding box for aspect ratio
        width <- max(x) - min(x)
        height <- max(y) - min(y)
        if (height > 0) {
          properties$aspect_ratio <- width / height
        }
        properties$mean_width <- mean(c(width, height))
        
        # Solidity (area / bounding box area)
        if (width > 0 && height > 0) {
          bbox_area <- width * height
          properties$solidity <- area / bbox_area
        }
      }
    }
  }, error = function(e) {
    # Silent error handling
  })
  
  return(properties)
}

# Calculate skeleton metrics
calculate_skeleton_metrics <- function(roi_data) {
  metrics <- list(
    total_length = 0,
    branch_points = 0,
    endpoints = 0,
    avg_branch_length = 0
  )
  
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
      }
    }
  }, error = function(e) {
    # Silent error handling
  })
  
  return(metrics)
}

# Get ROI coordinates for visualization
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

# ===== PROCESS ALL ROIs =====
cat("=== EXTRACTING ROI PROPERTIES ===\n")

roi_analysis_full <- data.frame()
processed_count <- 0

for (img_key in names(allROIs)) {
  img_data <- allROIs[[img_key]]
  img_name <- img_data$metadata$image_name
  
  # Find matching metadata row (handle suffix variations)
  base_name <- sub("(_RGavg_mask_renorm)?\\.(tif|png|jpg|jpeg)$", "", img_name, ignore.case = TRUE)
  meta_row <- metadata %>% 
    filter(grepl(paste0("^", gsub("[^[:alnum:]]", ".", base_name)), image_name, ignore.case = TRUE))
  
  # Get group classifications
  if (nrow(meta_row) > 0) {
    is_high_neurites <- (meta_row$grp_high_neurites_low_confluence[1] == 1 || 
                           meta_row$grp_high_neurites_high_confluence[1] == 1)
    is_low_neurites <- (meta_row$grp_low_neurites_low_confluence[1] == 1 || 
                          meta_row$grp_low_neurites_high_confluence[1] == 1)
    is_high_confluence <- (meta_row$grp_low_neurites_high_confluence[1] == 1 || 
                             meta_row$grp_high_neurites_high_confluence[1] == 1)
    is_low_confluence <- (meta_row$grp_high_neurites_low_confluence[1] == 1 || 
                            meta_row$grp_low_neurites_low_confluence[1] == 1)
    is_signal <- (meta_row$`_manual_choise`[1] == 1)
    is_noise <- (meta_row$manual_noise[1] == 1)
  } else {
    # No metadata found - skip or set to NA
    is_high_neurites <- NA
    is_low_neurites <- NA
    is_high_confluence <- NA
    is_low_confluence <- NA
    is_signal <- NA
    is_noise <- NA
  }
  
  # Process each ROI
  if (length(img_data$rois) > 0) {
    for (roi_name in names(img_data$rois)) {
      roi_obj <- img_data$rois[[roi_name]]
      
      # Extract properties
      props <- extract_roi_properties(roi_obj)
      skel_metrics <- calculate_skeleton_metrics(roi_obj)
      
      # Create row
      roi_row <- data.frame(
        image_key = img_key,
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
        is_high_neurites = is_high_neurites,
        is_low_neurites = is_low_neurites,
        is_high_confluence = is_high_confluence,
        is_low_confluence = is_low_confluence,
        is_signal = is_signal,
        is_noise = is_noise,
        stringsAsFactors = FALSE
      )
      
      roi_analysis_full <- rbind(roi_analysis_full, roi_row)
    }
  }
  
  processed_count <- processed_count + 1
  if (processed_count %% 10 == 0) {
    cat(sprintf("  Processed %d/%d images\n", processed_count, length(allROIs)))
  }
}

# Remove rows with NA values in key metrics
roi_analysis_full <- roi_analysis_full %>%
  filter(!is.na(area) & !is.na(circularity) & !is.na(skeleton_length))

cat("\n=== SUMMARY ===\n")
cat("Total ROIs analyzed:", nrow(roi_analysis_full), "\n")
cat("  With metadata:", sum(!is.na(roi_analysis_full$is_signal)), "\n")
cat("  Signal (manual_choice):", sum(roi_analysis_full$is_signal, na.rm = TRUE), "\n")
cat("  Noise (manual_noise):", sum(roi_analysis_full$is_noise, na.rm = TRUE), "\n")

# Save full analysis
saveRDS(roi_analysis_full, "allROIs_analysis.rds")
write_csv(roi_analysis_full, "OUT_optimize/allROIs_analysis.csv")
cat("\nSaved to:\n")
cat("  - allROIs_analysis.rds\n")
cat("  - OUT_optimize/allROIs_analysis.csv\n\n")

# ===== VISUALIZATION FUNCTION =====

visualize_rois_before_after <- function(
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
  
  # Create output directory
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
    stop("No image found in OUT_clear_bcgnd for: ", image_key)
  }
  img_path <- img_files[1]
  
  # Get ROIs for this image
  roi_data_subset <- roi_analysis_full %>%
    filter(image_key == !!image_key)
  
  if (nrow(roi_data_subset) == 0) {
    stop("No ROI data found for: ", image_key)
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
  
  # Create "AFTER" visualization (kept=green, removed=red)
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
  
  # Add text annotations
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
  
  # Save if requested
  if (save_output) {
    out_file <- file.path(output_dir, paste0(image_key, "_before_after.png"))
    image_write(img_combined, out_file)
    cat("Saved visualization to:", out_file, "\n")
  }
  
  # Display
  print(img_combined)
  
  # Return statistics
  list(
    image_key = image_key,
    total_rois = nrow(roi_data_subset),
    kept_rois = nrow(roi_data_filtered),
    removed_rois = nrow(roi_data_subset) - nrow(roi_data_filtered),
    retention_rate = nrow(roi_data_filtered) / nrow(roi_data_subset)
  )
}

# ===== EXAMPLE USAGE =====
cat("\n=== EXAMPLE: VISUALIZE WITH OPTIMAL PARAMETERS ===\n")

# Use optimal parameters from optimization
optimal_params <- list(
  min_area = 1.0,
  max_circularity = 0.10,
  min_aspect_ratio = 2.0,
  min_skeleton_length = 3.0,
  min_mean_width = 1.0
)

# Find an image with good number of ROIs
example_images <- roi_analysis_full %>%
  group_by(image_key) %>%
  summarise(n_rois = n(), .groups = "drop") %>%
  filter(n_rois > 100 & n_rois < 5000) %>%
  arrange(desc(n_rois)) %>%
  head(5)

if (nrow(example_images) > 0) {
  cat("\nCreating example visualizations for top images...\n\n")
  
  for (i in 1:min(3, nrow(example_images))) {
    img_key <- example_images$image_key[i]
    cat(sprintf("Processing %d/%d: %s\n", i, min(3, nrow(example_images)), img_key))
    
    tryCatch({
      stats <- visualize_rois_before_after(
        image_key = img_key,
        cutoff_params = optimal_params,
        line_width = 1,
        save_output = TRUE
      )
      
      cat(sprintf("  Total ROIs: %d\n", stats$total_rois))
      cat(sprintf("  Kept: %d (%.1f%%)\n", stats$kept_rois, stats$retention_rate * 100))
      cat(sprintf("  Removed: %d\n\n", stats$removed_rois))
    }, error = function(e) {
      cat("  Error:", e$message, "\n\n")
    })
  }
}

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("\nTo visualize a specific image, use:\n")
cat('visualize_rois_before_after(\n')
cat('  image_key = "your_image_key",\n')
cat('  cutoff_params = optimal_params,\n')
cat('  line_width = 2,\n')
cat('  save_output = TRUE\n')
cat(')\n\n')

cat("Available image keys:\n")
cat(paste(head(names(allROIs), 10), collapse = "\n"), "\n")
cat("... and", length(allROIs) - 10, "more\n")
