# ========================================
# ADAPTIVE IMAGE-LEVEL FILTERING & SCORING PIPELINE
# Two-stage approach: 
#   1. Use image features to set adaptive cutoffs per image
#   2. Calculate image-level scores from filtered signal skeletons
# ========================================

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(randomForest)
library(pROC)

# ---------- Settings ----------
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)
skeleton_file <- "allSkeletons_rawData.rds"
metadata_path <- "OUT_optimize/mask_area_percentages+optim_groups+inspected.csv"
output_dir <- "OUT_adaptive_filtering"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---------- Load Data ----------
cat("=== LOADING DATA ===\n\n")

if (!file.exists("skeleton_data_combined.rds")) {
  allSkeletons <- readRDS(skeleton_file)
  skeleton_list <- lapply(names(allSkeletons), function(img_key) {
    skel_params <- allSkeletons[[img_key]]$results_data
    if (!is.null(skel_params) && nrow(skel_params) > 0) {
      skel_params$image_key <- img_key
      return(skel_params)
    }
    return(NULL)
  })
  skeleton_data <- do.call(rbind, skeleton_list[!sapply(skeleton_list, is.null)])
  rownames(skeleton_data) <- NULL
  saveRDS(skeleton_data, "skeleton_data_combined.rds")
} else {
  skeleton_data <- readRDS("skeleton_data_combined.rds")
}

# Clean column names
colnames(skeleton_data) <- gsub("^#\\s*", "", colnames(skeleton_data))
colnames(skeleton_data) <- gsub(" ", "_", colnames(skeleton_data))
colnames(skeleton_data) <- make.names(colnames(skeleton_data))

# Convert to numeric
for (col in colnames(skeleton_data)) {
  if (col != "image_key") {
    skeleton_data[[col]] <- suppressWarnings(as.numeric(skeleton_data[[col]]))
  }
}

# Load metadata
metadata <- read_csv(metadata_path, show_col_types = FALSE)
metadata$image_key <- tools::file_path_sans_ext(metadata$image_name)
colnames(metadata) <- make.names(colnames(metadata))

# Merge with metadata
skeleton_data <- skeleton_data %>%
  left_join(metadata, by = "image_key") %>%
  mutate(
    class = case_when(
      X_manual_choise == "1" ~ "signal",
      manual_noise == "1" ~ "noise",
      TRUE ~ NA_character_
    ),
    class_binary = ifelse(class == "signal", 1, 0)
  ) %>%
  filter(!is.na(class))

cat("Data loaded:\n")
cat("  Total skeletons:", nrow(skeleton_data), "\n")
cat("  Unique images:", n_distinct(skeleton_data$image_key), "\n")
cat("  Signal skeletons:", sum(skeleton_data$class == "signal"), "\n")
cat("  Noise skeletons:", sum(skeleton_data$class == "noise"), "\n\n")

# ========================================
# STEP 1: IMAGE-LEVEL FEATURE AGGREGATION
# ========================================
cat("=== STEP 1: COMPUTING IMAGE-LEVEL FEATURES ===\n\n")

# Identify skeleton-level features (exclude labels and metadata)
skeleton_features <- colnames(skeleton_data)[sapply(skeleton_data, is.numeric)]
skeleton_features <- setdiff(skeleton_features, c("class_binary", "X_manual_choise", "manual_noise"))
skeleton_features <- skeleton_features[!grepl("^grp_|^mask_|area|percent", skeleton_features, ignore.case = TRUE)]

cat("Skeleton features:", length(skeleton_features), "\n")
print(skeleton_features)
cat("\n")

# Compute image-level statistics
image_stats <- skeleton_data %>%
  group_by(image_key) %>%
  summarise(
    # Count statistics
    n_skeletons = n(),
    n_signal = sum(class == "signal"),
    n_noise = sum(class == "noise"),
    pct_signal = 100 * mean(class == "signal"),
    
    # Skeleton length statistics
    mean_max_length = mean(Maximum_Branch_Length, na.rm = TRUE),
    median_max_length = median(Maximum_Branch_Length, na.rm = TRUE),
    sd_max_length = sd(Maximum_Branch_Length, na.rm = TRUE),
    q75_max_length = quantile(Maximum_Branch_Length, 0.75, na.rm = TRUE),
    q90_max_length = quantile(Maximum_Branch_Length, 0.90, na.rm = TRUE),
    
    mean_avg_length = mean(Average_Branch_Length, na.rm = TRUE),
    median_avg_length = median(Average_Branch_Length, na.rm = TRUE),
    
    # Junction statistics
    mean_junctions = mean(Junctions, na.rm = TRUE),
    median_junctions = median(Junctions, na.rm = TRUE),
    
    # Slab statistics
    mean_slab_voxels = mean(Slab_voxels, na.rm = TRUE),
    median_slab_voxels = median(Slab_voxels, na.rm = TRUE),
    
    # Distribution metrics
    cv_max_length = sd(Maximum_Branch_Length, na.rm = TRUE) / (mean(Maximum_Branch_Length, na.rm = TRUE) + 1),
    
    .groups = "drop"
  )

# Add image-level metadata features
image_level_metadata <- metadata %>%
  dplyr::select(image_key, contains("mask_"), contains("area"), contains("percent"), contains("grp_"))

image_stats <- image_stats %>%
  left_join(image_level_metadata, by = "image_key")

write_csv(image_stats, file.path(output_dir, "image_level_statistics.csv"))

cat("Image-level features computed:\n")
cat("  Total images:", nrow(image_stats), "\n")
cat("  Features per image:", ncol(image_stats) - 1, "\n\n")

# ========================================
# STEP 2: ADAPTIVE CUTOFF DETERMINATION
# ========================================
cat("=== STEP 2: LEARNING ADAPTIVE CUTOFFS PER IMAGE ===\n\n")

# For each image, determine optimal cutoff based on image characteristics
compute_optimal_cutoff <- function(img_key, skel_data, img_features) {
  img_skeletons <- skel_data %>% filter(image_key == img_key)
  img_info <- img_features %>% filter(image_key == img_key)
  
  if (nrow(img_skeletons) < 10) {
    return(data.frame(
      image_key = img_key,
      cutoff_method = "insufficient_data",
      length_cutoff = NA,
      slab_cutoff = NA,
      junction_cutoff = NA
    ))
  }
  
  # Method 1: Percentile-based (depends on image distribution)
  # Use stricter cutoffs for images with high noise percentage
  noise_pct <- img_info$pct_signal
  
  if (noise_pct < 30) {
    # High noise image - use stricter cutoffs (higher percentiles)
    length_cutoff <- quantile(img_skeletons$Maximum_Branch_Length, 0.60, na.rm = TRUE)
  } else if (noise_pct < 50) {
    length_cutoff <- quantile(img_skeletons$Maximum_Branch_Length, 0.40, na.rm = TRUE)
  } else {
    # Low noise image - use lenient cutoffs
    length_cutoff <- quantile(img_skeletons$Maximum_Branch_Length, 0.20, na.rm = TRUE)
  }
  
  # Method 2: Signal/noise distribution separation
  signal_skels <- img_skeletons %>% filter(class == "signal")
  noise_skels <- img_skeletons %>% filter(class == "noise")
  
  if (nrow(signal_skels) > 5 && nrow(noise_skels) > 5) {
    # Find threshold that maximizes separation
    signal_median <- median(signal_skels$Maximum_Branch_Length, na.rm = TRUE)
    noise_median <- median(noise_skels$Maximum_Branch_Length, na.rm = TRUE)
    
    # Use midpoint
    sep_cutoff <- (signal_median + noise_median) / 2
  } else {
    sep_cutoff <- NA
  }
  
  # Method 3: Image-feature-based adaptive cutoff
  # Higher cutoffs for images with generally longer skeletons
  img_median <- img_info$median_max_length
  adaptive_cutoff <- img_median * 0.5  # 50% of image median
  
  # Choose best method
  cutoffs <- c(length_cutoff, sep_cutoff, adaptive_cutoff)
  cutoffs <- cutoffs[!is.na(cutoffs)]
  
  if (length(cutoffs) > 0) {
    # Use median of available methods
    final_cutoff <- median(cutoffs)
  } else {
    final_cutoff <- 2  # Fallback
  }
  
  return(data.frame(
    image_key = img_key,
    cutoff_method = "adaptive",
    length_cutoff = final_cutoff,
    slab_cutoff = quantile(img_skeletons$Slab_voxels, 0.3, na.rm = TRUE),
    junction_cutoff = 0  # Keep all junctions
  ))
}

# Compute cutoffs for all images
image_cutoffs <- do.call(rbind, lapply(unique(skeleton_data$image_key), function(img) {
  compute_optimal_cutoff(img, skeleton_data, image_stats)
}))

write_csv(image_cutoffs, file.path(output_dir, "adaptive_cutoffs_per_image.csv"))

cat("Adaptive cutoffs computed:\n")
print(summary(image_cutoffs %>% dplyr::select(length_cutoff, slab_cutoff)))
cat("\n")

# ========================================
# STEP 3: APPLY FILTERS & EVALUATE
# ========================================
cat("=== STEP 3: APPLYING ADAPTIVE FILTERS ===\n\n")

# Join cutoffs to skeleton data
skeleton_filtered <- skeleton_data %>%
  left_join(image_cutoffs, by = "image_key") %>%
  mutate(
    predicted_signal = case_when(
      cutoff_method == "insufficient_data" ~ FALSE,
      Maximum_Branch_Length >= length_cutoff & Slab_voxels >= slab_cutoff ~ TRUE,
      TRUE ~ FALSE
    ),
    prediction = ifelse(predicted_signal, "signal", "noise")
  )

# Evaluate performance
confusion <- skeleton_filtered %>%
  summarise(
    tp = sum(prediction == "signal" & class == "signal"),
    tn = sum(prediction == "noise" & class == "noise"),
    fp = sum(prediction == "signal" & class == "noise"),
    fn = sum(prediction == "noise" & class == "signal")
  )

precision <- confusion$tp / (confusion$tp + confusion$fp)
recall <- confusion$tp / (confusion$tp + confusion$fn)
specificity <- confusion$tn / (confusion$tn + confusion$fp)
f1 <- 2 * (precision * recall) / (precision + recall)

cat("Adaptive Filter Performance (Skeleton-Level):\n")
cat("  Precision:", round(precision, 4), "\n")
cat("  Recall:", round(recall, 4), "\n")
cat("  Specificity:", round(specificity, 4), "\n")
cat("  F1 Score:", round(f1, 4), "\n\n")

# Per-image performance
image_performance <- skeleton_filtered %>%
  group_by(image_key) %>%
  summarise(
    n_total = n(),
    n_true_signal = sum(class == "signal"),
    n_predicted_signal = sum(predicted_signal),
    n_correct = sum(prediction == class),
    accuracy = mean(prediction == class),
    tp = sum(prediction == "signal" & class == "signal"),
    fp = sum(prediction == "signal" & class == "noise"),
    fn = sum(prediction == "noise" & class == "signal"),
    precision = ifelse(tp + fp > 0, tp / (tp + fp), NA),
    recall = ifelse(tp + fn > 0, tp / (tp + fn), NA),
    .groups = "drop"
  )

write_csv(image_performance, file.path(output_dir, "per_image_filtering_performance.csv"))

cat("Per-Image Performance Summary:\n")
print(summary(image_performance %>% dplyr::select(accuracy, precision, recall)))
cat("\n")

# ========================================
# STEP 4: IMAGE-LEVEL SCORING
# ========================================
cat("=== STEP 4: COMPUTING IMAGE-LEVEL SCORES ===\n\n")

# Calculate image scores based on filtered signal skeletons
image_scores <- skeleton_filtered %>%
  filter(predicted_signal == TRUE) %>%
  group_by(image_key) %>%
  summarise(
    # Primary score: Average length of signal skeletons
    avg_signal_length = mean(Maximum_Branch_Length, na.rm = TRUE),
    median_signal_length = median(Maximum_Branch_Length, na.rm = TRUE),
    
    # Secondary metrics
    total_signal_length = sum(Maximum_Branch_Length, na.rm = TRUE),
    n_signal_skeletons = n(),
    avg_signal_branches = mean(Branches, na.rm = TRUE),
    avg_signal_junctions = mean(Junctions, na.rm = TRUE),
    
    # Quality metrics
    signal_length_cv = sd(Maximum_Branch_Length, na.rm = TRUE) / (mean(Maximum_Branch_Length, na.rm = TRUE) + 1),
    
    .groups = "drop"
  )

# Merge with true signal counts for validation
image_scores <- image_scores %>%
  left_join(
    image_stats %>% dplyr::select(image_key, n_signal, n_skeletons),
    by = "image_key"
  ) %>%
  mutate(
    capture_rate = n_signal_skeletons / n_signal,
    signal_density = n_signal_skeletons / n_skeletons
  )

# Add image-level features for context
image_scores <- image_scores %>%
  left_join(
    image_level_metadata %>% dplyr::select(image_key, contains("mask_area")),
    by = "image_key"
  )

write_csv(image_scores, file.path(output_dir, "image_level_scores.csv"))

cat("Image-Level Scores Computed:\n")
cat("  Total images:", nrow(image_scores), "\n")
print(summary(image_scores %>% dplyr::select(avg_signal_length, n_signal_skeletons, capture_rate)))
cat("\n")

# ========================================
# STEP 5: VISUALIZATIONS
# ========================================
cat("=== STEP 5: GENERATING VISUALIZATIONS ===\n\n")

# Plot 1: Distribution of adaptive cutoffs
p1 <- ggplot(image_cutoffs %>% filter(!is.na(length_cutoff)), 
             aes(x = length_cutoff)) +
  geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
  geom_vline(xintercept = median(image_cutoffs$length_cutoff, na.rm = TRUE), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Adaptive Length Cutoffs",
       subtitle = paste("Median cutoff:", 
                        round(median(image_cutoffs$length_cutoff, na.rm = TRUE), 2)),
       x = "Length Cutoff (pixels/voxels)",
       y = "Number of Images") +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "adaptive_cutoff_distribution.png"), p1, width = 10, height = 6)

# Plot 2: Image score distribution
p2 <- ggplot(image_scores, aes(x = avg_signal_length)) +
  geom_histogram(bins = 50, fill = "darkgreen", alpha = 0.7) +
  geom_vline(xintercept = median(image_scores$avg_signal_length, na.rm = TRUE),
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Image-Level Scores: Average Signal Skeleton Length",
       subtitle = paste("Median score:", 
                        round(median(image_scores$avg_signal_length, na.rm = TRUE), 2)),
       x = "Average Signal Length",
       y = "Number of Images") +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "image_scores_distribution.png"), p2, width = 10, height = 6)

# Plot 3: Cutoff vs Image characteristics
if ("mask_area_percentage" %in% colnames(image_stats)) {
  image_cutoffs_with_area <- image_cutoffs %>%
    left_join(image_stats %>% dplyr::select(image_key, mask_area_percentage), 
              by = "image_key")
  
  p3 <- ggplot(image_cutoffs_with_area %>% filter(!is.na(length_cutoff)), 
               aes(x = mask_area_percentage, y = length_cutoff)) +
    geom_point(alpha = 0.5, color = "steelblue") +
    geom_smooth(method = "loess", color = "red", se = TRUE) +
    labs(title = "Adaptive Cutoffs vs Image Mask Area",
         x = "Mask Area Percentage",
         y = "Length Cutoff") +
    theme_minimal(base_size = 12)
  
  ggsave(file.path(output_dir, "cutoff_vs_mask_area.png"), p3, width = 10, height = 6)
}

# Plot 4: Signal capture rate
p4 <- ggplot(image_scores, aes(x = capture_rate)) +
  geom_histogram(bins = 50, fill = "darkorange", alpha = 0.7) +
  geom_vline(xintercept = median(image_scores$capture_rate, na.rm = TRUE),
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Signal Capture Rate Per Image",
       subtitle = paste("Median capture rate:", 
                        round(median(image_scores$capture_rate, na.rm = TRUE), 3)),
       x = "Capture Rate (Predicted Signal / True Signal)",
       y = "Number of Images") +
  xlim(0, 1.5) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "signal_capture_rate.png"), p4, width = 10, height = 6)

# Plot 5: Score comparison between images
top_bottom_images <- image_scores %>%
  arrange(desc(avg_signal_length)) %>%
  mutate(rank = row_number(),
         category = case_when(
           rank <= 10 ~ "Top 10",
           rank > n() - 10 ~ "Bottom 10",
           TRUE ~ "Middle"
         )) %>%
  filter(category != "Middle")

if (nrow(top_bottom_images) > 0) {
  p5 <- ggplot(top_bottom_images, 
               aes(x = reorder(image_key, avg_signal_length), 
                   y = avg_signal_length, 
                   fill = category)) +
    geom_col() +
    coord_flip() +
    labs(title = "Top 10 vs Bottom 10 Images by Signal Score",
         x = "Image",
         y = "Average Signal Skeleton Length",
         fill = "Category") +
    theme_minimal(base_size = 10)
  
  ggsave(file.path(output_dir, "top_bottom_images.png"), p5, width = 10, height = 8)
}

cat("Visualizations saved to:", output_dir, "\n\n")

# ========================================
# STEP 6: SUMMARY REPORT
# ========================================
cat("=== FINAL SUMMARY REPORT ===\n\n")

cat("ADAPTIVE FILTERING APPROACH:\n")
cat("  - Used image-level features to set per-image cutoffs\n")
cat("  - Cutoffs adapt to each image's skeleton distribution\n")
cat("  - Filtered", sum(skeleton_filtered$predicted_signal), "signal skeletons\n")
cat("  - Overall F1 Score:", round(f1, 4), "\n\n")

cat("IMAGE-LEVEL SCORES:\n")
cat("  - Primary Metric: Average signal skeleton length\n")
cat("  - Score Range:", 
    round(min(image_scores$avg_signal_length, na.rm = TRUE), 2), "to",
    round(max(image_scores$avg_signal_length, na.rm = TRUE), 2), "\n")
cat("  - Median Score:", 
    round(median(image_scores$avg_signal_length, na.rm = TRUE), 2), "\n\n")

cat("TOP 5 IMAGES BY SCORE:\n")
top5 <- image_scores %>%
  arrange(desc(avg_signal_length)) %>%
  head(5) %>%
  dplyr::select(image_key, avg_signal_length, n_signal_skeletons)
print(top5)

cat("\n\nBOTTOM 5 IMAGES BY SCORE:\n")
bottom5 <- image_scores %>%
  arrange(avg_signal_length) %>%
  head(5) %>%
  dplyr::select(image_key, avg_signal_length, n_signal_skeletons)
print(bottom5)

cat("\n\nOUTPUT FILES:\n")
cat("  1. adaptive_cutoffs_per_image.csv - Cutoffs for each image\n")
cat("  2. image_level_scores.csv - Final scores for comparison\n")
cat("  3. per_image_filtering_performance.csv - Quality metrics\n")
cat("  4. image_level_statistics.csv - Raw statistics\n")

cat("\n=== ANALYSIS COMPLETE ===\n")
 
