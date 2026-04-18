# ========================================
# ROI Parameter Optimization for Signal vs Noise Separation
# ========================================

# ---------- Libraries ----------
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(MASS)  # for lda()
  library(tidyr)
  library(pROC)  # for ROC analysis
})

# ---------- Settings ----------
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)
roi_file <- "allROIs_analysis.rds"
output_dir <- "OUT_optimize"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Choose the test class of interest
selected_class <- "grp_high_neurites_high_confluence"

# ---------- Load ROI Data ----------
cat("Loading ROI data...\n")
roi_df <- readRDS(roi_file)
roi_df <- roi_df[, !duplicated(colnames(roi_df))]

# ---------- Identify Test Set Images ----------
# First, check what columns are actually available
cat("Available columns in roi_df:\n")
print(colnames(roi_df))
cat("\n")

# Define the columns we need
needed_cols <- c("image_key", "image_name",
                 "grp_high_neurites_low_confluence",
                 "grp_low_neurites_high_confluence",
                 "grp_high_neurites_high_confluence",
                 "grp_low_neurites_low_confluence",
                 "_manual_choise", "manual_noise")

# Check which columns exist
existing_cols <- needed_cols[needed_cols %in% colnames(roi_df)]
missing_cols <- needed_cols[!needed_cols %in% colnames(roi_df)]

if (length(missing_cols) > 0) {
  cat("⚠️  Warning: Missing columns:", paste(missing_cols, collapse = ", "), "\n")
  cat("Proceeding with available columns only.\n\n")
}

image_meta <- roi_df %>%
  dplyr::select(all_of(existing_cols)) %>%
  dplyr::distinct()

grp_cols <- c("grp_high_neurites_low_confluence",
              "grp_low_neurites_high_confluence",
              "grp_high_neurites_high_confluence",
              "grp_low_neurites_low_confluence")

image_meta <- image_meta %>%
  mutate(is_in_test_group = rowSums(across(all_of(grp_cols), as.numeric)) >= 1)

test_images <- image_meta %>%
  filter(is_in_test_group == TRUE) %>%
  filter(.data[[selected_class]] == "1")

signal_img <- test_images %>% filter(`_manual_choise` == "1") %>% slice(1)
noise_img  <- test_images %>% filter(manual_noise == "1") %>% slice(1)

if (nrow(signal_img) == 0 | nrow(noise_img) == 0) {
  stop("❌ Not enough signal and noise images for the selected class.")
}

compare_keys <- c(signal_img$image_key, noise_img$image_key)
compare_names <- c(signal_img$image_name, noise_img$image_name)

cat("\n📊 Comparing Images:\n")
cat("  Signal:", signal_img$image_name, "\n")
cat("  Noise:", noise_img$image_name, "\n\n")

# ---------- Extract ROI Feature Data ----------
numeric_features <- c("area", "perimeter", "circularity", "aspect_ratio", 
                      "solidity", "mean_width", "skeleton_length",
                      "branch_points", "endpoints", "avg_branch_length")

roi_features <- roi_df %>%
  dplyr::filter(image_key %in% compare_keys) %>%
  dplyr::mutate(class = case_when(
    image_key == signal_img$image_key ~ "signal",
    image_key == noise_img$image_key ~ "noise"
  )) %>%
  dplyr::select(all_of(numeric_features), class) %>%
  dplyr::filter(complete.cases(.))

cat("Total ROIs:\n")
cat("  Signal:", sum(roi_features$class == "signal"), "\n")
cat("  Noise:", sum(roi_features$class == "noise"), "\n\n")

# Remove zero-variance columns
vars <- sapply(roi_features[, numeric_features], var, na.rm = TRUE)
valid_features <- names(vars[vars > 0])

if (length(valid_features) < length(numeric_features)) {
  cat("⚠️  Removed constant features:", setdiff(numeric_features, valid_features), "\n\n")
}

# ========================================
# 1. RUN LDA
# ========================================
cat("🔍 Running Linear Discriminant Analysis...\n")
lda_model <- lda(class ~ ., data = roi_features[, c(valid_features, "class")])

lda_pred <- predict(lda_model)
lda_scores <- as.data.frame(lda_pred$x)
lda_scores$class <- roi_features$class

# Plot LDA
p_lda <- ggplot(lda_scores, aes(x = LD1, fill = class)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "LDA: ROI Feature Separation",
       subtitle = paste("Signal:", signal_img$image_name, "vs Noise:", noise_img$image_name),
       x = "Linear Discriminant 1", y = "Density") +
  theme_minimal(base_size = 14)

ggsave(file.path(output_dir, "LDA_signal_vs_noise.png"), p_lda, width = 10, height = 6)

# Feature contributions
lda_weights <- data.frame(
  feature = rownames(lda_model$scaling),
  LD1_contribution = lda_model$scaling[, 1],
  abs_contrib = abs(lda_model$scaling[, 1])
) %>%
  arrange(desc(abs_contrib))

cat("\n=== TOP LDA CONTRIBUTING FEATURES ===\n")
print(lda_weights)

write_csv(lda_weights, file.path(output_dir, "LDA_feature_importance.csv"))

# ========================================
# 2. UNIVARIATE FEATURE ANALYSIS
# ========================================
cat("\n\n🔬 Analyzing individual features...\n")

feature_stats <- data.frame()

for (feat in valid_features) {
  signal_vals <- roi_features[roi_features$class == "signal", feat]
  noise_vals <- roi_features[roi_features$class == "noise", feat]
  
  # T-test
  ttest <- t.test(signal_vals, noise_vals)
  
  # Effect size (Cohen's d)
  pooled_sd <- sqrt(((length(signal_vals)-1)*var(signal_vals) + 
                       (length(noise_vals)-1)*var(noise_vals)) / 
                      (length(signal_vals) + length(noise_vals) - 2))
  cohens_d <- (mean(signal_vals) - mean(noise_vals)) / pooled_sd
  
  # ROC AUC for separation ability
  roc_obj <- roc(roi_features$class, roi_features[[feat]], 
                 levels = c("noise", "signal"), direction = "<", quiet = TRUE)
  
  feature_stats <- rbind(feature_stats, data.frame(
    feature = feat,
    signal_mean = mean(signal_vals),
    signal_sd = sd(signal_vals),
    noise_mean = mean(noise_vals),
    noise_sd = sd(noise_vals),
    p_value = ttest$p.value,
    cohens_d = cohens_d,
    auc = as.numeric(auc(roc_obj)),
    stringsAsFactors = FALSE
  ))
}

feature_stats <- feature_stats %>%
  dplyr::mutate(
    separation_power = abs(cohens_d) * auc,
    significant = p_value < 0.05
  ) %>%
  dplyr::arrange(desc(separation_power))

cat("\n=== FEATURE SEPARATION POWER ===\n")
print(feature_stats)

write_csv(feature_stats, file.path(output_dir, "feature_separation_stats.csv"))

# ========================================
# 3. OPTIMAL THRESHOLD FINDER
# ========================================
cat("\n\n🎯 Finding optimal thresholds for each feature...\n")

threshold_results <- data.frame()

for (feat in valid_features) {
  signal_vals <- roi_features[roi_features$class == "signal", feat]
  noise_vals <- roi_features[roi_features$class == "noise", feat]
  
  # Determine direction (signal higher or lower than noise?)
  signal_higher <- mean(signal_vals) > mean(noise_vals)
  
  # Test multiple threshold values
  all_vals <- c(signal_vals, noise_vals)
  thresholds <- quantile(all_vals, probs = seq(0.1, 0.9, by = 0.05))
  
  best_accuracy <- 0
  best_threshold <- NA
  best_direction <- NA
  
  for (thresh in thresholds) {
    if (signal_higher) {
      # Keep ROIs above threshold
      tp <- sum(signal_vals >= thresh)
      tn <- sum(noise_vals < thresh)
    } else {
      # Keep ROIs below threshold
      tp <- sum(signal_vals <= thresh)
      tn <- sum(noise_vals > thresh)
    }
    
    accuracy <- (tp + tn) / (length(signal_vals) + length(noise_vals))
    
    if (accuracy > best_accuracy) {
      best_accuracy <- accuracy
      best_threshold <- thresh
      best_direction <- ifelse(signal_higher, ">=", "<=")
    }
  }
  
  # Calculate sensitivity and specificity at best threshold
  if (signal_higher) {
    sensitivity <- sum(signal_vals >= best_threshold) / length(signal_vals)
    specificity <- sum(noise_vals < best_threshold) / length(noise_vals)
  } else {
    sensitivity <- sum(signal_vals <= best_threshold) / length(signal_vals)
    specificity <- sum(noise_vals > best_threshold) / length(noise_vals)
  }
  
  threshold_results <- rbind(threshold_results, data.frame(
    feature = feat,
    threshold = best_threshold,
    operator = best_direction,
    accuracy = best_accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    stringsAsFactors = FALSE
  ))
}

threshold_results <- threshold_results %>%
  dplyr::arrange(desc(accuracy))

cat("\n=== OPTIMAL SINGLE-FEATURE THRESHOLDS ===\n")
print(threshold_results)

write_csv(threshold_results, file.path(output_dir, "optimal_thresholds.csv"))

# ========================================
# 4. MULTI-FEATURE RULE GENERATOR
# ========================================
cat("\n\n🧮 Generating multi-feature filtering rules...\n")

# Select top 5 features by separation power
top_features <- head(feature_stats$feature, 5)
top_thresholds <- threshold_results %>% dplyr::filter(feature %in% top_features)

cat("\n=== RECOMMENDED FILTERING RULES ===\n")
cat("\nOption 1 - Single Best Feature:\n")
best_single <- threshold_results[1, ]
cat(sprintf("  %s %s %.3f (Accuracy: %.1f%%)\n", 
            best_single$feature, 
            best_single$operator, 
            best_single$threshold,
            best_single$accuracy * 100))

cat("\nOption 2 - Top 3 Features Combined (AND logic):\n")
for (i in 1:min(3, nrow(top_thresholds))) {
  row <- top_thresholds[i, ]
  cat(sprintf("  %s %s %.3f\n", row$feature, row$operator, row$threshold))
}

cat("\nOption 3 - LDA-based (top contributors):\n")
for (i in 1:min(3, nrow(lda_weights))) {
  feat <- lda_weights$feature[i]
  thresh_info <- threshold_results %>% dplyr::filter(feature == feat)
  if (nrow(thresh_info) > 0) {
    cat(sprintf("  %s %s %.3f (LDA weight: %.3f)\n", 
                feat, 
                thresh_info$operator, 
                thresh_info$threshold,
                lda_weights$LD1_contribution[i]))
  }
}

# ========================================
# 5. VISUALIZATIONS
# ========================================
cat("\n\n📈 Creating visualization plots...\n")

# Feature comparison plot
top5_features <- head(feature_stats$feature, 5)
plot_data <- roi_features %>%
  dplyr::select(all_of(top5_features), class) %>%
  tidyr::pivot_longer(cols = -class, names_to = "feature", values_to = "value")

p_features <- ggplot(plot_data, aes(x = class, y = value, fill = class)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~feature, scales = "free_y", ncol = 2) +
  labs(title = "Top 5 Discriminative Features",
       subtitle = "Distribution comparison between Signal and Noise ROIs",
       x = "", y = "Feature Value") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom")

ggsave(file.path(output_dir, "top_features_comparison.png"), 
       p_features, width = 10, height = 8)

# Feature importance plot
p_importance <- ggplot(feature_stats, aes(x = reorder(feature, separation_power), 
                                          y = separation_power)) +
  geom_col(aes(fill = significant)) +
  coord_flip() +
  labs(title = "Feature Separation Power",
       subtitle = "Cohen's d × AUC",
       x = "", y = "Separation Power") +
  scale_fill_manual(values = c("grey70", "steelblue"), 
                    labels = c("Not Significant", "Significant (p<0.05)")) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "feature_importance.png"), 
       p_importance, width = 8, height = 6)

# ========================================
# 6. GENERATE FILTERING CODE
# ========================================
cat("\n\n💾 Generating R filtering code...\n")

filtering_code <- c(
  "# ========================================",
  "# ROI Filtering Code - Auto-generated",
  "# ========================================",
  "",
  "# Load your ROI data",
  "# roi_data <- readRDS('your_roi_file.rds')",
  "",
  "# Option 1: Single best feature filter",
  sprintf("filtered_rois_opt1 <- roi_data %%>%%"),
  sprintf("  filter(%s %s %.3f)", 
          best_single$feature, 
          best_single$operator, 
          best_single$threshold),
  "",
  "# Option 2: Top 3 features (AND logic)",
  "filtered_rois_opt2 <- roi_data %>%"
)

for (i in 1:min(3, nrow(top_thresholds))) {
  row <- top_thresholds[i, ]
  filtering_code <- c(filtering_code,
                      sprintf("  filter(%s %s %.3f) %%>%%", 
                              row$feature, row$operator, row$threshold))
}
filtering_code[length(filtering_code)] <- gsub(" %>%$", "", filtering_code[length(filtering_code)])

filtering_code <- c(filtering_code,
                    "",
                    "# Option 3: LDA score filter",
                    "# Note: You'll need to apply the LDA model to new data",
                    "# lda_pred <- predict(lda_model, newdata = roi_data)",
                    "# filtered_rois_opt3 <- roi_data[lda_pred$x[,1] > 0, ]"
)

writeLines(filtering_code, file.path(output_dir, "filtering_code.R"))

cat("\n\n✅ ANALYSIS COMPLETE!\n")
cat("\nOutputs saved to:", output_dir, "\n")
cat("  - LDA_signal_vs_noise.png\n")
cat("  - LDA_feature_importance.csv\n")
cat("  - feature_separation_stats.csv\n")
cat("  - optimal_thresholds.csv\n")
cat("  - top_features_comparison.png\n")
cat("  - feature_importance.png\n")
cat("  - filtering_code.R\n")
cat("\n🎯 Check the console output and CSV files for recommended parameter sets!\n")
