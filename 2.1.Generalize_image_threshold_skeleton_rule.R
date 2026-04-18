#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tools)
})

optional_package_available <- function(pkg, purpose = "optional analysis") {
  if (requireNamespace(pkg, quietly = TRUE)) {
    return(TRUE)
  }
  cat(
    "[OPTIONAL PACKAGE MISSING]", pkg, "is not installed.",
    "Skipping", purpose, "instead of failing the whole generalization step.\n"
  )
  FALSE
}

# ============================================================
# SETTINGS
# ============================================================
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)
source(file.path(base_dir, "launchers", "pipeline_paths.R"))
ctx <- pipeline_context(base_dir)
dir.create(ctx$paths$generalized_dir, recursive = TRUE, showWarnings = FALSE)

filtered_rds <- ctx$paths$generalized_rds
metadata_path <- ctx$paths$optim_groups_inspected
output_dir <- "OUT_experimental_analysis"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n=== EXPERIMENTAL FACTOR ANALYSIS ===\n\n")

# ============================================================
# LOAD DATA
# ============================================================
cat("Loading filtered skeleton data...\n")
if (!file.exists(filtered_rds)) {
  stop(
    paste0(
      "Missing prerequisite for generalization: ", filtered_rds, "\n",
      "This file is produced by the cutoff/generalized filtering stage after a saved cutoff model is applied to the image set.\n",
      "Suggested fix: in the app, choose or create a validated cutoff profile, then run the analysis step that applies that profile to the images before rerunning 3.1. Generalize with New Images."
    ),
    call. = FALSE
  )
}
if (!file.exists(metadata_path)) {
  stop(
    paste0(
      "Missing metadata table for generalization: ", metadata_path, "\n",
      "Suggested fix: check the selected metadata CSV in Configuration and rerun the upstream setup/validation steps that create the inspected optimization table."
    ),
    call. = FALSE
  )
}
skeleton_data <- readRDS(filtered_rds)

# Clean up duplicate columns from joins (remove .x, .y suffixes)
colnames(skeleton_data) <- gsub("\\.x$", "", colnames(skeleton_data))
colnames(skeleton_data) <- gsub("\\.y$", "", colnames(skeleton_data))

# Remove completely duplicate columns
skeleton_data <- skeleton_data[, !duplicated(colnames(skeleton_data))]

# Standardize column names (convert dots to underscores for consistency)
colnames(skeleton_data) <- gsub("\\.", "_", colnames(skeleton_data))

# Load experimental metadata
metadata <- read_csv(metadata_path, show_col_types = FALSE)
metadata$image_key <- tools::file_path_sans_ext(metadata$image_name)

cat("  Total skeletons (signal):", nrow(skeleton_data), "\n")
cat("  Total images:", n_distinct(skeleton_data$image_key), "\n")
cat("  Skeleton columns:", paste(head(colnames(skeleton_data)[1:15], 15), collapse = ", "), "...\n\n")

# ============================================================
# STEP 1: AGGREGATE TO IMAGE LEVEL
# ============================================================
cat("Aggregating skeleton statistics to image level...\n\n")

image_features <- skeleton_data %>%
  filter(predicted_signal == TRUE) %>%
  group_by(image_key) %>%
  summarise(
    # Branch morphology
    mean_max_branch_length = mean(Maximum_Branch_Length, na.rm = TRUE),
    median_max_branch_length = median(Maximum_Branch_Length, na.rm = TRUE),
    sd_max_branch_length = sd(Maximum_Branch_Length, na.rm = TRUE),
    mean_avg_branch_length = mean(Average_Branch_Length, na.rm = TRUE),
    
    # Branching complexity
    mean_branches = mean(Branches, na.rm = TRUE),
    total_branches = sum(Branches, na.rm = TRUE),
    mean_junctions = mean(Junctions, na.rm = TRUE),
    total_junctions = sum(Junctions, na.rm = TRUE),
    
    # Branch points
    mean_triple_points = mean(Triple_points, na.rm = TRUE),
    mean_quadruple_points = mean(Quadruple_points, na.rm = TRUE),
    
    # Voxel content
    mean_slab_voxels = mean(Slab_voxels, na.rm = TRUE),
    mean_junction_voxels = mean(Junction_voxels, na.rm = TRUE),
    mean_endpoint_voxels = mean(End_point_voxels, na.rm = TRUE),
    
    # Skeleton count
    n_skeletons = n(),
    
    .groups = "drop"
  ) %>%
  # Merge back experimental factors and image names
  left_join(
    metadata %>% dplyr::select(image_key, image_name, contains("grp_"), 
                               contains("treatment"), contains("concentration")),
    by = "image_key"
  )

write_csv(image_features, file.path(output_dir, "image_aggregated_features.csv"))
cat("  Saved image-level feature aggregation.\n\n")

# ============================================================
# STEP 2: IDENTIFY EXPERIMENTAL FACTORS
# ============================================================
cat("Identifying experimental grouping factors...\n\n")

# Look for columns that define groups
grp_cols <- colnames(image_features)[grepl("^grp_", colnames(image_features))]
cat("Detected grouping columns:", paste(grp_cols, collapse = ", "), "\n\n")

# Extract feature columns (numeric, excluding group/metadata)
feature_cols <- c(
  "mean_max_branch_length", "median_max_branch_length", "sd_max_branch_length",
  "mean_avg_branch_length", "mean_branches", "total_branches",
  "mean_junctions", "total_junctions", "mean_triple_points",
  "mean_quadruple_points", "mean_slab_voxels", "mean_junction_voxels",
  "mean_endpoint_voxels", "n_skeletons"
)

feature_cols <- feature_cols[feature_cols %in% colnames(image_features)]
cat("Features for analysis:", paste(feature_cols, collapse = ", "), "\n\n")

# ============================================================
# STEP 3: UNIVARIATE STATISTICS BY GROUP
# ============================================================
cat("=== UNIVARIATE STATISTICS ===\n\n")

univariate_results <- data.frame()

for (feat in feature_cols) {
  for (grp_col in grp_cols) {
    test_data <- image_features %>%
      filter(!is.na(.data[[grp_col]]), !is.na(.data[[feat]])) %>%
      group_by(.data[[grp_col]]) %>%
      summarise(
        n = n(),
        mean = mean(.data[[feat]], na.rm = TRUE),
        sd = sd(.data[[feat]], na.rm = TRUE),
        median = median(.data[[feat]], na.rm = TRUE),
        min = min(.data[[feat]], na.rm = TRUE),
        max = max(.data[[feat]], na.rm = TRUE),
        .groups = "drop"
      )
    
    # ANOVA or Kruskal-Wallis
    if (nrow(test_data) >= 2) {
      test_vals <- image_features %>%
        filter(!is.na(.data[[grp_col]]), !is.na(.data[[feat]])) %>%
        pull(feat)
      
      grp_factor <- image_features %>%
        filter(!is.na(.data[[grp_col]]), !is.na(.data[[feat]])) %>%
        pull(grp_col)
      
      # ANOVA
      aov_model <- aov(test_vals ~ factor(grp_factor))
      aov_p <- summary(aov_model)[[1]]$`Pr(>F)`[1]
      
      # Kruskal-Wallis (non-parametric alternative)
      kw_p <- kruskal.test(test_vals ~ factor(grp_factor))$p.value
      
      # Effect size (eta-squared for ANOVA)
      ss_total <- sum((test_vals - mean(test_vals))^2)
      ss_error <- sum(residuals(aov_model)^2)
      eta_sq <- (ss_total - ss_error) / ss_total
      
      univariate_results <- rbind(univariate_results, data.frame(
        feature = feat,
        group_column = grp_col,
        n_groups = nrow(test_data),
        anova_p_value = aov_p,
        kruskal_wallis_p_value = kw_p,
        eta_squared = eta_sq,
        significant_anova = aov_p < 0.05,
        significant_kw = kw_p < 0.05,
        stringsAsFactors = FALSE
      ))
    }
  }
}

write_csv(univariate_results, file.path(output_dir, "univariate_anova_results.csv"))
cat("Saved univariate test results.\n\n")

# ============================================================
# STEP 4: MULTIVARIATE ANALYSIS (MANOVA)
# ============================================================
cat("=== MULTIVARIATE ANALYSIS (MANOVA) ===\n\n")

manova_results <- list()

for (grp_col in grp_cols) {
  test_data <- image_features %>%
    filter(!is.na(.data[[grp_col]]))
  
  # Remove rows with missing features
  test_data <- test_data[complete.cases(test_data[, feature_cols]), ]
  
  if (nrow(test_data) >= 3 && n_distinct(test_data[[grp_col]]) >= 2) {
    # Prepare matrix of features
    feature_matrix <- as.matrix(test_data[, feature_cols])
    
    # MANOVA
    manova_model <- manova(feature_matrix ~ factor(test_data[[grp_col]]))
    manova_summary <- summary(manova_model, test = "Wilks")
    
    cat("MANOVA for:", grp_col, "\n")
    print(manova_summary)
    cat("\n")
    
    manova_results[[grp_col]] <- list(
      model = manova_model,
      summary = manova_summary,
      n_images = nrow(test_data),
      n_groups = n_distinct(test_data[[grp_col]])
    )
  }
}

cat("\n")

# ============================================================
# STEP 5: DIMENSIONALITY REDUCTION
# ============================================================
cat("=== DIMENSIONALITY REDUCTION ===\n\n")

reduction_results <- list()
pca_result <- NULL
pca_data <- data.frame()
tsne_data <- data.frame()
umap_data <- data.frame()

if (length(grp_cols) < 1) {
  cat("[WARNING] No grp_* grouping columns were detected. Skipping dimensionality-reduction plots.\n\n")
} else {
  prep_rows <- !is.na(image_features[[grp_cols[1]]])
  prep_data <- image_features %>%
    filter(prep_rows) %>%
    dplyr::select(all_of(feature_cols))

  complete_rows <- complete.cases(prep_data)
  prep_data <- prep_data[complete_rows, , drop = FALSE]
  prep_image_keys <- image_features$image_key[prep_rows][complete_rows]
  prep_groups <- image_features[[grp_cols[1]]][prep_rows][complete_rows]

  varying_cols <- vapply(prep_data, function(x) is.numeric(x) && sd(x, na.rm = TRUE) > 0, logical(1))
  prep_data <- prep_data[, varying_cols, drop = FALSE]

  if (nrow(prep_data) < 3 || ncol(prep_data) < 2) {
    cat("[WARNING] Not enough complete, variable image-level features for dimensionality reduction.\n\n")
  } else {
    prep_data_scaled <- scale(prep_data)

    # PCA uses base R and should always be available.
    cat("Computing PCA with base R prcomp()...\n")
    pca_result <- prcomp(prep_data_scaled)
    pc_cols <- seq_len(min(3, ncol(pca_result$x)))
    pca_data <- as.data.frame(pca_result$x[, pc_cols, drop = FALSE])
    pca_data$image_key <- prep_image_keys
    pca_data$group <- prep_groups
    reduction_results$pca <- pca_data

    if (optional_package_available("Rtsne", "t-SNE embedding")) {
      cat("Computing t-SNE (may take a moment)...\n")
      set.seed(42)
      tsne_perplexity <- max(1, min(30, floor((nrow(prep_data_scaled) - 1) / 3)))
      tsne_result <- Rtsne::Rtsne(prep_data_scaled, dims = 2, perplexity = tsne_perplexity)
      tsne_data <- as.data.frame(tsne_result$Y)
      colnames(tsne_data) <- c("tSNE1", "tSNE2")
      tsne_data$image_key <- pca_data$image_key
      tsne_data$group <- pca_data$group
      reduction_results$tsne <- tsne_data
    }

    if (optional_package_available("umap", "UMAP embedding")) {
      cat("Computing UMAP...\n")
      umap_result <- umap::umap(prep_data_scaled)
      umap_data <- as.data.frame(umap_result$layout)
      colnames(umap_data) <- c("UMAP1", "UMAP2")
      umap_data$image_key <- pca_data$image_key
      umap_data$group <- pca_data$group
      reduction_results$umap <- umap_data
    }

    saveRDS(reduction_results, file.path(output_dir, "dimensionality_reduction_results.rds"))

    cat("  PCA variance explained (PC1):", round(100 * pca_result$sdev[1]^2 / sum(pca_result$sdev^2), 2), "%\n")
    cat("  PCA variance explained (available PCs up to 3):",
        round(100 * sum(pca_result$sdev[pc_cols]^2) / sum(pca_result$sdev^2), 2), "%\n\n")
  }
}

# ============================================================
# STEP 6: VISUALIZATIONS
# ============================================================
cat("Generating visualizations...\n\n")

# Plot 1: PCA
if (nrow(pca_data) > 0 && all(c("PC1", "PC2") %in% colnames(pca_data))) {
  p_pca <- ggplot(pca_data, aes(x = PC1, y = PC2, color = group, size = 3)) +
    geom_point(alpha = 0.7) +
    labs(title = "PCA: Neurite Skeleton Features by Experimental Group",
         x = paste0("PC1 (", round(100 * pca_result$sdev[1]^2 / sum(pca_result$sdev^2), 1), "%)"),
         y = paste0("PC2 (", round(100 * pca_result$sdev[2]^2 / sum(pca_result$sdev^2), 1), "%)"),
         color = "Group") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
  ggsave(file.path(output_dir, "pca_plot.png"), p_pca, width = 10, height = 8)
} else {
  cat("[WARNING] PCA plot skipped because PCA coordinates were not available.\n")
}

# Plot 2: t-SNE
if (nrow(tsne_data) > 0) {
  p_tsne <- ggplot(tsne_data, aes(x = tSNE1, y = tSNE2, color = group, size = 3)) +
    geom_point(alpha = 0.7) +
    labs(title = "t-SNE: Neurite Skeleton Features by Experimental Group",
         x = "t-SNE1", y = "t-SNE2", color = "Group") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
  ggsave(file.path(output_dir, "tsne_plot.png"), p_tsne, width = 10, height = 8)
}

# Plot 3: UMAP
if (nrow(umap_data) > 0) {
  p_umap <- ggplot(umap_data, aes(x = UMAP1, y = UMAP2, color = group, size = 3)) +
    geom_point(alpha = 0.7) +
    labs(title = "UMAP: Neurite Skeleton Features by Experimental Group",
         x = "UMAP1", y = "UMAP2", color = "Group") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
  ggsave(file.path(output_dir, "umap_plot.png"), p_umap, width = 10, height = 8)
}

# Plot 4: Feature distributions by group (top 4 significant features)
if (nrow(univariate_results) > 0) {
  top_features <- univariate_results %>%
    filter(significant_anova) %>%
    slice_max(eta_squared, n = 4) %>%
    pull(feature)
  
  if (length(top_features) > 0) {
    plot_data <- image_features %>%
      dplyr::select(all_of(c(grp_cols[1], top_features))) %>%
      pivot_longer(cols = -1, names_to = "feature", values_to = "value")
    
    p_distributions <- ggplot(plot_data, aes(x = .data[[grp_cols[1]]], y = value, fill = .data[[grp_cols[1]]])) +
      geom_boxplot(alpha = 0.7) +
      facet_wrap(~feature, scales = "free_y", ncol = 2) +
      labs(title = "Top Significant Features by Group",
           x = "", y = "Value", fill = "Group") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggsave(file.path(output_dir, "significant_features_boxplots.png"), 
           p_distributions, width = 12, height = 8)
  }
}

cat("  Plots saved to:", output_dir, "\n\n")

# ============================================================
# SUMMARY
# ============================================================
cat("=== EXPERIMENTAL ANALYSIS COMPLETE ===\n\n")
cat("Files generated:\n")
cat("  • image_aggregated_features.csv\n")
cat("  • univariate_anova_results.csv\n")
cat("  • dimensionality_reduction_results.rds\n")
cat("  • pca_plot.png\n")
cat("  • tsne_plot.png\n")
cat("  • umap_plot.png\n")
cat("  • significant_features_boxplots.png\n")
cat("\nOutput directory:", normalizePath(output_dir), "\n\n")
