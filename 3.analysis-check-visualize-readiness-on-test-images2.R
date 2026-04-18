# =============================
# Visualize ROIs in Test Set by Class (Filtered vs Unfiltered + PCA)
# =============================

library(dplyr)
library(magick)
library(readr)
library(ggplot2)

# ---------- SETTINGS ----------
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)
source(file.path(base_dir, "launchers", "pipeline_paths.R"))
ctx <- pipeline_context(base_dir)
roi_analysis_file <- ctx$paths$all_rois_analysis
best_params_file <- file.path(ctx$paths$out_opt, "parameter_optimization_results.csv")
allROIs_file <- if (file.exists(ctx$paths$all_rois_data)) ctx$paths$all_rois_data else ctx$paths$opt_rois_rds
output_dir <- file.path(ctx$paths$out_opt, "class_visualizations")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ---------- LOAD DATA ----------
cat("Loading ROI data and best parameters...\n")
roi_df <- readRDS(roi_analysis_file)
opt_params <- read_csv(best_params_file, show_col_types = FALSE) %>% 
  filter(is.finite(score)) %>% arrange(desc(score))
best_params <- opt_params[1, ]

cat("Loading allROIs object...\n")
allROIs <- readRDS(allROIs_file)

# ---------- USER-SELECTED CLASS ----------
selected_class <- "grp_high_neurites_low_confluence"
cat("Selected class:", selected_class, "\n\n")

# ---------- FILTER TO TEST SET IMAGES ----------
roi_df <- roi_df[, !duplicated(colnames(roi_df))]

image_meta <- roi_df %>%
  select(image_key, image_name,
         grp_high_neurites_low_confluence,
         grp_low_neurites_high_confluence,
         grp_high_neurites_high_confluence,
         grp_low_neurites_low_confluence,
         `_manual_choise`, manual_noise) %>%
  distinct()

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
  stop("Not enough images in selected class with both signal and noise examples.")
}

compare_imgs <- bind_rows(signal_img, noise_img)
compare_imgs$type <- c("signal", "noise")

# ---------- HELPER: ROI Polygon ----------
roi_to_xy <- function(roi_obj) {
  if (!is.null(roi_obj$coords)) {
    xy <- as.data.frame(roi_obj$coords)
    colnames(xy) <- c("x", "y")
    return(xy)
  }
  NULL
}

# ---------- VISUALIZATION FUNCTION ----------
visualize_image_with_rois <- function(image_key, label_title, filter_params = NULL) {
  rois <- allROIs[[image_key]]$rois
  img_name <- allROIs[[image_key]]$metadata$image_name
  img_path <- file.path(ctx$paths$out_clear, img_name)
  if (!file.exists(img_path)) stop("Image file not found: ", img_path)
  
  roi_data <- roi_df %>% filter(image_key == !!image_key)
  
  if (!is.null(filter_params)) {
    roi_data <- roi_data %>%
      filter(
        area >= filter_params$min_area,
        area <= filter_params$max_area,
        circularity >= filter_params$min_circularity,
        circularity <= filter_params$max_circularity,
        aspect_ratio >= filter_params$min_aspect_ratio,
        aspect_ratio <= filter_params$max_aspect_ratio
      )
  }
  
  cat("\nROI filtering parameters for", label_title, "\n")
  print(filter_params)
  cat("  => Number of ROIs after filtering:", nrow(roi_data), "\n")
  
  kept_rois <- roi_data$roi_name
  img <- image_read(img_path)
  info <- image_info(img)
  
  img_draw <- image_draw(img)
  for (roi_name in kept_rois) {
    if (roi_name %in% names(rois)) {
      xy <- roi_to_xy(rois[[roi_name]])
      if (!is.null(xy) && nrow(xy) >= 3) {
        lines(x = xy$x, y = xy$y, lwd = 2, col = "green")
      }
    }
  }
  dev.off()
  
  annotated_img <- image_annotate(img_draw, label_title, size = 30, color = "white", boxcolor = "black", location = "+10+10")
  return(annotated_img)
}

# ---------- PLOT COMPARISON ----------
for (filter_mode in c("filtered", "unfiltered")) {
  cat("Generating", filter_mode, "comparison...\n")
  
  imgs <- list()
  for (i in seq_len(nrow(compare_imgs))) {
    img_key <- compare_imgs$image_key[i]
    label <- paste0(toupper(compare_imgs$type[i]), " - ", filter_mode)
    if (filter_mode == "filtered") {
      imgs[[i]] <- visualize_image_with_rois(img_key, label, best_params)
    } else {
      imgs[[i]] <- visualize_image_with_rois(img_key, label, NULL)
    }
  }
  
  final_img <- image_append(c(imgs[[1]], imgs[[2]]), stack = FALSE)
  out_file <- file.path(output_dir, paste0("compare_", selected_class, "_", filter_mode, ".png"))
  image_write(final_img, out_file)
  cat("Saved to:", out_file, "\n")
}

# ---------- PCA ANALYSIS ON ROI FEATURES ----------
cat("\n=== PCA ANALYSIS BETWEEN SIGNAL AND NOISE IMAGE ===\n")

# Get all ROIs from both images
pca_roi_data <- roi_df %>%
  filter(image_key %in% compare_imgs$image_key) %>%
  mutate(class = ifelse(image_key == signal_img$image_key, "signal", "noise"))

# Choose numeric ROI features for PCA
numeric_features <- c("area", "perimeter", "circularity", "aspect_ratio", 
                      "solidity", "mean_width", "skeleton_length",
                      "branch_points", "endpoints", "avg_branch_length")

# Remove rows with missing values
pca_data <- pca_roi_data %>%
  select(all_of(numeric_features), class) %>%
  filter(complete.cases(.))

# Perform PCA
pca_model <- prcomp(pca_data[, numeric_features], scale. = TRUE)
pca_scores <- as.data.frame(pca_model$x[, 1:2])
pca_scores$class <- pca_data$class

# Plot PCA
p_pca <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = class)) +
  geom_point(alpha = 0.7) +
  labs(title = "PCA of ROI Features (Signal vs Noise)",
       x = "PC1", y = "PC2") +
  theme_minimal()

pca_plot_path <- file.path(output_dir, paste0("PCA_ROI_features_", selected_class, ".png"))
ggsave(pca_plot_path, p_pca, width = 8, height = 6)
cat("Saved PCA plot to:", pca_plot_path, "\n")

# Loadings
loadings <- as.data.frame(pca_model$rotation[, 1:2])
loadings$feature <- rownames(loadings)
top_features <- loadings %>%
  mutate(contrib = abs(PC1) + abs(PC2)) %>%
  arrange(desc(contrib)) %>%
  head(5)

cat("\nTop contributing ROI features to PC1 + PC2 separation:\n")
print(top_features)

write_csv(top_features, file.path(output_dir, paste0("PCA_top_features_", selected_class, ".csv")))

cat("\n✅ Done! Signal vs noise comparison and PCA completed for class:", selected_class, "\n")
