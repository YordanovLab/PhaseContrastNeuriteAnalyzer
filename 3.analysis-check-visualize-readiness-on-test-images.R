# =============================
# Visualize ROIs in Test Set by Class (Filtered vs Unfiltered)
# =============================

library(dplyr)
library(magick)
library(readr)

# ---------- SETTINGS ----------
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)
roi_analysis_file <- "allROIs_analysis.rds"
best_params_file <- "OUT_optimize/parameter_optimization_results.csv"
allROIs_file <- "allROIs_data.rds"
output_dir <- "OUT_optimize/class_visualizations"
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
# Choose one of:
# "grp_high_neurites_low_confluence"
# "grp_low_neurites_high_confluence"
# "grp_high_neurites_high_confluence"
# "grp_low_neurites_low_confluence"

selected_class <- "grp_high_neurites_high_confluence"
cat("Selected class:", selected_class, "\n\n")

# ---------- FILTER TO TEST SET IMAGES ----------
# Ensure column names are unique to prevent select() error
roi_df <- roi_df[, !duplicated(colnames(roi_df))]

# Extract relevant image-level metadata (one row per image)
image_meta <- roi_df %>%
  select(image_key, image_name,
         grp_high_neurites_low_confluence,
         grp_low_neurites_high_confluence,
         grp_high_neurites_high_confluence,
         grp_low_neurites_low_confluence,
         `_manual_choise`, manual_noise) %>%
  distinct()

# Dynamically identify which rows have any grp_* == 1 (i.e., considered test images)
grp_cols <- c("grp_high_neurites_low_confluence",
              "grp_low_neurites_high_confluence",
              "grp_high_neurites_high_confluence",
              "grp_low_neurites_low_confluence")

image_meta <- image_meta %>%
  mutate(is_in_test_group = rowSums(across(all_of(grp_cols), as.numeric)) >= 1)

# Filter only test set images in selected class
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
  img_path <- file.path("OUT_clear_bcgnd", img_name)
  if (!file.exists(img_path)) stop("Image file not found: ", img_path)
  
  # Subset ROI table for this image
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
  
  kept_rois <- roi_data$roi_name
  img <- image_read(img_path)
  info <- image_info(img)
  
  # Draw
  img_draw <- image_draw(img)
  for (roi_name in kept_rois) {
    if (roi_name %in% names(rois)) {
      xy <- roi_to_xy(rois[[roi_name]])
      if (!is.null(xy) && nrow(xy) >= 3) {
        lines(x = xy$x, y = xy$y, lwd = 2, col = "green")  # No Y inversion here
      }
    }
  }
  dev.off()
  
  # Annotate image
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
  
  # Combine and save
  final_img <- image_append(c(imgs[[1]], imgs[[2]]), stack = FALSE)
  out_file <- file.path(output_dir, paste0("compare_", selected_class, "_", filter_mode, ".png"))
  image_write(final_img, out_file)
  cat("Saved to:", out_file, "\n")
}

cat("\n✅ Done! Signal vs noise comparison completed for class:", selected_class, "\n")
