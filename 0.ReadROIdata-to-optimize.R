# Load required libraries
library(readr)
library(dplyr)
library(RImageJROI)
library(foreach)
library(doParallel)
library(fs)
library(tibble)

# Resolve base directory from command line or current working directory
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)
source(file.path(base_dir, "launchers", "pipeline_paths.R"))
ctx <- pipeline_context(base_dir)
dir.create(ctx$paths$out_opt, recursive = TRUE, showWarnings = FALSE)

# Read metadata
metadata_path <- ctx$paths$optim_groups_inspected
metadata <- read_csv(metadata_path, show_col_types = FALSE)

# Filter relevant images
selected_images <- metadata %>%
  filter(`_manual_choise` == 1 | manual_noise == 1) %>%
  select(
    image_name,
    grp_high_neurites_low_confluence,
    grp_low_neurites_high_confluence,
    grp_high_neurites_high_confluence,
    grp_low_neurites_low_confluence,
    `_manual_choise`,
    manual_noise
  )

cat("Found", nrow(selected_images), "selected images\n")
cat("  manual choice images:", sum(selected_images$`_manual_choise` == 1, na.rm = TRUE), "\n")
cat("  manual noise images:", sum(selected_images$manual_noise == 1, na.rm = TRUE), "\n")

if (nrow(selected_images) == 0) {
  stop(
    "No manually selected images were found in the inspected CSV. ",
    "Mark at least one image as choice or noise in Manual Inspection, then rerun writeback."
  )
}

if (sum(selected_images$manual_noise == 1, na.rm = TRUE) == 0) {
  cat("[WARN] No images are marked as manual_noise. Packaging can continue, but threshold optimization will run without a manually curated noise reference.\n")
}

# Define ROI root
roi_root <- ctx$paths$roi_root

resolve_roi_folder <- function(roi_root, image_name) {
  img_name_no_ext <- tools::file_path_sans_ext(image_name)
  candidates <- c(
    file.path(roi_root, paste0(img_name_no_ext, "_RGavg_mask_renorm")),
    file.path(roi_root, paste0(img_name_no_ext, "_RGavg_mask"))
  )
  hit <- candidates[dir.exists(candidates)]
  if (length(hit)) hit[[1]] else candidates[[1]]
}

# Setup parallel backend. Keep this conservative because ROI folders can contain
# many thousands of small .roi files and excessive workers can make WSL/Linux IO
# feel stuck rather than faster.
detected_cores <- parallel::detectCores()
num_cores <- max(1L, min(2L, detected_cores - 1L))
cat("Using", num_cores, "worker(s) for ROI packaging\n")
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Process in chunks (adjust chunk size if needed)
chunk_size <- 50
image_chunks <- split(selected_images, (seq_len(nrow(selected_images)) - 1) %/% chunk_size)

optROIs <- list()
summary_list <- list()
chunk_files <- c()  # Track intermediate files

# Process chunks in a loop
for (chunk_index in seq_along(image_chunks)) {
  image_chunk <- image_chunks[[chunk_index]]
  cat("\nProcessing chunk", chunk_index, "of", length(image_chunks), "\n")
  
  chunk_results <- foreach(i = seq_len(nrow(image_chunk)), .packages = c("RImageJROI", "tools", "fs", "dplyr")) %dopar% {
    row <- image_chunk[i, ]
    img_name <- row$image_name
    img_name_no_ext <- tools::file_path_sans_ext(img_name)
    roi_folder <- resolve_roi_folder(roi_root, img_name)
    
    if (!dir.exists(roi_folder)) {
      warning("ROI folder not found: ", roi_folder)
      return(NULL)
    }
    
    rois_list <- list()
    roi_files <- list.files(roi_folder, pattern = "\\.roi$", full.names = TRUE)
    roi_zip <- list.files(roi_folder, pattern = "_ROIs\\.zip$", full.names = TRUE)
    
    # Read ROI files
    if (length(roi_files) > 0) {
      for (roi_file in roi_files) {
        roi_name <- basename(roi_file)
        tryCatch({
          roi_data <- read.ijroi(roi_file)
          rois_list[[roi_name]] <- roi_data
        }, error = function(e) {
          warning("Error reading ROI file ", roi_file, ": ", e$message)
        })
      }
    }
    
    metadata_info <- list(
      image_name = img_name,
      grp_high_neurites_low_confluence = row$grp_high_neurites_low_confluence,
      grp_low_neurites_high_confluence = row$grp_low_neurites_high_confluence,
      grp_high_neurites_high_confluence = row$grp_high_neurites_high_confluence,
      grp_low_neurites_low_confluence = row$grp_low_neurites_low_confluence,
      manual_choise = row$`_manual_choise`,
      manual_noise = row$manual_noise,
      roi_folder_path = roi_folder,
      roi_zip_path = ifelse(length(roi_zip) > 0, roi_zip[1], NA)
    )
    
    list(
      name = img_name_no_ext,
      metadata = metadata_info,
      rois = rois_list,
      roi_count = length(rois_list)
    )
  }
  
  # Store results of this chunk
  for (res in chunk_results) {
    if (!is.null(res)) {
      optROIs[[res$name]] <- list(metadata = res$metadata, rois = res$rois)
      summary_list[[res$name]] <- tibble(
        image_name = res$metadata$image_name,
        roi_count = res$roi_count,
        grp_high_neurites_low_confluence = res$metadata$grp_high_neurites_low_confluence,
        grp_low_neurites_high_confluence = res$metadata$grp_low_neurites_high_confluence,
        grp_high_neurites_high_confluence = res$metadata$grp_high_neurites_high_confluence,
        grp_low_neurites_low_confluence = res$metadata$grp_low_neurites_low_confluence,
        manual_choise = res$metadata$manual_choise,
        manual_noise = res$metadata$manual_noise
      )
    }
  }
  
  # Save intermediate chunk
  chunk_file <- file.path(ctx$paths$out_opt, paste0("optROIs_data_chunk_", chunk_index, ".rds"))
  saveRDS(optROIs, file = chunk_file)
  chunk_files <- c(chunk_files, chunk_file)
  cat("Saved intermediate chunk:", chunk_index, "\n")
}

# Stop cluster
stopCluster(cl)

# Final save
final_rds <- ctx$paths$opt_rois_rds
saveRDS(optROIs, file = final_rds)
cat("Saved full optROIs data to", final_rds, "\n")

all_rois_rds <- ctx$paths$all_rois_data
saveRDS(optROIs, file = all_rois_rds)
cat("Saved compatibility copy for downstream visualization scripts to", all_rois_rds, "\n")

# Combine and save summary
summary_df <- bind_rows(summary_list)
summary_csv <- file.path(ctx$paths$out_opt, "optROIs_summary.csv")
write_csv(summary_df, summary_csv)
cat("Summary saved to:", summary_csv, "\n")

# Cleanup intermediate chunk files
cat("Deleting intermediate chunk files...\n")
file.remove(chunk_files)
cat("Cleanup complete. Deleted", length(chunk_files), "chunk files.\n")
