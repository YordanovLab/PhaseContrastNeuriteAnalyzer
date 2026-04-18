# ============================
# Collect all ROIs + visualize (optimized)
# ============================

suppressPackageStartupMessages({
  library(RImageJROI)
  library(magick)
  library(readr)
  library(dplyr)
  library(foreach)
  library(doParallel)
  library(tools)
})

# ----------------------------
# Paths / Working directory
# ----------------------------
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)

roi_root    <- "OUT_optimize/ROIs"
clear_root  <- "OUT_clear_bcgnd"
opt_root    <- "OUT_optimize"
out_rds     <- "allROIs_data.rds"
metadata_path <- file.path(opt_root, "mask_area_percentages+optim_groups+inspected.csv")

# ----------------------------
# Load metadata
# ----------------------------
metadata <- read_csv(metadata_path, show_col_types = FALSE)
metadata$image_key <- tools::file_path_sans_ext(metadata$image_name)

# ----------------------------
# Helper functions
# ----------------------------
escape_regex <- function(x) gsub("([][{}()+*^$.|\\?\\\\])", "\\\\\\1", x)

strip_known_suffix <- function(x) {
  sub("(_RGavg_mask_renorm)$", "", x, perl = TRUE)
}

find_image_in_clear <- function(base_no_ext) {
  pat <- paste0("^", escape_regex(base_no_ext), ".*\\.(tif|tiff|png|jpe?g)$")
  files <- list.files(clear_root, pattern = pat, full.names = TRUE, ignore.case = TRUE)
  if (length(files) > 0) files[1] else NA_character_
}

is_test_image <- function(base_no_ext) {
  cand <- list.files(
    opt_root,
    pattern = "(^|/)(choice|noise)(/|$)",
    recursive = TRUE, full.names = TRUE
  )
  if (!length(cand)) return(FALSE)
  any(grepl(paste0("(^|/)", escape_regex(base_no_ext), "([._-]|$)"),
            cand, ignore.case = TRUE))
}

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
      y = c(roi_obj$top,  roi_obj$top,   roi_obj$bottom, roi_obj$bottom, roi_obj$top)
    ))
  }
  NULL
}

plot_image_with_rois <- function(allROIs, image_key,
                                 roi_name_regex = NULL,
                                 max_rois = Inf,
                                 line_width = 1) {
  if (!image_key %in% names(allROIs)) stop("Image key not found: ", image_key)
  img_path <- find_image_in_clear(image_key)
  if (is.na(img_path)) stop("No image found for: ", image_key)
  
  roi_list <- allROIs[[image_key]]$rois
  if (length(roi_list) == 0) stop("No ROIs for: ", image_key)
  
  roi_names <- names(roi_list)
  if (!is.null(roi_name_regex)) {
    keep <- grepl(roi_name_regex, roi_names)
    roi_list <- roi_list[keep]
  }
  if (!length(roi_list)) stop("No ROIs matched the regex for: ", image_key)
  
  if (is.finite(max_rois) && length(roi_list) > max_rois) {
    roi_list <- roi_list[seq_len(max_rois)]
  }
  
  img <- magick::image_read(img_path)
  info <- magick::image_info(img)
  W <- info$width
  H <- info$height
  
  magick::image_draw(img)
  for (nm in names(roi_list)) {
    roi <- roi_list[[nm]]
    xy <- roi_to_xy(roi)
    if (is.null(xy) || nrow(xy) < 2) next
    lines(x = xy$x, y = H - xy$y, lwd = line_width)
  }
  dev.off()
  print(img)
}

# ----------------------------
# Parallel Setup
# ----------------------------
num_cores <- min(80, parallel::detectCores())
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# ----------------------------
# Chunked Parallel Processing
# ----------------------------
roi_subdirs <- list.dirs(roi_root, recursive = FALSE, full.names = TRUE)
if (!length(roi_subdirs)) stop("No ROI folders found in: ", roi_root)

chunk_size <- 100
chunks <- split(roi_subdirs, ceiling(seq_along(roi_subdirs) / chunk_size))

allROIs <- list()
processed_total <- 0L

for (chunk_i in seq_along(chunks)) {
  subdirs <- chunks[[chunk_i]]
  cat(sprintf("Processing chunk %d/%d (%d folders)...\n",
              chunk_i, length(chunks), length(subdirs)))
  
  chunk_result <- foreach(roi_dir = subdirs, .packages = c("RImageJROI", "tools", "dplyr")) %dopar% {
    base_dir_name <- basename(roi_dir)
    base_no_ext <- strip_known_suffix(base_dir_name)
    
    roi_files <- list.files(roi_dir, pattern = "\\.roi$", full.names = TRUE)
    roi_zip <- list.files(roi_dir, pattern = "_ROIs\\.zip$", full.names = TRUE)
    overlay_png <- list.files(roi_dir, pattern = "_overlay\\.png$", full.names = TRUE)
    
    rois_list <- list()
    if (length(roi_files)) {
      for (rf in roi_files) {
        nm <- basename(rf)
        obj <- tryCatch(read.ijroi(rf), error = function(e) NULL)
        if (!is.null(obj)) rois_list[[nm]] <- obj
      }
    }
    
    img_path <- find_image_in_clear(base_no_ext)
    img_name <- if (!is.na(img_path)) basename(img_path) else paste0(base_no_ext, ".tif")
    
    # Get metadata row if it exists
    meta_row <- metadata %>% filter(image_key == base_no_ext)
    meta_info <- list(
      image_name = img_name,
      roi_folder_path = roi_dir,
      roi_zip_path = if (length(roi_zip)) roi_zip[1] else NA_character_,
      overlay_path = if (length(overlay_png)) overlay_png[1] else NA_character_,
      is_test_image = is_test_image(base_no_ext)
    )
    
    if (nrow(meta_row) > 0) {
      meta_info <- c(meta_info, as.list(meta_row[1, ]))
    }
    
    list(name = base_no_ext, data = list(metadata = meta_info, rois = rois_list))
  }
  
  for (res in chunk_result) {
    if (!is.null(res)) {
      allROIs[[res$name]] <- res$data
      processed_total <- processed_total + 1
    }
  }
  
  cat(sprintf("Finished chunk %d | Total processed: %d\n", chunk_i, processed_total))
}

stopCluster(cl)

# ----------------------------
# Save RDS Output
# ----------------------------
saveRDS(allROIs, file = out_rds)
cat("\nSaved combined ROI list to: ", normalizePath(out_rds), "\n", sep = "")

# ----------------------------
# Example checks (uncomment for interactive debugging only)
# ----------------------------
# allROIs <- readRDS("allROIs_data.rds")
# names(allROIs)
# allROIs[["28.07- ne ST_Mtt-plaka2-RA-5-2qmka_sn1"]]$metadata
# plot_image_with_rois(allROIs, "4.08-posledendenMAI-Roz+RA-diff-10-roz+RA-roz_10_qmka4_sn1")
