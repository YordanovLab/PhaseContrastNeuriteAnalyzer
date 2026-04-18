# ============================
# Collect all ROIs + visualize
# ============================

# Required packages
suppressPackageStartupMessages({
  library(RImageJROI)  # read.ijroi
  library(magick)      # image_read + overlay drawing
})

# ----------------------------
# Paths / Working directory
# ----------------------------
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)

roi_root    <- "OUT_optimize/ROIs"
clear_root  <- "OUT_clear_bcgnd"   # original/cleaned images live here
opt_root    <- "OUT_optimize"      # used to detect test-set membership
out_rds     <- "allROIs_data.rds"  # saved in core_images (cwd)

# ----------------------------
# Helpers
# ----------------------------

escape_regex <- function(x) gsub("([][{}()+*^$.|\\?\\\\])", "\\\\\\1", x)

strip_known_suffix <- function(x) {
  # remove a common ROI-folder suffix if present
  sub("(_RGavg_mask_renorm)$", "", x, perl = TRUE)
}

# Find the corresponding image file in OUT_clear_bcgnd
find_image_in_clear <- function(base_no_ext) {
  pat <- paste0("^", escape_regex(base_no_ext), ".*\\.(tif|tiff|png|jpe?g)$")
  files <- list.files(clear_root, pattern = pat, full.names = TRUE, ignore.case = TRUE)
  if (length(files) > 0) files[1] else NA_character_
}

# Determine whether an image is part of the "test" set by presence under .../(choice|noise)/...
is_test_image <- function(base_no_ext) {
  # Find any path under OUT_optimize that has a choice/noise segment
  cand <- list.files(
    opt_root,
    pattern = "(^|/)(choice|noise)(/|$)",
    recursive = TRUE, full.names = TRUE
  )
  if (!length(cand)) return(FALSE)
  # Match by base name in file/folder
  any(grepl(paste0("(^|/)", escape_regex(base_no_ext), "([._-]|$)"),
            cand, ignore.case = TRUE))
}

# Robust ROI polygon extraction (works for polygon/freehand; falls back to bounding box)
roi_to_xy <- function(roi_obj) {
  # 1) coords matrix (common for polygon/freehand)
  if (!is.null(roi_obj$coords)) {
    xy <- as.data.frame(roi_obj$coords)
    colnames(xy) <- c("x", "y")
    return(xy)
  }
  # 2) x/y vectors (alternate representation)
  if (!is.null(roi_obj$x) && !is.null(roi_obj$y)) {
    return(data.frame(x = roi_obj$x, y = roi_obj$y))
  }
  # 3) rectangle/oval fallback to bounding box
  if (!is.null(roi_obj$left) && !is.null(roi_obj$top) &&
      !is.null(roi_obj$right) && !is.null(roi_obj$bottom)) {
    return(data.frame(
      x = c(roi_obj$left, roi_obj$right, roi_obj$right, roi_obj$left, roi_obj$left),
      y = c(roi_obj$top,  roi_obj$top,   roi_obj$bottom, roi_obj$bottom, roi_obj$top)
    ))
  }
  NULL
}

# Visualize an image + its ROIs (drawn as polygons / bounding boxes)
plot_image_with_rois <- function(allROIs, image_key,
                                 roi_name_regex = NULL,
                                 max_rois = Inf,
                                 line_width = 1) {
  if (!image_key %in% names(allROIs)) {
    stop("Image key '", image_key, "' not found in allROIs names.")
  }
  
  img_path <- find_image_in_clear(image_key)
  if (is.na(img_path)) stop("No image found in '", clear_root, "' for: ", image_key)
  
  # collect ROI objects
  roi_list <- allROIs[[image_key]]$rois
  if (length(roi_list) == 0) stop("No ROIs available for: ", image_key)
  
  # optional filter by ROI filename
  roi_names <- names(roi_list)
  if (!is.null(roi_name_regex)) {
    keep <- grepl(roi_name_regex, roi_names)
    roi_list <- roi_list[keep]
  }
  if (!length(roi_list)) stop("No ROIs match the provided 'roi_name_regex' for: ", image_key)
  
  if (is.finite(max_rois) && length(roi_list) > max_rois) {
    roi_list <- roi_list[seq_len(max_rois)]
  }
  
  # read image
  img <- magick::image_read(img_path)
  info <- magick::image_info(img)
  W <- info$width; H <- info$height
  
  # draw
  magick::image_draw(img)  # opens a drawing device with the image
  # Use pixel coordinates; invert Y (ROIs are usually top-left origin; base R is bottom-left)
  for (nm in names(roi_list)) {
    roi <- roi_list[[nm]]
    xy <- roi_to_xy(roi)
    if (is.null(xy) || nrow(xy) < 2) next
    # invert Y to match plotting coords
    lines(x = xy$x, y = H - xy$y, lwd = line_width)
  }
  dev.off()  # commits drawing to the magick image and closes device
  
  print(img) # show result in the viewer/plot pane
}

# ----------------------------
# Load every ROI folder
# ----------------------------
if (!dir.exists(roi_root)) stop("ROI root folder not found: ", roi_root)

roi_subdirs <- list.dirs(roi_root, recursive = FALSE, full.names = TRUE)
if (!length(roi_subdirs)) stop("No ROI subfolders found in: ", roi_root)

allROIs <- list()
processed <- 0L

for (roi_dir in roi_subdirs) {
  base_dir_name <- basename(roi_dir)
  base_no_ext   <- strip_known_suffix(base_dir_name)
  
  # Gather ROI files
  roi_files   <- list.files(roi_dir, pattern = "\\.roi$", full.names = TRUE)
  roi_zip     <- list.files(roi_dir, pattern = "_ROIs\\.zip$", full.names = TRUE)
  overlay_png <- list.files(roi_dir, pattern = "_overlay\\.png$", full.names = TRUE)
  
  # Read ROIs (safely)
  rois_list <- list()
  if (length(roi_files)) {
    for (rf in roi_files) {
      nm <- basename(rf)
      obj <- tryCatch(read.ijroi(rf), error = function(e) NULL)
      if (!is.null(obj)) rois_list[[nm]] <- obj
    }
  }
  
  # Prefer a true filename from OUT_clear_bcgnd if present
  img_path <- find_image_in_clear(base_no_ext)
  img_name <- if (!is.na(img_path)) basename(img_path) else paste0(base_no_ext, ".tif")
  
  allROIs[[base_no_ext]] <- list(
    metadata = list(
      image_name     = img_name,
      roi_folder_path= roi_dir,
      roi_zip_path   = if (length(roi_zip)) roi_zip[1] else NA_character_,
      overlay_path   = if (length(overlay_png)) overlay_png[1] else NA_character_,
      is_test_image  = is_test_image(base_no_ext)
    ),
    rois = rois_list
  )
  
  processed <- processed + 1L
  cat(sprintf("Processed %-4d | %-s  (ROIs: %d)\n",
              processed, base_no_ext, length(rois_list)))
}

# Save combined RDS in core_images
saveRDS(allROIs, file = out_rds)
cat("\nSaved combined ROI list to: ", normalizePath(out_rds), "\n", sep = "")

# ----------------------------
# Example usage (uncomment):
# ----------------------------
# 1) Load later:
# allROIs <- readRDS("allROIs_data.rds")
#
# 2) See which entries are test images:
# which(vapply(allROIs, function(x) isTRUE(x$metadata$is_test_image), logical(1)))
#
# 3) Visualize one image + its ROIs:
# plot_image_with_rois(allROIs, image_key = "4.08-posledendenMAI-Roz+RA-diff-10-roz+RA-roz_10_qmka4_sn1",
#                      roi_name_regex = NULL,  # e.g. "ROI_00(1|2)"
#                      max_rois = 500,
#                      line_width = 1)
