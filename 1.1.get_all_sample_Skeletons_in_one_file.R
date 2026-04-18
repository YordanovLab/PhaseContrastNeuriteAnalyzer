suppressPackageStartupMessages({
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

skeleton_root  <- "OUT_roi_skeletons_all/Skeletonization"
metadata_path  <- "OUT_optimize/mask_area_percentages+optim_groups+inspected.csv"
out_rds        <- "allSkeletons_rawData.rds"

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

is_test_image <- function(base_no_ext) {
  cand <- list.files(
    "OUT_optimize",
    pattern = "(^|/)(choice|noise)(/|$)",
    recursive = TRUE, full.names = TRUE
  )
  if (!length(cand)) return(FALSE)
  any(grepl(paste0("(^|/)", escape_regex(base_no_ext), "([._-]|$)"),
            cand, ignore.case = TRUE))
}

# ----------------------------
# Parallel Setup
# ----------------------------
num_cores <- min(80, parallel::detectCores())
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# ----------------------------
# Get all skeleton directories
# ----------------------------
skeleton_subdirs <- list.dirs(skeleton_root, recursive = FALSE, full.names = TRUE)
if (!length(skeleton_subdirs)) stop("No skeleton folders found in: ", skeleton_root)

cat(sprintf("Found %d skeleton folders\n", length(skeleton_subdirs)))

# ----------------------------
# Chunked Parallel Processing
# ----------------------------
chunk_size <- 100
chunks <- split(skeleton_subdirs, ceiling(seq_along(skeleton_subdirs) / chunk_size))

allSkeletons <- list()
processed_total <- 0L

for (chunk_i in seq_along(chunks)) {
  subdirs <- chunks[[chunk_i]]
  cat(sprintf("Processing chunk %d/%d (%d folders)...\n",
              chunk_i, length(chunks), length(subdirs)))
  
  chunk_result <- foreach(skeleton_dir = subdirs, .packages = c("readr", "dplyr", "tools")) %dopar% {
    base_dir_name <- basename(skeleton_dir)
    base_no_ext <- strip_known_suffix(base_dir_name)
    
    # Find results CSV and summary text
    results_csv <- list.files(skeleton_dir, pattern = "_skeleton_results\\.csv$", full.names = TRUE)
    summary_txt <- list.files(skeleton_dir, pattern = "_skeleton_summary\\.txt$", full.names = TRUE)
    
    if (length(results_csv) == 0) {
      return(NULL)
    }
    
    # Read raw results CSV
    results_data <- tryCatch(
      read_csv(results_csv[1], show_col_types = FALSE),
      error = function(e) NULL
    )
    
    # Read summary text file
    summary_text <- NA_character_
    if (length(summary_txt) > 0) {
      summary_text <- tryCatch(
        paste(readLines(summary_txt[1]), collapse = "\n"),
        error = function(e) NA_character_
      )
    }
    
    if (is.null(results_data)) {
      return(NULL)
    }
    
    # Get metadata row if it exists
    meta_row <- metadata %>% filter(image_key == base_no_ext)
    meta_info <- list(
      image_name = base_no_ext,
      skeleton_folder_path = skeleton_dir,
      skeleton_results_csv_path = results_csv[1],
      skeleton_summary_txt_path = if (length(summary_txt)) summary_txt[1] else NA_character_,
      is_test_image = is_test_image(base_no_ext)
    )
    
    if (nrow(meta_row) > 0) {
      meta_info <- c(meta_info, as.list(meta_row[1, ]))
    }
    
    list(
      name = base_no_ext,
      data = list(
        metadata = meta_info,
        results_data = results_data,
        summary_text = summary_text
      )
    )
  }
  
  for (res in chunk_result) {
    if (!is.null(res)) {
      allSkeletons[[res$name]] <- res$data
      processed_total <- processed_total + 1
    }
  }
  
  cat(sprintf("Finished chunk %d | Total processed: %d\n", chunk_i, processed_total))
}

stopCluster(cl)

# ----------------------------
# Save RDS Output
# ----------------------------
saveRDS(allSkeletons, file = out_rds)
cat("\nSaved raw skeleton data to: ", normalizePath(out_rds), "\n", sep = "")
cat("Total images processed: ", length(allSkeletons), "\n", sep = "")

# ----------------------------
# TEST
# ----------------------------
# Load
allSkeletons <- readRDS("allSkeletons_rawData.rds")

# Show names
names(allSkeletons)

# View metadata of an image
allSkeletons[[1]]$metadata

# View raw results data
head(allSkeletons[[1]]$results_data)

# Access skeleton image
skeleton_img <- allSkeletons[[1]]$skeleton_image
print(skeleton_img)

# View summary text
allSkeletons[[1]]$summary_text
