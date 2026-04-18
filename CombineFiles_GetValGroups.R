#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(purrr)
  library(tibble)
  library(rlang)
  library(patchwork)
})

# ---------------- Args & Paths ----------------
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
source(file.path(base_dir, "launchers", "pipeline_paths.R"))
ctx <- pipeline_context(base_dir)

a_path   <- ctx$paths$metadata
b_path   <- ctx$paths$mask_percentages
out_dir  <- ctx$paths$out_opt
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

message("[INFO] Base dir: ", base_dir)
message("[INFO] Reading A (metadata): ", a_path)
message("[INFO] Reading B (mask percentages): ", b_path)

# ---------------- Load Data ----------------
# (silence "New names:" message)
a <- suppressMessages(read_csv(a_path, show_col_types = FALSE))
b <- suppressMessages(read_csv(b_path, show_col_types = FALSE))

# Normalize image_name from B to match A
b <- b %>%
  mutate(image_name = sub("_RGavg_mask_renorm\\.tif$", ".tif", image_name))

b <- b %>%
  mutate(image_name = sub("_RGavg_mask\\.tif$", ".tif", image_name))

# Sanity checks
overlap <- intersect(a$image_name, b$image_name)
duplicates_a <- a$image_name[duplicated(a$image_name)]
duplicates_b <- b$image_name[duplicated(b$image_name)]
message("[INFO] Overlap n = ", length(overlap))
if (length(duplicates_a)) message("[WARN] Duplicated image_name in A: ", paste(unique(duplicates_a), collapse = ", "))
if (length(duplicates_b)) message("[WARN] Duplicated image_name in B: ", paste(unique(duplicates_b), collapse = ", "))

if (length(overlap) == 0) {
  a_only <- setdiff(unique(a$image_name), unique(b$image_name))
  b_only <- setdiff(unique(b$image_name), unique(a$image_name))
  stop(
    paste(
      "Metadata filename mismatch detected:",
      "the metadata table and the mask-area table have zero overlapping image_name values after suffix normalization.",
      "Examples only in metadata:",
      paste(utils::head(a_only, 5), collapse = ", "),
      "Examples only in measured masks:",
      paste(utils::head(b_only, 5), collapse = ", "),
      "Please make sure the metadata CSV uses the same renamed image filenames as the pipeline outputs.",
      sep = "\n"
    )
  )
}

# ---------------- Merge & Derive Measures ----------------
# Expecting columns: image_name (both), background% (in b), neurites_percent (in b), dir (in a or b)
merged_df <- inner_join(a, b, by = "image_name") %>%
  mutate(confluence = 100 - `background%`)

# ---------------- Data-quality checks ----------------
neurite_values <- merged_df$neurites_percent
neurite_values <- neurite_values[!is.na(neurite_values)]
neurite_unique <- unique(neurite_values)
neurite_nonzero_n <- sum(neurite_values > 0)

message("[INFO] Matched rows after merge = ", nrow(merged_df))
message("[INFO] Non-zero neurites rows = ", neurite_nonzero_n, " / ", length(neurite_values))

if (!length(neurite_values)) {
  stop(
    paste(
      "Validation grouping cannot continue:",
      "the merged metadata + mask table has no usable neurites_percent values.",
      "Please check the mask-area table and the upstream segmentation outputs.",
      sep = "\n"
    )
  )
}

if (all(neurite_values == 0) || length(neurite_unique) <= 1) {
  stop(
    paste(
      "Validation grouping cannot continue because neurites_percent is degenerate after merging.",
      sprintf("Matched rows: %s", nrow(merged_df)),
      sprintf("Non-zero neurites rows: %s", neurite_nonzero_n),
      sprintf("Unique neurites values: %s", paste(utils::head(sort(neurite_unique), 10), collapse = ", ")),
      "This usually means the current segmentation / mask quantification produced no neurite signal, so the quadrant thresholds would be artificial.",
      "Please inspect the ilastik masks and the mask-area percentages before creating validation groups.",
      sep = "\n"
    )
  )
}

# ---------------- Selection Function ----------------
select_quadrants <- function(df,
                             confluence_col = "confluence",
                             neurites_col  = "neurites_percent",
                             image_name_col = "image_name",
                             target_n = 15,
                             hi_start = 0.95,   # starting "high" quantile
                             lo_start = 0.05,   # starting "low" quantile
                             relax_step = 0.01, # how much to relax per step
                             min_hi = 0.65,     # don't relax beyond these
                             max_lo = 0.35,
                             seed = 42) {
  stopifnot(confluence_col %in% names(df),
            neurites_col  %in% names(df),
            image_name_col %in% names(df))

  set.seed(seed)

  # Bring working columns into standard names but keep ALL original columns
  x <- df %>%
    mutate(
      confluence = .data[[confluence_col]],
      neurites   = .data[[neurites_col]],
      image_name = .data[[image_name_col]]
    ) %>%
    filter(!is.na(confluence), !is.na(neurites), !is.na(image_name))

  # 1) Remove outliers by variable (Tukey 1.5*IQR rule)
  iqr_filter <- function(v) {
    q <- quantile(v, probs = c(0.25, 0.75), names = FALSE, na.rm = TRUE)
    iqr <- q[2] - q[1]
    lb  <- q[1] - 1.5 * iqr
    ub  <- q[2] + 1.5 * iqr
    v >= lb & v <= ub
  }
  x <- x %>% filter(iqr_filter(confluence), iqr_filter(neurites))

  requested_target_n <- target_n
  max_possible_balanced_n <- max(1L, floor(nrow(x) / 4L))
  target_n <- min(target_n, max_possible_balanced_n)
  message(
    "[INFO] Validation grouping target per quadrant: requested=",
    requested_target_n,
    ", starting_effective=",
    target_n,
    ", usable_rows_after_outlier_filter=",
    nrow(x)
  )

  # Helper: try to sample target_n based on cutoffs
  try_sample <- function(df, lo_n, hi_n) {
    lo_neur <- quantile(df$neurites, probs = lo_n, na.rm = TRUE)
    hi_neur <- quantile(df$neurites, probs = hi_n, na.rm = TRUE)
    lo_conf <- quantile(df$confluence, probs = lo_n, na.rm = TRUE)
    hi_conf <- quantile(df$confluence, probs = hi_n, na.rm = TRUE)

    groups_raw <- list(
      highNeur_lowConf  = df %>% filter(neurites >= hi_neur, confluence <= lo_conf),
      lowNeur_highConf  = df %>% filter(neurites <= lo_neur, confluence >= hi_conf),
      highNeur_highConf = df %>% filter(neurites >= hi_neur, confluence >= hi_conf),
      lowNeur_lowConf   = df %>% filter(neurites <= lo_neur, confluence <= lo_conf)
    )

    counts <- purrr::map_int(groups_raw, nrow)

    groups <- purrr::imap(groups_raw, ~{
      if (nrow(.x) >= target_n) dplyr::slice_sample(.x, n = target_n) else NULL
    })

    if (all(!purrr::map_lgl(groups, is.null))) {
      list(
        sampled = dplyr::bind_rows(
          groups$highNeur_lowConf  %>% mutate(group = "high neurites / low confluence"),
          groups$lowNeur_highConf  %>% mutate(group = "low neurites / high confluence"),
          groups$highNeur_highConf %>% mutate(group = "high neurites / high confluence"),
          groups$lowNeur_lowConf   %>% mutate(group = "low neurites / low confluence")
        ),
        counts = counts,
        cutoffs = tibble::tibble(
          variable      = c("neurites", "neurites", "confluence", "confluence"),
          side          = c("low", "high", "low", "high"),
          quantile_used = c(lo_n, hi_n, lo_n, hi_n),
          value_used    = c(lo_neur, hi_neur, lo_conf, hi_conf)
        )
      )
    } else {
      NULL
    }
  }

  # 2) Start from hi_start/lo_start and relax until each group has >= target_n.
  # If the requested target is too strict for this dataset, step down the target
  # automatically instead of failing with no output.
  tried <- NULL
  final_target_n <- target_n
  while (is.null(tried) && final_target_n >= 1L) {
    target_n <- final_target_n
    hi <- hi_start; lo <- lo_start
    tried <- try_sample(x, lo, hi)

    while (is.null(tried) && hi > min_hi && lo < max_lo) {
      hi <- max(min_hi, hi - relax_step)
      lo <- min(max_lo, lo + relax_step)
      tried <- try_sample(x, lo, hi)
    }

    if (is.null(tried)) {
      final_target_n <- final_target_n - 1L
    }
  }

  if (is.null(tried)) {
    stop(
      "Could not find all 4 validation quadrants even after reducing target_n to 1. ",
      "This means at least one quadrant is empty under the relaxed cutoffs. ",
      "Check whether neurites_percent and confluence are strongly correlated, or widen min_hi/max_lo further."
    )
  }

  target_n <- final_target_n
  if (target_n < requested_target_n) {
    message(
      "[WARN] Reduced target_n from ",
      requested_target_n,
      " to ",
      target_n,
      " because the current dataset could not supply four balanced groups at the requested size."
    )
  }

  sampled <- tried$sampled %>%
    mutate(.row_id = dplyr::row_number()) %>%
    select(group, image_name, confluence, neurites, everything())

  pre_counts <- tibble::tibble(
    group = c("high neurites / low confluence",
              "low neurites / high confluence",
              "high neurites / high confluence",
              "low neurites / low confluence"),
    candidates_before_sampling = unname(unlist(tried$counts))
  )

  cutoffs_tbl <- tried$cutoffs

  explanation <- paste0(
    "Outliers removed with Tukey's rule (1.5×IQR) independently on confluence and neurites.\n",
    "After outlier removal, 'low' and 'high' thresholds are defined by quantiles of the remaining data.\n",
    "We started at low=", sprintf('%.2f', lo_start),
    " and high=", sprintf('%.2f', hi_start),
    " and symmetrically relaxed (low increased, high decreased) in steps of ",
    relax_step,
    " until each quadrant had at least ", target_n, " candidates.\n",
    if (target_n < requested_target_n) paste0(
      "Requested target_n was ", requested_target_n,
      ", but the script automatically reduced it to ", target_n,
      " because only ", nrow(x),
      " usable rows remained after outlier filtering and four balanced quadrants of the requested size were not available.\n"
    ) else "",
    "Final quantiles used: low=", sprintf('%.2f', lo),
    ", high=", sprintf('%.2f', hi), ".\n",
    "Numeric cutoffs from those quantiles:\n",
    "  ? neurites: ? low (", signif(cutoffs_tbl$value_used[cutoffs_tbl$variable=='neurites' & cutoffs_tbl$side=='low'], 6),
    ") or ? high (", signif(cutoffs_tbl$value_used[cutoffs_tbl$variable=='neurites' & cutoffs_tbl$side=='high'], 6), ")\n",
    "  ? confluence: ? low (", signif(cutoffs_tbl$value_used[cutoffs_tbl$variable=='confluence' & cutoffs_tbl$side=='low'], 6),
    ") or ? high (", signif(cutoffs_tbl$value_used[cutoffs_tbl$variable=='confluence' & cutoffs_tbl$side=='high'], 6), ")\n",
    "Quadrant assignment:\n",
    "  ? high neurites / low confluence: neurites ? neurites_high_cutoff AND confluence ? confluence_low_cutoff\n",
    "  ? low neurites / high confluence: neurites ? neurites_low_cutoff AND confluence ? confluence_high_cutoff\n",
    "  ? high neurites / high confluence: neurites ? neurites_high_cutoff AND confluence ? confluence_high_cutoff\n",
    "  ? low neurites / low confluence: neurites ? neurites_low_cutoff AND confluence ? confluence_low_cutoff\n",
    "From each quadrant, exactly ", target_n, " images were uniformly sampled at random. ",
    "Every selected row includes its image_name for identification."
  )

  list(
    groups = sampled %>% group_split(group) %>% setNames(unique(sampled$group)),
    combined = sampled,
    cutoffs_used = tibble::tibble(low_quantile = lo, high_quantile = hi),
    cutoff_values = cutoffs_tbl,
    candidates_per_quadrant = pre_counts,
    explanation = explanation
  )
}

# ---------------- Run selection ----------------
result <- select_quadrants(
  merged_df,
  confluence_col = "confluence",
  neurites_col   = "neurites_percent",
  image_name_col = "image_name"
)

# ---------------- Plot + TXT helpers ----------------
make_quadrant_report_clean <- function(df, result,
                                       confluence_col = "confluence",
                                       neurites_col   = "neurites_percent") {
  compute_iqr_bounds <- function(v) {
    q1 <- quantile(v, 0.25, na.rm = TRUE)
    q3 <- quantile(v, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lb  <- q1 - 1.5 * iqr
    ub  <- q3 + 1.5 * iqr
    list(q1 = q1, q3 = q3, lb = lb, ub = ub)
  }

  dat <- df %>%
    transmute(
      confluence = .data[[confluence_col]],
      neurites   = .data[[neurites_col]]
    ) %>%
    filter(!is.na(confluence), !is.na(neurites))

  b_neur <- compute_iqr_bounds(dat$neurites)
  b_conf <- compute_iqr_bounds(dat$confluence)

  dat_filt <- dat %>%
    filter(neurites   >= b_neur$lb, neurites   <= b_neur$ub,
           confluence >= b_conf$lb, confluence <= b_conf$ub)

  get_cut <- function(tbl, var, side) {
    tbl %>% filter(variable == var, side == side) %>% pull(value_used) %>% as.numeric()
  }
  neur_low  <- get_cut(result$cutoff_values, "neurites",   "low")
  neur_high <- get_cut(result$cutoff_values, "neurites",   "high")
  conf_low  <- get_cut(result$cutoff_values, "confluence", "low")
  conf_high <- get_cut(result$cutoff_values, "confluence", "high")

  base_theme <- theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(linewidth = 0.2),
          plot.title = element_text(face = "bold", size = 12),
          plot.subtitle = element_blank())

  p_neur <- ggplot(dat_filt, aes(x = neurites)) +
    geom_histogram(bins = 30, alpha = 0.9) +
    geom_vline(xintercept = c(b_neur$lb, b_neur$ub), linetype = "dotted") +
    geom_vline(xintercept = c(b_neur$q1, b_neur$q3), linetype = "dotdash") +
    geom_vline(xintercept = c(neur_low, neur_high), linetype = "dashed") +
    labs(title = "Neurites distribution", x = "Neurites", y = "Count") +
    base_theme

  p_conf <- ggplot(dat_filt, aes(x = confluence)) +
    geom_histogram(bins = 30, alpha = 0.9) +
    geom_vline(xintercept = c(b_conf$lb, b_conf$ub), linetype = "dotted") +
    geom_vline(xintercept = c(b_conf$q1, b_conf$q3), linetype = "dotdash") +
    geom_vline(xintercept = c(conf_low, conf_high), linetype = "dashed") +
    labs(title = "Confluence distribution", x = "Confluence", y = "Count") +
    base_theme

  p_scatter <- ggplot(result$combined,
                      aes(x = confluence, y = neurites, color = group)) +
    geom_point(size = 2.6, alpha = 0.9) +
    geom_hline(yintercept = c(neur_low, neur_high), linetype = "dashed") +
    geom_vline(xintercept = c(conf_low, conf_high), linetype = "dashed") +
    labs(title = "Quadrant selection", x = "Confluence", y = "Neurites", color = NULL) +
    base_theme +
    theme(legend.position = "bottom")

  (p_neur | p_conf) / p_scatter
}

# Avoid self-referencing default argument causing "promise already under evaluation"
save_report_and_text <- function(df, result,
                                 confluence_col = "confluence",
                                 neurites_col   = "neurites_percent",
                                 out_dir_path   = NULL,
                                 jpg_name       = "quadrant_selection_report.jpg",
                                 txt_name       = "cutoff_groups_choice_explained.txt") {

  if (is.null(out_dir_path)) out_dir_path <- out_dir
  dir.create(out_dir_path, showWarnings = FALSE, recursive = TRUE)

  report_plot <- make_quadrant_report_clean(df, result, confluence_col, neurites_col)

  jpg_path <- file.path(out_dir_path, jpg_name)
  ggsave(filename = jpg_path,
         plot = report_plot,
         width = 12, height = 8, units = "in",
         dpi = 300, device = "jpeg")

  q_lo <- round(result$cutoffs_used$low_quantile[1], 3)
  q_hi <- round(result$cutoffs_used$high_quantile[1], 3)

  as_text <- function(x) paste(capture.output(print(x)), collapse = "\n")

  by_group <- result$combined %>%
    arrange(group, image_name) %>%
    group_by(group) %>%
    summarise(n = dplyr::n(),
              images = paste(image_name, collapse = ", "),
              .groups = "drop")

  text_lines <- c(
    "CUTOFF GROUPS CHOICE ? EXPLANATION",
    "----------------------------------",
    result$explanation, "",
    paste0("Quantile levels used: low=", q_lo, "  high=", q_hi), "",
    "Numeric cutoffs (by variable & side):",
    as_text(result$cutoff_values), "",
    "Candidates per quadrant (before sampling):",
    as_text(result$candidates_per_quadrant), "",
    "Selected images per group (n and names):",
    as_text(by_group), ""
  )

  txt_path <- file.path(out_dir_path, txt_name)
  writeLines(text_lines, txt_path)

  list(jpg = jpg_path, txt = txt_path)
}

# ---------------- Save plot + text ----------------
paths <- save_report_and_text(merged_df, result,
                              confluence_col = "confluence",
                              neurites_col   = "neurites_percent",
                              out_dir_path   = out_dir)
message("[INFO] Saved JPG: ", paths$jpg)
message("[INFO] Saved TXT: ", paths$txt)

# ---------------- Add 4 indicator columns and save CSV ----------------
norm_col <- function(lbl) {
  lbl %>%
    gsub(" / ", "_", ., fixed = TRUE) %>%
    gsub(" ", "_", .) %>%
    gsub("/", "_", .) %>%
    tolower() %>%
    paste0("grp_", .)
}

group_labels <- c("high neurites / low confluence",
                  "low neurites / high confluence",
                  "high neurites / high confluence",
                  "low neurites / low confluence")

indicator_cols <- setNames(vapply(group_labels, norm_col, character(1)),
                           group_labels)

sel_by_group <- lapply(group_labels, function(g) {
  result$combined %>%
    filter(group == g) %>%
    pull(image_name) %>%
    unique()
})
names(sel_by_group) <- group_labels

merged_df_out <- merged_df
for (g in group_labels) {
  colname <- indicator_cols[[g]]
  imgs <- sel_by_group[[g]]
  merged_df_out[[colname]] <- ifelse(merged_df_out$image_name %in% imgs, 1L, 0L)
}

csv_out_path <- file.path(out_dir, "mask_area_percentages+optim_groups.csv")
write_csv(merged_df_out, csv_out_path, na = "")
message("[INFO] Saved CSV with indicators: ", csv_out_path)

message("[INFO] All done.")
