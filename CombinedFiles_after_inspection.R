#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
  library(patchwork)
  library(tibble)
})

# ---------------- Args & Paths ----------------
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
source(file.path(base_dir, "launchers", "pipeline_paths.R"))
ctx <- pipeline_context(base_dir)
out_dir  <- ctx$paths$out_opt
csv_path <- file.path(out_dir, "mask_area_percentages+optim_groups.csv")
manifest_path <- file.path(out_dir, "manual_review_labels.csv")

if (!dir.exists(out_dir)) {
  stop("[ERROR] OUT_optimize not found at: ", out_dir)
}
if (!file.exists(csv_path)) {
  stop("[ERROR] CSV not found at: ", csv_path)
}

message("[INFO] Base dir: ", base_dir)
message("[INFO] OUT dir:  ", out_dir)
message("[INFO] CSV in:   ", csv_path)

# ---------------- Read CSV ----------------
df <- suppressMessages(read_csv(csv_path, show_col_types = FALSE))
required_cols <- c("image_name", "confluence", "neurites_percent",
                   "grp_high_neurites_low_confluence",
                   "grp_low_neurites_high_confluence",
                   "grp_high_neurites_high_confluence",
                   "grp_low_neurites_low_confluence")
if (!all(required_cols %in% names(df))) {
  stop("[ERROR] CSV must contain columns: ", paste(required_cols, collapse=", "))
}

# ---------------- Find manual labels from manifest or folder structure ----------------
subgroups <- c("high_neurites_low_confluence",
               "low_neurites_high_confluence",
               "high_neurites_high_confluence",
               "low_neurites_low_confluence")

list_images_in <- function(parent, subgroup, label) {
  legacy_map <- c(
    high_neurites_low_confluence = "high neurites - low confluence",
    low_neurites_high_confluence = "low neurites - high confluence",
    high_neurites_high_confluence = "high neurites - high confluence",
    low_neurites_low_confluence = "low neurites - low confluence"
  )

  candidates <- c(
    file.path(parent, subgroup, label),
    file.path(parent, legacy_map[[subgroup]], label)
  )

  files <- unlist(lapply(candidates, function(d) {
    if (!dir.exists(d)) return(character())
    list.files(d, full.names = TRUE, recursive = FALSE, include.dirs = FALSE)
  }))

  if (length(files) == 0) return(character())
  unique(basename(files))
}

if (file.exists(manifest_path)) {
  manifest <- suppressMessages(read_csv(manifest_path, show_col_types = FALSE))
  choice_imgs_by_group <- lapply(subgroups, function(sg) unique(manifest$image_name[manifest$group_id == sg & manifest$manual_label == "choice"]))
  names(choice_imgs_by_group) <- subgroups
  noise_imgs_by_group  <- lapply(subgroups, function(sg) unique(manifest$image_name[manifest$group_id == sg & manifest$manual_label == "noise"]))
  names(noise_imgs_by_group) <- subgroups
  not_sure_imgs_by_group <- lapply(subgroups, function(sg) unique(manifest$image_name[manifest$group_id == sg & manifest$manual_label == "not_sure"]))
  names(not_sure_imgs_by_group) <- subgroups
} else {
  choice_imgs_by_group <- lapply(subgroups, function(sg) list_images_in(out_dir, sg, "choice"))
  names(choice_imgs_by_group) <- subgroups
  noise_imgs_by_group  <- lapply(subgroups, function(sg) list_images_in(out_dir, sg, "noise"))
  names(noise_imgs_by_group) <- subgroups
  not_sure_imgs_by_group <- lapply(subgroups, function(sg) list_images_in(out_dir, sg, "not_sure"))
  names(not_sure_imgs_by_group) <- subgroups
}

choice_imgs <- unique(unlist(choice_imgs_by_group))
noise_imgs  <- unique(unlist(noise_imgs_by_group))
not_sure_imgs <- unique(unlist(not_sure_imgs_by_group))

message("[INFO] Found ", length(choice_imgs), " files in */choice, ",
        length(noise_imgs), " in */noise, ",
        length(not_sure_imgs), " in */not_sure.")

# ---------------- Add manual columns & save CSVs ----------------
df <- df %>%
  mutate(
    `_manual_choise` = as.integer(image_name %in% choice_imgs),  # (requested spelling)
    manual_noise     = as.integer(image_name %in% noise_imgs)
  )

# Overwrite original and also save a +inspected copy
write_csv(df, csv_path, na = "")
csv_path_inspected <- file.path(out_dir, "mask_area_percentages+optim_groups+inspected.csv")
write_csv(df, csv_path_inspected, na = "")
message("[INFO] Updated CSV saved (and +inspected copy):")
message("       ", csv_path)
message("       ", csv_path_inspected)

# ---------------- Recompute cutoffs (same approach as original report) ----------------
compute_iqr_bounds <- function(v) {
  q1 <- quantile(v, 0.25, na.rm = TRUE)
  q3 <- quantile(v, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lb  <- q1 - 1.5 * iqr
  ub  <- q3 + 1.5 * iqr
  list(q1 = q1, q3 = q3, lb = lb, ub = ub)
}

derive_cutoffs <- function(dat,
                           target_n = 10,
                           hi_start = 0.80, lo_start = 0.20,
                           relax_step = 0.02,
                           min_hi = 0.55, max_lo = 0.45) {
  b_neur <- compute_iqr_bounds(dat$neurites_percent)
  b_conf <- compute_iqr_bounds(dat$confluence)
  dat_f  <- dat %>%
    filter(neurites_percent >= b_neur$lb, neurites_percent <= b_neur$ub,
           confluence      >= b_conf$lb, confluence      <= b_conf$ub)

  enough <- function(lo, hi) {
    lo_neur <- quantile(dat_f$neurites_percent, probs = lo, na.rm = TRUE)
    hi_neur <- quantile(dat_f$neurites_percent, probs = hi, na.rm = TRUE)
    lo_conf <- quantile(dat_f$confluence,      probs = lo, na.rm = TRUE)
    hi_conf <- quantile(dat_f$confluence,      probs = hi, na.rm = TRUE)
    counts <- c(
      sum(dat_f$neurites_percent >= hi_neur & dat_f$confluence <= lo_conf),
      sum(dat_f$neurites_percent <= lo_neur & dat_f$confluence >= hi_conf),
      sum(dat_f$neurites_percent >= hi_neur & dat_f$confluence >= hi_conf),
      sum(dat_f$neurites_percent <= lo_neur & dat_f$confluence <= lo_conf)
    )
    all(counts >= target_n)  # scalar TRUE/FALSE
  }

  hi <- hi_start; lo <- lo_start
  if (nrow(dat_f) == 0) {
    return(list(
      b_neur=b_neur, b_conf=b_conf, lo=lo, hi=hi,
      neur_low=NA_real_, neur_high=NA_real_,
      conf_low=NA_real_, conf_high=NA_real_,
      dat_f=dat_f
    ))
  }

  while (!enough(lo, hi) && hi > min_hi && lo < max_lo) {
    hi <- max(min_hi, hi - relax_step)
    lo <- min(max_lo, lo + relax_step)
  }

  list(
    b_neur = b_neur,
    b_conf = b_conf,
    lo = lo,
    hi = hi,
    neur_low  = quantile(dat_f$neurites_percent, probs = lo, na.rm = TRUE),
    neur_high = quantile(dat_f$neurites_percent, probs = hi, na.rm = TRUE),
    conf_low  = quantile(dat_f$confluence,      probs = lo, na.rm = TRUE),
    conf_high = quantile(dat_f$confluence,      probs = hi, na.rm = TRUE),
    dat_f = dat_f
  )
}

cuts <- derive_cutoffs(df)

# ---------------- Rebuild "selected" points & plot like original ----------------
selected_df <- df %>%
  transmute(
    image_name, confluence, neurites_percent,
    group = dplyr::case_when(
      grp_high_neurites_low_confluence  == 1L ~ "high neurites / low confluence",
      grp_low_neurites_high_confluence  == 1L ~ "low neurites / high confluence",
      grp_high_neurites_high_confluence == 1L ~ "high neurites / high confluence",
      grp_low_neurites_low_confluence   == 1L ~ "low neurites / low confluence",
      TRUE ~ NA_character_
    )
  ) %>% filter(!is.na(group))

d_choice_sel <- selected_df %>%
  semi_join(df %>% filter(`_manual_choise` == 1L), by = "image_name")
d_noise_sel  <- selected_df %>%
  semi_join(df %>% filter(manual_noise == 1L), by = "image_name")

base_theme <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_blank())

p_neur <- ggplot(cuts$dat_f, aes(x = neurites_percent)) +
  geom_histogram(bins = 30, alpha = 0.9) +
  geom_vline(xintercept = c(cuts$b_neur$lb, cuts$b_neur$ub), linetype = "dotted") +
  geom_vline(xintercept = c(cuts$b_neur$q1, cuts$b_neur$q3), linetype = "dotdash") +
  geom_vline(xintercept = c(cuts$neur_low, cuts$neur_high),  linetype = "dashed") +
  labs(title = "Neurites distribution", x = "Neurites", y = "Count") +
  base_theme

p_conf <- ggplot(cuts$dat_f, aes(x = confluence)) +
  geom_histogram(bins = 30, alpha = 0.9) +
  geom_vline(xintercept = c(cuts$b_conf$lb, cuts$b_conf$ub), linetype = "dotted") +
  geom_vline(xintercept = c(cuts$b_conf$q1, cuts$b_conf$q3), linetype = "dotdash") +
  geom_vline(xintercept = c(cuts$conf_low, cuts$conf_high),  linetype = "dashed") +
  labs(title = "Confluence distribution", x = "Confluence", y = "Count") +
  base_theme

# Scatter of selected points, colored by group
p_scatter <- ggplot(selected_df, aes(x = confluence, y = neurites_percent, color = group)) +
  geom_point(size = 2.8, alpha = 0.95) +
  geom_hline(yintercept = c(cuts$neur_low, cuts$neur_high), linetype = "dashed") +
  geom_vline(xintercept = c(cuts$conf_low, cuts$conf_high), linetype = "dashed") +
  labs(title = "Quadrant selection (manual inspection)",
       x = "Confluence", y = "Neurites", color = "Group") +
  base_theme +
  theme(legend.position = "bottom")

# ---------------- Overlays: ROUND outlines only ----------------
# Choice -> GREEN circle outline
if (nrow(d_choice_sel) > 0) {
  p_scatter <- p_scatter +
    geom_point(
      data = d_choice_sel,
      aes(x = confluence, y = neurites_percent),
      inherit.aes = FALSE,
      shape = 21, fill = NA, color = "green4", stroke = 2.0, size = 4.6
    )
}
# Noise -> RED circle outline
if (nrow(d_noise_sel) > 0) {
  p_scatter <- p_scatter +
    geom_point(
      data = d_noise_sel,
      aes(x = confluence, y = neurites_percent),
      inherit.aes = FALSE,
      shape = 21, fill = NA, color = "red3", stroke = 2.0, size = 4.6
    )
}

report_plot <- (p_neur | p_conf) / p_scatter

# ---------------- Save plot ----------------
plot_out <- file.path(out_dir, "mask_area_percentages+optim_groups+inspected.jpg")
ggsave(filename = plot_out, plot = report_plot,
       width = 12, height = 8, units = "in", dpi = 300, device = "jpeg")

message("[INFO] Saved inspected plot: ", plot_out)

# ---------------- Subgroup report ----------------
sg_report <- tibble(
  subgroup = subgroups,
  n_choice = vapply(subgroups, function(sg) length(choice_imgs_by_group[[sg]]), integer(1)),
  n_noise  = vapply(subgroups, function(sg) length(noise_imgs_by_group[[sg]]), integer(1)),
  n_not_sure = vapply(subgroups, function(sg) length(not_sure_imgs_by_group[[sg]]), integer(1))
)

message("[REPORT] Manual inspection counts by subgroup:")
apply(sg_report, 1, function(r) {
  message(sprintf("  %-30s  choice: %3d   noise: %3d   not_sure: %3d",
                  r[["subgroup"]], as.integer(r[["n_choice"]]), as.integer(r[["n_noise"]]), as.integer(r[["n_not_sure"]])))
  NULL
})

message("[INFO] Done.")
