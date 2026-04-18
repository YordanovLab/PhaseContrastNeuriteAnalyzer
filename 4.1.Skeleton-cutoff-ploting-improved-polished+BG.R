#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(tools)
  library(multcomp)
  library(emmeans)
  library(broom)
})

# ============================================================
# SETTINGS
# ============================================================
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)
source(file.path(base_dir, "launchers", "pipeline_paths.R"))
ctx <- pipeline_context(base_dir)

filtered_rds <- ctx$paths$generalized_rds
metadata_path <- ctx$paths$optim_groups_inspected
output_dir <- ctx$paths$final_dir

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n=== ENHANCED BIOLOGICAL FACTOR ANALYSIS ===\n")
cat("Focus: Treatment effect (rosmarinic acid vs retinoic acid vs control)\n")
cat("Controlling for: time, concentration, confluence (excluding PMA)\n\n")

# ============================================================
# LOAD AND PREPARE DATA
# ============================================================
cat("Loading data...\n")
skeleton_data <- readRDS(filtered_rds)

# Clean up columns
colnames(skeleton_data) <- gsub("\\.x$", "", colnames(skeleton_data))
colnames(skeleton_data) <- gsub("\\.y$", "", colnames(skeleton_data))
skeleton_data <- skeleton_data[, !duplicated(colnames(skeleton_data))]
colnames(skeleton_data) <- gsub("\\.", "_", colnames(skeleton_data))

metadata <- read_csv(metadata_path, show_col_types = FALSE)
metadata$image_key <- tools::file_path_sans_ext(metadata$image_name)

# ============================================================
# AGGREGATE TO IMAGE LEVEL
# ============================================================
cat("Aggregating to image level...\n\n")

image_features <- skeleton_data %>%
  filter(predicted_signal == TRUE) %>%
  group_by(image_key) %>%
  summarise(
    mean_max_branch_length = mean(Maximum_Branch_Length, na.rm = TRUE),
    median_max_branch_length = median(Maximum_Branch_Length, na.rm = TRUE),
    mean_avg_branch_length = mean(Average_Branch_Length, na.rm = TRUE),
    n_skeletons = n(),
    .groups = "drop"
  ) %>%
  left_join(
    metadata %>% dplyr::select(
      image_key, 
      first_differentiating_agent,
      `first_agent_concentration(micromolar)`,
      phorbol_ester,
      intended_assay,
      neurites_percent,
      days_after_seeding,
      any_of(c("FBS_percentage", "cell_counts_at_seeding__x10_5_", 
               "confluence", "confluence_percent", "confluence_percentage"))
    ),
    by = "image_key"
  ) %>%
  filter(!is.na(first_differentiating_agent),
         !is.na(`first_agent_concentration(micromolar)`),
         !is.na(intended_assay))

# Create clean factors and bins
image_features <- image_features %>%
  mutate(
    treatment = factor(ifelse(first_differentiating_agent == "None", "CTRL", first_differentiating_agent)),
    concentration = as.numeric(`first_agent_concentration(micromolar)`),
    concentration_cat = factor(`first_agent_concentration(micromolar)`),
    phorbol = factor(ifelse(phorbol_ester, "PMA+", "PMA-")),
    assay = factor(intended_assay),
    neurite_pct = neurites_percent,
    time_numeric = as.numeric(days_after_seeding)
  )

# Handle confluence column
available_cols <- colnames(image_features)
if ("confluence" %in% available_cols) {
  confluence_col <- "confluence"
} else if (any(grepl("confluence", available_cols, ignore.case = TRUE))) {
  confluence_col <- grep("confluence", available_cols, ignore.case = TRUE, value = TRUE)[1]
} else {
  confluence_col <- NULL
}

if (!is.null(confluence_col)) {
  image_features <- image_features %>%
    mutate(
      confluence_bin = cut(.data[[confluence_col]], 
                           breaks = c(-Inf, 25, 50, 75, Inf),
                           labels = c("Low (<25%)", "Medium (25-50%)", 
                                      "High (50-75%)", "Very High (>75%)"))
    )
  cat("Found confluence data in column:", confluence_col, "\n")
} else {
  image_features$confluence_bin <- NA
  cat("Warning: No confluence column found.\n")
}

cat("Total images:", nrow(image_features), "\n")
cat("Treatments:", paste(unique(image_features$treatment), collapse = ", "), "\n\n")

# ============================================================
# PRIMARY STATISTICAL ANALYSIS (EXCLUDING PMA)
# ============================================================
cat("=== PRIMARY ANALYSIS ===\n")
cat("ANCOVA: neurite skeleton length ~ treatment + time + concentration + confluence\n")
cat("Testing treatment effect (rosmarinic acid vs retinoic acid vs control)\n")
cat("Controlling for time, concentration, and confluence (excluding PMA)\n\n")

analysis_data <- image_features %>%
  filter(!is.na(time_numeric), 
         !is.na(concentration),
         !is.na(confluence_bin)) %>%
  mutate(
    time_numeric = scale(time_numeric)[, 1],
    concentration = scale(concentration)[, 1]
  )

cat("Sample size for primary analysis:", nrow(analysis_data), "images\n\n")

if (nrow(analysis_data) > 20) {
  primary_model <- aov(mean_max_branch_length ~ treatment + time_numeric + concentration + confluence_bin,
                       data = analysis_data)
  
  cat("ANCOVA Results:\n")
  print(summary(primary_model))
  cat("\n")
  
  anova_table <- broom::tidy(primary_model)
  treatment_effect <- anova_table %>% filter(term == "treatment")
  
  cat("Treatment Effect (controlling for time, concentration, confluence):\n")
  cat("F =", round(treatment_effect$statistic, 4), 
      ", p =", round(treatment_effect$p.value, 4), "\n\n")
  
  emm_treatment <- emmeans(primary_model, ~ treatment)
  cat("Estimated Marginal Means by Treatment (adjusted for covariates):\n")
  print(as.data.frame(emm_treatment))
  cat("\n")
  
  cat("Post-hoc Tukey HSD Comparisons:\n")
  tukey_result <- multcomp::cld(emm_treatment, Letters = letters, adjust = "tukey")
  print(as.data.frame(tukey_result))
  cat("\n")
  
  write_csv(as.data.frame(anova_table), 
            file.path(output_dir, "primary_ancova_results.csv"))
  write_csv(as.data.frame(tukey_result), 
            file.path(output_dir, "treatment_posthoc_tukey.csv"))
}

# ============================================================
# PLOTS (larger readable labels + angled x-axis)
# ============================================================
cat("Creating plots with larger labels and angled x-axis text...\n")

# Theme baseline
theme_big <- theme_minimal(base_size = 20) +
  theme(
    axis.text.x = element_text(size = 18, angle = 45, hjust = 1, color = "black"),
    axis.text.y = element_text(size = 18, color = "black"),
    axis.title = element_text(size = 22, face = "bold"),
    strip.text = element_text(size = 22, face = "bold"),
    plot.title = element_text(size = 26, face = "bold"),
    plot.subtitle = element_text(size = 20),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 20, face = "bold"),
    legend.position = "bottom"
  )

# 1. Treatment × Confluence
plot_data_confluence <- image_features %>%
  filter(!is.na(confluence_bin)) %>%
  group_by(treatment, confluence_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 3)

if (nrow(plot_data_confluence) > 0) {
  p1 <- ggplot(image_features %>% 
                 filter(!is.na(confluence_bin),
                        treatment %in% unique(plot_data_confluence$treatment)),
               aes(x = treatment, y = mean_max_branch_length, fill = treatment)) +
    geom_boxplot(alpha = 0.7, width = 0.6, outlier.shape = NA) +
    geom_point(alpha = 0.3, position = position_jitter(width = 0.2), size = 3) +
    facet_wrap(~confluence_bin, nrow = 1) +
    labs(
      title = "Neurite Skeleton Length by Treatment and Initial Confluence",
      subtitle = "ANCOVA model controls for time and concentration",
      x = "Treatment",
      y = "Mean Max Branch Length (pixels)",
      fill = "Treatment"
    ) +
    theme_big
  
  ggsave(file.path(output_dir, "01_treatment_by_confluence.png"),
         p1, width = 18, height = 7, dpi = 300)
}

# 2. Treatment × Concentration
plot_data_conc <- image_features %>%
  filter(!is.na(concentration)) %>%
  group_by(treatment, concentration_cat) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 3)

if (nrow(plot_data_conc) > 0) {
  p2 <- ggplot(image_features %>% 
                 filter(!is.na(concentration_cat),
                        treatment %in% unique(plot_data_conc$treatment)),
               aes(x = treatment, y = mean_max_branch_length, fill = treatment)) +
    geom_boxplot(alpha = 0.7, width = 0.6, outlier.shape = NA) +
    geom_point(alpha = 0.3, position = position_jitter(width = 0.2), size = 3) +
    facet_wrap(~concentration_cat, nrow = 1) +
    labs(
      title = "Neurite Skeleton Length by Treatment and Concentration",
      subtitle = "ANCOVA model controls for time and confluence",
      x = "Treatment",
      y = "Mean Max Branch Length (pixels)",
      fill = "Treatment"
    ) +
    theme_big
  
  ggsave(file.path(output_dir, "02_treatment_by_concentration.png"),
         p2, width = 18, height = 7, dpi = 300)
}

# 3. Treatment × Time
plot_data_time <- image_features %>%
  filter(!is.na(time_numeric)) %>%
  mutate(time_bin = factor(time_numeric)) %>%
  group_by(treatment, time_bin) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 3)

if (nrow(plot_data_time) > 0) {
  p3 <- ggplot(image_features %>% 
                 filter(!is.na(time_numeric),
                        treatment %in% unique(plot_data_time$treatment)) %>%
                 mutate(time_label = paste0(time_numeric, " days")),
               aes(x = treatment, y = mean_max_branch_length, fill = treatment)) +
    geom_boxplot(alpha = 0.7, width = 0.6, outlier.shape = NA) +
    geom_point(alpha = 0.3, position = position_jitter(width = 0.2), size = 3) +
    facet_wrap(~time_label, nrow = 1) +
    labs(
      title = "Neurite Skeleton Length by Treatment and Time",
      subtitle = "ANCOVA model controls for concentration and confluence",
      x = "Treatment",
      y = "Mean Max Branch Length (pixels)",
      fill = "Treatment"
    ) +
    theme_big
  
  ggsave(file.path(output_dir, "03_treatment_by_time.png"),
         p3, width = 18, height = 7, dpi = 300)
}

# 4. Filtered subset — 2 days, medium confluence
cat("Creating filtered plot for time=2 days, medium confluence...\n")

filtered_subset <- image_features %>%
  filter(
    (round(time_numeric, 1) == 2 | days_after_seeding == 2),
    confluence_bin == "Medium (25-50%)"
  )

if (nrow(filtered_subset) >= 3) {
  p5 <- ggplot(filtered_subset,
               aes(x = treatment, y = mean_max_branch_length, fill = treatment)) +
    geom_boxplot(alpha = 0.7, width = 0.6, outlier.shape = NA) +
    geom_point(alpha = 0.4, position = position_jitter(width = 0.2), size = 3) +
    labs(
      title = "Neurite Skeleton Length (2 Days, Medium Confluence)",
      subtitle = "Filtered subset: 2-day cultures, medium confluence",
      x = "Treatment",
      y = "Mean Max Branch Length (pixels)",
      fill = "Treatment"
    ) +
    theme_big
  
  ggsave(file.path(output_dir, "05_treatment_filtered_time2_mediumConfluence.png"),
         p5, width = 14, height = 7, dpi = 300)
}

# 5. 6-day medium confluence PMA comparison
cat("Creating comparison plot for medium confluence after 6 days, PMA+ vs PMA-...\n")

filtered_subset_6d <- image_features %>%
  filter(
    (round(time_numeric, 1) == 6 | days_after_seeding == 6),
    confluence_bin == "Medium (25-50%)",
    !is.na(phorbol)
  )

if (nrow(filtered_subset_6d) >= 3) {
  p6 <- ggplot(filtered_subset_6d,
               aes(x = phorbol, y = mean_max_branch_length, fill = phorbol)) +
    geom_boxplot(alpha = 0.7, width = 0.6, outlier.shape = NA) +
    geom_point(alpha = 0.4, position = position_jitter(width = 0.2), size = 3) +
    facet_wrap(~treatment, nrow = 1) +
    labs(
      title = "Neurite Skeleton Length After 6 Days (Medium Confluence)",
      subtitle = "Comparison of PMA+ vs PMA− across treatments",
      x = "PMA Treatment",
      y = "Mean Max Branch Length (pixels)",
      fill = "PMA"
    ) +
    scale_fill_manual(values = c("PMA-" = "#999999", "PMA+" = "#E69F00"),
                      labels = c("Without PMA", "With PMA")) +
    theme_big
  
  ggsave(file.path(output_dir, "06_mediumConfluence_day6_PMA_comparison.png"),
         p6, width = 16, height = 7, dpi = 300)
}

# ============================================================
# SUMMARY REPORT
# ============================================================
cat("\n=== ANALYSIS COMPLETE ===\n\n")
cat("Statistical Outputs:\n")
cat("  - primary_ancova_results.csv\n")
cat("  - treatment_posthoc_tukey.csv\n\n")
cat("Plots generated:\n")
cat("  - 01_treatment_by_confluence.png\n")
cat("  - 02_treatment_by_concentration.png\n")
cat("  - 03_treatment_by_time.png\n")
cat("  - 05_treatment_filtered_time2_mediumConfluence.png\n")
cat("  - 06_mediumConfluence_day6_PMA_comparison.png\n\n")
cat("Output directory:", normalizePath(output_dir), "\n\n")

# BULGARIAN-LABELED PLOTS
# ============================================================

# ============================================================
# BULGARIAN-LABELED PLOTS (СТИЛ „ТРЕТИРАНЕ × ...“)
# ============================================================

# 1. Третиране × кл. плътност
p1_bg <- p1 +
  labs(
    title = "Дължина на невритите спрямо третирането и началната кл. плътност",
    subtitle = "",
    x = "Третиране",
    y = "Средна дължина на максимален\nневритен участък (px)",
    fill = "Третиране"
  )
ggsave(file.path(output_dir, "01_treatment_by_confluence_BG.png"), p1_bg, width = 18, height = 7)

# 2. Третиране × Концентрация
p2_bg <- p2 +
  labs(
    title = "Дължина на невритите спрямо третирането и концентрацията",
    subtitle = "",
    x = "Третиране",
    y = "Средна дължина на максимален\nневритен участък (px)",
    fill = "Третиране"
  )
ggsave(file.path(output_dir, "02_treatment_by_concentration_BG.png"), p2_bg, width = 18, height = 7)

# 3. Третиране × Време
p3_bg <- p3 +
  labs(
    title = "Дължина на невритите спрямо третирането и времето",
    subtitle = "",
    x = "Третиране",
    y = "Средна дължина на максимален\nневритен участък (px)",
    fill = "Третиране"
  )
ggsave(file.path(output_dir, "03_treatment_by_time_BG.png"), p3_bg, width = 18, height = 7)

# 3b. Третиране × Време (само балансирани времеви точки)
p3b_bg <- p3b +
  labs(
    title = "Дължина на невритите спрямо третирането и времето (само балансирани времеви точки)",
    subtitle = "",
    x = "Третиране",
    y = "Средна дължина на максимален\nневритен участък (px)",
    fill = "Третиране"
  )
ggsave(file.path(output_dir, "03b_treatment_by_time_balanced_BG.png"), p3b_bg, width = 18, height = 7)

# 4. 2 дни, средна кл. плътност
p5_bg <- p5 +
  labs(
    title = "Дължина на невритите (2 дни, средна кл. плътност)",
    subtitle = "",
    x = "Третиране",
    y = "Средна дължина на максимален\nневритен участък (px)",
    fill = "Третиране"
  )
ggsave(file.path(output_dir, "05_treatment_filtered_time2_mediumConfluence_BG.png"), p5_bg, width = 14, height = 7)

# 5. 6 дни, средна кл. плътност, сравнение PMA
p6_bg <- p6 +
  labs(
    title = "Дължина на невритите след 6 дни (средна кл. плътност)",
    subtitle = "",
    x = "Следтретиране с PMA",
    y = "Средна дължина на максимален\nневритен участък (px)",
    fill = "PMA"
  )
ggsave(file.path(output_dir, "06_mediumConfluence_day6_PMA_comparison_BG.png"), p6_bg, width = 16, height = 7)

cat("\n=== ВСИЧКИ БЪЛГАРСКИ ВАРИАНТИ НА ГРАФИКИТЕ СА ЗАПИСАНИ ===\n")
cat("Проверете файловете *_BG.png в директорията:", normalizePath(output_dir), "\n\n")


cat("\n=== ALL BULGARIAN PLOTS SAVED ===\n")
cat("Check files ending with _BG.png in:", normalizePath(output_dir), "\n\n")
