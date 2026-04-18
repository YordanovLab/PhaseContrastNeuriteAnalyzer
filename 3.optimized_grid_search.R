# Optimized Large-Scale Grid Search (up to 50,000 combinations)
# Run this AFTER the distribution analysis script

library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(viridis)

# Load data if needed
if (!exists("roi_analysis")) {
  roi_analysis <- read_csv("OUT_optimize/roi_analysis_raw.csv")
}
param_ranges <- readRDS("OUT_optimize/parameter_ranges.rds")
param_importance <- read_csv("OUT_optimize/parameter_importance.csv")

cat("=== CREATING OPTIMIZED PARAMETER GRID ===\n")

# Function to create smart sequence
create_smart_sequence <- function(values, n_points = 10, use_log = FALSE) {
  if (use_log) {
    log_vals <- log10(values)
    log_seq <- seq(min(log_vals), max(log_vals), length.out = n_points)
    return(unique(round(10^log_seq, 3)))
  } else {
    return(unique(round(seq(min(values), max(values), length.out = n_points), 3)))
  }
}

# Create parameter grid based on importance and distributions
# Top 4-5 most important parameters with fine granularity

# Area - vary MINIMUM only, maximum is always Inf
area_data <- roi_analysis$area[is.finite(roi_analysis$area) & roi_analysis$area > 0]
area_percentiles <- quantile(log10(area_data), probs = seq(0, 0.95, by = 0.05), na.rm = TRUE)
min_area_vals <- unique(round(10^area_percentiles[seq(1, length(area_percentiles), by = 2)], 0))
max_area_val <- Inf  # Fixed maximum

# Circularity - minimum is always 0, vary MAXIMUM only
min_circ_val <- 0  # Fixed minimum
max_circ_vals <- seq(0.1, 1.0, by = 0.1)

# Aspect ratio (use absolute log)
aspect_data <- roi_analysis$aspect_ratio[is.finite(roi_analysis$aspect_ratio) & roi_analysis$aspect_ratio > 0]
log_aspect <- abs(log10(aspect_data))
aspect_percentiles <- quantile(log_aspect, probs = seq(0, 0.95, by = 0.05), na.rm = TRUE)
min_aspect_vals <- unique(round(10^aspect_percentiles[seq(1, length(aspect_percentiles), by = 3)], 2))
max_aspect_vals <- c(quantile(aspect_data, c(0.90, 0.95, 0.99), na.rm = TRUE), Inf)

# Skeleton length
skel_data <- roi_analysis$skeleton_length[is.finite(roi_analysis$skeleton_length) & roi_analysis$skeleton_length > 0]
skel_percentiles <- quantile(log10(skel_data), probs = seq(0, 0.95, by = 0.05), na.rm = TRUE)
min_skel_vals <- unique(round(10^skel_percentiles[seq(1, length(skel_percentiles), by = 2)], 0))
max_skel_vals <- c(quantile(skel_data, c(0.75, 0.90, 0.95, 0.99), na.rm = TRUE), Inf)

# Mean width
width_data <- roi_analysis$mean_width[is.finite(roi_analysis$mean_width) & roi_analysis$mean_width > 0]
width_percentiles <- quantile(log10(width_data), probs = seq(0, 0.95, by = 0.1), na.rm = TRUE)
min_width_vals <- unique(round(10^width_percentiles, 1))
max_width_vals <- c(quantile(width_data, c(0.90, 0.95), na.rm = TRUE), Inf)

# Perimeter
perim_data <- roi_analysis$perimeter[is.finite(roi_analysis$perimeter) & roi_analysis$perimeter > 0]
perim_percentiles <- quantile(log10(perim_data), probs = seq(0, 0.95, by = 0.1), na.rm = TRUE)
min_perim_vals <- unique(round(10^perim_percentiles, 0))
max_perim_vals <- c(quantile(perim_data, c(0.90, 0.95), na.rm = TRUE), Inf)

cat("Parameter ranges:\n")
cat("  Area min:", length(min_area_vals), "values (max = Inf)\n")
cat("  Circularity max:", length(max_circ_vals), "values (min = 0)\n")
cat("  Aspect ratio min:", length(min_aspect_vals), "values\n")
cat("  Skeleton min:", length(min_skel_vals), "values\n")
cat("  Width min:", length(min_width_vals), "values\n")

# Create comprehensive grid (balance between parameters)
# Aim for ~50,000 combinations
param_grid <- expand.grid(
  min_area = min_area_vals[seq(1, length(min_area_vals), by = 2)],
  max_circularity = max_circ_vals,
  min_aspect_ratio = min_aspect_vals,
  min_skeleton_length = min_skel_vals[seq(1, length(min_skel_vals), by = 2)],
  min_mean_width = min_width_vals,
  stringsAsFactors = FALSE
)

cat("\nInitial grid size:", nrow(param_grid), "combinations\n")

# Sample if too large
if (nrow(param_grid) > 50000) {
  set.seed(42)
  param_grid <- param_grid[sample(1:nrow(param_grid), 50000), ]
  cat("Sampled to:", nrow(param_grid), "combinations\n")
}

# ===== EVALUATION FUNCTION =====
evaluate_comprehensive <- function(params, data) {
  # Filter based on all parameters
  # Area: min is varied, max is always Inf
  # Circularity: min is always 0, max is varied
  filtered_data <- data %>%
    filter(
      area >= params$min_area,
      circularity <= params$max_circularity,
      aspect_ratio >= params$min_aspect_ratio,
      skeleton_length >= params$min_skeleton_length,
      mean_width >= params$min_mean_width
    )
  
  n_retained <- nrow(filtered_data)
  
  default_return <- list(
    score = -Inf,
    n_retained = n_retained,
    retention_rate = n_retained / nrow(data),
    neurite_separation = NA_real_,
    confluence_separation = NA_real_,
    signal_noise_ratio = NA_real_,
    high_neurites_mean = NA_real_,
    low_neurites_mean = NA_real_,
    high_conf_mean = NA_real_,
    low_conf_mean = NA_real_,
    signal_mean = NA_real_,
    noise_mean = NA_real_
  )
  
  if (n_retained < 100) return(default_return)
  
  signal_data <- filtered_data %>% filter(is_signal)
  noise_data <- filtered_data %>% filter(is_noise)
  
  if (nrow(signal_data) < 50) return(default_return)
  
  # Calculate group means
  high_neur <- mean(signal_data$skeleton_length[signal_data$is_high_neurites], na.rm = TRUE)
  low_neur <- mean(signal_data$skeleton_length[signal_data$is_low_neurites], na.rm = TRUE)
  high_conf <- mean(signal_data$skeleton_length[signal_data$is_high_confluence], na.rm = TRUE)
  low_conf <- mean(signal_data$skeleton_length[signal_data$is_low_confluence], na.rm = TRUE)
  
  # Handle NaN/Inf
  if (is.nan(high_neur) || !is.finite(high_neur)) high_neur <- NA_real_
  if (is.nan(low_neur) || !is.finite(low_neur)) low_neur <- NA_real_
  if (is.nan(high_conf) || !is.finite(high_conf)) high_conf <- NA_real_
  if (is.nan(low_conf) || !is.finite(low_conf)) low_conf <- NA_real_
  
  # Calculate separations
  if (!is.na(high_neur) && !is.na(low_neur) && (high_neur + low_neur) > 0) {
    neurite_sep <- abs(high_neur - low_neur) / (mean(c(high_neur, low_neur)))
  } else {
    neurite_sep <- NA_real_
  }
  
  if (!is.na(high_conf) && !is.na(low_conf) && (high_conf + low_conf) > 0) {
    confluence_sep <- abs(high_conf - low_conf) / (mean(c(high_conf, low_conf)))
  } else {
    confluence_sep <- NA_real_
  }
  
  # Signal to noise
  signal_mean <- mean(signal_data$skeleton_length, na.rm = TRUE)
  noise_mean <- ifelse(nrow(noise_data) > 0, mean(noise_data$skeleton_length, na.rm = TRUE), 0)
  signal_noise <- signal_mean / (noise_mean + 1)
  
  # Composite score
  if (!is.na(neurite_sep) && !is.na(confluence_sep) && is.finite(signal_noise)) {
    # Maximize neurite separation, minimize confluence separation, maximize S/N
    score <- (neurite_sep * 3) - (confluence_sep * 1.5) + log(signal_noise)
  } else {
    score <- -Inf
  }
  
  return(list(
    score = score,
    n_retained = n_retained,
    retention_rate = n_retained / nrow(data),
    neurite_separation = neurite_sep,
    confluence_separation = confluence_sep,
    signal_noise_ratio = signal_noise,
    high_neurites_mean = high_neur,
    low_neurites_mean = low_neur,
    high_conf_mean = high_conf,
    low_conf_mean = low_conf,
    signal_mean = signal_mean,
    noise_mean = noise_mean
  ))
}

# ===== RUN OPTIMIZATION =====
cat("\n=== RUNNING OPTIMIZATION ===\n")
cat("Testing", nrow(param_grid), "parameter combinations...\n")

optimization_results <- list()
pb <- txtProgressBar(min = 0, max = nrow(param_grid), style = 3)

for (i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  results <- evaluate_comprehensive(params, roi_analysis)
  
  result_row <- data.frame(
    min_area = as.numeric(params$min_area),
    max_circularity = as.numeric(params$max_circularity),
    min_aspect_ratio = as.numeric(params$min_aspect_ratio),
    min_skeleton_length = as.numeric(params$min_skeleton_length),
    min_mean_width = as.numeric(params$min_mean_width),
    score = results$score,
    n_retained = results$n_retained,
    retention_rate = results$retention_rate,
    neurite_separation = results$neurite_separation,
    confluence_separation = results$confluence_separation,
    signal_noise_ratio = results$signal_noise_ratio,
    high_neurites_mean = results$high_neurites_mean,
    low_neurites_mean = results$low_neurites_mean,
    high_conf_mean = results$high_conf_mean,
    low_conf_mean = results$low_conf_mean,
    signal_mean = results$signal_mean,
    noise_mean = results$noise_mean,
    stringsAsFactors = FALSE
  )
  
  optimization_results[[i]] <- result_row
  
  if (i %% 1000 == 0) {
    setTxtProgressBar(pb, i)
  }
}
close(pb)

# Combine results
optimization_df <- do.call(rbind, optimization_results)
optimization_df <- optimization_df %>%
  filter(is.finite(score)) %>%
  arrange(desc(score))

write_csv(optimization_df, "OUT_optimize/comprehensive_optimization_results.csv")
cat("\nResults saved to: OUT_optimize/comprehensive_optimization_results.csv\n")

# ===== VISUALIZATIONS =====
cat("\n=== CREATING VISUALIZATIONS ===\n")

# Top results
top_results <- head(optimization_df, 100)

# 1. Score vs key metrics
p1 <- ggplot(top_results, aes(x = neurite_separation, y = signal_noise_ratio, 
                              size = score, color = confluence_separation)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis(option = "plasma") +
  labs(title = "Top 100 Parameter Sets: Optimization Landscape",
       x = "Neurite Separation (higher is better)",
       y = "Signal/Noise Ratio (higher is better)",
       size = "Score", color = "Confluence\nSeparation\n(lower is better)") +
  theme_minimal()

ggsave("OUT_optimize/optimization_landscape.png", p1, width = 12, height = 8)

# 2. Parameter effects on score
param_effects <- optimization_df %>%
  filter(score > quantile(score, 0.9, na.rm = TRUE)) %>%
  select(min_area, max_circularity, min_aspect_ratio, 
         min_skeleton_length, min_mean_width, score) %>%
  pivot_longer(cols = -score, names_to = "parameter", values_to = "value")

p2 <- ggplot(param_effects, aes(x = value, y = score)) +
  geom_point(alpha = 0.3, color = "steelblue") +
  geom_smooth(method = "loess", color = "red", se = TRUE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(title = "Parameter Effects on Score (Top 10% Results)",
       x = "Parameter Value", y = "Score") +
  theme_minimal()

ggsave("OUT_optimize/parameter_effects_on_score.png", p2, width = 14, height = 10)

# 3. Pareto front: Neurite separation vs Confluence separation
p3 <- ggplot(optimization_df %>% filter(score > quantile(score, 0.8, na.rm = TRUE)),
             aes(x = neurite_separation, y = -confluence_separation, color = signal_noise_ratio)) +
  geom_point(alpha = 0.5, size = 2) +
  scale_color_viridis(option = "magma") +
  labs(title = "Pareto Front: Neurite vs Confluence Separation (Top 20% Results)",
       x = "Neurite Separation (maximize →)",
       y = "Inverse Confluence Separation (maximize →, means low conf. sep.)",
       color = "Signal/Noise\nRatio") +
  theme_minimal()

ggsave("OUT_optimize/pareto_front.png", p3, width = 12, height = 8)

# 4. Retention rate vs Score
p4 <- ggplot(optimization_df, aes(x = retention_rate * 100, y = score)) +
  geom_hex(bins = 50) +
  scale_fill_viridis(option = "inferno", trans = "log10") +
  geom_vline(xintercept = c(10, 25, 50), linetype = "dashed", color = "red", alpha = 0.5) +
  labs(title = "Score vs ROI Retention Rate",
       x = "ROI Retention Rate (%)", y = "Optimization Score",
       fill = "Count") +
  theme_minimal()

ggsave("OUT_optimize/score_vs_retention.png", p4, width = 10, height = 8)

# 5. Heatmap: Two most important parameters
top_2_params <- head(param_importance$param, 2)
cat("\nCreating heatmap for top 2 parameters:", paste(top_2_params, collapse = " vs "), "\n")

# Prepare data for heatmap
if (length(top_2_params) >= 2) {
  param1_col <- paste0("min_", top_2_params[1])
  param2_col <- paste0("min_", top_2_params[2])
  
  if (param1_col %in% names(optimization_df) && param2_col %in% names(optimization_df)) {
    heatmap_data <- optimization_df %>%
      group_by(!!sym(param1_col), !!sym(param2_col)) %>%
      summarise(
        mean_score = mean(score, na.rm = TRUE),
        n = n(),
        .groups = "drop"
      ) %>%
      filter(n >= 5)
    
    p5 <- ggplot(heatmap_data, aes(x = !!sym(param1_col), y = !!sym(param2_col), fill = mean_score)) +
      geom_tile() +
      scale_fill_viridis(option = "plasma") +
      labs(title = paste("Parameter Interaction Heatmap:", top_2_params[1], "vs", top_2_params[2]),
           x = top_2_params[1], y = top_2_params[2], fill = "Mean Score") +
      theme_minimal()
    
    ggsave("OUT_optimize/parameter_interaction_heatmap.png", p5, width = 12, height = 10)
  }
}

# 6. Distribution of metrics for top solutions
top_10_results <- head(optimization_df, 10)

metrics_long <- top_10_results %>%
  mutate(rank = row_number()) %>%
  select(rank, neurite_separation, confluence_separation, signal_noise_ratio) %>%
  pivot_longer(cols = -rank, names_to = "metric", values_to = "value")

p6 <- ggplot(metrics_long, aes(x = factor(rank), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Top 10 Solutions: Metric Comparison",
       x = "Rank", y = "Value", fill = "Metric") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0))

ggsave("OUT_optimize/top10_metrics_comparison.png", p6, width = 12, height = 6)

# ===== BEST SOLUTIONS SUMMARY =====
cat("\n" , rep("=", 80), "\n", sep = "")
cat("=== TOP 10 PARAMETER COMBINATIONS ===\n")
cat(rep("=", 80), "\n\n", sep = "")

for (i in 1:min(10, nrow(optimization_df))) {
  cat(sprintf("\n--- RANK #%d ---\n", i))
  cat(sprintf("Score: %.4f\n", optimization_df$score[i]))
  cat(sprintf("ROIs Retained: %d (%.2f%%)\n", 
              optimization_df$n_retained[i],
              optimization_df$retention_rate[i] * 100))
  cat("\nParameters:\n")
  cat(sprintf("  Min Area: %.1f (max = Inf)\n", optimization_df$min_area[i]))
  cat(sprintf("  Max Circularity: %.2f (min = 0)\n", optimization_df$max_circularity[i]))
  cat(sprintf("  Min Aspect Ratio: %.2f (max = Inf)\n", optimization_df$min_aspect_ratio[i]))
  cat(sprintf("  Min Skeleton Length: %.1f (max = Inf)\n", optimization_df$min_skeleton_length[i]))
  cat(sprintf("  Min Mean Width: %.2f (max = Inf)\n", optimization_df$min_mean_width[i]))
  cat("\nPerformance Metrics:\n")
  cat(sprintf("  Neurite Separation: %.4f (HIGH is better)\n", optimization_df$neurite_separation[i]))
  cat(sprintf("  Confluence Separation: %.4f (LOW is better)\n", optimization_df$confluence_separation[i]))
  cat(sprintf("  Signal/Noise Ratio: %.4f (HIGH is better)\n", optimization_df$signal_noise_ratio[i]))
  cat("\nGroup Means (Skeleton Length):\n")
  cat(sprintf("  High Neurites: %.2f\n", optimization_df$high_neurites_mean[i]))
  cat(sprintf("  Low Neurites: %.2f\n", optimization_df$low_neurites_mean[i]))
  cat(sprintf("  High Confluence: %.2f\n", optimization_df$high_conf_mean[i]))
  cat(sprintf("  Low Confluence: %.2f\n", optimization_df$low_conf_mean[i]))
  cat(sprintf("  Signal Mean: %.2f\n", optimization_df$signal_mean[i]))
  cat(sprintf("  Noise Mean: %.2f\n", optimization_df$noise_mean[i]))
  cat("\n")
}

# Save top 10 to separate file
write_csv(head(optimization_df, 10), "OUT_optimize/top10_parameter_sets.csv")

# ===== FINAL RECOMMENDATION =====
cat("\n" , rep("=", 80), "\n", sep = "")
cat("=== RECOMMENDED PARAMETER SET (RANK #1) ===\n")
cat(rep("=", 80), "\n\n", sep = "")

best <- optimization_df[1, ]

cat("JUSTIFICATION:\n")
cat("This parameter combination was selected because it:\n")
cat(sprintf("1. Maximizes neurite separation (%.4f) - distinguishes high vs low neurite groups\n", 
            best$neurite_separation))
cat(sprintf("2. Minimizes confluence separation (%.4f) - ensures confluence doesn't confound results\n", 
            best$confluence_separation))
cat(sprintf("3. Maximizes signal-to-noise ratio (%.4f) - filters out noise ROIs effectively\n", 
            best$signal_noise_ratio))
cat(sprintf("4. Retains %.2f%% of ROIs - maintains sufficient data for analysis\n", 
            best$retention_rate * 100))
cat(sprintf("5. Achieves highest composite score (%.4f) balancing all objectives\n\n", best$score))

cat("PARAMETERS TO USE:\n")
cat(sprintf("  min_area = %.1f (max_area = Inf)\n", best$min_area))
cat(sprintf("  max_circularity = %.2f (min_circularity = 0)\n", best$max_circularity))
cat(sprintf("  min_aspect_ratio = %.2f (max_aspect_ratio = Inf)\n", best$min_aspect_ratio))
cat(sprintf("  min_skeleton_length = %.1f (max_skeleton_length = Inf)\n", best$min_skeleton_length))
cat(sprintf("  min_mean_width = %.2f (max_mean_width = Inf)\n\n", best$min_mean_width))

cat("EXPECTED RESULTS:\n")
cat(sprintf("  High neurites group mean skeleton length: %.2f\n", best$high_neurites_mean))
cat(sprintf("  Low neurites group mean skeleton length: %.2f\n", best$low_neurites_mean))
cat(sprintf("  Difference: %.2f (%.1f%% separation)\n", 
            abs(best$high_neurites_mean - best$low_neurites_mean),
            best$neurite_separation * 100))

# Create summary visualization for best solution
best_params_plot <- data.frame(
  metric = c("Neurite\nSeparation", "Confluence\nSeparation\n(inverted)", "Signal/Noise\nRatio"),
  value = c(best$neurite_separation, -best$confluence_separation, best$signal_noise_ratio / 5),
  optimal_direction = c("Higher →", "Higher →", "Higher →")
)

p7 <- ggplot(best_params_plot, aes(x = metric, y = value, fill = metric)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = optimal_direction), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "Best Parameter Set: Performance Metrics",
       subtitle = paste("Score:", round(best$score, 4)),
       x = "", y = "Normalized Value") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 11))

ggsave("OUT_optimize/best_solution_metrics.png", p7, width = 10, height = 6)

# Create comparison plot: Before vs After filtering
comparison_data <- data.frame(
  dataset = rep(c("All ROIs", "Filtered ROIs"), each = 4),
  group = rep(c("High Neurites", "Low Neurites", "Signal", "Noise"), 2),
  mean_skel = c(
    mean(roi_analysis$skeleton_length[roi_analysis$is_high_neurites & roi_analysis$is_signal], na.rm = TRUE),
    mean(roi_analysis$skeleton_length[roi_analysis$is_low_neurites & roi_analysis$is_signal], na.rm = TRUE),
    mean(roi_analysis$skeleton_length[roi_analysis$is_signal], na.rm = TRUE),
    mean(roi_analysis$skeleton_length[roi_analysis$is_noise], na.rm = TRUE),
    best$high_neurites_mean,
    best$low_neurites_mean,
    best$signal_mean,
    best$noise_mean
  )
)

p8 <- ggplot(comparison_data, aes(x = group, y = mean_skel, fill = dataset)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("All ROIs" = "grey70", "Filtered ROIs" = "steelblue")) +
  labs(title = "Impact of Optimal Filtering",
       subtitle = "Mean Skeleton Length by Group",
       x = "Group", y = "Mean Skeleton Length",
       fill = "Dataset") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("OUT_optimize/before_after_filtering.png", p8, width = 10, height = 6)

cat("\n" , rep("=", 80), "\n", sep = "")
cat("=== OPTIMIZATION COMPLETE ===\n")
cat(rep("=", 80), "\n\n", sep = "")
cat("All results and visualizations saved to: OUT_optimize/\n")
cat("\nKey files:\n")
cat("  - comprehensive_optimization_results.csv (all combinations)\n")
cat("  - top10_parameter_sets.csv (best solutions)\n")
cat("  - optimization_landscape.png (visual overview)\n")
cat("  - best_solution_metrics.png (recommended parameters)\n")
cat("  - before_after_filtering.png (impact demonstration)\n\n")

