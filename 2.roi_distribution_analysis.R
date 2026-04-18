# ROI Distribution Analysis and Comprehensive Parameter Optimization
library(dplyr)
library(ggplot2)
library(tidyr)
library(gridExtra)
library(scales)

# Assume roi_analysis is already loaded
# Load if needed: roi_analysis <- read_csv("OUT_optimize/roi_analysis_raw.csv")

# ===== PART 1: DISTRIBUTION ANALYSIS =====
cat("=== ANALYZING DISTRIBUTIONS ===\n")

# Function to test if log transformation improves normality
test_log_transformation <- function(x) {
  x_positive <- x[x > 0 & is.finite(x)]
  if (length(x_positive) < 30) return(list(use_log = FALSE, reason = "insufficient data"))
  
  # Shapiro-Wilk test (for smaller samples) or skewness
  skew_original <- (mean(x_positive) - median(x_positive)) / sd(x_positive)
  skew_log <- (mean(log(x_positive)) - median(log(x_positive))) / sd(log(x_positive))
  
  use_log <- abs(skew_log) < abs(skew_original)
  
  return(list(
    use_log = use_log,
    skew_original = skew_original,
    skew_log = skew_log
  ))
}

# Variables to analyze
vars_to_analyze <- c("area", "perimeter", "circularity", "aspect_ratio", 
                     "solidity", "mean_width", "skeleton_length", "avg_branch_length")

# Test each variable
distribution_tests <- list()
for (var in vars_to_analyze) {
  if (var %in% names(roi_analysis)) {
    test_result <- test_log_transformation(roi_analysis[[var]])
    distribution_tests[[var]] <- test_result
    cat(sprintf("%s: Use log = %s (skew: %.3f -> %.3f)\n", 
                var, test_result$use_log, 
                test_result$skew_original, test_result$skew_log))
  }
}

# Create distribution plots
plot_list <- list()
for (var in vars_to_analyze) {
  if (var %in% names(roi_analysis)) {
    data_var <- roi_analysis[[var]]
    data_var <- data_var[is.finite(data_var) & data_var > 0]
    
    use_log <- distribution_tests[[var]]$use_log
    
    if (use_log) {
      p <- ggplot(data.frame(x = log10(data_var)), aes(x = x)) +
        geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
        geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +
        labs(title = paste(var, "(log10)"), x = "log10(value)", y = "Count") +
        theme_minimal() +
        theme(plot.title = element_text(size = 10))
    } else {
      p <- ggplot(data.frame(x = data_var), aes(x = x)) +
        geom_histogram(bins = 50, fill = "steelblue", alpha = 0.7) +
        geom_density(aes(y = after_stat(count)), color = "red", linewidth = 1) +
        labs(title = var, x = "Value", y = "Count") +
        theme_minimal() +
        theme(plot.title = element_text(size = 10))
    }
    
    plot_list[[var]] <- p
  }
}

# Save distribution grid
png("OUT_optimize/roi_distributions_grid.png", width = 1600, height = 1200, res = 150)
do.call(grid.arrange, c(plot_list, ncol = 3))
dev.off()
cat("\nDistribution plots saved to: OUT_optimize/roi_distributions_grid.png\n")

# ===== PART 2: CALCULATE PERCENTILES FOR GRID =====
cat("\n=== CALCULATING PARAMETER RANGES ===\n")

# Calculate percentiles for each variable
param_ranges <- list()

for (var in vars_to_analyze) {
  if (var %in% names(roi_analysis)) {
    data_var <- roi_analysis[[var]]
    data_var <- data_var[is.finite(data_var) & data_var > 0]
    
    use_log <- distribution_tests[[var]]$use_log
    
    if (use_log) {
      log_data <- log10(data_var)
      percentiles <- quantile(log_data, probs = c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm = TRUE)
      param_ranges[[var]] <- list(
        use_log = TRUE,
        percentiles_log = percentiles,
        percentiles_original = 10^percentiles,
        min_val = min(data_var, na.rm = TRUE),
        max_val = max(data_var, na.rm = TRUE)
      )
    } else {
      percentiles <- quantile(data_var, probs = c(0.05, 0.25, 0.50, 0.75, 0.95), na.rm = TRUE)
      param_ranges[[var]] <- list(
        use_log = FALSE,
        percentiles = percentiles,
        min_val = min(data_var, na.rm = TRUE),
        max_val = max(data_var, na.rm = TRUE)
      )
    }
    
    cat(sprintf("%s: 5th=%.2f, 25th=%.2f, 50th=%.2f, 75th=%.2f, 95th=%.2f\n",
                var,
                param_ranges[[var]]$percentiles_original[1] %||% param_ranges[[var]]$percentiles[1],
                param_ranges[[var]]$percentiles_original[2] %||% param_ranges[[var]]$percentiles[2],
                param_ranges[[var]]$percentiles_original[3] %||% param_ranges[[var]]$percentiles[3],
                param_ranges[[var]]$percentiles_original[4] %||% param_ranges[[var]]$percentiles[4],
                param_ranges[[var]]$percentiles_original[5] %||% param_ranges[[var]]$percentiles[5]))
  }
}

# Save parameter ranges
saveRDS(param_ranges, "OUT_optimize/parameter_ranges.rds")

# ===== PART 3: SINGLE PARAMETER SENSITIVITY ANALYSIS =====
cat("\n=== SINGLE PARAMETER SENSITIVITY ANALYSIS ===\n")

# Function to evaluate single parameter threshold
evaluate_single_threshold <- function(param_name, threshold_value, is_min = TRUE, data = roi_analysis) {
  if (is_min) {
    filtered_data <- data %>% filter(.data[[param_name]] >= threshold_value)
  } else {
    filtered_data <- data %>% filter(.data[[param_name]] <= threshold_value)
  }
  
  n_retained <- nrow(filtered_data)
  
  if (n_retained < 100) {
    return(data.frame(
      param = param_name,
      threshold = threshold_value,
      is_min = is_min,
      n_retained = n_retained,
      neurite_sep = NA,
      confluence_sep = NA,
      signal_noise = NA
    ))
  }
  
  signal_data <- filtered_data %>% filter(is_signal)
  noise_data <- filtered_data %>% filter(is_noise)
  
  if (nrow(signal_data) < 50) {
    return(data.frame(
      param = param_name,
      threshold = threshold_value,
      is_min = is_min,
      n_retained = n_retained,
      neurite_sep = NA,
      confluence_sep = NA,
      signal_noise = NA
    ))
  }
  
  # Calculate metrics
  high_neur <- mean(signal_data$skeleton_length[signal_data$is_high_neurites], na.rm = TRUE)
  low_neur <- mean(signal_data$skeleton_length[signal_data$is_low_neurites], na.rm = TRUE)
  high_conf <- mean(signal_data$skeleton_length[signal_data$is_high_confluence], na.rm = TRUE)
  low_conf <- mean(signal_data$skeleton_length[signal_data$is_low_confluence], na.rm = TRUE)
  
  neurite_sep <- abs(high_neur - low_neur) / (mean(c(high_neur, low_neur)) + 1)
  confluence_sep <- abs(high_conf - low_conf) / (mean(c(high_conf, low_conf)) + 1)
  
  signal_mean <- mean(signal_data$skeleton_length, na.rm = TRUE)
  noise_mean <- ifelse(nrow(noise_data) > 0, mean(noise_data$skeleton_length, na.rm = TRUE), 0)
  signal_noise <- signal_mean / (noise_mean + 1)
  
  return(data.frame(
    param = param_name,
    threshold = threshold_value,
    is_min = is_min,
    n_retained = n_retained,
    neurite_sep = neurite_sep,
    confluence_sep = confluence_sep,
    signal_noise = signal_noise
  ))
}

# Test each parameter across range
sensitivity_results <- list()

for (var in c("area", "perimeter", "circularity", "aspect_ratio", "mean_width", 
              "skeleton_length", "avg_branch_length")) {
  if (!var %in% names(roi_analysis)) next
  
  cat("Testing", var, "...\n")
  
  data_var <- roi_analysis[[var]]
  data_var <- data_var[is.finite(data_var) & data_var > 0]
  
  # Create threshold sequence
  if (var == "circularity") {
    thresholds_min <- seq(0, 0.5, by = 0.05)
    thresholds_max <- seq(0.5, 1.0, by = 0.05)
  } else if (var == "aspect_ratio") {
    # Use log scale for aspect ratio
    log_vals <- abs(log10(data_var))
    thresholds_min <- seq(0, quantile(log_vals, 0.95, na.rm = TRUE), length.out = 20)
  } else {
    # For others, use percentile-based thresholds
    thresholds_min <- quantile(data_var, probs = seq(0, 0.95, by = 0.05), na.rm = TRUE)
  }
  
  # Test minimum thresholds
  for (thresh in thresholds_min) {
    if (var == "aspect_ratio") {
      # Convert back from log
      actual_thresh <- 10^thresh
      result <- evaluate_single_threshold(var, actual_thresh, is_min = TRUE)
    } else {
      result <- evaluate_single_threshold(var, thresh, is_min = TRUE)
    }
    sensitivity_results[[length(sensitivity_results) + 1]] <- result
  }
  
  # Test maximum thresholds (only for circularity)
  if (var == "circularity") {
    for (thresh in thresholds_max) {
      result <- evaluate_single_threshold(var, thresh, is_min = FALSE)
      sensitivity_results[[length(sensitivity_results) + 1]] <- result
    }
  }
}

sensitivity_df <- do.call(rbind, sensitivity_results)
write_csv(sensitivity_df, "OUT_optimize/single_parameter_sensitivity.csv")

# Create sensitivity plots
sensitivity_plots <- list()

for (var in unique(sensitivity_df$param)) {
  var_data <- sensitivity_df %>% filter(param == var)
  
  p1 <- ggplot(var_data, aes(x = threshold, y = neurite_sep, color = is_min)) +
    geom_line(linewidth = 1) +
    geom_point() +
    labs(title = paste(var, "- Neurite Separation"),
         x = "Threshold", y = "Neurite Separation") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  p2 <- ggplot(var_data, aes(x = threshold, y = confluence_sep, color = is_min)) +
    geom_line(linewidth = 1) +
    geom_point() +
    labs(title = paste(var, "- Confluence Separation"),
         x = "Threshold", y = "Confluence Separation") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  p3 <- ggplot(var_data, aes(x = threshold, y = signal_noise, color = is_min)) +
    geom_line(linewidth = 1) +
    geom_point() +
    labs(title = paste(var, "- Signal/Noise"),
         x = "Threshold", y = "Signal/Noise Ratio") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  combined <- grid.arrange(p1, p2, p3, ncol = 1)
  
  ggsave(paste0("OUT_optimize/sensitivity_", var, ".png"), 
         combined, width = 10, height = 12)
}

cat("Sensitivity plots saved to OUT_optimize/\n")

# ===== PART 4: IDENTIFY MOST IMPORTANT PARAMETERS =====
cat("\n=== IDENTIFYING MOST IMPORTANT PARAMETERS ===\n")

# Calculate impact for each parameter
param_importance <- sensitivity_df %>%
  filter(!is.na(neurite_sep)) %>%
  group_by(param) %>%
  summarise(
    neurite_sep_range = max(neurite_sep, na.rm = TRUE) - min(neurite_sep, na.rm = TRUE),
    confluence_sep_range = max(confluence_sep, na.rm = TRUE) - min(confluence_sep, na.rm = TRUE),
    signal_noise_range = max(signal_noise, na.rm = TRUE) - min(signal_noise, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    importance_score = neurite_sep_range * 2 - confluence_sep_range + log(signal_noise_range + 1)
  ) %>%
  arrange(desc(importance_score))

print(param_importance)
write_csv(param_importance, "OUT_optimize/parameter_importance.csv")

# Plot importance
p_importance <- ggplot(param_importance, aes(x = reorder(param, importance_score), y = importance_score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Parameter Importance for Optimization",
       x = "Parameter", y = "Importance Score") +
  theme_minimal()

ggsave("OUT_optimize/parameter_importance.png", p_importance, width = 10, height = 6)

# Select top parameters for grid search
top_params <- head(param_importance$param, 4)
cat("\nTop parameters for optimization:", paste(top_params, collapse = ", "), "\n")

cat("\n=== Script complete - distributions analyzed ===\n")
cat("Next: Run the optimized grid search with these parameters\n")
