#!/usr/bin/env Rscript
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggrepel)
  library(forcats)
  library(stringr)
  library(pheatmap)
  library(RColorBrewer)
})

# ======================================================
# PATHS AND SETUP
# ======================================================
args <- commandArgs(trailingOnly = TRUE)
base_dir <- if (length(args) >= 1) args[1] else getwd()
setwd(base_dir)
source(file.path(base_dir, "launchers", "pipeline_paths.R"))
ctx <- pipeline_context(base_dir)

metadata_path <- ctx$paths$optim_groups_inspected
outdir <- ctx$paths$metadata_pca_dir
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

cat("\n=== PCA AND HEATMAP ANALYSIS ON METADATA VARIABLES ===\n\n")

# ======================================================
# LOAD AND CLEAN METADATA
# ======================================================
metadata <- read_csv(metadata_path, show_col_types = FALSE)
metadata$image_key <- tools::file_path_sans_ext(metadata$image_name)

clean_names <- function(x) {
  x %>%
    gsub(" ", "_", .) %>%
    gsub("%", "pct", .) %>%
    gsub("\\(", "", .) %>%
    gsub("\\)", "", .) %>%
    make.names()
}
colnames(metadata) <- clean_names(colnames(metadata))

# ======================================================
# SUBSET RELEVANT COLUMNS
# ======================================================
meta_use <- metadata %>%
  select(
    image_key,
    days_after_seeding,
    matches("cell_counts_at_seeding"),
    FBS_percentage,
    first_differentiating_agent,
    matches("first_agent_concentration"),
    phorbol_ester,
    intended_assay,
    cell_bodies_percent,
    neurites_percent,
    backgroundpct,
    total_area_px,
    confluence
  )

# ======================================================
# COMBINE AGENT + CONCENTRATION AS ORDERED FACTOR
# ======================================================
meta_use <- meta_use %>%
  mutate(
    first_differentiating_agent = ifelse(
      is.na(first_differentiating_agent) | first_differentiating_agent == "",
      "None",
      first_differentiating_agent
    ),
    across(matches("first_agent_concentration"), ~ ifelse(is.na(.x), 0, .x)),
    AgentConc = paste0(first_differentiating_agent, "_",
                       get(grep("first_agent_concentration", names(meta_use), value = TRUE))),
    AgentConc = fct_inorder(AgentConc)
  )

# ======================================================
# PREPARE NUMERIC MATRIX FOR PCA
# ======================================================
num_vars <- meta_use %>%
  select(
    days_after_seeding,
    matches("cell_counts_at_seeding"),
    FBS_percentage,
    matches("first_agent_concentration"),
    cell_bodies_percent,
    neurites_percent,
    backgroundpct,
    total_area_px,
    confluence
  )

vars <- apply(num_vars, 2, var, na.rm = TRUE)
zero_var_cols <- names(vars[vars == 0 | is.na(vars)])
if (length(zero_var_cols) > 0) {
  cat("Dropping constant columns:", paste(zero_var_cols, collapse = ", "), "\n")
  num_vars <- num_vars[, setdiff(colnames(num_vars), zero_var_cols), drop = FALSE]
}

X_scaled <- scale(num_vars)

for (j in seq_len(ncol(X_scaled))) {
  bad <- !is.finite(X_scaled[, j])
  if (any(bad)) {
    cat("Replacing", sum(bad), "missing values in", colnames(X_scaled)[j], "with column mean\n")
    X_scaled[bad, j] <- mean(X_scaled[, j], na.rm = TRUE)
  }
}

# ======================================================
# PCA
# ======================================================
pca <- prcomp(X_scaled, center = FALSE, scale. = FALSE)
var_exp <- summary(pca)$importance[2, ] * 100

# ======================================================
# COMBINE SCORES WITH METADATA
# ======================================================
scores <- as.data.frame(pca$x[, 1:4])
colnames(scores)[1:4] <- c("PC1", "PC2", "PC3", "PC4")

meta_small <- meta_use %>%
  select(image_key, AgentConc, phorbol_ester, intended_assay, confluence) %>%
  distinct(image_key, .keep_all = TRUE)

dup_cols <- intersect(names(meta_small), names(scores))
if (length(dup_cols) > 0) {
  meta_small <- meta_small[, !names(meta_small) %in% dup_cols, drop = FALSE]
}

scores <- cbind(meta_small, scores)
names(scores) <- make.names(names(scores), unique = TRUE)
scores$image_label <- str_trunc(scores$image_key, width = 20)
scores <- scores %>% filter(is.finite(PC1), is.finite(PC2))

# ======================================================
# PCA SCATTER: AGENT × CONCENTRATION
# ======================================================
p1 <- ggplot(scores, aes(PC1, PC2, color = AgentConc, shape = intended_assay)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(group = AgentConc), type = "norm", linetype = "dashed", alpha = 0.4) +
  labs(
    title = "PCA of Image Metadata",
    subtitle = "Color: Differentiating Agent × Concentration",
    x = paste0("PC1 (", round(var_exp[1], 1), "% variance)"),
    y = paste0("PC2 (", round(var_exp[2], 1), "% variance)")
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "right")

ggsave(file.path(outdir, "PCA_metadata_by_agent_conc.png"), p1, width = 11, height = 7, dpi = 150)

# ======================================================
# PCA SCATTER: CONFLUENCE
# ======================================================
p2 <- ggplot(scores, aes(PC1, PC2, color = confluence)) +
  geom_point(size = 3, alpha = 0.85) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "PCA of Image Metadata",
    subtitle = "Color: Confluence (%)",
    color = "Confluence (%)",
    x = paste0("PC1 (", round(var_exp[1], 1), "% variance)"),
    y = paste0("PC2 (", round(var_exp[2], 1), "% variance)")
  ) +
  theme_minimal(base_size = 13)

ggsave(file.path(outdir, "PCA_metadata_confluence.png"), p2, width = 10, height = 7, dpi = 150)

# ======================================================
# PCA LOADINGS (PC1–PC2 and PC3–PC4)
# ======================================================
loadings <- as.data.frame(pca$rotation[, 1:4])
loadings$feature <- rownames(loadings)
loadings$abs_sum12 <- rowSums(abs(loadings[, c("PC1", "PC2")]))
loadings$abs_sum34 <- rowSums(abs(loadings[, c("PC3", "PC4")]))

top_load12 <- loadings %>% as_tibble() %>% arrange(desc(abs_sum12)) %>% slice_head(n = 10)
top_load34 <- loadings %>% as_tibble() %>% arrange(desc(abs_sum34)) %>% slice_head(n = 10)

arrow_scale <- 2.5

p3 <- ggplot() +
  geom_hline(yintercept = 0, color = "gray70", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray70", linetype = "dashed") +
  geom_segment(data = top_load12,
               aes(x = 0, y = 0, xend = PC1 * arrow_scale, yend = PC2 * arrow_scale),
               arrow = arrow(length = unit(0.2, "cm")),
               alpha = 0.8, color = "steelblue") +
  geom_text_repel(data = top_load12,
                  aes(x = PC1 * arrow_scale, y = PC2 * arrow_scale, label = feature),
                  size = 3.5) +
  labs(
    title = "Top PCA Loadings (Metadata Variables)",
    subtitle = "PC1 vs PC2",
    x = paste0("Loading on PC1 (", round(var_exp[1], 1), "% variance)"),
    y = paste0("Loading on PC2 (", round(var_exp[2], 1), "% variance)")
  ) +
  theme_minimal(base_size = 13)

ggsave(file.path(outdir, "PCA_metadata_loadings_PC1_PC2.png"), p3, width = 8, height = 6, dpi = 150)

p4 <- ggplot() +
  geom_hline(yintercept = 0, color = "gray70", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "gray70", linetype = "dashed") +
  geom_segment(data = top_load34,
               aes(x = 0, y = 0, xend = PC3 * arrow_scale, yend = PC4 * arrow_scale),
               arrow = arrow(length = unit(0.2, "cm")),
               alpha = 0.8, color = "darkorange") +
  geom_text_repel(data = top_load34,
                  aes(x = PC3 * arrow_scale, y = PC4 * arrow_scale, label = feature),
                  size = 3.5) +
  labs(
    title = "Top PCA Loadings (Metadata Variables)",
    subtitle = "PC3 vs PC4",
    x = paste0("Loading on PC3 (", round(var_exp[3], 1), "% variance)"),
    y = paste0("Loading on PC4 (", round(var_exp[4], 1), "% variance)")
  ) +
  theme_minimal(base_size = 13)

ggsave(file.path(outdir, "PCA_metadata_loadings_PC3_PC4.png"), p4, width = 8, height = 6, dpi = 150)

# ======================================================
# HEATMAP: FOCUS ON NEURITE PARAMETERS AFTER CUTOFFS
# ======================================================
heat_data <- meta_use %>%
  select(
    first_differentiating_agent,
    first_agent_concentrationmicromolar = matches("first_agent_concentration"),
    FBS_percentage,
    days_after_seeding,
    phorbol_ester,
    neurites_percent,
    cell_bodies_percent,
    confluence
  )

heat_data$phorbol_ester <- as.numeric(heat_data$phorbol_ester)

# Scale (z-score)
scaled_heat <- heat_data %>%
  mutate(across(where(is.numeric), ~ scale(.)[, 1])) %>%
  as.data.frame()

# Aggregate mean per condition
agg_heat <- scaled_heat %>%
  group_by(
    first_differentiating_agent,
    first_agent_concentrationmicromolar,
    FBS_percentage,
    days_after_seeding,
    phorbol_ester
  ) %>%
  summarise(across(c(neurites_percent, cell_bodies_percent, confluence),
                   \(x) mean(x, na.rm = TRUE)), .groups = "drop")

# Create unique condition labels
agg_heat$Condition <- paste0(
  agg_heat$first_differentiating_agent, "_",
  agg_heat$first_agent_concentrationmicromolar, "_",
  "FBS", agg_heat$FBS_percentage, "_Day", agg_heat$days_after_seeding
)

# Build numeric matrix
mat <- agg_heat %>%
  select(neurites_percent, cell_bodies_percent, confluence) %>%
  as.data.frame()
mat <- as.matrix(sapply(mat, as.numeric))  # ensure purely numeric
rownames(mat) <- make.unique(agg_heat$Condition)

# Row annotations
annotation_df <- as.data.frame(agg_heat %>%
                                 select(FBS_percentage, days_after_seeding, phorbol_ester) %>%
                                 mutate(
                                   phorbol_ester = factor(ifelse(phorbol_ester > 0, "Yes", "No")),
                                   FBS_percentage = factor(FBS_percentage),
                                   days_after_seeding = factor(days_after_seeding)
                                 ))
rownames(annotation_df) <- rownames(mat)

# Annotation color maps
ann_colors <- list(
  phorbol_ester = c(Yes = "#D55E00", No = "#0072B2"),
  FBS_percentage = setNames(
    brewer.pal(n = max(3, length(unique(annotation_df$FBS_percentage))), "Blues"),
    levels(annotation_df$FBS_percentage)
  ),
  days_after_seeding = setNames(
    brewer.pal(n = max(3, length(unique(annotation_df$days_after_seeding))), "Purples"),
    levels(annotation_df$days_after_seeding)
  )
)

# Plot heatmap
pheatmap(
  mat,
  color = colorRampPalette(rev(brewer.pal(11, "RdBu")))(200),
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  annotation_row = annotation_df,
  annotation_colors = ann_colors,
  show_rownames = TRUE,
  show_colnames = TRUE,
  main = "Scaled Neurite-Related Parameters by Agent, Conc, and Culture Conditions",
  fontsize_row = 8,
  fontsize_col = 10,
  border_color = NA,
  angle_col = 45,
  filename = file.path(outdir, "metadata_scaled_heatmap_neurite_focused.png"),  # <-- this saves it!
  width = 8.5,
  height = 7
)


ggsave(file.path(outdir, "metadata_scaled_heatmap_neurite_focused.png"), width = 8.5, height = 7, dpi = 150)

# ======================================================
# SAVE OBJECTS
# ======================================================
saveRDS(list(
  pca = pca,
  scores = scores,
  loadings = loadings,
  features = colnames(num_vars),
  heatmap_matrix = mat
), file = file.path(outdir, "PCA_metadata_objects.rds"))

cat("\nSaved outputs to:\n")
cat(" - PCA_metadata_by_agent_conc.png\n")
cat(" - PCA_metadata_confluence.png\n")
cat(" - PCA_metadata_loadings_PC1_PC2.png\n")
cat(" - PCA_metadata_loadings_PC3_PC4.png\n")
cat(" - metadata_scaled_heatmap_neurite_focused.png\n")
cat(" - PCA_metadata_objects.rds\n")
cat("\nOutput dir:", normalizePath(outdir), "\n\n")
