# Statistical Interpretation Guide

This guide explains the statistical logic used by the browser app after the image-analysis pipeline has produced the tab-3 generalized model-feature table.

## Statistical Unit

Most tab-4 analyses use one row per image per applied cutoff profile.

If one cutoff profile is selected, rows are approximately image-level observations. If two or more cutoff profiles are selected, the same biological image can appear more than once, once per model. This is useful for comparing cutoff models, but it should not be treated as independent biological replication.

For biological inference, usually filter to one trusted cutoff profile first. For model comparison, keep multiple profiles and interpret results as image-model rows.

## Training-Image Audit

Tab `4.0` identifies images used during ilastik training and cutoff optimization. These images can be included or excluded from downstream plots and models.

Use exclusion when you want a more conservative estimate of how well the final model generalizes to images not used during training or validation.

## PCA and Global Structure

Tab `4.1` runs PCA on selected numeric image/model features.

PCA is unsupervised. It does not test treatment effects directly. It shows which combinations of measured features explain broad variation in the dataset.

The app median-imputes missing numeric feature values before PCA, drops sparse or constant variables, centers and scales features, and then reports:

- scree plot: percent variance explained by each principal component
- score plot: where image/model rows fall in PC space
- loadings: which original variables push each selected PC up or down
- separation metrics: whether selected metadata or model labels align with the PCA coordinates

PCA separation by a chosen label is diagnostic, not proof. A visible separation should be followed by feature screening, grouped plots, and adjusted modeling.

## Feature Screening by Contrast

Tab `4.2` compares two selected groups for many numeric features.

The table reports:

- Cohen's d: standardized mean difference between the two groups
- t-test p-value: quick unadjusted feature screen
- Holm-adjusted p-value: multiple-testing correction across the screened feature list

This tab is for prioritizing candidate readouts. It is not the final biological conclusion because it does not automatically adjust for confounders such as confluence, seeding density, treatment day, or training-image status.

## Grouped Dot and Bar Plots

Tab `4.3` visualizes one selected outcome across groups.

Dots are individual image/model rows. Bars show group means. These plots are deliberately descriptive: they help detect outliers, imbalance, confounding, and whether a mean difference is driven by many images or by a few extreme points.

Optional normalization divides the plotted outcome by a selected numeric variable, such as confluence. This is a visual sensitivity check. Formal adjustment should be done in tab `4.5`.

## Covariate Correlation Check

Tab `4.4` checks pairwise Pearson correlations among selected numeric covariates.

Highly correlated covariates, commonly `|r| >= 0.80`, often carry overlapping information. Including many overlapping covariates in one model can make estimates unstable and hard to interpret.

The recommendation table is a heuristic. It helps avoid putting strongly correlated numeric variables into the same model, but biological interpretability and missingness should override the heuristic when appropriate.

## Adjusted Effect Modeling / ANCOVA

Tab `4.5` fits a linear model:

```text
outcome ~ primary_factor + covariates
```

The outcome should be the neurite or image-level readout to explain. The primary factor should be the biological or metadata grouping variable to test, such as treatment. Covariates should be adjustment variables, such as confluence or time, not the same variable as the outcome or the primary factor.

The app can optionally create a ratio-normalized outcome for ANCOVA, for example:

```text
image_total_skeleton_length / confluence ~ treatment + covariates
```

This is not equivalent to using confluence as a covariate:

```text
image_total_skeleton_length ~ treatment + confluence + covariates
```

The ratio model asks whether groups differ in neurite readout per unit denominator. The covariate-adjusted model asks whether groups differ in the original readout after statistically accounting for the denominator. Both can be useful, but they answer different questions. When a ratio denominator is selected, the app removes that denominator from the covariate list by default to avoid using the same quantity twice.

The contribution table uses partial drop-one F tests. Each term is tested by removing it from the full model while keeping the other selected terms. This is safer for adjusted interpretation than sequential ANOVA sums-of-squares, where term order can change the result.

The app reports:

- partial eta-squared: adjusted contribution of each term
- normalized impact size: square root of partial eta-squared
- adjusted primary-factor contrasts: selected two levels, each level versus reference/control, or all pairwise comparisons

Avoid over-adjustment. For example, if the outcome is total skeleton length, adjusting for retained ROI count or mean skeleton length changes the scientific question from "does treatment change total neurite burden?" to "does treatment change total neurite burden after fixing object count or object size?"

## Practical Analysis Order

1. Use tab `4.0` to decide whether to include or exclude training images.
2. Use tab `4.1` to understand global structure and possible confounders.
3. Use tab `4.2` to identify promising readouts for a selected two-group contrast.
4. Use tab `4.3` to inspect those readouts visually and check sample balance.
5. Use tab `4.4` to avoid strongly overlapping covariates.
6. Use tab `4.5` for adjusted modeling of a small number of biologically motivated outcomes.
7. Save publication candidates only after the plot and model specification match the biological question.
