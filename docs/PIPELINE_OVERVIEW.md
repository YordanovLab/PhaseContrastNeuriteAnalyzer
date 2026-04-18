# Neurite Analysis in Phase Contrast Images

## Goal
This project analyzes phase contrast images to derive neurite-related segmentation, ROI, skeleton, optimization, and downstream biological interpretation outputs.

## First principle
Start with the shared settings file:
- `config/pipeline_settings.env`

That file now centralizes:
- project root
- external executable locations
- common output folder names
- key handoff filenames such as `OUT_img_input1.txt`
- current operation mode
- optional active validation or optimization profile name

## Why `OUT_img_input1.txt` matters
`OUT_img_input1.txt` is a critical handoff file, not just a temporary list. It records the exact image set that later preprocessing will run on. If that list is wrong, the downstream pipeline can still run but analyze the wrong image universe.

## Architecture overview

### Setup and preprocessing chain
1. `0.Rename.sh`
   - uses shared settings
   - creates hard-linked renamed images in `PRE_renamed/`
2. `1.get_img_list.sh`
   - uses shared settings
   - builds `OUT_img_input1.txt`
3. `2.RunFIJI_clean_bckgnd.sh`
   - Bash runner
   - calls `Macro_1-clear_bckgnd.ijm`
4. `3.Run_ilastik_model.sh`
   - Bash runner
   - calls the ilastik executable with the configured `.ilp` project
5. `4.GetArea%Fiji.sh`
   - Bash runner
   - calls `Macro_2-get_mask_area_percentages.ijm`

### Validation and optimization chain
6. `5.SelectValidationImgGroups.sh`
   - Bash runner
   - calls `CombineFiles_GetValGroups.R`
   - creates validation group folders
7. manual review
   - user fills `choice/` and `noise/` folders
8. `6.SelectedValidationImgWriteSee.sh`
   - Bash runner
   - calls `CombinedFiles_after_inspection.R`
9. `7.RunOptimization.sh`
   - Bash runner
   - calls `saveROIs.ijm`
10. `0.ReadROIdata-to-optimize.R`
11. `1.OptimizeNeuriteParams-MAJOR_OPTIMIZE.R`

## Workflow decision logic

### Step 1: check whether saved validation cutoff outputs already exist
The first strategic decision is no longer "run everything again." It is:
- are there saved validation cutoff outputs already available?

Those saved outputs are intended to live in:
- `cache/validation_profiles/`

They can contain:
- manually inspected grouping tables
- ROI optimization results
- filtered best-parameter outputs
- supporting ROI summaries

### If no saved validation cutoff outputs exist
Use the full pipeline logic:
- validation with test images
- generalization with new images
- output plotting for visualization and interpretation

In practice that means:
1. run the validation and optimization chain
2. save the resulting cutoff profile if it is trusted
3. continue into generalized production analysis
4. finish with the visualization and interpretation module

### If saved validation cutoff outputs already exist
The frontend and documentation should present two choices:
- run a fresh validation again
- reuse a saved validation cutoff profile

#### Option A: run a fresh validation again
Follow the same full logic:
- validation with test images
- generalization with new images
- output plotting for visualization and interpretation

#### Option B: reuse a saved validation cutoff profile
The user should choose a saved profile by date or profile folder name, then continue with:
- generalization with new images
- output plotting for visualization and interpretation

### Production and downstream analysis chain
Once a good saved validation cutoff profile exists, later image analysis should not always require repeating manual optimization. That is why the project now distinguishes:
- validation and optimization mode
- production analysis mode with profile reuse

Primary supported production chain in the current browser app:
- Tab `3.2. Analysis Specification and Export`
- `3.Apply_cutoff_profile_to_all_images.R`
- `pipeline_outputs/OUT_generalized_analysis/generalized_model_feature_table.csv`
- `pipeline_outputs/OUT_generalized_analysis/generalized_model_feature_table.rds`

This table-based branch is the main handoff into tab 4 visualizations, PCA,
feature screening, grouped dot/bar plots, covariate checks, and ANCOVA.
The statistical assumptions, row-level interpretation, and recommended order of
analysis are described in `docs/STATISTICAL_INTERPRETATION.md`.

Legacy/supplemental production interpretation scripts are still available for
reproducibility:
- `2.1.Generalize_image_threshold_skeleton_rule.R`
- `3.1.analyzed-generalized-skeleton-data-PCA+ALMOST_WORKING.R`
- `4.1.Skeleton-cutoff-ploting-improved-polished+BG.R`

### Optimization diagnostics and support branch
These scripts are useful after image analysis outputs exist, but they are best understood as optimization-support and diagnostic tools rather than the default endpoint for a new user.

- `2.roi_distribution_analysis.R`
  - reads ROI-level raw analysis outputs and checks distribution structure, candidate ranges, and sensitivity
- `3.optimized_grid_search.R`
  - expands the optimization search and writes broader optimization landscape results
- `3.analysis-check-visualize-readiness-on-test-images2PCA.R`
  - supports ROI-level separation and threshold interpretation for signal versus noise
- `3.1.analysis-check-skeletons-readiness-on-test-images2PCA.R`
  - does a similar readiness and separability check on skeleton-derived features

### Visualization and interpretation module
This module sits after image-analysis outputs have already been generated and is meant to help the user understand what the pipeline produced, whether the learned thresholds behave sensibly, and how the final biological conclusions are presented.

Current browser app interpretation tabs:
- `4.0. Training Image Audit`
- `4.1. Global Structure: PCA and Separation`
- `4.2. Feature Screening by Contrast`
- `4.3. Visual Inspection: Grouped Dot and Bar Plot`
- `4.4. Covariate Correlation Check`
- `4.5. Adjusted Effect Modeling / ANCOVA`
- `4.6. Backend Visualization Scripts`

Current reusable feature table includes neurite/cell/background mask area
features, retained ROI/skeleton metrics, and composite normalization features
such as `image_total_skeleton_length_per_confluence`.

Upstream analysis objects that feed this module:
- `1.analyze-combine-ROIs-skeletons.R`
- `2.analyze-combine-ROIs-skeletons.R`

Preferred visualization and interpretation scripts:
- `3.analyze-visualize-check.R`
  - preferred ROI visualization endpoint for representative before/after views
- `3.analysis-check-visualize-readiness-on-test-images2.R`
  - preferred richer ROI visualization plus PCA interpretation endpoint
- `3.1.analyzed-generalized-skeleton-data-PCA+ALMOST_WORKING.R`
  - primary production interpretation script for metadata-driven PCA and condition structure
- `4.1.Skeleton-cutoff-ploting-improved-polished+BG.R`
  - final reporting endpoint with polished biological effect plots

Earlier but still useful supporting visual script:
- `2.analyze-visualize-check.R`
  - earlier cutoff-comparison visualization helper

### Reproducible report module
The browser app also contains tab `5. Reproducible Report`, which writes a
downloadable `.tar.gz` archive containing:
- `report.html`
- `RECREATE_ANALYSIS.txt`
- settings and environment tables
- copied active scripts/macros
- run logs and command history
- selected validation profile metadata
- key CSV/RDS outputs
- generated plots and reports

Large image and ROI folders are optional because they can make the archive very
large. Raw data and trained ilastik models should be deposited externally and
linked from `README.md`.

Authoritative file-choice record:
- `docs/ACTIVE_SCRIPT_CHOICES.md`

## Validation cutoff profiles
Reusable validated cutoff artifacts should now be treated as the primary reusable cache under:
- `cache/validation_profiles/`

Legacy location still supported for backward compatibility:
- `cache/optimization_profiles/`

Helper scripts:
- `launchers/save_optimization_profile.sh`
- `launchers/apply_optimization_profile.sh`

This prepares the project for a frontend choice between:
- run fresh validation and optimization
- reuse a previous validation cutoff profile

## Software the pipeline depends on
- Bash
- R and `Rscript`
- Fiji / ImageJ
- ilastik
- ImageMagick `convert` for mask renormalization
- Java for Fiji / ImageJ

See:
- `docs/SOFTWARE_SETUP.md`

## Important current design choice
The active numbered scripts remain in the project root so their relative-path logic keeps working. The project is organized around them rather than moving them prematurely.
