# Active Script Choices

This file resolves ambiguity by choosing active files only from the user-approved candidate list.

## Primary supported workflow

### Bash entry chain
- `0.Rename.sh`
- `1.get_img_list.sh`
- `2.RunFIJI_clean_bckgnd.sh`
- `3.Run_ilastik_model.sh`
- `4.GetArea%Fiji.sh`
- `5.SelectValidationImgGroups.sh`
- `6.SelectedValidationImgWriteSee.sh`
- `7.RunOptimization.sh`

### Core optimization R chain
- `0.ReadROIdata-to-optimize.R`
- `1.OptimizeNeuriteParams-MAJOR_OPTIMIZE.R`

### Core production R chain
- `2.1.Generalize_image_threshold_skeleton_rule.R`
- `3.1.analyzed-generalized-skeleton-data-PCA+ALMOST_WORKING.R`
- `4.1.Skeleton-cutoff-ploting-improved-polished+BG.R`

## Secondary but still allowed files from the approved list
These are treated as secondary, supporting, or exploratory analysis tools rather than the main production path.

- `1.1.get_all_sample_Skeletons_in_one_file.R`
- `1.1.OptimizeNeuriteParams-MAJOR_OPTIMIZE-Skeletons-Only.R`
- `1.analyze-combine-ROIs-skeletons.R`
- `1.get_all_sample_ROIs_in_one_file.R`
- `2.analyze-combine-ROIs-skeletons.R`
- `2.analyze-visualize-check.R`
- `2.roi_distribution_analysis.R`
- `3.1.analysis-check-skeletons-readiness-on-test-images2PCA.R`
- `3.analysis-check-visualize-readiness-on-test-images.R`
- `3.analysis-check-visualize-readiness-on-test-images2.R`
- `3.analysis-check-visualize-readiness-on-test-images2PCA.R`
- `3.analyze-visualize-check.R`
- `3.optimized_grid_search.R`

## Chosen file when duplicate naming existed
- Chosen active ROI collector: `1.get_all_sample_ROIs_in_one_file.R`
- The unnumbered `get_all_sample_ROIs_in_one_file.R` is treated as non-primary.

## Chosen file when multiple similarly named analysis files existed
- Preferred combined ROI+skeleton analysis: `2.analyze-combine-ROIs-skeletons.R`
- Preferred biological reporting endpoint: `4.1.Skeleton-cutoff-ploting-improved-polished+BG.R`
- Preferred ROI visualization endpoint: `3.analyze-visualize-check.R`
- Preferred ROI visualization plus PCA endpoint: `3.analysis-check-visualize-readiness-on-test-images2.R`

## Rationale for these choices
- If one file was clearly the later maintained variant in the working tree, prefer that one.
- If one file had visibly broader functionality without introducing extra conflict risk, prefer that one.
- If two files looked like near-duplicates, do not merge them automatically unless the merge is obviously safe.

### Concrete examples
- `1.get_all_sample_ROIs_in_one_file.R` was preferred over `get_all_sample_ROIs_in_one_file.R`
  - because it is treated as the numbered pipeline version and is now part of the maintained path
- `2.analyze-combine-ROIs-skeletons.R` was preferred over `1.analyze-combine-ROIs-skeletons.R`
  - because it appears to be the richer later-stage version with checkpoint handling and broader ROI+skeleton analysis flow
- `4.1.Skeleton-cutoff-ploting-improved-polished+BG.R` was preferred over the archived `...second...` and `...WORKING...` variants
  - because it is the maintained active version in the main tree and already contains the polished reporting endpoint
- `3.analyze-visualize-check.R` was preferred over the earlier similarly named ROI visualization files
  - because it is the richer fixed visualization script and works naturally downstream of the combined ROI analysis outputs
- `3.analysis-check-visualize-readiness-on-test-images2.R` was preferred over `3.analysis-check-visualize-readiness-on-test-images.R`
  - because it adds PCA-based interpretation on top of the class-comparison visual outputs without changing the same input-output role
- `3.1.analyzed-generalized-skeleton-data-PCA+ALMOST_WORKING.R`
  - remains primary only because it is the approved candidate that best matches the intended production PCA metadata stage
  - but its title still signals historical uncertainty, so it should be considered a later cleanup target

## Functional relationship notes for the previously uncertain analysis files

### Optimization diagnostics and support
- `2.roi_distribution_analysis.R`
  - reads raw ROI feature analysis
  - produces parameter-range and sensitivity diagnostics
  - should be treated as optimization-support, not as the main production endpoint
- `3.optimized_grid_search.R`
  - builds on the ROI distribution diagnostics
  - performs broader grid-search optimization and writes optimization landscape outputs
- `3.analysis-check-visualize-readiness-on-test-images2PCA.R`
  - despite its name, this behaves more like ROI parameter optimization and threshold finding than like a pure presentation script
- `3.1.analysis-check-skeletons-readiness-on-test-images2PCA.R`
  - is the skeleton-side equivalent: optimization support and separability diagnostics for signal versus noise skeleton features

### Visualization and interpretation module
- `1.analyze-combine-ROIs-skeletons.R`
  - creates the earlier combined ROI analysis object used by several downstream visualization scripts
- `2.analyze-combine-ROIs-skeletons.R`
  - is the preferred richer combined ROI+skeleton analysis upstream
- `2.analyze-visualize-check.R`
  - is an earlier ROI visualization helper that checks the visual effect of parameter cutoffs
- `3.analyze-visualize-check.R`
  - is the preferred ROI visualization endpoint for representative before/after category views
- `3.analysis-check-visualize-readiness-on-test-images2.R`
  - is the preferred richer ROI visualization-and-PCA interpretation script
- `3.1.analyzed-generalized-skeleton-data-PCA+ALMOST_WORKING.R`
  - is the primary production interpretation script for image-level feature structure and metadata-driven PCA
- `4.1.Skeleton-cutoff-ploting-improved-polished+BG.R`
  - is the final biological reporting endpoint with polished treatment-effect plots

## Merge policy
- Merge only when the newer file clearly omits a useful block from an older near-duplicate file and that block can be ported without changing the role of the script.
- Otherwise, keep one primary file and treat the others as secondary references to avoid hidden conflicts.

## Not treated as primary workflow
- `4.analysis-check-visualize-readiness-on-test-images.R`
  - currently appears empty and is not part of the supported workflow
