# Pipeline Outputs

This folder is intentionally kept in the GitHub repository as an empty landing
place for generated analysis outputs. The real output files are not committed to
git because they can be large and are reproducible from the dataset and scripts.

The app writes intermediate and final outputs here by default.

Typical generated structure:

```text
pipeline_outputs/
  PRE_renamed/
  OUT_img_input1.txt
  OUT_clear_bcgnd/
  OUT_segmentation/
  OUT_optimize/
  OUT_generalized_analysis/
```

Important notes:

- `PRE_renamed/` contains renamed hard links or linked image inputs used by the
  workflow.
- `OUT_clear_bcgnd/` contains Fiji-preprocessed red/green averaged images.
- `OUT_segmentation/` contains ilastik segmentation masks.
- `OUT_optimize/` contains validation, ROI extraction, skeleton, and cutoff
  optimization outputs.
- `OUT_generalized_analysis/` contains the production image-level feature tables
  used by the visualization and interpretation tabs.

You can delete generated contents of this folder when starting a fully fresh
analysis, but do not delete this `README.md` if you want the folder to remain
visible in git.

