# PhaseContrastNeuriteAnalyzer

Browser-guided pipeline for neurite analysis in phase-contrast microscopy images.
The app orchestrates Bash, Fiji/ImageJ macros, ilastik pixel classification, R
optimization scripts, production cutoff application, and downstream statistical
visualization.

Repository target:
https://github.com/YordanovLab/PhaseContrastNeuriteAnalyzer

## What the tool does

PhaseContrastNeuriteAnalyzer turns nested raw phase-contrast image folders into
reproducible neurite readouts:

1. Renames image inputs into unique hard-linked names.
2. Builds a fixed image universe in `OUT_img_input1.txt`.
3. Uses Fiji/ImageJ to create red-green averaged intensity images.
4. Uses an ilastik pixel-classification project to create segmentation masks.
5. Quantifies mask labels for neurites, cell bodies, and background.
6. Creates validation image groups for human review.
7. Extracts ROIs and skeleton summaries with Fiji.
8. Optimizes neurite ROI cutoffs with classic, continuity-aware, topology-aware,
   or ensemble scoring modes.
9. Saves reusable validated cutoff profiles.
10. Applies selected cutoff profile(s) to all production images.
11. Provides PCA, feature screening, grouped plots, covariate checks, ANCOVA, and
    reproducibility reports in the browser app.

## Repository layout

```text
.
  app.R                                      # Shiny browser app
  0.Rename.sh ... 7.RunOptimization.sh       # backend step launchers
  *.R                                       # analysis, optimization, reporting scripts
  *.ijm                                     # Fiji/ImageJ macros
  launchers/                                # app/profile helper scripts
  config/
    pipeline_manifest.json
    pipeline_settings.example.env           # copy to pipeline_settings.env
  docs/                                     # workflow and setup documentation
  frontend/                                 # static synchronized help page
```

Generated folders such as `pipeline_inputs/`, `pipeline_outputs/`, `cache/`,
`PRE_renamed/`, and trained ilastik model folders are ignored by git.

## Large datasets and trained models

Do not upload raw images or large trained model/output folders to GitHub.

The raw images, metadata, ilastik model, ilastik training examples, and compact
representative outputs are archived on Zenodo:

- Dataset DOI: https://doi.org/10.5281/zenodo.19634700
- Dataset record and download page: https://zenodo.org/records/19634700
- Trained ilastik model: included in the same Zenodo record under `ilastik/model/`

Zenodo is recommended because it provides persistent DOIs and supports records up
to 50 GB. Figshare and OSF are also possible; see
[`docs/DATA_AVAILABILITY.md`](docs/DATA_AVAILABILITY.md).

## Restoring the Zenodo Dataset into a Cloned Repository

After cloning the GitHub repository and downloading the Zenodo archive, copy the
dataset contents into the repository as follows. The paths below are relative to
the cloned `PhaseContrastNeuriteAnalyzer/` folder.

```text
Zenodo archive folder                         Repository destination
raw_images/*                                  pipeline_inputs/
metadata/IN_sample_metadata_zenodo_paths.csv  pipeline_inputs/IN_sample_metadata.csv
ilastik/model/*.ilp                           models/
ilastik/training_images_raw/                  example_data/ilastik_training/raw/
ilastik/training_images_preprocessed/         example_data/ilastik_training/preprocessed/
ilastik/example_expected_outputs/             example_data/ilastik_training/expected_outputs/
example_outputs/                              example_data/example_outputs/
```

The restored `pipeline_inputs/` folder should contain the nested raw image
folders directly, not an additional wrapper folder such as
`pipeline_inputs/raw_images/`. After copying, paths should look like:

```text
pipeline_inputs/1.08-smqna_na_sredite/...
pipeline_inputs/22.07-MTT/...
pipeline_inputs/IN_sample_metadata.csv
```

Novice-friendly fallback:

It is also supported to unzip the Zenodo archive directly inside
`pipeline_inputs/`. In that case the folder may look like:

```text
pipeline_inputs/raw_images/
pipeline_inputs/metadata/
pipeline_inputs/ilastik/
pipeline_inputs/example_outputs/
```

The app and shell launchers automatically detect this layout. They use
`pipeline_inputs/raw_images/` as the effective raw-image root,
`pipeline_inputs/metadata/IN_sample_metadata_zenodo_paths.csv` as the metadata
fallback, and `pipeline_inputs/ilastik/model/*.ilp` as the ilastik project
fallback if `models/Ilastik1_probably_working.ilp` is not present.

Using your own images and model:

You can use your own compatible data instead of the Zenodo archive. Put raw image
folders under `pipeline_inputs/` or choose another master input directory in the
Configuration tab. Provide a metadata CSV with an `image_name` column. The
expected value is the path-flattened rename output, for example:

```text
pipeline_inputs/2026-04-17_SHSY5Y/RosAc_10uM/plate1/wellB01_img001.tif
-> 2026-04-17_SHSY5Y-RosAc_10uM-plate1-wellB01_img001.tif
```

Recommended custom folder structure:

```text
pipeline_inputs/
  experiment_or_date/
    condition_or_treatment/
      plate_or_well_group/
        image_001.tif
        image_002.tif
```

Minimal metadata example:

```csv
image_name,first_differentiating_agent,first_agent_concentration(micromolar),days_after_seeding,FBS_percentage,phorbol_ester,intended_assay,unique_well_id
2026-04-17_SHSY5Y-CTRL-plate1-wellA01_img001.tif,None,0,2,10,FALSE,morphology,plate1_A01
2026-04-17_SHSY5Y-RosAc_10uM-plate1-wellB01_img001.tif,RosAc,10,2,10,FALSE,morphology,plate1_B01
```

Only `image_name` is strictly required for filename matching, but treatment,
concentration, day, FBS, assay, and well/sample identifiers are strongly
recommended for downstream plots and statistics. See
[`pipeline_inputs/README.md`](pipeline_inputs/README.md) for the full input and
metadata contract.

Provide a trained ilastik Pixel Classification `.ilp` project compatible with
the Fiji-preprocessed images and with classes for background, cell bodies, and
neurites. Put it in `models/`, put it in `pipeline_inputs/ilastik/model/`, or
select it in the Configuration tab. After segmentation, use the in-app
segmentation label-mapping check before quantifying mask areas.

Then create local settings:

```bash
cp config/pipeline_settings.example.env config/pipeline_settings.env
```

Edit `config/pipeline_settings.env` so the key paths point to the restored data:

```bash
INPUT_MASTER_DIR="./pipeline_inputs"
SAMPLE_METADATA_FILE="./pipeline_inputs/IN_sample_metadata.csv"
ILASTIK_PROJECT_FILE="./models/Ilastik1_probably_working.ilp"
```

The Zenodo metadata column `zenodo_raw_relative_path` is useful for auditing the
archive. During analysis, the app primarily uses `image_name` plus the selected
input root to reproduce the pipeline image naming.

## Quick start

### 1. Clone the repository

```bash
git clone https://github.com/YordanovLab/PhaseContrastNeuriteAnalyzer.git
cd PhaseContrastNeuriteAnalyzer
```

### 2. Create local settings

```bash
cp config/pipeline_settings.example.env config/pipeline_settings.env
```

Edit `config/pipeline_settings.env` and set:

- `FIJI_BIN`
- `ILASTIK_BIN`
- `ILASTIK_PROJECT_FILE`
- `SAMPLE_METADATA_FILE`
- `INPUT_MASTER_DIR`

Keep `PROJECT_ROOT="."` unless you intentionally moved the whole project.

### 3. Install R packages

Install R and then install the common packages used by the app and scripts:

```r
install.packages(c(
  "shiny", "dplyr", "readr", "ggplot2", "tidyr", "stringr",
  "purrr", "R.utils", "magick", "png"
))
```

Some optional analysis branches may require additional packages such as
`uwot`, `Rtsne`, `pheatmap`, or `emmeans`. The app includes a software/package
inspection table to show what is missing in your local environment.

### 4. Start the app

Linux or WSL:

```bash
bash launchers/start_pipeline.sh
```

Windows:

```text
Double-click START_PIPELINE_Windows.bat
```

Then open:

```text
http://127.0.0.1:3838
```

## Recommended runtime

The backend is Bash-first and is most naturally run on Linux or WSL.

For Windows users with WSL:

- Prefer running Bash, R, Fiji, and ilastik inside WSL.
- Use Linux-style paths in `config/pipeline_settings.env`.
- Enable WSL GUI support before running Fiji-based steps.
- Use Linux/WSL Fiji and ilastik builds rather than Windows `.exe` binaries for
  the backend scripts.
- Install ImageMagick in WSL/native Linux so ilastik mask renormalization can
  use either `magick` or `convert`.
- Fiji GUI-capable mode is preferred because headless Fiji can sometimes produce
  different outputs for ImageJ macro workflows.

## Installing Fiji/ImageJ

1. Download Fiji from https://imagej.net/software/fiji/downloads
2. Extract it into a stable folder, for example:

```text
/opt/imganalys/FIJI/Fiji.app/
```

3. Set `FIJI_BIN` to the executable, for example:

```text
FIJI_BIN="/opt/imganalys/FIJI/Fiji.app/fiji-linux-x64"
```

4. Open Fiji once interactively and install/update required plugins. For ROI and
   skeleton analysis, make sure standard ImageJ/Fiji morphology and skeleton
   tools are available.

## Installing ilastik

1. Download ilastik from https://www.ilastik.org/download.html
2. Extract it into a stable folder, for example:

```text
/opt/imganalys/ilastik-1.4.2rc1-gpu-Linux/
```

3. Set `ILASTIK_BIN` to:

```text
ILASTIK_BIN="/opt/imganalys/ilastik-1.4.2rc1-gpu-Linux/run_ilastik.sh"
```

4. Store your trained `.ilp` project outside git or download it from the linked
   dataset/model archive, then set:

```text
ILASTIK_PROJECT_FILE="models/Ilastik1_probably_working.ilp"
```

## Training the ilastik pixel classifier

The segmentation model should classify the same image type that will later be
processed by the app. This pipeline first creates Fiji background-corrected or
red-green averaged images, so ilastik should be trained on images produced with
the same Fiji preprocessing logic.

### Recommended training logic

1. Put representative raw phase-contrast/RGB images in the input workspace.
2. Run the app setup through Fiji preprocessing:
   `2.A.1.0` to `2.A.1.2`.
3. Use the generated `OUT_clear_bcgnd/*_RGavg.tif` images as ilastik training
   inputs.
4. In ilastik, create a **Pixel Classification** project.
5. Add three semantic labels:
   - `background`
   - `cell_bodies`
   - `neurites`
6. Paint examples for all three classes on multiple images:
   - Background: clear non-cell area, illumination artefacts, and empty regions.
   - Cell bodies: soma/cell-body regions, dense cell clusters, rounded bodies.
   - Neurites: thin elongated projections, branching neurite-like structures,
     and long interrupted neurite segments when biologically plausible.
7. Train with representative variation:
   - low and high confluence
   - low and high neurite density
   - different treatments/concentrations/days
   - imaging noise and uneven background examples
8. Export or save the ilastik `.ilp` project.
9. Configure `ILASTIK_PROJECT_FILE` in `config/pipeline_settings.env`.
10. In the app, use `2.A.2. Segmentation Label Check` to confirm that the mask
    labels match the downstream mapping:
    - cell bodies = `1`
    - neurites = `3`
    - background = `255`

If ilastik outputs labels in a different order, use the app label-check module to
map the detected peaks to the expected biological classes before mask-area
quantification.

## Main app workflow

### 1. Configuration

Set paths, metadata CSV, raw image input directory, Fiji executable, ilastik
executable, ilastik project file, and output workspace.

### 2. Validate New / Use Prior Cutoffs Model

Choose either:

- run a fresh validation using test images, or
- reuse a saved validated cutoff profile.

Fresh validation includes:

- setup and preprocessing
- segmentation label check
- validation group creation
- manual inspection
- ROI/skeleton extraction
- threshold optimization
- cutoff model inspection

### 3. Analyze Test Images

Apply one or more selected cutoff profiles to the full image set and export the
generalized model-feature table for downstream analysis.

### 4. Visualization and Interpretation

Includes:

- training-image audit
- PCA and separation diagnostics
- feature screening by contrast
- grouped dot/bar plots
- covariate correlation checks
- ANCOVA/adjusted effect modeling
- legacy backend visualization script cards

For the statistical assumptions and recommended interpretation order, see
[`docs/STATISTICAL_INTERPRETATION.md`](docs/STATISTICAL_INTERPRETATION.md).

### 5. Reproducible Report

Creates a downloadable `.tar.gz` archive containing:

- `report.html`
- `RECREATE_ANALYSIS.txt`
- settings, scripts, logs, selected profiles, key tables/RDS files, and plots
- optional large image/ROI folders if explicitly selected

## Important generated outputs

```text
pipeline_outputs/
  PRE_renamed/
  OUT_img_input1.txt
  OUT_clear_bcgnd/
  OUT_segmentation/
  OUT_optimize/
  OUT_generalized_analysis/

cache/
  validation_profiles/
  run_logs/
  analysis_reports/
```

These are ignored by git.

## Reproducibility checklist before publication

1. Confirm `.gitignore` excludes raw images, masks, ROIs, model files, outputs,
   logs, and local settings.
2. Upload raw data and trained ilastik model to Zenodo/Figshare/OSF.
3. Replace dataset/model placeholders in this README with persistent URLs/DOIs.
4. Run the app `5. Reproducible Report` tab and archive the generated report.
5. Commit only source code, docs, examples, and lightweight configuration.

## License and Citation

The software code in this repository is released under the MIT License. This
allows reuse, modification, redistribution, and adaptation with minimal
restriction, provided that the copyright and license notice are retained.

Scholarly citation is requested. If you use this repository, the associated
dataset, the trained ilastik model, or results generated by this workflow,
please cite:

1. The GitHub software repository:
   `https://github.com/YordanovLab/PhaseContrastNeuriteAnalyzer`
2. The Zenodo dataset:
   `https://doi.org/10.5281/zenodo.19634700`
3. The related Pharmacia publication when available:
   "Morphological evaluation of the differentiation status of SH-SY5Y cells
   upon rosmarinic and retinoic acid treatments"

GitHub should automatically expose citation information from
[`CITATION.cff`](CITATION.cff). The Zenodo dataset is governed by the license
selected on the Zenodo record.

## Authors, Affiliations, and Funding

Yordan Yordanov:

- Department of Pharmacology, Pharmacotherapy and Toxicology, Faculty of
  Pharmacy, Medical University - Sofia, Bulgaria
- Research Institute of Innovative Medical Science, Medical University - Sofia,
  Bulgaria

Siyana Hristova:

- Department of Pharmacology, Pharmacotherapy and Toxicology, Faculty of
  Pharmacy, Medical University - Sofia, Bulgaria

Funding:

This study was funded by the European Union - NextGenerationEU through the
National Recovery and Resilience Plan of the Republic of Bulgaria, project No.
BG-RRP-2.004-0004-C01.
