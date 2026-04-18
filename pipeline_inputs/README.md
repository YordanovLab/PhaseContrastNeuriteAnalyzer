# Pipeline Inputs

This folder is intentionally kept in the GitHub repository as an empty landing
place for downloaded datasets. Raw microscopy images and local metadata are not
committed to git.

## Restore the Zenodo dataset here

Download the dataset from Zenodo:

- DOI: https://doi.org/10.5281/zenodo.19634700
- Record: https://zenodo.org/records/19634700

After extracting the Zenodo archive, copy the contents of its `raw_images/`
folder directly into this `pipeline_inputs/` folder.

Simple fallback for novice users:

You may also unzip the Zenodo archive directly inside this `pipeline_inputs/`
folder. If the app sees this structure:

```text
pipeline_inputs/
  raw_images/
  metadata/
  ilastik/
  example_outputs/
```

it will automatically use `pipeline_inputs/raw_images/` as the image root,
`pipeline_inputs/metadata/IN_sample_metadata_zenodo_paths.csv` as the metadata
file, and `pipeline_inputs/ilastik/model/*.ilp` as the fallback ilastik project
file if no model is found in `models/`.

The result should look like this:

```text
pipeline_inputs/
  README.md
  IN_sample_metadata.csv
  1.08-smqna_na_sredite/
  22.07-MTT/
  25.07_MTT-SMQNAnaSR_RoS_RA/
  ...
```

Do not copy the archive as `pipeline_inputs/raw_images/...`; copy the contents
of `raw_images/` into `pipeline_inputs/`.

Then copy:

```text
metadata/IN_sample_metadata_zenodo_paths.csv
```

from the Zenodo archive to:

```text
pipeline_inputs/IN_sample_metadata.csv
```

Finally, update `config/pipeline_settings.env` so it contains:

```text
INPUT_MASTER_DIR="./pipeline_inputs"
SAMPLE_METADATA_FILE="./pipeline_inputs/IN_sample_metadata.csv"
```

## Using your own images and model

You can use your own compatible dataset instead of the Zenodo dataset.

### Recommended raw image folder structure

Put raw image folders here. The pipeline accepts nested folders, and the folder
names become part of the unique image name. This is useful because microscopy
experiments often reuse simple filenames such as `img01.tif` in many folders.

Recommended structure:

```text
pipeline_inputs/
  experiment_or_date/
    condition_or_treatment/
      plate_or_well_group/
        image_001.tif
        image_002.tif
```

Concrete example:

```text
pipeline_inputs/
  2026-04-17_SHSY5Y/
    CTRL/
      plate1/
        wellA01_img001.tif
        wellA02_img002.tif
    RosAc_10uM/
      plate1/
        wellB01_img001.tif
    RetAc_1uM/
      plate1/
        wellC01_img001.tif
```

Use only raw input images here. Do not put Fiji-preprocessed images, ilastik
masks, ROI files, or previous `pipeline_outputs/` folders inside the image input
tree.

Supported image extensions include `.tif`, `.tiff`, `.png`, `.jpg`, and `.jpeg`.
TIFF is preferred for microscopy.

### How filenames are matched to metadata

The first pipeline step creates a unique filename by flattening the path from the
selected input root to the image file. Folder separators become hyphens.

Example:

```text
Raw image path under pipeline_inputs:
2026-04-17_SHSY5Y/RosAc_10uM/plate1/wellB01_img001.tif

Expected metadata image_name:
2026-04-17_SHSY5Y-RosAc_10uM-plate1-wellB01_img001.tif
```

If the same filename appears in different folders, this path-flattening keeps the
images unique.

Avoid `/` and `\` in metadata `image_name` values. Use the flattened name with
hyphens exactly as shown above.

### Required metadata structure

Then provide a metadata CSV and select it in the app Configuration tab. The CSV
must contain an `image_name` column. The expected value is the path-flattened name
created by the rename step:

```text
my_experiment/plate_1/image_001.tif -> my_experiment-plate_1-image_001.tif
```

Minimal metadata example:

```csv
image_name,first_differentiating_agent,first_agent_concentration(micromolar),days_after_seeding,FBS_percentage,phorbol_ester,intended_assay,unique_well_id
2026-04-17_SHSY5Y-CTRL-plate1-wellA01_img001.tif,None,0,2,10,FALSE,morphology,plate1_A01
2026-04-17_SHSY5Y-RosAc_10uM-plate1-wellB01_img001.tif,RosAc,10,2,10,FALSE,morphology,plate1_B01
2026-04-17_SHSY5Y-RetAc_1uM-plate1-wellC01_img001.tif,RetAc,1,2,10,FALSE,morphology,plate1_C01
```

Only `image_name` is strictly required for filename matching, but the other
columns are strongly recommended because they are used later for grouping,
visualization, PCA, covariate checks, and ANCOVA.

Recommended metadata columns:

- `image_name`: required; path-flattened filename matching the rename step.
- `first_differentiating_agent`: treatment or main differentiating agent, such
  as `None`, `RosAc`, or `RetAc`.
- `first_agent_concentration(micromolar)`: numeric concentration.
- `days_after_seeding`: numeric day/time variable.
- `FBS_percentage`: numeric FBS percentage.
- `phorbol_ester`: `TRUE` or `FALSE`, if relevant.
- `intended_assay`: assay or experiment category.
- `unique_well_id`: unique well/sample identifier if available.

Keep one row per raw image. If an image has no metadata except its filename,
still include a row with `image_name` so the app can track it.

For segmentation, provide a trained ilastik Pixel Classification `.ilp` project
compatible with the Fiji-preprocessed images and with classes for background,
cell bodies, and neurites. Put it at:

```text
pipeline_inputs/ilastik/model/your_model.ilp
```

or select the `.ilp` file in the app Configuration tab.
