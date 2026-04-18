# Data and Model Availability

This repository is intentionally code-first. Do not commit raw microscopy images,
large TIFF stacks, ROI payload folders, ilastik project files, or generated
pipeline outputs to GitHub.

## Archived dataset

The raw images, metadata, ilastik model, ilastik training examples, and compact
representative outputs are archived on Zenodo:

- Dataset DOI: https://doi.org/10.5281/zenodo.19634700
- Dataset record and download page: https://zenodo.org/records/19634700
- Trained ilastik model: included in the same Zenodo record under `ilastik/model/`

## Recommended place for future large dataset versions

For future microscopy dataset versions around several GB, the recommended public
archive is:

- Zenodo: https://zenodo.org/

Zenodo is appropriate because it issues DOIs and currently supports records up to
50 GB with up to 100 files. If the image dataset contains many files, package
them into one or a few ZIP/TAR archives before upload. After publication, place
the DOI and download link in `README.md`.

Good alternatives:

- Figshare: https://figshare.com/
- OSF: https://osf.io/

Figshare is also suitable for a 5 GB dataset and supports DOI-style scholarly
sharing. OSF can work for public projects, but single-file limits and project
storage caps require more care.

## Suggested external archive structure

Package external data like this:

```text
PhaseContrastNeuriteAnalyzer_data_v1/
  raw_images/
  metadata/
    IN_sample_metadata.csv
  ilastik_training/
    training_set_raw_before_preprocessing/
    trained_project/
      Ilastik1_probably_working.ilp
  example_outputs_optional/
  README_data.md
```

## Restoring the archive into the GitHub repository

After downloading the external dataset archive and cloning the repository, copy
the files into these repository-local destinations:

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

The destination `pipeline_inputs/` should contain the raw image subfolders
directly. Avoid creating an extra nested wrapper such as
`pipeline_inputs/raw_images/...`, because the app expects the selected input root
to be the directory that contains the image folders.

Fallback direct-unzip layout:

Users may also unzip the Zenodo archive directly into `pipeline_inputs/`. The
app and shell launchers detect this layout automatically:

```text
pipeline_inputs/raw_images/       -> effective raw-image root
pipeline_inputs/metadata/         -> metadata fallback
pipeline_inputs/ilastik/model/    -> ilastik project fallback
```

For custom datasets, structure raw images so each path can be flattened into a
unique metadata filename:

```text
pipeline_inputs/experiment_or_date/condition_or_treatment/plate_or_well/image_001.tif
```

The matching metadata `image_name` should be:

```text
experiment_or_date-condition_or_treatment-plate_or_well-image_001.tif
```

At minimum, custom metadata must contain `image_name`. Recommended additional
columns include treatment, concentration, days after seeding, FBS percentage,
phorbol ester status, assay name, and unique well/sample identifier.

Then copy:

```text
config/pipeline_settings.example.env -> config/pipeline_settings.env
```

and set at minimum:

```text
INPUT_MASTER_DIR="./pipeline_inputs"
SAMPLE_METADATA_FILE="./pipeline_inputs/IN_sample_metadata.csv"
ILASTIK_PROJECT_FILE="./models/Ilastik1_probably_working.ilp"
```

## What belongs in GitHub versus the data archive

Keep in GitHub:

- Shiny app and launcher scripts
- Bash, R, and ImageJ macro source files
- documentation
- small example metadata templates
- configuration examples

Keep in Zenodo/Figshare/OSF:

- raw phase-contrast image datasets
- trained ilastik `.ilp` models
- large masks and ROI folders
- generated output archives
- validation profile archives, if large

## GitHub README links

The GitHub README should point to:

- https://doi.org/10.5281/zenodo.19634700
- https://zenodo.org/records/19634700
