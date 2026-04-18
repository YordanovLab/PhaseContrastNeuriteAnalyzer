# Software Setup

PhaseContrastNeuriteAnalyzer is easiest to run on Linux or Windows with WSL.
The browser interface is a Shiny app, but the scientific backend uses Bash,
Fiji/ImageJ, ilastik, ImageJ macros, and R scripts.

## Required software

- R with `Rscript`
- R package `shiny`
- Bash
- Fiji / ImageJ
- ilastik
- Java, if required by your Fiji distribution
- ImageMagick command-line tools (`magick` or `convert`) when ilastik mask renormalization is enabled
- Optional but useful: R package `magick` for TIFF previews

## Recommended runtime by operating system

### Linux

Install all tools natively and edit `config/pipeline_settings.env`.
Use normal POSIX paths such as `/home/user/...` or `/opt/imganalys/...`.

### Windows with WSL

Recommended for this project:

- install R, Bash tools, Fiji, and ilastik inside WSL
- keep paths in Linux/WSL format such as `/mnt/c/...` or `/opt/...`
- enable WSL GUI support before running Fiji-based steps
- avoid using Windows `.exe` Fiji/ilastik binaries from the WSL backend; use the Linux/WSL distributions of Fiji and ilastik for reproducible macro/batch behavior

Fiji GUI-capable mode is preferred because some headless ImageJ/Fiji runs can
produce subtly different outputs for macro workflows.

The shared settings loader can convert simple `C:/...` paths to WSL paths when
`wslpath` is available, but the recommended configuration is still to save
Linux/WSL paths directly.

### Native Windows only

This is possible but less tested. You would need Windows-native R, Fiji, and
ilastik paths and may need to adapt Bash-dependent launchers.

## Install R and R packages

Install R from:

- https://cran.r-project.org/

Then install common packages:

```r
install.packages(c(
  "shiny", "dplyr", "readr", "ggplot2", "tidyr", "stringr",
  "purrr", "R.utils", "magick", "png"
))
```

Optional analysis branches may also use packages such as:

```r
install.packages(c("uwot", "Rtsne", "pheatmap", "emmeans"))
```

Use the app configuration/software tables to check which packages are detected.

Install ImageMagick for mask renormalization:

```bash
sudo apt update
sudo apt install imagemagick
```

## Install Fiji / ImageJ

Download Fiji:

- https://imagej.net/software/fiji/downloads

Example Linux/WSL layout:

```text
/opt/imganalys/FIJI/Fiji.app/
```

Example setting:

```text
FIJI_BIN="/opt/imganalys/FIJI/Fiji.app/fiji-linux-x64"
```

Open Fiji once interactively, update it, and confirm ImageJ macro execution
works. The pipeline uses Fiji for:

- red-green image preprocessing
- mask area quantification
- ROI extraction
- skeleton/ROI summary generation

## Install ilastik

Download ilastik:

- https://www.ilastik.org/download.html

Example Linux/WSL layout:

```text
/opt/imganalys/ilastik-1.4.2rc1-gpu-Linux/
```

Example setting:

```text
ILASTIK_BIN="/opt/imganalys/ilastik-1.4.2rc1-gpu-Linux/run_ilastik.sh"
```

Keep trained `.ilp` project files outside git or download them from a linked
dataset/model archive.

## Train the ilastik pixel classifier

The ilastik project should be trained on the same image type used at production
time. In this workflow, raw images are first transformed by Fiji preprocessing,
so train ilastik on the Fiji-generated `*_RGavg.tif` images.

Recommended training procedure:

1. Run the app setup through Fiji preprocessing.
2. Open ilastik and create a Pixel Classification project.
3. Add preprocessed images from `pipeline_outputs/OUT_clear_bcgnd/`.
4. Create three labels:
   - background
   - cell bodies
   - neurites
5. Paint examples across multiple representative images:
   - empty background and uneven illumination
   - rounded cell-body/soma regions
   - thin neurites, branches, long interrupted neurites
   - examples from low/high confluence and low/high neurite density
6. Train interactively until predictions are stable.
7. Save the `.ilp` project.
8. Set `ILASTIK_PROJECT_FILE` in `config/pipeline_settings.env`.
9. Run the app segmentation label check before validation grouping.

The app expects downstream labels to map to:

- cell bodies = `1`
- neurites = `3`
- background = `255`

If ilastik produces a different label order, use the app label-mapping check to
assign the detected peaks to the biological classes before quantification.

## Local settings

Copy:

```bash
cp config/pipeline_settings.example.env config/pipeline_settings.env
```

Then edit:

- `FIJI_BIN`
- `ILASTIK_BIN`
- `ILASTIK_PROJECT_FILE`
- `SAMPLE_METADATA_FILE`
- `INPUT_MASTER_DIR`

Do not commit `config/pipeline_settings.env`; it is intentionally ignored.
