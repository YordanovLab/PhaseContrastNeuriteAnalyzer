const instructionData = {
  active_choices: {
    primary: {
      title: "Chosen active files: primary supported chain",
      subtitle: "These are the main files the frontend and docs should present first.",
      body: [
        "Primary supported optimization chain: 0.ReadROIdata-to-optimize.R and 1.OptimizeNeuriteParams-MAJOR_OPTIMIZE.R.",
        "Primary supported production chain: 2.1.Generalize_image_threshold_skeleton_rule.R, 3.1.analyzed-generalized-skeleton-data-PCA+ALMOST_WORKING.R, and 4.1.Skeleton-cutoff-ploting-improved-polished+BG.R.",
        "The Bash entry chain remains the numbered 0 through 7 scripts."
      ],
      reference: "#mode-module"
    },
    secondary: {
      title: "Chosen active files: secondary approved files",
      subtitle: "These files are still allowed from the approved list, but they are not the default path for a new user.",
      body: [
        "Secondary files remain available as supporting or exploratory analysis tools rather than the main production branch.",
        "The authoritative record of those choices now lives in docs/ACTIVE_SCRIPT_CHOICES.md."
      ],
      reference: "#mode-module"
    }
  },
  software_module: {
    detected: {
      title: "Software module: detected from the current session",
      subtitle: "This reflects what was visible from the current Windows-side shell session.",
      body: [
        "Visible from this session: bash and wsl were available on the Windows side.",
        "Not visible from this session: native Rscript and native java were not found, and the Linux-style Fiji and ilastik paths configured in the shared settings file were not visible from Windows.",
        "The project could not confirm the WSL-side installations because WSL access from this session returned an access-denied error."
      ],
      reference: "#software-module"
    },
    choice: {
      title: "Software module: Windows or WSL guidance",
      subtitle: "This is the environment-choice logic the frontend should expose clearly.",
      body: [
        "If you are on Windows with WSL, the current project is more naturally aligned with WSL or Linux-style execution because the entry scripts are Bash-first and the default Fiji and ilastik paths are Unix-style.",
        "So the frontend should ask a simple early question: do you want to run this project in Windows or in WSL? The recommended default is WSL when Bash, R, Fiji, and ilastik are installed there."
      ],
      reference: "#software-module"
    },
    install: {
      title: "Software module: installation guidance",
      subtitle: "This points users to the operating-system-specific setup help.",
      body: [
        "The installation guidance is now summarized in docs/SOFTWARE_SETUP.md with separate notes for Linux, Windows, and Windows with WSL.",
        "Whichever environment the user chooses, they should update config/pipeline_settings.env so the shared pipeline settings match the actual executable and project file locations."
      ],
      reference: "#software-module"
    },
    repro: {
      title: "Software module: reproducibility versions",
      subtitle: "This explains how the functional app now surfaces environment details for later GitHub reuse.",
      body: [
        "The functional app now includes a reproducibility section that lists the current software environment it can detect, including the running R session, Bash, WSL, Java, and the configured Fiji and ilastik paths.",
        "It also scans the project R scripts for referenced packages and reports which versions are currently installed in the running R session. This gives users a practical environment summary to save alongside results when sharing the project."
      ],
      reference: "#software-module"
    }
  },
  settings_module: {
    summary: {
      title: "Settings and required functionality",
      subtitle: "This is the architectural first module of the frontend.",
      body: [
        "The pipeline now has a shared settings file, config/pipeline_settings.env, which is the main place to confirm project root, common output folders, external executables, and key input filenames.",
        "This comes first because later scripts should not silently assume their own paths or outputs. The settings layer makes the project easier to understand and safer to rerun."
      ],
      reference: "#ref-settings"
    },
    critical: {
      title: "Settings module: critical fields",
      subtitle: "These are the fields the user should confirm before running the pipeline.",
      body: [
        "Confirm PROJECT_ROOT, PRE_RENAMED_DIR_NAME, IMAGE_INPUT_LIST_NAME, FIJI_BIN, ILASTIK_BIN, ILASTIK_PROJECT_FILE, OUT_CLEAR_BCGND_DIR_NAME, OUT_SEGMENTATION_DIR_NAME, OUT_OPTIMIZE_DIR_NAME, and SAMPLE_METADATA_FILE.",
        "These values govern where the early pipeline reads from, where it writes to, and which external tools and models it calls."
      ],
      reference: "#ref-settings"
    },
    dependencies: {
      title: "Settings module: backend dependencies",
      subtitle: "This shows how the shared settings connect to the active entry scripts.",
      body: [
        "The early Bash entry scripts now read the shared settings file through a common loader, so the project root, output directories, and external executable paths are not duplicated in each script.",
        "That shared configuration now affects image renaming, image-list generation, Fiji preprocessing, ilastik segmentation, mask quantification, validation grouping, inspection write-back, and the ROI extraction entry point."
      ],
      reference: "#ref-settings"
    },
    imglist: {
      title: "Settings module: why OUT_img_input1.txt matters",
      subtitle: "This is the most important early handoff file in the frontend logic.",
      body: [
        "OUT_img_input1.txt is the explicit list of images that later batch preprocessing will use. That means it fixes the input image universe and makes the later outputs easier to interpret and reproduce.",
        "If the image list is wrong or built from the wrong folder, every later stage can still run but produce the wrong analysis. That is why the frontend now treats this file as part of the first settings module."
      ],
      reference: "#ref-settings"
    },
    confirm: {
      title: "Settings module: what to confirm before running",
      subtitle: "Use this checklist before the active pipeline starts.",
      body: [
        "Confirm that the renamed image folder name is correct, that the image-list file name is correct, and that the image list is being built from the intended folder.",
        "Also confirm the Fiji path, the ilastik path, the ilastik project file, the main output folder names, and the sample metadata filename so later scripts point to the same project state."
      ],
      reference: "#ref-settings"
    }
  },
  validation_profile_module: {
    summary: {
      title: "Saved validation cutoff profile decision",
      subtitle: "This is now the first major workflow branch in the project.",
      body: [
        "Before running the later pipeline, first check whether a trusted saved validation cutoff profile already exists.",
        "If no saved profile exists, the logic is: Validation with Test Images, then Generalization with New Images, then output plotting for Visualization and Interpretation.",
        "If a saved profile exists, the user should be allowed either to run a fresh validation again or to reuse one saved profile by date or folder name."
      ],
      reference: "#ref-validation-profile"
    },
    fresh: {
      title: "Saved validation cutoff profile: fresh validation route",
      subtitle: "Use this when there is no trusted prior profile, or when the old one should be replaced.",
      body: [
        "Fresh validation route: Validation with Test Images, then Generalization with New Images, then output plotting for Visualization and Interpretation.",
        "In practice this means running the validation grouping, manual inspection, ROI extraction, and optimization steps first, then saving the resulting validated cutoff profile if it is trusted."
      ],
      reference: "#ref-validation-profile"
    },
    reuse: {
      title: "Saved validation cutoff profile: reuse route",
      subtitle: "Use this when a previous validated cutoff profile is still suitable.",
      body: [
        "When saved profiles exist, the user should select one by date or folder name from cache/validation_profiles or the legacy cache/optimization_profiles folder.",
        "After selecting that saved profile, the workflow should continue directly with Generalization on New Images and then the Visualization and Interpretation module."
      ],
      reference: "#ref-validation-profile"
    }
  },
  prepare_inputs: {
    summary: {
      title: "Prepare image inputs",
      subtitle: "This opens the first backend guidance block for the renaming and image-list stages.",
      body: [
        "The renaming step creates a PRE_renamed folder, keeps the nested folder hierarchy, and gives each image a unique name derived from its path. This prevents collisions later when outputs from different folders need to be combined.",
        "The image-list step then scans PRE_renamed recursively, converts each match to an absolute path, and saves the results in OUT_img_input1.txt. Later batch tools rely on that file so the exact same image set is passed forward.",
        "In the functional app these stages are treated as image-countable steps, so the user can see expected images versus produced images, explicit dropout names, and safe retry options for missing items where supported."
      ],
      reference: "#ref-prepare-inputs"
    },
    inputs: {
      title: "Prepare image inputs: inputs and outputs",
      subtitle: "Use this when you need to know what goes into the step and what file it creates for the next one.",
      body: [
        "Input: nested raw image folders with the predefined folder hierarchy.",
        "Intermediate output: PRE_renamed/, which contains hard-linked files with path-derived unique names.",
        "Forward handoff: OUT_img_input1.txt, a plain text list of absolute image paths for the Fiji preprocessing stage."
      ],
      reference: "#ref-prepare-inputs"
    },
    logic: {
      title: "Prepare image inputs: why this stage exists",
      subtitle: "This is the continuity logic behind the first two scripts.",
      body: [
        "Without unique path-derived names, later aggregate outputs can become ambiguous when different folders contain similarly named image files.",
        "Without one shared absolute-path list, later tools may accidentally process different image subsets. These first two scripts make the rest of the pipeline deterministic."
      ],
      reference: "#ref-prepare-inputs"
    }
  },
  fiji_preprocess: {
    summary: {
      title: "Fiji preprocessing",
      subtitle: "This stage standardizes the image intensity representation before segmentation.",
      body: [
        "The Fiji macro reads the image list, opens each image, splits RGB channels, computes a pixel-wise average of the red and green channels, and saves the derived image as an _RGavg.tif file.",
        "It also closes all images before moving to the next one, which keeps the batch run stable when many files are processed.",
        "The functional app now checks this stage image by image so it can flag specific missing _RGavg outputs instead of only checking whether the output folder exists."
      ],
      reference: "#ref-fiji-preprocess"
    },
    logic: {
      title: "Fiji preprocessing: algorithm logic",
      subtitle: "This explains what the backend text is doing in plain language.",
      body: [
        "This step turns a color image into one standardized intensity image so the segmentation model sees a more controlled representation.",
        "By averaging the red and green channels, the pipeline focuses the next stage on a derived signal that is easier to compare across files than a raw multichannel image."
      ],
      reference: "#ref-fiji-preprocess"
    },
    files: {
      title: "Fiji preprocessing: related files",
      subtitle: "These are the files directly connected to this stage.",
      body: [
        "Runner: 2.RunFIJI_clean_bckgnd.sh",
        "Macro: Macro_1-clear_bckgnd.ijm",
        "Reads: OUT_img_input1.txt",
        "Writes: OUT_clear_bcgnd/"
      ],
      reference: "#ref-fiji-preprocess"
    }
  },
  ilastik_segmentation: {
    summary: {
      title: "Ilastik segmentation",
      subtitle: "This is the main segmentation handoff between preprocessing and quantification.",
      body: [
        "The batch script loads the trained ilastik project, processes every matching TIFF in OUT_clear_bcgnd, and writes a segmentation mask for each image into OUT_segmentation.",
        "It then renormalizes the mask and deletes the unadjusted intermediate so downstream tools read a stable output format."
      ],
      reference: "#ref-ilastik"
    },
    logic: {
      title: "Ilastik segmentation: algorithm logic",
      subtitle: "This clarifies what the model stage contributes.",
      body: [
        "This stage converts preprocessed intensity images into labeled mask images. It is the model-based step that decides where neurites, cell bodies, and background are likely to be.",
        "The renormalization step matters because later macros assume a stable label encoding when they calculate area fractions.",
        "The app also treats this as a countable image batch, so missing mask outputs can be surfaced as dropouts and retried separately from already successful masks."
      ],
      reference: "#ref-ilastik"
    },
    outputs: {
      title: "Ilastik segmentation: outputs",
      subtitle: "Use this when checking what the next stage expects.",
      body: [
        "Input folder: OUT_clear_bcgnd/",
        "Output folder: OUT_segmentation/",
        "Main artifact: per-image renormalized mask TIFFs for quantitative analysis."
      ],
      reference: "#ref-ilastik"
    }
  },
  mask_quantification: {
    summary: {
      title: "Mask quantification",
      subtitle: "This stage turns segmentation masks into the first compact measurement table.",
      body: [
        "The macro iterates over mask images, builds a histogram of pixel values, and converts the labeled pixels into class-wise area percentages.",
        "The output table is later merged with metadata and used to build manual validation groups."
      ],
      reference: "#ref-mask-quant"
    },
    logic: {
      title: "Mask quantification: pixel label logic",
      subtitle: "This is the key backend paragraph for understanding the area percentages.",
      body: [
        "The functional app now asks the user to confirm the segmentation label mapping before validation grouping. The current defaults are 1 = cell bodies, 3 = neurites, and 255 = background.",
        "By counting the confirmed labels and dividing by total area, it produces biologically interpretable fractions that can be compared across images."
      ],
      reference: "#ref-mask-quant"
    },
    outputs: {
      title: "Mask quantification: output table",
      subtitle: "This shows what the downstream grouping script receives.",
      body: [
        "Main output: OUT_optimize/mask_area_percentages.csv",
        "Columns include image_name, cell_bodies_percent, neurites_percent, background%, and total_area_px."
      ],
      reference: "#ref-mask-quant"
    }
  },
  segmentation_label_mapping: {
    summary: {
      title: "Segmentation label mapping check",
      subtitle: "This checkpoint prevents swapped mask labels from silently breaking validation.",
      body: [
        "The functional app opens a segmentation mask from OUT_segmentation, plots its 0-255 pixel histogram, and lists the detected non-empty pixel peaks.",
        "It then renders a colored preview using the current assignments: red for neurites, blue for cell bodies, and yellow for background. This preview uses the R package magick in the same R environment that runs Shiny."
      ],
      reference: "#ref-mask-quant"
    },
    logic: {
      title: "Segmentation label mapping: pixel logic",
      subtitle: "Use this when the neurites histogram or validation grouping looks biologically wrong.",
      body: [
        "The expected working case is exactly three peaks. With the current model outputs, the defaults are neurites = 3, cell bodies = 1, and background = 255.",
        "If the colors look biologically exchanged, the user should assign the three detected peaks to the correct classes and confirm the mapping before rerunning mask-area quantification."
      ],
      reference: "#ref-mask-quant"
    },
    outputs: {
      title: "Segmentation label mapping: downstream use",
      subtitle: "The confirmed values become shared backend settings.",
      body: [
        "The app saves SEG_LABEL_NEURITES, SEG_LABEL_CELL_BODIES, and SEG_LABEL_BACKGROUND in config/pipeline_settings.env.",
        "The Fiji mask-area macro and the ROI extraction macro read those settings through their Bash wrappers, so later quantification and ROI extraction use the confirmed labels."
      ],
      reference: "#ref-mask-quant"
    }
  },
  manual_validation: {
    summary: {
      title: "Manual validation loop",
      subtitle: "This is the human-in-the-loop bridge between automatic quantification and later optimization.",
      body: [
        "The grouping script merges metadata with mask percentages, derives confluence, removes outliers, and selects four extreme quadrants of neurites and confluence.",
        "After that, the user manually sorts examples into choice and noise folders. A follow-up script then writes those decisions back into the data tables."
      ],
      reference: "#ref-manual-validation"
    },
    manual: {
      title: "Manual validation loop: required user action",
      subtitle: "This is the practical instruction text a user needs during review.",
      body: [
        "After the grouped folders are created, manually inspect the linked images and populate the choice and noise subfolders.",
        "Only after that should you run the write-back step, because the inspection script expects those folders to reflect your manual judgment."
      ],
      reference: "#ref-manual-validation"
    },
    logic: {
      title: "Manual validation loop: grouping logic",
      subtitle: "This opens the more analytical backend explanation for why the quadrants exist.",
      body: [
        "The grouping R script uses quantiles of neurites and confluence, removes outliers, and relaxes the thresholds stepwise until enough candidates exist in each quadrant.",
        "This creates a structured and reproducible way to present the user with images that are most informative for separating signal from noise."
      ],
      reference: "#ref-manual-validation"
    }
  },
  roi_analysis: {
    summary: {
      title: "ROI, skeleton, and analysis",
      subtitle: "This is the downstream analysis branch after manual validation has stabilized the input set.",
      body: [
        "The ROI macro exports ROI objects and optional skeleton summaries from segmentation masks. The first R stage then packages the validated images into an optimization-ready R object.",
        "Later scripts search ROI threshold rules and carry the resulting signal definition into generalized and biological analysis."
      ],
      reference: "#ref-roi-analysis"
    },
    optimization: {
      title: "ROI, skeleton, and analysis: optimization logic",
      subtitle: "This explains the scoring logic behind the main optimization stage.",
      body: [
        "The optimization script tests ROI geometry thresholds and scores them by how well they separate high-neurite from low-neurite images, how little they are dominated by confluence, and how strongly they distinguish signal from noise.",
        "The goal is not just to keep many ROIs. The goal is to keep biologically useful ROIs that reflect neurite structure rather than artefact."
      ],
      reference: "#ref-roi-analysis"
    },
    biology: {
      title: "ROI, skeleton, and analysis: biological interpretation",
      subtitle: "This opens the later-stage analysis logic in a compact form.",
      body: [
        "After the generalized signal rule is defined, later scripts aggregate skeleton or ROI features to image level, merge experimental metadata, and run statistical and multivariate analyses.",
        "This is where the pipeline shifts from tuning the extraction rule to drawing biological conclusions from the filtered neurite signal."
      ],
      reference: "#ref-roi-analysis"
    }
  },
  interpretation_module: {
    summary: {
      title: "Visualization and Interpretation Module",
      subtitle: "This module comes after image-analysis outputs already exist and helps the user understand them.",
      body: [
        "This is not the raw image-generation branch. It is the post-analysis branch for checking representative ROI behavior, class separation, PCA structure, and final biological conclusions.",
        "The preferred scripts here are 3.analyze-visualize-check.R and 3.analysis-check-visualize-readiness-on-test-images2.R for ROI-focused interpretation, followed by 3.1.analyzed-generalized-skeleton-data-PCA+ALMOST_WORKING.R and 4.1.Skeleton-cutoff-ploting-improved-polished+BG.R for production-level interpretation and reporting."
      ],
      reference: "#ref-interpretation"
    },
    hierarchy: {
      title: "Visualization and Interpretation Module: Script Hierarchy",
      subtitle: "This makes the functional chain visible instead of leaving the similar filenames ambiguous.",
      body: [
        "Upstream builders: 1.analyze-combine-ROIs-skeletons.R and 2.analyze-combine-ROIs-skeletons.R create the combined analysis objects that later visualization scripts read.",
        "Preferred visualization layer: 3.analyze-visualize-check.R for representative ROI visual checks and 3.analysis-check-visualize-readiness-on-test-images2.R for a richer comparison plus PCA view.",
        "Production interpretation layer: 3.1.analyzed-generalized-skeleton-data-PCA+ALMOST_WORKING.R for metadata-driven structure and 4.1.Skeleton-cutoff-ploting-improved-polished+BG.R for the final biological reporting plots."
      ],
      reference: "#ref-interpretation"
    },
    outputs: {
      title: "Visualization and Interpretation Module: Expected Outputs",
      subtitle: "Use this when deciding whether you are in a diagnostic, interpretation, or final-reporting stage.",
      body: [
        "Expected outputs include representative ROI visualization panels, class-comparison figures, PCA plots, optimization-readiness plots, and polished final biological reporting figures.",
        "Optimization-diagnostic support scripts such as 2.roi_distribution_analysis.R, 3.optimized_grid_search.R, 3.analysis-check-visualize-readiness-on-test-images2PCA.R, and 3.1.analysis-check-skeletons-readiness-on-test-images2PCA.R belong nearby, but they are better understood as diagnostics rather than the final presentation layer.",
        "The functional app mirrors this logic with readiness panels that distinguish ready, incomplete, and missing states, and for countable image stages it shows completed images versus expected images rather than only a file-path count."
      ],
      reference: "#ref-interpretation"
    },
    diagnostics: {
      title: "Visualization and Interpretation Module: Optimization Diagnostics",
      subtitle: "These scripts explain why threshold choices or feature subsets behave the way they do.",
      body: [
        "2.roi_distribution_analysis.R checks ROI feature distributions, candidate ranges, and sensitivity. 3.optimized_grid_search.R expands that into a broader optimization landscape.",
        "3.analysis-check-visualize-readiness-on-test-images2PCA.R and 3.1.analysis-check-skeletons-readiness-on-test-images2PCA.R are best treated as separability and readiness diagnostics for ROI and skeleton features rather than as the main final-reporting files."
      ],
      reference: "#ref-interpretation"
    }
  }
};

const titleNode = document.getElementById("instruction-title");
const subtitleNode = document.getElementById("instruction-subtitle");
const bodyNode = document.getElementById("instruction-body");
const linksNode = document.getElementById("instruction-links");
const resetButton = document.getElementById("reset-instruction");

function renderInstruction(topic, view) {
  const entry = instructionData[topic] && instructionData[topic][view];
  if (!entry) {
    return;
  }

  titleNode.textContent = entry.title;
  subtitleNode.textContent = entry.subtitle;
  bodyNode.innerHTML = entry.body.map((paragraph) => `<p>${paragraph}</p>`).join("");
  linksNode.innerHTML = `
    <a class="button button-secondary" href="${entry.reference}">Open fuller reference below</a>
    <a class="button button-ghost" href="#settings-module">Open settings module</a>
    <a class="button button-ghost" href="../docs/PIPELINE_OVERVIEW.md">Open backend overview file</a>
    <a class="button button-ghost" href="../docs/SOFTWARE_SETUP.md">Open software setup file</a>
  `;

  document.getElementById("instruction-desk").scrollIntoView({ behavior: "smooth", block: "start" });
}

document.querySelectorAll(".chip").forEach((button) => {
  button.addEventListener("click", () => {
    renderInstruction(button.dataset.topic, button.dataset.view);
  });
});

resetButton.addEventListener("click", () => {
  titleNode.textContent = "Choose a workflow panel";
  subtitleNode.textContent = "Click a summary or logic button in a workflow card to open the relevant backend guidance here.";
  bodyNode.innerHTML = `
    <p>
      This area is meant to keep the important text visible without forcing the user to open separate files.
      It can show a short stage summary, inputs and outputs, manual actions, or the reasoning behind a step.
    </p>
  `;
  linksNode.innerHTML = `<a class="button button-secondary" href="#reference-library">Open fuller reference below</a>`;
});
