# Frontend Guidelines

## Main goal
The browser interface should guide a non-terminal user through a mixed Bash + Fiji + ilastik + R workflow without hiding the real pipeline logic.

## Keep the frontend simple
- One vertical workflow from top to bottom
- One card per stage
- Show inputs, outputs, and required manual actions clearly
- Separate automatic stages from manual review stages

## Good default layout
1. Settings and requirements module
2. Project status bar
3. Step cards in pipeline order
4. Current output folders and last-run artifacts
5. Logs and troubleshooting
6. Advanced settings in collapsible panels

## The first module should always be settings
- It should explain which shared configuration file drives the pipeline.
- It should clearly state why `OUT_img_input1.txt` matters.
- It should let the user confirm the image source folder, output list file, Fiji path, ilastik path, model file, and metadata file before running anything else.
- It should make it obvious which later scripts depend on those values.

## What each step card should show
- step number
- step name
- script or macro used
- what goes in
- what comes out
- whether user action is required
- launch button
- last run status

## Good interaction practices
- Prefer plain action labels like `Prepare images`, `Run segmentation`, `Open manual review folders`
- Avoid exposing too many parameters at once
- Put file paths in a compact details area
- Always show where outputs were written

## Visual clarity
- Use strong section labels
- Avoid crowded sidebars
- Keep status colors consistent:
  - pending
  - running
  - needs user action
  - complete
  - failed

## Important honesty rule
The frontend should not pretend everything is a single native algorithm.
It should clearly state when a step runs:
- Bash
- Fiji/ImageJ macro
- ilastik batch model
- R script

That will make the system easier to trust and easier to debug.
