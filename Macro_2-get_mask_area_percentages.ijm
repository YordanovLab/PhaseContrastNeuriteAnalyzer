// Macro_2-get_mask_area_percentages.ijm
// Reads masks from "<BASE_DIR>/OUT_segmentation".
// Label defaults are:
//   1 = cell_bodies, 3 = neurites, 255 = background
// The wrapper can override these labels by passing:
//   BASE_DIR|CELL_LABEL|NEURITE_LABEL|BACKGROUND_LABEL
// Builds an ImageJ table row-by-row, then writes CSV to:
//   "<BASE_DIR>/OUT_optimize/mask_area_percentages.csv"
// Columns: image_name,cell_bodies_percent,neurites_percent,background%,total_area_px

// ---------- Helpers ----------
function isImageFile(name) {
    name = toLowerCase(name);
    return endsWith(name, ".tif") || endsWith(name, ".tiff") ||
           endsWith(name, ".png") || endsWith(name, ".jpg")  ||
           endsWith(name, ".jpeg")|| endsWith(name, ".bmp");
}

// ---------- Input base directory ----------
arg = getArgument();
if (arg == "") {
    // Running inside Fiji without a CLI argument: ask user for output workspace folder
    baseDir = getDirectory("Choose output workspace directory (containing OUT_segmentation)");
    if (baseDir == null || baseDir == "") exit("No folder selected.");
} else {
    parts = split(arg, "|");
    baseDir = parts[0];
}
if (!endsWith(baseDir, File.separator)) baseDir = baseDir + File.separator;

// ---------- IO paths ----------
segDir = baseDir + "OUT_segmentation" + File.separator;
outDir = baseDir + "OUT_optimize" + File.separator;
csvPath = outDir + "mask_area_percentages.csv";

// ---------- Ensure input/output ----------
if (!File.exists(segDir)) {
    showMessage("Error", "Input folder not found:\n" + segDir);
    exit();
}
if (!File.exists(outDir)) File.makeDirectory(outDir);

// ---------- Table setup (start fresh) ----------
tname = "mask_area_percentages";
Table.create(tname);            // create empty table
// (Optional) if a stale CSV exists, delete to avoid confusion
if (File.exists(csvPath)) File.delete(csvPath);

// ---------- Label constants ----------
CELL_VAL = 1;
NEURITE_VAL = 3;
BACK_VAL = 255;
if (arg != "") {
    parts = split(arg, "|");
    if (parts.length >= 4) {
        CELL_VAL = parseInt(parts[1]);
        NEURITE_VAL = parseInt(parts[2]);
        BACK_VAL = parseInt(parts[3]);
    }
}

print("Mask quantification label mapping:");
print("  cell_bodies = " + CELL_VAL);
print("  neurites    = " + NEURITE_VAL);
print("  background  = " + BACK_VAL);

// ---------- Iterate files and populate table ----------
fileList = getFileList(segDir);
row = 0;
nProcessed = 0;

for (i = 0; i < fileList.length; i++) {
    name = fileList[i];
    path = segDir + name;

    // Skip folders & non-images
    if (File.isDirectory(path)) continue;
    if (!isImageFile(name)) continue;

    // Open, ensure 8-bit single-plane for histogram counting
    open(path);
    run("8-bit");

    getDimensions(w, h, channels, slices, frames);
    total_px = w * h;

    // 256-bin histogram for 8-bit
    getHistogram(values, counts, 256);

    // Grab counts at the exact label values (guarded)
    cell_count = 0;
    if (CELL_VAL >= 0 && CELL_VAL < lengthOf(counts)) cell_count = counts[CELL_VAL];

    neurite_count = 0;
    if (NEURITE_VAL >= 0 && NEURITE_VAL < lengthOf(counts)) neurite_count = counts[NEURITE_VAL];

    back_count = 0;
    if (BACK_VAL >= 0 && BACK_VAL < lengthOf(counts)) back_count = counts[BACK_VAL];

    // Percentages over full image area
    cell_pct    = 100.0 * cell_count    / total_px;
    neurite_pct = 100.0 * neurite_count / total_px;
    back_pct    = 100.0 * back_count    / total_px;

    // Append a row to the table
    Table.set("image_name",          row, name);
    Table.set("cell_bodies_percent", row, d2s(cell_pct, 6));
    Table.set("neurites_percent",    row, d2s(neurite_pct, 6));
    Table.set("background%",         row, d2s(back_pct, 6));
    Table.set("total_area_px",       row, total_px);

    // Progress + cleanup
    showStatus("Processed: " + (row+1) + " / " + fileList.length + " -> " + name);
    close();
    row++;
    nProcessed++;
}

// ---------- Save table once at the end ----------
Table.save(csvPath);
// (Optional) show the table in ImageJ
// Table.show(tname);

// Log
print("Processed " + nProcessed + " image(s).");
print("CSV saved to: " + csvPath);
run("Quit");
