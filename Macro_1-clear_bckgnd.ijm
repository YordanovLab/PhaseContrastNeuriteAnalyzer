// === CONFIGURATION ===
arg = getArgument();
partsArg = split(arg, "||");
if (arg == "") {
    baseDir = getDirectory("Choose the output workspace folder containing OUT_img_input1.txt");
    if (baseDir == null || baseDir == "") exit("No folder selected.");
} else {
    baseDir = partsArg[0];
}
if (!endsWith(baseDir, File.separator)) baseDir = baseDir + File.separator;

// Define output folder
if (partsArg.length >= 3 && trim(partsArg[2]) != "") {
    outputDir = trim(partsArg[2]);
    if (!endsWith(outputDir, File.separator)) outputDir = outputDir + File.separator;
} else {
    outputDir = baseDir + "OUT_clear_bcgnd" + File.separator;
}
File.makeDirectory(outputDir);

// Read full image paths from input file
if (partsArg.length >= 2 && trim(partsArg[1]) != "") {
    listFile = trim(partsArg[1]);
} else {
    listFile = baseDir + "OUT_img_input1.txt";
}
if (!File.exists(listFile)) {
    exit("ERROR: Input file not found:\n" + listFile);
}
fileString = File.openAsString(listFile);
fileList = split(fileString, "\n");

// === PROCESSING LOOP ===
setBatchMode(true);

for (i = 0; i < fileList.length; i++) {
    imagePath = trim(fileList[i]);
    if (imagePath == "") continue; // skip empty lines

    print("Processing: " + imagePath);
    open(imagePath);

    // Extract filename from path
    parts = split(imagePath, File.separator);
    imageName = parts[parts.length - 1];

    // This preprocessing step expects RGB inputs. Skip grayscale or single-channel
    // TIFFs instead of crashing the whole batch.
    selectImage(imageName);
    getDimensions(w, h, channels, slices, frames);
    if (channels < 3 && bitDepth() != 24) {
        print("Skipping unsupported non-RGB image: " + imagePath);
        run("Close All");
        continue;
    }

    // Split channels
    run("Split Channels");

    // Define channel titles (ImageJ naming convention after split)
    redTitle   = imageName + " (red)";
    greenTitle = imageName + " (green)";
    blueTitle  = imageName + " (blue)";

    if (!isOpen(redTitle) || !isOpen(greenTitle)) {
        print("Skipping image because red/green channels were not created as expected: " + imagePath);
        run("Close All");
        continue;
    }

    // Average red and green channels
    imageCalculator("Average create", redTitle, greenTitle);
    avgTitle = "Result of " + redTitle;

    if (!isOpen(avgTitle)) {
        print("Skipping image because RG averaging failed: " + imagePath);
        run("Close All");
        continue;
    }

    // Save averaged image to output folder
    selectImage(avgTitle);
    saveAs("Tiff", outputDir + replace(imageName, ".tif", "_RGavg.tif"));

    // Close everything
    run("Close All");
}

setBatchMode(false);
print("Processing complete. Files saved in: " + outputDir);
run("Quit");
