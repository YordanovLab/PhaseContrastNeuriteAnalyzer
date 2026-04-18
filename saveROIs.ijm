// Batch process all images in the output workspace's OUT_segmentation folder
logPath = "";
diagCsvPath = "";

function log(msg) {
    line = "[DEBUG] " + msg;
    print(line);
    if (logPath != "") File.append(line + "\n", logPath);
}

function appendDiagRow(imageName, bitDepthValue, totalPx, minBefore, maxBefore, meanBefore,
    count0, count1, count3, count7, count255, targetLabel, maskForeground, roiCount, zipSaved, overlaySaved, status, note) {
    safeImageName = replace("" + imageName, ",", ";");
    safeBitDepth = replace("" + bitDepthValue, ",", ";");
    safeStatus = replace("" + status, ",", ";");
    safeNote = replace("" + note, ",", ";");
    row = safeImageName + "," +
          safeBitDepth + "," +
          totalPx + "," +
          minBefore + "," +
          maxBefore + "," +
          meanBefore + "," +
          count0 + "," +
          count1 + "," +
          count3 + "," +
          count7 + "," +
          count255 + "," +
          targetLabel + "," +
          maskForeground + "," +
          roiCount + "," +
          zipSaved + "," +
          overlaySaved + "," +
          safeStatus + "," +
          safeNote + "\n";
    File.append(row, diagCsvPath);
}

// Analysis parameters
minSize = 1;
maxSize = "Infinity";
minCirc = 0;
maxCirc = 1.00;

// Setup paths
arg = getArgument();
if (arg == "") {
    baseDir = call("java.lang.System.getProperty", "user.dir");
    configuredNeuriteLabel = 3;
} else {
    parts = split(arg, "|");
    baseDir = parts[0];
    configuredNeuriteLabel = 3;
    if (parts.length >= 2) configuredNeuriteLabel = parseInt(parts[1]);
}
sep = File.separator;
if (lengthOf(baseDir) == 0) exit("Empty working directory.");
tail = substring(baseDir, lengthOf(baseDir)-1, lengthOf(baseDir));
if (tail != sep) baseDir = baseDir + sep;
log("Base dir: " + baseDir);
log("Configured neurite target label: " + configuredNeuriteLabel);

// Define input and output directories
inputDir = baseDir + "OUT_segmentation" + sep;
outOptDir = baseDir + "OUT_optimize" + sep;
roisRoot = outOptDir + "ROIs" + sep;
logPath = outOptDir + "roi_extraction_detailed_log.txt";
diagCsvPath = outOptDir + "roi_extraction_diagnostics.csv";

// Create root output directory if it doesn't exist
if (!File.exists(outOptDir)) File.makeDirectory(outOptDir);
if (!File.exists(roisRoot)) File.makeDirectory(roisRoot);
if (File.exists(logPath)) File.delete(logPath);
if (File.exists(diagCsvPath)) File.delete(diagCsvPath);
File.append("image_name,bit_depth,total_px,min_before,max_before,mean_before,count_0,count_1,count_3,count_7,count_255,target_label,mask_foreground_px,roi_count,zip_saved,overlay_saved,status,note\n", diagCsvPath);
log("ROI extraction diagnostics will be written to: " + diagCsvPath);
log("ROI extraction text log will be written to: " + logPath);

// Get list of files in input directory
fileList = getFileList(inputDir);
log("Found " + fileList.length + " files in input directory");

// Filter for image files (optional - adjust extensions as needed)
imageFiles = newArray();
for (i = 0; i < fileList.length; i++) {
    if (endsWith(fileList[i], ".tif") || endsWith(fileList[i], ".tiff") || 
        endsWith(fileList[i], ".png") || endsWith(fileList[i], ".jpg")) {
        imageFiles = Array.concat(imageFiles, fileList[i]);
    }
}
log("Found " + imageFiles.length + " image files to process");

// Process each image
totalROIs = 0;
for (fileIdx = 0; fileIdx < imageFiles.length; fileIdx++) {
    fileName = imageFiles[fileIdx];
    filePath = inputDir + fileName;
    imgTitle = fileName;
    imgBitDepth = "unknown";
    totalPx = 0;
    minBefore = "";
    maxBefore = "";
    meanBefore = "";
    count0 = 0;
    count1 = 0;
    count3 = 0;
    count7 = 0;
    count255 = 0;
    targetLabel = -1;
    maskForeground = 0;
    roiCount = 0;
    zipSaved = "no";
    overlaySaved = "no";
    status = "started";
    note = "";
    outDir = "";
    
    log("Processing file " + (fileIdx + 1) + "/" + imageFiles.length + ": " + fileName);
    
    // Open image
    open(filePath);
    imgTitle = getTitle();
    log("Image title: " + imgTitle);
    
    selectImage(imgTitle);
    imgBitDepth = bitDepth();
    getDimensions(w, h, channels, slices, frames);
    totalPx = w * h;
    
    // Get image stats before processing
    getStatistics(area, mean, min, max);
    minBefore = min;
    maxBefore = max;
    meanBefore = mean;
    log("Before threshold - min: " + min + ", max: " + max + ", mean: " + mean);

    run("8-bit");
    getHistogram(values, counts, 256);
    count0 = counts[0];
    count1 = counts[1];
    count3 = counts[3];
    count7 = counts[7];
    count255 = counts[255];
    log("Class pixel counts - value0: " + count0 + ", value1: " + count1 + ", value3: " + count3 + ", value7: " + count7 + ", background(255): " + count255);

    configuredCount = 0;
    if (configuredNeuriteLabel >= 0 && configuredNeuriteLabel < lengthOf(counts)) configuredCount = counts[configuredNeuriteLabel];

    if (configuredCount > 0) {
        targetLabel = configuredNeuriteLabel;
        log("Using configured neurite target label " + configuredNeuriteLabel + " with " + configuredCount + " pixels.");
    } else {
        status = "no_supported_neurite_label_pixels";
        note = "Histogram contains zero pixels with the configured neurite label " + configuredNeuriteLabel + ", so no neurite ROI extraction can occur from this mask.";
        log("WARNING: " + note);
        appendDiagRow(imgTitle, imgBitDepth, totalPx, minBefore, maxBefore, meanBefore, count0, count1, count3, count7, count255, targetLabel, maskForeground, roiCount, zipSaved, overlaySaved, status, note);
        close("*");
        continue;
    }

    // Keep only the neurite-like class from the segmentation mask.
    setThreshold(targetLabel, targetLabel);
    setOption("BlackBackground", true);
    run("Convert to Mask");
    
    // Check mask stats
    getStatistics(area, mean, min, max);
    log("After mask - min: " + min + ", max: " + max + ", mean: " + mean);
    getHistogram(maskValues, maskCounts, 256);
    maskForeground = 0;
    for (k = 1; k < lengthOf(maskCounts); k++) {
        maskForeground = maskForeground + maskCounts[k];
    }
    log("Binary mask foreground pixels after neurite isolation: " + maskForeground);

    if (maskForeground <= 0) {
        status = "empty_binary_mask_after_threshold";
        note = "Foreground-labeled pixels were expected, but the binary mask contains no foreground pixels after thresholding and mask conversion.";
        log("WARNING: " + note);
        appendDiagRow(imgTitle, imgBitDepth, totalPx, minBefore, maxBefore, meanBefore, count0, count1, count3, count7, count255, targetLabel, maskForeground, roiCount, zipSaved, overlaySaved, status, note);
        close("*");
        continue;
    }
    
    // Run particle analysis
    log("Running particle analysis with parameters: size=" + minSize + "-" + maxSize + 
        ", circularity=" + minCirc + "-" + maxCirc);
    
    run("Analyze Particles...", 
        "size=" + minSize + "-" + maxSize + " " +
        "circularity=" + minCirc + "-" + maxCirc + " " +
        "show=Nothing " +
        "display exclude clear " +
        "add");
    
    // Get ROI count
    roiCount = roiManager("count");
    log("Found " + roiCount + " ROIs");
    totalROIs += roiCount;
    
    if (roiCount > 0) {
        // Create output directory based on image name (without extension) only when payload exists.
        imgNameNoExt = substring(imgTitle, 0, lastIndexOf(imgTitle, "."));
        outDir = roisRoot + imgNameNoExt + sep;
        if (!File.exists(outDir)) File.makeDirectory(outDir);
        log("Output directory: " + outDir);

        // Save all ROIs as a zip file
        roiManager("Save", outDir + imgTitle + "_ROIs.zip");
        zipSaved = "yes";
        log("Saved ROI set: " + outDir + imgTitle + "_ROIs.zip");
        
        // Create ROI list text file
        roiListFileName = "ROI-list-threshold-size=" + minSize + "-" + maxSize + 
                         "_circularity=" + minCirc + "-" + maxCirc + ".txt";
        roiListPath = outDir + roiListFileName;
        roiList = "";
        
        // Save individual ROIs and build list
        for (i = 0; i < roiCount; i++) {
            roiManager("select", i);
            roiName = "ROI_" + IJ.pad(i + 1, 4) + ".roi";
            roiManager("Save", outDir + roiName);
            roiList = roiList + roiName + "\n";
        }
        log("Saved " + roiCount + " individual ROI files");
        
        // Save ROI list to text file
        File.saveString(roiList, roiListPath);
        log("Saved ROI list: " + roiListPath);
        
        // Create overlay image for visualization
        selectImage(imgTitle);
        roiManager("Show All");
        run("Flatten");
        saveAs("PNG", outDir + imgTitle + "_overlay.png");
        overlaySaved = "yes";
        log("Saved overlay image");
        status = "roi_payload_saved";
        note = "ROI extraction and save completed successfully.";
    } else {
        status = "analyze_particles_zero_rois";
        note = "Analyze Particles completed but found zero foreground objects. Check whether the segmentation mask contains disconnected neurite-like regions that survive thresholding.";
        log("WARNING: " + note);
    }

    appendDiagRow(imgTitle, imgBitDepth, totalPx, minBefore, maxBefore, meanBefore, count0, count1, count3, count7, count255, targetLabel, maskForeground, roiCount, zipSaved, overlaySaved, status, note);
    
    // Cleanup
    roiManager("reset");
    close("*");
    log("Processing complete for: " + imgTitle);
}

log("=== BATCH PROCESSING COMPLETE ===");
log("Processed " + imageFiles.length + " images");
log("Total ROIs detected: " + totalROIs);
log("Detailed diagnostics CSV: " + diagCsvPath);
print("Batch Analysis Complete");
print("Processing finished for " + imageFiles.length + " images");
print("Total ROIs found: " + totalROIs);
print("Output saved to: " + roisRoot);
run("Quit");
