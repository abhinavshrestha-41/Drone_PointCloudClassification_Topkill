###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: shre9292@vandals.uidaho.edu

### Purpose(s): 
# * Average RF classification probabilities by tree

### Last update: 07/14/23

###########################################################################

gc()

# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
start_ptm <- Sys.time()

require(tidyr)
require(dplyr)
require(terra)
#require(ggplot2)
require(lidR)
require(sf)
#require(sp)
# require(ggrepel)
require(randomForest)
require(rfUtilities)
require(data.table)
require(caret)
require(stars)
require(future)
require(future.apply)
#require(rgdal)
#source("errorAccuracyOutputs_function.R") # make sure this file is in the working directory


# =========================================================================

#                           CHANGE VARIABLES HERE

# =========================================================================


# ------------------------- SET DIR, PATH, FILE ---------------------------
message("Initializing workspace and paths...")
# setwd("PATH/TO/DIR")

dir  = getwd()

dataPATH = file.path(dir, "DATA")

exportPATH = file.path(dir, "EXPORTS")

# ---

# INPUT
pointCloud_fileName <- "M2_subset_li2012_RFClassif_maxProb.las"
treeSeg_shpName <- "M2_subset_li2012_treeSeg_noNA_noDuplicates.shp"

# OUTPUT
exportShp_name <- "TreeSeg_polygons_mean_RF_maxProb_M2_subset_li2012.shp"

# ---

# -------------------------------------------------------------------------

# -------------------------------------------------------------------------

# Load point cloud file 
message("------------------------------------------------------------")
message("STARTING DAMAGE ASSESSMENT SCRIPT")

message("Importing RF classified point cloud...")
ptm1 <- Sys.time()

pointCloud_fullPath <- file.path(dataPATH, pointCloud_fileName)

pointCloudObj <- lidR::readUAVLAS(pointCloud_fullPath)
setnames(pointCloudObj@data, "RF_RBI_NDVI_REDEDGE", "RF_Class") # change RF classification column if not named as "RF_Class"
setnames(pointCloudObj@data, "RBI_NDVI_REDEDGE_max_prob", "RF_maxProb") # change RF maximum probability column if not named as "RF_maxProb"

# remove ground classified points
pointCloudObj_noGround_noShadows <- lidR::filter_poi(pointCloudObj, Classification == 0) 

# remove any points from ground to half a meter
pointCloudObj_noGround_noShadows <- lidR::filter_poi(pointCloudObj, Z >= 0.5) 

# remove shadow classified points
pointCloudObj_noGround_noShadows <- lidR::filter_poi(pointCloudObj_noGround_noShadows, RF_Class != 4) 

message("Point cloud imported, shadows and ground classified points filtered.")
(Sys.time() - ptm1)
message("------------------------------------------------------------")

message("Initializing data and variables for damage assessment algorithm:")

message("--> Extracting point cloud attributes as data.table")
# create data table of point cloud file attributes 
pointCloudObj_DT <- data.table(pointCloudObj_noGround_noShadows@data)

message("--> Creating empty data table to store damage assessment results")
# create data table to store damage assessment by treeID
mean_RFmaxProb_byTree_DT <- data.table(treeID = numeric(), 
                              mean_RF_maxProb = numeric())

message("--> Creating a list of unique treeIDs")
# Create a list of treeIDs to loop through
treeID_list <- unique(pointCloudObj_DT$treeID[!is.na(pointCloudObj_DT$treeID)])

# testTreeID <- 7 # For troubleshooting


# Store column numbers for the Z (height) and classification variables 
ZcolNum <- as.numeric(which(colnames(pointCloudObj_DT) == "Z"))
RFmaxProbColNum <- as.numeric(which(colnames(pointCloudObj_DT) == "RF_maxProb"))
message("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
message("HOUSEKEEPING: removing temporary files and running garbage collection...")
rm(pointCloudObj)
rm(pointCloudObj_noGround_noShadows)
gc()
message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

message("------------------------------------------------------------")
message("Damage assessment iteration STARTING:")
message("Message readouts indicate the algorithm used/damage type for each damaged tree")
ptm2 <- Sys.time()

# Iterate throught each point cloud of individual tree (in treeID_list) and asses damage
for (treeNum in treeID_list) {

  # Sieve to catch any NAs 
  if (is.na(treeNum)) {
    next
  }
    
    # Create a subset data table of Z value (height information) and RF classification of ONLY the individual tree (by treeID)
    tree_DT_RFmaxProb <- pointCloudObj_DT[treeID == treeNum, c(..ZcolNum, ..RFmaxProbColNum)] # DOUBLE CHECK THE COLUMN NUMBERS, data.table reads variables with preceding `..` as expressions
    
    # Calculate classification statistics for individual tree under "treeStats" R-vector-object
    treeStats <- c("mean_RF_maxProb" = mean(tree_DT_RFmaxProb$RF_maxProb),  
                   "totalPoints" = nrow( tree_DT_RFmaxProb))

    # If the tree itself has less than 200 points total, insufficient to make assess the structure and damage on the tree, skips to next.
    if (treeStats[["totalPoints"]] <= 200) {
    next # Skips tree if points are too less
    } else {
     # Add damage analysis data to data table 
     mean_RFmaxProb_byTree_DT  <- rbindlist(list(mean_RFmaxProb_byTree_DT , data.table(treeID = treeNum,
                                                                                       mean_RF_maxProb = treeStats[["mean_RF_maxProb"]])))
    }      
}

message("Mean RF max prob per tree calculation complete.")

message("Labeling snags as trees with > 75% top-kill.")

(Sys.time() - ptm2)
message("------------------------------------------------------------")

message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
message("                  Garbage Collection                        ")
gc()
message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
# --------------------------------------------------------------------------------------

# Adding damage assessment metrics to tree segmented polygons

# --------------------------------------------------------------------------------------

message("Merge damage assessment information to tree crown delineation shapefile:")
ptm3 <- Sys.time()


message("--> Importing tree crown delineation shapefile...")
# import shapefile

treeSeg_shpPath <- file.path(dataPATH, treeSeg_shpName)

treeSeg_shpObj <- terra::vect(treeSeg_shpPath)


message("--> Merging shapefile attribute table to damage assessment data.table by treeID...")
treeSeg_shpObj_merged <- merge(treeSeg_shpObj, mean_RFmaxProb_byTree_DT, by = "treeID")

message("--> Exporting damage assessment shapefile...")
# export merged shapefile

exportShp_path <- file.path(exportPATH, exportShp_name)

terra::writeVector(x = treeSeg_shpObj_merged,
                   filename = exportShp_path,
                   layer = exportShp_name,
                   filetype = "ESRI Shapefile",
                   overwrite = TRUE)

message("Damage assessment shapefile exported.")
(Sys.time() - ptm3)
message("------------------------------------------------------------\n")

message("Total time taken:")
(Sys.time() - start_ptm)

message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
message("Warning messages (if any:)")
scriptWarnings <- warnings()
if (is.null(scriptWarnings) == TRUE){
  message("There were no warning messages during script execution.")
} else {
  scriptWarnings
}
message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")
message("!==================================================================!")
message("!                   Script run successfully!                       !")
message("!==================================================================!")