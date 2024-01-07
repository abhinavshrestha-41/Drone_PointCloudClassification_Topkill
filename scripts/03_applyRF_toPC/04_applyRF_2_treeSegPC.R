###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s): 
# * Apply selected RF model (3-var): RBI + NDVI + REDEDGE

### Last update: 12/05/23

###########################################################################

# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
start_ptm <- Sys.time()

message("Importing required libraries...")
message("----------------------------------------------------------------")

# require(tidyr)
# require(dplyr)
require(terra)
# require(ggplot2)
require(lidR)
require(sf)
# require(sp)
# require(ggrepel)
require(randomForest)
require(rfUtilities)
require(data.table)
require(caret)
require(future)
require(future.apply)
#require(lidRViewer)

source("errorAccuracyOutputs_function.R") # make sure this file is in the working directory


# =========================================================================

#                           CHANGE VARIABLES HERE

# =========================================================================


# ------------------------- SET DIR, PATH, FILE ---------------------------

message("Initializing workspace and path...")
message("----------------------------------------------------------------")

# setwd("PATH/TO/DIR")

dir  = getwd()

dataPATH = file.path(dir, "DATA")

exportPATH = file.path(dir, "EXPORTS")

# SET IMPORT AND EXPORT POINT CLOUD FILE NAME

#Import
pointCloud_fileName <- "M2_subset_li2012_treeSeg.las"
refData_csv_name = "PC_ReferenceData_CloudCompare.csv" # INPUT (.csv file of reference data generated with "create_ReferenceData_POINTS.R" script)

#Export
pointCloud_exportName <- "M2_subset_li2012_RFClassif_maxProb.las"

# -------------------------------------------------------------------------



# -------------------------------------------------------------------------

message("Importing reference data set and creating random forest (RF) model...")
ptm1 <- Sys.time()

# Import reference data set 

refData_csv_fullPath = file.path(dataPATH, refData_csv_name)
refData_csv_file = read.csv(refData_csv_fullPath)

refData_DT <- data.table(refData_csv_file) # convert csv file to data table (faster)
rm(refData_csv_file) # no need for this file

refData_DT$class <- as.factor(refData_DT$class) # set class as factor for classification RF

# -------------------------------------------------------------------------

# Create RF model object in R using the `randomForest` package

# ****************       CHANGE VARIABLES HERE       ****************                       
# * Specify the predictor variables for the RF model

# RBI + NDVI + REDEDGE RF model
set.seed(1234)
rf_model_RBI_NDVI_REDEDGE <- randomForest::randomForest(class ~ RBI + NDVI + REDEDGE, 
                                                   data = refData_DT, 
                                                   ntree = 500, 
                                                   keep.forest = TRUE, 
                                                   importance = TRUE)

message("RF model created")

# output results in a formatted table using `errorAccuracyOutputs` function (sourced in the Import section)
errorAccuracyOutputs(rf_model_RBI_NDVI_REDEDGE)

(Sys.time() - ptm1)
message("----------------------------------------------------------------")

# -------------------------------------------------------------------------


message("Importing tree segmented point cloud...")
ptm2 <- Sys.time()

# Load point cloud data and apply RF model to point cloud

pointCloud_fullPath <- file.path(dataPATH, pointCloud_fileName)

pointCloud_obj <- lidR::readUAVLAS(pointCloud_fullPath)

# Extract the point attributes as a data table 

pointCloud_DT <- data.table::data.table(pointCloud_obj@data)

message("Point cloud imported.")
(Sys.time() - ptm2)

message("----------------------------------------------------------------")

# ---
message("Renaming columns and calculating indices...")
ptm3 <- Sys.time()

# reflectances stored as (value*100000), revert back to true reflectance 
pointCloud_DT[, c(16:20)] <- ((pointCloud_DT[, c(16:20) ]) / 10000) 

# rename column headers with band name
colnames(pointCloud_DT)[16:20] <- c("BLU", "GRE", "RED", "REDEDGE", "NearIR") 


# Create new columns for indices and perform calculations; colnames should be same as ones used for rf_model

# Normalized difference vegetation index (NDVI)
pointCloud_DT$NDVI <- ( pointCloud_DT$NearIR - pointCloud_DT$RED ) / ( pointCloud_DT$NearIR + pointCloud_DT$RED )

# Normalized difference rededge index (NDRE)
pointCloud_DT$NDRE <- ( pointCloud_DT$NearIR - pointCloud_DT$REDEDGE ) / ( pointCloud_DT$NearIR + pointCloud_DT$REDEDGE )

# Simple ratio (SR)
pointCloud_DT$SR   <- ( pointCloud_DT$NearIR / pointCloud_DT$RED )

# Green leaf index (GLI)
pointCloud_DT$GLI  <- ( (pointCloud_DT$GRE - pointCloud_DT$RED) + (pointCloud_DT$GRE - pointCloud_DT$BLU) ) / ( (2 * pointCloud_DT$GRE) + pointCloud_DT$RED + pointCloud_DT$BLU )

# Red-green index (RGI)
pointCloud_DT$RGI  <- (pointCloud_DT$RED / pointCloud_DT$GRE)

# Excess green index (ExG)
pointCloud_DT$ExG  <- ((2 * pointCloud_DT$GRE) - (pointCloud_DT$RED + pointCloud_DT$BLU))

# Mean red-blue-green index (meanRGB)
pointCloud_DT$meanRGB <- ((pointCloud_DT$RED + pointCloud_DT$GRE + pointCloud_DT$BLU) / 3)

# Red-blue index (RBI)
pointCloud_DT$RBI <- (pointCloud_DT$RED / pointCloud_DT$BLU)


message("Reflectances re-factored, columns renamed, and indices calculated.")
(Sys.time() - ptm3)
message("----------------------------------------------------------------")

# ---

message("Sub-setting data.table and applying RF model to predict discrete class...")
ptm4 <- Sys.time()

# Subset only necessary data (reflectance and indices)

pointCloudData_predictors <- pointCloud_DT[, c(16:20, 22:29)] # col 21 is treeID

# ---


# Apply RF model to pointCloud_DT using `pointCloudData_predictors` 

# RF model predicting each point as a discrete class
pointCloud_DT$RF_RBI_NDVI_REDEDGE <- as.integer(predict(rf_model_RBI_NDVI_REDEDGE, pointCloudData_predictors))

message("RF classification of discrete class complete.")
(Sys.time() - ptm4)
message("----------------------------------------------------------------")

# ---

message("Applying RF model to calculate class probabilities...")
ptm5 <- Sys.time()

# RF model predicting each point with class probabilities
class_probs_PointCloud_RBI_NDVI_REDEDGE <- predict(rf_model_RBI_NDVI_REDEDGE, pointCloudData_predictors, type = "prob") # creates a matrix where nrow = number of points (observations) and ncol = number of classes and their probability for each observation

# Rename the columns

colnames(class_probs_PointCloud_RBI_NDVI_REDEDGE) <- paste0("Class_", 1:ncol(class_probs_PointCloud_RBI_NDVI_REDEDGE), "_prob")


# temporary data.table to calculate the maximum probability for each observation
tempDT <- data.table(class_probs_PointCloud_RBI_NDVI_REDEDGE) # create copy
tempDT[, "RBI_NDVI_REDEDGE_max_prob" := do.call(pmax, .SD), .SDcols = c("Class_1_prob", "Class_2_prob", "Class_3_prob", "Class_4_prob")]

# add max probability to point cloud data.table
pointCloud_DT <- cbind(pointCloud_DT, tempDT$RBI_NDVI_REDEDGE_max_prob)

# rename column name
setnames(pointCloud_DT, old = names(pointCloud_DT)[ncol(pointCloud_DT)], new = "RBI_NDVI_REDEDGE_max_prob")


message("RF prediction of class probability complete.")
(Sys.time() - ptm5)
message("----------------------------------------------------------------")

# ---

# Housekeeping: removing R-objects not needed any further
message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
message("HOUSEKEEPING: removing temporary files and executing garbage collection...")
rm(pointCloud_obj)
rm(tempDT)
rm(class_probs_PointCloud_RBI_NDVI_REDEDGE)
gc()
message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# -------------------------------------------------------------------------

message("Importing 'clean' point cloud to manually add RF classification and max class probability...")
ptm6 <- Sys.time()

# Import a "clean" point cloud file and add the Random forest classification and class probabilities to the point attributes

clean_pointCloudCopy_fullPath <- file.path(dataPATH, pointCloud_fileName)

clean_pointCloudCopy_Obj <- lidR::readUAVLAS(clean_pointCloudCopy_fullPath)
lidR::st_crs(clean_pointCloudCopy_Obj) <- 26912

clean_pointCloudCopy_DT <- clean_pointCloudCopy_Obj@data


# --- 

message("'Clean' point cloud imported.")
(Sys.time() - ptm6)
message("----------------------------------------------------------------")

message("Preparing point cloud object for export...")
ptm7 <- Sys.time()

# Add RF classification (discrete class prediction) data
clean_pointCloudCopy_DT$RF_RBI_NDVI_REDEDGE <- pointCloud_DT$RF_RBI_NDVI_REDEDGE

# Add RF classification probability data
clean_pointCloudCopy_DT$RBI_NDVI_REDEDGE_max_prob <- pointCloud_DT$RBI_NDVI_REDEDGE_max_prob

message("--> RF classifications added.")

message("--> Freeing up space from the `Extra Bytes Slots`:")

# Free up space from the `Extra Bytes Slots` of the imported "clean" point cloud LAS-Class R-object

message("----> Removing reflectances from data.table")
# remove reflectances from data table
clean_pointCloudCopy_DT <- clean_pointCloudCopy_DT[, -(16:20)]

message("----> Removing reflectance attribute from point cloud object")
# remove reflectances from point cloud file
clean_pointCloudCopy_Obj <- remove_lasattribute(clean_pointCloudCopy_Obj, "Scalar__field")
clean_pointCloudCopy_Obj <- remove_lasattribute(clean_pointCloudCopy_Obj, "Scalar__field__#2")
clean_pointCloudCopy_Obj <- remove_lasattribute(clean_pointCloudCopy_Obj, "Scalar__field__#3")
clean_pointCloudCopy_Obj <- remove_lasattribute(clean_pointCloudCopy_Obj, "Scalar__field__#4")
clean_pointCloudCopy_Obj <- remove_lasattribute(clean_pointCloudCopy_Obj, "Scalar__field__#5")

message("----> Adding RF classification and max probability to `@data` slot of 'clean' point cloud...")
# Add data table back into point cloud file 
clean_pointCloudCopy_Obj@data <- clean_pointCloudCopy_DT

# --- 

message("--> Updating LAS-header file...")
# Update header file of point cloud object to export

# ****************       CHANGE VARIABLES HERE       ****************                       
# * Specify the predictor variables for the RF model to export in the LAS header file

clean_pointCloudCopy_Obj <- add_lasattribute(clean_pointCloudCopy_Obj, name = "RF_RBI_NDVI_REDEDGE", desc = "RF RBI + NDVI + REDEDGE")

clean_pointCloudCopy_Obj <- add_lasattribute(clean_pointCloudCopy_Obj, name = "RBI_NDVI_REDEDGE_max_prob", desc = "RF RBI_NDVI_REDEDGE max prob")

# --- 

message("Exporting point cloud to the EXPORTS folder...")
# Export point cloud object 

pointCloud_exportPath <- file.path(exportPATH, pointCloud_exportName)

lidR::writeLAS(clean_pointCloudCopy_Obj, pointCloud_exportPath)

message("RF classified point cloud successfully exported.")
(Sys.time() - ptm7)
message("----------------------------------------------------------------\n")

message("Total time taken:")
(Sys.time() - start_ptm)
message("!=========================================================================!")
message("!                        SCRIPT RUN SUCCESSFULLY                          !")
message("!=========================================================================!")