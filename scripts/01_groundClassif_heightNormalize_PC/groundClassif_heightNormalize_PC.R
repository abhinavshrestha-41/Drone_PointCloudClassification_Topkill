###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s): 
# - Ground classification of point cloud data set
# - Height normalization of ground classified point cloud data set

### Last update: 12/04/2023

###########################################################################

# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
message("Importing libraries...")
start_ptm <- Sys.time()


library(lidR)
library(terra)
library(sf)
library(stars)
# =========================================================================



# =========================================================================

#                           CHANGE VARIABLES HERE

# =========================================================================


# ------------------------- SET DIR, PATH, FILE ---------------------------

message("Initializing workspace paths...")

# Initialize workspace 

# setwd("PATH/TO/DIR")

dir = getwd()

dataPATH = file.path(dir, "DATA")

exportPATH = file.path(dir, "EXPORTS")

pointCloud_fileName = "M2_subset_PointCloud.las" # INPUT .LAS file

pointCloud_groundClassifExportName = "M2_subset_groundClassif_PC.las"

pointCloud_heightNormalizedExportName = "M2_subset_heightNormalized_PC.las"


# --------- SET THRESHOLDS FOR GROUND CLASSIFICATION ALGORITHM ------------

# Sequence of windows sizes
ws <- seq(0.75, 3, 0.75) # default is (3,12,3), decreased to better classify ground points for subset
message("--> Ground classification moving window size set to: (", ws[1], ", ", ws[2], ", ", ws[3], ")")

# Sequence of height thresholds
th <- seq(0.1, 2.0, length.out = length(ws))
message("--> Height thresholds for ground/non-ground classification: ", list(th))


# =========================================================================



# =======================       SCRIPT START        =======================

message("------------------------------------------------------------")

message("STARTING GROUND CLASSIFICATION AND HEIGHT NORMALIZATION SCRIPT")

message("Importing point cloud data...")

ptm0 <- Sys.time() # start time

# Import point cloud data

pointCloud_fullPath = file.path(dataPATH, pointCloud_fileName)

pointCloud_obj      = lidR::readUAVLAS(pointCloud_fullPath)

message("Point cloud import complete.")
(Sys.time() - ptm0)

message("------------------------------------------------------------")

# Variables are set according to suggestions made by Mohal et al. (2021) for individual tree detection and crown delineation using SfM photogrammetric point clouds
# https://www.degruyter.com/document/doi/10.1515/geo-2020-0290/html?lang=en 

message("STEP 1: GROUND CLASSIFICATION OF POINT CLOUD")
ptm1 <- Sys.time()


message("--> Starting ground classification operation...")
message("----> Using the `classify_ground()` function with a progressive morphological filter algorithm (`pmf()`).")
groundClassif_ptm <- Sys.time()

# Run ground classification algorithm
groundClassifPC_obj <- lidR::classify_ground(pointCloud_obj, pmf(ws, th))

message("Ground classification complete.")
Sys.time() - groundClassif_ptm
message("---")

message("Exporting ground classified point cloud to the EXPORTS folder.")
groundClassifExport_ptm <- Sys.time()

# Export ground classified point cloud
lidR::writeLAS(groundClassifPC_obj, file.path(exportPATH, pointCloud_groundClassifExportName))

message("Ground classified point cloud export complete.")
Sys.time() - groundClassifExport_ptm
message("---")
message("Total time taken for ground classification step:")
Sys.time() - ptm1
message("------------------------------------------------------------\n")

message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
message("HOUSEKEEPING: removing temporary files and executing garbage collection...")
rm(pointCloud_obj)
gc()
message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

message("STEP 2: HEIGHT NORMALIZATION OF POINT CLOUD")
ptm2 <- Sys.time()


message("-->Starting height normalization operation on point cloud...")
message("----> Using the `normalize_height()` function using the K-nearest neighbor with inverse distance weighting algorithm (`knnidw()`).")
heightNorm_ptm <- Sys.time()

heightNormPC_obj <- lidR::normalize_height(groundClassifPC_obj, knnidw())

message("Height normalization complete.")
Sys.time() - heightNorm_ptm
message("---")

message("Exporting height normalized point cloud to the EXPORTS folder.")
heightNormExport_ptm <- Sys.time()

lidR::writeLAS(heightNormPC_obj, file.path(exportPATH, pointCloud_heightNormalizedExportName))

message("Height normalized point cloud export complete.")
Sys.time() - heightNormExport_ptm
message("---")
message("Total time taken for height normalization step:")
Sys.time() - ptm2
message("------------------------------------------------------------\n")

message("Total time taken to execute script: ")
(Sys.time() - start_ptm)

message("!==================================================================!")
message("!                   Script run successfully!                       !")
message("!==================================================================!")


# =======================       SCRIPT END         =======================