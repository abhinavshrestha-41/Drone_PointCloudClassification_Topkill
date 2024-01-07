###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: shre9292@vandals.uidaho.edu

### Purpose(s): 
# * filter reference trees and export as point cloud
# * reference trees are snags and top-kill

### Last update: 07/15/2023

###########################################################################

gc()

# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
start_ptm <- Sys.time()

require(tidyr)
require(dplyr)
require(terra)
# require(ggplot2)
require(lidR)
require(sf)
# require(sp)
# require(ggrepel)
# require(randomForest)
# require(rfUtilities)
require(data.table)
# require(caret)
# require(rgdal)
#source("errorAccuracyOutputs_function.R") # make sure this file is in the working directory


# =========================================================================

# -------------------------------------------------------------------------

# Initialize PATH, DIR, FILE

# -------------------------------------------------------------------------
#setwd("C:\\Users\\abhin\\Documents\\GitHub\\MS_Project_Scripts\\MS_Project_Workspace\\R_Workspace\\R_Scripts\\assessDamage_RFClassifPC")


dir  = getwd()

dataPATH = file.path(dir, "DATA")

exportPATH = file.path(dir, "EXPORTS")

# -------------------------------------------------------------------------

# load point shapefile with reference tree IDs 

snag_vect <- terra::vect(file.path(dataPATH, "refSnags.shp"))
snag_treeIDlist <- c(snag_vect$NAME)

topkill_vect <- terra::vect(file.path(dataPATH, "refTopkill.shp"))
topkill_treeIDlist <- c(topkill_vect$NAME)

# -------------------------------------------------------------------------

# Load point cloud file 

pointCloud_fileName <- "M2_li2012_RFClassif_maxProb.las"

pointCloud_fullPath <- file.path(dataPATH, pointCloud_fileName)

pointCloudObj <- lidR::readUAVLAS(pointCloud_fullPath)

# -------------------------------------------------------------------------

# filter by treeID

refSnag_PC <- filter_poi(pointCloudObj, treeID %in% snag_treeIDlist)

refTopkill_PC <- filter_poi(pointCloudObj, treeID %in% topkill_treeIDlist)

# -------------------------------------------------------------------------

rm(pointCloudObj)
gc()

# Export filtered PC 

lidR::writeLAS(refSnag_PC, file.path(exportPATH, "M2_refSnag_RFClassif.las"))
lidR::writeLAS(refTopkill_PC, file.path(exportPATH, "M2_refTopkill_RFClassif.las"))

Sys.time() - start_ptm


# lidR::plot(lidR::filter_poi(refSnag_PC, treeID == 1187),
#            color = "RF_RBI_NDVI_REDEDGE",
#            size = 6,
#            shape = 2,
#            pal = colorRampPalette(c("darkgreen", "gray", "tomato", "black")),
#            bg = "white")
