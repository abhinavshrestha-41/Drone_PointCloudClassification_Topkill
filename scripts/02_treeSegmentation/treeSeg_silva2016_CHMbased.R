###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s): 
# - Segment trees based on CHM (raster-based)
#   + Integrate CHM located trees to point cloud segmentation
#   + Algorithm uses silva2016 algorithm from the lidR package

### Last update: 07/10/2023

###########################################################################

# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
message("Importing libraries...")

start_ptm <- Sys.time()

require(lidR)
require(terra)
require(stars)
# require(ForestTools)
require(sf)
# require(sp)
require(rgl)
# require(rgdal)
require(future)
require(future.apply)

# =========================================================================

#                           CHANGE VARIABLES HERE

# =========================================================================


# ------------------------- SET DIR, PATH, FILE ---------------------------
message("Initializing workspace paths...")

# Initialize dir and paths

# setwd("PATH/TO/DIR")

dir = getwd()

dataPATH = file.path(dir, "DATA")

exportPATH = file.path(dir, "EXPORTS")

pointCloud_fileName = "M1_MS_heightNormalized_PC.las" # INPUT

pointCloud_ExportfileName = "M1_silva2016_lmf_treeSeg_0.5sCHM.las" # OUTPUT

treeSegmentation_ExportfileName = "M1_silva2016_lmf_treeSeg_noNA_noDuplicates_0.5sCHM.las" # OUTPUT

# EXPORT CHM? 
exportCHM = "no" # yes/no
exportCHM_name = "M1_MS_CHM.tif"


# Set variables for script

CRS_epsg <- 26912 # set CRS with EPSG code


# Set variables for tree segmentation algorithm. See lidR manual and silva et al (2016) for more detail

# RASTERIZE_CANOPY function to create CHM

rasterRes       = 0.5   # resolution of output CHM
subCircleRadius = 0     # Radius of the circles

# the rasterize canopy is set to p2r ("point to raster") algorithm with the na.fill parameter set to knnidw. The knnidw has the following parameters:                
knn_num     = 2 # Number of k-nearest neighbours
idw_power   = 2 # Power for inverse-distance weighting


# Smoothing CHM
smoothingWindow <- matrix(1, 3, 3) #set smoothing window

# Fixed window local maxima filter variables for tree segmentation algorithm
windowSize = 3 # diameter of moving search window
minHgt = 0 # minimum height of tree
windowShape = "circular" # "circular" or "square"



message("------------------------------------------------------------")

# =======================       SCRIPT START        =======================

message("STARTING TREE SEGMENTATION SCRIPT")
message("Importing point cloud data...")
ptm1 <- Sys.time()

# Import data, remove ground and any points with Z < 0

pointCloud_fullPath = file.path(dataPATH, pointCloud_fileName)

pointCloud_obj      = lidR::readUAVLAS(pointCloud_fullPath)
lidR::st_crs(pointCloud_obj) <- 26912 # set CRS

message("Point cloud imported. Pre-processing point cloud: filtering any points with Z value under 0...")
pointCloud_obj      = lidR::filter_poi(pointCloud_obj, Z >= 0)

message("Filtering any points with ground classification")
pointCloud_obj      = lidR::filter_poi(pointCloud_obj, Classification != 2)

message("Point cloud pre-processing complete.")
(Sys.time() - ptm1)
message("------------------------------------------------------------")

# =========================================================================

message("Creating canopy height model (CHM)...")
ptm2 <- Sys.time()

# Create CHM 
chm_rast <- lidR::rasterize_canopy(pointCloud_obj, 
                                   res = rasterRes, 
                                   lidR::p2r(na.fill = lidR::knnidw(k = knn_num, 
                                                                    p = idw_power), 
                                            subcircle = subCircleRadius))
terra::crs(chm_rast) <- paste0("espg:", CRS_epsg) # set crs

message("CHM generated")

if (exportCHM == "yes") {
    
    message("Exporting CHM to EXPORTS directory")
    terra::writeRaster(chm_rast, file.path(exportPATH, exportCHM_name))
}


(Sys.time() - ptm2)
message("------------------------------------------------------------")

message("Smoothing CHM...")
ptm3 <- Sys.time()


sCHM_rast <- terra::focal(chm_rast, smoothingWindow, fun = mean, na.rm = TRUE)

message("Smoothed CHM (sCHM) generated.")
(Sys.time() - ptm3)
message("------------------------------------------------------------")

message("Locating tree tops using fixed window local maxima filter on sCHM...")
ptm4 <- Sys.time()


# Fixed window local maxima filter (LMF)
ttops_lmf_sCHM <- lidR::locate_trees(sCHM_rast, lidR::lmf(ws = windowSize,
                                                            hmin = minHgt, 
                                                            shape = windowShape))


message("Tree tops located with sCHM!")

(Sys.time() - ptm4)
message("------------------------------------------------------------")

message("Using located tree tops to segment point cloud...")
ptm5 <- Sys.time()

# set CHM-based algorithm in an R-object
algo_lmf <- lidR::silva2016(sCHM_rast, ttops_lmf_sCHM)

# Segement point cloud
treeSegmentedPC_silva2016_lmf <- lidR::segment_trees(pointCloud_obj, algo_lmf)


message("Point cloud segmented to individual trees using tree tops located from sCHM.")
(Sys.time() - ptm5)

message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
message("HOUSEKEEPING: removing temporary files and running garbage collection...")

rm(chm_rast)
rm(sCHM_rast)
rm(ttops_lmf_sCHM)
gc()

message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

message("Exporting point cloud to EXPORTS folder")
ptm6 <- Sys.time()

lidR::writeLAS(treeSegmentedPC_silva2016_lmf, file.path(exportPATH, pointCloud_ExportfileName))

message("Tree segmented point cloud exported successfully to the EXPORTS folder")
(Sys.time() - ptm6)
message("------------------------------------------------------------")

message("Starting individual tree crown delineation from segmented point cloud...")
ptm7 <- Sys.time()


message("Preparing point cloud for tree crown segmentation...")
message("--> Removing duplicate points from the point cloud")
treeSegmentedPC_silva2016_lmf_noNA <- lidR::filter_duplicates(treeSegmentedPC_silva2016_lmf)

message("--> Removing treeIDs labeled as 'NA'...")
treeSegmentedPC_silva2016_lmf_noNA <- lidR::filter_poi(treeSegmentedPC_silva2016_lmf_noNA, !is.na(treeID))

lidR::las_check(treeSegmentedPC_silva2016_lmf_noNA)

message("--> Exporting tree segmented point cloud without duplicates or NAs to EXPORTS folder...")
lidR::writeLAS(treeSegmentedPC_silva2016_lmf_noNA, file.path(exportPATH, pointCloud_ExportfileName))

message("Export complete.")
(Sys.time() - ptm7)

message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
message("HOUSEKEEPING: removing temporary files and executing garbage collection...")
rm(treeSegmentedPC_silva2016_lmf)
gc()
message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

message("Starting individual tree crown delineation algorithm...")
ptm8 <- Sys.time()

# Tree crown delineation

treeCrowns_silva2016_lmf <- lidR::crown_metrics(treeSegmentedPC_silva2016_lmf_noNA, 
                                            func = ~list(z_max = max(Z)), 
                                            geom = "concave", 
                                            concaveman = c(1.5, 0))


message("Individual tree crown delineation complete.")
(Sys.time() - ptm8)

message("Exporting as shapefile...")
ptm9 <- Sys.time()

sf::st_crs(treeCrowns_silva2016_lmf) <- paste0("espg:", CRS_epsg) # set crs

# Convert sf to vect to export as .shp
treeCrowns_silva2016_vect <- terra::vect(treeCrowns_silva2016_lmf)    

terra::writeVector(treeCrowns_silva2016_vect, file.path(exportPATH, treeSegmentation_ExportfileName))

message("ITCD shapefile exported sucessfully to the EXPORTS folder.")
(Sys.time() - ptm9)
message("------------------------------------------------------------")
message("Total time taken:")
(Sys.time() - start_ptm)
message("!==================================================================!")
message("!                   Script run successfully!                       !")
message("!==================================================================!")