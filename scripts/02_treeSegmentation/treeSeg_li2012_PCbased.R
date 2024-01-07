###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s): 
# - Point cloud-based segmentation
#   + Using li2012 algorithm from the lidR package

### Last update: 12/04/2023

###########################################################################

# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
message("Importing libraries...")
start_ptm <- Sys.time()

require(lidR)
require(terra)
require(ForestTools)
require(sf)
require(sp)
require(rgl)
require(rgdal)

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

pointCloud_fileName = "M2_subset_heightNormalized_PC.las" # INPUT

pointCloud_ExportfileName = "M2_subset_li2012_treeSeg.las" # OUTPUT

treeSegmentation_ExportfileName = "M2_subset_li2012_treeSeg_noNA_noDuplicates.shp" # OUTPUT


# Set variables for script

CRS_epsg <- 26912 # set CRS with EPSG code


# Set variables for tree segmentation algorithm. See lidR manual and li et al (2012) for more detail

DT_1        = 0.75  # distance 1; see reference page 79 in Li et al. (2012)
DT_2        = 0.75  # distance 2; see reference page 79 in Li et al. (2012)
speedUp    = 10    # maximum radius of crown
searchRad   = 0     # search radius
height_min  = 1.35  # minimum height of tree


message("------------------------------------------------------------")



# =======================       SCRIPT START        =======================


message("STARTING TREE SEGMENTATION SCRIPT")
message("Importing point cloud data...")
ptm1 <- Sys.time()

# Import data, remove ground and any points with Z < 0

pointCloud_fullPath = file.path(dataPATH, pointCloud_fileName)

pointCloud_obj      = lidR::readUAVLAS(pointCloud_fullPath)
lidR::st_crs(pointCloud_obj) <- CRS_epsg # set CRS

pointCloud_obj      = lidR::filter_poi(pointCloud_obj, Z >= 0)

pointCloud_obj      = lidR::filter_poi(pointCloud_obj, Classification != 2)

message("Point cloud pre-processing complete.")
(Sys.time() - ptm1)

message("------------------------------------------------------------")

# Direct PC segmentation using li2012 algorithm 

message("Starting point cloud segmentation... ")
ptm1 <- Sys.time()

treeSegmentedPC_li2012 <- segment_trees(pointCloud_obj, li2012(dt1 = DT_1, 
                                                                dt2 = DT_2, 
                                                                speed_up = speedUp, 
                                                                R = searchRad, 
                                                                hmin = height_min))  


message("Point cloud segmentation complete.")
(Sys.time() - ptm1)
message("------------------------------------------------------------")

message(" Exporting segmented point cloud...")
ptm2 <- Sys.time()

lidR::writeLAS(treeSegmentedPC_li2012, file.path(exportPATH, pointCloud_ExportfileName))


message("Point cloud exported sucessfully to the EXPORTS folder.")
(Sys.time() - ptm2)

message("------------------------------------------------------------")

message("Preparing point cloud for tree crown segmentation...")

message("--> Removing duplicate points from the point cloud")

ptm3 <- Sys.time()

treeSegmentedPC_li2012_noNA <- lidR::filter_duplicates(treeSegmentedPC_li2012)

message("--> Removing treeIDs labeled as 'NA'...")
treeSegmentedPC_li2012_noNA <- lidR::filter_poi(treeSegmentedPC_li2012_noNA, !is.na(treeID))


lidR::las_check(treeSegmentedPC_li2012_noNA)

message("Exporting tree segmented point cloud without duplicates or NAs to EXPORTS folder...")

lidR::writeLAS(treeSegmentedPC_li2012_noNA, file.path(exportPATH, pointCloud_ExportfileName))

message("Export complete.")
(Sys.time() - ptm3)

message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
message("HOUSEKEEPING: removing temporary files and executing garbage collection...")

rm(treeSegmentedPC_li2012)
gc()

message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

message("Starting individual tree crown delineation algorithm...")
ptm4 <- Sys.time()

# Individual tree crown delineation from tree segmented point cloud
treeCrowns_li2012 <- lidR::crown_metrics(treeSegmentedPC_li2012_noNA, 
                                            func = ~list(z_max = max(Z)), 
                                            geom = "concave", 
                                            concaveman = c(1.5, 0))





sf::st_crs(treeCrowns_li2012) <- paste0("espg:", CRS_epsg) # set crs

# plot(treeCrowns_li2012$geometry) # for plotting

# Convert sf to vect to export as .shp
treeCrowns_li2012_vect <- terra::vect(treeCrowns_li2012)    

terra::writeVector(treeCrowns_li2012_vect, file.path(exportPATH, treeSegmentation_ExportfileName))

message("Individual tree crown delineation complete.")
message("ITCD shapefile exported sucessfully to the EXPORTS folder.")

(Sys.time() - ptm4)

message("------------------------------------------------------------")

message("Total time taken:")
(Sys.time() - start_ptm)

message("!==================================================================!")
message("!                   Script run successfully!                       !")
message("!==================================================================!")

# =======================       SCRIPT END         =======================