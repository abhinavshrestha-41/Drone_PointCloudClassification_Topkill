###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s):
# * Spatially aggregate tree-level damage metrics to specified grid cell size
# * Spatial aggregation includes computing the mean of: percent damage, percent top-kill, length of top-kill; and mode of damage type of trees that fall within the grid cell (of set size)


### Last update: 12/05/23

###########################################################################

# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
start_ptm <- Sys.time()


library(terra)
library(lidR)
library(tidyr)
library(sf)
library(DescTools)
library(RColorBrewer)


# =========================================================================

#                           CHANGE VARIABLES HERE

# =========================================================================


# ------------------------- SET DIR, PATH, FILE ---------------------------
message("Initializing workspace and paths...")

# setwd("PATH/TO/DIR")

dir <- getwd()

dataPATH    <- file.path(dir, "DATA")
exportPATH  <- file.path(dir, "EXPORTS") 

# INPUT
inputTreeDamage_shpName = "M2_subset_TreeDamageAlgo_TreeSegPolygons.shp"

# EXPORT
rasterExportName = "M2_subset_modalDamageType_10m.tif"

# ------------------------- SET SCRIPT PARAMETERS ---------------------------

# Set what to spatially aggregate: 
# - by damage type (mode) = 1
# - by damage percent (mean) = 2
# - by top-kill length (mean) = 3
# - by top-kill percent (mean) = 4

spatAgg_category <- 1

# SPATIAL RESOLUTION (m)
delta_xy = 10

# =========================================================================

#                    SPATIAL AGGREGATION SCRIPT (START)

# =========================================================================


# Import tree crowns (polygon)
crown_poly_shp <- terra::vect(file.path(exportPATH, inputTreeDamage_shpName))

# Filter NAs
damageLabelList <- c("Healthy", "Minor damage", "Moderate damage", "Major damage", "Dead (red)", "Dead (gray)", "Dead (mixed)")

crown_poly_shp <- terra::subset(crown_poly_shp, crown_poly_shp$damageType %in% damageLabelList)

# Convert damage labels to numbers
damageType_dict <- c("Healthy"           = 1, 
                     "Minor damage"      = 2, 
                     "Moderate damage"   = 3, 
                     "Major damage"      = 4, 
                     "Dead (red)"        = 5, 
                     "Dead (gray)"       = 6,
                     "Dead (mixed)"      = 7)


# Convert shapefile attributes to data table
crown_poly_DT <- data.table::as.data.table(crown_poly_shp)

# Ensure top-kill length and percent values are numbers (not characters)
crown_poly_DT$topKill_pe <- as.numeric(crown_poly_DT$topKill_pe)
crown_poly_DT$topKill_am <- as.numeric(crown_poly_DT$topKill_am)

# Add new column to data table and recode damage type to numbers
crown_poly_DT$damageType_NumCode <- damageType_dict[crown_poly_DT$damageType]

# Merge the two data tables based on the 'treeID' column
crown_poly_DamageCode_shp <- merge(crown_poly_shp, crown_poly_DT[, .(treeID, damageType_NumCode)], by = "treeID", all.x = TRUE) # 'all.x = TRUE' ensures that all rows from 'spatVect' are retained, and missing values are filled with NA for the 'damageType_NumCode' column


# Convert tree crown polygon to point (centroid)
crown_point_shp <- terra::centroids(crown_poly_DamageCode_shp, TRUE)


# FOR Troubleshooting/exploring
# treeID_list <- c(1, 3, 4, 5, 6)
# 
# crown_poly_shp_sub <- terra::subset(crown_poly_shp, crown_poly_shp$treeID %in% treeID_list)
# crown_point_shp_sub <- terra::subset(crown_point_shp, crown_point_shp$treeID %in% treeID_list)
# 
# terra::plot(crown_poly_shp_sub)
# terra::points(crown_point_shp_sub)

# Create "fishnet" polygon grid, from Jeff

# ---------------------------------------------------------------------
# Create bounding box of the region of interest
# ---------------------------------------------------------------------

# Get extent of shapefile 

shpFile_extent <- terra::ext(crown_poly_shp)
beg_x <- shpFile_extent[1] #xmin
end_x <- shpFile_extent[2] #xmax
beg_y <- shpFile_extent[3] #ymin
end_y <- shpFile_extent[4] #ymax
crs_shpFile <- terra::crs(crown_poly_shp)


studyArea_bounding_box <- matrix(c(beg_x,end_y,
                                    end_x,end_y,
                                    end_x,beg_y,
                                    beg_x,beg_y,
                                    beg_x,end_y),
                                  byrow = TRUE, ncol = 2) %>%
                                    list() %>%
                                    sf::st_polygon() %>%
                                    sf::st_sfc(., crs = crs_shpFile)


# ---------------------------------------------------------------------
# Generate the fishnet
# ---------------------------------------------------------------------

# below produces polygon[1,1] = 1, polygon[1886,2216] = 4179376
#                polygons are embedded in a matrix of [1886,2216]



studyArea_fishnet_polygons <- sf::st_make_grid(studyArea_bounding_box, cellsize = c(delta_xy, delta_xy),
                                            crs = crs_shpFile, what = 'polygons') %>%
    sf::st_sf('geometry' = ., data.frame('GRIDCELL_ID' = 1:length(.))) # adds GRIDCELL_ID unique to each grid cell/polgyon

# terra::plot(crown_poly_shp)
# terra::plot(studyArea_bounding_box, col = "transparent", add = TRUE)
# terra::plot(studyArea_fishnet_polygons, col = "transparent", add = TRUE)

# ---------------------------------------------------------------------
# create a raster with same extent, size as studyArea_fishnet_polygons
# ---------------------------------------------------------------------

studyArea_fishnet_polygons_Extent <- sf::st_bbox(studyArea_fishnet_polygons)

num_grid_cells_x <- round((studyArea_fishnet_polygons_Extent$xmax - studyArea_fishnet_polygons_Extent$xmin) / delta_xy)
num_grid_cells_y <- round((studyArea_fishnet_polygons_Extent$ymax - studyArea_fishnet_polygons_Extent$ymin) / delta_xy)


# SpatRaster has x = columns (second dimension), y = rows (first dimension)
studyArea_grid_SpatRaster = terra::rast(nrows=num_grid_cells_y,
                                        ncols=num_grid_cells_x,
                                        xmin=studyArea_fishnet_polygons_Extent$xmin, 
                                        xmax=studyArea_fishnet_polygons_Extent$xmax, 
                                        ymin=studyArea_fishnet_polygons_Extent$ymin, 
                                        ymax=studyArea_fishnet_polygons_Extent$ymax,
                                        resolution=delta_xy, 
                                        vals=studyArea_fishnet_polygons$GRIDCELL_ID,
                                        crs=crs_shpFile)


img <- as.matrix(studyArea_grid_SpatRaster, wide = T)


# FROM JEFF:
transpose_y_rev <- function(input_matrix) {
    
    # 
    # PURPOSE:  Transpose and reverse in y (2nd) dimension a 2D array (matrix)
    # 
    # Inputs:  input_matrix    matrix (=2D array)
    # 
    # Returns:  modified matrix (2D array)
    # 
    
    return(apply(apply(apply(input_matrix,1,rev),1,rev),1,rev)) 
    
}

# FROM JEFF:
SpatRaster_values_to_matrix <- function(raster_obj) {
    
    # 
    # PURPOSE:  Convert a 2D SpatRaster (terra package) raster into a matrix (2D array) whose origin is in lower left
    # 
    # Inputs:  raster_obj    2D SpatRaster
    # 
    # Returns:  matrix (2D array) of SpatRaster values
    # 
    
    return(transpose_y_rev(as.matrix(raster_obj,wide=T)))
    
}


img <- SpatRaster_values_to_matrix(studyArea_grid_SpatRaster)

# FROM JEFF:
matrix_to_SpatRaster_values <- function(input_matrix,input_raster_obj) {
   
# 
# PURPOSE:  Insert values of a matrix (2D array) into a 2D SpatRaster (terra package) raster
# 
# Inputs:  input_matrix        matrix with values
# Inputs:  input_raster_obj    2D SpatRaster where values should be put
# 
# Returns: modified input_raster_obj
# 

   values(input_raster_obj) = t(
                                apply(apply(apply(t(input_matrix),1,rev),1,rev),1,rev) # reverses in y dimension
                               )

   return(input_raster_obj)

}

# ----------------------------------------------------------------------------------------------------------------------

#       MERGE TREE CENTROIDS + FISHNET POLYGON; SAMPLE MODE/MEAN DAMAGE METRICS OF CENTROIDS IN CELL BY GRIDCELL ID

# ----------------------------------------------------------------------------------------------------------------------

# Intersect centroids to fishnet grid
studyArea_fishnet_polygons_spatVect <- terra::vect(studyArea_fishnet_polygons)
merged_fishnet_toCentroids <- terra::intersect(crown_point_shp, studyArea_fishnet_polygons_spatVect)
merged_centroids_toFishnet <-  terra::intersect(studyArea_fishnet_polygons_spatVect, crown_point_shp)



# Convert to data table
merged_fishnet_centroids_DT <- data.table::as.data.table(merged_fishnet_toCentroids)

# Ensure top-kill length and percent values are numbers (not characters)
merged_fishnet_centroids_DT$topKill_pe <- as.numeric(merged_fishnet_centroids_DT$topKill_pe)
merged_fishnet_centroids_DT$topKill_am <- as.numeric(merged_fishnet_centroids_DT$topKill_am)


if (spatAgg_category == 1) {
    
    # Compute modal damage type by grid cell ID
    for (i in c(1:range(studyArea_fishnet_polygons_spatVect)[2])){
        
        subsetDT <- merged_fishnet_centroids_DT[which(merged_fishnet_centroids_DT$GRIDCELL_ID == i), ]
        
        # catch empty data.table (if no intersect between point and polygon), forward to next iteration
        if (nrow(subsetDT) == 0){
            
            img[i] <- NA
            
            next
            
        }
        
        modalDamageType <- DescTools::Mode(subsetDT$damageType_NumCode)
        
        img[i] <- modalDamageType[1]
    }
    
    studyArea_grid_SpatRaster <- matrix_to_SpatRaster_values(img, studyArea_grid_SpatRaster)
    
} else if (spatAgg_category == 2) {
    
    # Compute modal damage type by grid cell ID
    for (i in c(1:range(studyArea_fishnet_polygons_spatVect)[2])){
        
        subsetDT <- merged_fishnet_centroids_DT[which(merged_fishnet_centroids_DT$GRIDCELL_ID == i), ]
        
        # catch empty data.table (if no intersect between point and polygon), forward to next iteration
        if (nrow(subsetDT) == 0){
            
            img[i] <- NA
            
            next
            
        }
        
        mean_percDamage_byGridCell <- mean(subsetDT$perc_Damag)
        
        img[i] <- mean_percDamage_byGridCell[1]
    }
    
    studyArea_grid_SpatRaster <- matrix_to_SpatRaster_values(img, studyArea_grid_SpatRaster)

} else if (spatAgg_category == 3) {
    
    # Compute modal damage type by grid cell ID
    for (i in c(1:range(studyArea_fishnet_polygons_spatVect)[2])){
        
        subsetDT <- merged_fishnet_centroids_DT[which(merged_fishnet_centroids_DT$GRIDCELL_ID == i), ]
        
        # catch empty data.table (if no intersect between point and polygon), forward to next iteration
        if (nrow(subsetDT) == 0){
            
            img[i] <- NA
            
            next
            
        }
        
        mean_topkill_length_byGridCell <- mean(as.numeric(subsetDT$topKill_am))
        
        img[i] <- mean_topkill_length_byGridCell[1]
    }
    
    studyArea_grid_SpatRaster <- matrix_to_SpatRaster_values(img, studyArea_grid_SpatRaster) 
    
} else if (spatAgg_category == 4) {
    
    # Compute modal damage type by grid cell ID
    for (i in c(1:range(studyArea_fishnet_polygons_spatVect)[2])){
        
        subsetDT <- merged_fishnet_centroids_DT[which(merged_fishnet_centroids_DT$GRIDCELL_ID == i), ]
        
        # catch empty data.table (if no intersect between point and polygon), forward to next iteration
        if (nrow(subsetDT) == 0){
            
            img[i] <- NA
            
            next
            
        }
        
        mean_topkill_perc_byGridCell <- mean(subsetDT$topKill_pe)
        
        img[i] <- mean_topkill_perc_byGridCell[1]
    }
    
    studyArea_grid_SpatRaster <- matrix_to_SpatRaster_values(img, studyArea_grid_SpatRaster) 
    
}



terra::writeRaster(studyArea_grid_SpatRaster, file.path(exportPATH, rasterExportName), overwrite=TRUE)


# FOR PLOTING:

# raster_values <- c(sort(unique(values(studyArea_grid_SpatRaster)))) # Sample raster values (replace with your actual raster values)

# damageType_colPalette <- c("1" = "darkgreen",
#                            "2" = "darkblue",
#                            "3" = "orange",
#                            "4" = "red",
#                            "5" = "darkorange",
#                            "6" = "darkred",
#                            "7" = "darkgray")

# raster_colors <- damageType_colPalette[as.character(raster_values)] # Map raster values to colors using the palette

# PLOT
# terra::plot(studyArea_grid_SpatRaster, col = raster_colors)

# terra::plot(studyArea_grid_SpatRaster, col = RColorBrewer::brewer.pal(n = 5, name = "OrRd"))