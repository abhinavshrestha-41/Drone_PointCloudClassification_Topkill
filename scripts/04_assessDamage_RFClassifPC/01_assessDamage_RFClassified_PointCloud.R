###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s): 
# * Damage analysis of RF classified point cloud
# * Integration of damage metrics into data products 
# * Export data products

## CLASSIFICATION BASIS:

# '-' = 1st level, '--' = 2nd level, etc...

# ALL TREES
# - HEALTHY (<=5% DAMAGE)
# - DAMAGED (> 5% DAMAGE)
# -- MINOR DAMAGE (5-25% DAMAGE)
# -- MODERATE DAMAGE (26-75% DAMAGE)
# -- MAJOR DAMAGE (76-90% DAMAGE)
# -- DEAD (> 90% DAMAGE)
# --- RED (>75% RED)
# --- GRAY (>75% GRAY)
# --- MIXED (<75% RED AND <75% GRAY, BUT >90% DAMAGE)

### Last update: 12/05/23

###########################################################################

gc()

# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
start_ptm <- Sys.time()

library(tidyr)
library(dplyr)
library(terra)
#library(ggplot2)
library(lidR)
library(sf)
#library(sp)
# library(ggrepel)
library(randomForest)
library(rfUtilities)
library(data.table)
library(caret)
library(stars)
library(future)
library(future.apply)
#library(rgdal)
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

# FILE NAMES FOR LOADING AND EXPORTING 

pointCloud_fileName <- "M2_subset_li2012_RFClassif_maxProb.las"

treeSeg_shpName <- "M2_subset_li2012_treeSeg_noNA_noDuplicates.shp"

exportShp_name <- "M2_subset_TreeDamageAlgo_TreeSegPolygons.shp"

# ------------------ SET THRESHOLDS USED IN ALGORITHM  --------------------

# set fixed bin height
heightBin = 0.25

# Set percent damage threshold for damage analysis
damagePerc_topKill_bin2bin = 90 # for bin2bin
damagePerc_topKill_top2bin = 80 # for top2bin


# =========================================================================

#                       TREE DAMAGE ALGORITHM (START)

# =========================================================================

# -------------------------------------------------------------------------

# LOAD DATA AND INITIALIZE VARIABLES

# -------------------------------------------------------------------------

# Load point cloud file 
message("------------------------------------------------------------")
message("STARTING DAMAGE ASSESSMENT SCRIPT")

message("Importing RF classified point cloud...")
ptm1 <- Sys.time()

pointCloud_fullPath <- file.path(dataPATH, pointCloud_fileName)

pointCloudObj <- lidR::readUAVLAS(pointCloud_fullPath)
setnames(pointCloudObj@data, "RF_RBI_NDVI_REDEDGE", "RF_Class") # change RF classification column if not named as "RF_Class"

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


# create data table of point cloud file attributes 

message("--> Extracting point cloud attributes as data.table")

pointCloudObj_DT <- data.table(pointCloudObj_noGround_noShadows@data)

message("--> Creating empty data table to store damage assessment results")

# create data table to store damage assessment by treeID
damageAssess_DT <- data.table(treeID = numeric(), 
                              perc_Green = numeric(), 
                              perc_Gray = numeric(), 
                              perc_Red = numeric(), 
                              perc_Damage = numeric(),
                              totalPoints = numeric(), 
                              damageType = character(),
                              topKill_status = character(),
                              topKill_perc = numeric(), 
                              topKill_amount = numeric())



# Create a list of treeIDs to loop through
message("--> Creating a list of unique treeIDs")

treeID_list <- unique(pointCloudObj_DT$treeID[!is.na(pointCloudObj_DT$treeID)])


# Print thresholds used before perfoming executing algorithm
message("--> fixed height bin for damage assessment set as: ", heightBin)
message("--> damage percent threshold for 'bin2bin' algorithm set as: ", damagePerc_topKill_bin2bin)
message("--> damage percent threshold for 'top2bin' algorithm set as: ", damagePerc_topKill_top2bin)

# testTreeID <- 7 # For troubleshooting

# Store column numbers for the Z (height) and classification variables 
ZcolNum <- as.numeric(which(colnames(pointCloudObj_DT) == "Z"))
RFClassColNum <- as.numeric(which(colnames(pointCloudObj_DT) == "RF_Class"))




message("\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
message("HOUSEKEEPING: removing temporary files and running garbage collection...")

rm(pointCloudObj)
rm(pointCloudObj_noGround_noShadows)
gc()

message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n")

message("------------------------------------------------------------")



# ------------------------ LOOP BY EACH TREE (START) ------------------------

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
    tree_DT_RFClass <- pointCloudObj_DT[treeID == treeNum, c(..ZcolNum, ..RFClassColNum )] # DOUBLE CHECK THE COLUMN NUMBERS, data.table reads variables with preceding `..` as expressions
    
    # Initialize "damageStats" R-vector-object
    damageStats <- c("Damage_type"  = "NA",
                     "Topkill_status" = NA,
                    "Topkill_perc" = NA, 
                    "Topkill_amount" = NA)


    # Calculate classification statistics for individual tree under "treeStats" R-vector-object
    treeStats <- c("percGreen" = ((mean(tree_DT_RFClass$RF_Class == 1)) * 100),
                    "percGray" = ((mean(tree_DT_RFClass$RF_Class == 2)) * 100),
                    "percRed" = ((mean(tree_DT_RFClass$RF_Class == 3)) * 100), 
                    "percDamage" = ((mean(tree_DT_RFClass$RF_Class == 2)) * 100) + ((mean(tree_DT_RFClass$RF_Class == 3)) * 100), 
                   "totalPoints" = nrow(tree_DT_RFClass))

    # If the tree itself has less than 200 points total, insufficient to make assess the structure and damage on the tree, skips to next.
    if (treeStats[["totalPoints"]] <= 200) {
     # will pass the damageStats variable as NAs and treeStats as calculated 
      damageStats = damageStats
    } else {
      
# ----------------------------------------------------------------------------------------------------------------------
#                                                   HEALTHY VS DAMAGED                                         
# ----------------------------------------------------------------------------------------------------------------------
        
        
      # Minimal damaged (Healthy tree) sieve
      if (treeStats[["percDamage"]] <= 5) {                        
          
          damageStats <- c("Damage_type" = "Healthy",
                           "Topkill_status" = "Healthy",
                           "Topkill_perc" = 0, 
                           "Topkill_amount" = 0)
          
          message("Healthy tree -- no damage assessment. Moving to next tree.")     
          
      } else {

      # If not healthy, it could be a type of damaged tree: minor damage, moderate damage, or major damage (red, gray, other).
  
# ----------------------------------------------------------------------------------------------------------------------
#                                                   DIFFERENT DAMAGE TYPES                                         
# ----------------------------------------------------------------------------------------------------------------------
          
          
          if ((treeStats[["percDamage"]] > 5) & (treeStats[["percDamage"]] <= 25)) {
          
              # ------------
              # MINOR DAMAGE
              # ------------
              
              damageStats[["Damage_type"]] = "Minor damage" 
              
          } else if ((treeStats[["percDamage"]] > 25) & (treeStats[["percDamage"]] <= 75)) {
              
              # ---------------
              # MODERATE DAMAGE
              # ---------------
              
              damageStats[["Damage_type"]] = "Moderate damage"
              
          } else if ((treeStats[["percDamage"]] > 75) & (treeStats[["percDamage"]] <= 90)) {
              
              # ------------
              # MAJOR DAMAGE
              # ------------
              
              damageStats[["Damage_type"]] = "Major damage"


          } else if ((treeStats[["percDamage"]] > 90)) {

              # ------------
              # DEAD
              # ------------

              if ((treeStats[["percRed"]] >= 75)) {
                    
                    # ------------
                    # DEAD (red)
                    # ------------
                  
                  damageStats[["Damage_type"]] = "Dead (red)"      
                  
              } else if ((treeStats[["percGray"]] >= 75)) {

                    # ------------
                    # DEAD (gray)
                    # ------------
                  
                  damageStats[["Damage_type"]] = "Dead (gray)"
                  
              } else {
                  
                    # ------------
                    # DEAD (mixed)
                    # ------------

                  damageStats[["Damage_type"]] = "Dead (mixed)"
                  
              }
          }
          
          
          
# ----------------------------------------------------------------------------------------------------------------------
#                                             TOP-KILL ANALYSIS (top2bin, bin2bin)                                         
# ----------------------------------------------------------------------------------------------------------------------
           
          # * Top2bin algorithm: assesses damage from the top of the tree (Z_max) to incremental fixed height bins to the ground. 
          # * Algorithm uses a slightly lenient approach with percent damage threshold set to 80%
          # * This algorthim does a better job assessing top-kill damage for trees with > 50% damaged points
          
          # Generate sequence of height bins to search for top-kill
          binLims <- seq.int(from = max(tree_DT_RFClass$Z), to = 0, by = -heightBin)
          
          if (treeStats[["percDamage"]] > 50) {
                      
                      # Initialize flag to mark and catch any incremental fixed height bin with 0 points
                      emptyBinFlag = 0
  
                      for (i in 2:(length(binLims) - 1)) {
       
                          # subset by height bin and calculate percent damage per height bin     
                          temp_sub      <- tree_DT_RFClass[(tree_DT_RFClass$Z <= binLims[1]) & (tree_DT_RFClass$Z > binLims[i])] 
                          temp_subStats <- c("totalPoints" = nrow(temp_sub),
                                              "percDamage" = ((mean(temp_sub$RF_Class == 2)) * 100) + ((mean(temp_sub$RF_Class == 3)) * 100))
                    
                          if (temp_subStats[["totalPoints"]] < 20) { 
                              next
                          }
  
                          # Check, catch, and mark any incremental fixed height bin with 0 points
                          emptyBin_checkDT <- tree_DT_RFClass[(tree_DT_RFClass$Z <= binLims[i]) & (tree_DT_RFClass$Z > binLims[i + 1])]
  
                          emptyBin_check <- nrow(emptyBin_checkDT)
  
                          if (emptyBin_check == 0 & i != (length(binLims) - 1)) {
                              
                              emptyBinFlag = emptyBinFlag + 1
                              
                              if (emptyBinFlag == 1) {
                                  binBeforeFirstEmpty = i - 1
                              }
                              next
                          } 
                          
                          # Non top-kill damaged 
                          if ((temp_subStats[["percDamage"]] < damagePerc_topKill_top2bin) & (i == 2)) {
                              
                              damageStats[["Topkill_status"]]  = "Non-top-kill"
                              
                              damageStats[["Topkill_perc"]] = 0
                              
                              damageStats[["Topkill_amount"]] = 0
                              
                              message("top2bin: non top-kill")
                          
                              break
                          
                          # Top-kill
                          } else if (temp_subStats[["percDamage"]] < damagePerc_topKill_top2bin) {
                              
                              damageStats[["Topkill_status"]]  = "Top-kill"
                              damageStats[["Topkill_perc"]] = ( ( (max(tree_DT_RFClass$Z) - binLims[i - 1]) / max(tree_DT_RFClass$Z) ) * 100)
                              damageStats[["Topkill_amount"]] = (max(tree_DT_RFClass$Z) - binLims[i - 1])
                              topKillPlane_Z = binLims[i - 1]
                              
                              message("top2bin: top-kill")
  
                          
                              break
                              
                          # If algorithm runs all the way to the last height bin:
  
                          # Check for empty bins and if there are, use the height bin before the empty bin: 
                          } else if ((i == (length(binLims) - 1)) & temp_subStats[["percDamage"]] > damagePerc_topKill_top2bin) {
                              if (emptyBinFlag > 1) {
                              
                              damageStats[["Topkill_status"]]  = "Top-kill"
                              
                              damageStats[["Topkill_perc"]] = ( ( (max(tree_DT_RFClass$Z) - binLims[binBeforeFirstEmpty]) / max(tree_DT_RFClass$Z) ) * 100)
                              
                              damageStats[["Topkill_amount"]] = (max(tree_DT_RFClass$Z) - binLims[binBeforeFirstEmpty])
                              
                              topKillPlane_Z = binLims[binBeforeFirstEmpty]
                              
                              message("top2bin: top-kill with missing points in middle")
  
                              # If there are no empty bins, top-kill all the way to the bottom: most likely a snag.
                              } else {
                              
                              damageStats[["Topkill_status"]]  = "Top-kill"
                              
                              damageStats[["Topkill_perc"]] = ( ( (max(tree_DT_RFClass$Z) - binLims[i]) / max(tree_DT_RFClass$Z) ) * 100)
                              
                              damageStats[["Topkill_amount"]] = (max(tree_DT_RFClass$Z) - binLims[i])
                              
                              topKillPlane_Z = binLims[i]
                              
                              message("top2bin: top-kill")
  
                              break
                              }
                          }
                      }
          } else {
              
              # BIN2BIN: fixed height bin (bin-to-bin) damage analysis
              # * Algorithm is stricter and uses a 90% damage threshold
              # * Algorithm does a better job estimating top-kill for trees with <50% damaged points
              
              for (i in 1:(length(binLims) - 1)) {
                              
                      # subset by height bin and calculate percent damage per height bin     
                      temp_sub      <- tree_DT_RFClass[(tree_DT_RFClass$Z <= binLims[i]) & (tree_DT_RFClass$Z > binLims[i + 1])] 
                      temp_subStats <- c("totalPoints" = nrow(temp_sub),
                                          "percDamage" = ((mean(temp_sub$RF_Class == 2)) * 100) + ((mean(temp_sub$RF_Class == 3)) * 100))
                      
                      if (temp_subStats[["totalPoints"]] < 15) { #20 points = 5 points ~ class (???)
                          next
                      }
                      
                      # Non top-kill but damaged 
                      if ((temp_subStats[["percDamage"]] < damagePerc_topKill_bin2bin) & (binLims[i] == max(tree_DT_RFClass$Z))) {
                          
                          damageStats[["Topkill_status"]]  = "Non-top-kill"
                          
                          damageStats[["Topkill_perc"]] = 0
                          
                          damageStats[["Topkill_amount"]] = 0
                          
                          message("bin2bin: non-top-kill damaged")

                          break
                      
                      # Top-kill
                      } else if (temp_subStats[["percDamage"]] < damagePerc_topKill_bin2bin) {
                          
                          
                          damageStats[["Topkill_status"]]  = "Top-kill"
                          
                          damageStats[["Topkill_perc"]] = ( ( (max(tree_DT_RFClass$Z) - binLims[i]) / max(tree_DT_RFClass$Z) ) * 100)
                          
                          damageStats[["Topkill_amount"]] = (max(tree_DT_RFClass$Z) - binLims[i])
  
                          topKillPlane_Z = binLims[i]
                          
                          message("bin2bin: top-kill damaged")
                      
                          break
                      }
                  }
          }
      }
      
    }
          
# Add damage analysis data to data table
damageAssess_DT <- rbindlist(list(damageAssess_DT, data.table(treeID = treeNum,
                                                            perc_Green = treeStats[["percGreen"]],
                                                            perc_Gray = treeStats[["percGray"]],
                                                            perc_Red = treeStats[["percRed"]],
                                                            perc_Damage = treeStats[["percDamage"]],
                                                            totalPoints = treeStats[["totalPoints"]],
                                                            damageType = damageStats[["Damage_type"]],
                                                            topKill_status = damageStats[["Topkill_status"]],
                                                            topKill_perc = damageStats[["Topkill_perc"]],
                                                            topKill_amount = damageStats[["Topkill_amount"]])))

}

message("Damage assessment loop complete.")


(Sys.time() - ptm2)
message("------------------------------------------------------------")

message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
message("                  Garbage Collection                        ")
gc()
message("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

# ------------------------ LOOP BY EACH TREE (END) ------------------------






# =========================================================================

#  COMBINE TREE SEGEMENTATION POLYGONS AND TREE DAMAGE ALGORITHM RESULTS

# =========================================================================




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
treeSeg_shpObj_merged <- merge(treeSeg_shpObj, damageAssess_DT, by = "treeID")
crs(treeSeg_shpObj_merged) <- "epsg:26912"


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


# =========================================================================

#                       TREE DAMAGE ALGORITHM (END)

# =========================================================================