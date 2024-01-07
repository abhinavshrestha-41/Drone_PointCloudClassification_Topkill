###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: shre9292@vandals.uidaho.edu

### Purpose(s): 
# * Damage assessment of RF classified point cloud
# * Integration of damage metrics into data products 
# * Export data products

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

# -------------------------------------------------------------------------

# Initialize PATH, DIR, FILE

# -------------------------------------------------------------------------
message("Initializing workspace and paths...")
setwd("C:/Users/shre9292/OneDrive - University of Idaho/Documents/GitHub/MS_Project_Scripts/MS_Project_Workspace/R_Workspace/R_Scripts/04_assessDamage_RFClassifPC")

dir  = getwd()

dataPATH = file.path(dir, "DATA")

exportPATH = file.path(dir, "EXPORTS")

# ---

pointCloud_fileName <- "M2_li2012_RFClassif_maxProb.las"

treeSeg_shpName <- "M2_treeSegPolygons_li2012.shp"

exportShp_name <- "TreeSeg_polygons_damageAssessment_M2_li2012_60t2b_80b2b_60snagTH.shp"

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
damageAssess_DT <- data.table(treeID = numeric(), 
                              perc_Green = numeric(), 
                              perc_Gray = numeric(), 
                              perc_Red = numeric(), 
                              perc_Damage = numeric(),
                              totalPoints = numeric(), 
                              damageType = character(), 
                              topKill_perc = numeric(), 
                              topKill_amount = numeric())

message("--> Creating a list of unique treeIDs")
# Create a list of treeIDs to loop through
treeID_list <- unique(pointCloudObj_DT$treeID[!is.na(pointCloudObj_DT$treeID)])

# set fixed bin height
heightBin = 0.25
message("--> fixed height bin for damage assessment set as: ", heightBin)


# testTreeID <- 7 # For troubleshooting

# Set percent damage threshold for damage analysis
damagePerc_topKill_bin2bin = 80
damagePerc_topKill_top2bin = 60
message("--> damage percent threshold for 'bin2bin' algorithm set as: ", damagePerc_topKill_bin2bin)
message("--> damage percent threshold for 'top2bin' algorithm set as: ", damagePerc_topKill_top2bin)

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
      
      # Minimal damaged (Healthy tree) sieve
      if (treeStats[["percDamage"]] < 5) {                        
          damageStats <- c("Damage_type" = "Minimal damage",
                          "Topkill_perc" = 0, 
                          "Topkill_amount" = 0)
          message("Minimal damaged tree, moving to next tree.")     
      } else {

      # If not healthy, it could be a type of damaged tree: snag, red (top-kill/non top-kill), damaged (top-kill/non top-kill).
  
          # Generate sequence of height bins to search for top-kill
          binLims <- seq.int(from = max(tree_DT_RFClass$Z), to = 0, by = -heightBin)
  
          # Red tree seive: if cond is true, adds a tag "Red tree" to "Damage_type" property of "damageStats" R-vector-object   
          if (treeStats[["percRed"]] >= 60) {                  
                          damageStats[["Damage_type"]] = "Red tree" 
                      } else {
                          damageStats[["Damage_type"]] = "Damaged tree"
                      }
          
  
          # Snag sieve: uses top2bin damage analysis algorithm. 
          # * Top2bin algorithm: assesses damage from the top of the tree (Z_max) to incremental fixed height bins to the ground. 
          # * Algorithm uses a slightly lenient approach with percent damage threshold set to 80%
          
          if (treeStats[["percDamage"]] > 50) {
                      
                      # Initialize flag to mark and catch any incremental fixed height bin with 0 points
                      emptyBinFlag = 0
  
                      for (i in 2:(length(binLims) - 1)) {
       
                          # subset by height bin and calculate percent damage per height bin     
                          temp_sub      <- tree_DT_RFClass[(tree_DT_RFClass$Z <= binLims[1]) & (tree_DT_RFClass$Z > binLims[i])] 
                          temp_subStats <- c("totalPoints" = nrow(temp_sub),
                                              "percDamage" = ((mean(temp_sub$RF_Class == 2)) * 100) + ((mean(temp_sub$RF_Class == 3)) * 100))
                    
                          if (temp_subStats[["totalPoints"]] < 20) { #20 points = 5 points ~ class (???)
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
                          
                          # Non top-kill but damaged 
                          if ((temp_subStats[["percDamage"]] < damagePerc_topKill_top2bin) & (i == 2)) {
                              
                              damageStats[["Damage_type"]]  = paste0(damageStats[["Damage_type"]], ", non top-kill")
                              
                              damageStats[["Topkill_perc"]] = 0
                              
                              damageStats[["Topkill_amount"]] = 0
                              
                              message("top2bin: non top-kill damaged")
                          
                              break
                          
                          # Top-kill
                          } else if (temp_subStats[["percDamage"]] < damagePerc_topKill_top2bin) {
                              
                              damageStats[["Damage_type"]]  = paste0(damageStats[["Damage_type"]], ", top-kill")
                              damageStats[["Topkill_perc"]] = ( ( (max(tree_DT_RFClass$Z) - binLims[i - 1]) / max(tree_DT_RFClass$Z) ) * 100)
                              damageStats[["Topkill_amount"]] = (max(tree_DT_RFClass$Z) - binLims[i - 1])
                              topKillPlane_Z = binLims[i - 1]
                              message("top2bin: top-kill")
  
                          
                              break
                              
                          # If algorithm runs all the way to the last height bin:
  
                          # Check for empty bins and if there are, use the height bin before the empty bin: 
                          } else if ((i == (length(binLims) - 1)) & temp_subStats[["percDamage"]] > damagePerc_topKill_top2bin) {
                              if (emptyBinFlag > 1) {
                              
                              damageStats[["Damage_type"]]  = paste0(damageStats[["Damage_type"]], ", top-kill")
                              
                              damageStats[["Topkill_perc"]] = ( ( (max(tree_DT_RFClass$Z) - binLims[binBeforeFirstEmpty]) / max(tree_DT_RFClass$Z) ) * 100)
                              
                              damageStats[["Topkill_amount"]] = (max(tree_DT_RFClass$Z) - binLims[binBeforeFirstEmpty])
                              
                              topKillPlane_Z = binLims[binBeforeFirstEmpty]
                              
                              message("top2bin: top-kill with missing points in middle")
  
                              # If there are no empty bins, top-kill all the way to the bottom: most likely a snag.
                              } else {
                              
                              damageStats[["Damage_type"]]  = paste0(damageStats[["Damage_type"]], ", top-kill")
                              
                              damageStats[["Topkill_perc"]] = ( ( (max(tree_DT_RFClass$Z) - binLims[i]) / max(tree_DT_RFClass$Z) ) * 100)
                              
                              damageStats[["Topkill_amount"]] = (max(tree_DT_RFClass$Z) - binLims[i])
                              
                              topKillPlane_Z = binLims[i]
                              
                              message("top2bin: potential snag")
  
                              break
                              }
                          }
                      }
          } else {
              
              # BIN2BIN: fixed height bin (bin-to-bin) damage analysis
              # * Algorithm is stricter and uses a 90% damage threshold
              
              for (i in 1:(length(binLims) - 1)) {
                              
                      # subset by height bin and calculate percent damage per height bin     
                      temp_sub      <- tree_DT_RFClass[(tree_DT_RFClass$Z <= binLims[i]) & (tree_DT_RFClass$Z > binLims[i + 1])] 
                      temp_subStats <- c("totalPoints" = nrow(temp_sub),
                                          "percDamage" = ((mean(temp_sub$RF_Class == 2)) * 100) + ((mean(temp_sub$RF_Class == 3)) * 100))
                      
                      if (temp_subStats[["totalPoints"]] < 20) { #20 points = 5 points ~ class (???)
                          next
                      }
                      
                      # Non top-kill but damaged 
                      if ((temp_subStats[["percDamage"]] < damagePerc_topKill_bin2bin) & (binLims[i] == max(tree_DT_RFClass$Z))) {
                          
                          damageStats[["Damage_type"]]  = paste0(damageStats[["Damage_type"]], ", non top-kill")
                          
                          damageStats[["Topkill_perc"]] = 0
                          
                          damageStats[["Topkill_amount"]] = 0
                          
                          message("bin2bin: non-top-kill damaged")

                          break
                      
                      # Top-kill
                      } else if (temp_subStats[["percDamage"]] < damagePerc_topKill_bin2bin) {
                          
                          
                          damageStats[["Damage_type"]]  = paste0(damageStats[["Damage_type"]], ", top-kill")
                          
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
                                                            topKill_perc = damageStats[["Topkill_perc"]],
                                                            topKill_amount = damageStats[["Topkill_amount"]])))

}

message("Damage assessment loop complete.")

message("Labeling snags as trees with > 60% top-kill.")
# Label snags 
damageAssess_DT$topKill_perc <- as.numeric(damageAssess_DT$topKill_perc)
damageAssess_DT[(round(topKill_perc) >= 60 & perc_Red >= 60), damageType := "Red tree, snag"]
damageAssess_DT[(round(topKill_perc) >= 60 & perc_Red < 60), damageType := "Snag"]

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
treeSeg_shpObj_merged <- merge(treeSeg_shpObj, damageAssess_DT, by = "treeID")

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