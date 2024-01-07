###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s): 
# * Damage assessment by damage % threshold script

### Last update: 12/05/23

###########################################################################


# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
start_ptm <- Sys.time()

require(tidyr)
require(dplyr)
require(terra)
require(ggplot2)
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

pointCloud_fileName <- "M1_li2012_RFClassif_maxProb.las"

pointCloud_fullPath <- file.path(dataPATH, pointCloud_fileName)

pointCloudObj <- lidR::readUAVLAS(pointCloud_fullPath)
setnames(pointCloudObj@data, "RF_RBI_NDVI_REDEDGE", "RF_Class") # change RF classification column if not named as "RF_Class"

# remove ground classified points
pointCloudObj_noGround_noShadows <- lidR::filter_poi(pointCloudObj, Classification == 0)

# remove shadow classified points
pointCloudObj_noGround_noShadows <- lidR::filter_poi(pointCloudObj_noGround_noShadows, RF_Class != 4)


# create data table of point cloud file attributes 
pointCloudObj_DT <- data.table(pointCloudObj_noGround_noShadows@data)
rm(pointCloudObj_noGround_noShadows)
rm(pointCloudObj)
gc()


ZcolNum <- as.numeric(which(colnames(pointCloudObj_DT) == "Z"))
RFClassColNum <- as.numeric(which(colnames(pointCloudObj_DT) == "RF_Class"))


damagePerc_list <- list(60, 70, 75, 80, 85, 90, 95, 100)

treeID_list <- unique(pointCloudObj_DT$treeID[!is.na(pointCloudObj_DT$treeID)])

heightBin <- 0.25

topkillAsses_damagePercTH_DT <- data.table(treeID = numeric(), 
                                           perc_Green = numeric(), 
                                           perc_Gray = numeric(), 
                                           perc_Red = numeric(), 
                                           perc_Damage = numeric(),
                                           totalPoints = numeric(),
                                           damagePercTH = numeric(),
                                           damageType = character(), 
                                           topKill_perc = numeric(), 
                                           topKill_amount = numeric())

for (damagePerc_topKill in damagePerc_list) {
  
  for (treeNum in treeID_list) {
    
    # Sieve to catch any NAs 
    if (is.na(treeNum)) {
      next
    }
    
    tree_DT_RFClass <- pointCloudObj_DT[treeID == treeNum, c(..ZcolNum, ..RFClassColNum )] # DOUBLE CHECK THE COLUMN NUMBERS, data.table reads variables with preceding `..` as expressions
    
    damageStats <- c("Damage_type"  = NA,
                     "Topkill_perc" = NA, 
                     "Topkill_amount" = NA)
    binLims = NA # if tree has minimal damage, algorithm does not calculate binLims, hence, init binLims as NA since height of topkill for healthy tree = NA
    i = 1 # such that binLims[i] = NA for healthy trees
    
    # Calculate classification statistics for individual tree
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
                  
    
      # HEALTHY TREE (MINIMAL DAMAGE)
      if (treeStats[["percDamage"]] < 5) {                        
        damageStats <- c("Damage_type" = "Minimal damage",
                        "Topkill_perc" = 0, 
                        "Topkill_amount" = 0)
        
        # DAMAGED TREE (Snag, Red (top-kill/non top-kill), damaged (top-kill/non top-kill))
      } else {
        
        # Generate sequence of height bins to search for top-kill
        binLims <- seq.int(from = max(tree_DT_RFClass$Z), to = 0, by = -heightBin)
        
        # Red tree test (adds tag)  
        if (treeStats[["percRed"]] >= 60) {                  
          damageStats[["Damage_type"]] = "Red tree" 
        } else {
          damageStats[["Damage_type"]] = "Damaged tree"
        }
        
        
        # SNAG SIEVE (top-to-height bin damage analysis)    
        
        if (treeStats[["percDamage"]] > 50) {
    
                emptyBinFlag = 0

                for (i in 2:(length(binLims) - 1)) {

                    # subset by height bin and calculate percent damage per height bin     
                    temp_sub      <- tree_DT_RFClass[(tree_DT_RFClass$Z <= binLims[1]) & (tree_DT_RFClass$Z > binLims[i])] 
                    temp_subStats <- c("totalPoints" = nrow(temp_sub),
                                        "percDamage" = ((mean(temp_sub$RF_Class == 2)) * 100) + ((mean(temp_sub$RF_Class == 3)) * 100))
                                        
                    if (temp_subStats[["totalPoints"]] < 20) { #20 points = 5 points ~ class (???)
                        next
                    }

                    
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
                    if ((temp_subStats[["percDamage"]] < damagePerc_topKill) & (i == 2)) {
                        
                        damageStats[["Damage_type"]]  = paste0(damageStats[["Damage_type"]], ", non top-kill")
                        damageStats[["Topkill_perc"]] = 0
                        damageStats[["Topkill_amount"]] = 0
                        message("top2bin: Non top-kill damaged")

                        break
                    
                    # Top-kill
                    } else if (temp_subStats[["percDamage"]] < damagePerc_topKill) {
                        
                        damageStats[["Damage_type"]]  = paste0(damageStats[["Damage_type"]], ", top-kill")
                        damageStats[["Topkill_perc"]] = ( ( (max(tree_DT_RFClass$Z) - binLims[i - 1]) / max(tree_DT_RFClass$Z) ) * 100)
                        damageStats[["Topkill_amount"]] = (max(tree_DT_RFClass$Z) - binLims[i - 1])
                        topKillPlane_Z = binLims[i - 1]
                        
                        message("top2bin: Top-Kill")

                    
                        break
                        
                    # If algorithm runs all the way to the last height bin:

                    # Check for empty bins and if there are, use the height bin before the empty bin: 
                    } else if ((i == (length(binLims) - 1)) & temp_subStats[["percDamage"]] > damagePerc_topKill) {
                        if (emptyBinFlag != 0) {
                        damageStats[["Damage_type"]]  = paste0(damageStats[["Damage_type"]], ", top-kill")
                        damageStats[["Topkill_perc"]] = ( ( (max(tree_DT_RFClass$Z) - binLims[binBeforeFirstEmpty]) / max(tree_DT_RFClass$Z) ) * 100)
                        damageStats[["Topkill_amount"]] = (max(tree_DT_RFClass$Z) - binLims[binBeforeFirstEmpty])
                        topKillPlane_Z = binLims[binBeforeFirstEmpty]
                        message("top2bin: Top-kill with missing points in middle")

                        # If there are no empty bins, top-kill all the way to the bottom (snag):
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
        
        
        # Fixed height bin (bin-by-bin) damage analysis
        
        for (i in 1:(length(binLims) - 1)) {
                        
                # subset by height bin and calculate percent damage per height bin     
                temp_sub      <- tree_DT_RFClass[(tree_DT_RFClass$Z <= binLims[i]) & (tree_DT_RFClass$Z > binLims[i + 1])] 
                temp_subStats <- c("totalPoints" = nrow(temp_sub),
                                    "percDamage" = ((mean(temp_sub$RF_Class == 2)) * 100) + ((mean(temp_sub$RF_Class == 3)) * 100))
                
                if (temp_subStats[["totalPoints"]] < 20) { #20 points = 5 points ~ class (???)
                    next
                }
                
                # Non top-kill but damaged 
                if ((temp_subStats[["percDamage"]] < damagePerc_topKill) & (binLims[i] == max(tree_DT_RFClass$Z))) {
                    
                    damageStats[["Damage_type"]]  = paste0(damageStats[["Damage_type"]], ", non top-kill")
                    damageStats[["Topkill_perc"]] = 0
                    damageStats[["Topkill_amount"]] = 0
                    message("bin2bin: non top-kill damaged")

                    break
                
                # Top-kill
                } else if (temp_subStats[["percDamage"]] < damagePerc_topKill) {
                    
                    damageStats[["Damage_type"]]  = paste0(damageStats[["Damage_type"]], ", top-kill")
                    damageStats[["Topkill_perc"]] = ( ( (max(tree_DT_RFClass$Z) - binLims[i]) / max(tree_DT_RFClass$Z) ) * 100)
                    damageStats[["Topkill_amount"]] = (max(tree_DT_RFClass$Z) - binLims[i])

                    message("bin2bin: top-kill")

                    topKillPlane_Z = binLims[i]

                
                    break
                }
            }
          }
      }
    }
              topkillAsses_damagePercTH_DT <- rbindlist(list(topkillAsses_damagePercTH_DT, data.table(treeID = treeNum,
                                                                                                      perc_Green = treeStats[["percGreen"]],
                                                                                                      perc_Gray = treeStats[["percGray"]],
                                                                                                      perc_Red = treeStats[["percRed"]],
                                                                                                      perc_Damage = treeStats[["percDamage"]],
                                                                                                      totalPoints = treeStats[["totalPoints"]],
                                                                                                      damagePercTH = damagePerc_topKill,
                                                                                                      damageType = damageStats[["Damage_type"]],
                                                                                                      topKill_perc = damageStats[["Topkill_perc"]],
                                                                                                      topKill_amount = damageStats[["Topkill_amount"]])))
              
                                                                        
      
 }
    
}


# View(topkillAsses_damagePercTH_DT)

# Label snags
topkillAsses_damagePercTH_DT$topKill_perc <- as.numeric(topkillAsses_damagePercTH_DT$topKill_perc)
topkillAsses_damagePercTH_DT[round(topKill_perc) >= 75, damageType := "Snag"]

unique(topkillAsses_damagePercTH_DT$damageType)

# EXPORT
write.csv(topkillAsses_damagePercTH_DT, file.path(exportPATH, "topkillAsses_damagePercTH_fullSiteM2_li2012.csv"), row.names = FALSE)

message("Script run successfully!")
Sys.time() - start_ptm








