
### Contact information: shre9292@vandals.uidaho.edu

### Purpose(s): 
# * filter reference trees and export as point cloud
# * reference trees are snags and top-kill

### Last update: 07/13/2023

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
require(plotly)
#source("errorAccuracyOutputs_function.R") # make sure this file is in the working directory


# =========================================================================

# -------------------------------------------------------------------------

# Initialize PATH, DIR, FILE

# -------------------------------------------------------------------------
setwd("C:\\Users\\abhin\\Documents\\GitHub\\MS_Project_Scripts\\MS_Project_Workspace\\R_Workspace\\R_Scripts\\04_assessDamage_RFClassifPC")

dir  = getwd()

dataPATH = file.path(dir, "DATA")

exportPATH = file.path(dir, "EXPORTS")

# -------------------------------------------------------------------------

# Extract RF classification and apply damage assessment algorithm to reference trees
# - Unlike `assessDamage_RFClassif_PC` script, this scripts iterates through different damage percentage thresholds

 
ref_snagPC_fileName <- "M2_refSnag_RFClassif.las"
ref_topkillPC_fileName <- "M2_refTopkill_RFClassif.las"

ref_snagPC_obj <- lidR::readUAVLAS(file.path(exportPATH, ref_snagPC_fileName))
ref_topkillPC_obj <- lidR::readUAVLAS(file.path(exportPATH, ref_topkillPC_fileName))

setnames(ref_snagPC_obj@data, "RF_RBI_NDVI_REDEDGE", "RF_Class") # change RF classification column if not named as "RF_Class"
setnames(ref_topkillPC_obj@data, "RF_RBI_NDVI_REDEDGE", "RF_Class") # change RF classification column if not named as "RF_Class"

# remove ground classified points
ref_snagPC_obj_noGroundShadows <- lidR::filter_poi(ref_snagPC_obj, Classification == 0)
ref_topkillPC_obj_noGroundShadows <- lidR::filter_poi(ref_topkillPC_obj, Classification == 0)

# remove shadow classified points
ref_snagPC_obj_noGroundShadows <- lidR::filter_poi(ref_snagPC_obj_noGroundShadows, RF_Class != 4)
ref_topkillPC_obj_noGroundShadows <- lidR::filter_poi(ref_topkillPC_obj_noGroundShadows, RF_Class != 4)


# create data table of point cloud file attributes 
refSnag_DT <- data.table(ref_snagPC_obj_noGroundShadows@data)
refTopkill_DT <- data.table(ref_topkillPC_obj_noGroundShadows@data)


# label ref damage type 
refSnag_DT$ref_damageType <- "Snag"
refTopkill_DT$ref_damageType <- "Top-kill"


# Create combined DT
combined_ref_DT <- rbind(refSnag_DT, refTopkill_DT)

rm(ref_snagPC_obj_noGroundShadows)
rm(ref_topkillPC_obj_noGroundShadows)
rm(ref_snagPC_obj)
rm(ref_topkillPC_obj)
gc()


ZcolNum <- as.numeric(which(colnames(combined_ref_DT) == "Z"))
RFClassColNum <- as.numeric(which(colnames(combined_ref_DT) == "RF_Class"))
ref_damageTypeColNum <- as.numeric((which(colnames(combined_ref_DT) == "ref_damageType")))


damagePerc_list <- list(60, 70, 75, 80, 85, 90, 95, 100)

treeID_list <- unique(combined_ref_DT$treeID[!is.na(combined_ref_DT$treeID)])

heightBin = 0.25

topkillAsses_damagePercTH_DT <- data.table(treeID = numeric(), 
                                           percGreen = numeric(), 
                                           percGray = numeric(), 
                                           percRed = numeric(), 
                                           percDamage = numeric(),
                                           damagePercTH = numeric(), 
                                           damageType = character(),
                                           topKillHeight = numeric(), 
                                           topKillPerc = numeric(), 
                                           topKillAmount = numeric(), 
                                           ref_damageType = character())

# BEGIN LOOP:
for (damagePerc_topKill in damagePerc_list) {
  
  for (treeNum in treeID_list) {
    
    # Sieve to catch any NAs 
    if (is.na(treeNum)) {
      next
    }
    
    tree_DT_RFClass <- combined_ref_DT[treeID == treeNum, c(..ZcolNum, ..RFClassColNum, ..ref_damageTypeColNum )] # DOUBLE CHECK THE COLUMN NUMBERS, data.table reads variables with preceding `..` as expressions
    
    damageStats <- c("Damage_type"  = "NA",
                     "Topkill_perc" = NA, 
                     "Topkill_amount" = NA)
    
    
    # Calculate classification statistics for individual tree
    treeStats <- c("percGreen" = ((mean(tree_DT_RFClass$RF_Class == 1)) * 100),
                   "percGray" = ((mean(tree_DT_RFClass$RF_Class == 2)) * 100),
                   "percRed" = ((mean(tree_DT_RFClass$RF_Class == 3)) * 100), 
                   "percDamage" = ((mean(tree_DT_RFClass$RF_Class == 2)) * 100) + ((mean(tree_DT_RFClass$RF_Class == 3)) * 100))
    
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
      
      
      # top2bin algorithm (snag sieve)    
      
      if (treeStats[["percDamage"]] > 50) {
  
              emptyBinFlag = 0

              for (i in 2:(length(binLims) - 1)) {
                  
                  emptyBin_checkDT <- tree_DT_RFClass[(tree_DT_RFClass$Z <= binLims[i]) & (tree_DT_RFClass$Z > binLims[i + 1])]

                  emptyBin_check <- nrow(emptyBin_checkDT)

                  if (emptyBin_check == 0 & i != (length(binLims) - 1)) {
                      
                      emptyBinFlag = emptyBinFlag + 1
                      
                      if (emptyBinFlag == 1) {
                          binBeforeFirstEmpty = i - 1
                      }
                      next
                  } 

                  # subset by height bin and calculate percent damage per height bin     
                  temp_sub      <- tree_DT_RFClass[(tree_DT_RFClass$Z <= binLims[1]) & (tree_DT_RFClass$Z > binLims[i])] 
                  temp_subStats <- c("totalPoints" = nrow(temp_sub),
                                      "percDamage" = ((mean(temp_sub$RF_Class == 2)) * 100) + ((mean(temp_sub$RF_Class == 3)) * 100))

                  if (temp_subStats[["totalPoints"]] < 20) { #20 points = 5 points ~ class (???)
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
                      if (emptyBinFlag > 1) {
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
      
      
      # bin2bin damage analysis
      
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
              topkillAsses_damagePercTH_DT <- rbindlist(list(topkillAsses_damagePercTH_DT, data.table(treeID = treeNum,  
                                                                                                      percGreen = treeStats[["percGreen"]], 
                                                                                                      percGray = treeStats[["percGray"]],
                                                                                                      percRed = treeStats[["percRed"]], 
                                                                                                      percDamage = treeStats[["percDamage"]],
                                                                                                      damagePercTH = damagePerc_topKill,
                                                                                                      damageType = damageStats[["Damage_type"]],
                                                                                                      topKillHeight = binLims[i - 1], 
                                                                                                      topKillPerc = as.numeric(damageStats[["Topkill_perc"]]), 
                                                                                                      topKillAmount = as.numeric(damageStats[["Topkill_amount"]]), 
                                                                                                      ref_damageType = unique(tree_DT_RFClass[[3]]))))
                                                                        
      
 }
    
}



View(topkillAsses_damagePercTH_DT)

topkillAsses_damagePercTH_DT$ref_damageType <- factor(topkillAsses_damagePercTH_DT$ref_damageType)


# Figure for allowing top2bin algorithm for >50 percDamage trees:
# ggplot(data = topkillAsses_damagePercTH_DT, aes(y = percDamage, fill = ref_damageType)) +
#     geom_boxplot() + 
#     labs(x = "Reference damage type", 
#         y = "Percent of damage classified points (%)") +
#     theme_classic()


# subDT <- topkillAsses_damagePercTH_DT[which(topkillAsses_damagePercTH_DT$damagePercTH == 60), ]
# View(subDT)


# ggplot(data = subDT, aes(y = topKillPerc, fill = ref_damageType)) + 
# geom_boxplot()

# Label snags
topkillAsses_damagePercTH_DT$topKillPerc <- as.numeric(topkillAsses_damagePercTH_DT$topKillPerc)
topkillAsses_damagePercTH_DT[round(topKillPerc) >= 75, damageType := "Snag"]

# unique(topkillAsses_damagePercTH_DT$damageType)

write.csv(topkillAsses_damagePercTH_DT, file.path(exportPATH, "damageAssessTH_refSnags_Topkill_Trees.csv"), row.names = FALSE)

message("Script run successfully!")
Sys.time() - start_ptm

#----------------------------------------------------------------------