###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s): 
# * filter reference trees and export as point cloud
# * reference trees are snags and top-kill

### Last update: 12/05/23

###########################################################################

gc()

# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
# start_ptm <- Sys.time()

require(tidyr)
require(dplyr)
require(terra)
require(ggplot2)
# require(lidR)
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


# Data import and preparation

snag_topkill_ref_damageAssessTH_DT <- data.table(read.csv(file.path(exportPATH, "damageAssessTH_refSnags_Topkill_Trees.csv")))

snag_topkill_ref_damageAssessTH_DT$damageType <- factor(snag_topkill_ref_damageAssessTH_DT$damageType)
snag_topkill_ref_damageAssessTH_DT$damagePercTH <- factor(snag_topkill_ref_damageAssessTH_DT$damagePercTH)
snag_topkill_ref_damageAssessTH_DT$ref_damageType <- factor(snag_topkill_ref_damageAssessTH_DT$ref_damageType)
snag_topkill_ref_damageAssessTH_DT$treeID <- factor(snag_topkill_ref_damageAssessTH_DT$treeID)


# -------------------------------------------------------------------------

#                               PLOT 1
#                              --------


# Plot damage percentage by damage type of reference trees
# --> Reason for top2bin algorithm for %damage > 50:


ggplot(data = snag_topkill_ref_damageAssessTH_DT, aes(x = ref_damageType, 
                                                      y = percDamage, 
                                                      #fill = ref_damageType
                                                      )) +
    geom_boxplot(color = "black") +
    labs(x = "Damage type of reference data set",
         y = "Percent damaged points in each tree (%)") +
    geom_abline(intercept = 50, slope = 0, color = "black", linewidth = 1, linetype = 2, alpha = 0.25) +
    #scale_fill_manual(values = c("gray42", "midnightblue")) +
    theme_classic() +
    theme(legend.position = "none", 
          axis.text = element_text(size = 12, color = "black"), 
          axis.title.x = element_text(size = 14, margin = margin(t = 20)),
          axis.title.y = element_text(size = 14, margin = margin(r = 15)))

# -------------------------------------------------------------------------

# Subset data tables by reference damage type

# subset snags only
snagSub_DT <- snag_topkill_ref_damageAssessTH_DT[which(snag_topkill_ref_damageAssessTH_DT$ref_damageType == "Snag"),]

validSnags_subDT <- snagSub_DT[!treeID %in% c("967", "4345", "834", "1286", "645", "1187", "2796")] # point cloud of some snags are not valid (too less points, or invalid segmentation)

# topkill only
topkillSub_DT <- snag_topkill_ref_damageAssessTH_DT[ref_damageType == "Top-kill"]

# Combine the valid snags and top-kill again into a combined data table
combined_ref_validSnags_topkill_DT <- rbind(validSnags_subDT[damagePercTH == "80" & ref_damageType == "Snag"], topkillSub_DT[damagePercTH == "90" & ref_damageType == "Top-kill"])

# -------------------------------------------------------------------------

#                               PLOT 2
#                              --------

# Plot boxplot comparing percent top-kill seen in reference snags vs reference top-killed trees
# --> Reason for labeling snags as > 75% top-kill:

ggplot(data =combined_ref_validSnags_topkill_DT, aes(x = ref_damageType, 
                                                     y = topKillPerc, 
                                                     # fill = ref_damageType
                                                     )) +
    geom_boxplot(color = "black") +
    labs(x = "Damage type of reference data set",
         y = "Percent top-kill (%)") +
    geom_abline(intercept = 75, slope = 0, color = "black", linewidth = 1, linetype = 2, alpha = 0.25) +
    # scale_fill_manual(values = c("gray42", "midnightblue")) +
    theme_classic() +
    theme(legend.position = "none", 
          axis.text = element_text(size = 12, color = "black"), 
          axis.title.x = element_text(size = 14, margin = margin(t = 20)),
          axis.title.y = element_text(size = 14, margin = margin(r = 15)))










