###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s): 
# * Analyze results of the top-kill estimated with different damage % thresholds

### Last update: 12/05/23

###########################################################################

# =========================================================================

# IMPORT LIBRARIES

# =========================================================================


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

inputCSVFileName = "topkillAsses_damagePercTH_fullSiteM2_li2012.csv" # from "01_topKillEstimated_withDifferentDamagePercTH.R" script

# -------------------------------------------------------------------------

# Import and prep data

damagePercTH_analysisDT <- data.table(read.csv(file.path(exportPATH, )))
damagePercTH_analysisDT$treeID <- factor(damagePercTH_analysisDT$treeID)
damagePercTH_analysisDT$damagePercTH <- factor(damagePercTH_analysisDT$damagePercTH)
damagePercTH_analysisDT$damageType <- factor(damagePercTH_analysisDT$damageType)

damagePercTH_analysisDT_wide <- data.table(pivot_wider(damagePercTH_analysisDT,
                                                       id_cols = treeID,
                                                       names_from = damagePercTH,
                                                       values_from = topKill_perc))

colnames(damagePercTH_analysisDT_wide) <- c("treeID", "topKill_perc_60percDamageTH","topKill_perc_70percDamageTH", "topKill_perc_75percDamageTH", "topKill_perc_80percDamageTH", "topKill_perc_85percDamageTH", "topKill_perc_90percDamageTH", "topKill_perc_95percDamageTH", "topKill_perc_100percDamageTH")

# Columns to compare 
# Options: c("topKill_perc_60percDamageTH","topKill_perc_70percDamageTH", "topKill_perc_75percDamageTH", "topKill_perc_80percDamageTH", "topKill_perc_85percDamageTH", "topKill_perc_90percDamageTH", "topKill_perc_95percDamageTH", "topKill_perc_100percDamageTH")

col1 = "topKill_perc_90percDamageTH"

col2 = "topKill_perc_80percDamageTH" 

ncol1 = as.numeric(which(colnames(damagePercTH_analysisDT_wide) == col1))

ncol2 = as.numeric(which(colnames(damagePercTH_analysisDT_wide) == col2))

subsetDT <- damagePercTH_analysisDT_wide[, c(1, ..ncol1, ..ncol2)]

# --- 

# subset DT without NAs and all zeros for density 2D plot

# Identify rows with complete cases (no NAs)
complete_rows <- complete.cases(subsetDT)

# Identify rows with complete cases (no NAs)
complete_rows <- complete.cases(subsetDT)

# Sub-setting data table with rows without NAs
subsetDT_noNA <- subsetDT[complete_rows]

# Sub-setting data table to remove rows with all zeros
subsetDT_noNA_noZero <- subsetDT_noNA[rowSums(subsetDT_noNA[, -1] == 0) < ncol(subsetDT_noNA) - 1] # if total # of zeros in a row is less than 8 (total # cols with values)

# # -------------------------------------------------------------------------

# create key-value pair-like R vector object to automatically label the plots

plotLabels <- c("topKill_perc_60percDamageTH" = "60%",
                "topKill_perc_70percDamageTH" = "70%", 
                "topKill_perc_75percDamageTH" = "75%", 
                "topKill_perc_80percDamageTH" = "80%", 
                "topKill_perc_85percDamageTH" = "85%", 
                "topKill_perc_90percDamageTH" = "90%", 
                "topKill_perc_95percDamageTH" = "95%", 
                "topKill_perc_100percDamageTH" = "100%")

# -------------------------------------------------------------------------

#                               PLOT 1
#                              --------

# Scatter plot with %top-kill by % damage threshold on either axes
# --> Shows that there is not much difference in using 90% vs 80% damage threshold
# --> Gives reason for top2bin (more lenient algorithm) to use damage TH of 80% to test for snags

# Combined scatter plot and density plot with %top-kill by % damage threshold on either axes
library(RColorBrewer)
mycolors <- colorRampPalette(RColorBrewer::brewer.pal(9, "OrRd"))(14)
ggplot(subsetDT_noNA_noZero, aes(x = .data[[col1]], y = .data[[col2]])) +
    geom_density_2d_filled(contour_var = "density") +
    geom_density_2d(color = "black", linetype = 2) +
    geom_point(alpha = 0.25) +
    geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1, linetype = 2, alpha = 0.5) +
    scale_fill_manual(values = mycolors)+
    labs(x = paste0("Percentage of top-kill with a ",  plotLabels[[col1]], " damage threshold"), 
         y = paste0("Percentage of top-kill with a ",  plotLabels[[col2]], " damage threshold")) +
    theme_classic() +
    theme(legend.position = "none", 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14))+
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))


# -------------------------------------------------------------------------

#                               PLOT 2
#                              --------

# Histogram plot of differences in % top-kill estimated with different % damage thresholds
# --> Shows that there is not much difference in using 90% vs 80% damage threshold
# --> Gives reason for top2bin (more lenient algorithm) to use damage TH of 80% to test for snags


subsetDT_noNA[,topKill_perc_diff := abs(.SD[[1]] - .SD[[2]]), .SDcols = c(col1, col2)]
# # NOTES:
# # - `.SD` is a form of "special symbol" in the data.table package.
# # - ".SD is a data.table containing the Subset of x's Data for each group, excluding any columns used in by (or keyby)"
# # --> Useful in dynamically compare two columns by calling variables with the column names.

ggplot(data = subsetDT_noNA, aes(x = topKill_perc_diff)) +
    geom_histogram(bins = 50, fill = "midnightblue", color = "black")+
    labs(x = paste0("Difference between top-kill% estimated by ", plotLabels[[col2]], " vs ", plotLabels[[col1]], " damage thresholds"), 
         y = "Frequency") +
    theme_classic()+
    theme(legend.position = "none", 
          axis.text = element_text(size = 12), 
          axis.title = element_text(size = 14)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))