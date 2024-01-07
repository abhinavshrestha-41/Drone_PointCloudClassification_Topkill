#-----------------------------------------------------------------------------------------#
#- Function to summarize attributes of damage assessment shp and return formatted tables -#
#-----------------------------------------------------------------------------------------#
# A Shrestha - UIdaho - DEC 2023 - NOTE only use for 'SpatVector' objects pkg:`terra`     #
#-----------------------------------------------------------------------------------------#

#' Summarizes damage assessment shapefile created from the tree-lvel damage assessment algorithm 
#'
#' This function takes object(s) of class 'SpatVector' from the `terra` package.
#'
#' @param spatVectObj An object of class 'SpatVector' from the `terra` package. Use `terra::vect()` to import .shp file into R and pass the SpatVector object to this function. If there are more than one 'SpatVector' objects used as input, the function will merge the shapefile attributes to return a merged (total) damage assessment analysis. 
#' 
#' @return Message with labeling each summary table, print output of the formatted table and all returned tables stored as variables in the global environment in the running R session
#'
#' @export NULL


# =========================================================================

# IMPORT LIBRARIES

# =========================================================================

library(terra)
# library(ggplot2)
# library(lidR)
# library(sf)
# library(sp)
# library(ggrepel)
# library(randomForest)
# library(rfUtilities)
library(data.table)
# library(stars)
# library(caret)
# library(rgdal)
#source("errorAccuracyOutputs_function.R") # make sure this file is in the working directory


damageAssessSummary <- function(...) {
  
    inputsList <- list(...)  # Convert input variables to a list
  
    num_inputs <- length(inputsList)  # Get the number of input variables
    
    if (num_inputs == 1) {
      
      damageAssessShp_DT <- data.table::as.data.table(inputsList[[1]])
      
    } else {
      
      # Merge input data from all variables
      damageAssessShp_DT <- data.table(treeID = numeric(), 
                                       z_max = numeric(),
                                       perc_Green = numeric(), 
                                       perc_Red = numeric(), 
                                       perc_Gray = numeric(), 
                                       perc_Damag = numeric(),
                                       totalPoint = numeric(), 
                                       damageType = character(),
                                       topKill_st = character(),
                                       topKill_pe = numeric(), 
                                       topKill_am = numeric())
      
      for (i in 1:num_inputs) {
      
          damageAssessShp_DT <- rbind(damageAssessShp_DT, as.data.table(inputsList[[i]]))
      
          }
    }
    
    # removing invalid NA and Damaged tree rows of data:
    damageAssessShp_DT <- damageAssessShp_DT[damageType != "NA"] # same as `damageAssessShp_DT[-(which(damageAssessShp_DT$damageType == "NA")), ]` but a faster (slightly) data.table method. 
    damageAssessShp_DT <- damageAssessShp_DT[damageType != "Damaged tree"]
    
    damageAssessShp_DT$topKill_pe <- as.numeric(damageAssessShp_DT$topKill_pe)
    damageAssessShp_DT$topKill_am <- as.numeric(damageAssessShp_DT$topKill_am)
    
    # Set the damage classes to be analyzed 
    
    damageType_orderedList <- c("Healthy", "Minor damage", "Moderate damage", "Major damage", "Dead (red)", "Dead (gray)", "Dead (mixed)")
    
    damageAssessShp_DT$damageType <- factor(damageAssessShp_DT$damageType, 
                                            levels = damageType_orderedList)
    
    
    TreeCondition <- c("Healthy", 
                       "Damaged", 
                       "Minor damage",
                       "Moderate damage", 
                       "Major damage", 
                       "Dead (red)",
                       "Dead (gray)", 
                       "Dead (mixed)", 
                       "Total")
    
    totalTrees <- nrow(damageAssessShp_DT)

    # Damage summary HEALTHY VS DAMAGED
    
    healthyVDamage_typeList <- c("Healthy", "Damaged")
    
    # Empty R-object to hold damage summary data
    Assessment_by_damageTypeMat <- c()
    
    for (DamageType in healthyVDamage_typeList) {
        
        if (is.na(DamageType) == TRUE) {
            next
        } else {
            
            if (DamageType == "Healthy") {
             
             subsetDT <- damageAssessShp_DT[which(damageAssessShp_DT$damageType == DamageType), ]
             
             
             Assessment_by_damageType <- c(paste0(nrow(subsetDT), " (", round(((nrow(subsetDT) / totalTrees) * 100), 2), "%)"),
                                           as.numeric(round(mean(subsetDT$perc_Green), 2)),
                                           as.numeric(round(mean(subsetDT$perc_Red), 2)),
                                           as.numeric(round(mean(subsetDT$perc_Gray), 2)),
                                           as.numeric(round(mean(subsetDT$perc_Damag), 2)), 
                                           "_", 
                                           "-", 
                                           "-", 
                                           "-")
             
             Assessment_by_damageTypeMat <- rbind(Assessment_by_damageTypeMat, Assessment_by_damageType)
             
            } else {
             
             subsetDT <- damageAssessShp_DT[-which(damageAssessShp_DT$damageType == "Healthy"), ]
             
             damagedTreeTotal <- nrow(subsetDT)
             
             nonTopKill_total <- length(which(subsetDT$topKill_st == "Non-top-kill"))
             
             topkill_subsetDT <- subsetDT[which(subsetDT$topKill_st == "Top-kill")]
             
             Assessment_by_damageType <- c(paste0(damagedTreeTotal, " (", round(((damagedTreeTotal / totalTrees) * 100), 2), "%)"),
                                           as.numeric(round(mean(subsetDT$perc_Green), 2)),
                                           as.numeric(round(mean(subsetDT$perc_Red), 2)),
                                           as.numeric(round(mean(subsetDT$perc_Gray), 2)),
                                           as.numeric(round(mean(subsetDT$perc_Damag), 2)),
                                           nonTopKill_total, 
                                           nrow(topkill_subsetDT), 
                                           as.numeric(round(mean(topkill_subsetDT$topKill_pe), 2)), 
                                           as.numeric(round(mean(topkill_subsetDT$topKill_am), 2)))
             
             Assessment_by_damageTypeMat <- rbind(Assessment_by_damageTypeMat, Assessment_by_damageType)
             
            }
            
            
        }
    }
    
    # Damage summary different damage types 
    
    Damagetypes_List <- c("Minor damage", "Moderate damage", "Major damage", "Dead (red)", "Dead (gray)", "Dead (mixed)")
    
    for (DamageType in Damagetypes_List) {
        
        if (is.na(DamageType) == TRUE) {
            next
        } else {
            
                subsetDT <- damageAssessShp_DT[which(damageAssessShp_DT$damageType == DamageType), ]
                
                nonTopKill_total <- length(which(subsetDT$topKill_st == "Non-top-kill"))
                
                topkill_subsetDT <- subsetDT[which(subsetDT$topKill_st == "Top-kill")]
                
                Assessment_by_damageType <- c(paste0(nrow(subsetDT), " (", round(((nrow(subsetDT) / damagedTreeTotal) * 100), 2), "%)"),
                                              as.numeric(round(mean(subsetDT$perc_Green), 2)),
                                              as.numeric(round(mean(subsetDT$perc_Red), 2)),
                                              as.numeric(round(mean(subsetDT$perc_Gray), 2)),
                                              as.numeric(round(mean(subsetDT$perc_Damag), 2)),
                                              nonTopKill_total, 
                                              nrow(topkill_subsetDT), 
                                              as.numeric(round(mean(topkill_subsetDT$topKill_pe), 2)), 
                                              as.numeric(round(mean(topkill_subsetDT$topKill_am), 2)))
                
                Assessment_by_damageTypeMat <- rbind(Assessment_by_damageTypeMat, Assessment_by_damageType)
                
            }
            
            
        }
    
    topKillPercAmount_subDT <- damageAssessShp_DT[topKill_st == "Top-kill"]
    
    fullSite_classPercs <- c(paste0(totalTrees, " (", round(((totalTrees / totalTrees) * 100), 2), "%)"),
                             round(mean(damageAssessShp_DT$perc_Green), 2),
                             round(mean(damageAssessShp_DT$perc_Gray), 2),
                             round(mean(damageAssessShp_DT$perc_Red), 2),
                             round(mean(damageAssessShp_DT$perc_Damag), 2),
                             length(which(damageAssessShp_DT$topKill_st == "Non-top-kill")), 
                             length(which(damageAssessShp_DT$topKill_st == "Top-kill")),
                             round(mean(topKillPercAmount_subDT$topKill_pe), 2), 
                             round(mean(topKillPercAmount_subDT$topKill_am), 2))
    
    Assessment_by_damageTypeMat <- rbind(Assessment_by_damageTypeMat, fullSite_classPercs)
    
    outputTable <- data.table(cbind(TreeCondition, Assessment_by_damageTypeMat))
    
    colnames(outputTable) <- c("Tree Condition", "Number of trees", "Mean perc green (%)", "Mean perc red (%)", "Mean perc gray (%)", "Mean perc damage (%)", "Non-top-kill (# trees)", "Top-kill (# trees)", "Mean perc top-kill (%)", "Mean top-kill amount (m)")
    
    
    Damage_Assessment_OutputTable <<- outputTable
    message("====================================================================================================================")
    
    message("                               Damage Assessment of mission site by damage type                                     ")
    
    message("--------------------------------------------------------------------------------------------------------------------")
    print(outputTable)
    message("\n --> Output table stored as `Damage_Assessment_OutputTable` in Global Environment! |")
    message("====================================================================================================================\n")
    
    # ------
    
      
}
