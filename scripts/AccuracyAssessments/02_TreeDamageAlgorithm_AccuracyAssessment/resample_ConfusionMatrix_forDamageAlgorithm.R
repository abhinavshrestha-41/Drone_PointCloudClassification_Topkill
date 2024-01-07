###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s): 
# - Compute average confusion matrix from resampling (with replacement) and bootstrapping.

### Last update: 12/05/2023

###########################################################################


# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
message("Importing libraries...")
library(caret)
library(data.table)

# =========================================================================

#                           CHANGE VARIABLES HERE

# =========================================================================


# ------------------------- SET DIR, PATH, FILE ---------------------------

# setwd("DIR)

dir = getwd()

dataPATH = file.path(dir, "DATA")

exportPATH = file.path(dir, "EXPORTS")

# INPUTS
assessmentPoints_DamageTypeAlgo_importCSVName = "MSProject_DamageType_newAlgo_AApoints_NEW.csv" # .csv file exported from ArcGIS point shapefile attribute table

assessmentPoints_TopKillAlgo_importCSVName = "MSProject_topKillStatus_newAlgo_AApoints_NEW.csv"

# OUTPUTS
meanConfMatrix_DamageTypeAlgo_exportCSVName = "MeanConfMatrix_damageTYPES_AA_NEWdamageAlgo_500bootstrap.csv"

meanConfMatrix_TopKillAlgo_exportCSVName = "MeanConfMatrix_topKillStatus_AA_NEWdamageAlgo_500bootstrap.csv"

# ------------------ SET SCRIPT PARAMS  --------------------

num_bootstrap = 500 # number of bootstrap iterations


# =====================================================================
#   COMPUTE MEAN CONFUSION MATRIX - RESAMPLED BOOTSTRAPPING (START)
# =====================================================================

# ----------------------------------------------------------------------------------------------------------------------
#                                                   DIFFERENT DAMAGE TYPES                                         
# ----------------------------------------------------------------------------------------------------------------------


# Read AA points 
damageAssess_AA_df <- data.frame(read.csv(file.path(dataPATH, assessmentPoints_DamageTypeAlgo_importCSVName)))

# Remove OID/FID (from ArcGIS, comment out if irrelevant)
damageAssess_AA_df <- damageAssess_AA_df[,-1]

# Set variables as factor
damageAssess_AA_df$Classified <- factor(damageAssess_AA_df$Classified, 
                                           levels = c(1,2,3,4,5,6,7))

damageAssess_AA_df$GrndTruth <- factor(damageAssess_AA_df$GrndTruth, 
                                          levels = c(1,2,3,4,5,6,7))


# fixed number of samples for classes 3 to 7 
damageAssessDF_class3 <- damageAssess_AA_df[which(damageAssess_AA_df$GrndTruth == 3), ]
damageAssessDF_class4 <- damageAssess_AA_df[which(damageAssess_AA_df$GrndTruth == 4), ]
damageAssessDF_class5 <- damageAssess_AA_df[which(damageAssess_AA_df$GrndTruth == 5), ] # (no class 5 in AA)
damageAssessDF_class6 <- damageAssess_AA_df[which(damageAssess_AA_df$GrndTruth == 6), ]
damageAssessDF_class7 <- damageAssess_AA_df[which(damageAssess_AA_df$GrndTruth == 7), ]

# seperate DF to sample from for classes 1 and 2
damageAssessDF_class1 <- damageAssess_AA_df[which(damageAssess_AA_df$GrndTruth == 1), ]
damageAssessDF_class2 <- damageAssess_AA_df[which(damageAssess_AA_df$GrndTruth == 2), ]


confMat_list <- list()
boot_OA <- c()
# Initialize an empty matrix to store the mean confusion matrix
mean_confMat <- matrix(0, nrow = 7, ncol = 7)

set.seed(1234)
for (i in 1:num_bootstrap){
    
    # --------        SET N-CLASS for RESAMPLE        --------
    damageAssess_Class1_bootDF <- damageAssessDF_class1[sample(1:nrow(damageAssessDF_class1), size = 75, replace = TRUE), ]
    damageAssess_Class2_bootDF <- damageAssessDF_class2[sample(1:nrow(damageAssessDF_class2), size = 25, replace = TRUE), ]
    
    
    bootDF <- rbind(damageAssess_Class1_bootDF, 
                    damageAssess_Class2_bootDF, 
                    damageAssessDF_class3, 
                    damageAssessDF_class4,
                    damageAssessDF_class5,
                    damageAssessDF_class6, 
                    damageAssessDF_class7)
    
    confMat_obj <- caret::confusionMatrix(bootDF$Classified, bootDF$GrndTruth, mode = "everything")
    
    
    
    
    confMat_list[[i]] <- confMat_obj
    boot_OA <- c(boot_OA, confMat_list[[i]]$overall[[1]])
    mean_confMat <- mean_confMat + confMat_list[[i]]$table

}

avg_OA <- mean(boot_OA)

avg_OA


# Divide the summed confusion matrix by the number of matrices to get the mean
mean_confMat <- mean_confMat / num_bootstrap

# Optionally, you can round the values for a cleaner output
mean_confMat <- round(mean_confMat, digits = 0)

# Print the mean confusion matrix
mean_confMat



write.csv(mean_confMat, file.path(exportPATH, meanConfMatrix_DamageTypeAlgo_exportCSVName))


# ----------------------------------------------------------------------------------------------------------------------
#                                                   TOP-KILL VS NON-TOP-KILL                                       
# ----------------------------------------------------------------------------------------------------------------------

topKillAssess_AA_df <- data.frame(read.csv(file.path(dataPATH, assessmentPoints_TopKillAlgo_importCSVName)))
topKillAssess_AA_df <- topKillAssess_AA_df[,-1]

#subset only for top-kill vs non-top-kill (remove healthy class, '1')
topKillAssess_AA_df <- topKillAssess_AA_df[-which(topKillAssess_AA_df$Classified == 1),]
topKillAssess_AA_df <- topKillAssess_AA_df[-which(topKillAssess_AA_df$GrndTruth == 1),]

topKillAssess_AA_df$Classified <- factor(topKillAssess_AA_df$Classified, 
                                         levels = c(2,3))

topKillAssess_AA_df$GrndTruth <- factor(topKillAssess_AA_df$GrndTruth, 
                                        levels = c(2,3))





# seperate DF to sample from for classes 1, 2, 3
topKillAssessDF_class2 <- topKillAssess_AA_df[which(topKillAssess_AA_df$GrndTruth == 2), ]
topKillAssessDF_class3 <- topKillAssess_AA_df[which(topKillAssess_AA_df$GrndTruth == 3), ]

num_bootstrap <- 100

confMat_list <- list()
boot_OA <- c()
# Initialize an empty matrix to store the mean confusion matrix
mean_confMat <- matrix(0, nrow = 2, ncol = 2)

set.seed(1234)
for (i in 1:num_bootstrap){
  
  # --------        SET N-CLASS for RESAMPLE        --------
  topKillAssess_Class3_bootDF <- topKillAssessDF_class3[sample(1:nrow(topKillAssessDF_class3), size = 40, replace = TRUE), ]
 
  
  bootDF <- rbind(topKillAssessDF_class2,
                  topKillAssess_Class3_bootDF)
  
  confMat_obj <- caret::confusionMatrix(bootDF$Classified, bootDF$GrndTruth, mode = "everything")
  
  
  
  
  confMat_list[[i]] <- confMat_obj
  boot_OA <- c(boot_OA, confMat_list[[i]]$overall[[1]])
  mean_confMat <- mean_confMat + confMat_list[[i]]$table
  
}

avg_OA <- mean(boot_OA)

avg_OA


# Divide the summed confusion matrix by the number of matrices to get the mean
mean_confMat <- mean_confMat / num_bootstrap

# Optionally, you can round the values for a cleaner output
mean_confMat <- round(mean_confMat, digits = 0)

# Print the mean confusion matrix
mean_confMat

write.csv(mean_confMat, file.path(exportPATH, meanConfMatrix_TopKillAlgo_exportCSVName))

# =====================================================================
#   COMPUTE MEAN CONFUSION MATRIX - RESAMPLED BOOTSTRAPPING (END)
# =====================================================================