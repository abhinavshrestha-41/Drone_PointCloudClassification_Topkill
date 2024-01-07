###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s): 
# * Best Subsets approach for RF variable and model selection 

### Last update: 12/5/23

###########################################################################

# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
setwd("C:\\Users\\abhin\\Documents\\GitHub\\MS_Project_Scripts\\MS_Project_Workspace\\R_Workspace\\R_Scripts\\RF_Diagnotics")


require(tidyr)
require(dplyr)
require(terra)
require(ggplot2)
require(lidR)
require(sf)
require(sp)
require(ggrepel)
require(randomForest)
require(rfUtilities)
require(data.table)
require(caret)
source("errorAccuracyOutputs_function.R") # make sure this file is in the working directory


# =========================================================================

#                           CHANGE VARIABLES HERE

# =========================================================================


# ------------------------- SET DIR, PATH, FILE ---------------------------

dir  = getwd()

dataPATH = file.path(dir, "DATA")

exportPATH = file.path(dir, "EXPORTS")

refData_csv_name = "PC_ReferenceData_CloudCompare_PointID_V2.csv" # INPUT (.csv file of reference data generated with "create_ReferenceData_POINTS.R" script)

# -------------------------------------------------------------------------



# -------------------------------------------------------------------------

# Import reference data set 

refData_csv_fullPath = file.path(dataPATH, refData_csv_name)
refData_csv_file = read.csv(refData_csv_fullPath)

refData_DT <- data.table(refData_csv_file) # convert csv file to data table (faster)

refData_DT$class <- as.factor(refData_DT$class) # set class as factor for classification RF

num_RFTree <- 500 # number of tree for random forest model

# -------------------------------------------------------------------------



# -------------------------------------------------------------------------
# 1 Variable RF model selection 


# List of variables in reference data (minus class label)
varList <- names(refData_DT[, -c(14:15)])

# Initialize empty data table 
RF_1Var_rankTable <- data.table(Variable = character(), OOB = numeric(), OA = numeric())

ptm <- Sys.time()
for (i in seq(1:length(varList))) {

  # Specify the formula using the actual column names from the dataset
  formula <- as.formula(paste("class ~", paste(varList[i])))
  
  set.seed(1234)
  rf_model <- randomForest::randomForest(formula = formula, 
                                         data = refData_DT, 
                                         ntree = num_RFTree, 
                                         keep.forest = TRUE, 
                                         importance = TRUE)
  
  # Calculate accuracy metrics of each model
  variableName <- varList[i]
  OOB_var <- ((rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]) * 100)[[1]]
  OA_var  <- sum(diag(rf_model$confusion[,-5])) / sum(rf_model$confusion[,-5]) * 100
  
  # Add accuracy metrics data to table
  RF_1Var_rankTable <- rbindlist(list(RF_1Var_rankTable, data.table(Variable = variableName, 
                                                                    OOB = OOB_var, 
                                                                    OA = OA_var)))
  
}
Sys.time() - ptm

RF_1Var_OOB_rankTable <- RF_1Var_rankTable[order(OOB)]

# create a list of variables (OOB based rank)
RF_1Var_OOB_rankedList <- c(RF_1Var_OOB_rankTable[[1]])

# ~~~~~~~~~~~~~~~~    CHANGE VARIABLES HERE FOR EXPORT    ~~~~~~~~~~~~~~~~

export_csvName <- "RF_1VAR_OOB_rankedTable_V2.csv"
export_csvPath <- file.path(exportPATH, export_csvName)

write.csv(RF_1Var_OOB_rankTable, export_csvPath)

# -------------------------------------------------------------------------


# -------------------------------------------------------------------------

# 2 variable model selection

# Initialize empty data table 
RF_2Var_rankTable <- data.table(Variable = character(), Corr = numeric(), OOB = numeric(), OA = numeric())

# Set correlation threshold
corrTH <- 0.7

ptm <- Sys.time()
for (i in 1:length(RF_1Var_OOB_rankedList)){
  
  for (j in i:length(RF_1Var_OOB_rankedList)){
    
    # To not run model with variable
    if (RF_1Var_OOB_rankedList[i] != RF_1Var_OOB_rankedList[j]){ 
      
      # Run models for variables that have |correlation| < 0.7
      if (abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[j])], method = "pearson")) < corrTH){
        
        formula <- as.formula(paste("class ~", paste(RF_1Var_OOB_rankedList[i], "+", RF_1Var_OOB_rankedList[j], collapse = "+")))
        
        set.seed(1234)
        rf_model <- randomForest::randomForest(formula = formula, 
                                               data = refData_DT, 
                                               ntree = num_RFTree, 
                                               keep.forest = TRUE, 
                                               importance = TRUE)
        
        # Calculate accuracy metrics of each model
        variableName <- paste(RF_1Var_OOB_rankedList[i], "+", RF_1Var_OOB_rankedList[j], collapse = "+")
        corr_var <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[j])], method = "pearson")
        OOB_var <-((rf_model$err.rate[nrow(rf_model$err.rate), "OOB"]) * 100)[[1]]
        OA_var  <- sum(diag(rf_model$confusion[,-5])) / sum(rf_model$confusion[,-5]) * 100
        
        # Add accuracy metrics data to table
        RF_2Var_rankTable <- rbindlist(list(RF_2Var_rankTable, data.table(Variable = variableName, 
                                                                          Corr = corr_var,
                                                                          OOB = OOB_var, 
                                                                          OA = OA_var)))
        
      }
    }
    
  }
 
}
Sys.time() - ptm
RF_2Var_OOB_OA_Corr_rankTable <- RF_2Var_rankTable[order(OOB, -OA, abs(Corr))]

setcolorder(RF_2Var_OOB_OA_Corr_rankTable, c(1,3,4,2))
head(RF_2Var_OOB_OA_Corr_rankTable, n = 20)

#RF_2Var_OOB_rankTable <- RF_2Var_rankTable[order(OOB)]
#head(RF_2Var_OOB_rankTable, n = 10)


# ~~~~~~~~~~~~~~~~    CHANGE VARIABLES HERE FOR EXPORT    ~~~~~~~~~~~~~~~~


export_csvName <- "RF_2VAR_OOB_OA_Corr_rankedTable_V2.csv"
export_csvPath <- file.path(exportPATH, export_csvName)

write.csv(RF_2Var_OOB_OA_Corr_rankTable, export_csvPath)

# -------------------------------------------------------------------------




# -------------------------------------------------------------------------

# 3 variable model selection 

# Initialize empty data table
RF_3Var_rankTable <- data.table(Variable = character(), OOB = numeric(), OA = numeric(), Corr1_2 = numeric(), Corr1_3 = numeric(), Corr2_3 = numeric())
corrTH <- 0.7 # Set correlation threshold

ptm <- Sys.time()
for (i in 1:length(RF_1Var_OOB_rankedList)){
  
  for (j in i:length(RF_1Var_OOB_rankedList)){
    
    for (k in j:length(RF_1Var_OOB_rankedList)){
      
      if (i != j & i != k & j != k) {
        
        if (abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[j])], method = "pearson")) < corrTH &
            abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[k])], method = "pearson")) < corrTH &
            abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[j])], refData_DT[, get(RF_1Var_OOB_rankedList[k])], method = "pearson")) < corrTH){
          
          formula <- as.formula(paste("class ~", paste(RF_1Var_OOB_rankedList[i], "+", RF_1Var_OOB_rankedList[j], "+", RF_1Var_OOB_rankedList[k], collapse = "+")))
          
          set.seed(1234)
          rf_model <- randomForest::randomForest(formula = formula, 
                                                 data = refData_DT, 
                                                 ntree = num_RFTree, 
                                                 keep.forest = TRUE, 
                                                 importance = TRUE)
          
          variableName <- paste(RF_1Var_OOB_rankedList[i], "+", RF_1Var_OOB_rankedList[j], "+", RF_1Var_OOB_rankedList[k], collapse = "+")
          corr_1_2 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[j])], method = "pearson")
          corr_1_3 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[k])], method = "pearson")
          corr_2_3 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[j])], refData_DT[, get(RF_1Var_OOB_rankedList[k])], method = "pearson")
          
          OOB_var <- ((rf_model$err.rate[nrow(rf_model$err.rate), "OOB"])*100)[[1]]
          OA_var  <- sum(diag(rf_model$confusion[,-5])) / sum(rf_model$confusion[,-5]) * 100
          
          RF_3Var_rankTable <- rbindlist(list(RF_3Var_rankTable, data.table(Variable = variableName, 
                                                                            OOB = OOB_var, 
                                                                            OA = OA_var, 
                                                                            Corr1_2 = corr_1_2,
                                                                            Corr1_3 = corr_1_3, 
                                                                            Corr2_3 = corr_2_3)))
          
        }
      }
    }
  }
}

Sys.time() - ptm


RF_3Var_rankTable[, max_absoluteCorr := apply(abs(RF_3Var_rankTable[, c("Corr1_2", "Corr1_3", "Corr2_3")]), 1, max)]

RF_3Var_OOB_OA_Corr_rankTable <- RF_3Var_rankTable[order(OOB, -OA, max_absoluteCorr)]

setcolorder(RF_3Var_OOB_OA_Corr_rankTable, c(1,2,3,ncol(RF_3Var_OOB_OA_Corr_rankTable)))

head(RF_3Var_OOB_OA_Corr_rankTable, n = 20)

RF_3Var_OOB_rankTable[1]

# ~~~~~~~~~~~~~~~~    CHANGE VARIABLES HERE FOR EXPORT    ~~~~~~~~~~~~~~~~

export_csvName <- "RF_3VAR_OOB_OA_Corr_rankedTable_V2.csv"
export_csvPath <- file.path(exportPATH, export_csvName)

write.csv(RF_3Var_OOB_OA_Corr_rankTable , export_csvPath)

# ------------------------------------------------------------------------

# -------------------------------------------------------------------------

# 4 variable model selection
# Initialize empty data table
RF_4Var_rankTable <- data.table(Variable = character(), OOB = numeric(), OA = numeric(), Corr1_2 = numeric(), Corr1_3 = numeric(), Corr1_4 = numeric(), Corr2_3 = numeric(), Corr2_4 = numeric(), Corr3_4 = numeric())
corrTH <- 0.7 # Set correlation threshold

ptm <- Sys.time()
for (i in 1:length(RF_1Var_OOB_rankedList)){
  
  for (j in i:length(RF_1Var_OOB_rankedList)){
    
    for (k in j:length(RF_1Var_OOB_rankedList)){
      
      for (l in k:length(RF_1Var_OOB_rankedList)){
        
        if (i != j & i != k & i != l & j != k & j != l & k != l){
          
          if (abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[j])], method = "pearson")) < corrTH &
              abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[k])], method = "pearson")) < corrTH &
              abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[l])], method = "pearson")) < corrTH &
              abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[j])], refData_DT[, get(RF_1Var_OOB_rankedList[k])], method = "pearson")) < corrTH &
              abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[j])], refData_DT[, get(RF_1Var_OOB_rankedList[l])], method = "pearson")) < corrTH &
              abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[k])], refData_DT[, get(RF_1Var_OOB_rankedList[l])], method = "pearson")) < corrTH){
            
            formula <- as.formula(paste("class ~", paste(RF_1Var_OOB_rankedList[i], "+", RF_1Var_OOB_rankedList[j], "+", RF_1Var_OOB_rankedList[k], "+", RF_1Var_OOB_rankedList[l], collapse = "+")))
            
            set.seed(1234)
            rf_model <- randomForest::randomForest(formula = formula,
                                                   data = refData_DT,
                                                   ntree = num_RFTree,
                                                   keep.forest = TRUE,
                                                   importance = TRUE)
            variableName <- paste(RF_1Var_OOB_rankedList[i], "+", RF_1Var_OOB_rankedList[j], "+", RF_1Var_OOB_rankedList[k], "+", RF_1Var_OOB_rankedList[l], collapse = "+")
            corr_1_2 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[j])], method = "pearson")
            corr_1_3 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[k])], method = "pearson")
            corr_1_4 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[l])], method = "pearson")
            corr_2_3 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[j])], refData_DT[, get(RF_1Var_OOB_rankedList[k])], method = "pearson")
            corr_2_4 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[j])], refData_DT[, get(RF_1Var_OOB_rankedList[l])], method = "pearson")
            corr_3_4 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[k])], refData_DT[, get(RF_1Var_OOB_rankedList[l])], method = "pearson")
            OOB_var <- ((rf_model$err.rate[nrow(rf_model$err.rate), "OOB"])*100)[[1]]
            OA_var  <- sum(diag(rf_model$confusion[,-5])) / sum(rf_model$confusion[,-5]) * 100
            RF_4Var_rankTable <- rbindlist(list(RF_4Var_rankTable, data.table(Variable = variableName,
                                                                              OOB = OOB_var,
                                                                              OA = OA_var,
                                                                              Corr1_2 = corr_1_2,
                                                                              Corr1_3 = corr_1_3,
                                                                              Corr1_4 = corr_1_4,
                                                                              Corr2_3 = corr_2_3,
                                                                              Corr2_4 = corr_2_4,
                                                                              Corr3_4 = corr_3_4)))
          }
        }
      }
    }
  }
}
Sys.time() - ptm

RF_4Var_rankTable[, max_absoluteCorr := apply(abs(RF_4Var_rankTable[, c("Corr1_2", "Corr1_3", "Corr1_4", "Corr2_3", "Corr2_4", "Corr3_4")]), 1, max)]
RF_4Var_OOB_OA_Corr_rankTable <- RF_4Var_rankTable[order(OOB, -OA, max_absoluteCorr)]

setcolorder(RF_4Var_OOB_OA_Corr_rankTable, c(1,2,3,ncol(RF_4Var_OOB_OA_Corr_rankTable)))

head(RF_4Var_OOB_OA_Corr_rankTable, n = 20)

# ~~~~~~~~~~~~~~~~    CHANGE VARIABLES HERE FOR EXPORT    ~~~~~~~~~~~~~~~~

export_csvName <- "RF_4VAR_OOB_OA_Corr_rankedTable_V2.csv"
export_csvPath <- file.path(exportPATH, export_csvName)
write.csv(RF_4Var_OOB_OA_Corr_rankTable, export_csvPath)



# -------------------------------------------------------------------------

# 5 variable model selection

# Initialize empty data table
RF_5Var_rankTable <- data.table(Variable = character(), OOB = numeric(), OA = numeric(), Corr1_2 = numeric(), Corr1_3 = numeric(), Corr1_4 = numeric(), Corr1_5 = numeric(), Corr2_3 = numeric(), Corr2_4 = numeric(), Corr2_5 = numeric(), Corr3_4 = numeric(), Corr3_5 = numeric(), Corr4_5 = numeric())

#corrTH <- 0 # Set correlation threshold

ptm <- Sys.time()
for (i in 1:length(RF_1Var_OOB_rankedList)){
  for (j in i:length(RF_1Var_OOB_rankedList)){
    for (k in j:length(RF_1Var_OOB_rankedList)){
      for (l in k:length(RF_1Var_OOB_rankedList)){
        for (m in l:length(RF_1Var_OOB_rankedList)){
          if (i != j & i != k & i != l & i != m & j != k & j != l & j != m & k != l & k != m & l != m){
            if (abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[j])], method = "pearson")) < corrTH &
                abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[k])], method = "pearson")) < corrTH &
                abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[l])], method = "pearson")) < corrTH &
                abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[m])], method = "pearson")) < corrTH &
                abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[j])], refData_DT[, get(RF_1Var_OOB_rankedList[k])], method = "pearson")) < corrTH &
                abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[j])], refData_DT[, get(RF_1Var_OOB_rankedList[l])], method = "pearson")) < corrTH &
                abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[j])], refData_DT[, get(RF_1Var_OOB_rankedList[m])], method = "pearson")) < corrTH &
                abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[k])], refData_DT[, get(RF_1Var_OOB_rankedList[l])], method = "pearson")) < corrTH &
                abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[k])], refData_DT[, get(RF_1Var_OOB_rankedList[m])], method = "pearson")) < corrTH &
                abs(cor(refData_DT[, get(RF_1Var_OOB_rankedList[l])], refData_DT[, get(RF_1Var_OOB_rankedList[m])], method = "pearson")) < corrTH){
            formula <- as.formula(paste("class ~", paste(RF_1Var_OOB_rankedList[i], "+", RF_1Var_OOB_rankedList[j], "+", RF_1Var_OOB_rankedList[k], "+", RF_1Var_OOB_rankedList[l], "+", RF_1Var_OOB_rankedList[m],collapse = "+")))
            set.seed(1234)
            rf_model <- randomForest::randomForest(formula = formula,
                                                   data = refData_DT,
                                                   ntree = num_RFTree,
                                                   keep.forest = TRUE,
                                                   importance = TRUE)
            variableName <- paste(RF_1Var_OOB_rankedList[i], "+", RF_1Var_OOB_rankedList[j], "+", RF_1Var_OOB_rankedList[k], "+", RF_1Var_OOB_rankedList[l], "+", RF_1Var_OOB_rankedList[m], collapse = "+")
            corr_1_2 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[j])], method = "pearson")
            corr_1_3 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[k])], method = "pearson")
            corr_1_4 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[l])], method = "pearson")
            corr_1_5 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[i])], refData_DT[, get(RF_1Var_OOB_rankedList[m])], method = "pearson")
            corr_2_3 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[j])], refData_DT[, get(RF_1Var_OOB_rankedList[k])], method = "pearson")
            corr_2_4 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[j])], refData_DT[, get(RF_1Var_OOB_rankedList[l])], method = "pearson")
            corr_2_5 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[j])], refData_DT[, get(RF_1Var_OOB_rankedList[m])], method = "pearson")
            corr_3_4 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[k])], refData_DT[, get(RF_1Var_OOB_rankedList[l])], method = "pearson")
            corr_3_5 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[k])], refData_DT[, get(RF_1Var_OOB_rankedList[m])], method = "pearson")
            corr_4_5 <- cor(refData_DT[, get(RF_1Var_OOB_rankedList[l])], refData_DT[, get(RF_1Var_OOB_rankedList[m])], method = "pearson")
            OOB_var <- ((rf_model$err.rate[nrow(rf_model$err.rate), "OOB"])*100)[[1]]
            OA_var  <- sum(diag(rf_model$confusion[,-5])) / sum(rf_model$confusion[,-5]) * 100
            RF_5Var_rankTable <- rbindlist(list(RF_5Var_rankTable, data.table(Variable = variableName,
                                                                              OOB = OOB_var,
                                                                              OA = OA_var,
                                                                              Corr1_2 = corr_1_2,
                                                                              Corr1_3 = corr_1_3,
                                                                              Corr1_4 = corr_1_4,
                                                                              Corr1_5 = corr_1_5,
                                                                              Corr2_3 = corr_2_3,
                                                                              Corr2_4 = corr_2_4,
                                                                              Corr2_5 = corr_2_5,
                                                                              Corr3_4 = corr_3_4,
                                                                              Corr3_5 = corr_3_5,
                                                                              Corr4_5 = corr_4_5)))
          }
        }
      }
    }
  }
}
#}
Sys.time() - ptm
RF_5Var_rankTable[, max_absoluteCorr := apply(abs(RF_5Var_rankTable[, c("Corr1_2", "Corr1_3", "Corr1_4", "Corr1_5", "Corr2_3", "Corr2_4", "Corr2_5", "Corr3_4", "Corr3_5", "Corr4_5")]), 1, max)]
RF_5Var_OOB_OA_Corr_rankTable <- RF_5Var_rankTable[order(OOB, -OA, max_absoluteCorr)]

setcolorder(RF_5Var_OOB_OA_Corr_rankTable, c(1,2,3,ncol(RF_5Var_OOB_OA_Corr_rankTable)))
head(RF_5Var_OOB_OA_Corr_rankTable, n = 20)

# ~~~~~~~~~~~~~~~~    CHANGE VARIABLES HERE FOR EXPORT    ~~~~~~~~~~~~~~~~

export_csvName <- "RF_5VAR_OOB_OA_Corr_rankedTable.csv"
export_csvPath <- file.path(exportPATH, export_csvName)
write.csv(RF_5Var_OOB_OA_Corr_rankTable, export_csvPath)


# =========================================================================


# Based on RF accuracy metrics ranked table, construct 1 var, 2 var, and 3 var RF models

head(RF_1Var_OOB_rankTable, n = 5)
head(RF_2Var_OOB_OA_Corr_rankTable, n = 15)
head(RF_3Var_OOB_OA_Corr_rankTable, n = 5)



# -------------------------------------------------------------------------

rf_1Var_GLI <- randomForest::randomForest(class ~ GLI, 
                                          data = refData_DT, 
                                          ntree = num_RFTree, 
                                          keep.forest = TRUE, 
                                          importance = TRUE)
# print rf model results
print(rf_1Var_GLI) 

# output results in a formatted table using `errorAccuracyOutputs` function (sourced in the Import section)
errorAccuracyOutputs(rf_1Var_GLI)

# PLOT PARTIAL DEPENDENCE PLOTS

# Type is set to 1 to return mean decrease accuracy metric only
rf_1Var_GLI_ImportanceTable <- randomForest::importance(rf_1Var_GLI, scale = TRUE, type = 1) 

# From rf_model_ImportanceTable, create list of variable names in decreasing order of mean decrease accuracy value
var_list_1Var_GLI <- rownames(rf_1Var_GLI_ImportanceTable)[order(rf_1Var_GLI_ImportanceTable, decreasing=TRUE)]


class_list = c("green", "gray", "red", "shadows") # for auto labeling partial dependence plots


# Set flag to determine class to plot partial dependence (1: green, 2: gray, 3: red, 4: shadow)

class_flag = 2

# plot
#plotArea <- par(mfrow=c(2, 3)) # comment-out this line and `par(plotArea)` after for loop for individual plot outputs
for (i in seq_along(var_list_1Var_GLI)) {
  partialPlot(rf_1Var_GLI, 
              pred.data   = refData_DT, 
              which.class = class_flag, 
              x.var       = var_list_1Var_GLI[i], 
              xlab        = var_list_1Var_GLI[i],
              main        = paste( class_list[class_flag], "class partial dependence", var_list_1Var_GLI[i]))
}
#par(plotArea)

# -------------------------------------------------------------------------



# -------------------------------------------------------------------------


rf_2Var_GLI_GRE <- randomForest::randomForest(class ~ GLI + GRE, 
                                          data = refData_DT, 
                                          ntree = num_RFTree, 
                                          keep.forest = TRUE, 
                                          importance = TRUE)
# print rf model results
print(rf_2Var_GLI_GRE) 

# output results in a formatted table using `errorAccuracyOutputs` function (sourced in the Import section)
errorAccuracyOutputs(rf_2Var_GLI_GRE)

# PLOT PARTIAL DEPENDENCE PLOTS

# Type is set to 1 to return mean decrease accuracy metric only
rf_2Var_GLI_GRE_ImportanceTable <- randomForest::importance(rf_2Var_GLI_GRE, scale = TRUE, type = 1) 

# From rf_model_ImportanceTable, create list of variable names in decreasing order of mean decrease accuracy value
var_list_2Var_GLI_GRE <- rownames(rf_2Var_GLI_GRE_ImportanceTable)[order(rf_2Var_GLI_GRE_ImportanceTable, decreasing=TRUE)]


class_list = c("green", "gray", "red", "shadows") # for auto labeling partial dependence plots


# Set flag to determine class to plot partial dependence (1: green, 2: gray, 3: red, 4: shadow)

class_flag = 2

# plot
#plotArea <- par(mfrow=c(2, 3)) # comment-out this line and `par(plotArea)` after for loop for individual plot outputs
for (i in seq_along(var_list_2Var_GLI_GRE)) {
  partialPlot(rf_2Var_GLI_GRE, 
              pred.data   = refData_DT, 
              which.class = class_flag, 
              x.var       = var_list_2Var_GLI_GRE[i], 
              xlab        = var_list_2Var_GLI_GRE[i],
              main        = paste( class_list[class_flag], "class partial dependence", var_list_2Var_GLI_GRE[i]))
}
#par(plotArea)



# -------------------------------------------------------------------------






# -------------------------------------------------------------------------

rf_3Var_GLI_GRE_RBI <- randomForest::randomForest(class ~ GLI + GRE + RBI, 
                                              data = refData_DT, 
                                              ntree = num_RFTree, 
                                              keep.forest = TRUE, 
                                              importance = TRUE)
# print rf model results
print(rf_3Var_GLI_GRE_RBI) 

# output results in a formatted table using `errorAccuracyOutputs` function (sourced in the Import section)
errorAccuracyOutputs(rf_3Var_GLI_GRE_RBI)  

# PLOT PARTIAL DEPENDENCE PLOTS

# Type is set to 1 to return mean decrease accuracy metric only
rf_3Var_GLI_GRE_RBI_ImportanceTable <- randomForest::importance(rf_3Var_GLI_GRE_RBI, scale = TRUE, type = 1) 

# From rf_model_ImportanceTable, create list of variable names in decreasing order of mean decrease accuracy value
var_list_3Var_GLI_GRE_RBI <- rownames(rf_3Var_GLI_GRE_RBI_ImportanceTable)[order(rf_3Var_GLI_GRE_RBI_ImportanceTable, decreasing=TRUE)]


class_list = c("green", "gray", "red", "shadows") # for auto labeling partial dependence plots


# Set flag to determine class to plot partial dependence (1: green, 2: gray, 3: red, 4: shadow)

class_flag = 2

# plot
#plotArea <- par(mfrow=c(2, 3)) # comment-out this line and `par(plotArea)` after for loop for individual plot outputs
for (i in seq_along(var_list_3Var_GLI_GRE_RBI)) {
  partialPlot(rf_3Var_GLI_GRE_RBI, 
              pred.data   = refData_DT, 
              which.class = class_flag, 
              x.var       = var_list_3Var_GLI_GRE_RBI[i], 
              xlab        = var_list_3Var_GLI_GRE_RBI[i],
              main        = paste( class_list[class_flag], "class partial dependence", var_list_3Var_GLI_GRE_RBI[i]))
}
#par(plotArea)
