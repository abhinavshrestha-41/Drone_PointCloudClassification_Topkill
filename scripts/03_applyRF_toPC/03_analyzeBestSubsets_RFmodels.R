###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s): 
# * analyze best subsets RF model results

### Last update: 12/08/23

###########################################################################

# =========================================================================

# IMPORT LIBRARIES

# =========================================================================

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
require(plotly)
require(GGally)
require(cowplot)
require(pdp)
source("errorAccuracyOutputs_function.R") # make sure this file is in the working directory



# =========================================================================

# -------------------------------------------------------------------------

# Initialize PATH, DIR, FILE

# -------------------------------------------------------------------------

# setwd("PATH/TO/DIR")

dir  = getwd()

dataPATH = file.path(dir, "DATA")

exportPATH = file.path(dir, "EXPORTS")

refData_DF <- data.frame(read.csv(file.path(dataPATH, "PC_ReferenceData_CloudCompare_PointID.csv")))
refData_DF$class <- factor(refData_DF$class)

# -------------------------------------------------------------------------

# Import CSVs of best subsets tests with different numbers of RF

oneVar_bestSubsetRFmodelsDF <- data.frame(read.csv(file.path(exportPATH, "RF_1VAR_OOB_rankedTable.csv")))
oneVar_bestSubsetRFmodelsDF$numVars <- 1

twoVar_bestSubsetRFmodelsDF <- data.frame(read.csv(file.path(exportPATH, "RF_2VAR_OOB_OA_Corr_rankedTable.csv")))
twoVar_bestSubsetRFmodelsDF$numVars <- 2

threeVar_bestSubsetRFmodelsDF <- data.frame(read.csv(file.path(exportPATH, "RF_3VAR_OOB_OA_Corr_rankedTable.csv")))
threeVar_bestSubsetRFmodelsDF$numVars <- 3

fourVar_bestSubsetRFmodelsDF <- data.frame(read.csv(file.path(exportPATH, "RF_4VAR_OOB_OA_Corr_rankedTable.csv")))
fourVar_bestSubsetRFmodelsDF$numVars <- 4

# -------------------------------------------------------------------------

combinedDF <- rbind(oneVar_bestSubsetRFmodelsDF, 
                    twoVar_bestSubsetRFmodelsDF[,c(1:4, ncol(twoVar_bestSubsetRFmodelsDF))], threeVar_bestSubsetRFmodelsDF[,c(1:4, ncol(threeVar_bestSubsetRFmodelsDF))], fourVar_bestSubsetRFmodelsDF[,c(1:4, ncol(fourVar_bestSubsetRFmodelsDF))])

combinedDF$Variable <- as.factor(combinedDF$Variable)

combinedDF$numVarsFactor <- as.factor(combinedDF$numVars)


# ---------------- PLOT OA by number of variables used in RF model ----------------

ggplot(data = combinedDF, aes(x = numVars, y = OA)) +
  # geom_jitter(alpha = 0.1, width = 0.05) + # for jitter points 
  geom_line(data = aggregate(OA ~ numVars, data = combinedDF, FUN = max), aes(group = 1), color = "red", linewidth = 1) +
  geom_point(data = aggregate(OA ~ numVars, data = combinedDF, FUN = max)) + 
  labs(x = "Number of explanatory variables used in random forest model", 
       y = "Overall accuracy (%)") +
  xlim(1, 4) +
  ylim(80, 100) + 
  theme_classic() +
    theme(legend.position = "none", 
          axis.text = element_text(size = 14), 
          axis.title.x = element_text(size = 18, margin = ggplot2::margin(t = 20, unit = "pt")),  
          axis.title.y = element_text(size = 18, margin = ggplot2::margin(r = 20, unit = "pt"))) 

# -------------------------------------------------------------------------


# ---------------- PLOT GGpairs plots for RF model ---------------- 

varList <- c("RBI", "NDVI", "REDEDGE")

subsetDF <- as.data.frame(refData_DF[,c(varList, "class", "ID_point")])

# ggpairs plot with colored scatterplot and histogram plots
GGally::ggpairs(subsetDF, 
                columns = varList,  # Specify the columns for scatterplot and histogram
                mapping = aes(color = class, fill = class),
                lower = list(continuous = "points", combo = "facetdensity"),
                diag = list(continuous = "barDiag"), 
                title = "RBI + NDVI + REDEDGE (three-variable random forest model)", 
                legend = 1) +
  theme(title = element_text(size = 16)) +
  scale_color_manual(values = c("darkgreen", "darkgray", "red", "black"), 
                     guide = "none") +
  scale_fill_manual(values = c("darkgreen", "darkgray", "red", "black"), 
                    name = "Class", 
                    labels = c("Green", "Gray", "Red", "Shadows"))



# ------------------------------------------------------------------------

# ---------------- Partial dependency plots ---------------- 

Vars <- c("RBI", "NDVI", "REDEDGE")
gridObj <- list()

for (v in seq_along(Vars)){
  
  formula <- as.formula(paste("class ~ RGI +", Vars[v], collapse = "+"))
  
  # RF model
  set.seed(1234)
  rf_model <- randomForest::randomForest(class ~ RBI + NDVI + REDEDGE, 
                                                 data = refData_DF, 
                                                 ntree = 500, 
                                                 keep.forest = TRUE, 
                                                 importance = TRUE)

  # output results in a formatted table using `errorAccuracyOutputs` function (sourced in the Import section)
  errorAccuracyOutputs(rf_model)
  
  
  # ALTERNATIVELY: manually set variable list
  var_list <- attributes(rf_model$importance)$dimnames[[1]]
  
  # PLOT PARTIAL DEPENDENCE PLOTS
  
  class_list = c("green", "gray", "red", "shadows") # for auto labeling partial dependence plots
  
  
  # Set flag to determine class to plot partial dependence (1: green, 2: gray, 3: red, 4: shadow)
  class_flag = 3 
  
  
  
  autoplotObj <- list()
  
  for (i in seq_along(var_list)) {
    
    pdpObj <- pdp::partial(rf_model, 
                           which.class = class_flag,
                           pred.var = var_list[i], 
                           plot = FALSE)
    autoplotObj[[i]] <- ggplot2::autoplot(pdpObj,
                                       plot.pdp = TRUE,
                                       linetype = 2,
                                       smooth = TRUE,
                                       smooth.method = "auto",
                                       rug = TRUE,
                                       ylab = "",
                                       xlab = var_list[i],
                                       train = refData_DF, 
                                       main = paste(class_list[class_flag], "class partial dependence",var_list[i]))
  }
  
  gridObj[[v]] <- gridExtra::grid.arrange(grobs = autoplotObj, nrow = 1, ncol = 2)

}

do.call(gridExtra::grid.arrange, c(gridObj, nrow = 3, ncol = 2))
# by using do.call() to pass the elements of the gridObj list as arguments to grid.arrange(). This ensures that the grid.arrange() function receives the correct arguments and creates the grid arrangement of Large gtable objects.

