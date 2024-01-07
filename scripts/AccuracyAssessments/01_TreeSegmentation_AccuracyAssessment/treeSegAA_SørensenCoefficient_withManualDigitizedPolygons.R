###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s): 
# - Compute Sørensen Coefficient for accuracy assessment of tree segmentation (where, reference data are the manually digitized tree crowns)

### Last update: 12/05/2023

###########################################################################


# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
message("Importing libraries...")
library(data.table)

# =========================================================================

#                           CHANGE VARIABLES HERE

# =========================================================================


# ------------------------- SET DIR, PATH, FILE ---------------------------

dir = getwd()

dataPath = file.path(dir, "DATA")


# INPUT
assessmentPolygons_csvName = "ManualDigitization_V_treeSeg_100.csv"


# =====================================================================
#                   COMPUTE Sørensen Coefficient (START)
# =====================================================================

dt <- data.table::data.table(read.csv(file.path(dataPath, csvName)))

classA_dt <- dt[which(dt$Class_ABC == "A"), ]
classB_dt <- dt[which(dt$Class_ABC == "B"), ]
classC_dt <- dt[which(dt$Class_ABC == "C"), ]


classA_avgArea <- mean(classA_dt$Shape_Area)
classB_avgArea <- mean(classB_dt$Shape_Area)
classC_avgArea <- mean(classC_dt$Shape_Area)

SC = ((2 * classA_avgArea)/((2 * classA_avgArea) + classB_avgArea + classC_avgArea))

SC

# =====================================================================
#                   COMPUTE Sørensen Coefficient (END) 
# =====================================================================
