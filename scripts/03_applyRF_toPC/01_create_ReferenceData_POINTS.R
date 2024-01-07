###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: shre9292@vandals.uidaho.edu

### Purpose(s): 
# * Assemble point cloud (PC) based reference (ref) data set (create/export PC with point ID attribute) 

### Last update: 12/05/23

###########################################################################

ptm <- Sys.time()

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
require(rfUtilities)
require(data.table)

# =========================================================================


# =========================================================================

#                           CHANGE VARIABLES HERE

# =========================================================================


# ------------------------- SET DIR, PATH, FILE ---------------------------

# setwd("PATH/TO/DIR")

dir  = getwd()

dataPATH = file.path(dir, "DATA")

exportPATH = file.path(dir, "EXPORTS")

pointCloud_name = "MS_Subset_PointID.las" # INPUT (point cloud with "ID_point" feature)

RefData_Name = "CC_RefData _V3.csv" # INPUT (.csv file created from assembling reference data on CloudCompare with "ID_point" feature)

RefData_CombinedCSVName = "PC_ReferenceData_CloudCompare_PointID_V2.csv"


# ====================       COMBINE CSV AND POINT CLOUD for REF DATA (START)     =====================


# -------------------------------------------------------------------------

# CREATE POINT CLOUD DATA WITH POINT ID FOR USE IN CLOUDCOMPARE 

# - uncomment the code to export a point cloud with a "point ID" attribute to use in CloudCompare to assemble the reference data set of points

# -------------------------------------------------------------------------


# # Import point cloud data
# 
# pointCloud_name = "DATA/Tree_PCsegmented_MSsubset.las"
# 
# pointCloud_Full = file.path(dir, pointCloud_name)
# 
# pointCloud_file <- lidR::readLAS(pointCloud_Full)
# 
# 
# # Extract attributes of point cloud data as a data table 
# 
# pointCloud_DT <- data.table::data.table(pointCloud_file@data)
# 
# 
# # Add new column to the data table named "ID_point" and assign numbers from 1 to nrow(DT)
# # Note: Could not name the attribute "point_ID/pointID" as .las data format already has an attribute with similar name.
# 
# pointCloud_DT$ID_point<- seq(from = 1, to = nrow(pointCloud_DT), by = 1)
# 
# # Add new DT with "ID_point" attribute back into LAS point cloud file 
# 
# pointCloud_file@data <- pointCloud_DT
# 
# 
# # To export the LAS point cloud file with the newly added attributes, the new attributes need to be added to the header file of the point cloud
# 
# pointCloud_file <- lidR::add_lasattribute(pointCloud_file,
#                                     name = "ID_point", 
#                                     desc = "ID number for each point")
# 
# print(lidR::header(pointCloud_file)) # to check if attribute was added
#                                      # appears in the "Extra Bytes Description"
# 
# 
# 
# # Export point cloud LAS file
# 
# pointCloud_exportName = "MS_Subset_PointID.las"
# pointCloud_exportFull = file.path(exportPATH, pointCloud_exportName)
# 
# 
# lidR::writeLAS(pointCloud_file, pointCloud_exportFull)
# 
# test <- lidR::readLAS(pointCloud_exportFull) # test if export was successfull.
# names(test@data)


# -------------------------------------------------------------------------

# IMPORT POINT CLOUD AND COMBINE WITH CSV FILE (REF DATA)

# - Imported point cloud MUST have "ID_point" attribute (created from the above commented out code)
# - .CSV file created from assembling reference data set using CloudCompare

# -------------------------------------------------------------------------


# Import point cloud data

pointCloud_Full = file.path(exportPATH, pointCloud_name)

pointCloud_file <- lidR::readUAVLAS(pointCloud_Full)


# Extract attributes of point cloud data as a data table 

pointCloud_DT <- data.table::data.table(pointCloud_file@data)


# IMPORT REF DATA

RefData_Full = file.path(dir, RefData_Name)

CSV_RefData <- read.csv(RefData_Full)
CSV_RefData_DT <- data.table(CSV_RefData) # store as data table


# Join ref data with Point cloud data by point ID ("ID_point")
CC_refData_DT <- merge.data.table(pointCloud_DT, CSV_RefData_DT, by = "ID_point")
CC_refData_DT <- CC_refData_DT[, c(1, 17:21, 23)] # subset bands and reference class only

CC_refData_DT[,2:6] <- (CC_refData_DT[,2:6]/10000) # reflectances stored as (value*100000), revert back to true reflectance 

colnames(CC_refData_DT)[2:6] <- c('BLU','GRE','RED', 'REDEDGE', 'NearIR') # rename column headers with band name
                                                                       # Note: named NIR as NearIR as .las format already has NIR attribute 


# Create new columns for indices and perform calculations; colnames should be same as ones used for rf_classifier

CC_refData_DT$NDVI <- ( CC_refData_DT$NearIR - CC_refData_DT$RED ) / ( CC_refData_DT$NearIR + CC_refData_DT$RED )

CC_refData_DT$NDRE <- ( CC_refData_DT$NearIR - CC_refData_DT$REDEDGE ) / ( CC_refData_DT$NearIR + CC_refData_DT$REDEDGE )

CC_refData_DT$SR   <- ( CC_refData_DT$NearIR / CC_refData_DT$RED )

CC_refData_DT$GLI  <- ( (CC_refData_DT$GRE - CC_refData_DT$RED) + (CC_refData_DT$GRE - CC_refData_DT$BLU) )/( (2*CC_refData_DT$GRE) + CC_refData_DT$RED + CC_refData_DT$BLU )

CC_refData_DT$RGI  <- (CC_refData_DT$RED / CC_refData_DT$GRE)

CC_refData_DT$ExG  <- ((2*CC_refData_DT$GRE)-(CC_refData_DT$RED+CC_refData_DT$BLU))

CC_refData_DT$meanRGB <- ((CC_refData_DT$RED + CC_refData_DT$GRE + CC_refData_DT$BLU)/3)

CC_refData_DT$RBI <- (CC_refData_DT$RED/CC_refData_DT$BLU)

exportDT_copy <- CC_refData_DT

data.table::setcolorder(exportDT_copy, c(2:6, 8:15, 1, 7)) # rearranging columns

write.csv(exportDT_copy, 
          file = file.path(exportPATH, RefData_CombinedCSVName), 
          row.names = FALSE)

# ====================       COMBINE CSV AND POINT CLOUD for REF DATA (END)     ================


# ----------------------------------------------------------------------------------------------


# ====================       PLOT CLASS SPECTRA AND HISTOGRAMS (START)     =====================



# Calculate average values and standard deviations 

require(magrittr)
# Calculate the average values for each reflectance band and index, for each class
CC_refData_DT_AVG <- CC_refData_DT %>%
  dplyr::group_by(class) %>%
  dplyr::summarize(across(BLU:RBI, mean))

# Reshape data from wide to long format
CC_refData_DT_AVG_PivLong <- CC_refData_DT_AVG %>%
  tidyr::pivot_longer(cols=BLU:RBI, names_to="Reflectance", values_to="Average")

# Change order of the factor levels
CC_refData_DT_AVG_PivLong$Reflectance <- factor(CC_refData_DT_AVG_PivLong$Reflectance, levels=c('BLU','GRE','RED', 'REDEDGE', 'NearIR', 'NDVI', 'NDRE', 'SR', 'GLI', 'RGI', 'ExG', 'meanRGB', 'RBI'))


# Calculate standard deviation for each group
CC_refData_DT_SD <- CC_refData_DT %>%
  dplyr::group_by(class) %>%
  dplyr::summarize(across(BLU:RBI, sd), .groups = "drop")

# Reshape data from wide to long format
CC_refData_DT_SD_PivLong <- CC_refData_DT_SD %>%
  tidyr::pivot_longer(cols=BLU:RBI, names_to="Reflectance", values_to="SD")


# Create a JoinID column to join average and SD calculations for AVG and SD dfs

CC_refData_DT_AVG_PivLong <- CC_refData_DT_AVG_PivLong %>%
  dplyr::mutate(JoinID = dplyr::row_number())

CC_refData_DT_SD_PivLong <- CC_refData_DT_SD_PivLong %>%
  dplyr::mutate(JoinID = dplyr::row_number())


# Merge standard deviation with average data
CC_refData_DT_AVG_PivLong <- CC_refData_DT_AVG_PivLong %>%
  dplyr::left_join(CC_refData_DT_SD_PivLong %>% dplyr::select(JoinID, SD), by = "JoinID", keep = FALSE)

# ---------------------------------------------------------------------------------------------


# ----------------                    CHANGE VARIABLES HERE                   -----------------                       

# PLOT

# FLAGS

# PLOT TYPE SPECTRA (1) or INDICES (2)

plotType <- 1

# Remove (1) or (2) include simple ratio (SR)
noSR <- 1

# ---------------------------------------------------------------------------------------------

# Subset dataframe based on flags 

if (plotType == 1){
  searchBand_EMspectrum <- c('BLU','GRE','RED', 'REDEDGE', 'NearIR')
  subset_Arr <- as.integer()
  for (band in searchBand_EMspectrum){
    indARR <- which(CC_refData_DT_AVG_PivLong$Reflectance == band)
    subset_Arr <- c(subset_Arr, indARR)
  }
  subset_df <- CC_refData_DT_AVG_PivLong[subset_Arr,]
} else if (plotType == 2){
  if (noSR == 1){
    searchBand_Indices <- c('NDVI', 'NDRE', 'GLI', 'ExG', 'meanRGB')
  } else if (noSR == 2) {
    searchBand_Indices <- c('NDVI', 'NDRE', 'SR', 'GLI', 'RGI', 'ExG', 'meanRGB', 'RBI')
  }
  subset_Arr <- as.integer()
  for (band in searchBand_Indices){
    indARR <- which(CC_refData_DT_AVG_PivLong$Reflectance == band)# & (CC_refData_DT_AVG_PivLong$class == "gray" | CC_refData_DT_AVG_PivLong$class == "red"))
    subset_Arr <- c(subset_Arr, indARR)
  }
  subset_df <- CC_refData_DT_AVG_PivLong[subset_Arr,]
} else {
  print("Please set 'plotType' to either 1 or 2")
}


# PLOT
if (plotType == 1){
  ggplot(subset_df, aes(x=Reflectance, y=Average, group=class, color=factor(class))) +
    geom_line(linewidth = 1.2) +
    geom_point() +
    # geom_jitter(data = test_df, aes(x = Reflectance, y = Vals, group = class, color=factor(class)), alpha = 0.2) +
    geom_errorbar(aes(ymin=Average-SD, ymax=Average+SD), width=.2)+
    geom_label_repel(aes(label=round(Average, 3)), 
                     label.size = 0.25, 
                     box.padding = 0.3, 
                     point.padding = 0.5) +
    scale_color_manual(values=c("darkgreen", "gray", "red", "black"),
                       name = "class",
                       labels = c("green", "gray", "red", "shadow")) +
    labs(x="Band", 
         y="Average values", 
         title="Avg values of classes by band")+
    theme_minimal()
} else if (plotType == 2){
  ggplot(subset_df, aes(x=Reflectance, y=Average, group=class, color=factor(class))) +
    geom_line(linewidth = 1.2) +
    geom_point() +
    # geom_jitter(data = test_df, aes(x = Reflectance, y = Vals, group = class, color=factor(class)), alpha = 0.2) +
    geom_errorbar(aes(ymin=Average-SD, ymax=Average+SD), width=.2)+
    geom_label_repel(aes(label=round(Average, 3)), 
                     label.size = 0.25, 
                     box.padding = 0.3, 
                     point.padding = 0.5) +
    scale_color_manual(values=c("darkgreen", "gray", "red", "black"),
                       name = "class",
                       labels = c("green", "gray", "red", "shadow")) +
    labs(x="Indices", 
         y="Average values", 
         title="Avg values of classes by indices")+
    theme_minimal()
}


# PLOT HISTOGRAMS 

# Subset only spectrum columns
CC_refData_DT_spectrum <- CC_refData_DT[,c("BLU", "GRE", "RED", "REDEDGE", "NearIR", "class")]
CC_refData_DT_indices <- CC_refData_DT[, c('NDVI', 'NDRE', 'SR', 'GLI', 'RGI', 'ExG', 'meanRGB', 'RBI', "class")]

# Reshape data into a long format
CC_refData_DT_spectrum_long <- gather(CC_refData_DT_spectrum, key = "Column", value = "Value", -class)
CC_refData_DT_indices_long <- gather(CC_refData_DT_indices, key = "Column", value = "Value", -class)

# Create a facet-grid of frequency histograms
ggplot(CC_refData_DT_spectrum_long, aes(x = Value, fill = factor(class))) + 
  geom_histogram(binwidth = 0.01, alpha = 0.5, position = "identity") +
  facet_wrap(. ~ Column, nrow = 2, ncol = 3, scales = "free") + 
  labs(y = "Frequency") +
  scale_fill_manual(values = c("darkgreen", "gray", "red", "black"),
                    labels = c("green","gray", "red", "shadows"),
                    name = "classes")+
  scale_color_manual(values = c("darkgreen", "gray", "red", "black"),
                     labels = c("green","gray", "red", "shadows"),
                     name = "classes")+
  theme_bw()

ggplot(CC_refData_DT_indices_long, aes(x = Value, fill = factor(class))) + 
  geom_histogram(binwidth = 0.01, alpha = 0.5, position = "identity") +
  stat_density(geom = "line", aes(color = factor(class)), linewidth = 0.5, alpha = 0.8)+
  facet_wrap(. ~ Column, nrow = 3, ncol = 3, scales = "free") + 
  labs(y = "Frequency") +
  scale_fill_manual(values = c("darkgreen", "gray", "red", "black"),
                    labels = c("green","gray", "red", "shadows"),
                    name = "classes")+
  scale_color_manual(values = c("darkgreen", "gray", "red", "black"),
                     labels = c("green","gray", "red", "shadows"),
                     name = "classes")+
  theme_bw()

# ====================       PLOT CLASS SPECTRA AND HISTOGRAMS (END)     =====================