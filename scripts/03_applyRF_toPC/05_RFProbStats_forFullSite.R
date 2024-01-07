###########################################################################

### Programmer: Abhinav Shrestha

### Contact information: abhinav.shrestha96@gmail.com

### Purpose(s): 
# * calculate statistics of RF classification probability

### Last update: 12/11/23

###########################################################################

gc()

# =========================================================================

# IMPORT LIBRARIES

# =========================================================================
require(tidyr)
require(dplyr)
require(terra)
require(ggplot2)
require(lidR)
require(sf)
require(data.table)
require(gridExtra)

# =========================================================================


# -------------------------------------------------------------------------

# Initialize PATH, DIR, FILE

# -------------------------------------------------------------------------

dir  = getwd()

dataPATH = file.path(dir, "DATA")

exportPATH = file.path(dir, "EXPORTS")

# -------------------------------------------------------------------------

# Load point cloud file 

pointCloud_fileName1 <- "M1_li2012_RFClassif_maxProb.las"
pointCloud_fileName2 <- "M2_li2012_RFClassif_maxProb.las"
 
pointCloud_fullPath1 <- file.path(exportPATH, pointCloud_fileName1)
pointCloud_fullPath2 <- file.path(exportPATH, pointCloud_fileName2)

pointCloudObj1 <- lidR::readUAVLAS(pointCloud_fullPath1)

RFProb_DT <- data.table(RF_Class = numeric(), 
                        RF_Prob = numeric())

RFProb_DT <- rbindlist(list(RFProb_DT, data.table(RF_Class = pointCloudObj1@data$RF_RBI_NDVI_REDEDGE,
                                             RF_Prob = pointCloudObj1@data$RBI_NDVI_REDEDGE_max_prob)))

rm(pointCloudObj1)

pointCloudObj2 <- lidR::readUAVLAS(pointCloud_fullPath2)


RFProb_DT <- rbindlist(list(RFProb_DT, data.table(RF_Class = pointCloudObj2@data$RF_RBI_NDVI_REDEDGE,
                                                  RF_Prob = pointCloudObj2@data$RBI_NDVI_REDEDGE_max_prob)))

rm(pointCloudObj2)

RFProb_DT$RF_Class <- factor(RFProb_DT$RF_Class)

RFProb_DTgreen <- RFProb_DT[which(RFProb_DT$RF_Class == 1),]
RFProb_DTgray <- RFProb_DT[which(RFProb_DT$RF_Class == 2),]
RFProb_DTred <- RFProb_DT[which(RFProb_DT$RF_Class == 3),]
RFProb_DTshad <- RFProb_DT[which(RFProb_DT$RF_Class == 4),]

histGreen <- ggplot(data = RFProb_DT, aes(x = RF_Prob, fill = RF_Class, after_stat(ncount))) +
              geom_histogram(data = RFProb_DTgreen, position = "identity", binwidth = 0.01) +
              theme_classic() +
              scale_fill_manual(name = "Class",
                                values = c("darkgreen"),
                                labels = c("Green")) +
              labs(title = "Green class",
                   x = "Classification probability",
                   y = "Frequency") +
              scale_x_continuous(expand = c(0, 0)) +
              scale_y_continuous(expand = c(0, 0)) +
              theme(legend.position = "none", 
                    axis.text = element_text(size = 12, color = "black"), 
                    axis.title = element_text(size = 14, color = "black"), 
                    title = element_text(size = 14))


histGray <- ggplot(data = RFProb_DT, aes(x = RF_Prob, fill = RF_Class, after_stat(ncount))) +
  geom_histogram(data = RFProb_DTgray, position = "identity", binwidth = 0.01) +
  theme_classic() +
  scale_fill_manual(name = "Class",
                    values = c("gray"),
                    labels = c("Gray")) +
  labs(title = "Gray class",
       x = "Classification probability",
       y = "Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12, color = "black"), 
        axis.title = element_text(size = 14, color = "black"), 
        title = element_text(size = 14))


histRed <- ggplot(data = RFProb_DT, aes(x = RF_Prob, fill = RF_Class, after_stat(ncount))) +
  geom_histogram(data = RFProb_DTred, position = "identity", binwidth = 0.01) +
  theme_classic() +
  scale_fill_manual(name = "Class",
                    values = c("red"),
                    labels = c("Red")) +
  labs(title = "Red class",
       x = "Classification probability",
       y = "Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12, color = "black"), 
        axis.title = element_text(size = 14, color = "black"), 
        title = element_text(size = 14))


histShad <- ggplot(data = RFProb_DT, aes(x = RF_Prob, fill = RF_Class, after_stat(ncount))) +
            geom_histogram(data = RFProb_DTshad, position = "identity", binwidth = 0.01) +
            theme_classic() +
            scale_fill_manual(name = "Class",
                              values = c("black"),
                              labels = c("Shadow")) +
            labs(title = "Shadow class",
                 x = "Classification probability",
                 y = "Frequency") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(legend.position = "none", 
        axis.text = element_text(size = 12, color = "black"), 
        axis.title = element_text(size = 14, color = "black"), 
        title = element_text(size = 14))



grid.arrange(histAll_overLap, histGreen, histGray, histRed, nrow = 2, ncol = 2)
grid.arrange(histGreen, histGray, histRed, histShad, nrow = 2, ncol = 2)

# --- EXTRA: other variations of plots ---

# histAll_transparent <- ggplot(data = RFProb_DT, aes(x = RF_Prob, fill = RF_Class, after_stat(ncount))) +
#                           geom_histogram(data = RFProb_DTgreen, position = "identity", binwidth = 0.01, fill = "transparent", color = "darkgreen") +
#                           geom_histogram(data = RFProb_DTgray, position = "identity", binwidth = 0.01, fill = "transparent", color = "gray") +
#                           geom_histogram(data = RFProb_DTred, position = "identity", binwidth = 0.01, fill = "transparent", color = "red") +
#                           geom_histogram(data = RFProb_DTshad, position = "identity", binwidth = 0.01, fill = "transparent", color = "black") +
#                           theme_classic() +
#                           scale_color_manual(name = "Class",
#                                              labels = c("Green", "Gray", "Red", "Shadow")) +
#                           labs(x = "Classification probability",
#                                y = "Frequency")



# histAll_overLap <- ggplot(data = RFProb_DT, aes(x = RF_Prob, fill = RF_Class, after_stat(ncount))) +
#                       geom_histogram(data = RFProb_DTgreen, position = "identity", binwidth = 0.01, alpha = 1) +
#                       geom_histogram(data = RFProb_DTgray, position = "identity", binwidth = 0.01, alpha = 0.75) +
#                       geom_histogram(data = RFProb_DTred, position = "identity", binwidth = 0.01, alpha = 0.50) +
#                       geom_histogram(data = RFProb_DTshad, position = "identity", binwidth = 0.01, alpha = 0.25) +
#                       theme_classic() +
#                       scale_fill_manual(name = "Class",
#                         values = c("darkgreen", "gray", "red", "black"),
#                         labels = c("Green", "Gray", "Red", "Shadow")) +
#                       labs(x = "Classification probability",
#                            y = "Frequency") +
#                       scale_x_continuous(expand = c(0, 0)) +
#                       scale_y_continuous(expand = c(0, 0)) + 
#                       theme(legend.title = element_text(size = 18), 
#                             legend.text = element_text(size = 16))

# histAll_dodge <- ggplot(data = RFProb_DT, aes(x = RF_Prob, fill = RF_Class)) +
#                   geom_histogram(position = "dodge", bins = 5) + 
#                   theme_classic() +
#                   scale_fill_manual(name = "Class",
#                                     values = c("darkgreen", "gray", "red", "black"), 
#                                     labels = c("Green", "Gray", "Red", "Shadow")) +
#                   labs(x = "Classification probability", 
#                        y = "Frequency")
# 
# 
# histNoGreen_dodge <- ggplot(data = RFProb_DT_noGreen, aes(x = RF_Prob, fill = RF_Class)) +
#                       geom_histogram(position = "dodge", bins = 5) + 
#                       theme_classic() +
#                       scale_fill_manual(name = "Class",
#                                         values = c("gray", "red", "black"), 
#                                         labels = c("Gray", "Red", "Shadow")) +
#                       labs(x = "Classification probability", 
#                            y = "Frequency")
# 
# 
# histNoGreenNoShadow_dodge <- ggplot(data = RFProb_DT_noGreen_noShadow, aes(x = RF_Prob, fill = RF_Class)) +
#                               geom_histogram(position = "dodge", bins = 5) + 
#                               theme_classic() +
#                               scale_fill_manual(name = "Class",
#                                                 values = c("gray", "red"), 
#                                                 labels = c("Gray", "Red")) +
#                               labs(x = "Classification probability", 
#                                    y = "Frequency")


# grid.arrange(histAll_overLap, histAll_dodge, histNoGreen_dodge, histNoGreenNoShadow_dodge, nrow = 2, ncol = 2)
# grid.arrange(histAll_dodge, histNoGreenNoShadow_dodge, nrow = 2, ncol = 1)

# boxPlot <- ggplot(data = RFProb_DT, aes(x = RF_Class, y = RF_Prob)) +
#   geom_boxplot(color = "black") +
#   scale_fill_manual(name = "Class",
#                     values = c("darkgreen", "gray", "red", "black"),
#                     labels = c("Green", "Gray", "Red", "Shadow")) +
#   labs(x = "Class",
#        y = "Probability") +
#   theme_classic() +
#   scale_x_discrete(labels=c("1" = "Green", "2" = "Gray","3" = "Red", "4" = "Shadow"))
# 
# 
# ggplot(data = RFProb_DT, aes(fill = RF_Class)) +
#   scale_fill_manual(name = "Class",
#                     values = c("darkgreen", "gray", "red", "black"),
#                     labels = c("Green", "Gray", "Red", "Shadow"))