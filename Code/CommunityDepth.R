# ---- Load Required Libraries ----
# These packages are used for data reading, manipulation, and plotting.

library(gtools)
library(grid)
library(readxl)
library(arsenal)
library(ggpmisc)
library(gridExtra)
library(caret)
library(tidyr)
library(stringr)
library(dplyr)
library(RColorBrewer)

# ---- Read in the preprocessed LysoTracker summary data from each cruise ----

# Read in processed summary for New England Shelf cruise
neslysotracker <- read.csv("Data/20241205_NESLysoTrackerProcessed.csv") %>%
  mutate(Cruise = "North East Shelf")

# Read in processed summary for California Current System cruise

ccslysotracker <-read.csv("Data/20241203_CCSLysoTrackerProcessed.csv") %>%
  mutate(Cruise = "California Current System") %>%
  dplyr::select(-c(avcrypto, sdcrypto, Light))# Drop unshared/unneeded columns for merging

# ---- Combine Datasets and Create Depth Bins ----

# Merge both cruises into a single dataframe
alllysotracker <- rbind(ccslysotracker, neslysotracker)

# Bin depths into 10-meter intervals from 0–70 m
alllysotracker$DepthBin <-  cut(alllysotracker$Depth, breaks = seq(0, max(70), by = 10), right = FALSE, 
                        labels = paste(seq(0, max(70) - 10, by = 10), 
                                       seq(10, max(70), by = 10), sep = "-"))

# Set station order for consistent plotting
alllysotracker$Station <- factor(alllysotracker$Station, 
                         levels = c("1", "2", "3", "4", "5", 
                                    "6", "7", "8", "9", "10", "X"))  

# Reverse depth bin levels so shallow is at the top of the heatmaps
alllysotracker$DepthBin <- factor(alllysotracker$DepthBin, levels = rev(levels(alllysotracker$DepthBin)))

# ---- Generate Plots for Key Populations ----

nanoeukaryotes <- ggplot(alllysotracker, aes(x = Station, y = DepthBin, fill = avnano)) +
  geom_tile(color = "white") +
  facet_wrap(~Cruise, scales = "free_x") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(x = "Station", y = "Depth Bin (m)", fill = expression(paste("Phototrophs ",mL^{'-1'}))) +
  theme_classic() +
  theme(text = element_text(size=16) )+
  ggtitle("d)")

heterotrophs <- ggplot(alllysotracker, aes(x = Station, y = DepthBin, fill = avhetero)) +
  geom_tile(color = "white") +
  facet_wrap(~Cruise, scales = "free_x") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(x = "Station", y = "Depth Bin (m)", fill = expression(paste("Heterotrophs ",mL^{'-1'}))) +
  theme_classic() +
  theme(text = element_text(size=16) )+
  ggtitle("c)")

synechococcus <- ggplot(alllysotracker, aes(x = Station, y = DepthBin, fill = avsyn)) +
  geom_tile(color = "white") +
  facet_wrap(~Cruise, scales = "free_x") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(x = "Station", y = "Depth Bin (m)", fill = expression(paste("Synechococcus ",mL^{'-1'}))) +
  theme_classic() +
  theme(text = element_text(size=16) )+
  ggtitle("a)")

# Read in bacteria dataframe from a separate file and do manipulations as above
bacdf <- read_excel("Data/BacteriaConcentrations.xlsx") 
bacdf$DepthBin <-  cut(bacdf$Depth, breaks = seq(0, max(70), by = 10), right = FALSE, 
                       labels = paste(seq(0, max(70) - 10, by = 10), 
                                      seq(10, max(70), by = 10), sep = "-"))
bacdf$DepthBin <- factor(bacdf$DepthBin, levels = rev(levels(bacdf$DepthBin)))
bacdf$Station <- factor(bacdf$Station, 
                        levels = c("1", "2", "3", "4", "5", 
                                   "6", "7", "8", "9", "10", "X"))  

bacteria <- ggplot(bacdf, aes(x = Station, y = DepthBin, fill = bacteria)) +
  geom_tile(color = "white") +
  facet_wrap(~Cruise, scales = "free_x") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(x = "Station", y = "Depth Bin (m)", fill = expression(paste("Bacteria ",mL^{'-1'}))) +
  theme_classic() +
  theme(text = element_text(size=16) )+
  ggtitle("b)")
bacteria

# ---- Combine Plots into a Composite Figure ----
fig4 <- grid.arrange(synechococcus, bacteria, heterotrophs, nanoeukaryotes)
# Save the full figure at high resolution for publication
ggsave("Figures/Figure4.tiff", plot = fig4, width = 15, height = 7, units = "in", dpi = 300)

# ---- Save Merged Dataset for Reuse ----
savedlysotracker <- alllysotracker %>%
  group_by(Cruise, Station) %>%
  dplyr::arrange(Depth, .by_group = TRUE) %>%
  mutate(
    DepthRank = row_number(),
    DepthCode = case_when(
      DepthRank == 1 ~ "Surface",
      DepthRank == 2 ~ "DCM",
      TRUE ~ "Deep")) %>%
  ungroup() %>%
  select(!DepthRank)
write.csv(savedlysotracker, "Data/AllCruiseLysoTracker.csv")
