# =============================================================================
# Supplemental Figure 12: Depth-Binned Population Heatmaps by Cruise
# -----------------------------------------------------------------------------
# Merges LysoTracker cruise summaries, bins samples into 10 m depth intervals,
# and builds Synechococcus/bacteria/heterotroph/phototroph heatmaps across
# stations, then writes out a depth-coded version of the merged dataset.
# =============================================================================

# ---- Load required libraries ----
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
library(scales)


# =============================================================================
# PART 1: Load and Merge Cruise Summaries
# =============================================================================

# ---- NES processed LysoTracker summary ----
neslysotracker <- read.csv("Data/20241205_NESLysoTrackerProcessed.csv") %>%
  mutate(Cruise = "Northeast U.S Shelf")

# ---- CCS processed summary; drop columns not shared with NES before merging ----
ccslysotracker <- read.csv("Data/20241203_CCSLysoTrackerProcessed.csv") %>%
  mutate(Cruise = "California Current System") %>%
  dplyr::select(-c(avcrypto, sdcrypto, Light))

# ---- Combine cruises; blank out one flagged Synechococcus outlier ----
alllysotracker <- rbind(ccslysotracker, neslysotracker) %>%
  mutate(avsyn = if_else(Station == 4 & Depth == 22 & Cruise == "Northeast U.S Shelf",
                         NA_real_, avsyn))


# =============================================================================
# PART 2: Depth Binning and Factor Ordering
# =============================================================================

# ---- Bin depths into 10 m intervals (0–70 m) ----
alllysotracker$DepthBin <- cut(alllysotracker$Depth, breaks = seq(0, max(70), by = 10), right = FALSE,
                               labels = paste(seq(0, max(70) - 10, by = 10),
                                              seq(10, max(70), by = 10), sep = "-"))

# ---- Fixed station order for the x-axis ----
alllysotracker$Station <- factor(alllysotracker$Station,
                                 levels = c("1", "2", "3", "4", "5",
                                            "6", "7", "8", "9", "10", "X"))

# ---- Reverse depth bins so shallow sits at the top of each heatmap ----
alllysotracker$DepthBin <- factor(alllysotracker$DepthBin, levels = rev(levels(alllysotracker$DepthBin)))


# =============================================================================
# PART 3: Population Heatmaps
# =============================================================================

# ---- d) Phototrophic nanoeukaryotes ----
nanoeukaryotes <- ggplot(alllysotracker, aes(x = Station, y = DepthBin, fill = avnano)) +
  geom_tile(color = "white") +
  facet_wrap(~Cruise, scales = "free_x") +
  scale_fill_viridis_c(option = "plasma", na.value = "white", labels = label_scientific(digits = 1)) +
  labs(x = "Station", y = "Depth Bin (m)", fill = expression(paste("Phototrophs ", mL^{'-1'}))) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  ggtitle("d)")

# ---- c) Heterotrophs ----
heterotrophs <- ggplot(alllysotracker, aes(x = Station, y = DepthBin, fill = avhetero)) +
  geom_tile(color = "white") +
  facet_wrap(~Cruise, scales = "free_x") +
  scale_fill_viridis_c(option = "plasma", na.value = "white", labels = label_scientific(digits = 1)) +
  labs(x = "Station", y = "Depth Bin (m)", fill = expression(paste("Heterotrophs ", mL^{'-1'}))) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  ggtitle("c)")

# ---- a) Synechococcus ----
synechococcus <- ggplot(alllysotracker, aes(x = Station, y = DepthBin, fill = avsyn)) +
  geom_tile(color = "white") +
  facet_wrap(~Cruise, scales = "free_x") +
  scale_fill_viridis_c(option = "plasma", na.value = "white", labels = label_scientific(digits = 1)) +
  labs(x = "Station", y = "Depth Bin (m)", fill = expression(paste("Synechococcus ", mL^{'-1'}))) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  ggtitle("a)")

# ---- b) Bacteria (loaded and binned separately, same scheme as above) ----
bacdf <- read_excel("Data/BacteriaConcentrations.xlsx") %>%
  mutate(Cruise = ifelse(Cruise == "North East Shelf", "Northeast U.S. Shelf", Cruise))
bacdf$DepthBin <- cut(bacdf$Depth, breaks = seq(0, max(70), by = 10), right = FALSE,
                      labels = paste(seq(0, max(70) - 10, by = 10),
                                     seq(10, max(70), by = 10), sep = "-"))
bacdf$DepthBin <- factor(bacdf$DepthBin, levels = rev(levels(bacdf$DepthBin)))
bacdf$Station <- factor(bacdf$Station,
                        levels = c("1", "2", "3", "4", "5",
                                   "6", "7", "8", "9", "10", "X"))

bacteria <- ggplot(bacdf, aes(x = Station, y = DepthBin, fill = bacteria)) +
  geom_tile(color = "white") +
  facet_wrap(~Cruise, scales = "free_x") +
  scale_fill_viridis_c(option = "plasma", na.value = "white", labels = label_scientific(digits = 1)) +
  labs(x = "Station", y = "Depth Bin (m)", fill = expression(paste("Bacteria ", mL^{'-1'}))) +
  theme_classic() +
  theme(text = element_text(size = 16)) +
  ggtitle("b)")
bacteria


# =============================================================================
# PART 4: Assemble Composite Figure and Export
# =============================================================================

# ---- Arrange all four heatmaps and save ----
suppfig11 <- grid.arrange(synechococcus, bacteria, heterotrophs, nanoeukaryotes)
ggsave("Figures/SuppFig12.tiff", plot = suppfig11, width = 15, height = 6, units = "in", dpi = 300)


# =============================================================================
# PART 5: Write Out Depth-Coded Merged Dataset
# =============================================================================

# ---- Rank depths within each cruise/station and label Surface/DCM/Deep ----
savedlysotracker <- alllysotracker %>%
  group_by(Cruise, Station) %>%
  dplyr::arrange(Depth, .by_group = TRUE) %>%
  mutate(
    DepthRank = row_number(),
    DepthCode = case_when(
      DepthRank == 1 ~ "Surface",
      DepthRank == 2 ~ "DCM",
      TRUE ~ "Deep"
    )
  ) %>%
  ungroup() %>%
  select(!DepthRank)
write.csv(savedlysotracker, "Data/AllCruiseLysoTracker.csv")