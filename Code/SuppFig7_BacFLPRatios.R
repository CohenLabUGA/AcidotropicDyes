# =============================================================================
# Supplemental Figure 7: Bacteria-to-FLP Ratio by Station
# -----------------------------------------------------------------------------
# Summarizes bacterial concentrations at the shallowest depth per station,
# computes a bacteria:FLP ratio, and plots it by station with per-cruise mean
# reference lines.
# =============================================================================

# ---- Load required libraries ----
library(ggplot2)
library(data.table)
library(dplyr)
library(plyr)
library(readxl)
library(gtools)
library(ggpubr)
library(tidyr)
library(RColorBrewer)
library(ggpmisc)
library(gridExtra)
library(cowplot)


# =============================================================================
# PART 1: Load and Summarize Bacterial Concentrations
# =============================================================================

# ---- Per station/depth/cruise means; scale to a bacteria:FLP ratio (per 1e5) ----
# Then keep only the shallowest depth at each station.
bac <- read_excel("Data/BacteriaConcentrations.xlsx") %>%
  group_by(Station, Depth, Cruise) %>%
  mutate(ratio = bacteria / (10^5)) %>%
  dplyr::summarise(
    avbac   = mean(bacteria),
    sdbac   = sd(bacteria),
    avratio = mean(ratio),
    sdratio = sd(ratio)
  ) %>%
  ungroup() %>%
  group_by(Station, Cruise) %>%
  dplyr::filter(Depth == min(Depth, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Cruise = ifelse(Cruise == "North East Shelf", "Northeast U.S. Shelf", Cruise))

# ---- Fixed station ordering for the x-axis ----
bac$Station <- factor(bac$Station, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "X"))

# ---- Per-cruise mean ratio for reference lines ----
bac_means <- bac %>%
  group_by(Cruise) %>%
  dplyr::summarise(mean_ratio = mean(avratio, na.rm = TRUE))


# =============================================================================
# PART 2: Plot and Export
# =============================================================================

# ---- Bacteria:FLP ratio by station, faceted by cruise, with mean lines ----
ratio <- ggplot(bac, aes(x = Station, y = avratio)) +
  geom_point(size = 3) +
  facet_wrap(~Cruise, scales = "free_x") +
  theme_bw() +
  geom_hline(data = bac_means, aes(yintercept = mean_ratio),
             color = "red", linetype = "dashed") +
  geom_errorbar(aes(ymin = avratio - sdratio, ymax = avratio + sdratio), width = 0.2) +
  labs(y = "Bacteria:FLP Ratio") +
  theme(text = element_text(size = 16))

ggsave("Figures/SuppFig7.tiff", ratio, height = 5.5, width = 8, dpi = 300, unit = "in")
