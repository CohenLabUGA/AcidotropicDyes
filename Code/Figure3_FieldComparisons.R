# =============================================================================
# Figure 3: Station Maps, Field Mixotrophy Metrics, and Photo Panels
# -----------------------------------------------------------------------------
# Builds bathymetric station maps for the NES and CCS cruises (panels a/b),
# LysoTracker staining and FLP ingestion-rate plots (panels c/d), and assembles
# randomized microscopy photo blocks (panels e/f) for later stitching in
# Illustrator.
# =============================================================================

# ---- Load required libraries ----
library(marmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(ggrepel)
library(ggpubr)
library(readxl)
library(cowplot)
library(gtools)
library(readxl)
library(arsenal)
library(ggpmisc)
library(gridExtra)
library(caret)
library(tidyr)
library(stringr)
library(dplyr)
library(grid)
library(tiff)
library(ggforce)
library(broom)

set.seed(123)

# =============================================================================
# PART 1: Load and Prepare Field Data
# =============================================================================

# ---- FLP ingestion-rate data; standardize depth labels and cruise order ----
flpdf <- read_excel("Data/AllFLPData.xlsx") %>%
  mutate(
    Depth  = ifelse(Depth == "SCM", "DCM", Depth),
    Cruise = ifelse(Cruise == "North East Shelf", "Northeast U.S. Shelf", Cruise),
    Cruise = factor(Cruise, levels = c("Northeast U.S. Shelf", "California Current System"))
  )

# ---- LysoTracker data; drop deep samples and convert proportions to percent ----
lysodf <- read.csv("Data/AllCruiseLysoTracker.csv") %>%
  dplyr::filter(!DepthCode == "Deep") %>%
  mutate(
    avpercent = 100 * avpercent,
    sdpercent = 100 * sdpercent
  ) %>%
  mutate(
    Cruise = ifelse(Cruise == "North East Shelf", "Northeast U.S. Shelf", Cruise))

# ---- Insert blank placeholder stations so layouts line up across panels ----
missing_stations <- data.frame(
  Station   = factor(c("3", "5", "8"), levels = levels(lysodf$Station)),
  Cruise    = "Northeast U.S. Shelf",
  DepthCode = NA,
  avpercent = NA,
  sdpercent = NA,
  avmixo    = NA,
  sdmixo    = NA
)

lysodf <- bind_rows(lysodf, missing_stations) %>%
  dplyr::filter(!is.na(Station)) %>%
  mutate(Cruise = factor(Cruise, levels = c("Northeast U.S. Shelf", "California Current System")))

# ---- Enforce identical station ordering across both datasets ----
station_levels <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "X")
lysodf$Station <- factor(lysodf$Station, levels = station_levels)
flpdf$Station  <- factor(flpdf$Station,  levels = station_levels)

# ---- Jitter station x-positions to reduce point overlap ----
lysodf$jittered_x <- jitter(as.numeric(lysodf$Station), amount = 0.25)
flpdf$jittered_x  <- jitter(as.numeric(flpdf$Station),  amount = 0.25)


# =============================================================================
# PART 5: Field Mixotrophy Plots (panels c + d)
# =============================================================================

# ---- Per-cruise means for reference lines ----
flp_means <- flpdf %>%
  group_by(Cruise) %>%
  dplyr::summarise(mean_csgr = mean(avgrazing, na.rm = TRUE))

lyso_means <- lysodf %>%
  group_by(Cruise) %>%
  dplyr::summarise(
    mean_conc    = mean(avmixo,    na.rm = TRUE),
    mean_percent = mean(avpercent, na.rm = TRUE)
  )

# ---- Shared scale/theme components reused across both panels ----
scale_x_station_flp  <- scale_x_continuous(breaks = unique(as.numeric(as.factor(flpdf$Station))),
                                           labels = levels(as.factor(flpdf$Station)))
scale_x_station_lyso <- scale_x_continuous(breaks = 1:11, labels = levels(lysodf$Station))
scale_color_depth    <- scale_color_manual(values = c("DCM" = "gray", "Surface" = "black"), 
                                           labels=c("DCM"="DCM/SCM", "Surface" = "Surface"))
scale_shape_cruise   <- scale_shape_manual(values = c("California Current System" = 15, "Northeast U.S. Shelf" = 16))
scale_shape_method   <- scale_shape_manual(values = c("Microscopy" = 15, "FlowCytometry" = 16))
theme_fig            <- theme_bw() + theme(legend.position = "none", text = element_text(size = 18))

# ---- c) LysoTracker percent-stained pigmented cells, with per-cruise mean line ----
lysopercent <- ggplot(lysodf, aes(x = jittered_x, y = avpercent, color = DepthCode)) +
  geom_point(size = 3) +
  facet_wrap(~Cruise, scales = "free_x") +
  geom_errorbar(aes(ymin = avpercent - sdpercent, ymax = avpercent + sdpercent), width = 0.2) +
  labs(y = "Percent of Stained Pigmented Cells", x = "Station") +
  scale_color_depth + scale_x_station_lyso + theme_fig +
  ggtitle("a) LysoTracker Staining") + theme(text = element_text(size = 16)) +
  geom_hline(data = lyso_means, aes(yintercept = mean_percent), color = "red", linetype = "dashed")

# ---- d) FLP cell-specific ingestion rate (CSGR), with per-cruise mean line ----
csgr <- ggplot(flpdf, aes(x = Station, color = Depth, shape = Method)) +
  geom_point(aes(y = avgrazing), size = 3) +
  scale_shape_method +
  geom_errorbar(aes(ymin = avgrazing - sdgrazing, ymax = avgrazing + sdgrazing), width = 0.8) +
  labs(y = expression(paste("Cell specific grazing rate (Bacteria pigmented ", nanoeukaryote^{"-1 "}, hour^{-1}, ")")), x = "Station") +
  facet_wrap(~Cruise, scales = "free_x") +
  ggtitle("b) FLP Incubations") +
  scale_color_depth +
  theme_bw() + theme(text = element_text(size = 16), legend.position = "none") +
  geom_hline(data = flp_means, aes(yintercept = mean_csgr), color = "red", linetype = "dashed")

# ---- Build a standalone plot solely to extract a shared legend ----
legend <- get_legend(
  ggplot(flpdf, aes(x = Station, y = avpercent, color = Depth, shape = Method)) +
    geom_point(size = 3) +
    theme_bw() +
    scale_color_depth +
    scale_shape_method +
    theme(text = element_text(size = 18))
)

# ---- Combine both panels with the shared legend and export (panels c + d) ----
mixoplot <- grid.arrange(
  arrangeGrob(lysopercent, csgr, ncol = 2, top = ""),
  legend,
  ncol   = 2,
  widths = c(4, 1)
)

ggsave("Figures/Figure3ab.tiff", plot = mixoplot, width = 14, height = 8, units = "in", dpi = 300)


# =============================================================================
# PART 6: Microscopy Photo Panels (panels e + f)
# =============================================================================
# Saved separately for final stitching in Illustrator.
# Step 1: Download photos from Zenodo (https://doi.org/10.5281/zenodo.16813438)
#         into the Data folder, rename the folder to "Photos", and unzip.

# ---- Step 2a: Load CCS photos, convert to grobs, take a random 49-photo subset ----
ccstiffs <- list.files("Data/Photos/CCSphotos", pattern = "\\.tif$", full.names = TRUE)
ccs_grobs <- lapply(ccstiffs, function(f) {
  img <- readTIFF(f, native = TRUE)
  rasterGrob(img, interpolate = TRUE)
})
ccs_grobs <- sample(ccs_grobs, 49)

# ---- Step 2b: Same for NES photos ----
nestiffs <- list.files("Data/Photos/NESphotos", pattern = "\\.tif$", full.names = TRUE)
nes_grobs <- lapply(nestiffs, function(f) {
  img <- readTIFF(f, native = TRUE)
  rasterGrob(img, interpolate = TRUE)
})
nes_grobs <- sample(nes_grobs, 49)

# ---- Step 3: Arrange each set into a 7x7 grid ----
ccs_block <- arrangeGrob(grobs = ccs_grobs, ncol = 7)
nes_block <- arrangeGrob(grobs = nes_grobs, ncol = 7)

# ---- Step 4: Add panel labels and export each block ----
ccs_labeled <- plot_grid(
  ggdraw() + draw_label("d) California Current System", fontface = "bold", size = 18),
  ccs_block, ncol = 1, rel_heights = c(0.1, 1)
)

nes_labeled <- plot_grid(
  ggdraw() + draw_label("c) Northeast U.S. Shelf", fontface = "bold", size = 18),
  nes_block, ncol = 1, rel_heights = c(0.1, 1)
)

ggsave("Figures/Figure3c.tiff", plot = nes_labeled, width = 5.5, height = 6, units = "in", dpi = 300)
ggsave("Figures/Figure3d.tiff", plot = ccs_labeled, width = 5.5, height = 6, units = "in", dpi = 300)
