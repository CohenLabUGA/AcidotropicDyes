# ---- Load required libraries ----
library(marmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(dplyr)
library(ggrepel)
library(ggpubr)
library(readxl)
# ========================================
# PART 1: Load Station Metadata
# ========================================

nes <- read_excel("Data/StationData.xlsx", sheet="NES")
ccs <- read_excel("Data/StationData.xlsx", sheet="CCS")

# Load global coastline shapefile
country <- ne_countries(scale = "medium", returnclass = "sf")


# ========================================
# PART 2: CCS Map Plotting
# ========================================

# ---- Download and process bathymetry for CCS region ----
ccsbathy <-getNOAA.bathy(lon1=-127, lon2=-110, lat1=35, lat2=46, resolution = 1)

bat_xyz <- as.xyz(ccsbathy)
ccs$Long_deg <- as.numeric(ccs$Long_deg)

# ---- Build CCS map ----
ccsplot <- ggplot() + 
  geom_sf(data = country) +
  geom_tile(data = bat_xyz, aes(x = V1, y = V2, fill = -V3)) +
  geom_sf(data = country) +
  coord_sf(xlim = c(-127, -119), 
           ylim = c(36, 45)) +
  labs(x = "Longitude", y = "Latitude", fill = "Depth (m)") +
  theme_minimal() +
  geom_point(data=ccs, aes(x=Long_deg, y=Lat_deg), colour ="white", size=5) + 
  geom_point(data=ccs, aes(x=Long_deg, y=Lat_deg), colour ="black", size=2)+
  ggtitle("b) California Current System") +
  scale_x_continuous(breaks = seq(-127, -119, by = 4))+
  scale_fill_gradientn(
    name = "Depth (m)",
    colours = c("#9ecae1", "#3182bd", "#08519c", "navy"),
    limits = c(-10, max(-bat_xyz$V3, na.rm = TRUE)))+
  geom_label_repel(
    data = ccs,
    aes(x = Long_deg, y = Lat_deg, label = station),
    size = 4,
    fill = "white",         
    color = "black",        
    box.padding = 0.4,
    point.padding = 0,
    min.segment.length = 0,
    force = 1,
    max.overlaps = Inf,
    seed = 42)
ccsplot

# ========================================
# PART 3: NES Map Plotting
# ========================================

# ---- Download and process bathymetry for NES region ----
nesbathy <-getNOAA.bathy(lon1=-69, lon2=-75, lat1=38, lat2=45, resolution = 1)
bat_xyz <- as.xyz(nesbathy)

# ---- Build NES map ----
nesplot <- ggplot() + 
  geom_sf(data = country) +
  geom_tile(data = bat_xyz, aes(x = V1, y = V2, fill = -V3)) +
  geom_sf(data = country) +
  coord_sf(xlim = c(-69, -73), 
           ylim = c(39, 42.3)) +
  labs(x = "Longitude", y = "Latitude", fill = "Depth (m)") +
  theme_minimal() +
  geom_point(data=nes, aes(x=Longitude, y=Latitude), colour ="white", size=5) + 
  geom_point(data=nes, aes(x=Longitude, y=Latitude), colour ="black", size=2) + 
  ggtitle("a) New England Shelf") +
  scale_fill_gradientn(
    name = "Depth (m)",
    colours = c("#9ecae1", "#3182bd", "#08519c", "navy"),
    limits = c(-10, max(-bat_xyz$V3, na.rm = TRUE)))+
  geom_label_repel(
    data = nes,
    aes(x = Longitude, y = Latitude, label = Site),
    size = 4,
    fill = "white",         
    color = "black",        
    box.padding = 0.4,
    point.padding = 0,
    min.segment.length = 0,
    force = 1,
    max.overlaps = Inf,
    seed = 42) 
nesplot


# ========================================
# PART 4: Combine and Save Plot
# ========================================
maps <- ggarrange(nesplot, ccsplot,
  nrow = 1,align = "hv",
  labels = NULL, widths = c(1, 1))
ggsave("Figures/Figure1.tiff", plot = maps, width = 10, height = 7, units = "in", dpi = 300)
