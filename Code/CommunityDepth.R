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

neslysotracker <- read.csv("Data/20241205_NESLysoTrackerProcessed.csv") %>%
  mutate(Cruise = "New England Shelf")

ccslysotracker <-read.csv("Data/20241203_CCSLysoTrackerProcessed.csv") %>%
  mutate(Cruise = "California Current System") %>%
  dplyr::select(-c(avcrypto, sdcrypto, Light))

### Merge dfs
alllysotracker <- rbind(ccslysotracker, neslysotracker)
alllysotracker$DepthBin <-  cut(alllysotracker$Depth, breaks = seq(0, max(70), by = 10), right = FALSE, 
                        labels = paste(seq(0, max(70) - 10, by = 10), 
                                       seq(10, max(70), by = 10), sep = "-"))

alllysotracker$Station <- factor(alllysotracker$Station, 
                         levels = c("1", "2", "3", "4", "5", 
                                    "6", "7", "8", "9", "10", "X"))  

depth_colors <- c(
  "0-10" = "#B3CDE3",  
  "10-20" = "#73A8D6", 
  "20-30" = "#4F81C7", 
  "30-40" = "#1F4E79",
  "40-50" = "#103654", 
  "50-60" = "#5C5C5C", 
  "60-70" = "#000000")

alllysotracker$DepthBin <- factor(alllysotracker$DepthBin, levels = rev(levels(alllysotracker$DepthBin)))

#### Heat plots ####
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
bac

fig4 <- grid.arrange(synechococcus, bacteria, heterotrophs, nanoeukaryotes)
ggsave("Figures/Figure4.tiff", plot = fig4, width = 15, height = 7, units = "in", dpi = 300)

write.csv(alllysotracker, "Data/AllCruiseLysoTracker.csv")
