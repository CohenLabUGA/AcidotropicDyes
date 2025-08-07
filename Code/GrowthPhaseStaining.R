# Load in required libraries
library(ggplot2)
library(data.table)
library(dplyr)
library(plyr)
library(readxl)
library(gtools)
library(ggpubr)
library(tidyr)

# Create custom order for cultures to appear in
ordered_names <- c("Chaetocerous sp", "P. subcurvata", "F. cylindrus", "Chaetocerous sp 02", "Chaetocerous sp 12", "Odontella sp", "Chaetocerous sp 22", "O. rostrata", "G. huxleyi", "G. oceanica", "Tetraselmis sp.", "T. chuii", "Chlamydomonas sp.", "M. antarctica", "P. tychotreta", "G. cryophilia")

# Read in dataframe and format for plotting
df <- read_excel("Data/StainingGrowthPhases.xlsx") %>%
  mutate(Av=(Av*100), Std=Std*100)
df$Name <- factor(df$Name, levels = ordered_names)

# Plot
graph <- ggplot(df, aes(x = factor(Type, levels = c("Stationary", "Exponential", "24 hour darkness")), 
                                y = Av, color = Stain)) +
  geom_point(position = position_dodge(width = 0.15), size = 2) +
  geom_errorbar(aes(ymin = Av - Std, ymax = Av + Std),
                width = 0.3,
                position = position_dodge(width = 0.15))+
  labs(y = "Percent of stained cells (%)", x = "Growth Phase") +
  facet_wrap(~ Name) +
  theme_bw() +
  scale_color_manual(values = c("LysoTracker" = "darkgreen", "LysoSensor" = "skyblue")) +
  theme(axis.text.x=element_text(angle=45, vjust=1,hjust=1))
  

# Save plot
ggsave("Figures/SuppFigGrowthPhase.tiff", plot = graph, width = 12, height = 12, units = "in", dpi = 300)

