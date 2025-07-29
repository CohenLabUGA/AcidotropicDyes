# ---- Load Required Libraries ----
# These packages cover data wrangling, visualization, and plot export
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

# ========================================
# PART 1: Define Culture Order and Colors
# ========================================
# ---- Order of cultures for x-axis display ----
resultsorder <- c("P. subcurvata ", "Chaetocerous sp.", "F. cylindrus", "Chaetocerous sp. 02", 'Odontella sp.', 'Chaetocerous sp. 12', 'Chaetocerous sp. 22', 'P. tricornutum', 'O. rostrata', 'G. oceanica', 'G. huxleyi', 'Tetraselmis sp.', 'T. chui', 'Chlamydomonas sp.', 'M. polaris', 'P. tychotreta', 'M. antarctica', 'G. cryophilia')

# ---- Assign consistent colors by phytoplankton group ----
color <- c("Diatom"= "#CAB2D6", 
           "Coccolithophore"="#33A02C",
           "Chlorophyte"="#A6CEE3",
           "Prasinophyte"="#B15928",
           "Cryptophyte"="#FF7F00")

# ========================================
# PART 2: Load and Summarize Tracker & Sensor Data
# ========================================

# ---- Load LysoTracker data ----
trackerdata <- read_excel("Data/CultureLysoData.xlsx", sheet="LysoTracker") %>%
  group_by(Name, FullNames, Place, Metabolism, Type) %>%
  dplyr:: summarise(
    mean_Lyso = mean(Lyso, na.rm = TRUE),
    n_bio = n(),
    std=sd(Lyso),
    .groups = "drop") %>%
  mutate(percent=mean_Lyso*100, std=std*100)

# ---- Load LysoSensor data ----
sensordata <- read_excel("Data/CultureLysoData.xlsx", sheet="LysoSensor")  %>%
  group_by(Name, FullNames, Place, Metabolism, Type) %>%
  dplyr::summarise(
    mean_Lyso = mean(AvSensor, na.rm = TRUE),
    n_bio = n(),
    std=sd(AvSensor),
    .groups = "drop") %>%
  mutate(percent=mean_Lyso*100, std=std*100) %>%
  mutate(Cytometer = "CytPix")

# ========================================
# PART 3: Create Individual Barplots
# ========================================

# ---- LysoTracker Barplot ----
tracker <- ggplot(trackerdata, aes(x = factor(Name, levels = resultsorder), y = percent)) + 
  geom_bar(stat = "identity", 
    aes(fill = Type))+
  geom_errorbar(aes(ymin = percent -std, ymax = percent + std), 
                width = 0.2) +
  theme_classic() +
  labs(x = "", y = "Proportion Stained LysoTracker", fill='Group') +
  ggtitle("a)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 10, 30, 10),
    text = element_text(size=16), 
    legend.position = "none") +
  scale_fill_manual(values = color)+
  scale_y_continuous(limits = c(-5, 110), breaks = seq(0, 100, 25))
tracker

# ---- LysoSensor Barplot ----
sensor <- ggplot(sensordata, aes(x = factor(Name, levels = resultsorder), y = percent)) + 
  geom_bar(stat = "identity", 
    aes(fill = Type))+
  geom_errorbar(aes(ymin = percent -std, ymax = percent + std), 
                width = 0.2) +
  theme_classic() +
  labs(x = "", y = "Proportion Stained LysoSensor", fill='Group') +
  ggtitle("b)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
    plot.margin = margin(10, 10, 30, 10),
    text = element_text(size=16), 
    legend.position = "bottom") +
  scale_fill_manual(values = color)+
  scale_y_continuous(limits = c(-5, 110), breaks = seq(0, 100, 25))
sensor

# ========================================
# PART 4: Combine Plots and Export Figure
# ========================================

# ---- Combine panels vertically ----
final <- grid.arrange(tracker, sensor)
# ---- Save as TIFF for publication ----
ggsave("Figures/Figure2.tiff", plot = final, width = 10, height = 10, units = "in", dpi = 300)

# ========================================
# Put data in a table
# ========================================
# Format dataframes
trackerdata <- read_excel("Data/CultureLysoData.xlsx", sheet="LysoTracker") %>%
  group_by(Name, FullNames, Place, Metabolism, Type, Cytometer) %>%
  dplyr:: summarise(
    mean_Lyso = mean(Lyso, na.rm = TRUE),
    n_bio = n(),
    std=sd(Lyso),
    .groups = "drop") %>%
  mutate(percent=mean_Lyso*100, std=std*100)

trackerdata_mod <- trackerdata %>%
  mutate(Source = paste0("LysoTracker_", Cytometer))

sensordata_mod <- sensordata %>%
  mutate(Cytometer=="CytPix")%>%
  mutate(Source = "LysoSensor")

# Combine lysotracker and lysosensor data
combined_df <- bind_rows(trackerdata_mod, sensordata_mod) %>%
  select(FullNames, Source, percent, std, n_bio) %>%
  mutate(
    value = sprintf("(%d); %.2f ± %.2f", n_bio, percent, std)
  )

# Pivot dataframe wider 
wide_df <- combined_df %>%
  select(FullNames, Source, value) %>%
  pivot_wider(names_from = Source, values_from = value)

# Format table
table <- wide_df %>%
  arrange(FullNames) %>%
  gt() %>%
  cols_label(
    FullNames = "Culture Name",
    `LysoTracker_CytPix` = "LysoTracker (CytPix)",
    `LysoTracker_Guava` = "LysoTracker (Guava)",
    LysoSensor = "LysoSensor (CytPix)"
  ) %>%
  tab_header(title = "Staining Summary ((n); Mean ± StDev)")
table

# Save table
gtsave(table, "Figures/Table2.png")

