# ---- Load Required Libraries ----
# These packages are used for data cleaning, summarizing, visualization, and table export.
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(gt)
library(RColorBrewer)
library(patchwork)

# ===================================
# PART 1: Fluorescence Data Wrangling
# ===================================

# ---- Load raw fluorescence data ----
# Sheet contains mean and peak fluorescence for individual replicates
data <- read_excel("Data/CultureLysoData.xlsx", sheet="LysoTrackerFluoresence") %>%
  filter(Stain %in% c("Control", "Tracker"))

sensordata <- read_excel("Data/CultureLysoData.xlsx", sheet="LysoTrackerFluoresence") %>%
  filter(Stain %in% c("Control", "Sensor"))

# ---- Load and summarize acidotropic dye staining data ----
# Summarize percent of stained cells and biological replicate count for each culture data load in and calculate av and std of biological replicates
trackerdata <- read_excel("Data/CultureLysoData.xlsx", sheet="LysoTracker") %>%
  group_by(Name, Place, Metabolism, Type) %>%
  dplyr:: summarise(
    mean_Lyso = mean(Lyso, na.rm = TRUE),
    n_bio = n(),
    std=sd(Lyso),
    .groups = "drop") %>%
  mutate(percent=mean_Lyso*100, std=std*100)
trackerdata$Culture <- trackerdata$Name

sensorstaineddata <- read_excel("Data/CultureLysoData.xlsx", sheet="LysoSensor") %>%
  group_by(Name, Place, Metabolism, Type) %>%
  dplyr:: summarise(
    mean_Lyso = mean(AvSensor, na.rm = TRUE),
    n_bio = n(),
    std=sd(AvSensor),
    .groups = "drop") %>%
  mutate(percent=mean_Lyso*100, std=std*100)
sensorstaineddata$Culture <- sensorstaineddata$Name

# =========================================
# PART 2: Summarize Fluorescence Intensities
# =========================================
# ---- Calculate mean/SD of fluorescence changes between Control and Tracker staining ----
summary_table <- data %>%
  dplyr::select(Culture, Replicate, Stain, Mean, Peak) %>% 
  pivot_wider(names_from = Stain, values_from = c(Mean, Peak)) %>% 
  drop_na(Mean_Tracker, Mean_Control) %>% 
  mutate(
    Mean_Change = Mean_Tracker - Mean_Control,
    Peak_Change = Peak_Tracker - Peak_Control) %>%
  group_by(Culture) %>%
  dplyr::summarise(
    Mean_Tracker_Avg = mean(Mean_Tracker, na.rm = TRUE),
    Mean_Tracker_SD  = sd(Mean_Tracker, na.rm = TRUE),
    Mean_Control_Avg = mean(Mean_Control, na.rm = TRUE),
    Mean_Control_SD  = sd(Mean_Control, na.rm = TRUE),
    Mean_Change_Avg  = mean(Mean_Change, na.rm = TRUE),
    Mean_Change_SD   = sd(Mean_Change, na.rm = TRUE),
    Peak_Tracker_Avg = mean(Peak_Tracker, na.rm = TRUE),
    Peak_Tracker_SD  = sd(Peak_Tracker, na.rm = TRUE),
    Peak_Control_Avg = mean(Peak_Control, na.rm = TRUE),
    Peak_Control_SD  = sd(Peak_Control, na.rm = TRUE),
    Peak_Change_Avg  = mean(Peak_Change, na.rm = TRUE),
    Peak_Change_SD   = sd(Peak_Change, na.rm = TRUE)) 

summarysensor <- sensordata %>%
  dplyr::select(Culture, Replicate, Stain, VioletMean) %>% 
  pivot_wider(names_from = Stain, values_from = c(VioletMean)) %>% 
  drop_na(Sensor, Control) %>% 
  mutate(Change = Sensor - Control) %>%
  group_by(Culture) %>%
  dplyr::summarise(Mean_Sensor = mean(Sensor),
                   Mean_Control = mean(Control),
                   SD_Control = sd(Control),
                   SD_Sensor = sd(Sensor), 
                   Mean_Sensor_Change=mean(Change), 
                   SD_Sensor_Change=sd(Change))
meanfsc <- data %>%
  group_by(Culture) %>%
  dplyr::summarise(MeanFSC= mean(FSCMean))

# ---- Merge fluorescence and staining data and calculate normalized green fluoresence (by FSC) ----
merged_data_some <- left_join(summary_table, trackerdata, by = "Culture") 
merged_data <- left_join(merged_data_some, meanfsc, by = "Culture") %>% 
  mutate(normalizedgreen=Mean_Tracker_Avg / MeanFSC) %>%
  mutate(normalizedgreensd=Mean_Tracker_SD / MeanFSC)

merged_data_some <- left_join(summarysensor, sensorstaineddata, by = "Culture") 
merged_sensor <- left_join(merged_data_some, meanfsc, by = "Culture") %>% 
  mutate(normalizedfsc=(Mean_Sensor / MeanFSC)) %>%
  mutate(normalizedfscsd=(SD_Sensor / MeanFSC)) 
# =========================================
# PART 3: Generate Summary Table for Manuscript
# =========================================

# ---- Format data for display ----
trackertable <- summary_table %>%
  mutate(
    LysoT_Stained_Mean   = paste0(round(Mean_Tracker_Avg, 2), " ± ", round(Mean_Tracker_SD, 2)),
    LysoT_Control_Mean   = paste0(round(Mean_Control_Avg, 2), " ± ", round(Mean_Control_SD, 2)),
    Tracker_Minus_Control = paste0(round(Mean_Change_Avg, 2), " ± ", round(Mean_Change_SD, 2))) %>%
  dplyr::select(Culture, LysoT_Stained_Mean, LysoT_Control_Mean, Tracker_Minus_Control)

sensortable <- summarysensor %>%
  left_join(meanfsc, by="Culture") %>%
  mutate(
    LysoS_Stained_Mean   = paste0(round(Mean_Sensor, 2), " ± ", round(SD_Sensor, 2)),
    LysoS_Control_Mean   = paste0(round(Mean_Control, 2), " ± ", round(SD_Control, 2)),
    Sensor_Minus_Control = paste0(round(Mean_Sensor_Change, 2), " ± ", round(SD_Sensor_Change, 2))) %>%
  dplyr::select(Culture, LysoS_Stained_Mean, LysoS_Control_Mean, Sensor_Minus_Control, MeanFSC)

# --- Merge Tracker + Sensor + Control ---
supptablefluorescence <- trackertable %>%
  left_join(sensortable, by="Culture") %>%
  dplyr::select(
    Culture,
    LysoT_Stained_Mean, LysoT_Control_Mean, Tracker_Minus_Control,
    LysoS_Stained_Mean, LysoS_Control_Mean, Sensor_Minus_Control,
    MeanFSC
  ) %>%
  gt() %>%
  tab_header(title = "Mean Fluorescence (Mean ± SD)") %>%
  cols_label(
    LysoT_Stained_Mean   = "LysoT Stained Mean",
    LysoT_Control_Mean   = "LysoT Control Mean",
    Tracker_Minus_Control = "Tracker – Control",
    LysoS_Stained_Mean   = "LysoS Stained Mean",
    LysoS_Control_Mean   = "LysoS Control Mean",
    Sensor_Minus_Control  = "Sensor – Control",
    MeanFSC              = "Mean FSC"
  )
supptablefluorescence
# ---- Save summary table as image ----
gtsave(supptablefluorescence, filename = "Figures/SuppTable4.png", vwidth = 1800, vheight = 3200, zoom = 3)

# =========================================
# PART 4: Generate Fluorescence Change Plot
# =========================================
# ---- Scatter plot of mean fluorescence change vs percent stained ----
resultsorder <- c("P. subcurvata ", "Chaetocerous sp.", "F. cylindrus", "Chaetocerous sp. 02", 'Odontella sp.', 'Chaetocerous sp. 12', 'Chaetocerous sp. 22', 'P. tricornutum', 'O. rostrata', 'G. oceanica', 'G. huxleyi', 'Tetraselmis sp.', 'T. chui', 'Chlamydomonas sp.', 'M. polaris', 'P. tychotreta', 'M. antarctica', 'G. cryophilia')


normalizedfsctracker <- ggplot(merged_data, aes(x = factor(Culture, levels=resultsorder), y = (normalizedgreen+.001), color=percent)) +
  scale_color_gradientn(colors = c("black", "purple", "yellow3"))+
  geom_point(size = 4) +
  geom_errorbar(aes(
    ymin = pmax((normalizedgreen+.001) - (normalizedgreensd+.001), 1e-3),   # lower bound can't go below ~0
    ymax = (normalizedgreen+.001) + (normalizedgreensd+.001)
  ), width = 0.7) +
  labs(title = "",
       y = expression("log"[10]*"(Δ Green Fluorescence / FSC)"),
       x = "Culture",
       color = "Percent Stained\nLysoTracker") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        text = element_text(size=14) )+
  ggtitle("a)")+
  scale_y_log10()
normalizedfsctracker

normalizedfscsensor <- ggplot(merged_sensor, aes(x = factor(Culture, levels=resultsorder), y = (normalizedfsc+0.001), color=percent)) +
  scale_color_gradientn(colors = c("black", "purple", "yellow3"))+
  geom_point(size = 4) +
  geom_errorbar(aes(
    ymin =(normalizedfsc+0.001) - (normalizedfscsd+0.001),
    ymax = (normalizedfsc+0.001) + (normalizedfscsd+0.001)
  ), width = 0.7)+
  labs(title = "",
       y = expression("log"[10]*"(Δ Blue Fluorescence / FSC)"),
       x = "Culture",
       color = "Percent Stained\nLysoSensor") +
  ggtitle("b)")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        text = element_text(size=14) ) +
  scale_y_log10() + 
  ylim(0,0.105)
normalizedfscsensor

# ---- Generate bar plot to show culture types in the same manner as figure 2 ----
type_bar <- merged_data %>%
  mutate(Culture = factor(Culture, levels = unique(merged_data$Culture)))


color <- c("Diatom"= "#CAB2D6", 
           "Coccolithophore"="#33A02C",
           "Chlorophyte"="#A6CEE3",
           "Prasinophyte"="#B15928",
           "Cryptophyte"="#FF7F00")

type_plot <- ggplot(type_bar, aes(x = factor(Culture, levels = resultsorder), y = 1, fill = Type)) +
  geom_tile() +
  scale_fill_manual(values = color)+
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.margin = margin(0, 20, 0, 20), text = element_text(size=16))

# ---- Combine scatter plot and bar plot of culture type----
combined_plot <- normalizedfsctracker/normalizedfscsensor / type_plot+ 
  plot_layout(heights = c(1, 1,0.05)) 
combined_plot

# ---- Save Figure 3 ----
ggsave("Figures/Figure3.tiff", plot = combined_plot, width = 10, height = 10, units = "in", dpi = 300)

