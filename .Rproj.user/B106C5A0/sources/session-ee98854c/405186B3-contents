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

# ---- Load and summarize LysoTracker biological replicate data ----
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

# =========================================
# PART 2: Summarize Fluorescence Intensities
# =========================================
# ---- Calculate mean/SD of fluorescence changes between Control and Tracker staining ----
summary_table <- data %>%
  select(Culture, Replicate, Stain, Mean, Peak) %>% 
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

# ---- Merge fluorescence and staining data ----
merged_data <- left_join(summary_table, trackerdata, by = "Culture") 

# =========================================
# PART 3: Generate Summary Table for Manuscript
# =========================================

# ---- Format data for display ----
table <- merged_data %>%
  mutate(
    Mean_Display_Tracker = paste0(round(Mean_Tracker_Avg, 2), " ± ", round(Mean_Tracker_SD, 2)),
    Mean_Display_Control = paste0(round(Mean_Control_Avg, 2), " ± ", round(Mean_Control_SD, 2)),
    Peak_Display_Tracker = paste0(round(Peak_Tracker_Avg, 2), " ± ", round(Peak_Tracker_SD, 2)),
    Peak_Display_Control = paste0(round(Peak_Control_Avg, 2), " ± ", round(Peak_Control_SD, 2)),
    Change_Mean_Display = paste0(round(Mean_Change_Avg, 2), " ± ", round(Mean_Change_SD, 2)),
    Change_Peak_Display = paste0(round(Peak_Change_Avg, 2), " ± ", round(Peak_Change_SD, 2)),
    DisplayPercent=paste(round(percent, 2), " ± ", round(std, 2))) %>%
  select(Culture, Mean_Display_Tracker, Mean_Display_Control, 
         Peak_Display_Tracker, Peak_Display_Control, Change_Mean_Display, Change_Peak_Display, DisplayPercent) %>%
  gt() %>%
  tab_header(
    title = "Fluorescence Intensity: LysoTracker Stained vs Control (Mean ± SD)") %>%
  cols_label(
    Mean_Display_Tracker = "Stained Mean ± SD",
    Mean_Display_Control = "Control Mean ± SD",
    Peak_Display_Tracker = "Stained Peak ± SD",
    Peak_Display_Control = "Control Peak ± SD",
    Change_Mean_Display = "Stained - Control Mean", 
    Change_Peak_Display = "Stained - Control Peak Fluroesence", 
    DisplayPercent = "Proportion Stained")
table
# ---- Save summary table as image ----
gtsave(table, filename = "Figures/Table2.png")

# =========================================
# PART 4: Generate Fluorescence Change Plot
# =========================================
# ---- Scatter plot of mean fluorescence change vs percent stained ----
meanchange <- ggplot(merged_data, aes(x = Culture, y = Mean_Change_Avg, color=percent)) +
  scale_color_gradientn(colors = c("black", "purple", "yellow3"))+
  geom_point(size = 4) +
  geom_errorbar(aes(ymin = Mean_Change_Avg - Mean_Change_SD, ymax=Mean_Change_Avg + Mean_Change_SD), width=0.7)+
  labs(title = "Mean Fluorescence Intensity Change",
       y = "Log Fluoresence Change\n(Stained - Control)",
       x = "Culture",
       color = "Percent Stained\nLysoTracker") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        text = element_text(size=16) )+
  scale_y_log10()
meanchange

# ---- Generate bar plot to show culture types in the same manner as figure 2 ----
type_bar <- merged_data %>%
  mutate(Culture = factor(Culture, levels = unique(merged_data$Culture)))

resultsorder <- c("P. subcurvata ", "Chaetocerous sp.", "F. cylindrus", "Chaetocerous sp. 02", 'Odontella sp.', 'Chaetocerous sp. 12', 'Chaetocerous sp. 22', 'P. tricornutum', 'O. rostrata', 'G. oceanica', 'G. huxleyi', 'Tetraselmis sp.', 'T. chui', 'Chlamydomonas sp.', 'M. polaris', 'P. tychotreta', 'M. antarctica', 'G. cryophilia')

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
combined_plot <- meanchange / type_plot+ 
  plot_layout(heights = c(1, 0.05)) 
combined_plot

# ---- Save Figure 3 ----
ggsave("Figures/Figure3.tiff", plot = combined_plot, width = 12, height = 7, units = "in", dpi = 300)

