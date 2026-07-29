# =============================================================================
# Supplemental Table 4 + Figure 2: Per-Cell Fluorescence from FCS Files
# -----------------------------------------------------------------------------
# Reads raw flow cytometry (.fcs) files per culture, computes FSC-normalized
# and raw LysoTracker/LysoSensor fluorescence with control subtraction, exports
# a summary table, and builds the normalized-fluorescence scatter figure.
#
# NOTE: the fcs files associated with this analysis can be found at
#       https://doi.org/10.5281/zenodo.16813438.
# =============================================================================

library(flowCore)
library(dplyr)
library(purrr)
library(gt)
library(ggplot2)
library(readxl)
library(gridExtra)
library(stringr)
library(patchwork)


# =============================================================================
# PART 1: Locate Culture Folders
# =============================================================================

# ---- Parent directory holding one subfolder per culture ----
parent <- "~/Desktop/FCMdata/ZenodoData/CultureFCSfiles"  # point to downloaded files
culture_dirs <- list.dirs(parent, recursive = FALSE)


# =============================================================================
# PART 2: Per-File and Per-Culture Metric Functions
# =============================================================================

# ---- Compute per-cell metrics from a single FCS file ----
# Returns raw and FSC-normalized green (Tracker) / blue (Sensor) channels.
compute_metrics <- function(fcs_path) {
  f <- read.FCS(fcs_path)
  e <- exprs(f)
  
  list(
    tracker_raw = e[, "BL1-H"],
    sensor_raw  = e[, "VL1-H"],
    tracker     = e[, "BL1-H"] / e[, "FSC-H"],  # green / FSC
    sensor      = e[, "VL1-H"] / e[, "FSC-H"],  # blue / FSC
    fsc         = e[, "FSC-H"]
  )
}

# ---- Process one culture folder into a one-row summary tibble ----
process_culture <- function(culture_dir) {
  
  files <- list.files(culture_dir, pattern = "\\.fcs$", full.names = TRUE)
  
  # ---- Split files by role (stained vs. control) ----
  tracker_files <- grep("Tracker", files, value = TRUE)
  sensor_files  <- grep("Sensor",  files, value = TRUE)
  control_files <- grep("Control", files, value = TRUE)
  
  # ---- Compute per-cell metrics for every file ----
  tracker_vals <- lapply(tracker_files, compute_metrics)
  sensor_vals  <- lapply(sensor_files,  compute_metrics)
  control_vals <- lapply(control_files, compute_metrics)
  
  # ---- Helper: mean of one channel across the cells in each replicate ----
  mean_per_rep <- function(vals_list, channel) {
    if (length(vals_list) == 0) return(numeric(0))
    sapply(vals_list, function(x) mean(x[[channel]], na.rm = TRUE))
  }
  
  # -----------------------------
  # Per-replicate means
  # -----------------------------
  
  # ---- Normalized (channel / FSC) ----
  tracker_reps <- mean_per_rep(tracker_vals, "tracker")
  sensor_reps  <- mean_per_rep(sensor_vals,  "sensor")
  
  control_tracker_reps <- mean_per_rep(control_vals, "tracker")
  control_sensor_reps  <- mean_per_rep(control_vals, "sensor")
  control_fsc_reps     <- mean_per_rep(control_vals, "fsc")
  
  # ---- Raw (un-normalized) ----
  tracker_raw_reps <- mean_per_rep(tracker_vals, "tracker_raw")
  sensor_raw_reps  <- mean_per_rep(sensor_vals,  "sensor_raw")
  
  control_tracker_raw_reps <- mean_per_rep(control_vals, "tracker_raw")
  control_sensor_raw_reps  <- mean_per_rep(control_vals, "sensor_raw")
  
  # ---- NA-safe mean/SD (guard against empty or single-replicate sets) ----
  safe_mean <- function(x) if (length(x) == 0) NA_real_ else mean(x, na.rm = TRUE)
  safe_sd   <- function(x) if (length(x) <= 1) NA_real_ else sd(x, na.rm = TRUE)
  
  # ---- Means and SDs across replicates ----
  tracker_mean <- safe_mean(tracker_reps)
  tracker_sd   <- safe_sd(tracker_reps)
  
  sensor_mean <- safe_mean(sensor_reps)
  sensor_sd   <- safe_sd(sensor_reps)
  
  control_tracker_mean <- safe_mean(control_tracker_reps)
  control_tracker_sd   <- safe_sd(control_tracker_reps)
  
  control_sensor_mean <- safe_mean(control_sensor_reps)
  control_sensor_sd   <- safe_sd(control_sensor_reps)
  
  control_fsc_mean <- safe_mean(control_fsc_reps)
  control_fsc_sd   <- safe_sd(control_fsc_reps)
  
  tracker_raw_mean <- safe_mean(tracker_raw_reps)
  tracker_raw_sd   <- safe_sd(tracker_raw_reps)
  
  sensor_raw_mean <- safe_mean(sensor_raw_reps)
  sensor_raw_sd   <- safe_sd(sensor_raw_reps)
  
  control_tracker_raw_mean <- safe_mean(control_tracker_raw_reps)
  control_tracker_raw_sd   <- safe_sd(control_tracker_raw_reps)
  
  control_sensor_raw_mean <- safe_mean(control_sensor_raw_reps)
  control_sensor_raw_sd   <- safe_sd(control_sensor_raw_reps)
  
  # ---- Background subtraction (stained replicate minus mean control) ----
  tracker_minus_control_reps <- tracker_reps - control_tracker_mean
  sensor_minus_control_reps  <- sensor_reps  - control_sensor_mean
  
  tracker_minus_control_reps_raw <- tracker_raw_reps - control_tracker_raw_mean
  sensor_minus_control_reps_raw  <- sensor_raw_reps  - control_sensor_raw_mean
  
  # ---- Assemble the one-row summary for this culture ----
  tibble(
    Culture      = basename(culture_dir),
    Tracker_Mean = tracker_mean,
    Tracker_SD   = tracker_sd,
    Sensor_Mean  = sensor_mean,
    Sensor_SD    = sensor_sd,
    
    Control_Tracker_Mean = control_tracker_mean,
    Control_Tracker_SD   = control_tracker_sd,
    Control_Sensor_Mean  = control_sensor_mean,
    Control_Sensor_SD    = control_sensor_sd,
    Control_FSC_Mean     = control_fsc_mean,
    Control_FSC_SD       = control_fsc_sd,
    
    Tracker_minus_Control    = safe_mean(tracker_minus_control_reps),
    Tracker_minus_Control_SD = safe_sd(tracker_minus_control_reps),
    Sensor_minus_Control     = safe_mean(sensor_minus_control_reps),
    Sensor_minus_Control_SD  = safe_sd(sensor_minus_control_reps),
    
    Tracker_Raw_Mean = tracker_raw_mean,
    Tracker_Raw_SD   = tracker_raw_sd,
    Sensor_Raw_Mean  = sensor_raw_mean,
    Sensor_Raw_SD    = sensor_raw_sd,
    
    Control_Tracker_Raw_Mean = control_tracker_raw_mean,
    Control_Tracker_Raw_SD   = control_tracker_raw_sd,
    Control_Sensor_Raw_Mean  = control_sensor_raw_mean,
    Control_Sensor_Raw_SD    = control_sensor_raw_sd,
    
    Raw_Tracker_minus_Control    = safe_mean(tracker_minus_control_reps_raw),
    Raw_Tracker_minus_Control_SD = safe_sd(tracker_minus_control_reps_raw),
    Raw_Sensor_minus_Control     = safe_mean(sensor_minus_control_reps_raw),
    Raw_Sensor_minus_Control_SD  = safe_sd(sensor_minus_control_reps_raw)
  )
}


# =============================================================================
# PART 3: Build the Summary Table Across All Cultures
# =============================================================================

# ---- First pass over all culture folders ----
summary_table <- map_df(culture_dirs, process_culture)

# ---- Re-run and strip leading numeric/underscore prefixes from folder names ----
culture_dirs <- list.dirs(parent, recursive = FALSE)
summary_table <- map_df(culture_dirs, process_culture) %>%
  mutate(Culture = sub("^[0-9_]+", "", Culture))

# ---- Join short folder names to descriptive display names ----
summary_table <- read_excel("~/Desktop/FCMdata/ZenodoData/MapNames.xlsx") %>%
  left_join(summary_table, by = c("shortname" = "Culture"))


# =============================================================================
# PART 4: Format and Export Supplemental Table 4
# =============================================================================

# ---- Build "mean ± SD" strings (rounded) and render the gt table ----
table_df <- summary_table %>%
  mutate(
    LysoT_Stained_Mean    = paste0(round(Tracker_Raw_Mean, -2), " ± ", round(Tracker_Raw_SD, -2)),
    LysoT_Control_Mean    = paste0(round(Control_Tracker_Raw_Mean, -2), " ± ", round(Control_Tracker_Raw_SD, -2)),
    Tracker_Minus_Control = paste0(round(Raw_Tracker_minus_Control, -2), " ± ", round(Raw_Tracker_minus_Control_SD, -2)),
    LysoS_Stained_Mean    = paste0(round(Sensor_Raw_Mean, -2), " ± ", round(Sensor_Raw_SD, -2)),
    LysoS_Control_Mean    = paste0(round(Control_Sensor_Raw_Mean, -2), " ± ", round(Control_Sensor_Raw_SD, -2)),
    Sensor_Minus_Control  = paste0(round(Raw_Sensor_minus_Control, -2), " ± ", round(Raw_Sensor_minus_Control_SD, -2)),
    MeanFSC               = paste0(round(Control_FSC_Mean, -3), " ± ", round(Control_FSC_SD, -3))
  ) %>%
  dplyr::select(
    realname, LysoT_Stained_Mean, LysoT_Control_Mean, Tracker_Minus_Control,
    LysoS_Stained_Mean, LysoS_Control_Mean, Sensor_Minus_Control, MeanFSC
  ) %>%
  gt() %>%
  tab_header(title = "Mean Fluorescence (Mean ± SD)") %>%
  cols_label(
    realname              = "Culture",
    LysoT_Stained_Mean    = "LysoT Stained",
    LysoT_Control_Mean    = "LysoT Control",
    Tracker_Minus_Control = "LysoT – Control",
    LysoS_Stained_Mean    = "LysoS Stained",
    LysoS_Control_Mean    = "LysoS Control",
    Sensor_Minus_Control  = "LysoS – Control",
    MeanFSC               = "Mean FSC"
  )
table_df

gtsave(table_df, filename = "Figures/SuppTable4.png", vwidth = 1800, vheight = 3200, zoom = 3)


# =============================================================================
# PART 5: Prepare Staining Data for Plotting
# =============================================================================

# ---- Fixed left-to-right culture order for all x-axes ----
resultsorder <- c(
  "P. subcurvata", "C. neogracile", "F. cylindrus", "Chaetoceros sp. 02",
  "Odontella sp.", "Chaetoceros sp. 12", "Chaetoceros sp. 22", "P. tricornutum",
  "O. rostrata", "Chlamydomonas sp.", "M. polaris", "M. antarctica",
  "G. oceanica", "G. huxleyi", "Tetraselmis sp.", "T. chui", "P. tychotreta",
  "G. cryophila", "Ochromonas sp. 1393", "Ochromonas sp. 2951", "P. micans",
  "A. sanguinea"
)

# ---- Percent-stained LysoTracker per culture ----
trackerdata <- read_excel("Data/CultureLysoData.xlsx") %>%
  group_by(Name) %>%
  dplyr::summarise(
    mean_Lyso = mean(Tracker, na.rm = TRUE),
    n_bio     = n(),
    std       = sd(Tracker),
    .groups   = "drop"
  ) %>%
  mutate(Tracker = mean_Lyso * 100, sdTracker = std * 100)
trackerdata$Culture <- trackerdata$Name

# ---- Percent-stained LysoSensor per culture ----
sensorstaineddata <- read_excel("Data/CultureLysoData.xlsx") %>%
  group_by(Name) %>%
  dplyr::summarise(
    mean_Lyso = mean(Sensor, na.rm = TRUE),
    n_bio     = n(),
    std       = sd(Sensor),
    .groups   = "drop"
  ) %>%
  mutate(Sensor = mean_Lyso * 100, SdSensor = std * 100)
sensorstaineddata$Culture <- sensorstaineddata$Name

# ---- Merge Tracker + Sensor percent-stained into one frame ----
stainingdata <- trackerdata %>%
  left_join(sensorstaineddata, by = "Culture") %>%
  select(Culture, Tracker, sdTracker, Sensor, SdSensor)

# ---- Strip leading/trailing whitespace (incl. non-breaking spaces) ----
stainingdata$Culture <- str_replace_all(stainingdata$Culture, "^[\\s\\u00A0]+|[\\s\\u00A0]+$", "")

# ---- Attach percent-stained values to the FCS fluorescence summary ----
plotdf <- summary_table %>%
  left_join(stainingdata, by = c("realname" = "Culture"))


# =============================================================================
# PART 6: Normalized Fluorescence Scatter Plots
# =============================================================================

# ---- LysoTracker: log10 background-subtracted green/FSC, colored by % stained (panel a) ----
# The +.001 offset keeps values positive for the log scale.
normalizedfsctracker <- ggplot(plotdf, aes(x = factor(realname, levels = resultsorder), y = (Tracker_minus_Control + .001), color = Tracker)) +
  scale_color_gradientn(colors = c("black", "purple", "yellow3")) +
  geom_point(size = 4) +
  geom_errorbar(aes(
    ymin = pmax((Tracker_minus_Control + .001) - (Tracker_minus_Control_SD + .001), 1e-3),  # clamp lower bound near 0
    ymax = (Tracker_minus_Control + .001) + (Tracker_minus_Control_SD + .001)
  ), width = 0.7) +
  labs(
    title = "",
    y     = expression("log"[10] * "(Δ Green Fluorescence / FSC)"),
    x     = "",
    color = "Percent Stained\nLysoTracker"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text        = element_text(size = 14)
  ) +
  ggtitle("a)") +
  scale_y_log10()
normalizedfsctracker

# ---- LysoSensor: same construction with its own positive offset (panel b) ----
val <- 1.2e-03
normalizedfscsensor <- ggplot(plotdf, aes(x = factor(realname, levels = resultsorder), y = (Sensor_minus_Control + val), color = Sensor)) +
  scale_color_gradientn(colors = c("black", "purple", "yellow3")) +
  geom_point(size = 4) +
  geom_errorbar(aes(
    ymin = pmax((Sensor_minus_Control + val) - (Sensor_minus_Control_SD + val), 1e-3),  # clamp lower bound near 0
    ymax = (Sensor_minus_Control + val) + (Sensor_minus_Control_SD + val)
  ), width = 0.7) +
  labs(
    title = "",
    y     = expression("log"[10] * "(Δ Blue Fluorescence / FSC)"),
    x     = "",
    color = "Percent Stained\nLysoSensor"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text        = element_text(size = 14)
  ) +
  ggtitle("b)") +
  scale_y_log10()
normalizedfscsensor

# ---- Quick side-by-side preview ----
grid.arrange(normalizedfsctracker, normalizedfscsensor)


# =============================================================================
# PART 7: Class Color Strip and Final Figure Assembly (Figure 2)
# =============================================================================

# ---- Culture -> taxonomic class lookup for the color strip ----
type_bar <- read_excel("Data/CultureLysoData.xlsx") %>%
  select(Name, Type)

# ---- Consistent fill colors keyed by phytoplankton class ----
color <- c(
  "Bacillariophyceae"    = "#CAB2D6",
  "Prymnesiophyceae"     = "#33A02C",
  "Chlorophyceae"        = "#A6CEE3",
  "Chlorodendrophyceae"  = "#1F78B4",
  "Mamiellophyceae"      = "#B15928",
  "Cryptophyceae"        = "#FF7F00",
  "Pyramimonadophyceae"  = "#B2DF8A",
  "Dinophyceae"          = "#E31A1C",
  "Chrysophyceae"        = "#FDBF6F"
)

# ---- Thin colored tile strip encoding each culture's class ----
type_plot <- ggplot(type_bar, aes(x = factor(Name, levels = resultsorder), y = 1, fill = Type)) +
  geom_tile() +
  scale_fill_manual(values = color) +
  theme_void() +
  labs(fill = "Class") +
  theme(
    legend.position = "bottom",
    plot.margin     = margin(0, 20, 0, 20),
    text            = element_text(size = 16)
  )

# ---- Stack both scatter plots over the class strip and export ----
combined_plot <- normalizedfsctracker / normalizedfscsensor / type_plot +
  plot_layout(heights = c(1, 1, 0.05))
combined_plot

ggsave("Figures/Figure2.tiff", plot = combined_plot, width = 10, height = 10, units = "in", dpi = 300)