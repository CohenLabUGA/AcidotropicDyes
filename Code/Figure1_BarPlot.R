# =============================================================================
# Figure 1 Production: LysoTracker/LysoSensor Staining + FLP Ingestion Rates
# -----------------------------------------------------------------------------
# Builds per-culture staining barplots (Tracker & Sensor), a companion summary
# table, and a cell-specific ingestion rate (CSGR) barplot, then assembles the
# publication figures.
# =============================================================================

# ---- Load required libraries ----
# Cover data wrangling, visualization, and plot/table export
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
library(gt)
library(patchwork)
library(ggpattern)

# =============================================================================
# PART 1: Define Culture Order and Colors
# =============================================================================

# ---- Fixed left-to-right order of cultures for all x-axes ----
resultsorder <- c(
  "P. subcurvata", "C. neogracile", "F. cylindrus", "Chaetoceros sp. 02",
  "Odontella sp.", "Chaetoceros sp. 12", "Chaetoceros sp. 22", "P. tricornutum",
  "O. rostrata", "Chlamydomonas sp.", "M. polaris", "M. antarctica",
  "G. oceanica", "G. huxleyi", "Tetraselmis sp.", "T. chui", "P. tychotreta",
  "G. cryophila", "Ochromonas sp. 1393", "Ochromonas sp. 2951", "P. micans",
  "A. sanguinea"
)

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


# =============================================================================
# PART 1B: Define Size Classes
# =============================================================================

# ---- Membership of the two non-nano classes; everything else defaults to Nano ----
pico_taxa  <- c("M. polaris")
large_taxa <- c("Chaetoceros sp. 12", "O. rostrata", "Odontella sp.", "A. sanguinea")

# ---- Named lookup vector: culture name -> size class ----
sizeclass <- setNames(
  ifelse(resultsorder %in% pico_taxa,  "Pico",
         ifelse(resultsorder %in% large_taxa, "Large", "Nano")),
  resultsorder
)

# ---- Size-class levels used for all legends and factors ----
size_levels <- c("Pico", "Nano", "Large")

# ---- Human-readable class labels for legends/captions ----
size_labs <- c(
  "Pico"  = "<3 \u00B5m",
  "Nano"  = "3\u201350 \u00B5m",
  "Large" = ">50 \u00B5m"
)

# =============================================================================
# PART 2: Load and Summarize Tracker & Sensor Data
# =============================================================================

# ---- LysoTracker: mean, replicate count, and SD per culture (as percent) ----
trackerdata <- read_excel("Data/CultureLysoData.xlsx") %>%
  group_by(Name, Type, Metabolism) %>%
  dplyr::summarise(
    mean_Lyso = mean(Tracker, na.rm = TRUE),
    n_bio     = n(),
    std       = sd(Tracker),
    .groups   = "drop"
  ) %>%
  mutate(percent = mean_Lyso * 100, std = std * 100)

# ---- LysoSensor: same summary; tag cytometer used ----
sensordata <- read_excel("Data/CultureLysoData.xlsx") %>%
  group_by(Name, Type, Metabolism) %>%
  dplyr::summarise(
    mean_Lyso = mean(Sensor, na.rm = TRUE),
    n_bio     = n(),
    std       = sd(Sensor),
    .groups   = "drop"
  ) %>%
  mutate(percent = mean_Lyso * 100, std = std * 100) %>%
  mutate(Cytometer = "CytPix")


# =============================================================================
# PART 3: Create Individual Barplots
# =============================================================================

# ---- LysoTracker barplot (panel b) ----
trackerdata <- trackerdata %>%
  mutate(SizeClass = factor(sizeclass[Name], levels = size_levels))

sensordata <- sensordata %>%
  mutate(SizeClass = factor(sizeclass[Name], levels = size_levels))

# ---- Nano left unpatterned so the majority of bars stay visually clean ----
size_patterns <- c("Pico" = "crosshatch", "Nano" = "none", "Large" = "stripe")

# ---- LysoTracker barplot with patterns (panel b) ----
tracker <- ggplot(trackerdata, aes(x = factor(Name, levels = resultsorder), y = percent)) +
  geom_col_pattern(
    aes(fill = Type, pattern = SizeClass),
    pattern_fill    = "black",
    pattern_colour  = "black",
    pattern_density = 0.10,
    pattern_spacing = 0.05,
    pattern_angle   = 45,
    pattern_size    = 0.25,
    colour          = "black",
    linewidth       = 0.2
  ) +
  geom_errorbar(aes(ymin = percent - std, ymax = percent + std), width = 0.2) +
  theme_classic() +
  labs(x = "", y = "Percent Stained LysoTracker", fill = "Group") +
  ggtitle("b)") +
  scale_fill_manual(values = color) +
  scale_pattern_manual(values = size_patterns, labels = size_labs, name = "Size class") +
  scale_y_continuous(limits = c(-5, 110), breaks = seq(0, 100, 25)) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    plot.margin     = margin(10, 10, 30, 10),
    text            = element_text(size = 16),
    legend.position = "none"
  ) +
  facet_wrap(~Metabolism, scales = "free_x")
tracker

# ---- LysoSensor barplot with patterns (panel c); carries both legends ----
sensor <- ggplot(sensordata, aes(x = factor(Name, levels = resultsorder), y = percent)) +
  geom_col_pattern(
    aes(fill = Type, pattern = SizeClass),
    pattern_fill    = "black",
    pattern_colour  = "black",
    pattern_density = 0.10,
    pattern_spacing = 0.05,
    pattern_angle   = 45,
    pattern_size    = 0.25,
    colour          = "black",
    linewidth       = 0.2
  ) +
  geom_errorbar(aes(ymin = percent - std, ymax = percent + std), width = 0.2) +
  theme_classic() +
  labs(x = "", y = "Percent Stained LysoSensor", fill = "") +
  ggtitle("c)") +
  scale_fill_manual(values = color, guide = guide_legend(override.aes = list(pattern = "none"))) +
  scale_pattern_manual(
    values = size_patterns,
    labels = size_labs,
    name   = "Size class",
    guide  = guide_legend(override.aes = list(fill = "white", colour = "black"))
  ) +
  scale_y_continuous(limits = c(-5, 110), breaks = seq(0, 100, 25)) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    plot.margin     = margin(10, 10, 30, 10),
    text            = element_text(size = 16),
    legend.position = "bottom",
    legend.box      = "vertical"
  ) +
  facet_wrap(~Metabolism, scales = "free_x")
sensor

# =============================================================================
# PART 5: Build Staining Summary Table (Supplemental Table 2)
# =============================================================================

# ---- Re-summarize Tracker without Metabolism grouping for the table ----
trackerdata <- read_excel("Data/CultureLysoData.xlsx") %>%
  group_by(Name, Type) %>%
  dplyr::summarise(
    mean_Lyso = mean(Tracker, na.rm = TRUE),
    n_bio     = n(),
    std       = sd(Tracker),
    .groups   = "drop"
  ) %>%
  mutate(percent = mean_Lyso * 100, std = std * 100)

# ---- Tag each dataset with its dye source ----
trackerdata_mod <- trackerdata %>%
  mutate(Source = "LysoTracker")

sensordata_mod <- sensordata %>%
  mutate(Cytometer == "CytPix") %>%   # no-op from original; result dropped downstream
  mutate(Source = "LysoSensor")

# ---- Combine dyes and build a formatted "(n); mean ± SD" string ----
combined_df <- bind_rows(trackerdata_mod, sensordata_mod) %>%
  dplyr::select(Name, Source, percent, std, n_bio) %>%
  mutate(value = sprintf("(%d); %.2f ± %.2f", n_bio, percent, std))

# ---- Cell concentration range examined per culture (rounded to nearest 10) ----
cellconcrange <- read_excel("Data/CultureLysoData.xlsx") %>%
  group_by(Name) %>%
  dplyr::summarise(
    cellrange = paste0(
      round(min(CellConcentration, na.rm = TRUE), -1), " – ",
      round(max(CellConcentration, na.rm = TRUE), -1)
    )
  )

# ---- Pivot to one column per dye, then attach the cell range ----
wide_df <- combined_df %>%
  dplyr::select(Name, Source, value) %>%
  pivot_wider(names_from = Source, values_from = value)

table_df <- wide_df %>%
  left_join(cellconcrange, by = "Name")

# ---- Render and export the gt table ----
table <- table_df %>%
  arrange(Name) %>%
  gt() %>%
  cols_label(
    Name         = "Culture Name",
    LysoTracker  = "LysoTracker",
    LysoSensor   = "LysoSensor",
    cellrange    = "Range of Cells Examined"
  ) %>%
  tab_header(title = "Staining Summary ((n); Mean ± StDev)")
table

gtsave(table, filename = "Figures/SuppTable2.png", vwidth = 1500, vheight = 3200, zoom = 3)


# =============================================================================
# PART 6: FLP Ingestion Rates (CSGR)
# =============================================================================

set.seed(123)

# =============================================================================
# PART 6: FLP Ingestion Rates (CSGR)
# -----------------------------------------------------------------------------
# NOTE ON THE BUG: the block that creates AvGrazing_plot / SdGrazing_plot had
# been dropped from df (it now only lives in dfnorm), so grazing_plot was
# mapping y = AvGrazing_plot on a column that does not exist in df.
# That mutate is the DUAL-AXIS RESCALING (dinoflagellates -> shared axis), not
# size normalization, so it is restored below. All MinSize/MaxSize
# normalization, the sizedf join, dfnorm, and the diagnostic plot are removed.
# =============================================================================

set.seed(123)

# ---- Load FLP ingestion rates; clean names, drop NA rows ----
df <- read_excel("Data/CultureFLP.xlsx", sheet = "FLP_toplot") %>%
  mutate(Name = trimws(gsub("\u00A0", " ", Name))) %>%           # strip non-breaking spaces
  filter(!is.na(AvGrazing), !is.na(SdGrazing)) %>%
  mutate(Metabolism = factor(Metabolism, levels = c("Autotroph", "Mixotroph")))

# ---- Dinoflagellates plotted on their own (secondary) axis ----
dino_taxa <- c("A. sanguinea", "P. micans")

# ---- Scale factor: max non-dino mixotroph rate / max dino rate ----
#      Bars for dino taxa are multiplied by this so they occupy the same
#      vertical span as the other mixotrophs; the right-hand axis undoes it.
max_other <- max(df$AvGrazing[!(df$Name %in% dino_taxa) & df$Metabolism == "Mixotroph"], na.rm = TRUE)
max_dino  <- max(df$AvGrazing[df$Name %in% dino_taxa], na.rm = TRUE)
scale_factor <- max_other / max_dino

# ---- Apply scaling to the dinoflagellates; suppress error bars for autotrophs ----
df <- df %>%
  mutate(
    AvGrazing_plot = ifelse(Name %in% dino_taxa, AvGrazing * scale_factor, AvGrazing),
    SdGrazing_plot = ifelse(Name %in% dino_taxa, SdGrazing * scale_factor, SdGrazing),
    SdGrazing_plot = ifelse(Metabolism == "Autotroph", NA, SdGrazing_plot),
    name_plot      = ifelse(Name == "A. sanguinea", "A. sanguinea*", Name)
  )

# ---- Locate the scaled dinoflagellates to shade their region on the plot ----
axis_color <- "gray70"

mixotroph_order <- resultsorder[resultsorder %in% df$Name[df$Metabolism == "Mixotroph"]]
asang_pos       <- which(mixotroph_order == "A. sanguinea")
pmicans_pos     <- which(mixotroph_order == "P. micans")

# ---- Culture order for the ingestion plot (A. sanguinea gets the "*" scaling flag) ----
resultsorder_grazing <- c(
  "P. subcurvata", "C. neogracile", "F. cylindrus", "Chaetoceros sp. 02",
  "Odontella sp.", "Chaetoceros sp. 12", "Chaetoceros sp. 22", "P. tricornutum",
  "O. rostrata", "Chlamydomonas sp.", "M. polaris", "M. antarctica",
  "G. oceanica", "G. huxleyi", "Tetraselmis sp.", "T. chui", "P. tychotreta",
  "G. cryophila", "Ochromonas sp. 1393", "Ochromonas sp. 2951", "P. micans",
  "A. sanguinea*"
)

# ---- Shaded rectangle spanning the scaled dinoflagellate bars ----
highlight_df <- data.frame(
  Metabolism = factor("Mixotroph", levels = c("Autotroph", "Mixotroph")),
  xmin       = min(asang_pos, pmicans_pos) - 0.5,
  xmax       = max(asang_pos, pmicans_pos) + 0.5
)

# ---- CSGR barplot with a secondary (rescaled) axis for dinoflagellates (panel a) ----
grazing_plot <- ggplot(df, aes(x = factor(name_plot, levels = resultsorder_grazing), y = AvGrazing_plot, fill = Type)) +
  geom_rect(
    data = highlight_df,
    aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
    fill = axis_color, alpha = 0.3,
    inherit.aes = FALSE) +
  geom_bar(stat = "identity", na.rm = TRUE) +
  geom_errorbar(
    aes(ymin = AvGrazing_plot - SdGrazing_plot, ymax = AvGrazing_plot + SdGrazing_plot),
    width = 0.2, na.rm = TRUE) +
  scale_y_continuous(
    name     = expression("CSGR (Bacteria cell"^{-1} * " hr"^{-1} * ")"),
    sec.axis = sec_axis(~ . / scale_factor, name = expression("CSGR (Bacteria cell"^{-1} * " hr"^{-1} * ")"))) +
  theme_classic() +
  ggtitle("a)") +
  facet_wrap(~Metabolism, scales = "free_x") +
  labs(x = "") +
  scale_fill_manual(values = color) +
  theme(
    legend.position    = "none",
    axis.text.x        = element_text(angle = 45, hjust = 1),
    axis.title.y.right = element_text(color = axis_color),
    axis.text.y.right  = element_text(color = axis_color),
    plot.margin        = margin(10, 10, 30, 10),
    text               = element_text(size = 16)) + 
  theme(legend.position="none")
grazing_plot


# =============================================================================
# PART 7: Assemble Full Figure and Export (Figure 1)
# =============================================================================

# ---- Stack CSGR / Tracker / Sensor with a shared bottom legend ----
final <- (grazing_plot / tracker / sensor) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")
final

ggsave("Figures/Figure1.tiff", plot = final, width = 14, height = 16, units = "in", dpi = 300)

# =============================================================================
# PART 8.4: Export the Shared Legend as a Standalone File
# -----------------------------------------------------------------------------
# Pulls the guide-box off tracker_legend, converts it to a plot object, and
# writes it out on its own. Useful for dropping the key into a slide, a poster,
# or assembling panels manually in Illustrator/Inkscape.
# =============================================================================

library(ggplotify)  
trackerdata <- read_excel("Data/CultureLysoData.xlsx") %>%
  group_by(Name, Type, Metabolism) %>%
  dplyr::summarise(
    mean_Lyso = mean(Tracker, na.rm = TRUE),
    n_bio     = n(),
    std       = sd(Tracker),
    .groups   = "drop"
  ) %>%
  mutate(percent = mean_Lyso * 100, std = std * 100)

trackerdata <- trackerdata %>%
  mutate(SizeClass = factor(sizeclass[Name], levels = size_levels))

tracker_legend <- ggplot(trackerdata, aes(x = factor(Name, levels = resultsorder), y = percent)) +
  geom_col_pattern(
    aes(fill = Type, pattern = SizeClass),
    pattern_fill    = "black",
    pattern_colour  = "black",
    pattern_density = 0.10,
    pattern_spacing = 0.015,
    pattern_angle   = 45,
    pattern_size    = 0.2,
    colour          = "black",
    linewidth       = 0.2
  ) +
  theme_classic() +
scale_fill_manual(
  values = color,
  name   = "Class",
  guide  = guide_legend(
    order          = 1,
    nrow           = 3,
    byrow          = TRUE,
    title.position = "top",
    override.aes   = list(pattern = "none", colour = "black", linewidth = 0.2)
  )
) +
scale_pattern_manual(
  values = size_patterns,
  labels = size_labs,
  name   = "Size class",
  guide  = guide_legend(
    order          = 2,
    nrow           = 3,
    byrow          = TRUE,
    title.position = "top",
    override.aes   = list(fill = "white", colour = "black")
  )
) +
  theme(
    text             = element_text(size = 16),
    legend.position  = "bottom",      
    legend.box       = "horizontal",
    legend.box.just  = "top",
    legend.title     = element_text(face = "bold"),
    legend.key.size  = unit(0.9, "lines"),
    legend.spacing.x = unit(1.2, "cm")
  )

legend_grob <- cowplot::get_plot_component(tracker_legend, "guide-box-bottom", return_all = FALSE)


get_legend_grob <- function(p) {
  g   <- ggplotGrob(p)
  idx <- which(grepl("guide-box", g$layout$name))
  for (i in idx) {
    gr <- g$grobs[[i]]
  }
}

legend_grob <- get_legend_grob(tracker_legend)

legend_only <- ggplotify::as.ggplot(legend_grob)
legend_only

ggsave("Figures/Figure1_Legend.tiff", plot = legend_only,
       width = 12, height = 2, units = "in", dpi = 300)

