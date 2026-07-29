# =============================================================================
# Supplemental Figure 8: NES Day/Night Mixotroph Abundance and T1–T0 Percent
# -----------------------------------------------------------------------------
# Summarizes NES prey-uptake flow cytometry into day/night mixotroph
# concentrations per station, computes the T1–T0 change as a percent of
# nanoeukaryotes, and overlays both on a dual-axis faceted bar plot.
# =============================================================================

# ---- Load required libraries ----
library(ggplot2)
library(data.table)
library(dplyr)
library(readxl)
library(gtools)
library(grid)
library(readxl)
library(arsenal)
library(ggpmisc)
library(gridExtra)
library(caret)
library(tidyr)
library(stringr)


# =============================================================================
# PART 1: Load Raw Data
# =============================================================================

# ---- Load NES FLP/FCM data; treat Timepoint as a discrete factor ----
df <- read_excel("Data/NES_FLP_FCM.xlsx") %>%
  mutate(Timepoint = factor(Timepoint))


# =============================================================================
# PART 2: Summarize Mixotroph Abundance (Day/Night)
# =============================================================================

# ---- Mean/SD mixotroph abundance per station, prey type, depth, timepoint ----
daynight <- df %>%
  filter(Type %in% c("Ecoli", "Green"), Station != 0) %>%
  group_by(Station, Type, Place, Timepoint) %>%
  dplyr::summarise(
    avmixo  = mean(mixo),
    sdmixo  = sd(mixo),
    .groups = "drop"
  )


# =============================================================================
# PART 3: Average Nanoeukaryote Abundance
# =============================================================================

# ---- Mean total nanoeukaryotes (nano + mixo), excluding Timepoint 2 ----
avnano <- df %>%
  filter(Timepoint != 2, Type %in% c("Ecoli", "Green")) %>%
  mutate(totalnano = nano + mixo) %>%
  group_by(Station, Type, Place) %>%
  dplyr::summarise(
    avnano  = mean(totalnano),
    sdnano  = sd(totalnano),
    .groups = "drop"
  )


# =============================================================================
# PART 4: T1–T0 Mixotroph Change and Percent Contribution
# =============================================================================

# ---- Difference in mean mixotroph abundance between T1 and T0 ----
# Expressed as a percent of average nanoeukaryotes; standardize depth label.
daynightsub <- df %>%
  filter(Type %in% c("Ecoli", "Green"), Station != 0) %>%
  group_by(Station, Type, Place, Timepoint) %>%
  dplyr::summarise(avmixo = mean(mixo), .groups = "drop") %>%
  pivot_wider(names_from = Timepoint, values_from = avmixo) %>%
  mutate(diff = `1` - `0`) %>%
  left_join(avnano, by = c("Station", "Type", "Place")) %>%
  mutate(
    percentmixo = (diff / avnano) * 100,
    Timepoint   = factor(1),
    Depth       = if_else(Place == "SCM", "DCM", Place)
  ) %>%
  dplyr::select(Station, Type, Place, percentmixo, Timepoint, Depth) %>%
  drop_na()


# =============================================================================
# PART 5: Relabel Factors for Display
# =============================================================================

# ---- Map raw codes to publication labels ----
relabeller <- function(x) {
  dplyr::recode(x,
                Ecoli = "E. coli",
                Green = "Microspheres",
                DCM   = "SCM")
}

daynight <- daynight %>%
  filter(Timepoint %in% c("0", "1")) %>%
  mutate(
    Type  = relabeller(Type),
    Place = relabeller(Place)
  )

daynightsub <- daynightsub %>%
  mutate(
    Type  = relabeller(Type),
    Place = relabeller(Place)
  )


# =============================================================================
# PART 6: Build the Dual-Axis Faceted Plot
# =============================================================================

# ---- Scaling factor aligning the percent overlay to the abundance axis ----
coeff <- 40  # initial value, recomputed below from the data ranges
max_mixo <- max(daynight$avmixo + daynight$sdmixo, na.rm = TRUE)
max_pct  <- max(daynightsub$percentmixo, na.rm = TRUE)
coeff <- max_mixo / max_pct

# ---- Bars: mixotroph abundance by depth/timepoint; red points: T1–T0 percent ----
suppfig7 <- ggplot(daynight, aes(x = Place, y = avmixo, fill = Timepoint)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_grid(Type ~ Station, scales = "fixed") +
  geom_errorbar(
    aes(ymin = avmixo - sdmixo, ymax = avmixo + sdmixo),
    position = position_dodge(.9),
    width = 0.3
  ) +
  geom_point(
    data = daynightsub,
    aes(x = Place, y = percentmixo * coeff),
    colour = "red",
    size = 2,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(
    name = "Concentration of potential mixotrophs (cells/mL)",
    sec.axis = sec_axis(~ . / coeff, name = "Percent of mixotrophic nanoeukaryotes (T1–T0)")
  ) +
  scale_fill_manual(values = c("gray80", "gray20")) +
  theme_bw() +
  theme(axis.title.y.right = element_text(color = "red"), text = element_text(size = 14)) +
  labs(y = "Concentration of Potential Mixotrophs (cells/mL)", x = "Depth")


# =============================================================================
# PART 7: Export
# =============================================================================

ggsave("Figures/SuppFig8.tiff", suppfig7, width = 10, height = 6, dpi = 300)