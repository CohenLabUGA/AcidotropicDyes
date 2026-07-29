# =============================================================================
# Figure 4, Supplemental Figures 10 & 12: Fe Incubation Mixotrophy & Taxonomy
# -----------------------------------------------------------------------------
# Builds a station-level taxonomic composition bar plot (Supp Fig 12), the Fe
# incubation LysoTracker + taxonomy figure (Figure 4a/b), and a nanoeukaryote/
# nitrate time-series (Supp Fig 10). 
# =============================================================================

# ---- Load required libraries ----
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
library(cowplot)


# =============================================================================
# PART 1: Supplemental Figure 13 — Taxonomic Composition by Station
# =============================================================================

# ---- Read station-level taxonomy; treat Station as a discrete axis ----
df <- read.csv("Data/station_protist_taxonomy.csv")
df$Station <- as.factor(df$Station)

# ---- Custom brown/green/blue palette for taxonomic lineages ----
browngreenblue <- c("#D2B48C", "#A9BA9D", "#556B2F", "#8B5E3C", "#B77952", "#4682B4", "#87CEEB", "#3CB371", "#003366")

# ---- Stacked bar plot: lineage composition per station ----
stationtaxa <- ggplot(df, aes(x = Station, y = Avg_Percentage, fill = lineage)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = browngreenblue) +
  theme_classic() +
  labs(x = "Station", y = "% Protist Normalized Reads", fill = "Taxonomic Group") +
  theme(text = element_text(size = 16))

ggsave("Figures/SuppFig13.tiff", plot = stationtaxa, width = 8, height = 6, units = "in", dpi = 300)


# =============================================================================
# PART 2: Figure 4a — Fe Treatment Effect on Mixotrophy (LysoTracker)
# =============================================================================

# ---- Load Fe incubation data: recode timepoints to days, keep Control, avg technical reps ----
incubations <- read_excel("Data/CCSIncubations.xlsx") %>%
  mutate(Timepoint = case_when(
    Timepoint == "T0" ~ 0,
    Timepoint == "T1" ~ 2,
    Timepoint == "T2" ~ 7,
    Timepoint == "T3" ~ 11
  )) %>%
  mutate(Treatment = case_when(
    Treatment == "control" ~ "Control"
  )) %>%
  mutate(proportionmixos = proportionmixos * 100) %>%
  filter(Treatment == "Control")

# ---- Collapse technical reps to one value per biological replicate ----
incubations_reps <- incubations %>%
  group_by(Timepoint, Treatment, Replicate) %>%
  dplyr::summarise(proportionmixos = mean(proportionmixos), .groups = "drop")

# ---- Biological-replicate mean/SD, restricted to the final two timepoints ----
incubationsbiorepssd <- incubations_reps %>%
  group_by(Treatment, Timepoint) %>%
  dplyr::summarise(avprop = mean(proportionmixos),
                   sdprop = sd(proportionmixos), .groups = "drop") %>%
  filter(Timepoint %in% c(7, 11))

# ---- Normality check and timepoint comparison ----
shapiro.test(incubations_reps$proportionmixos)
t_test <- t.test(proportionmixos ~ Timepoint,
                 data = filter(incubations_reps, Timepoint %in% c(7, 11)))
print(t_test)

# ---- Load incubation taxonomy, Control only, matched timepoints ----
taxa <- read_excel("Data/CubiTaxa.xlsx") %>%
  filter(Treatment == "Control", Timepoint %in% c(7, 11))

# ---- Rescale taxon percentages so each timepoint's stack sums to avprop ----
# PercentReads is renormalized within timepoint first, so segment heights are
# exact even if the raw taxonomy columns do not sum to precisely 100.
taxa_scaled <- taxa %>%
  group_by(Timepoint) %>%
  mutate(RelAbund = PercentReads / sum(PercentReads)) %>%
  ungroup() %>%
  left_join(incubationsbiorepssd, by = "Timepoint") %>%
  mutate(StainedPercent = RelAbund * avprop)

# ---- Palette ordered to match the taxa in this panel ----
browngreenblue <- c("#8B5E3C", "#D2B48C", "#B77952", "#4682B4", "#87CEEB", "#003366",
                    "#556B2F", "#3CB371", "#A9BA9D")

# ---- Figure 4: stacked bars scaled to percent stained, error bars on the total ----
figure4 <- ggplot() +
  geom_bar(data = taxa_scaled,
           aes(x = Timepoint, y = StainedPercent, fill = Taxa),
           stat = "identity", color = "black", linewidth = 0.2) +
  geom_errorbar(data = incubationsbiorepssd,
                aes(x = Timepoint, ymin = avprop - sdprop, ymax = avprop + sdprop),
                width = 0.4, color = "black") +
  scale_fill_manual(values = browngreenblue) +
  theme_classic(base_size = 14) +
  labs(x = "Day of Incubation",
       y = "Percent Stained LysoTracker",
       fill = "Taxonomic Group") +
  scale_x_continuous(breaks = c(7, 11)) +
  ylim(0, 100)
figure4

ggsave("Figures/Figure4.tiff", plot = figure4, width = 8, height = 6, units = "in", dpi = 300)
# =============================================================================
# PART 4: Supplemental Figure 11 — Nanoeukaryotes and NO3 Over Time
# =============================================================================

# ---- Reload incubation data (Control), recoding timepoints to days ----
incubations <- read_excel("Data/CCSIncubations.xlsx") %>%
  mutate(Timepoint = case_when(
    Timepoint == "T0" ~ 0,
    Timepoint == "T1" ~ 2,
    Timepoint == "T2" ~ 7,
    Timepoint == "T3" ~ 11
  )) %>%
  mutate(Treatment = case_when(
    Treatment == "control" ~ "Control"
  )) %>%
  filter(Treatment == "Control")

# ---- Scaling coefficient to overlay NO3 on a secondary axis ----
coeff <- 700

# ---- Nanoeukaryote abundance (left axis) + nitrate (right axis) over time ----
supp10 <- ggplot() +
  geom_point(data = incubations, aes(x = Timepoint, y = nanoeukaryotes), color = "black") +
  geom_smooth(data = incubations, aes(x = Timepoint, y = nanoeukaryotes), se = FALSE, colour = "black") +
  geom_point(data = incubations, aes(x = Timepoint, y = NO3 * coeff), color = "gray80") +
  geom_smooth(data = incubations, aes(x = Timepoint, y = NO3 * coeff), color = "gray80", se = FALSE, linetype = "dashed") +
  scale_y_continuous(
    name   = "Phototrophic Nanoeuks (cells/mL)",
    labels = scales::label_scientific(style = "plain"),
    sec.axis = sec_axis(trans = ~ . / coeff, name = "Nitrate Concentration (µM)")
  ) +
  theme_bw() +
  theme(axis.title.y.right = element_text(color = "gray80"), text = element_text(size = 16)) +
  scale_x_continuous(breaks = c(0, 2, 7, 11)) +
  labs(x = "Time (days)")
supp10

ggsave("Figures/SuppFig11.tiff", plot = supp10, width = 8, height = 6, units = "in", dpi = 300)