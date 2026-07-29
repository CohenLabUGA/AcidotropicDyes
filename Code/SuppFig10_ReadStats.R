# =============================================================================
# Supplemental Figure 10: Read Mapping and Annotation Rates
# -----------------------------------------------------------------------------
# Summarizes the percent of reads mapped and taxonomically annotated across
# stations (panel a) and incubation timepoints (panel b), plotting each as
# grouped bar charts with mean ± SD.
# =============================================================================

# ---- Load required libraries ----
library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(gt)
library(RColorBrewer)
library(patchwork)
library(gridExtra)


# =============================================================================
# PART 1: Incubation Timepoint Read Stats (panel b)
# =============================================================================

# ---- Load data; rename metrics and convert proportions to percent ----
df <- read_excel("Data/PUPCYCLE_Reads_Stats.xlsx")
stndf <- read_excel("Data/PUPCYCLE_Reads_Stats.xlsx", sheet = "Stations")

df <- df %>%
  dplyr::rename(
    Mapped    = `% Reads Mapped`,
    Annotated = `% Reads Taxonomically Annotated (MFT)`
  ) %>%
  mutate(
    Mapped    = (Mapped * 100),
    Annotated = (Annotated * 100)
  )

# ---- Reshape to long form and summarize by timepoint ----
df_long <- df %>%
  pivot_longer(
    cols = c(Mapped, Annotated),
    names_to = "Category",
    values_to = "Percent"
  )

df_summary <- df_long %>%
  group_by(Days, Category) %>%
  dplyr::summarise(
    mean_percent = mean(Percent),
    sd_percent   = sd(Percent),
    .groups = "drop"
  )
df_summary$Days <- as.factor(df_summary$Days)

# ---- Grouped bar chart: mapped vs. annotated by incubation day ----
incubation <- ggplot(df_summary, aes(x = Days, y = mean_percent, fill = Category)) +
  geom_col(position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = mean_percent - sd_percent, ymax = mean_percent + sd_percent),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  theme_classic(base_size = 14) +
  labs(y = "% Reads (mean ± SD)", x = "Timepoint", fill = "") +
  scale_fill_manual(values = c("gray20", "gray80")) +
  ggtitle("b)")


# =============================================================================
# PART 2: Per-Station Read Stats (panel a)
# =============================================================================

# ---- Same rename/scale on the Stations sheet ----
stndf <- stndf %>%
  dplyr::rename(
    Mapped    = `% Reads Mapped`,
    Annotated = `% Reads Taxonomically Annotated (MFT)`
  ) %>%
  mutate(
    Mapped    = (Mapped * 100),
    Annotated = (Annotated * 100)
  )

# ---- Reshape to long form and summarize by station (dropping NA rows) ----
stndf_long <- stndf %>%
  pivot_longer(
    cols = c(Mapped, Annotated),
    names_to = "Category",
    values_to = "Percent"
  )

stndf_summary <- stndf_long %>%
  group_by(Station, Category) %>%
  dplyr::summarise(
    mean_percent = mean(Percent),
    sd_percent   = sd(Percent),
    .groups = "drop"
  ) %>%
  na.omit()
stndf_summary$Station <- as.factor(stndf_summary$Station)

# ---- Grouped bar chart: mapped vs. annotated by station ----
stns <- ggplot(stndf_summary, aes(x = Station, y = mean_percent, fill = Category)) +
  geom_col(position = position_dodge(width = 0.8), color = "black") +
  geom_errorbar(aes(ymin = mean_percent - sd_percent, ymax = mean_percent + sd_percent),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  theme_classic(base_size = 14) +
  labs(y = "% Reads (mean ± SD)", x = "Station", fill = "") +
  scale_fill_manual(values = c("gray20", "gray80")) +
  ggtitle("a)")


# =============================================================================
# PART 3: Combine Panels and Export
# =============================================================================

# ---- Stack station panel (a) over incubation panel (b) and save ----
readstats <- grid.arrange(stns, incubation)
ggsave("Figures/SuppFig10.tiff", plot = readstats, width = 7, height = 7, units = "in", dpi = 300)