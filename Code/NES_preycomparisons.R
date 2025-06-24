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

df <- read_excel("Data/NES_FLP_FCM.xlsx") %>%
  mutate(Timepoint = factor(Timepoint),
    percentmixo = mixo / (nano + mixo),
    totalnano = nano + mixo)

# Summarized data for all timepoints
summdata <- df %>%
  filter(Type %in% c("Ecoli", "Green")) %>%
  group_by(Place, Station, Time, Type, Timepoint) %>%
  summarise(
    avpercent = mean(percentmixo),
    sdpercent = sd(percentmixo),
    .groups = "drop")

# T1 - T0 delta calculation for each replicate
allcalc <- df %>%
  filter(Type %in% c("Ecoli", "Green")) %>%
  group_by(Station, Place, Time, Type, Rep) %>%
  filter(all(c(0, 1) %in% Timepoint)) %>%
  pivot_wider(names_from = Timepoint, values_from = percentmixo) %>%
  summarise(
    avpercent = mean(`1` - `0`, na.rm = TRUE),
    sdpercent = sd(`1` - `0`, na.rm = TRUE),
    .groups = "drop")

# Day/Night plot data (averaged over replicates)
daynight <- df %>%
  filter(Type %in% c("Ecoli", "Green"), Station != 0) %>%
  group_by(Station, Type, Place, Timepoint) %>%
  summarise(
    avpercent = mean(percentmixo),
    sdpercent = sd(percentmixo),
    avmixo = mean(mixo),
    sdmixo = sd(mixo),
    .groups = "drop")

# Average nanoeukaryote abundance (excluding T2)
avnano <- df %>%
  filter(Timepoint != 2, Type %in% c("Ecoli", "Green")) %>%
  group_by(Station, Type, Place) %>%
  summarise(
    avnano = mean(totalnano),
    sdnano = sd(totalnano),
    .groups = "drop")

# Difference T1-T0 and percent contribution
daynightsub <- df %>%
  filter(Type %in% c("Ecoli", "Green"), Station != 0) %>%
  group_by(Station, Type, Place, Timepoint) %>%
  summarise(avmixo = mean(mixo), .groups = "drop") %>%
  pivot_wider(names_from = Timepoint, values_from = avmixo) %>%
  mutate(diff = `1` - `0`) %>%
  left_join(avnano, by = c("Station", "Type", "Place")) %>%
  mutate(
    percentmixo = (diff / avnano) * 100,
    Timepoint = factor(1),
    Depth = if_else(Place == "SCM", "DCM", Place)) %>%
  select(Station, Type, Place, percentmixo, Timepoint, Depth) %>%
  drop_na()

# Relabel Type and Place for consistency
relabeller <- function(x) {
  x <- recode(x, "Ecoli" = "E. coli", "Green" = "Microspheres", "DCM" = "SCM")
  return(x)
}

daynight <- daynight %>%
  filter(Timepoint %in% c("0", "1")) %>%
  mutate(
    Type = relabeller(Type),
    Place = relabeller(Place))

daynightsub <- daynightsub %>%
  mutate(
    Type = relabeller(Type),
    Place = relabeller(Place))

# Plot
coeff <- 40

suppfig5 <- ggplot(daynight, aes(x = Place, y = avmixo, fill = Timepoint)) + 
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') +
  facet_grid(Type ~ Station, scales = "free") +
  geom_errorbar(
    aes(ymin = avmixo - sdmixo, ymax = avmixo + sdmixo),
    position = position_dodge(.9),
    width = 0.3) + 
  geom_point(
    data = daynightsub,
    aes(x = Place, y = percentmixo * coeff),
    colour = "red",
    size = 2,
    inherit.aes = FALSE) +
  scale_y_continuous(name = "Concentration of potential mixotrophs (cells/mL)",
    sec.axis = sec_axis(~./coeff, name = "Proportion of mixotrophic nanoeukaryotes (T1–T0)")) +
  scale_fill_manual(values = c("gray80", "gray20")) +
  theme_bw() +
  theme(axis.title.y.right = element_text(color = "red"), text = element_text(size = 14)) +
  labs(y = "Concentration of Potential Mixotrophs (cells/mL)", x = "Depth")

ggsave("Figures/Supp5.tiff", suppfig5, width = 10, height = 6, dpi = 300)
