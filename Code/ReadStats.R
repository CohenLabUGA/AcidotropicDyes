library(dplyr)
library(readxl)
library(tidyr)
library(ggplot2)
library(gt)
library(RColorBrewer)
library(patchwork)
library(gridExtra)

df <- read_excel("Data/PUPCYCLE_Reads_Stats.xlsx")
stndf <- read_excel("Data/PUPCYCLE_Reads_Stats.xlsx", sheet="Stations")

df <- df %>%
  dplyr::rename(Mapped = `% Reads Mapped`, 
         Annotated = `% Reads Taxonomically Annotated (MFT)`) %>%
  mutate(Mapped=(Mapped*100), 
         Annotated=(Annotated*100))

df_long <- df %>%
  pivot_longer(cols = c(Mapped, Annotated),
               names_to = "Category",
               values_to = "Percent")

df_summary <- df_long %>%
  group_by(Days, Category) %>%
  dplyr::summarise(mean_percent = mean(Percent),
            sd_percent = sd(Percent),
            .groups = "drop")

df_summary$Days <- as.factor(df_summary$Days)
incubation <- ggplot(df_summary, aes(x = Days, y = mean_percent, fill = Category)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = mean_percent - sd_percent,
                    ymax = mean_percent + sd_percent),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  theme_classic(base_size = 14) +
  labs(y = "% Reads (mean ± SD)", x = "Timepoint", fill = "") +
  scale_fill_manual(values=c("gray20", "gray80")) +
  ggtitle("b)")


stndf <- stndf %>%
  dplyr::rename(Mapped = `% Reads Mapped`, 
         Annotated = `% Reads Taxonomically Annotated (MFT)`) %>%
  mutate(Mapped=(Mapped*100), 
         Annotated=(Annotated*100))

stndf_long <- stndf %>%
  pivot_longer(cols = c(Mapped, Annotated),
               names_to = "Category",
               values_to = "Percent")

stndf_summary <- stndf_long %>%
  group_by(Station, Category) %>%
  dplyr::summarise(mean_percent = mean(Percent),
            sd_percent = sd(Percent),
            .groups = "drop") %>%
  na.omit()

stndf_summary$Station <- as.factor(stndf_summary$Station)

stns <- ggplot(stndf_summary, aes(x = Station, y = mean_percent, fill = Category)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = mean_percent - sd_percent,
                    ymax = mean_percent + sd_percent),
                position = position_dodge(width = 0.8),
                width = 0.2) +
  theme_classic(base_size = 14) +
  labs(y = "% Reads (mean ± SD)", x = "Station", fill = "") +
  scale_fill_manual(values=c("black", "gray")) +
  ggtitle("a)")

readstats <- grid.arrange(stns, incubation)

ggsave("Figures/SuppFigure8.tiff", plot = readstats, width = 7, height = 7, units = "in", dpi = 300)
