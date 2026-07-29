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

guava <- read_excel("Data/CultureLysoData.xlsx", sheet="Guava") %>%
  group_by(Name) %>%
  dplyr::summarise(avTracker_guava=mean(AvTracker)*100, SdTracker_guava=sd(AvTracker)*100)

cytpix <- read_excel("Data/CultureLysoData.xlsx") %>%
  group_by(Name) %>%
  dplyr::summarise(avTracker_cytpix=mean(Tracker)*100, sdTracker_cytpix =sd(Tracker)*100)

shared <- intersect(guava$Name, cytpix$Name)
shared

guava_sub <- guava[guava$Name %in% shared, ]
cytpix_sub <- cytpix[cytpix$Name %in% shared, ]


compare_df <- guava_sub %>%
  select(Name, avTracker_guava, SdTracker_guava) %>%
  inner_join(
    cytpix_sub %>% select(Name, avTracker_cytpix, sdTracker_cytpix),
    by = "Name")

mod <- lm(avTracker_cytpix ~ avTracker_guava, data = compare_df)
r2 <- round(summary(mod)$r.squared, 3)

plot <- ggplot(compare_df, aes(x = avTracker_guava, y = avTracker_cytpix, color = Name)) +
  geom_point(size=3) +
  geom_errorbar(aes(ymin=avTracker_cytpix - sdTracker_cytpix, ymax=avTracker_cytpix + sdTracker_cytpix)) + 
  geom_errorbarh(aes(xmin=avTracker_guava - SdTracker_guava, xmax=avTracker_guava + SdTracker_guava))+
  theme_bw() + 
  scale_color_brewer(palette = "Paired") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  labs(x="Percent Stained LysoTracker - Guava", y="Percent Stained LysoTracker - Cytpix", color="Culture") +
  annotate("text", x = 20, y = Inf, label = paste0("R² = ", r2),
           hjust = 1.1, vjust = 1.5, size = 5) +
  theme(text = element_text(size=16) )
  
ggsave("Figures/SuppFig5.tiff", plot = plot, width = 8, height = 6, units = "in", dpi = 300)


