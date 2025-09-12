# ---- Load Required Libraries ----
library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)

# ----------------------------------------------------------
# Supplemental Figure 3: Grazing bar plots + microscopy image
# ----------------------------------------------------------
set.seed(123)
# Read and summarize ingestion rate and FLP data
df <- read_excel("Data/SupplementalGrazing.xlsx") %>%
  group_by(Culture) %>%
  dplyr::summarise(avingestion=mean(Ingestion), sdingestion=sd(Ingestion), 
                   avpercent=(100*mean(FLPPercent)), sdpercent=(sd(FLPPercent) *100))

# A. Plot ingestion rate
IR <- ggplot(df, aes(x=Culture, y=avingestion)) + 
  geom_bar(stat = "identity", fill="gray18")+
  geom_errorbar(aes(ymin=avingestion - sdingestion, ymax=avingestion+sdingestion), width=0.3) +
  theme_bw() + 
  ggtitle("a)")+
  labs(x="", y=expression(paste("Ingestion Rate (Bacteria ", cell^{'-1'},hour^{-1}, ")"))) 
IR

# B. Plot percent of cells ingesting FLPs
percent <- ggplot(df, aes(x=Culture, y=avpercent)) + 
  geom_bar(stat = "identity", fill="gray80")+
  geom_errorbar(aes(ymin=avpercent - sdpercent, ymax=avpercent+sdpercent), width=0.3) +
  theme_bw() + 
  labs(y="Percent of Eating Cells FLP (%)") +
  ggtitle("b)")
percent

# C. Add microscopy image
photo <- cowplot::ggdraw() + 
  cowplot::draw_image("Data/TetraZStack-1.png") +
  cowplot::draw_label("c)", x = 0.05, y = 0.95, hjust = 0, vjust = 9, size = 14)
# Combine A and B into left panel
barplots <- plot_grid(
  IR, percent,
  ncol = 1,
  label_x = 0.1,   
  label_y = c(1., 1.05),
  align = 'v',
  rel_heights = c(1, 1)
)
barplots

# Final layout: barplots (left) + image (right)
final_figure <- plot_grid(
  barplots, photo,
  ncol = 2,
  label_x = c(0, 0.02), 
  label_y = c(1, 1),
  rel_widths = c(1.1, 1)
)
print(final_figure)

# Save full supplemental figure
ggsave("Figures/SuppFig4.png", final_figure, width = 10, height = 8, dpi = 300)


# ----------------------------------------------------------
# Supplemental Figure 2: Southern Ocean Mixotroph Cultures
# ----------------------------------------------------------

# Read and summarize ingestion rate data for SO mixotrophs
suppfiguredf <- read_excel("Data/SupplementalGrazing.xlsx", sheet="SOmixos") %>%
  group_by(Culture) %>%
  dplyr::summarise(AvIR=mean(IngestionRate), sdIR=sd(IngestionRate)) %>% 
  mutate(Culture = case_when(
    Culture == "GC" ~ "G. cryophila",
    Culture == "MA" ~ "M. antarctica",
    Culture == "PT" ~ "P. tychotreta"))

# Create bar plot of ingestion rate
somixos <- ggplot(suppfiguredf, aes(x=Culture, y=AvIR)) + 
  geom_bar(stat = "identity", color="gray18")+
  geom_errorbar(aes(ymin=AvIR - sdIR, ymax=AvIR+sdIR), width=0.3) +
  theme_bw() + 
  labs(y=expression(paste("Ingestion Rate (Bacteria ", cell^{'-1'},hour^{-1}, ")"))) 

# Save figure
ggsave("Figures/SuppFig3.png", somixos, width = 6, height = 6, dpi = 300)

