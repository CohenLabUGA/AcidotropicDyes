library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)

df <- read_excel("Data/SupplementalGrazing.xlsx") %>%
  group_by(Culture) %>%
  dplyr::summarise(avingestion=mean(Ingestion), sdingestion=sd(Ingestion), 
                   avpercent=(100*mean(FLPPercent)), sdpercent=(sd(FLPPercent) *100))


IR <- ggplot(df, aes(x=Culture, y=avingestion)) + 
  geom_bar(stat = "identity", fill="gray18")+
  geom_errorbar(aes(ymin=avingestion - sdingestion, ymax=avingestion+sdingestion), width=0.3) +
  theme_bw() + 
  labs(x="", y=expression(paste("Ingestion Rate (Bacteria ", cell^{'-1'},hour^{-1}, ")"))) 
IR

percent <- ggplot(df, aes(x=Culture, y=avpercent)) + 
  geom_bar(stat = "identity", fill="gray80")+
  geom_errorbar(aes(ymin=avpercent - sdpercent, ymax=avpercent+sdpercent), width=0.3) +
  theme_bw() + 
  labs(y="Percent of Eating Cells FLP (%)") 
percent

photo <- cowplot::ggdraw() + 
  cowplot::draw_image("Data/TetraZStack-1.png")
barplots <- plot_grid(
  IR, percent,
  ncol = 1,
  labels = c("A", "B"),
  label_x = 0.1,   
  label_y = c(1., 1.05),
  align = 'v',
  rel_heights = c(1, 1)
)
barplots

final_figure <- plot_grid(
  barplots, photo,
  ncol = 2,
  labels = c("", "C"),
  label_x = c(0, 0.02), 
  label_y = c(1, 1),
  rel_widths = c(1.1, 1)
)
print(final_figure)

ggsave("Figures/Supp3.png", final_figure, width = 10, height = 6, dpi = 300)

#### Supplemental Figure 2 ####
suppfiguredf <- read_excel("Data/SupplementalGrazing.xlsx", sheet="SOmixos") %>%
  group_by(Culture) %>%
  dplyr::summarise(AvIR=mean(IngestionRate), sdIR=sd(IngestionRate)) %>% 
  mutate(Culture = case_when(
    Culture == "GC" ~ "G. cryophila",
    Culture == "MA" ~ "M. antarctica",
    Culture == "PT" ~ "P. tychotreta"))

somixos <- ggplot(suppfiguredf, aes(x=Culture, y=AvIR)) + 
  geom_bar(stat = "identity", color="gray18")+
  geom_errorbar(aes(ymin=AvIR - sdIR, ymax=AvIR+sdIR), width=0.3) +
  theme_bw() + 
  labs(y=expression(paste("Ingestion Rate (Bacteria ", cell^{'-1'},hour^{-1}, ")"))) 

ggsave("Figures/Supp2.png", somixos, width = 6, height = 6, dpi = 300)

