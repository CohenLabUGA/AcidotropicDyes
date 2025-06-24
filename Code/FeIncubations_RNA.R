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

#### Creation of supplemental figure 11####
df <- read.csv("Data/station_protist_taxonomy.csv")
df$Station <- as.factor(df$Station)
browngreenblue <- c("#D2B48C", "#A9BA9D", "#556B2F", "#8B5E3C", "#B77952", "#4682B4", "#87CEEB", "#3CB371", "#003366")

stationtaxa <- ggplot(df, aes(x=Station, y=Avg_Percentage, fill=lineage))+
  geom_bar(stat="identity") +
  scale_fill_manual(values =browngreenblue) +
  theme_classic()+
  labs(x="Station", y='% Protist Normalized Reads', fill='Taxonomic Group')

ggsave("Figures/SuppFig11.tiff", plot = stationtaxa, width = 6, height = 6, units = "in", dpi = 300)

#### Creation of Figure 6 ####
order <- c("Control", "Iron Addition", "DFB (iron chelator)")
custom_colors <- c("Control" ="#999999",
                   "Iron Addition" = "black",
                   "DFB (iron chelator)" = "gray80")


feincubations <- read_excel("Data/FeIncubations.xlsx") %>%
  mutate(Timepoint = case_when(
    Timepoint =="T0" ~0,
    Timepoint =="T1" ~2,
    Timepoint == "T2" ~ 7,
    Timepoint == "T3" ~ 11)) %>%
  mutate(Treatment = case_when(
    Treatment=="control" ~ "Control", 
    Treatment =="dfb" ~"DFB (iron chelator)", 
    Treatment =="iron"~"Iron Addition")) %>%
  mutate(proportionmixos = proportionmixos *100)

# Create a dataframe with only the necessary data
data <- data.frame(timepoint = as.factor(feincubations$Timepoint),
                   treatment = as.factor(feincubations$Treatment),
                   result = feincubations$proportionmixos)

# Run anova, p values are also extracted to report. 
df_aov <- data %>%
  dplyr::group_by(treatment) %>%
  tidyr::nest() %>%
  rowwise() %>% 
  dplyr::mutate(
    aov_results = list(aov(result ~ timepoint, data = data)),
    aov_summary = list(summary(aov_results)),
    p_value = aov_summary[[1]][["Pr(>F)"]][1],  # Extract p-value from summary
    emm = list(emmeans::emmeans(aov_results, "timepoint", type = "response")),
    cld = list(multcomp::cld(emm, Letters = LETTERS, reverse = TRUE))
  ) %>% 
  dplyr::select(-data, -aov_results, -aov_summary, -emm) %>%
  tidyr::unnest(cols = c(cld)) %>%
  dplyr::mutate(cld = trimws(.group)) %>%
  dplyr::select(-.group)

# Plot data
lysotrackerplot <- ggplot(df_aov, aes(x= factor(treatment, levels=order), y = emmean, fill = factor(treatment, levels=order)))+
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), position = position_dodge(.9), width = 0.2) + 
  geom_text(aes(label = cld, y = upper.CL), vjust = -0.5, position = position_dodge(0.9),size = 3)+
  labs(x="", y="Percent of Grazing\nPhototrophs (LysoTracker)", fill="Treatment")+
  theme_classic(base_size = 14)+
  scale_fill_manual(values = custom_colors) +
  facet_grid(~timepoint) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("a)")
lysotrackerplot

## Taxonomy plot Fe incubations ##
taxa <- read_excel("Data/CubiTaxa.xlsx")

browngreenblue <- c("#8B5E3C", "#D2B48C", "#B77952", "#4682B4", "#87CEEB", "#003366", 
                    "#556B2F", "#3CB371", "#A9BA9D")

alltaxa <- ggplot(taxa, aes(x=factor(Treatment, levels=order), Treatment, y=PercentReads, fill=Taxa))+
  geom_bar(stat="identity") +
  facet_grid(~Timepoint) +
  scale_fill_manual(values = browngreenblue) +
  theme_classic(base_size = 14)+
  ggtitle("b)")+
  labs(x="", y='% Protist Normalized Reads', fill='Taxonomic Group')+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

alltaxa

final_plot <- plot_grid(lysotrackerplot, alltaxa, ncol = 1, rel_heights = c(1, 1))

final_plot

ggsave("Figures/Figure6.tiff", plot = final_plot, width = 12, height = 12, units = "in", dpi = 300)


#### Supplemental figure 12 ####
coeff <- 700
supp12 <- nutanano <- ggplot()+
  geom_point(data=feincubations, aes(x=Timepoint, y=nanoeukaryotes), color="black") + 
  geom_smooth(data=feincubations, aes(x=Timepoint, y=nanoeukaryotes), se=FALSE, colour="black")+
  geom_point(data=feincubations, aes(x=Timepoint, y=NO3*coeff), color="gray80")+
  geom_smooth(data=feincubations, aes(x=Timepoint, y=NO3*coeff), color="gray80", se=FALSE, linetype="dashed" )+
  facet_grid(~factor(Treatment, levels=c("Control", "Iron Addition", "DFB (iron chelator)")), scales="free") +
  scale_y_continuous(name="Phototrophic Nanoeuks (cells/mL)", 
                     labels = scales::label_scientific(style = "plain"),
                     sec.axis=sec_axis(trans=~./coeff, name="Nitrate Concentration (µM)")) +
  theme_bw()+theme(axis.title.y.right= element_text(color="gray80"), 
                   text=element_text(size=17))+
  scale_fill_manual(values=c("#9EC1A3", "#40798C", "#1B264F"))+
  scale_x_continuous(breaks=c(0,2,7,11)) +
  labs(x="Time (days)")
supp12

ggsave("Figures/SuppFig12.tiff", plot = supp12, width = 12, height = 6, units = "in", dpi = 300)

