# ---- Load Required Libraries ----
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


# --------------------------------------------------------
# Supplemental Figure 12: Taxonomic composition by station
# --------------------------------------------------------

# Read in dataframe
df <- read.csv("Data/station_protist_taxonomy.csv") 
df$Station <- as.factor(df$Station)

# Custom color palette for taxonomic groups
browngreenblue <- c("#D2B48C", "#A9BA9D", "#556B2F", "#8B5E3C", "#B77952", "#4682B4", "#87CEEB", "#3CB371", "#003366")

# Stacked bar plot of taxonomic groups by station
stationtaxa <- ggplot(df, aes(x=Station, y=Avg_Percentage, fill=lineage))+
  geom_bar(stat="identity") +
  scale_fill_manual(values =browngreenblue) +
  theme_classic()+
  labs(x="Station", y='% Protist Normalized Reads', fill='Taxonomic Group')

# Save figure
ggsave("Figures/SuppFig13.tiff", plot = stationtaxa, width = 6, height = 6, units = "in", dpi = 300)

# --------------------------------------------------------
# Figure 6a: Effect of Fe treatments on mixotrophy (LysoTracker)
# --------------------------------------------------------

# Read and format iron incubation data
incubations <- read_excel("Data/CCSIncubations.xlsx") %>%
  mutate(Timepoint = case_when(
    Timepoint =="T0" ~0,
    Timepoint =="T1" ~2,
    Timepoint == "T2" ~ 7,
    Timepoint == "T3" ~ 11)) %>%
  mutate(Treatment = case_when(
    Treatment=="control" ~ "Control"))%>%
  mutate(proportionmixos = proportionmixos *100) %>%
  filter(Treatment=="Control")

# Prepare data for ANOVA 
data <- data.frame(timepoint = as.factor(incubations$Timepoint),
                   treatment = as.factor(incubations$Treatment),
                   result = incubations$proportionmixos)

# Perform one-way ANOVA per treatment, calculate posthoc pairwise comparisons
df_letters <- data %>%
  group_by(treatment) %>%
  nest() %>% # Nest each treatment's data into its own tibble
  rowwise() %>% # Iterate row by row so each treatment is processed independently
  mutate(
    fit = list(lm(result ~ timepoint, data = data)), # Fit a linear model by timepoint within each treatment (with two groups and a categorical predictor, same as t-test)
    emm = list(emmeans::emmeans(fit, "timepoint", type = "response")), # compute estimated marginal means for each timepoint
    cld = list(multcomp::cld(emm, Letters = LETTERS, reverse = TRUE)) # Run letter display to assign letters to timepoints based on posthoc pairwise comparisons
  ) %>%
  select(-data, -fit, -emm) %>%
  unnest(cld) %>%
  mutate(cld = trimws(.group)) %>%
  select(-.group)

# Confirm with strict t-test
shapiro.test(data$result)
t_test <- t.test(result ~ timepoint, data = data)
print(t_test)

# Plot LysoTracker staining in iron manipulation experiments
lysotrackerplot <- ggplot(df_letters, aes(x = timepoint, y = emmean, fill = treatment)) +
  geom_bar(position = 'dodge', stat = 'identity') +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL),
                position = position_dodge(.9), width = 0.2, color = "black") +
  geom_text(aes(label = cld, y = upper.CL),
            vjust = -0.5, position = position_dodge(0.9),
            size = 3, color = "black") +
  labs(x = "", y = "Percent of Grazing\nPhototrophs (LysoTracker)",fill = "Taxonomic Group") +
  theme_classic(base_size = 14) +
  scale_fill_manual(values = "gray80") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylim(0, 100) +
  ggtitle("a)")
lysotrackerplot


# --------------------------------------------------------
# Figure 6b: Protist taxonomic composition in Fe incubations
# --------------------------------------------------------

# Read in data
taxa <- read_excel("Data/CubiTaxa.xlsx") %>%
  filter(Treatment== "Control")

# Define color pallette
browngreenblue <- c("#8B5E3C", "#D2B48C", "#B77952", "#4682B4", "#87CEEB", "#003366", 
                    "#556B2F", "#3CB371", "#A9BA9D")

# Bar plot of normalized reads by taxonomic group and treatment/timepoint
alltaxa <- ggplot(taxa, aes(x=Timepoint, Treatment, y=PercentReads, fill=Taxa))+
  geom_bar(stat="identity") +
  scale_fill_manual(values = browngreenblue) +
  theme_classic(base_size = 14)+
  ggtitle("b)")+
  labs(x="Day of Incubation", y='% Protist Normalized Reads', fill='Taxonomic Group')+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  scale_x_continuous(breaks = c(7, 11)) 


alltaxa

# Combine subplots a and b
final_plot <- plot_grid(lysotrackerplot, alltaxa, ncol = 1, rel_heights = c(1, 1))
final_plot
# Save figure
ggsave("Figures/Figure6.tiff", plot = final_plot, width = 8, height = 8, units = "in", dpi = 300)

# --------------------------------------------------------
# Supplemental Figure 13: Nanoeukaryotes and NO3 over time
# --------------------------------------------------------

# Coefficient to scale nitrate for plotting on secondary axis
coeff <- 700

# Plot: nanoeukaryote abundance + NO3 over time by treatment
supp12 <- ggplot()+
  geom_point(data=incubations, aes(x=Timepoint, y=nanoeukaryotes), color="black") + 
  geom_smooth(data=incubations, aes(x=Timepoint, y=nanoeukaryotes), se=FALSE, colour="black")+
  geom_point(data=incubations, aes(x=Timepoint, y=NO3*coeff), color="gray80")+
  geom_smooth(data=incubations, aes(x=Timepoint, y=NO3*coeff), color="gray80", se=FALSE, linetype="dashed" )+
  scale_y_continuous(name="Phototrophic Nanoeuks (cells/mL)", 
                     labels = scales::label_scientific(style = "plain"),
                     sec.axis=sec_axis(trans=~./coeff, name="Nitrate Concentration (µM)")) +
  theme_bw()+theme(axis.title.y.right= element_text(color="gray80"), 
                   text=element_text(size=17))+
  scale_x_continuous(breaks=c(0,2,7,11)) +
  labs(x="Time (days)")
supp12

# Save the figure
ggsave("Figures/SuppFig12.tiff", plot = supp12, width = 12, height = 6, units = "in", dpi = 300)

