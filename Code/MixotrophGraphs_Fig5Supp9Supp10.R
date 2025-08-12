# ---- Load Required Libraries ----
library(cowplot)
library(gtools)
library(grid)
library(readxl)
library(arsenal)
library(ggpmisc)
library(gridExtra)
library(caret)
library(tidyr)
library(stringr)
library(dplyr)
library(grid)
library(tiff)
library(cowplot)
set.seed(123)

# ========================================
# Load and Prepare Data
# ========================================

# Load FLP data and clean up depth labels
flpdf <- read_excel("Data/AllFLPData.xlsx") %>%
  mutate(Depth = ifelse(Depth == "SCM", "DCM", Depth))

# Load LysoTracker data and convert proportions to percentages
lysodf <- read.csv("Data/AllCruiseLysoTracker.csv") %>%
  filter(!DepthCode =="Deep") %>%
  mutate(avpercent=100*avpercent,
         sdpercent=100*sdpercent) 

# Add in placeholder (blank) stations for consistent layout
missing_stations <- data.frame(
  Station = factor(c("3", "5", "8"), levels = levels(lysodf$Station)),  
  Cruise = "North East Shelf",  
  DepthCode = NA,             
  avpercent = NA,
  sdpercent = NA,
  avmixo = NA,
  sdmixo = NA)
lysodf <- bind_rows(lysodf, missing_stations)
lysodf <- lysodf %>%
  filter(!is.na(Station))

# Ensure consistent station ordering across datasets
lysodf$Station <- factor(lysodf$Station, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "X"))
flpdf$Station  <- factor(flpdf$Station,  levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "X"))

# Add jitter to x-axis for clearer point visualization
lysodf$jittered_x <- jitter(as.numeric(lysodf$Station), amount = 0.25)
flpdf$jittered_x  <- jitter(as.numeric(flpdf$Station),  amount = 0.25)


# ========================================
# Create Figure 5: FLP and LysoTracker Concentration + Percent
# ========================================
## Calculate intercruise means of concentration and percent
flp_means <- flpdf %>%
  group_by(Cruise) %>%
  summarise(mean_conc = mean(avconc, na.rm = TRUE), 
            mean_percent=mean(avpercent, na.rm=TRUE))

lyso_means <- lysodf %>%
  group_by(Cruise) %>%
  summarise(mean_conc = mean(avmixo, na.rm = TRUE), 
            mean_percent=mean(avpercent, na.rm=TRUE))
# ---- a) FLP mixotroph concentration ----
FLPmixoconc <- ggplot(flpdf, aes(x=jittered_x, y=avconc, color=Depth, shape=Cruise)) + 
  geom_point(size=3) +
  scale_shape_manual(values = c("California Current System" = 15,
                                "North East Shelf" = 16)) + 
  facet_wrap(~Cruise, scales="free_x") +
  geom_errorbar(aes(ymin=avconc-sdconc, ymax=avconc+sdconc), width=0.2) +
  labs(y = expression(paste("Mixotroph Concentration FLP ", "(Cells ",mL^{'-1'}, ')')), x="Station") + 
  theme_bw() +
  scale_color_manual(values = c("DCM" = "gray", "Surface" = "black"))+
  theme(legend.position = 'none')+
  ggtitle("a)")+
  scale_x_continuous(
    breaks = unique(as.numeric(as.factor(flpdf$Station))),
    labels = levels(as.factor(flpdf$Station))) +
  theme(text = element_text(size=18) ) +
  geom_hline(data = flp_means, aes(yintercept = mean_conc), 
             color = "red", linetype = "dashed")
FLPmixoconc

# ---- c) FLP mixotroph percent of nanoeukaryotes ----
FLPpercent <- ggplot(flpdf, aes(x = jittered_x, y = avpercent, color = Depth, shape=Method)) + 
  geom_point(size=3) +
  scale_shape_manual(values = c("Microscopy" = 15,
                                "FlowCytometry" = 16))+
  geom_errorbar(
    aes(ymin = avpercent - sdpercent, ymax = avpercent + sdpercent),
    width = 0.1
  ) +
  scale_x_continuous(
    breaks = unique(as.numeric(as.factor(flpdf$Station))),
    labels = levels(as.factor(flpdf$Station))
  ) +
  ggtitle("c)")+
  facet_wrap(~Cruise, scales = "free_x") +
  labs(y = expression(paste("Proportion of Mixotrophic Phototrophs FLP")), x="Station") + 
  theme_bw() +
  scale_color_manual(values = c("DCM" = "gray", "Surface" = "black")) +
  theme(legend.position = 'none')+  theme(text = element_text(size=18) ) +
  geom_hline(data = flp_means, aes(yintercept = mean_percent), 
             color = "red", linetype = "dashed")
FLPpercent

# ---- d) LysoTracker mixotroph proportion of nanoeukaryotes ----
lysopercent <- ggplot(lysodf, aes(x = jittered_x, y = avpercent, color = DepthCode)) + 
  geom_point(size = 3) +
  facet_wrap(~Cruise, scales = "free_x") +
  geom_errorbar(aes(ymin = avpercent - sdpercent, ymax = avpercent + sdpercent), width = 0.2) +
  labs(y = expression(paste("Percent of Mixotrophic Phototrophs LysoT")), x = "Station") + 
  theme_bw() +
  scale_color_manual(values = c("DCM" = "gray", "Surface" = "black")) +
  theme(legend.position = 'none', text = element_text(size = 18)) +
  ggtitle("d)")+
  scale_x_continuous(
    breaks = 1:11, 
    labels = levels(lysodf$Station)) +
  geom_hline(data = lyso_means, aes(yintercept = mean_percent), 
             color = "red", linetype = "dashed")
lysopercent

# ---- b) LysoTracker mixotroph concentration ----
lysoconcentration <- ggplot(lysodf, aes(x = jittered_x, y = avmixo, color = DepthCode)) + 
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = avmixo - sdmixo, ymax = avmixo + sdmixo),
                width = 0.2) +
  facet_wrap(~Cruise, scales = "free_x") +  
  labs(y = expression(paste("Mixotroph Concentration LysoT ", "(Cells ", mL^{-1}, ')')),
       x = "Station") + 
  theme_bw() +
  scale_color_manual(values = c("DCM" = "gray", "Surface" = "black")) +
  theme(legend.position = 'none', text = element_text(size = 18)) +
  ggtitle("b)")+
  scale_x_continuous(
    breaks = 1:11, 
    labels = levels(lysodf$Station)) +
  geom_hline(data = lyso_means, aes(yintercept = mean_conc), 
             color = "red", linetype = "dashed")
lysoconcentration

# ---- Extract legend from one plot ----
legend <- ggplot(flpdf, aes(x = Station, y = avpercent, color = Depth, shape=Method)) +
  geom_point(size=3) +
  theme_bw()+
  scale_color_manual(values = c("DCM" = "gray", "Surface" = "black")) +
  scale_shape_manual(values = c("Microscopy" = 15,
                                "FlowCytometry" = 16))+
  theme(text = element_text(size=18) )

legend <- get_legend(legend)

# ---- Combine all into one figure ----
mixoplot <- grid.arrange(
  arrangeGrob(
    FLPmixoconc, lysoconcentration,
    FLPpercent, lysopercent,
    ncol = 2,
    top = ""),
  legend,
  ncol = 2,
  widths = c(4, 1))
# ---- Save final Figure 5 ----
ggsave("Figures/Figure5.tiff", plot = mixoplot, width = 14, height = 11, units = "in", dpi = 300)

# ---- Save photos to be stitched to Figure 5 in illustrator ---
# Step 1: Download photos from Zenodo into the Data folder https://zenodo.org/records/16813439 and rename folder to "Photos"

# Step 2: Load in all CCS photos and turn into grobs with a random subset of 49 photos
ccstiffs <- list.files("Data/Photos/CCS", pattern="\\.tif$", full.names = TRUE)
# Read tiff's and turn into grobs
ccs_grobs <- lapply(ccstiffs, function(f) {
  img <- readTIFF(f, native = TRUE)
  rasterGrob(img, interpolate = TRUE)
})
# Take a random subset of 49
ccs_grobs <- sample(ccs_grobs, 49)

# Repeat for NES photos
nestiffs <- list.files("Data/Photos/NES", pattern="\\.tif$", full.names = TRUE)
nes_grobs <- lapply(nestiffs, function(f) {
  img <- readTIFF(f, native = TRUE)
  rasterGrob(img, interpolate = TRUE)
})
nes_grobs <- sample(nes_grobs, 49)

# Step 3: Arrange the grobs into columns of 7 each (7x7 square)
ccs_block <- arrangeGrob(grobs = ccs_grobs, ncol = 7)
nes_block <- arrangeGrob(grobs = nes_grobs, ncol = 7)

# Step 4: Label and save each image
ccs_labeled <- plot_grid(ggdraw() + draw_label("California Current System", fontface = "bold", size = 18),
                         ccs_block, ncol = 1, rel_heights = c(0.1, 1))
ggsave("Figures/CCSphotos.tiff", plot = ccs_labeled, width = 8, height = 8, units = "in", dpi = 300)

nes_labeled <- plot_grid(ggdraw() + draw_label("North East Shelf", fontface = "bold", size = 18),
                         nes_block, ncol = 1, rel_heights = c(0.1, 1))
ggsave("Figures/NESphotos.tiff", plot = nes_labeled, width = 8, height = 8, units = "in", dpi = 300)

# ========================================
# Supplemental Figure 11: FLP Grazing by Cruise
# ========================================

# ---- Plot grazing rate by cruise ----
csgr <- ggplot(flpdf, aes(x = Station, color = Depth, shape=Method)) + 
  geom_point(aes(y=avgrazing), size=3) +
  scale_shape_manual(values = c("Microscopy" = 15,
                                "FlowCytometry" = 16))+
  geom_errorbar(
    aes(ymin = avgrazing - sdgrazing, 
        ymax = avgrazing + sdgrazing),width=0.8) +
  labs(y = expression(paste("Cell specific grazing rate (Bacteria ", cell^{'-1'},hour^{-1}, ")")), x="Station") + 
  facet_wrap(~Cruise, scales="free_x")+
  theme_bw() +
  scale_color_manual(values = c("DCM" = "gray", "Surface" = "black")) +
  theme(text = element_text(size=16) )
csgr

# ---- Plot bacterivory rate ----
br <- ggplot(flpdf, aes(x = Station, color = Depth, shape=Method)) + 
  geom_point(aes(y=avnoBR), size=3) +
  scale_shape_manual(values = c("Microscopy" = 15,
                                "FlowCytometry" = 16))+
  geom_errorbar(
    aes(ymin = avnoBR - sdnoBR, 
        ymax = avnoBR + sdnoBR),width=0.8) +
  labs(y = expression(paste("Bacterivory Rate (Bacteria ", mL^{'-1'},hour^{-1}, ")")), x="Station") + 
  facet_wrap(~Cruise, scales="free_x")+
  theme_bw() +
  scale_color_manual(values = c("DCM" = "gray", "Surface" = "black")) +
  theme(text = element_text(size=16), legend.position = "none" )
br

# ---- Combine and save Supplemental Fig 10 ----
supp11 <- grid.arrange(br, csgr, nrow=1)
ggsave("Figures/SuppFig11.tiff", plot = supp11, width = 16, height = 8, units = "in", dpi = 300)


# ---- Create supplemental figure comparison ----
lysodf_tomerge <- lysodf %>%
  select(Station, avpercent,sdpercent,avmixo,sdmixo,Cruise,DepthCode) %>%
  dplyr::rename(Depth = DepthCode, 
                avconc = avmixo, 
                sdconc =sdmixo) 
flpdf_tomerge <- flpdf %>%
  select(Station, avpercent,sdpercent,avconc,sdconc,Cruise,Depth, Method)

mergedflplyso <- full_join(
  lysodf_tomerge,
  flpdf_tomerge,
  by = c("Station", "Cruise", "Depth"),
  suffix = c("_lyso", "_flp")) %>%
  mutate(Method = replace_na(Method, "FlowCytometry"))

r2_conc <- mergedflplyso %>%
  group_by(Cruise) %>%
  do({
    fit <- lm(avconc_flp ~ avconc_lyso, data = .)
    tidy_fit <- glance(fit)
    data.frame(Cruise = unique(.$Cruise), r2 = tidy_fit$r.squared)
  })

r2_percent <- mergedflplyso %>%
  group_by(Cruise) %>%
  do({
    fit <- lm(avpercent_flp ~ avpercent_lyso, data = .)
    tidy_fit <- glance(fit)
    data.frame(Cruise = unique(.$Cruise), r2 = tidy_fit$r.squared)
  })

label_pos_conc <- r2_conc %>%
  mutate(x = Inf, y = -Inf, label = paste0("R² = ", round(r2, 2)))

label_pos_percent <- r2_percent %>%
  mutate(x = Inf, y = -Inf, label = paste0("R² = ", round(r2, 2)))

conc <- ggplot(mergedflplyso, aes(x=avconc_lyso, y=avconc_flp, color=Depth, shape=Method)) + 
  geom_point(size=3, alpha=0.8) +
  facet_wrap(~Cruise, scales="free") +
  geom_errorbar(aes(ymin=avconc_flp - sdconc_flp, ymax=avconc_flp + sdconc_flp), alpha=0.4) +
  geom_errorbarh(aes(xmin=avconc_lyso - sdconc_lyso, xmax=avconc_lyso + sdconc_lyso), alpha=0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color="red") +  # 1:1 line
  geom_text(data = label_pos_conc, aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            hjust = 1.1, vjust = -0.5, size = 5, color = "red") +
  theme_bw() +  
  scale_color_manual(values = c("DCM" = "gray", "Surface" = "black")) +
  labs(y="Mixotroph Concentration FLP", x="Mixotroph Concentration LysoTracker") +
  ggtitle("b)") +  scale_shape_manual(values = c("Microscopy" = 15,
                                                 "FlowCytometry" = 16))+
  theme(text = element_text(size=18), legend.position="none")

percent <- ggplot(mergedflplyso, aes(x=avpercent_lyso, y=avpercent_flp, color=Depth, shape=Method)) + 
  geom_point(size=3, alpha=0.8) +
  facet_wrap(~Cruise, scales="free") +
  geom_errorbar(aes(ymin=avpercent_flp - sdpercent_flp, ymax=avpercent_flp + sdpercent_flp), alpha=0.4) +
  geom_errorbarh(aes(xmin=avpercent_lyso - sdpercent_lyso , xmax=avpercent_lyso + sdpercent_lyso), alpha=0.4) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed", color="red") +  # 1:1 line
  geom_text(data = label_pos_percent, aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            hjust = 1.1, vjust = -0.5, size = 5, color = "red") +
  theme_bw() +  
  scale_color_manual(values = c("DCM" = "gray", "Surface" = "black")) +
  labs(y="Mixotroph Percent FLP", x="Mixotroph Percent LysoTracker") +
  ggtitle("a)") +
  scale_shape_manual(values = c("Microscopy" = 15,
                                "FlowCytometry" = 16))+
  theme(text = element_text(size=18), legend.position="none")
grid.arrange(percent, conc)

legend <- ggplot(flpdf, aes(x = Station, y = avpercent, color = Depth, shape=Method)) +
  geom_point(size=3) +
  theme_bw()+
  scale_color_manual(values = c("DCM" = "gray", "Surface" = "black")) +
  scale_shape_manual(values = c("Microscopy" = 15,
                                "FlowCytometry" = 16))+
  theme(text = element_text(size=18) )

legend <- get_legend(legend)

# ---- Combine all into one figure ----
compplot <- grid.arrange(
  arrangeGrob(
    percent, conc,
    ncol = 1,
    top = ""),
  legend,
  ncol = 2,
  widths = c(4, 1))

ggsave("Figures/CompPlot.tiff", plot = compplot, width = 10, height = 8, units = "in", dpi = 300)

