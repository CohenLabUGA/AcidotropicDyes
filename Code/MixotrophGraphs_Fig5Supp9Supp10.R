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

flpdf <- read_excel("Data/AllFLPData.xlsx") %>%
  mutate(Depth = ifelse(Depth == "SCM", "DCM", Depth))
lysodf <- read.csv("Data/AllCruiseLysoTracker.csv") %>%
  filter(!DepthCode =="Deep") %>%
  mutate(avpercent=100*avpercent,
         sdpercent=100*sdpercent) 

missing_stations <- data.frame(
  Station = factor(c("3", "5", "8"), levels = levels(lysodf$Station)),  
  Cruise = "New England Shelf",  
  DepthCode = NA,             
  avpercent = NA,
  sdpercent = NA,
  avmixo = NA,
  sdmixo = NA)
lysodf <- bind_rows(lysodf, missing_stations)
lysodf <- lysodf %>%
  filter(!is.na(Station))

lysodf$Station <- factor(lysodf$Station, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "X"))
flpdf$Station  <- factor(flpdf$Station,  levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "X"))

lysodf$jittered_x <- jitter(as.numeric(lysodf$Station), amount = 0.25)
flpdf$jittered_x  <- jitter(as.numeric(flpdf$Station),  amount = 0.25)

FLPmixoconc <- ggplot(flpdf, aes(x=jittered_x, y=avconc, color=Depth, shape=Cruise)) + 
  geom_point(size=3) +
  scale_shape_manual(values = c("California Current System" = 15,
                                "New England Shelf" = 16)) + 
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
  theme(text = element_text(size=18) )
FLPmixoconc


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
  theme(legend.position = 'none')+  theme(text = element_text(size=18) )
FLPpercent


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
    labels = levels(lysodf$Station))
lysopercent

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
    labels = levels(lysodf$Station))
lysoconcentration


legend <- ggplot(flpdf, aes(x = Station, y = avpercent, color = Depth, shape=Method)) +
  geom_point() +
  theme_bw()+
  scale_color_manual(values = c("DCM" = "gray", "Surface" = "black")) +
  scale_shape_manual(values = c("Microscopy" = 15,
                                "FlowCytometry" = 16))+
  theme(text = element_text(size=18) )

legend <- get_legend(legend)


mixoplot <- grid.arrange(
  arrangeGrob(
    FLPmixoconc, lysoconcentration,
    FLPpercent, lysopercent,
    ncol = 2,
    top = ""),
  legend,
  ncol = 2,
  widths = c(4, 1)  )
ggsave("Figures/Figure5.tiff", plot = mixoplot, width = 14, height = 11, units = "in", dpi = 300)

#### Supplemental Fig 9 ####
nesgrazing <- flpdf %>%
  filter(Cruise=="New England Shelf") %>%
  mutate(Depth = ifelse(Depth == "DCM", "SCM", Depth))

csgr <- ggplot(nesgrazing, aes(x = jittered_x, color = Method, shape=Method)) + 
  geom_point(aes(y=avgrazing), size=3) +
  scale_shape_manual(values = c("Microscopy" = 15,
                                "FlowCytometry" = 16))+
  geom_errorbar(aes(ymin = avgrazing - sdgrazing, 
                    ymax = avgrazing + sdgrazing), width=0.8) +
  labs(y = expression(paste("Cell specific grazing rate (Bacteria ", cell^{'-1'},hour^{-1}, ")")), x="Station") + 
  theme_bw() +
  scale_color_manual(values = c("Microscopy" = "gray", "FlowCytometry" = "black")) +
  theme(text = element_text(size=16) )+ scale_x_continuous(
    breaks = unique(as.numeric(as.factor(flpdf$Station))),
    labels = levels(as.factor(flpdf$Station)))+
  facet_wrap(~Depth)

csgr

percentmixos <- ggplot(nesgrazing, aes(x = jittered_x, color = Method, shape=Method)) + 
  geom_point(aes(y=avpercent), size=3) + 
  scale_shape_manual(values = c("Microscopy" = 15,
                              "FlowCytometry" = 16))+
  geom_errorbar(aes(ymin = avpercent - sdpercent, 
                    ymax = avpercent + sdpercent), width=0.8) +
  labs(y = "Percent of Mixotrophic Phototrophs", x="Station") + 
  theme_bw() +
  scale_color_manual(values = c("Microscopy" = "gray", "FlowCytometry" = "black")) +
  theme(text = element_text(size=16) )+ scale_x_continuous(
    breaks = unique(as.numeric(as.factor(flpdf$Station))),
    labels = levels(as.factor(flpdf$Station))) +
  facet_wrap(~Depth)
percentmixos

proportioncsgr <- grid.arrange(percentmixos, csgr)
ggsave("Figures/SuppFig9.tiff", plot = proportioncsgr, width = 9, height = 10, units = "in", dpi = 300)

#### Supplemental Figure 10 ####
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

supp10 <- grid.arrange(br, csgr, nrow=1)
ggsave("Figures/SuppFig10.tiff", plot = supp10, width = 16, height = 8, units = "in", dpi = 300)
