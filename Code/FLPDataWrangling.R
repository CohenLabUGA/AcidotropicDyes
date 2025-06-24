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

set.seed(123)

#### NES Wrangling ####
nesflpfcm <- read_excel("Data/NES_FLP_FCM.xlsx")
nesflpfcm$Timepoint <- as.factor(nesflpfcm$Timepoint)

avnano <- nesflpfcm %>%
  group_by(Station, Place, Time) %>%
  mutate(totalnano=nano+mixo)%>%
  dplyr::summarise(avnano=mean(totalnano), sdnano=sd(totalnano))

submixoconc <- nesflpfcm %>%
  filter(Type=="Green") %>%
  group_by(Station, Place, Timepoint, Rep, Time) %>%
  pivot_wider(id_cols=c(Station, Place, Type, Rep, Time),
              names_from=Timepoint, 
              values_from=mixo) %>%
  mutate(submixoconc= `1`-`0`)

calcpercent <- merge(avnano, submixoconc) %>%
  mutate(percentmixo=(submixoconc/(avnano)*100)) 

bac  <- read_excel("Data/NESBacteria.xlsx")

merge_columns <- c("Station", "Place", "Time")
merged_df <- merge(calcpercent, bac, by = merge_columns)

nesgrazing <- merged_df %>%
  mutate(CSGR=((submixoconc/avnano) * (bacteria/(10^5)))) %>%
  mutate(nanoBR = ((submixoconc/avnano * submixoconc * (bacteria/10^5))))%>%
  mutate(loggrazing=log10(CSGR))%>%
  group_by(Station, Place) %>%
  dplyr::summarise(avconc=mean(submixoconc), sdconc=sd(submixoconc), 
                   avpercent=mean(percentmixo), sdpercent=sd(percentmixo), 
                   avgrazing=mean(CSGR), sdgrazing=sd(CSGR), 
                   avbac=mean(bacteria), sdbac=sd(bacteria), 
                   avloggrazing=mean(loggrazing), sdloggrazing=sd(loggrazing), 
                   avnanoBR=mean(nanoBR), sdnanoBR=sd(nanoBR)) %>%
  mutate(Cruise = "New England Shelf") %>%
  filter(!(Station == 9 & Place == "DCM")) %>%
  mutate(Depth=Place) %>%
  dplyr::select(!Place)
nesgrazing$Station <- as.factor(nesgrazing$Station)

blankstationsNES <- data.frame(
  Station=c(6, 10), 
  avgrazing = NA, sdgrazing =NA, 
  avbac=NA, sdbac=NA, 
  avconc=NA, sdconc=NA, 
  avloggrazing=NA, sdloggrazing=NA, 
  avpercent=NA, sdpercent=NA, 
  avnano=NA, sdnano=NA,
  avnanoBR=NA, sdnanoBR=NA, 
  Cruise="New England Shelf", 
  Depth="Surface")
blankstationsNES$Station <- as.factor(blankstationsNES$Station)

nesflpgrazing <- rbind(nesgrazing, blankstationsNES)

nesflpgrazing$Method <- "fcm"
nesmicroscope <- read_excel("Data/NES_FLP_Microscopy.xlsx") %>%
  group_by(Depth, Station) %>%
  mutate(IR = (NumMixo/NanoEuk)) %>%
  dplyr::summarise(microscopepercent=(mean(PercentMixo)), sdmicroscopepercent=(sd(PercentMixo)), 
                   avIR=mean(IR), sdIR=sd(IR), 
                   microscopemixo=mean(NumMixo), sdmicroscopemixo=sd(NumMixo))
nesmicroscope$Method <- "Microscopy"
nesgrazing <- nesflpgrazing %>%
  left_join(nesmicroscope, by=c("Station", "Depth")) %>%
  mutate(microscopeGR =  (avIR * (avbac/(10^5))), 
         microscopeBR = (avIR * microscopemixo * (avbac/(10^5))))

#### CCS wrangling ####

ccsgrazing <- read_excel("~/Desktop/OneDrive/Cruises/PUPCYCLE/MicroscopeCounts/AllMicroscope.xlsx") %>%
  group_by(Station) %>%
  mutate(CSGR=(mixo_cellsmL/red_cellsmL) * (bacteria/(10^5))) %>%
  mutate(nanoBR = ((mixo_cellsmL/red_cellsmL) * mixo_cellsmL * (bacteria/10^5)))%>%
  dplyr::summarise(
    avbac=mean(bacteria), sdbac=sd(bacteria), 
    avpercent=mean(percentmixo), sdpercent=sd(percentmixo),
    avconc=mean(submixoconc), sdconc=sd(submixoconc), 
    avloggrazing=mean(loggrazing), sdloggrazing=sd(loggrazing), 
    avgrazing=mean(CSGR), sdgrazing=sd(CSGR), 
    avnano=mean(red_cellsmL), sdnano=sd(red_cellsmL), 
    avnanoBR = mean(nanoBR), sdnanoBR=sd(nanoBR)) %>%
  mutate(Cruise = "California Current System") %>%
  mutate(Depth="Surface") 
ccsgrazing$Station <- as.factor(ccsgrazing$Station)

#### Save dataframes for combination and future use ####
write.csv(ccsgrazing, "Data/CCS_FLP_Processed.csv")
write.csv(nesgrazing, "Data/NES_FLP_Processed.csv")

