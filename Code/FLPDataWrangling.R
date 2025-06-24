# ---- Load Required Libraries ----
# These libraries support data cleaning, reshaping, plotting, and statistical processing.
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
# Ensure reproducibility if random elements are added later
set.seed(123)

# ====================================================================
# PART 1: PROCESS NEW ENGLAND SHELF (NES) FLP + FCM + Microscopy Data
# ====================================================================

# ---- Load and format NES flow cytometry data ----
nesflpfcm <- read_excel("Data/NES_FLP_FCM.xlsx")
nesflpfcm$Timepoint <- as.factor(nesflpfcm$Timepoint)

# ---- Calculate average nanoeukaryote abundance per station, place, and time ----
avnano <- nesflpfcm %>%
  group_by(Station, Place, Time) %>%
  mutate(totalnano=nano+mixo)%>%
  dplyr::summarise(avnano=mean(totalnano), sdnano=sd(totalnano))

# ---- Compute change in mixotroph abundance from T0 to T1 (submixoconc) ----
submixoconc <- nesflpfcm %>%
  filter(Type=="Green") %>%
  group_by(Station, Place, Timepoint, Rep, Time) %>%
  pivot_wider(id_cols=c(Station, Place, Type, Rep, Time),
              names_from=Timepoint, 
              values_from=mixo) %>%
  mutate(submixoconc= `1`-`0`)

# ---- Merge and calculate percent mixotroph contribution ----
calcpercent <- merge(avnano, submixoconc) %>%
  mutate(percentmixo=(submixoconc/(avnano)*100)) 

# ---- Add bacterial data ----
bac  <- read_excel("Data/NESBacteria.xlsx")
merge_columns <- c("Station", "Place", "Time")
merged_df <- merge(calcpercent, bac, by = merge_columns)

# ---- Calculate grazing metrics (CSGR and nanoBR), then summarize across replicates ----
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

# ---- Add placeholder rows for stations with no data (for plot completeness) ----
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

# ---- Combine real and placeholder NES data ----
nesflpgrazing <- rbind(nesgrazing, blankstationsNES)

nesflpgrazing$Method <- "fcm"

# ---- Load and summarize microscopy-based FLP data ----
nesmicroscope <- read_excel("Data/NES_FLP_Microscopy.xlsx") %>%
  group_by(Depth, Station) %>%
  mutate(IR = (NumMixo/NanoEuk)) %>%
  dplyr::summarise(microscopepercent=(mean(PercentMixo)), sdmicroscopepercent=(sd(PercentMixo)), 
                   avIR=mean(IR), sdIR=sd(IR), 
                   microscopemixo=mean(NumMixo), sdmicroscopemixo=sd(NumMixo))
nesmicroscope$Method <- "Microscopy"

# ---- Join microscopy and FCM data, calculate additional microscopy-derived metrics ---
nesgrazing <- nesflpgrazing %>%
  left_join(nesmicroscope, by=c("Station", "Depth")) %>%
  mutate(microscopeGR =  (avIR * (avbac/(10^5))), 
         microscopeBR = (avIR * microscopemixo * (avbac/(10^5))))

# ========================
# PART 2: PROCESS CCS DATA
# ========================
# ---- Load, calculate, and summarize CCS FLP data ----
ccsgrazing <- read_excel("Data/CCSRawFLP.xlsx") %>%
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


# ===========================================
# PART 3: SAVE CLEANED AND SUMMARIZED OUTPUTS
# ===========================================
write.csv(ccsgrazing, "Data/CCS_FLP_Processed.csv")
write.csv(nesgrazing, "Data/NES_FLP_Processed.csv")

