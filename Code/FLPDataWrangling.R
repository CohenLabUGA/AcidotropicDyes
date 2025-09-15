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
  filter(Type=="Green") %>% # Filter for only bead prey, called "Green" in this dataset
  group_by(Station, Place, Timepoint, Rep, Time) %>%
  pivot_wider(id_cols=c(Station, Place, Type, Rep, Time),
              names_from=Timepoint, 
              values_from=mixo) %>%
  mutate(submixoconc= `1`-`0`)

# ---- Merge and calculate percent mixotroph contribution ----
calcpercent <- avnano %>%
  left_join(submixoconc, by=c("Station", "Place", "Time")) %>%
  mutate(percentmixo=(submixoconc/(avnano)*100)) # avnano represents all nanoeukaryotes, including mixotrophs

# ---- Add bacterial data ----
bac  <- read_excel("Data/NESBacteria.xlsx")
merge_columns <- c("Station", "Place", "Time")
merged_df <- merge(calcpercent, bac, by = merge_columns)

# ---- Calculate grazing metrics (CSGR and nanoBR), then summarize across replicates ----
nesgrazing <- merged_df %>%
  mutate(CSGR=(((submixoconc/avnano)/1) * (bacteria/(10^5)))) %>% # dividing submixoconc/avnano by 1 accounts for the 1 hour incubation
  mutate(loggrazing=log10(CSGR))%>%
  mutate(nanoBR = (CSGR * submixoconc))%>% 
  group_by(Station, Place) %>%
  dplyr::summarise(avconc=mean(submixoconc), sdconc=sd(submixoconc), 
                   avpercent=mean(percentmixo), sdpercent=sd(percentmixo), 
                   avgrazing=mean(CSGR), sdgrazing=sd(CSGR), 
                   avbac=mean(bacteria), sdbac=sd(bacteria), 
                   avloggrazing=mean(loggrazing), sdloggrazing=sd(loggrazing), 
                   avnanoBR=mean(nanoBR), sdnanoBR=sd(nanoBR)) %>%
  mutate(Cruise = "North East Shelf") %>%
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
  avnanoBR=NA, sdnanoBR=NA, 
  Cruise="North East Shelf", 
  Depth="Surface")
blankstationsNES$Station <- as.factor(blankstationsNES$Station)

# ---- Combine real and placeholder NES data ----
nesflpgrazing <- rbind(nesgrazing, blankstationsNES)

nesflpgrazing$Method <- "fcm"

# ---- Load and summarize microscopy-based FLP data ----
nesmicroscope <- read_excel("Data/NES_FLP_Microscopy.xlsx") %>%
  group_by(Depth, Station) %>%
  na.omit() %>%
  dplyr::summarise(microscopepercent=(mean(percentchange)), sdmicroscopepercent=(sd(percentchange)))
                   
nesmicroscope$Method <- "Microscopy"

# ---- Join microscopy and FCM data, calculate additional microscopy-derived metrics ---
nesgrazing <- nesflpgrazing %>%
  left_join(nesmicroscope, by=c("Station", "Depth")) %>%
  mutate(microscopeGR =  (microscopepercent * (avbac/(10^5))))

# ========================
# PART 2: PROCESS CCS DATA
# ========================
# ---- Load, calculate, and summarize CCS FLP data ----
# Step 1: calculate the subtracted T1-T0 values for mixotrophs
ccssubtracted <- read_excel("Data/CCSRawFLP.xlsx") %>%
  group_by(Station) %>%
  group_by(Station, Timepoint, Replicate) %>%
  pivot_wider(id_cols=c(Station, Replicate),
              names_from=Timepoint, 
              values_from=c(mixo_cellsmL, percentmixo)) %>%
  mutate(subpercentmixo=(percentmixo_T1-percentmixo_T0), 
         submixoconc=(mixo_cellsmL_T1-mixo_cellsmL_T0)) %>%
  dplyr::select(Station, Replicate, subpercentmixo, submixoconc)

# Step 2: Calculate nanoeukaryote values
ccsnanoeukaryotes <- read_excel("Data/CCSRawFLP.xlsx") %>%
  group_by(Station, Replicate) %>%
  dplyr::summarise(nanoeukconcentration=mean(red_cellsmL+mixo_cellsmL))

# Step 3: load in bacteria concentrations
ccsbacteria <- read_excel("Data/BacteriaConcentrations.xlsx") %>%
  filter(Cruise =="California Current System") %>%
  filter(Depth<11.9) %>% #Include only surface depths
  group_by(Station) %>%
  dplyr::summarise(bacteria=mean(bacteria))
ccsbacteria$Station <- as.numeric(ccsbacteria$Station)

# Step 4: Merge dataframes together
ccsmicroscopedata <- left_join(ccssubtracted, ccsnanoeukaryotes, by=c("Station", "Replicate"))
ccsmicroscopedata <- left_join(ccsmicroscopedata, ccsbacteria, by=c("Station"))

# Step 5: Calculate grazing metrics
ccsgrazing <- ccsmicroscopedata %>%
  mutate(CSGR=((submixoconc/nanoeukconcentration)/1) * (bacteria/(10^5))) %>% #Divided by incubation time in hours (1)
  mutate(nanoBR = CSGR * submixoconc)%>%
  mutate(loggrazing=log10(CSGR)) %>%
  group_by(Station) %>%
  na.omit() %>%
  dplyr::summarise(
    avpercent=mean(subpercentmixo), sdpercent=sd(subpercentmixo),
    avconc=mean(submixoconc), sdconc=sd(submixoconc), 
    avloggrazing=mean(loggrazing), sdloggrazing=sd(loggrazing), 
    avgrazing=mean(CSGR), sdgrazing=sd(CSGR), 
    avnano=mean(nanoeukconcentration), sdnano=sd(nanoeukconcentration), 
    avnanoBR = mean(nanoBR), sdnanoBR=sd(nanoBR)) %>%
  mutate(Cruise = "California Current System") %>%
  mutate(Depth="Surface") 
ccsgrazing$Station <- as.factor(ccsgrazing$Station)


# ===========================================
# PART 3: SAVE CLEANED AND SUMMARIZED OUTPUTS
# ===========================================
write.csv(ccsgrazing, "Data/CCS_FLP_Processed.csv")
write.csv(nesgrazing, "Data/NES_FLP_Processed.csv")

