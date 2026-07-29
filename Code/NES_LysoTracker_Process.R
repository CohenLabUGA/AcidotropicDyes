# ---- Load Required Libraries ----
# These packages are used for data manipulation, plotting, and reading Excel files
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


# ---- Read Raw Flow Cytometry Data ----
# Data collected from the New England Shelf LysoTracker experiments.
# The file contains measurements of nanoeukaryote populations with and without LysoTracker staining
# as well as synechococcus, picoeukaryotes, and heterotrophic nanoeukaryotes
df <- read_excel("Data/20241205_NESLysoTrackerRaw.xlsx")

# ---- Step 1: Summarize Unstained Community Composition ----
# For samples where no LysoTracker was added (Lyso == "no"),
# we summarize average and standard deviation of various community groups (Synechococcus, picoeukaryotes, nanoeukaryotes
# across each technical duplicate pair. 
community <- df %>%
  filter(LysoAdded=="No") %>%
  group_by(Station, Depth) %>%
  dplyr::summarise(avsyn=mean(syn), sdsyn=sd(syn), 
                   avpico=mean(picoeuk), sdpico=sd(picoeuk), 
                   avnano=mean(nanoeuk), sdnano=sd(nanoeuk))

# ---- Step 2: Calculate LysoTracker-Induced Changes in Mixotrophy and Heterotrophy ----
#   - heterotrophic concentration (hetero)
#   - percent of nanoeukaryotes that are mixotrophic (percentmixo)
#   - number of stained nanoeukaryotes
# Differences are calculated per replicate between LysoTracker-treated (yes) and untreated (no) samples.
# and then averaged across technical duplicates. 

mixohetero <- df %>%
  mutate(percentmixo=(green/(green+nanoeuk))) %>%
  group_by(Station, Depth, Rep, Time) %>%
  dplyr::summarise(
    hetero_diff = heteros[LysoAdded == "Yes"] - heteros[LysoAdded == "No"],
    percentmixo_diff = percentmixo[LysoAdded == "Yes"] - percentmixo[LysoAdded == "No"],
    stainednanoeuks_diff = green[LysoAdded == "Yes"] - green[LysoAdded == "No"],
    .groups = "drop"
  ) %>%
  group_by(Station,Depth) %>%
  dplyr::summarise(avhetero=mean(hetero_diff), sdhetero=sd(hetero_diff),
                   avpercent=mean(percentmixo_diff), sdpercent=sd(percentmixo_diff), 
                   avmixo=mean(stainednanoeuks_diff), sdmixo=sd(stainednanoeuks_diff))

# ---- Step 3: Combine Community and Mixotrophy Data ----
# Merge baseline community data and experimental data

merged <- merge(community, mixohetero)

# ---- Step 4: Export Final Summarized Dataset ----
# This file includes both community composition (unstained) and LysoTracker treatments

write.csv(merged, "Data/20241205_NESLysoTrackerProcessed.csv")
