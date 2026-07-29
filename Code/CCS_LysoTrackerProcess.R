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
# Data collected from the California Current System LysoTracker experiments.
# The file contains measurements of nanoeukaryote populations with and without LysoTracker staining
  # as well as synechococcus, picoeukaryotes, and heterotrophic nanoeukaryotes
df <- read_excel("Data/20241203_CCSLysoTrackerRaw.xlsx")

# ---- Step 1: Summarize Unstained Community Composition ----
# For samples where no LysoTracker was added (Lyso == "no"),
# we summarize average and standard deviation of various community groups (Synechococcus, picoeukaryotes, nanoeukaryotes
# across each technical duplicate pair. 

community <- df %>%
  filter(Lyso=="no") %>%
  group_by(Station, Light, Depth) %>%
  dplyr::summarise(avsyn=mean(syn), sdsyn=sd(syn), 
                   avpico=mean(picoeuk), sdpico=sd(picoeuk), 
                   avcrypto=mean(cryptophyte), sdcrypto=sd(cryptophyte), 
                   avnano=mean(unstainednanoeuks), sdnano=sd(unstainednanoeuks))

# Extract only community stations (1–4), which had no staining or mixotrophy treatment
# These stations didn't have the correct conceentratiosn of LysoTracker added, and 
  # will be used only for community values, not mixotroph or heterotroph values.
commstns <- community %>%
  filter(Station %in% 1:4)


# ---- Step 2: Calculate LysoTracker-Induced Changes in Mixotrophy and Heterotrophy ----
# For experimental stations (5–10), we calculate differences in:
#   - heterotrophic concentration (hetero)
#   - percent of nanoeukaryotes that are mixotrophic (percentmixo)
#   - number of stained nanoeukaryotes
# Differences are calculated per replicate between LysoTracker-treated (yes) and untreated (no) samples.
  # and then averaged across technical duplicates. 

mixohetero <- df %>%
  mutate(percentmixo=(stainednanoeuks/(stainednanoeuks+unstainednanoeuks))) %>%
  filter(Station %in% 5:10)%>%
  group_by(Station, Light, Depth, Rep) %>%
  dplyr::summarise(
    hetero_diff = hetero[Lyso == "yes"] - hetero[Lyso == "no"],
    percentmixo_diff = percentmixo[Lyso == "yes"] - percentmixo[Lyso == "no"],
    stainednanoeuks_diff = stainednanoeuks[Lyso == "yes"] - stainednanoeuks[Lyso == "no"],
    .groups = "drop") %>%
  group_by(Station,Light,Depth) %>%
  dplyr::summarise(avhetero=mean(hetero_diff), sdhetero=sd(hetero_diff),
                   avpercent=mean(percentmixo_diff), sdpercent=sd(percentmixo_diff), 
                   avmixo=mean(stainednanoeuks_diff), sdmixo=sd(stainednanoeuks_diff))


# ---- Step 3: Combine Community and Mixotrophy Data ----
# Merge baseline community data and experimental data for stations 5–10.
# Also re-add stations 1–4, which had only community structure data.

merged <- merge(community, mixohetero)
final <- rbind(commstns, merged)

# ---- Step 4: Export Final Summarized Dataset ----
# This file includes both community composition (unstained) and LysoTracker treatments
write.csv(final, "Data/20241203_CCSLysoTrackerProcessed.csv")

