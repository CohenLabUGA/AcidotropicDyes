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

df <- read_excel("Data/20241205_NESLysoTrackerRaw.xlsx")

community <- df %>%
  filter(LysoAdded=="No") %>%
  group_by(Station, Depth) %>%
  dplyr::summarise(avsyn=mean(syn), sdsyn=sd(syn), 
                   avpico=mean(picoeuk), sdpico=sd(picoeuk), 
                   avnano=mean(nanoeuk), sdnano=sd(nanoeuk))

duplicates <- df %>%
  group_by(Station, Depth, Rep, LysoAdded, Time) %>%
  summarise(count = n(), .groups = "drop") %>%
  filter(count > 1)

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

merged <- merge(community, mixohetero)

write.csv(merged, "Data/20241205_NESLysoTrackerProcessed.csv")
