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

df <- read_excel("Data/20241203_CCSLysoTrackerRaw.xlsx")

community <- df %>%
  filter(Lyso=="no") %>%
  group_by(Station, Light, Depth) %>%
  dplyr::summarise(avsyn=mean(syn), sdsyn=sd(syn), 
                   avpico=mean(picoeuk), sdpico=sd(picoeuk), 
                   avcrypto=mean(cryptophyte), sdcrypto=sd(cryptophyte), 
                   avnano=mean(unstainednanoeuks), sdnano=sd(unstainednanoeuks))

commstns <- community %>%
  filter(Station %in% 1:4)

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

merged <- merge(community, mixohetero)
final <- rbind(commstns, merged)

write.csv(final, "Data/20241203_CCSLysoTrackerProcessed.csv")