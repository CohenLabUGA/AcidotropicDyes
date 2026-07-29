library(tidyverse)
library(ggplot2)

setwd("Data/Zstackfluoresence")

files <- tibble(
  stack = rep(1:8, each = 2),
  channel = rep(c("Red", "Green"), times = 8),
  file = c(
    "Red1.csv", "Green1.csv",
    "Red2.csv", "Green2.csv",
    "Red3.csv", "Green3.csv",
    "Red4.csv", "Green4.csv",
    "Red5.csv", "Green5.csv",
    "Red6.csv", "Green6.csv",
    "Red7.csv", "Green7.csv",
    "Red8.csv", "Green8.csv"))

fluor <- files %>%
  mutate(data = map(file, ~
                      read_csv(.x) %>%
                      rename(Distance = !!names(.)[1]))) %>%
  unnest(data)

# ---- 3. Plot ----
ggplot(fluor, aes(x = Distance, y = Gray_Value, color = channel)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ stack, ncol = 4) +
  scale_color_manual(values = c("Green" = "green3", "Red" = "red3")) +
  labs(
    x = "Distance",
    y = "Mean fluorescence intensity",
    color = "Channel") +
  theme_classic()

