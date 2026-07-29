# =============================================================================
# FLP Flow Cytometry Gating Scheme: Single-Sample Cytogram Panels
# -----------------------------------------------------------------------------
# Loads one NES FLP sample, gates cells into functional populations (syn, pico,
# nano, mixo, beads), and builds a multi-panel cytogram figure illustrating the
# gating scheme across fluorescence/scatter channel pairs.
# =============================================================================

library(flowCore)
library(ggcyto)
library(ggplot2)
library(patchwork)
library(dplyr)
library(grid)


# =============================================================================
# PART 1: Load Example Cytogram
# =============================================================================

# ---- Read a single FLP FCS file and convert to a data frame ----
fcsfile <- read.FCS("~/Desktop/FCMdata/ZenodoData/CruiseFCSfiles/NES/FLP_fcm.fcs", emptyValue = FALSE)
df <- as.data.frame(exprs(fcsfile))


# =============================================================================
# PART 2: Define Population Gates
# =============================================================================

# ---- Synechococcus: high yellow, low FSC ----
gate_syn <- function(d) {
  d[["YEL-B-HLin"]] > 100 &
    d[["FSC-HLin"]] > 1 & d[["FSC-HLin"]] < 100
}

# ---- Picoeukaryotes: moderate yellow, moderate red ----
gate_pico <- function(d) {
  d[["YEL-B-HLin"]] > 10   & d[["YEL-B-HLin"]] < 100 &
    d[["RED-B-HLin"]] > 10 & d[["RED-B-HLin"]] < 100
}

# ---- Mixotrophs: elevated green with high red ----
gate_mixo <- function(d) {
  d[["GRN-B-HLin"]] > 50 & d[["GRN-B-HLin"]] < 100000 &
    d[["RED-B-HLin"]] > 100
}

# ---- Nanoeukaryotes: low green, high red ----
gate_nano <- function(d) {
  d[["GRN-B-HLin"]] < 200 &
    d[["RED-B-HLin"]] > 100
}

# ---- FLP beads: elevated green, low FSC ----
gate_beads <- function(d) {
  d[["GRN-B-HLin"]] > 10   &
    d[["FSC-HLin"]] > 1 & d[["FSC-HLin"]] < 100
}


# =============================================================================
# PART 3: Assign Populations
# =============================================================================
# Gates are applied in order; later assignments overwrite earlier ones, so the
# ordering encodes population priority.

assign_population <- function(d) {
  pop <- rep("other", nrow(d))
  pop[gate_nano(d)]   <- "nano"
  pop[gate_mixo(d)]   <- "mixo"
  pop[gate_pico(d)]   <- "pico"
  pop[gate_syn(d)]    <- "syn"
  pop[gate_beads(d)]  <- "beads"
  factor(pop, levels = c("other", "nano", "mixo", "pico", "syn", "beads"))
}

df$population <- assign_population(df)


# =============================================================================
# PART 4: Aesthetic Mappings and Shared Theme
# =============================================================================

# ---- Per-population color / alpha / point-size scales ----
pop_colors <- c("other" = "black",
                "nano" = "#4472C4", "beads" = "#70AD47", "pico" = "#C00000",
                "syn" = "#FF6600", "mixo" = "#CC79A7")
pop_alpha <- c("other" = 0.15,
               "nano" = 0.6, "mixo" = 1, "pico" = 0.6, "syn" = 0.7, "beads" = 0.7)
pop_size <- c("other" = 0.3,
              "nano" = 0.5, "mixo" = 0.8, "pico" = 0.5, "syn" = 0.6, "beads" = 0.7)

# ---- Shared cytogram theme ----
theme_cyto <- function() {
  theme_bw(base_size = 11) +
    theme(
      panel.grid       = element_blank(),
      strip.background = element_rect(fill = "grey90"),
      legend.position  = "none",
      plot.title       = element_text(size = 10, face = "bold")
    )
}


# =============================================================================
# PART 5: Cytogram Plot Functions
# =============================================================================
# One function per channel-pair view, each with its gate box/line and label.

# ---- Picoeukaryotes: yellow vs. red ----
p_yelred <- function(data, title) {
  ggplot(data, aes(`YEL-B-HLin`, `RED-B-HLin`,
                   color = population, alpha = population, size = population)) +
    geom_point(shape = 16, stroke = 0) +
    scale_color_manual(values = pop_colors) +
    scale_alpha_manual(values = pop_alpha) +
    scale_size_manual(values = pop_size) +
    annotate("rect", xmin = 10,   xmax = 100, ymin = 10,  ymax = 100,
             color = "black", fill = NA, linewidth = 0.5) +
    annotate("text", x = 50, y = 150, label = "picoeukaryotes", color = "black", size = 3) +
    labs(title = title, x = "Rel. Yellow Fluorescence (488: 575/25)", y = "Rel. Red Fluorescence (488: 695/50)") +
    scale_x_log10() + scale_y_log10() +
    theme_cyto()
}

# ---- Synechococcus: FSC vs. yellow ----
p_fscsyn <- function(data, title) {
  ggplot(data, aes(`FSC-HLin`, `YEL-B-HLin`,
                   color = population, alpha = population, size = population)) +
    geom_point(shape = 16, stroke = 0) +
    scale_color_manual(values = pop_colors) +
    scale_alpha_manual(values = pop_alpha) +
    scale_size_manual(values = pop_size) +
    annotate("rect", xmin = 1,  xmax = 100, ymin = 90,  ymax = 3000,
             color = "black", fill = NA, linewidth = 0.5) +
    annotate("text", x = 50, y = 10000, label = "synechococcus", color = "black", size = 3) +
    labs(title = title, x = "Forward Scatter (FSC)", y = "Rel. Yellow Fluorescence (488: 575/25)") +
    scale_x_log10() + scale_y_log10(limits = c(1, 100000)) +
    theme_cyto()
}

# ---- Phototrophs overview: SSC vs. red ----
p_nano <- function(data, title) {
  ggplot(data, aes(`SSC-HLin`, `RED-B-HLin`,
                   color = population, alpha = population, size = population)) +
    geom_point(shape = 16, stroke = 0) +
    scale_color_manual(values = pop_colors) +
    scale_alpha_manual(values = pop_alpha) +
    scale_size_manual(values = pop_size) +
    annotate("rect", xmin = 6,  xmax = 100000, ymin = 100,  ymax = 100000,
             color = "black", fill = NA, linewidth = 0.5) +
    annotate("text", x = 1000,    y = 80000, label = "phototrophs", color = "black", size = 3) +
    labs(title = title, x = "Side Scatter (SSC)", y = "Rel. Red Fluorescence (488: 695/50)") +
    scale_x_log10() + scale_y_log10() +
    theme_cyto()
}

# ---- Mixotrophs: green vs. red, with green gate line ----
p_mixo <- function(data, title) {
  ggplot(data, aes(`GRN-B-HLin`, `RED-B-HLin`,
                   color = population, alpha = population, size = population)) +
    geom_point(shape = 16, stroke = 0) +
    scale_color_manual(values = pop_colors) +
    scale_alpha_manual(values = pop_alpha) +
    scale_size_manual(values = pop_size) +
    geom_vline(xintercept = 50, color = "black", linewidth = 0.4) +
    annotate("text", x = 1000, y = 80000, label = "mixotrophs",  color = "black", size = 3) +
    labs(title = title, x = "Rel. Green Fluorescence (488: 512/18)", y = "Rel. Red Fluorescence (488: 695/50)") +
    scale_x_log10() + scale_y_log10() +
    theme_cyto()
}

# ---- FLP beads: FSC vs. green ----
p_bead <- function(data, title) {
  ggplot(data, aes(`FSC-HLin`, `GRN-B-HLin`,
                   color = population, alpha = population, size = population)) +
    geom_point(shape = 16, stroke = 0) +
    scale_color_manual(values = pop_colors) +
    scale_alpha_manual(values = pop_alpha) +
    scale_size_manual(values = pop_size) +
    annotate("rect", xmin = 1, xmax = 100, ymin = 10, ymax = 1000,
             color = "black", fill = NA, linewidth = 0.5) +
    annotate("text", x = 50, y = 2000, label = "beads", color = "black", size = 3) +
    labs(title = title, x = "Forward Scatter (FSC)", y = "Rel. Green Fluorescence (488: 512/18)") +
    scale_x_log10() + scale_y_log10() +
    theme_cyto()
}


# =============================================================================
# PART 6: Assemble Final Figure
# =============================================================================
# Panel e) is restricted to nano/mixo points to isolate the phototroph subset.

final_figure <- (
  p_yelred(df, "a)") | p_fscsyn(df, "b)") | p_bead(df, "c)")) /
  (p_nano(df, "d)") | p_mixo(df %>% dplyr::filter(population %in% c("nano", "mixo")), "e) Subset to only phototrophs"))

final_figure

ggsave("Figures/SuppFig9_FLP_FCM.png", final_figure, width = 8, height = 8)

