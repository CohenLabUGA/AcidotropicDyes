# =============================================================================
# Supplemental Figure 2: Field Flow Cytometry Cytograms (CCS + NES)
# -----------------------------------------------------------------------------
# Loads paired stained/control FCS files from two cruises, gates cells into
# functional populations (syn, pico, nano, mixo, heterotrophs), and builds
# multi-panel cytogram blocks for each cruise before combining them into one
# figure. 
# =============================================================================

library(flowCore)
library(ggcyto)
library(ggplot2)
library(patchwork)
library(dplyr)
library(grid)


# =============================================================================
# PART 1: Load Example Cytograms
# =============================================================================

# ---- Paired stained (LysoTracker) and control FCS files for CCS and NES ----
stained      <- read.FCS("~/Desktop/FCMdata/ZenodoData/CruiseFCSfiles/CCS/Stn9/Stn09-1.fcs", emptyValue = FALSE)
unstained    <- read.FCS("~/Desktop/FCMdata/ZenodoData/CruiseFCSfiles/CCS/Stn9/Stn09-9.fcs", emptyValue = FALSE)
nesstained   <- read.FCS("~/Desktop/FCMdata/ZenodoData/CruiseFCSfiles/NES/L1/L1-3.fcs",      emptyValue = FALSE)
nesunstained <- read.FCS("~/Desktop/FCMdata/ZenodoData/CruiseFCSfiles/NES/L1/L1-4.fcs",      emptyValue = FALSE)

# ---- Convert each FCS to a data frame and tag stained vs. control ----
df_s <- as.data.frame(exprs(stained));   df_s$sample <- "LysoTracker"
df_u <- as.data.frame(exprs(unstained)); df_u$sample <- "Control"
df   <- rbind(df_s, df_u)

nes_s <- as.data.frame(exprs(nesstained));   nes_s$sample <- "LysoTracker"
nes_u <- as.data.frame(exprs(nesunstained)); nes_u$sample <- "Control"
nes_df <- rbind(nes_s, nes_u)


# =============================================================================
# PART 2: Define Population Gates
# =============================================================================
# CCS and NES use separate thresholds where population boundaries differ.

# ---- Synechococcus: high yellow, low FSC ----
gate_syn <- function(d) {
  d[["YEL-B-HLin"]] > 500 &
    d[["FSC-HLin"]] > 1 & d[["FSC-HLin"]] < 100
}
gate_syn_nes <- function(d) {
  d[["YEL-B-HLin"]] > 450 &
    d[["FSC-HLin"]] > 0 & d[["FSC-HLin"]] < 100
}

# ---- Picoeukaryotes: moderate yellow, moderate red ----
gate_pico <- function(d) {
  d[["YEL-B-HLin"]] > 1   & d[["YEL-B-HLin"]] < 500 &
    d[["RED-B-HLin"]] > 35 & d[["RED-B-HLin"]] < 200
}
gate_pico_nes <- function(d) {
  d[["YEL-B-HLin"]] > 10   & d[["YEL-B-HLin"]] < 500 &
    d[["RED-B-HLin"]] > 100 & d[["RED-B-HLin"]] < 1000
}

# ---- Mixotrophic nanoeukaryotes: high green (LysoTracker) + high red ----
gate_mixo <- function(d) {
  d[["GRN-B-HLin"]] > 500 &
    d[["RED-B-HLin"]] > 200
}

# ---- Nanoeukaryotes: low green, high red ----
gate_nano <- function(d) {
  d[["GRN-B-HLin"]] < 500 &
    d[["RED-B-HLin"]] > 200
}

# ---- Heterotrophs: high green, low red, large FSC ----
gate_hetero <- function(d) {
  d[["GRN-B-HLin"]] > 500 &
    d[["RED-B-HLin"]] < 200 &
    d[["FSC-HLin"]]   > 100
}


# =============================================================================
# PART 3: Assign Populations
# =============================================================================
# Gates are applied in order; later assignments overwrite earlier ones, so the
# ordering encodes population priority.

assign_population <- function(d) {
  pop <- rep("other", nrow(d))
  pop[gate_hetero(d)] <- "heterotrophs"
  pop[gate_nano(d)]   <- "nano"
  pop[gate_mixo(d)]   <- "mixo"
  pop[gate_pico(d)]   <- "pico"
  pop[gate_syn(d)]    <- "syn"
  factor(pop, levels = c("other", "nano", "mixo", "pico", "syn", "heterotrophs"))
}

assign_population_nes <- function(d) {
  pop <- rep("other", nrow(d))
  pop[gate_hetero(d)]    <- "heterotrophs"
  pop[gate_nano(d)]      <- "nano"
  pop[gate_mixo(d)]      <- "mixo"
  pop[gate_pico_nes(d)]  <- "pico"
  pop[gate_syn_nes(d)]   <- "syn"
  factor(pop, levels = c("other", "nano", "mixo", "pico", "syn", "heterotrophs"))
}

df$population     <- assign_population(df)
nes_df$population <- assign_population_nes(nes_df)


# =============================================================================
# PART 4: Split into Stained / Control Subsets
# =============================================================================
# Sorted by population so higher-priority points draw on top.

s <- df %>%
  dplyr::filter(sample == "LysoTracker") %>%
  dplyr::arrange(population)
u <- df %>%
  dplyr::filter(sample == "Control") %>%
  dplyr::arrange(population)
nes_u <- nes_df %>%
  dplyr::filter(sample == "Control") %>%
  dplyr::arrange(population)
nes_s <- nes_df %>%
  dplyr::filter(sample == "LysoTracker") %>%
  dplyr::arrange(population)


# =============================================================================
# PART 5: Aesthetic Mappings and Shared Theme
# =============================================================================

# ---- Per-population color / alpha / point-size scales ----
pop_colors <- c("other" = "black",
                "nano" = "#4472C4", "mixo" = "#70AD47", "pico" = "#C00000",
                "syn" = "#FF6600", "heterotrophs" = "purple")
pop_alpha <- c("other" = 0.15,
               "nano" = 0.6, "mixo" = 0.6, "pico" = 0.6, "syn" = 0.7, "heterotrophs" = 0.6)
pop_size <- c("other" = 0.3,
              "nano" = 0.5, "mixo" = 0.5, "pico" = 0.5, "syn" = 0.6, "heterotrophs" = 0.5)

# ---- Shared cytogram theme ----
theme_cyto <- function() {
  theme_bw(base_size = 11) +
    theme(
      panel.grid       = element_blank(),
      strip.background = element_rect(fill = "grey90"),
      legend.position  = "none",
      element_text(size = 8),
      plot.title       = element_text(size = 10, face = "bold")
    )
}

# ---- Shared axis limits so stained and control panels are directly comparable ----
ccs_fsc_lim <- c(1, 1e5); ccs_red_lim <- c(1, 1e5)
ccs_yel_lim <- c(1, 1e5); ccs_grn_lim <- c(1, 2e5)

nes_fsc_lim <- c(1, 1e5); nes_red_lim <- c(1, 1e5)
nes_yel_lim <- c(1, 1e5); nes_grn_lim <- c(1, 2e5)


# =============================================================================
# PART 6: Cytogram Plot Functions
# =============================================================================
# One function per gate view; CCS and NES variants differ only in gate boxes
# and axis limits.

# ---- Picoeukaryotes: yellow vs. red ----
p_yelred <- function(data, title, xlim = ccs_yel_lim, ylim = ccs_red_lim) {
  ggplot(data, aes(`YEL-B-HLin`, `RED-B-HLin`,
                   color = population, alpha = population, size = population)) +
    geom_point(shape = 16, stroke = 0) +
    scale_color_manual(values = pop_colors) +
    scale_alpha_manual(values = pop_alpha) +
    scale_size_manual(values = pop_size) +
    annotate("rect", xmin = 5,   xmax = 500, ymin = 35,  ymax = 200,
             color = "black", fill = NA, linewidth = 0.5) +
    annotate("text", x = 100, y = 300, label = "picoeukaryotes", color = "black", size = 3, face = "bold") +
    labs(title = title, x = "Rel. Yellow Fluorescence (488: 575/25)", y = "Rel. Red Fluorescence (488: 695/50)") +
    scale_x_log10(limits = xlim) + scale_y_log10(limits = ylim) +
    theme_cyto()
}

nes_p_yelred <- function(data, title, xlim = nes_yel_lim, ylim = nes_red_lim) {
  ggplot(data, aes(`YEL-B-HLin`, `RED-B-HLin`,
                   color = population, alpha = population, size = population)) +
    geom_point(shape = 16, stroke = 0) +
    scale_color_manual(values = pop_colors) +
    scale_alpha_manual(values = pop_alpha) +
    scale_size_manual(values = pop_size) +
    annotate("rect", xmin = 10,  xmax = 500, ymin = 100, ymax = 1000,
             color = "black", fill = NA, linewidth = 0.5) +
    annotate("text", x = 70, y = 1500, label = "picoeukaryotes", color = "black", size = 3) +
    labs(title = title, x = "Rel. Yellow Fluorescence (488: 575/25)", y = "Rel. Red Fluorescence (488: 695/50)") +
    scale_x_log10(limits = xlim) + scale_y_log10(limits = ylim) +
    theme_cyto()
}

# ---- Synechococcus: FSC vs. yellow ----
p_fscsyn <- function(data, title, xlim = ccs_fsc_lim, ylim = ccs_yel_lim) {
  ggplot(data, aes(`FSC-HLin`, `YEL-B-HLin`,
                   color = population, alpha = population, size = population)) +
    geom_point(shape = 16, stroke = 0) +
    scale_color_manual(values = pop_colors) +
    scale_alpha_manual(values = pop_alpha) +
    scale_size_manual(values = pop_size) +
    annotate("rect", xmin = 1,  xmax = 100, ymin = 700,  ymax = 6000,
             color = "black", fill = NA, linewidth = 0.5) +
    annotate("text", x = 50, y = 10000, label = "synechococcus", color = "black", size = 3) +
    labs(title = title, x = "Forward Scatter (FSC)", y = "Rel. Yellow Fluorescence (488: 575/25)") +
    scale_x_log10(limits = xlim) + scale_y_log10(limits = ylim) +
    theme_cyto()
}

nes_p_fscsyn <- function(data, title, xlim = nes_fsc_lim, ylim = nes_yel_lim) {
  ggplot(data, aes(`FSC-HLin`, `YEL-B-HLin`,
                   color = population, alpha = population, size = population)) +
    geom_point(shape = 16, stroke = 0) +
    scale_color_manual(values = pop_colors) +
    scale_alpha_manual(values = pop_alpha) +
    scale_size_manual(values = pop_size) +
    annotate("rect", xmin = 0,  xmax = 100, ymin = 480,  ymax = 9000,
             color = "black", fill = NA, linewidth = 0.5) +
    annotate("text", x = 50, y = 15000, label = "synechococcus", color = "black", size = 3) +
    labs(title = title, x = "Forward Scatter (FSC)", y = "Rel. Yellow Fluorescence (488: 575/25)") +
    scale_x_log10(limits = xlim) + scale_y_log10(limits = ylim) +
    theme_cyto()
}

# ---- Nanoeukaryotes / mixotrophs: green vs. red (gate lines instead of a box) ----
p_mixo <- function(data, title, xlim = ccs_grn_lim, ylim = ccs_red_lim) {
  ggplot(data, aes(`GRN-B-HLin`, `RED-B-HLin`,
                   color = population, alpha = population, size = population)) +
    geom_point(shape = 16, stroke = 0) +
    scale_color_manual(values = pop_colors) +
    scale_alpha_manual(values = pop_alpha) +
    scale_size_manual(values = pop_size) +
    geom_hline(yintercept = 200, color = "black", linewidth = 0.4) +
    geom_vline(xintercept = 500, color = "black", linewidth = 0.4) +
    annotate("text", x = 15,    y = 80000, label = "nanoeukaryotes", color = "black", size = 3) +
    annotate("text", x = 10000, y = 80000, label = "mixotrophic nanoeukaryotes",  color = "black", size = 3) +
    labs(title = title, x = "Rel. Green Fluroescence (488: 512/18)", y = "Rel. Red Fluorescence (488: 695/50)") +
    scale_x_log10(limits = xlim) + scale_y_log10(limits = ylim) +
    theme_cyto()
}

nes_p_mixo <- function(data, title, xlim = nes_grn_lim, ylim = nes_red_lim) {
  ggplot(data, aes(`GRN-B-HLin`, `RED-B-HLin`,
                   color = population, alpha = population, size = population)) +
    geom_point(shape = 16, stroke = 0) +
    scale_color_manual(values = pop_colors) +
    scale_alpha_manual(values = pop_alpha) +
    scale_size_manual(values = pop_size) +
    geom_hline(yintercept = 1000, color = "black", linewidth = 0.4) +
    geom_vline(xintercept = 500,  color = "black", linewidth = 0.4) +
    annotate("text", x = 15,    y = 80000, label = "nanoeukaryotes", color = "black", size = 3) +
    annotate("text", x = 10000, y = 80000, label = "mixotrophic nanoeukaryotes",  color = "black", size = 3) +
    labs(title = title, x = "Rel. Green Fluroescence (488: 512/18)", y = "Rel. Red Fluorescence (488: 695/50") +
    scale_x_log10(limits = xlim) + scale_y_log10(limits = ylim) +
    theme_cyto()
}

# ---- Heterotrophs: FSC vs. green ----
p_hetero <- function(data, title, xlim = ccs_fsc_lim, ylim = ccs_grn_lim) {
  ggplot(data, aes(`FSC-HLin`, `GRN-B-HLin`,
                   color = population, alpha = population, size = population)) +
    geom_point(shape = 16, stroke = 0) +
    scale_color_manual(values = pop_colors) +
    scale_alpha_manual(values = pop_alpha) +
    scale_size_manual(values = pop_size) +
    annotate("rect", xmin = 150, xmax = 1e5, ymin = 500, ymax = 1e5,
             color = "black", fill = NA, linewidth = 0.5) +
    annotate("text", x = 1000, y = 150000, label = "heterotrophs", color = "black", size = 3) +
    labs(title = title, x = "Forward Scatter (FSC)", y = "Rel. Green Fluroescence (488: 512/18)") +
    scale_x_log10(limits = xlim) + scale_y_log10(limits = ylim) +
    theme_cyto()
}


# =============================================================================
# PART 7: Assemble Per-Cruise Blocks and Final Figure
# =============================================================================

# ---- Stack the four gate views (control/stained pairs) for each cruise ----
ccs_block <- (
  p_yelred(u, "a) Control") | p_fscsyn(u, "b) Control")) / (
    p_mixo(s, "c) LysoTracker") | p_mixo(u, "d) Control")) / (
      p_hetero(s, "e) LysoTracker") | p_hetero(u, "f) Control"))

nes_block <- (
  nes_p_yelred(nes_u, "g) Control") | nes_p_fscsyn(nes_u, "h) Control")) / (
    nes_p_mixo(nes_s, "i) LysoTracker") | nes_p_mixo(nes_u, "j) Control")) / (
      p_hetero(nes_s, "k) LysoTracker", xlim = nes_fsc_lim, ylim = nes_grn_lim) |
        p_hetero(nes_u, "l) Control", xlim = nes_fsc_lim, ylim = nes_grn_lim))

# ---- Add a cruise title above each block ----
ccs_titled <- wrap_elements(
  ccs_block +
    plot_annotation(title = "California Current System",
                    theme = theme(plot.title = element_text(size = 14, face = "bold",
                                                            hjust = 0.5, margin = margin(b = 6)))))

nes_titled <- wrap_elements(
  nes_block +
    plot_annotation(title = "Northeast U.S. Shelf",
                    theme = theme(plot.title = element_text(size = 14, face = "bold",
                                                            hjust = 0.5, margin = margin(b = 6)))))

# ---- Place both cruise blocks side by side and export ----
final_plot <- ccs_titled | nes_titled
final_plot
ggsave("Figures/SuppFig2_FieldFCM.png", final_plot, width = 18, height = 13)