# =============================================================================
# LysoTracker/LysoSensor Culture Cytograms: Four-Culture Gating Figure
# -----------------------------------------------------------------------------
# Creates a 4-panel cytogram block (LysoTracker/LysoSensor × stained/control)
# =============================================================================

library(flowCore)
library(ggcyto)
library(ggplot2)
library(patchwork)

# =============================================================================
# PART 1: Shared Theme and Gate Colors
# =============================================================================

theme_cyto <- function() {
  theme_bw(base_size = 11) +
    theme(
      panel.grid       = element_blank(),
      strip.background = element_rect(fill = "grey90"),
      legend.position  = "none",
      plot.title       = element_text(size = 10, face = "bold")
    )
}

gate_colors <- function(highlight = c("pico", "nano", "big")) {
  highlight <- match.arg(highlight)
  
  c(
    pico = if (highlight == "pico") "red" else "black",
    nano = if (highlight == "nano") "red" else "black",
    big  = if (highlight == "big")  "red" else "black"
  )
}

# =============================================================================
# PART 2: Per-Culture Plotting Function
# =============================================================================

plot_lyso_culture <- function(culture_name,
                              tracker_path,
                              sensor_path,
                              control_path,
                              highlight,
                              tracker_gates = c(pico = 500, nano = 11000, big = 60000),
                              sensor_gates  = c(pico = 300, nano = 2000, big = 11000)) {
  
  # ---------------------------------------------------------------------------
  # Load FCS files
  # ---------------------------------------------------------------------------
  
  df_tracker <- as.data.frame(exprs(read.FCS(tracker_path, emptyValue = FALSE)))
  df_sensor  <- as.data.frame(exprs(read.FCS(sensor_path, emptyValue = FALSE)))
  df_control <- as.data.frame(exprs(read.FCS(control_path, emptyValue = FALSE)))
  
  col_t <- gate_colors(highlight)
  col_s <- gate_colors(highlight)
  
  # ---------------------------------------------------------------------------
  # Helper function to add size gates
  # ---------------------------------------------------------------------------
  
  add_gates <- function(p, gates, cols) {
    p +
      geom_vline(xintercept = gates["pico"], color = cols["pico"], linewidth = 0.6) +
      geom_vline(xintercept = gates["nano"], color = cols["nano"], linewidth = 0.6) +
      geom_vline(xintercept = gates["big"],  color = cols["big"],  linewidth = 0.6)
  }
  
  # ---------------------------------------------------------------------------
  # Common y-axis for every panel in every culture
  # ---------------------------------------------------------------------------
  
  y_scale <- scale_y_log10(
    limits = c(1e3, 1e6),
    breaks = c(1e3, 1e4, 1e5, 1e6)
  )
  
  # ---------------------------------------------------------------------------
  # LysoTracker stained
  # ---------------------------------------------------------------------------
  
  p_tracker_stained <- add_gates(
    ggplot(df_tracker, aes(`BL1-H`, `BL3-H`)) +
      geom_point(shape = 16, stroke = 0, alpha = 0.8) +
      labs(
        title = "a) LysoTracker Stained",
        x = "Rel. Green Fluorescence (488: 530/30)",
        y = "Rel. Red Fluorescence (488: 695/40)"
      ) +
      theme_cyto() +
      scale_x_log10(limits = c(10, 2000000)) +
      y_scale,
    tracker_gates,
    col_t
  )
  
  # ---------------------------------------------------------------------------
  # LysoSensor stained
  # ---------------------------------------------------------------------------
  
  p_sensor_stained <- add_gates(
    ggplot(df_sensor, aes(`VL1-H`, `BL3-H`)) +
      geom_point(shape = 16, stroke = 0, alpha = 0.8) +
      labs(
        title = "b) LysoSensor Stained",
        x = "Rel. Violet Fluorescence (405: 440/50)",
        y = "Rel. Red Fluorescence (488: 695/40)"
      ) +
      theme_cyto() +
      scale_x_log10(limits = c(10, 100000)) +
      y_scale,
    sensor_gates,
    col_s
  )
  
  # ---------------------------------------------------------------------------
  # LysoTracker control
  # ---------------------------------------------------------------------------
  
  p_tracker_control <- add_gates(
    ggplot(df_control, aes(`BL1-H`, `BL3-H`)) +
      geom_point(shape = 16, stroke = 0, alpha = 0.6) +
      labs(
        title = "c) LysoTracker Control",
        x = "Rel. Green Fluorescence (488: 530/30)",
        y = "Rel. Red Fluorescence (488: 695/40)"
      ) +
      theme_cyto() +
      scale_x_log10(limits = c(10, 2000000)) +
      y_scale,
    tracker_gates,
    col_t
  )
  
  # ---------------------------------------------------------------------------
  # LysoSensor control
  # ---------------------------------------------------------------------------
  
  p_sensor_control <- add_gates(
    ggplot(df_control, aes(`VL1-H`, `BL3-H`)) +
      geom_point(shape = 16, stroke = 0, alpha = 0.8) +
      labs(
        title = "d) LysoSensor Control",
        x = "Rel. Violet Fluorescence (405: 440/50)",
        y = "Rel. Red Fluorescence (488: 695/40)"
      ) +
      theme_cyto() +
      scale_x_log10(limits = c(10, 100000)) +
      y_scale,
    sensor_gates,
    col_s
  )
  
  # ---------------------------------------------------------------------------
  # Assemble four-panel figure
  # ---------------------------------------------------------------------------
  
  wrap_elements(
    (p_tracker_stained | p_sensor_stained) /
      (p_tracker_control | p_sensor_control) +
      plot_annotation(
        title = culture_name,
        theme = theme(
          plot.title = element_text(
            size = 14,
            face = "bold.italic",
            hjust = 0.5,
            margin = margin(b = 6)
          )
        )
      )
  )
}

# =============================================================================
# PART 3: Culture Definitions and Figure Assembly
# =============================================================================

cultures <- list(
  list(
    name = "Micromonas polaris",
    folder = "20260114_micromonas",
    highlight = "pico"
  ),
  list(
    name = "Odontella sp.",
    folder = "20260114_UNC2314",
    highlight = "big"
  ),
  list(
    name = "Tetraselmis chui",
    folder = "20260114_chui",
    highlight = "nano"
  ),
  list(
    name = "Chlamydomonas sp.",
    folder = "20260109_chlamydomonas",
    highlight = "nano"
  ), 
  list(
    name="Ochromonas sp. 2951", 
    folder="20260520_ochromonas2951", 
    highlight="nano"
  ), 
  list(
    name="Akashiwo sanguinea", 
    folder="20260126_akashiwo", 
    highlight="big"
))

base_dir <- "~/Desktop/FCMdata/ZenodoData/CultureFCSfiles"

plots <- lapply(cultures, function(x) {
  plot_lyso_culture(
    culture_name = x$name,
    tracker_path = file.path(base_dir, x$folder, "TrackerA.fcs"),
    sensor_path  = file.path(base_dir, x$folder, "SensorA.fcs"),
    control_path = file.path(base_dir, x$folder, "ControlA.fcs"),
    highlight    = x$highlight
  )
})

final_plot <- ((plots[[1]] | plots[[2]]  | plots[[6]]) /
  (plots[[3]] | plots[[4]] | plots[[5]]))

final_plot

ggsave("Figures/SuppFig3_CultureFCM.png",final_plot,width = 20, height = 13, dpi = 300)
