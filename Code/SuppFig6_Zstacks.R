# =============================================================================
# Z-Stack Fluorescence Figures: Line Profiles Paired with Cropped Micrographs
# -----------------------------------------------------------------------------
# Defines a function that pairs per-stack Red/Green fluorescence line profiles
# with cropped confocal images for a single cell, then applies it to three
# cells and exports each as a TIFF.
# =============================================================================

library(tidyverse)
library(patchwork)
library(magick)


# =============================================================================
# PART 1: Per-Cell Plotting Function
# =============================================================================
# For one cell directory, reads Red/Green fluorescence CSVs across z-stacks,
# crops the matching images, and stacks profile+image rows into one figure.

make_cell_plot <- function(cell_dir, crop_string, out_name, n_stacks = 6) {
  
  ## ---- Read fluorescence data ----
  # Build a table of Red/Green CSV paths for each z-stack slice.
  files <- tibble(
    stack   = rep(1:n_stacks, each = 2),
    channel = rep(c("Red", "Green"), times = n_stacks),
    file = c(rbind(
      file.path(cell_dir, paste0("Red", 1:n_stacks, ".csv")),
      file.path(cell_dir, paste0("Green", 1:n_stacks, ".csv"))
    ))
  )
  
  # Read each CSV, rename the first column to Distance, and unnest to long form.
  fluor <- files %>%
    rowwise() %>%
    mutate(data = list(read_csv(file, show_col_types = FALSE) %>%
                         dplyr::rename(Distance = 1))) %>%
    ungroup() %>%
    select(-file) %>%
    unnest(cols = data)
  
  # Shared y-range so all stacks are on a common intensity scale.
  y_limits <- range(fluor$Gray_Value, na.rm = TRUE)
  y_limits
  
  ## ---- Crop images ----
  # One JPG per z-stack slice, cropped to the supplied region.
  image_files <- file.path(cell_dir, paste0(1:n_stacks, ".jpg"))
  
  cropped_imgs <- map(image_files, ~
                        image_read(.x) %>% image_crop(crop_string))
  
  ## ---- Build plots ----
  # For each slice: a fluorescence line plot beside its cropped image.
  plot_list <- map(1:n_stacks, function(i) {
    
    p <- fluor %>%
      filter(stack == i) %>%
      ggplot(aes(Distance, Gray_Value, color = channel)) +
      geom_line(linewidth = 0.5) +
      scale_color_manual(values = c(Green = "green3", Red = "red3")) +
      scale_y_continuous(limits = y_limits) +
      labs(title = paste(i), x = "", y = "") +
      theme_classic() +
      theme(legend.position = "none")
    
    img_grob <- grid::rasterGrob(cropped_imgs[[i]], interpolate = TRUE)
    
    img_plot <- ggplot() +
      annotation_custom(img_grob,
                        xmin = -Inf, xmax = Inf,
                        ymin = -Inf, ymax = Inf) +
      theme_void()
    
    p + img_plot + plot_layout(widths = c(1, 1))
  })
  
  ## ---- Combine + save ----
  # Stack all slice rows into a single vertical figure.
  final_plot <- wrap_plots(plot_list, ncol = 1)
  
  return(final_plot)
}


# =============================================================================
# PART 2: Build and Export Figures for Each Cell
# =============================================================================

a <- make_cell_plot(cell_dir   = "Data/Zstackfluorescence/Chui3",
                    crop_string = "330x330+850+620", out_name   = "Cell3_fluorescence.png")
b <- make_cell_plot(cell_dir   = "Data/Zstackfluorescence/Chui1",
                    crop_string = "330x330+1050+950", out_name   = "Cell1_fluorescence.png")
c <- make_cell_plot(cell_dir   = "Data/Zstackfluorescence/Tetraselmis1",
                    crop_string = "330x330+575+550", out_name   = "zstack_fluorescence.png")

ggsave("Figures/Zstack/Chui3.tiff",        plot = a, width = 3.5, height = 10.3, units = "in", dpi = 300)
ggsave("Figures/Zstack/Chui1.tiff",        plot = b, width = 3.5, height = 10.3, units = "in", dpi = 300)
ggsave("Figures/Zstack/Tetraselmis1.tiff", plot = c, width = 3.5, height = 10.3, units = "in", dpi = 300)


# =============================================================================
# PART 3: Crop-String Helper (for determining crop regions)
# =============================================================================
# Uncomment and adjust to preview a crop region before setting crop_string above.
# crop_string <- "700x700+846+622"  # change this
# image_read("1.jpg") %>% image_crop(crop_string)