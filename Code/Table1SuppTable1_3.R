# =============================================================================
# Table 1 and Supplemental Tables: Culture Metadata + Instrument Settings
# -----------------------------------------------------------------------------
# Builds Table 1 (culture growth conditions with cited superscripts), a
# biological-replicate supplemental table, and a flow cytometer settings table,
# exporting each as a PNG via gt.
# =============================================================================

# ---- Load required libraries ----
library(dplyr)
library(readxl)
library(tidyr)
library(gt)
library(tibble)
library(glue)


# =============================================================================
# PART 1: Table 1 — Culture Growth Conditions
# =============================================================================

# ---- Load data; convert size range to "W x L" and append citation superscripts ----
Table1 <- read_excel("Data/Table1.xlsx") %>%
  mutate(`CytPixSize` = gsub(" to ", " x ", `CytPixSize`)) %>%
  mutate(
    Culture = case_when(
      Culture == "Gephyrocapsa oceanica (UGA06)"        ~ glue("{Culture}<sup>2</sup>"),
      Culture == "Gephyrocapsa huxleyi (UGA13)"         ~ glue("{Culture}<sup>2</sup>"),
      Culture == "Odontella rostrata (UGA01)"           ~ glue("{Culture}<sup>2</sup>"),
      Culture == "Chaetoceros neogracile (RS19)"        ~ glue("{Culture}<sup>1</sup>"),
      Culture == "Geminigera cryophila (CCMP2564)"      ~ glue("{Culture}<sup>3</sup>"),
      Culture == "Mantoniella antarctica (SL-175)"      ~ glue("{Culture}<sup>3</sup>"),
      Culture == "Pyramimonas tychotreta (I-9 Pyram)"   ~ glue("{Culture}<sup>3</sup>"),
      TRUE ~ Culture
    )
  ) %>%
  mutate(
    Metabolism = case_when(
      Culture == "Gephyrocapsa oceanica (UGA06)"        ~ glue("{Metabolism}<sup>10</sup>"),
      Culture == "Gephyrocapsa huxleyi (UGA13)"         ~ glue("{Metabolism}<sup>10</sup>"),
      Culture == "Tetraselmis sp. "                     ~ glue("{Metabolism}<sup>6</sup>"),
      Culture == "Tetraselmis chui (PLY429)"            ~ glue("{Metabolism}<sup>6</sup>"),
      Culture == "Miromonas polaris (CCMP2099)"         ~ glue("{Metabolism}<sup>4</sup>"),
      Culture == "Geminigera cryophila (CCMP2564)"      ~ glue("{Metabolism}<sup>5</sup>"),
      Culture == "Mantoniella antarctica (SL-175)"      ~ glue("{Metabolism}<sup>5</sup>"),
      Culture == "Pyramimonas tychotreta (I-9 Pyram)"   ~ glue("{Metabolism}<sup>5</sup>"),
      Culture == "Protocentrum micans"                  ~ glue("{Metabolism}<sup>9</sup>"),
      Culture == "Akashiwo sanguinea (ARC339)"          ~ glue("{Metabolism}<sup>8</sup>"),
      Culture == "Ochromonas sp. (CCMP1393)"            ~ glue("{Metabolism}<sup>7</sup>"),
      Culture == "Ochromonas sp. (CCMP2951)"            ~ glue("{Metabolism}<sup>7</sup>"),
      TRUE ~ Metabolism
    )
  )

# ---- Render gt table: markdown superscripts, header labels, and source notes ----
gt_table <- Table1 %>%
  gt() %>%
  fmt_markdown(columns = vars(Culture, Metabolism, CytPixSize)) %>%
  cols_label(
    `Light Intensity (µmol photons meter second)` :=
      html("Light Intensity<br>(µmol photons m<sup>−2</sup> s<sup>−1</sup>)"),
    `Temperature (ºC)` := "Temperature (°C)",
    CytPixSize := html("Size (µm)<br>(Width × Length)")
  ) %>%
  cols_align(
    align = "center",
    columns = c(
      `Temperature (ºC)`,
      `Light Intensity (µmol photons meter second)`,
      Metabolism,
      CytPixSize
    )
  ) %>%
  cols_width(Metabolism ~ px(150)) %>%
  tab_source_note(source_note = html(
    "<sup>1</sup>Kellogg et al. (2022), Limnology and Oceanography<br>
      <sup>2</sup>Quirk et al. (2025), Limnology and Oceanography<br>
       <sup>3</sup>Gast et al. (2014), FEMS Microbiol Ecol<br>
      <sup>4</sup>McKie-Krisberg & Sanders (2014), ISME; Wilken et al. (2019), Phil Trans R Soc B; Jimenez et al. (2021), J Phycol<br>
       <sup>5</sup>McKie-Krisberg et al. (2015), Microb Ecol; This study<br>
       <sup>6</sup>Chiang et al. (2026), Microb Ecol; This study<br>
      <sup>7</sup>Wilken et al. (2020), J Phycol; This study<br>
       <sup>8</sup>Wu et al. (2025), Sci Total Env; This study<br>
        <sup>9</sup>Jeong et al. (2005), Mar Ecol Prog Ser; Mena et al. (2025), J Plankton Res; This study<br>
        <sup>10</sup>Ye et al. (2024), Biology; This study<br>
       <sup>*</sup>Simon et al. (2017), Protist<br>"))

gt_table

gtsave(gt_table, filename = "Figures/Table1.png", vwidth = 1800, vheight = 3200, zoom = 3)


# =============================================================================
# PART 2: Supplemental Table 1 — Flow Cytometer Settings
# =============================================================================

# ---- Load instrument settings ----
data <- read_excel("Data/FlowCytometerInfo.xlsx")

# ---- Render gt table: grouped spanners for gains/voltages and wavelengths ----
supp1table <- data %>%
  gt() %>%
  tab_spanner(
    label = "Gain and Voltage Settings",
    columns = c(
      `Guava Gain - Culture Tests & NES FLP`,
      `Guava Gain - LysoTracker CCS`,
      `Guava Gain - LysoTracker NES`,
      `CytPix Voltage - Culture Tests`
    )
  ) %>%
  tab_spanner(
    label = "Detection Wavelengths",
    columns = c(
      `CytPix Wavelength (nm)`,
      `Guava Wavelength (nm)`
    )
  ) %>%
  fmt_missing(columns = everything(), missing_text = "—") %>%
  tab_header(title = md("**Instrument Settings for Guava EasyCyte and Attune CytPix Flow Cytometers**")) %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = 16
  ) %>%
  cols_align(align = "center")

supp1table

gtsave(supp1table, filename = "Figures/SuppTable1.png", vwidth = 1000, vheight = 3200, zoom = 3)


# =============================================================================
# PART 3: Supplemental Table 3 — Biological Replicates per Station
# =============================================================================

# ---- Replicate counts by station (surface vs. SCM) ----
supptable3 <- tibble::tibble(
  Station     = c("1", "2", "4", "7", "9", "X"),
  `BioRepSUR` = c(1, 1, 2, 1, 1, 1),
  `BioRepSCM` = c(1, 1, 1, 2, 2, 1)
)

# ---- Render and export gt table ----
gt_rep_table <- supptable3 %>%
  gt() %>%
  cols_label(
    Station     = "Station",
    `BioRepSUR` = "Biological Replicates\nSurface",
    `BioRepSCM` = "Biological Replicates\nSCM"
  ) %>%
  cols_align(align = "center")

gt_rep_table

gtsave(gt_rep_table, filename = "Figures/SuppTable3.png")