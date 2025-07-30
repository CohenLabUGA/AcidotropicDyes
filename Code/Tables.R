## Load in necessary packages ##
library(dplyr)
library(readxl)
library(tidyr)
library(gt)
library(tibble)
library(glue)

#### Making of Table 1 ####
## Load in dataset for Table 1 and formate with superscripts##
Table1 <- read_excel("Data/Table1.xlsx") %>%
  mutate(
    Acquisition = case_when(
      Culture == "Gephyrocapsa oceanica (UGA06)" ~ glue("{Acquisition}<sup>4</sup>"),
      Culture == "Gephyrocapsa huxleyi (UGA13)" ~ glue("{Acquisition}<sup>4</sup>"),
      Culture == "Tetraselmis chui (PLY429)" ~ glue("{Acquisition}<sup>5</sup>"),
      Culture == "Odontella rostrata (UGA01)" ~ glue("{Acquisition}<sup>4</sup>"),
      Culture == "Chaetoceros neogracile (RS19)" ~ glue("{Acquisition}<sup>6</sup>"),
      Culture == "Geminigeria cryophila (CCMP2564)" ~ glue("{Acquisition}<sup>3</sup>"),
      Culture == "Mantoniella antarctica (SL-175)" ~ glue("{Acquisition}<sup>3</sup>"),
      Culture == "Pyramimonas tychotreta (I-9 Pyram)" ~ glue("{Acquisition}<sup>3</sup>"),
      
      TRUE ~ Acquisition)) %>%
  mutate(
    Metabolism = case_when(
      Culture == "Gephyrocapsa oceanica (UGA06)" ~ glue("{Metabolism}<sup>1</sup>"),
      Culture == "Gephyrocapsa huxleyi (UGA13)" ~ glue("{Metabolism}<sup>1</sup>"),
      Culture == "Tetraselmis sp. " ~ glue("{Metabolism}<sup>2</sup>"),
      Culture == "Tetraselmis chui (PLY429)" ~ glue("{Metabolism}<sup>2</sup>"),
      Culture == "Geminigeria cryophila (CCMP2564)" ~ glue("{Metabolism}<sup>3</sup>"),
      Culture == "Mantoniella antarctica (SL-175)" ~ glue("{Metabolism}<sup>3</sup>"),
      Culture == "Pyramimonas tychotreta (I-9 Pyram)" ~ glue("{Metabolism}<sup>3</sup>"),
      TRUE ~ Metabolism))

## Format into gt table with sources at the bottom ##
gt_table <- Table1 %>%
  gt() %>%
  fmt_markdown(columns = vars(Acquisition, Metabolism)) %>%
  cols_label(
    `Light Intensity (µmol photons meter second)` := 
      html("Light Intensity<br>(µmol photons m<sup>−2</sup> s<sup>−1</sup>)"),
    `Temperature (ºC)` := "Temperature (°C)"
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Temperature (ºC)`, `Light Intensity (µmol photons meter second)`, Metabolism)) %>%
  cols_width(
    Metabolism ~ px(150)
  ) %>%
  tab_source_note(
    source_note = html(
      "<sup>1</sup>Ye et al. (2024), Biology ; Godrijan et al. (2020), Limnology and Oceanography<br>
      <sup>2</sup>This study<br>
      <sup>3</sup>Gast et al. (2014), FEMS Microbiol Ecol<br>
       <sup>4</sup>Quirk et al. (in revision), Limnology and Oceanography<br>
     <sup>5</sup>Milford Strain Collection<br>
      <sup>6</sup>Kellogg et al. (2022), Limnology and Oceanography<br>"))

gt_table

# Save table #
gtsave(gt_table, filename = "Figures/Table1.png", vwidth = 1800, vheight = 3200, zoom = 3)
#### Making of supplemental table 1 ####
# Create data frame
supptable2 <- tibble::tibble(
  Station = c("1", "2", "4", "7", "9", "X"),
  `BioRepSUR` = c(1, 1, 2, 1, 1, 1),
  `BioRepSCM` = c(1, 1, 1, 2, 2, 1))

# Format into gt table
gt_rep_table <- supptable2 %>%
  gt() %>%
  cols_label(Station = "Station",
             `BioRepSUR` = "Biological Replicates\nSurface",
             `BioRepSCM` = "Biological Replicates\nSCM") %>%
  cols_align(align = "center")

gt_rep_table

# Save table
gtsave(gt_rep_table, filename = "Figures/SuppTable2.png")

#### Making of Supplemental Table of Flow Cytometer Values ####
data <- read_excel("Data/FlowCytometerInfo.xlsx") 

supp1table <- data %>%
  gt() %>%
  tab_spanner(label = "Gain and Voltage Settings",
    columns = c(
      `Guava Gain (Culture Tests & NES FLP)`,
      `LysoTracker CCS Cruise`,
      `LysoTracker NES Cruise`,
      `Culture Tests CytPix Voltage (V)`)) %>%
  tab_spanner(label = "Detection Wavelengths",
    columns = c(
      `Wavelength CytPix (nm)`,
      `Wavelength Guava (nm)`)) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "—") %>%
  tab_header(title = md("**Instrument Settings for Guava EasyCyte and Attune CytPix Flow Cytometers**")) %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = 16) %>%
  cols_align(align = "center")

supp1table

gtsave(supp1table, filename = "Figures/SuppTable1.png", vwidth = 1500, vheight = 3200, zoom = 3)
