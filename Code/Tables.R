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
  mutate(`CytPixSize` = gsub(" to ", " x ", `CytPixSize`)) %>%
  mutate(
    Acquisition = case_when(
      Culture == "Gephyrocapsa oceanica (UGA06)" ~ glue("{Acquisition}<sup>5</sup>"),
      Culture == "Gephyrocapsa huxleyi (UGA13)" ~ glue("{Acquisition}<sup>5</sup>"),
      Culture == "Tetraselmis chui (PLY429)" ~ glue("{Acquisition}<sup>6</sup>"),
      Culture == "Odontella rostrata (UGA01)" ~ glue("{Acquisition}<sup>5</sup>"),
      Culture == "Chaetoceros neogracile (RS19)" ~ glue("{Acquisition}<sup>7</sup>"),
      Culture == "Geminigeria cryophila (CCMP2564)" ~ glue("{Acquisition}<sup>4</sup>"),
      Culture == "Mantoniella antarctica (SL-175)" ~ glue("{Acquisition}<sup>4</sup>"),
      Culture == "Pyramimonas tychotreta (I-9 Pyram)" ~ glue("{Acquisition}<sup>4</sup>"),
      
      TRUE ~ Acquisition)) %>%
  mutate(
    Metabolism = case_when(
      Culture == "Gephyrocapsa oceanica (UGA06)" ~ glue("{Metabolism}<sup>1</sup>"),
      Culture == "Gephyrocapsa huxleyi (UGA13)" ~ glue("{Metabolism}<sup>1</sup>"),
      Culture == "Tetraselmis sp. " ~ glue("{Metabolism}<sup>2</sup>"),
      Culture == "Tetraselmis chui (PLY429)" ~ glue("{Metabolism}<sup>2</sup>"),
      Culture == "Micromonas polaris (CCMP2099)" ~ glue("{Metabolism}<sup>3</sup>"),
      Culture == "Geminigeria cryophila (CCMP2564)" ~ glue("{Metabolism}<sup>4</sup>"),
      Culture == "Mantoniella antarctica (SL-175)" ~ glue("{Metabolism}<sup>4</sup>"),
      Culture == "Pyramimonas tychotreta (I-9 Pyram)" ~ glue("{Metabolism}<sup>4</sup>"),
      TRUE ~ Metabolism))

## Format into gt table with sources at the bottom ##
gt_table <- Table1 %>%
  gt() %>%
  fmt_markdown(columns = vars(Acquisition, Metabolism, CytPixSize)) %>%
  cols_label(
    `Light Intensity (µmol photons meter second)` := 
      html("Light Intensity<br>(µmol photons m<sup>−2</sup> s<sup>−1</sup>)"),
    `Temperature (ºC)` := "Temperature (°C)", 
    `CytPixSize` := "CytPix Size (Width x Length)(µm)"
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Temperature (ºC)`, `Light Intensity (µmol photons meter second)`, Metabolism, `CytPixSize`)) %>%
  cols_width(
    Metabolism ~ px(150)
  ) %>%
  tab_source_note(
    source_note = html(
      "<sup>1</sup>Ye et al. (2024), Biology ; Godrijan et al. (2020), Limnology and Oceanography<br>
      <sup>2</sup>This study<br>
       <sup>3</sup>McKie-Krisberg & Sanders (2014), ISME; Wilken et al. (2019), Phil Trans R Soc B; Jimenez et al. (2021), J Phycol<br>
      <sup>4</sup>Gast et al. (2014), FEMS Microbiol Ecol<br>
       <sup>5</sup>Quirk et al. (in revision), Limnology and Oceanography<br>
     <sup>6</sup>Milford Strain Collection<br>
      <sup>7</sup>Kellogg et al. (2022), Limnology and Oceanography<br>
      <sup>8</sup>Simon et al. (2017), Protist<br>"))

gt_table

# Save table #
gtsave(gt_table, filename = "Figures/Table1.png", vwidth = 1800, vheight = 3200, zoom = 3)
#### Making of supplemental table 1 ####
# Create data frame
supptable3 <- tibble::tibble(
  Station = c("1", "2", "4", "7", "9", "X"),
  `BioRepSUR` = c(1, 1, 2, 1, 1, 1),
  `BioRepSCM` = c(1, 1, 1, 2, 2, 1))

# Format into gt table
gt_rep_table <- supptable3 %>%
  gt() %>%
  cols_label(Station = "Station",
             `BioRepSUR` = "Biological Replicates\nSurface",
             `BioRepSCM` = "Biological Replicates\nSCM") %>%
  cols_align(align = "center")

gt_rep_table

# Save table
gtsave(gt_rep_table, filename = "Figures/SuppTable3.png")

#### Making of Supplemental Table of Flow Cytometer Values ####
data <- read_excel("Data/FlowCytometerInfo.xlsx") 

supp2table <- data %>%
  gt() %>%
  tab_spanner(label = "Gain and Voltage Settings",
    columns = c(
      `Guava Gain - Culture Tests & NES FLP`,
      `Guava Gain - LysoTracker CCS`,
      `Guava Gain - LysoTracker NES`,
      `CytPix Voltage - Culture Tests`)) %>%
  tab_spanner(label = "Detection Wavelengths",
    columns = c(
      `CytPix Wavelength (nm)`,
      `Guava Wavelength (nm)`)) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "—") %>%
  tab_header(title = md("**Instrument Settings for Guava EasyCyte and Attune CytPix Flow Cytometers**")) %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = 16) %>%
  cols_align(align = "center")

supp2table

gtsave(supp2table, filename = "Figures/SuppTable2.png", vwidth = 1000, vheight = 3200, zoom = 3)
