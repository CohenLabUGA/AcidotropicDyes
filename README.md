# Evaluating acidotropic dyes for detecting mixotrophy in protists: insights from cultures and field communities

Data and analysis code for Cook et al., *Limnology and Oceanography: Methods*.

## Project background

Mixotrophic protists combine photosynthesis with phagotrophic consumption of prey within a single cell, and are important for nutrient cycling and microbial food web structure. These organisms are difficult to detect in natural communities. This study evaluates acidotropic dyes — LysoTracker Green DND-26 and LysoSensor Blue DND-167 — which accumulate in acidic vacuoles and can be detected by flow cytometry.

The study has two parts. First, both dyes were tested against 22 laboratory cultures spanning known photoautotrophs and confirmed mixotrophs, with fluorescently labeled particle (FLP) incubations run in parallel to independently confirm ingestion. Second, LysoTracker staining and FLP-derived cell specific grazing rates (CSGR) were compared across two contrasting systems — the Northeast U.S. Shelf (NES) and the California Current System (CCS) — plus a shipboard incubation of CCS water that produced a diatom-dominated bloom.

## Repository layout

| Path | Contents |
|---|---|
| [`AcidotropicManuscript.Rproj`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/AcidotropicManuscript.Rproj) | Open this first; all paths are project-relative |
| [`Code/`](https://github.com/CohenLabUGA/AcidotropicDyes/tree/main/Code) | Analysis and figure-generation scripts |
| [`Data/`](https://github.com/CohenLabUGA/AcidotropicDyes/tree/main/Data) | Raw and processed data |
| [`Data/1_DataREADME.md`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/1_DataREADME.md) | Column-by-column descriptions and units |
| [`Data/Photos/`](https://github.com/CohenLabUGA/AcidotropicDyes/tree/main/Data/Photos) | Attune CytPix images used in Figure 3c,d |
| [`Data/Zstackfluorescence/`](https://github.com/CohenLabUGA/AcidotropicDyes/tree/main/Data/Zstackfluorescence) | *Tetraselmis* z-stack images and intensity profiles |
| [`Figures/`](https://github.com/CohenLabUGA/AcidotropicDyes/tree/main/Figures) | Script outputs |

Raw flow cytometry `.fcs` files and the full community composition image sets are archived on Zenodo: **https://doi.org/10.5281/zenodo.16813438**

## Getting started

1. Open [`AcidotropicManuscript.Rproj`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/AcidotropicManuscript.Rproj) in RStudio. Every script reads with paths relative to the project root (`Data/...`) and writes to `Figures/...`, so scripts will fail if run outside the project.
2. To reproduce the four scripts that read `.fcs` files directly, download the Zenodo archive and update the `~/Desktop/FCMdata/ZenodoData/` paths at the top of each — see [FCS-dependent scripts](#fcs-dependent-scripts).
3. Run the processing scripts before the figure scripts — see [Run order](#run-order).

Analyses were run in R 4.3.1 (RStudio 2024.12.1.563). Beyond the tidyverse, scripts draw on `ggplot2`, `patchwork`, `cowplot`, `gt`, `ggpattern`, `ggpubr`, `ggpmisc`, `RColorBrewer`, `gtools`, `gridExtra`, `flowCore`, `ggcyto`, `magick`, `marmap`, `rnaturalearth`, `sf`, and `ggrepel`.

## Run order

Several scripts consume files written by other scripts. Run them in this order.

**1. Process raw field flow cytometry**

| Script | Reads | Writes |
|---|---|---|
| [`CCS_LysoTrackerProcess.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/CCS_LysoTrackerProcess.R) | [`20241203_CCSLysoTrackerRaw.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241203_CCSLysoTrackerRaw.xlsx) | [`20241203_CCSLysoTrackerProcessed.csv`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241203_CCSLysoTrackerProcessed.csv) |
| [`NES_LysoTracker_Process.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/NES_LysoTracker_Process.R) | [`20241205_NESLysoTrackerRaw.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241205_NESLysoTrackerRaw.xlsx) | [`20241205_NESLysoTrackerProcessed.csv`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241205_NESLysoTrackerProcessed.csv) |

**2. Merge cruises**

[`SuppFig12_CommunityDepth.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig12_CommunityDepth.R) reads both processed CSVs and writes [`AllCruiseLysoTracker.csv`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/AllCruiseLysoTracker.csv) in addition to plotting Supplemental Figure 12. That merged file is required by Figure 3 and both statistics scripts, so this script must run even if you only want the figures.

**3. Process FLP incubations**

[`FLPDataWrangling.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/FLPDataWrangling.R) reads the NES flow cytometry, NES microscopy, CCS microscopy, and bacterial abundance files, and writes [`NES_FLP_Processed.csv`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NES_FLP_Processed.csv) and [`CCS_FLP_Processed.csv`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CCS_FLP_Processed.csv).

> [`AllFLPData.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/AllFLPData.xlsx) was assembled manually from the two processed FLP CSVs and is committed directly. It is not regenerated by any script.

**4. Figures, tables, and statistics** — these can then be run in any order.

## Figures and tables

### Main text

| Output | Script | Key inputs |
|---|---|---|
| [Table 1](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/Table1.png) | [`Table1SuppTable2-3.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Table1SuppTable2-3.R) | [`Table1.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/Table1.xlsx) |
| [Figure 1](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/Figure1.tiff) · [legend](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/Figure1_Legend.tiff) | [`Figure1_BarPlot.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Figure1_BarPlot.R) | [`CultureLysoData.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CultureLysoData.xlsx), [`CultureFLP.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CultureFLP.xlsx) |
| [Figure 2](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/Figure2.tiff) | [`Figure2_FluorescenceChanges.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Figure2_FluorescenceChanges.R) | Culture `.fcs` files (Zenodo), [`CultureLysoData.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CultureLysoData.xlsx) |
| [Figure 3ab](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/Figure3ab.tiff) · [3c](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/Figure3c.tiff) · [3d](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/Figure3d.tiff) | [`Figure3_FieldComparisons.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Figure3_FieldComparisons.R) | [`AllFLPData.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/AllFLPData.xlsx), [`AllCruiseLysoTracker.csv`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/AllCruiseLysoTracker.csv), [`Photos/`](https://github.com/CohenLabUGA/AcidotropicDyes/tree/main/Data/Photos) |
| [Figure 4](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/Figure4.tiff) | [`Figure4_SuppFigs11_13_CCSIncubations.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Figure4_SuppFigs11_13_CCSIncubations.R) | [`CCSIncubations.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CCSIncubations.xlsx), [`CubiTaxa.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CubiTaxa.xlsx) |

Figure 1 exports with its legend as a separate file. Figure 3 exports as three pieces — panels a–b, the NES image block, and the CCS image block — which were combined externally.

### Supplemental figures

| Output | Script | Key inputs |
|---|---|---|
| [Supp Fig 1](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppFig1.tiff) — station maps | [`SuppFig1_Maps.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig1_Maps.R) | [`StationData.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/StationData.xlsx) |
| [Supp Fig 2](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppFig2_FieldFCM.png) — field cytograms and gates | [`SuppFig2_FieldFCM.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig2_FieldFCM.R) | Cruise `.fcs` files (Zenodo) |
| [Supp Fig 3](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppFig3.tiff) — culture cytograms and gates | [`SuppFig3_CultureFCM.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig3_CultureFCM.R) | Culture `.fcs` files (Zenodo) |
| [Supp Fig 4](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppFig4.tiff) — staining by growth phase | [`SuppFig4_GrowthPhaseStaining.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig4_GrowthPhaseStaining.R) | [`StainingGrowthPhases.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/StainingGrowthPhases.xlsx) |
| [Supp Fig 5](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppFig5_CultureFCM.png) — CytPix vs. Guava comparison | [`SuppFig5_CompareCytometers.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig5_CompareCytometers.R) | [`CultureLysoData.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CultureLysoData.xlsx) (both sheets) |
| [Supp Fig 6](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppFig6.png) — *Tetraselmis* z-stacks | [`SuppFig6_Zstacks.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig6_Zstacks.R) | [`Zstackfluorescence/`](https://github.com/CohenLabUGA/AcidotropicDyes/tree/main/Data/Zstackfluorescence) |
| [Supp Fig 7](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppFig7.tiff) — bacteria:FLP ratios | [`SuppFig7_BacFLPRatios.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig7_BacFLPRatios.R) | [`BacteriaConcentrations.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/BacteriaConcentrations.xlsx) |
| [Supp Fig 8](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppFig8.tiff) — NES prey type comparison | [`SuppFig8_NESPreyComparisons.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig8_NESPreyComparisons.R) | [`NES_FLP_FCM.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NES_FLP_FCM.xlsx) |
| [Supp Fig 9](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppFig9_FLP_FCM.png) — FLP gating scheme | [`SuppFig9_FLP_FCM.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig9_FLP_FCM.R) | NES FLP `.fcs` file (Zenodo) |
| [Supp Fig 10](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppFig10.tiff) — reads mapped and annotated | [`SuppFig10_ReadStats.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig10_ReadStats.R) | [`PUPCYCLE_Reads_Stats.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/PUPCYCLE_Reads_Stats.xlsx) |
| [Supp Fig 11](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppFig11.tiff) — nanoeukaryotes and nitrate | [`Figure4_SuppFigs11_13_CCSIncubations.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Figure4_SuppFigs11_13_CCSIncubations.R) | [`CCSIncubations.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CCSIncubations.xlsx) |
| [Supp Fig 12](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppFig12.tiff) — depth-binned abundances | [`SuppFig12_CommunityDepth.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig12_CommunityDepth.R) | Both processed LysoTracker CSVs, [`BacteriaConcentrations.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/BacteriaConcentrations.xlsx) |
| [Supp Fig 13](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppFig13.tiff) — CCS transect taxonomy | [`Figure4_SuppFigs11_13_CCSIncubations.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Figure4_SuppFigs11_13_CCSIncubations.R) | [`station_protist_taxonomy.csv`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/station_protist_taxonomy.csv) |

Supplemental Figure 6 is assembled from three per-cell panels written to [`Figures/Zstack/`](https://github.com/CohenLabUGA/AcidotropicDyes/tree/main/Figures/Zstack) — [`Chui1.tiff`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/Zstack/Chui1.tiff), [`Chui3.tiff`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/Zstack/Chui3.tiff), and [`Tetraselmis1.tiff`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/Zstack/Tetraselmis1.tiff) — combined externally into the published version.

### Supplemental tables

| Output | Script |
|---|---|
| [Supp Table 1](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppTable1.png) — instrument settings | [`Table1SuppTable1_3.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Table1SuppTable1_3.R) |
| [Supp Table 2](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppTable2.png) — replicates and staining summary | [`Figure1_BarPlot.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Figure1_BarPlot.R) |
| [Supp Table 3](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppTable3.png) — NES microscopy replicates | [`Table1SuppTable1_3.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Table1SuppTable1_3-3.R) |
| [Supp Table 4](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Figures/SuppTable4.png) — fluorescence parameters | [`Figure2_FluorescenceChanges.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Figure2_FluorescenceChanges.R) |

### Statistics

Neither script produces a figure; both print test results to the console.

- [`FLPStatistics.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/FLPStatistics.R) — Shapiro-Wilk normality tests followed by t-tests or Wilcoxon rank-sum tests comparing CSGR and LysoTracker-positive percentages between cruises, and flow cytometry vs. microscopy within the NES.
- [`CruiseCommunityStats.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/CruiseCommunityStats.R) — the equivalent tests for community abundance variables (*Synechococcus*, bacteria, heterotrophs, phototrophs) between cruises.

## Data

[`Data/1_DataREADME.md`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/1_DataREADME.md) documents every spreadsheet column and its units. In brief:

**Cultures**

| File | Contents |
|---|---|
| [`CultureLysoData.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CultureLysoData.xlsx) | Percent positively stained per culture and replicate; default sheet is Attune CytPix, `Guava` sheet holds the cross-instrument subset |
| [`Table1.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/Table1.xlsx) | Isolation location, growth conditions, metabolism, class, cell dimensions |
| [`StainingGrowthPhases.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/StainingGrowthPhases.xlsx) | Staining at stationary, exponential, and 24 h dark |
| [`SupplementalGrazing.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/SupplementalGrazing.xlsx) | Culture FLP ingestion; `FLP_toplot` is the summary sheet used by Figure 1 |

**Field — LysoTracker**

| File | Contents |
|---|---|
| [`20241203_CCSLysoTrackerRaw.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241203_CCSLysoTrackerRaw.xlsx) · [`20241205_NESLysoTrackerRaw.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241205_NESLysoTrackerRaw.xlsx) | Per-well counts by population |
| [`20241203_CCSLysoTrackerProcessed.csv`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241203_CCSLysoTrackerProcessed.csv) · [`20241205_NESLysoTrackerProcessed.csv`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241205_NESLysoTrackerProcessed.csv) | Replicate means and standard deviations by station and depth |
| [`AllCruiseLysoTracker.csv`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/AllCruiseLysoTracker.csv) | Both cruises merged with depth bins |

**Field — FLP**

| File | Contents |
|---|---|
| [`NES_FLP_FCM.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NES_FLP_FCM.xlsx) · [`NES_FLP_Microscopy.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NES_FLP_Microscopy.xlsx) · [`CCSRawFLP.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CCSRawFLP.xlsx) | Raw counts |
| [`NES_FLP_Processed.csv`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NES_FLP_Processed.csv) · [`CCS_FLP_Processed.csv`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CCS_FLP_Processed.csv) · [`AllFLPData.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/AllFLPData.xlsx) | Uptake rates and CSGR |
| [`BacteriaConcentrations.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/BacteriaConcentrations.xlsx) · [`NESBacteria.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NESBacteria.xlsx) | SYBR Green I bacterial counts used to scale CSGR |

**Metatranscriptomes** (from Speciale 2025; see [Citation and related work](#citation-and-related-work))

| File | Contents |
|---|---|
| [`station_protist_taxonomy.csv`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/station_protist_taxonomy.csv) · [`CubiTaxa.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CubiTaxa.xlsx) | Normalized protist reads by taxonomic group, transect and incubation |
| [`CCSIncubations.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CCSIncubations.xlsx) | Nanoeukaryote abundance and nitrate through the incubation |
| [`PUPCYCLE_Reads_Stats.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/PUPCYCLE_Reads_Stats.xlsx) | Mapping and annotation rates |

**Other**

| File | Contents |
|---|---|
| [`FlowCytometerInfo.xlsx`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/FlowCytometerInfo.xlsx) | Gain, voltage, and detection wavelengths for both instruments |
| [`Photos/`](https://github.com/CohenLabUGA/AcidotropicDyes/tree/main/Data/Photos) | Attune CytPix images, 74 × 74 µm, sampled at random for Figure 3c,d |
| [`Zstackfluorescence/`](https://github.com/CohenLabUGA/AcidotropicDyes/tree/main/Data/Zstackfluorescence) | *Tetraselmis* z-stack images with per-slice red and green intensity profiles |

### FCS-dependent scripts

Four scripts read `.fcs` files that are not in this repository. Download the [Zenodo archive](https://doi.org/10.5281/zenodo.16813438) and edit the path near the top of each:

| Script | Path to update |
|---|---|
| [`Figure2_FluorescenceChanges.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Figure2_FluorescenceChanges.R) | `parent` (line ~28) and `MapNames.xlsx` (line ~181) |
| [`SuppFig2_FieldFCM.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig2_FieldFCM.R) | four `read.FCS()` calls (lines ~23–26) |
| [`SuppFig3_CultureFCM.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig3_CultureFCM.R) | `base_dir` (line ~212) |
| [`SuppFig9_FLP_FCM.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SuppFig9_FLP_FCM.R) | `fcsfile` (line ~22) |

[`Figure3_FieldComparisons.R`](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Figure3_FieldComparisons.R) uses the CytPix images, which are committed under [`Data/Photos/`](https://github.com/CohenLabUGA/AcidotropicDyes/tree/main/Data/Photos); the full image set is also on Zenodo.

## Citation and related work

The CCS metatranscriptome and incubation data originate from a larger sampling effort described in Speciale, E. (2025), *The Molecular Physiology of Mixotrophic Phytoplankton Under Iron-Limited Upwelling Conditions*, M.S. thesis, University of North Carolina at Chapel Hill: https://cdr.lib.unc.edu/downloads/th83md43g

Field LysoTracker staining across both cruises is analyzed further in Ewton, E. (2025), *Quantifying Mixotrophy Potential as a Function of Environmental and Biological Drivers in Coastal and Estuarine Plankton Communities*, Ph.D. dissertation, University of Rhode Island: https://digitalcommons.uri.edu/oa_diss/4497/

## Contact

Corresponding author: Natalie R. Cohen (cohen@uga.edu), Skidaway Institute of Oceanography, University of Georgia.
