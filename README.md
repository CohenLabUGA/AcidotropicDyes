# Evaluating acidotropic dyes for detecting mixotrophy in protists: insights from cultures and field communities

## Project Background
Mixotrophic organisms, those that can employ both phagotrophy and photosynthesis, are extremely important for nutrient cycling and the microbial food web. Unfortunately, these organisms are difficult to detect in the natural environment. This study evaluated a tool for measuring mixotrophs in aquatic environments, acidotropic dyes. These dyes are thought to bind to acidic digestion vacuoles and can be identified using flow cytometry. We test these dyes in a suite of lab organisms and then compare them in the field to the community standard for measuring mixotrophs, fluorescently labeled particle incubations in both the New England Shelf (NES) and California Current System (CCS). 

This repository contains data and code used in the analysis of this project. 
## Data
All raw and processed dataframes used in this analysis are hosted in the folder [Data](https://github.com/CohenLabUGA/AcidotropicDyes/tree/main/Data)

Raw data of culture staining with acidotropic dyes can be found in [CultureLysoData.xlsx](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CultureLysoData.xlsx), with tabs for LysoTracker, LysoSensor, and LysoTrackerFluorescence giving data for LysoTracker staining & LysoSensor staining data used for Figure 2, and the green fluorescence data used for Figure 3. A table detailing the culture conditions was made using [this](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/Table1.xlsx)  dataset 

The station data for the field samples can be found at [StationData.xlsx](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/StationData.xlsx), which was used to make Figure 1. Raw flow cytometry data for both cruises can be found [here](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241205_NESLysoTrackerRaw.xlsx) for NES and [here](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241203_CCSLysoTrackerRaw.xlsx) for CCS. This data was then processed; the processed form of these data can be found [here](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241205_NESLysoTrackerProcessed.csv) for NES and [here](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241203_CCSLysoTrackerProcessed.csv) for CCS. A combined datasheet with all field LysoTracker data can be found at [AllCruiseLysoTracker.csv](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/AllCruiseLysoTracker.csv). 

Similarly, FLP incubations had both raw and processed data. The NES had both microscopy and flow cytometry FLP analysis, found [here](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NES_FLP_Microscopy.xlsx) and [here](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NES_FLP_FCM.xlsx) respectively. These were processed to make [NES_FLP_Processed.csv](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NES_FLP_Processed.csv). Only microscopy was available for the CCS FLP analysis. Raw data can be found [here](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CCSRawFLP.xlsx) and processed data [here](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CCS_FLP_Processed.csv). All FLP data was combined into the file [AllFLPData.xlsx](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/AllFLPData.xlsx). For grazing calculations, bacteria was needed. NES specific data can be found [here](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NESBacteria.xlsx), and all bacteria data can be found at [BacteriaConcentrations.xlsx](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/BacteriaConcentrations.xlsx). 

For the CCS transect, RNA data and iron manipulation experiments were also conducted. The RNA taxonomy information can be found [here](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CubiTaxa.xlsx) for the iron manipulation experiment and [here](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/station_protist_taxonomy.csv) for the stations. 

Finally, the data necessary for supplemental figures relating to grazing on the two cruises can be found [here](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/SupplementalGrazing.xlsx) with tabs for both Tetraselmis spp. and the Southern Ocean mixotrophs, with the z-stack image in supplemental figure 3 [here](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/TetraZStack-1.png)
## R Scripts
Various R scripts within an R project were created to process data, make figures, and run statistical analyses. 

Maps of each cruise transect seen in Figure 1 was made using [Maps.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Maps.R)

Tables 1 and supplementary 1 were created using [Tables.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/Tables.R)

Acidotropic dye staining of cultures seen in Figure 2 was made using [LysoBarPlot.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/LysoBarPlot.R)

Fluoresence data seen in Figure 3 and supplemental table 2 were made using [FluorescenceTable.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/FluoresenceTable.R)

FLP data was processed using [FLPDataWrangling.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/FLPDataWrangling.R) and LysoTracker data was processed for the CCS cruise using [CCS_LysoTrackerProcess.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/CCS_LysoTrackerProcess.R) and for the NES cruise [NES_LysoTracker_Process.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/NES_LysoTracker_Process.R). 

Community composition on each cruise seen in Figure 4 was plotted using [CommunityDepth.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/CommunityDepth.R) with statistics run in [CruiseCommunityStats.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/CruiseCommunityStats.R). 

Mixotroph proportions, concentrations, and grazing on both cruises found in Figures 5, Supplemental figure 9, and Supplemental figure 10 were plotted using [MixotrophGraphs_Fig5Supp9Supp10.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/MixotrophGraphs_Fig5Supp9Supp10.R). Significant differences were calculated in the file [FLPStatistics.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/FLPStatistics.R). 

Fe manipulation experiments and RNA data for the CCS cruise, seen in Figures 6, Supplemental Figure 11, and Supplemental Figure 12, were plotted using [FeIncubations_RNA.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/FeIncubations_RNA.R). 


Finally, supplemental grazing data for the mixotrophs (Supplemental Figures 2 and 3) was plotted using [SupplementalGrazing.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/SupplementalGrazing.R), and prey type comparisons on the NES cruise (Supplemental Figure 5) were plotted using [NES_preycomparisons.R](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Code/NES_preycomparisons.R). 

