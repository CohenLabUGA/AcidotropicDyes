**This readme contains more detailed descriptions of each spreadsheet in the Data folder, including column header descriptions and units.**

1.[ 20241203_CCSLysoTrackerProcessed.csv](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241203_CCSLysoTrackerProcessed.csv)
- Station - Station number along the transect
- Light - Percent light attenuation (46%, 22%, 10% or 1%)
- Depth - Depth of the water sample collection in meters
- avsyn/sdsyn - average and standard deviation across biological replicates for *Synechococcus* concentrations (cells/mL)
- avpico/sdpico - average and standard deviation across biological replicates for picoeukaryote concentrations (cells/mL)
- avcrypto/sdcrypto - average and standard deviation across biological replicates for cryptophyte concentrations (cells/mL)
- avnano/sdnano - average and standard deviation across biological replicates for nanoeukaryote concentrations (cells/mL)
- avhetero/sdhetero - average and standard deviation across biological replicates for heterotrophic nanoeukaryote concentrations (cells/mL)
- avpercent/sdpercent -  average and standard deviation across biological replicates for percent of mixotrophic nanoeukaryotes 
- avmixo/sdmixo - average and standard deviation across biological replicates for mixotrophic nanoeukaryote concentrations (cells/mL)

2.[20251203_CCSLysoTrackerRaw](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241203_CCSLysoTrackerRaw.xlsx)
- Station - Station number along the transect
- Light - Percent light attenuation (46%, 22%, 10% or 1%)
- Depth - Depth of the water sample collection in meters
- Rep - Biological replicate (A or B)
- Lyso - Yes if LysoTracker was added to the well (stained sample), no if LysoTracker was not added to the well (control sample)
- Well - For internal regulation, well the sample was run in on the flow cytometer
- syn - *Synechococcus* concentration (cells/mL)
- picoeuk - picoeukaryote concentration (cells/mL)
- hetero - heterotrophic nanoeukaryote concentration (cells/mL)
- cryptophyte - cryptophyte concentration (cells/mL)
- largenanoeuks - size class not used in this study (cells/mL)
- unstainednanoeuks - nanoeukaryotes that did not stain with LysoTracker (cells/mL)
- stainednanoeuks - nanoeukaryotes that did stain with LysoTracker (cells/mL)

3.[20251205_NESLysoTrackerProcessed.csv](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241205_NESLysoTrackerProcessed.csv)
- Station - Station number along the transect
- Depth - Depth of the water sample collection in meters
- avsyn/sdsyn - average and standard deviation across biological replicates for *Synechococcus* concentrations (cells/mL)
- avpico/sdpico - average and standard deviation across biological replicates for picoeukaryote concentrations (cells/mL)
- avnano/sdnano - average and standard deviation across biological replicates for nanoeukaryote concentrations (cells/mL)
- avhetero/sdhetero - average and standard deviation across biological replicates for heterotrophic nanoeukaryote concentrations (cells/mL)
- avpercent/sdpercent -  average and standard deviation across biological replicates for percent of mixotrophic nanoeukaryotes 
- avmixo/sdmixo - average and standard deviation across biological replicates for mixotrophic nanoeukaryote concentrations (cells/mL)

4.[20251205_NESLysoTrackerRaw](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/20241205_NESLysoTrackerRaw.xlsx.)
- Station - Station number along the transect
- Time - day or night samples for those applicable
- Depth - Depth of the water sample collection in meters
- Lyso - Yes if LysoTracker was added to the well (stained sample), no if LysoTracker was not added to the well (control sample)
- Rep - Biological replicate (A or B)
- Well - For internal regulation, well the sample was run in on the flow cytometer
- syn - *Synechococcus* concentration (cells/mL)
- picoeuk - picoeukaryote concentration (cells/mL)
- nanoeuk - nanoeukaryotes that did not stain with LysoTracker (cells/mL)
- green - nanoeukaryotes that did stain with LysoTracker (cells/mL)
- R2 - internal gate for QC
- hetero - heterotrophic nanoeukaryote concentration (cells/mL)

5.[AllCruiseLysoTracker.csv](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/AllCruiseLysoTracker.csv)
- Station - Station number along the transect
- Depth - Depth of the water sample collection in meters
- avsyn/sdsyn - average and standard deviation across biological replicates for *Synechococcus* concentrations (cells/mL)
- avpico/sdpico - average and standard deviation across biological replicates for picoeukaryote concentrations (cells/mL)
- avnano/sdnano - average and standard deviation across biological replicates for nanoeukaryote concentrations (cells/mL)
- avhetero/sdhetero - average and standard deviation across biological replicates for heterotrophic nanoeukaryote concentrations (cells/mL)
- avpercent/sdpercent -  average and standard deviation across biological replicates for percent of mixotrophic nanoeukaryotes 
- avmixo/sdmixo - average and standard deviation across biological replicates for mixotrophic nanoeukaryote concentrations (cells/mL)
- Cruise - California Current System or North East Shelf
- DepthBin - groups samples into 10m bins based on Depth column
- DepthCode - Code (Surface/ DCM/Deep) for where the sample was taken

6.[AllFLPData](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/AllFLPData.xlsx) (manually combined CCS_FLP_Processed.csv and NES_FLP_Processed.csv)
- Station - Station number along the transect
- avpercent/sdpercent -  average and standard deviation across biological replicates for percent of mixotrophic phototrophs
- avconc/sdconc - average and standard deviation across biological replicates for mixotrophic phototroph concentrations (cells/mL)
- avloggrazing/sdloggrazing - average and standard deviation of the log base 10 of the cell specific grazing rate
- avgrazing/sdgrazing - average and standard deviation of the cell specific grazing rate. Units are bacteria/ (cell* hour)
- avno/sdno - average and standard deviation of nanoeukaryote concentrations (cells/mL)
- avnoBR/sdnoBR - average and standard deviation of the community bacterivory rate. Units are bacteria / (mL * hour)
- Cruise - California Current System or North East Shelf
- Depth - Surface of SCM (subsurface chlorophyll maximum)
- Method - FLP analysis method. Microscopy or FLowCytometry

7.[BacteriaConcentrations](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/BacteriaConcentrations.xlsx)
- Station - Station number along transect
- Depth - Depth of the water sample collection in meters
- Time - Time collected (Day/Night) for North East Shelf Samples, replicate for California Current System Samples
- bacteria - bacteria concentration (cells/mL)
- Cruise - California Current System or North East Shelf

8.[CCSIncubations](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CCSIncubations.xlsx)
- Timepoint - four timepoints were sampled during the incubation. T0 at initial water collection, T1 at 2 days, T2 at 7 days, and T3 at 11 days
- Treatment - control, dfb, or iron treatments. Only control was used for this study
- Replicate - Biological replicate (A, B, or C)
- Well - For internal regulation, well the sample was run in on the flow cytometer
- proportionmixos - proportion (percent/100) of mixotrophic nanoeukaroytes
- nanoeukaryotes - concentration (cells/mL) of nanoeukaryotic cells
- heterotrophs - concentration (cells/mL) of heterotrophic cells
- NO3 - nitrate concentration (µM)

9.[CCSRawFLP](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CCSRawFLP.xlsx)
- SlideNumber - For internal regulation, each slide was assigned a unique number
- Station - Station number along transect
- Depth - Surface for all
- Prey - surrogate prey added to incubation, bead for all
- Timepoint - T0 for samples taken immediately after the spike of surrogate prey, T1 for samples taken after 1 hour of incubation
- Replicate - Biological replicate (A, B, or C)
- CountChlCells - count of chlorophyll containing cells
- CountMixoCells - count of chlorophyll containing cells that had also ingested a bead
- CountBeadIngested - count of beads ingested by mixotrophic cells
- Method - square for all, indicates field of view method of counting
- NumSquaresCounted - number of fields of view assessed
- Objective - objective used for counting (40x for all)
- VolumeFiltered - volume (in mL) filtered onto the slide
- red_cellsmL - concentration (cells/mL) of chlorophyll containing cells
- mixo_cellsmL - concentration (cells/mL) of chlorophyll containing cells that had also ingested a bead
- percentmixo - percent of mixotrophic cells

10.[CCS_FLP_Processed.csv](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CCS_FLP_Processed.csv)
- Station - Station number along transect
- avpercent/sdpercent - average and standard deviation of the percent of mixotrophic phototrophs
- avconc/sdconc - average and standard deviation of the mixotrophic cell concentration (cells/mL)
- avloggrazing/sdloggrazing - average and standard deviation of the log base 10 transformed cell specific grazing rate
- avgrazing/sdgrazing - average and standard deviation of the cell specific grazing rate. Units are bacteria/ (cell* hour)
- avnano/sdnano - average and standard deviation of nanoeukaryote concentrations (cells/mL)
- avnanoBR/sdnanoBR - average and standard deviation of the community bacterivory rate. Units are bacteria / (mL * hour)
- Cruise - California Current System identifier
- Depth - Depth code (Surface for all)

11.[CubiTaxa](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CubiTaxa.xlsx)
 - Timepoint - Days of incubation. Either 7 (indicates T2 of the incubation experiment) or 11 (indicates T3 of the incubation experiment) 
 - Treatment - Control, DFB (iron chelator), or Iron Addition treatments. Only control was used for this study.
 - Taxa - Main taxonomic groups that the reads mapped to
 - PercentReads - average percent of reads that mapped to each taxonomic group
 - SdPercentReads - standard deviation of the percent of reads that mapped to each taxonomic group

12.[CultureLysoData](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/CultureLysoData.xlsx)

*Tab: LysoTracker*
- Name - Species name
- Cytometer - Name of the flow cytometer the sample was measured on. CytPix or Guava
- Place - location of isolation for each culture
- Metabolism - best prediction of metabolism (Phototroph or Mixotroph)
- Type - Lineage identification for each organisms
- Time - date of measurement
- Lyso - percent of cells stained with LysoTracker. Average of technical replicates
- sdLyso - standard deviation of technical replicates for percent of cells stained with LysoTracker
- CellConcentration - concentration of cells (cells/mL) during staining
- ln - natural log of the cell concentration

*Tab: LysoSensor*
- Name - Species name
- Place - location of isolation for each culture
- Metabolism - best prediction of metabolism (Phototroph or Mixotroph)
- Type - Lineage identification for each organisms
- Time - date of measurement
- AvSensor - percent of cells stained with LysoSensor. Average of technical replicates
- SdSensor - standard deviation of technical replicates for percent of cells stained with LysoSensor

*Tab: Fluoresence*
- Culture - Species name
- Replicate - Replicate (A or B)
- Stain - stain of that particular well (LysoTracker = Tracker, LysoSensor = Sensor, unstained control = Control)
- Mean - mean green fluorescence of the cell population
- Median - median green fluorescence of the cell population
- SD - standard deviation of green fluorescence of the cell population
- CV - %CV of green fluorescence of the cell population
- Peak - peak green fluorescence of the cell population
- FSCMean - Mean forward scatter value of the cell population
- RedMean - Mean red fluorescence of the cell population
- VioletMean - Mean blue fluorescence of the cell population

13.[FlowCytometerInfo](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/FlowCytometerInfo.xlsx)
- Channel - detection channel for the flow cytometers
- Guava Gain - Culture Tests & NES FLP : Gain on the Guava flow cytometer used for the culture tests and NES FLP analysis
- Guava Gain - LysoTracker CCS : Gain on the Guava flow cytometer used for LysoTracker analysis on the CCS cruise
- Guava Gain - LysoTracker NES : Gain on the Guava flow cytometer used for LysoTracker analysis on the NES cruise
- CytPix Voltage - Culture Tests : Voltage (V) on the CytPix flow cytometer used for culture tests
- CytPix Wavelength (nm) : Wavelength of each detection channel on the CytPix flow cytometer
- Guava Wavelength (nm) : Wavelength of each detection channel on the Guava flow cytometer

14.[NESBacteria](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NESBacteria.xlsx)
- Station - Station number along transect
- Place - Surface or DCM sample
- Depth - Depth of sample collection in meters
- Time - time of day of sample collection
- bacteria - bacteria concentration in cells/mL

15.[NES_FLP_FCM](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NES_FLP_FCM.xlsx)
- File - for internal regulation, name of the file the data is collected in
- Station - Station number along transect
- Well - for internal regulation, well number the sample was run in on the flow cytometer
- Place - DCM or surface, place of sample collection
- Timepoint - samples collected immediately after surrogate prey spike are indicated with a 0. Samples taken after 1 hour of incubation are indicated with a 1
- Time - time of day the sample was taken
- Type - surrogate prey type added. Green indicates plastic beads, Ecoli indicates commercially available *E. coli*, SW indicates samples with no prey added. 
- Rep - biological replicate (A or B)
- nano - concentration of nanoeukaryotes (cells/mL)
- mixo - concentration of mixotrophic nanoeukaryotes (cells/mL)

16.[NES_FLP_Microscopy](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NES_FLP_Microscopy.xlsx)
- Station - Station number along transect
- Timepoint - samples collected immediately after surrogate prey spike are indicated with a T0. Samples taken after 1 hour of incubation are indicated with a T1
- Day/Night - time of day the sample was taken
- Depth - Depth of sample collection, surface or dcm
- chlcells - count of chlorophyll containing cells
- mixocells - count of chlorophyll containing cells that ingested a bead
- preycount - count of prey ingested by mixotrophs
- percentmixo - percent of chlorophyll containing cells that ingested a bead
- Lines_counted - number of transects counted along the slide
- percentchange - subtraction of the percentmixo from T1 - T0 samples

17.NES_FLP_Processed(https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NES_FLP_Processed.csv)
- Station - Station number along transect
- avconc/sdconc - average and standard deviation of the mixotrophic cell concentration (cells/mL) from flow cytometry samples
- avpercent/sdpercent - average and standard deviation of the percent of mixotrophic phototrophs from flow cytometry samples
- avgrazing/sdgrazing - average and standard deviation of the cell specific grazing rate for flow cytometry samples. Units are bacteria/ (cell* hour)
- avbac/sdbac - average and standard deviation of bacteria concentrations (cells/mL)
- avloggrazing/sdloggrazing - average and standard deviation of the log base 10 transformation of the cell specific grazing rate for flow cytometry samples
- avnanoBR/sdnanoBR - average and standard deviation of the community bacterivory rate for flow cytometry samples. Units are bacteria / (mL * hour)
- Cruise - North East Shelf identifier
- Depth - Depth code (Surface or DCM)
- Method.x - indication that the left portion of the dataframe is from flow cytometry samples
- microscopepercent/sdmicroscopepercent - average and standard deviation of the percent of mixotrophic phototrophs from microscopy samples
- Method.y - indication that the right portion of the dataframe is from microscopy samples
- microscopeGR - cell specific grazing rate (bacteria / cell* hour) for microscopy samples

18.[NES_LysoTracker](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/NES_LysoTracker.xlsx)
- Station - Station number along transect
- Place - location of water collection. DCM, Surface, or the depth in meters for deeper samples
- Depth - depth of water collection in meters
- avsyn/sdsyn - average and standard deviation across biological replicates for *Synechococcus* concentrations (cells/mL)
- avpico/sdpico - average and standard deviation across biological replicates for picoeukaryote concentrations (cells/mL)
- avnano/sdnano - average and standard deviation across biological replicates for nanoeukaryote concentrations (cells/mL)
- avhetero/sdhetero - average and standard deviation across biological replicates for heterotrophic nanoeukaryote concentrations (cells/mL)
- avpercent/sdpercent -  average and standard deviation across biological replicates for percent of mixotrophic nanoeukaryotes 
- avmixo/sdmixo - average and standard deviation across biological replicates for mixotrophic nanoeukaryote concentrations (cells/mL)

19.[PUPCYCLE_Reads_Stats](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/PUPCYCLE_Reads_Stats.xlsx)

*Tab: Incubation*
- Sample ID - Internal identifier for samples
- Timepoint - T2 or T3, indicates sampling timepoint of the incubation
- Days - day of incubation sampling (7 or 11)
- Treatment - treatment identifier, all control samples
- Replicate - biological replicate (A, B, or C)
- Total # Reads - Total number of reads for each sample
- Reads Mapped - Total number of reads that were mapped 
- % Reads Mapped - Percent (Mapped / Total) of reads that were mapped
- Reads Taxonomically Annotated - Total number of reads that were taxonomically annotated 
- % Reads taxonomically annotated - Percent (Annotated /Total) of reads that were taxonomically annotated

*Tab: Stations*
- Sample ID - Internal identifier for samples
- Station - Station number along transect
- Replicate - biological replicate (A, B, or C)
- Total # Reads - Total number of reads for each sample
- Reads Mapped - Total number of reads that were mapped 
- % Reads Mapped - Percent (Mapped / Total) of reads that were mapped
- Reads Taxonomically Annotated - Total number of reads that were taxonomically annotated 
- % Reads taxonomically annotated - Percent (Annotated /Total) of reads that were taxonomically annotated

20.[StainingGrowthPhases](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/StainingGrowthPhases.xlsx)
- Group - taxonomic lineage of the organisms
- Name - species identification
- Type - Growth phase of the organism during staining (Stationary, Exponential, or 24 hour darkness)
- Stain - LysoTracker or LysoSensor
- Av - mean percent of cells stained by the acidotropic dye
- Std - standard deviation of cells stained by the acidotropic dye

21.[StationData](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/StationData.xlsx)

*Tab: NES*
- Site - Station along the transect
- Latitude - latitude in degrees north 
- Longitude - longitude in degrees west

*Tab: CCS*
- Lat_deg - latitude in degrees north 
- Long_deg - longitude in degrees west
- Station - Station along the transect

22.[SupplementalGrazing](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/SupplementalGrazing.xlsx)

*Tab: Tetraselmis*
- Culture - name of species 
- Replicate - Biological replicate (A,B or C)
- Ingestion - ingestion rate (bacteria / (cell* hour)) 
- FLPPercent - percent of eating cells determined by FLP incubations

*Tab: SOmixos*
- Culture - short identifier for species. GC = G. *cryophila*, MA = *M. antarctica*, PT = *P. tychotreta*
- Date - date of experiment
- IngestionRate -  ingestion rate (bacteria / (cell* hour)). Average of biological replicates for each experiment
- SdIngestionRate -  Standard deviation of biological replicate ingestion rate

*Tab: Raw_TetraselmisFLP*
- Culture - name of species
- Replicate - Biological replicate (A, B, or C)
- Timepoint - samples taken immediately after surrogate prey spike are T0. Samples taken after 1 hour of incubation are T1
- ChlCount - count of cells containing chlorophyll
- mixocount - count of cells containing chlorophyll that also ingested a prey
- beadingested - count of prey ingested
- percentmixo - percent of mixotrophic cells
- VolFiltered - volume of water filtered
- Areaofsquare - area (mm^2) of each field of view
- NumSquareCounted - number of fields of view counted
- nanoeukconc - concentration of all cells
- mixoconc - concentration of eating cells
- subpercent - subtraction (T1-T0) of percent of mixotrophic cells
- av - average of subpercent for each culture
- sd - standard deviation of subpercent for each culture

*Tab: Raw_TetraselmsiPreyRemoval*
Top panel: Measured concentrations
- average mixotroph concentrations (cells/mL) across timepoints for each biological replicate
- Average of bacterial concentrations (cells/mL) in both bacteria alone and bacteria with mixotroph treatments for each biological replicate and each sampled timepoint (T0 and T24 hours)
Bottom panel: Calculations 
- calculation of k (growth constant for bacteria growth) for each biological replicate
- calculation of g (grazing coefficient) for each biological replicate
- calculation of C (average cell concentration during time interval) for each biological replicate
- calculation of F (volume swept clear) for each biological replicate
- calculation of I (ingestion rate) for each biological replicate

*Tab: Raw_SOmixoPreyRemoval*
- Same calculations as above but using the timepoints T24 and T48 hours. Each experiment is specified by a number that is identified in the key at the top of the tab

23.[Table1](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/Table1.xlsx)
- Culture - species name and unique identifier if applicable
- Isolation Location - location of the species' isolation
- Temperature (ºC) - temperature of species maintenance in degrees Celcius
- Light Cycle - hours in light and/or dark for each culture
- Light Intensity (µmol photons meter second) - light intensity of species maintenance
- Metabolism - known / assumed metabolism for each species, Phototroph or Mixotroph
- Lineage - major lineage for each species
- Acquisition - name of the isolator or scientist we received the culture from
- CytPixSize - approximate size dimensions (µm) for each culture. Width by length is reported 

24.[station_protist_taxonomy](https://github.com/CohenLabUGA/AcidotropicDyes/blob/main/Data/station_protist_taxonomy.csv)
- Site - Station along the transect
- lineage - Main taxonomic groups that the reads mapped to
- Avg_Total_Count - total count of reads that mapped to each lineage, averaged by biological replicate
- SD_Total_Count - standard deviation of reads that mapped to each lineage
- Avg_Percentage - average percent of reads that mapped to each taxonomic group
- SD_Percentage - standard deviation of the percent of reads that mapped to each taxonomic group
