# nes_lter_zooplankton_abundance_v2

# NES-LTER Zooplankton Abundance Data Package Scripts

This repository contains R scripts for creating the NES-LTER zooplankton abundance data package submitted to the Environmental Data Initiative (EDI) repository. The data package provides zooplankton abundance data collected during Northeast U.S. Shelf Long-Term Ecological Research (NES-LTER) transect cruises, ongoing since 2018.

## Repository Contents
- `merge_data.R`: Merges multiple abundance files into a unified dataset.
- `abundance_zoop.R`: Calculates zooplankton concentrations in volumetric units (100 m³) and aerial units (10 m²) and generates the final CSV files submitted to EDI.

The generated CSV files include metadata and calculated abundance metrics, ready for inclusion in the NES-LTER data package.

## Data Package Overview

### Title
**Zooplankton Abundance from Net Tows on Northeast U.S. Shelf Long-Term Ecological Research (NES-LTER) Transect Cruises, Ongoing Since 2018**

### Summary 

This data package provides abundance data for zooplankton collected during seasonal NES-LTER transect cruises. Sampling occurs at standard NES-LTER stations (L1–L11) and the Martha’s Vineyard Coastal Observatory (MVCO) using a 61-cm Bongo net with two mesh sizes (335 µm and 150 µm). The package includes data from the 335-µm mesh net, processed and analyzed by Morski Instytut Rybacki in Szczecin, Poland, and NOAA’s Northeast Fisheries Science Center.

Key details:
- Sampling region: Northeast U.S. Shelf, along longitude 70° 53' W from near Martha's Vineyard to the continental shelf break.
- Sampling depths: 20–200 meters.
- Units provided: Volumetric (100 m³) and aerial (10 m²) zooplankton abundance.
- Data versions: Includes staged and unstaged abundance data.
- Metadata: Cruise and station information are included in supplemental tables.

## Requirements

These scripts require the following R packages:
- tidyverse
- suncalc
- here

Install the required packages using:
```r
install.packages(c("dplyr", "tidyr", "readr"))
```

## Citation

Please cite the data package as: Northeast U.S. Shelf Long-Term Ecological Research (NES-LTER). (202X). Zooplankton abundance from net tows on Northeast U.S. Shelf Long-Term Ecological Research (NES-LTER) Transect cruises, ongoing since 2018. v2. Environmental Data Initiative.
