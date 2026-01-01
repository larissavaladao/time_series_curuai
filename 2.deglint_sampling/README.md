# Deglint Sampling - Remote Sensing Reflectance Correction and Data Extraction

## Overview

This directory contains the processing pipeline for atmospherically corrected images including: sun glint correction and transformation to remote sensing reflectance and spectral sampling of Landsat satellite imagery over the Curuai floodplain. The primary objective is to extract spectrally-corrected remote sensing reflectance values at in-situ field sampling locations, enabling comparison between satellite and field measurements for algorithm validation and calibration.

## Methodology

### 1. Sun Glint Correction

For specific corrections in aquatic environments, particularly the sun glint (specular reflection), procedures based on shortwave infrared (SWIR) bands were adopted, following the recommendations of:
- Kay et al. (2009)
- Maciel et al. (2021)

### 2. Remote Sensing Reflectance Transformation

The surface reflectance (Rsat) was divided by π to obtain the corrected remote sensing reflectance:

$$R_{rs} = \frac{R_{sat}}{\pi}$$

### 3. Glint Correction Algorithm

The glint correction method described by Wang and Shi (2007) was applied. This method assumes that the signal in the shortwave infrared band (SWIR) corresponds to the specular reflection of the water surface, even under high turbidity conditions:

$$R_{rs(VNIR)} = R_{sat(VNIR)} - R_{sat(SWIR)}$$

**Key Assumption**: The SWIR signal is negligible for water due to extremely high water absorption (Wang et al., 2007), even at higher Total Suspended Sediment (TSS) and Total Suspended Inorganic matter (TSI) concentrations. This method has demonstrated successful results in Brazilian waters (Maciel et al., 2020; Lobo et al., 2021).

### 4. Spectral Sampling Strategy

For each field collection point in the Curuai database:

- **Temporal Window**: Landsat images within a 16-day window before and after the field sampling date were selected
- **Spatial Window**: Mean spectral values were extracted in 3×3 pixel windows around each collection point coordinate
- **Quality Filtering**: Only values meeting these criteria were retained:
  - Minimum 4 valid pixels (no cloud contamination) in the 3×3 window
  - Smallest temporal difference relative to field sampling date

## Datasets Used

### Input Data
- **Field Database**: *Dataset_CFP.xlsx* - Contains in-situ measurements including:
  - Collection date and time
  - Geographic coordinates (latitude/longitude)
  - Water quality parameters (turbidity, chlorophyll, SPM, TOC, etc.)
  - Optical properties (chlorophyll A and B)
  
- **Satellite Images**:
  - PY6S corrected Landsat imagery
  - Spatial Resolution: 30m

### Output Data
- Extracted spectral statistics from Landsat images

Both output files contain the following statistics for each valid sample:
- Mean, median, min, max, and standard deviation for blue, green, red, and near-infrared (NIR) bands
- Image acquisition date
- Temporal difference from field sampling date
- Cloud cover percentage
- Number of valid pixels in sampling window

## Main Notebook

**[deglint_sample_py6s_field_points.ipynb](deglint_sample_py6s_field_points.ipynb)**

