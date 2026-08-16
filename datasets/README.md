Here is the `README.md` for the `datasets` folder, translated to English, maintaining the objective and professional format without emojis:

# Datasets

## Overview

This directory contains the analysis results, raw data, and input datasets necessary to feed the project's analytical notebooks. It includes graphical analyses, images, tables, field measurements, and geospatial boundaries required to reproduce the methodology and generate the figures presented in the study.

## Directory Structure

### 1. `HydroWeb`

Contains the water level and river discharge data used to establish the relationship between the Óbidos monitoring station and the hydrological behavior of the Curuai floodplain.

### 2. `original field sampling`

Contains the original in-situ water quality parameters collected during field campaigns. These datasets are used for model calibration and validation.

### 3. `shapefiles`

Contains the vector data boundaries (shapefiles) used to spatially delimit the local and regional watersheds for the analyses.

### 4. `Landsat Sampling`

Contains analysis results comprising surface reflectance data extracted from Landsat satellite imagery. These spectral values correspond to the exact spatial coordinates and temporal windows of the field sampling.

### 5. `Parameters Time Series`

Contains the core analytical results and estimated values for each environmental parameter throughout the time series. This directory is subdivided into:

* **`Water Period Definitions`**: Establishes the water level relationships between Óbidos and Curuai, defining the exact start and end dates of the hydrological periods (Rising, High Water, Falling, Low Water) across the studied decades.
* **`TSS Modeling`**: Contains the outputs of the Total Suspended Solids (TSS) modeling, performance metrics, and the model selection process.
* **`Results`**: Stores the raw calculated values for each parameter along the time series, differentiating between the regional and local basins where applicable.
* **`merged_df`**: Contains the unified DataFrames integrating all time series parameters for both basins. It also includes a subdirectory containing the final statistical analyses and correlation matrices.

### 6. `pca_total`

Contains the consolidated results and outputs from the Principal Component Analysis (PCA) applied to the environmental datasets.

## Data Sources and References

The raw data provided in this repository was obtained from the following sources:

* **Brazilian National Water Agency (ANA - Agência Nacional de Águas)**: Water level and discharge data acquired via the HidroWeb portal ([https://www.snirh.gov.br/hidroweb/apresentacao](https://www.snirh.gov.br/hidroweb/apresentacao)).
* **Field Data**: In-situ water quality parameters sourced from Roque et al. (2024). Available at: [https://rmets.onlinelibrary.wiley.com/doi/10.1002/gdj3.207](https://rmets.onlinelibrary.wiley.com/doi/10.1002/gdj3.207).