# Hydrological connectivity controls four decades of suspended sediment dynamics in an Amazon floodplain

## Overview

This repository contains the code and notebooks used to implement the analyses presented in the scientific article **"Hydrological connectivity controls four decades of suspended sediment dynamics in an Amazon floodplain"**.

<p align="center">
  <img src="5.%20parameters_by_period/time_series_txt.gif" alt="Time Series Animation" width="800">
</p>

The project is organized as a sequential pipeline. The directories are numbered in the exact order the steps should be executed to ensure the correct reproduction of the methodology and results of the article.

## Repository Structure (Pipeline)

### 1. `1.py6sCorrection`
Module for atmospheric correction of satellite images using the Py6S Radiative Transfer model. It performs the conversion of top-of-atmosphere (TOA) radiance to surface reflectance, executed through a Docker environment integration with Google Earth Engine.

### 2. `2.deglint_sampling`
Responsible for the correction of the water's specular reflection (sun-glint) based on shortwave infrared (SWIR) bands and the conversion to remote sensing reflectance ($R_{rs}$). It performs the extraction of spectral values exactly at the coordinates and temporal windows of field sampling.

### 3. `3.manage_data`
Focused on data cleaning, structuring, and harmonization. It consists of merging data extracted from different satellites (e.g., Landsat 7 and Landsat 8), removing duplicate values based on temporal criteria, and multi-parameter regression analysis.

<p align="center">
  <img src="3.manage_data/analysis_date.png" alt="Samples Distribution" width="48%">
  <img src="3.manage_data/band_TSS.png" alt="Reflectance and TSS Relation" width="48%">
</p>

### 4. `4.TSS_modeling`
Implementation, calibration, and selection of models to estimate Total Suspended Solids (TSS) or Suspended Particulate Matter (SPM). It evaluates empirical approaches and machine learning algorithms (Random Forest, Gradient Boosting Regressor, SVM etc), using cross-validation to select the most robust predictive model.  
Results can be viewed at: https://ee-curuai.projects.earthengine.app/view/curuai-mosaic-viewer

<p align="center">
  <img src="4.TSS_modeling/metrics_comparison.jpg" alt="TSS Model Comparison" width="800">
</p>

### 5. `5.parameters_by_period`
Temporal stratification of the analysis. It calculates the average metrics and environmental parameters (Area, TSS, Discharge, Wind, Precipitation, and Land Use Classes) grouped by specific hydrological periods of the region (Rising, High Water, Falling, and Low Water), in addition to generating corresponding satellite image mosaics.

<p align="center">
  <img src="5.%20parameters_by_period/TSS_TS_txt_cb.gif" alt="TSS Time Series Animation" width="800">
</p>

### 6. `6.Stat_analysis_and_PCA`
Core of hypothesis testing, correlations, and multivariate integrations. It executes Principal Component Analysis (PCA) and hierarchical temporal trend detection (Linear, Mann-Kendall, and WAVK), consolidating the results into publication-ready visualizations and heatmaps.

<p align="center">
  <img src="6.Stat_analysis/pca_local.jpg" alt="PCA Local" width="48%">
  <img src="6.Stat_analysis/pca_regional.jpg" alt="PCA Regional" width="48%">
</p>

### `datasets` Directory
Contains the analysis results, raw and input datasets necessary to feed the notebooks. Including graphical analysis, images, tables, field measurements and geospatial boundaries used to reproduce the project's analyses and figures.

## Usage and Reproducibility

To reproduce the results presented in the article, follow the steps below:

1. **Data Preparation:** Ensure that the corresponding input data are located in the `datasets/` directory or adjust the paths within the scripts.
2. **Sequential Execution:** Open and execute the notebooks in the numerical order of the directories, starting with `1.py6sCorrection` until completing step 6. The results of one directory act as mandatory input for the subsequent directory.
3. **Computational Environment:** It is recommended to use a Python 3.8+ environment configured with the libraries listed in the project (including `geopandas`, `rasterio`, `numpy`, `pandas`, `scikit-learn`, and `matplotlib`, as well as Jupyter and Earth Engine API integrations). Step 1 has a specific Docker environment documented internally for its execution.
