# Data Management and Analysis Notebooks

This directory contains two complementary Jupyter notebooks for managing and analyzing Landsat satellite data combined with field sampling observations.

## Overview

The notebooks process Landsat 7 (LD7) and Landsat 8 (LD8) satellite imagery linked to water quality field measurements from the Curuai floodplain (Amazon region). The workflow includes data cleaning, deduplication, merging, and multi-parameter regression analysis.

---

## Notebook 1: `data_analysis.ipynb`

### Purpose
Load, clean, and merge Landsat satellite sampling datasets (LD7 and LD8), removing duplicates and producing two harmonized output datasets.

### Key Steps

1. **Import Packages**: Load pandas, seaborn, and os for data manipulation and visualization

2. **Load Satellite Data**: 
   - Read `py6s_LD7_data.csv` and `py6s_LD8_data.csv`
   - Both datasets contain spectral band values (blue, green, red, NIR) with windowed statistics (mean, median, min, max, stdDev, count)

3. **Data Quality Filtering**:
   - Keep only depth class 1 samples
   - Remove records with zero or negative spectral means (radiometric errors)
   - Compute absolute date difference (`dif_date_point_abs`) between image acquisition and field sampling

4. **Deduplication Within Satellites**:
   - For each (datetime, SAMPLE_SITE) combination, keep only the image temporally closest to field sampling
   - Separate records into single samples, duplicates, and multiples

5. **Merge Datasets**:
   - Combine cleaned LD7 and LD8 data with satellite origin tags
   - Identify records that appear in both satellite datasets

6. **Cross-Satellite Deduplication**:
   - Generate two output datasets:
     - **`merge_LD_minDate`**: For each sample ID, keeps the image closest in time (LD7 or LD8)
     - **`merge_LD_LD8`**: For each sample ID, prioritizes LD8 images when available

7. **Evaluation**: 
   - Plot distributions of time differences across locations and water periods
   - Compare satellite mission coverage and seasonal patterns

### Output Files
- `merge_min_data.csv`: Temporally-optimized merged dataset
- `merge_LD8prio_data.csv`: LD8-prioritized merged dataset

### Column Descriptions
- `CLOUD_COVER`: Cloud cover percentage in satellite image
- `*band*_mean/median/min/max/stdDev`: Spectral statistics within sampling window
- `count_pixel`: Number of valid (non-masked) pixels
- `datetime`: Image acquisition date
- `dif_date_point`: Days between image and field sampling
- `system_index`: Image identifier in GEE assets
- `LOCATION`: Study site (e.g., Amazon locations)
- `WATER_PERIOD`: Hydrological period (Rising, High Water, Falling, Low Water)

---

## Notebook 2: `data_analysis2.ipynb`

### Purpose
Analyze water quality parameters using Landsat spectral data across different hydrological periods. Perform regression analysis to identify spectral-parameter relationships.

### Key Steps

1. **Import Data**:
   - Load merged datasets: `min_date.csv` and `LD8_priority.csv`
   - Drop index columns and inspect data types

2. **Data Cleaning**:
   - Replace detection limit strings (`'< 0,01'`, `'< 0,001'`, etc.) with 0
   - Convert specified columns to float64 (CHLOROPHYLL_B, POC, P_ORGANIC, P_TOTAL, TURBIDITY)

3. **SPM Analysis** (Suspended Particulate Matter):
   - Create 4×2 subplots (4 bands × 2 datasets) for each water period
   - Plot scatter of spectral values vs. SPM
   - Overlay linear regression lines with equations and R² values
   - Visualize patterns separately for min-date and LD8-priority datasets

4. **Water Quality Parameters** (by water period):
   - **Chlorophyll**: CHLOROPHYLL, CHLOROPHYLL_A, CHLOROPHYLL_B
   - **Organic Carbon**: DOC (Dissolved), POC (Particulate), TOC (Total)
   - **Nitrogen**: N_TOTAL, N_TOTAL_DISSOLVED
   - **Phosphorus**: P_ORGANIC, P_TOTAL
   - **Silica & Turbidity**: SILICA, TURBIDITY

### Visualizations

#### SPM Subplots
- **Format**: 4 rows (blue, green, red, NIR bands) × 2 columns (min-date vs. LD8 priority)
- **Per Water Period**: Separate figure for each hydrological condition
- **Features**:
  - Scatter points colored by water period
  - Linear regression line with equation and R² value
  - Grid background for readability
