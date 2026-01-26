# Time Series Parameter Analysis - Curuai Watershed

## Overview

This directory contains a comprehensive workflow for obtaining the mean values for the environmental parameters in each hydrological period of each year (except for lulc classes, which are annual). It also implements alysis and vizualization for environmental time series data from the Curuai watershed. The analysis integrates multiple data sources including water level periods, satellite imagery, and environmental parameters to produce publication-quality time series visualizations.

## Execution Order

**The notebooks must be executed in sequential order** as each one depends on outputs from previous steps. Follow this sequence:

### 1. **1.set_periods.ipynb** - Define Hydrological Periods
- **Purpose**: Identifies and defines water periods (flood/drought seasons) based on discharge data
- **Input**: Raw hydrological time series data
- **Output**: Water period definitions saved to dataset directory
- **Key Functions**: Peak detection algorithm to identify seasonal patterns

### 2. **2.landsat_water_period_mosaic.ipynb** - Generate Satellite Mosaics
- **Purpose**: Creates Landsat satellite image mosaics for each defined water period
- **Input**: Water period definitions from step 1
- **Output**: Processed satellite imagery organized by water period
- **Tools**: Google Earth Engine for satellite data processing
- **Note**: Requires Earth Engine authentication

### 3. **3.area_spm_time_series.ipynb** - Calculate Area and Water Quality Metrics
- **Purpose**: Extracts surface area and suspended particulate matter (SPM) time series from satellite imagery
- **Input**: Landsat mosaics from step 2
- **Output**: Area and SPM values aggregated by water period
- **Metrics**: 
  - Open water surface area (km²)
  - Suspended particulate matter concentration

### 4. **4.chirps_time_series.ipynb** - Process Precipitation Data
- **Purpose**: Downloads and processes precipitation data from CHIRPS satellite dataset
- **Input**: Study area boundaries
- **Output**: Accumulated precipitation for each water period (mm)
- **Source**: Climate Hazards Group Infrared Precipitation with Stations (CHIRPS)

### 5. **5.discharge_timeseries.ipynb** - Compile River Discharge Data
- **Purpose**: Processes river discharge measurements and aggregates by water period
- **Input**: In-situ discharge monitoring data
- **Output**: Mean discharge values (m³/s) for each period

### 6. **6.wind_time_series.ipynb** - Extract Wind Speed Data
- **Purpose**: Processes wind speed measurements from meteorological stations
- **Input**: Wind speed observations
- **Output**: Mean wind speed (m/s) for each water period

### 7. **7.mapbiomas_toolkit_att.js** - Land Cover Classification (Optional)
- **Purpose**: JavaScript script for MapBiomas land cover classification
- **Type**: Google Earth Engine script
- **Use**: For detailed land use/land cover analysis

### 8. **8.land_cover_time_series.ipynb** - Visualize Land Cover Changes
- **Purpose**: Generates time series visualizations of land cover changes by watershed and class
- **Input**: Consolidated LULC data
- **Output**: Grid plots showing Natural vs Anthropic land cover trends over time
- **Features**: 
  - Separate subplot for each watershed-class combination
  - 4-row grid layout for easy comparison

---

## Results and Visualizations

### Output Gif Files

The following animated GIF files visualize the time series analysis results:

| File | Description |
|------|-------------|
| **time_series.gif** | Animated time series showing all parameters (Area, TSS, Discharge, Wind, Precipitation) |
| **time_series_txt.gif** | Annotated version with text labels and parameter values |
| **TSS_time_series.gif** | Focused animation on Total Suspended Solids trends |
| **TSS_time_series_txt.gif** | Annotated TSS time series with labeled values |

### Output Directory Structure

```
Area/
├── LULC_time_series.jpg          # Land cover change plots
├── Area_time_series.jpg          # Water surface area trends
└── [other area-specific plots]

Results/
├── lulc_data.csv                 # Consolidated land cover data
├── Area/                         # Area-related visualizations
├── TSS/                          # Total suspended solids plots
├── Precipitation/                # Precipitation visualizations
├── Discharge/                    # River discharge plots
└── Wind/                         # Wind speed plots
```

---

## Data Requirements

Before running this workflow, ensure you have:

1. **Raw Data Files**
   - Discharge and water level measurements
   - Land use/land cover classification data
   - Satellite imagery access (via Google Earth Engine)

2. **Required Packages**
   - `pandas`: Data manipulation
   - `numpy`: Numerical computing
   - `matplotlib`: Visualization
   - `seaborn`: Statistical graphics
   - `geemap`: Earth Engine Python interface
   - `ee` (earthengine-api): Google Earth Engine access
   - `scipy`: Signal processing

3. **Authentication**
   - Google Earth Engine account and authentication
   - Appropriate permissions for data access

---

## Key Parameters by Period

The analysis organizes all data by **water periods**, defined as:
- **Flood Period**: High discharge, high water levels
- **Drought Period**: Low discharge, low water levels

Time series metrics are calculated for each period:
- **Area** (km²): Open water surface area
- **TSS** (mg/L): Total suspended solids concentration  
- **Discharge** (m³/s): River discharge rate
- **Wind** (m/s): Mean wind speed
- **Precipitation** (mm): Accumulated rainfall per period

---

## Usage Notes

- **Sequential Execution**: Each notebook builds on previous outputs. Do not skip or reorder steps.
- **Earth Engine**: Steps 2-3 require Google Earth Engine authentication via `geemap.ee_authenticate()`
- **Memory Requirements**: Processing satellite imagery (step 2-3) may require adequate computational resources
- **Output Storage**: Ensure sufficient disk space for satellite imagery and processed data
- **Customization**: Update directory paths in each notebook if using a different data location

---

## Contact & Documentation

For detailed methodology and parameter definitions, refer to individual notebook documentation.
