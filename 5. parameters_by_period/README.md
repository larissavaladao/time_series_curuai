# Time Series Parameter Analysis - Curuai Watershed

## Overview

This directory contains a comprehensive workflow for obtaining the mean values for the environmental parameters in each hydrological period of each year (except for lulc classes, which are annual). It also implements alysis and vizualization for environmental time series data from the Curuai watershed. The analysis includes multiple data sources including water level periods, satellite imagery, and environmental parameters to produce publication-quality time series visualizations.

![TSS MODELING](time_series_curuai\5. parameters_by_period\TSS_TS_txt_cb.gif)


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

### 3. **3.area_tss_time_series.ipynb** - Calculate Area and Water Quality Metrics
- **Purpose**: Extracts surface area and suspended particulate matter (TSS) time series from satellite imagery
- **Input**: Landsat mosaics from step 2
- **Output**: Area and TSS values aggregated by water period
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
| **time_series_txt.gif** | Animated time series showing Curuai image mosaics 
| **TSS_time_series_txt.gif** | Focused animation on Total Suspended Solids trends |

### Output Directory Structure

```

Results/
├── lulc_data.csv                 # Consolidated land cover data
├── Area/                         # Area-related visualizations
├── TSS/                          # Total suspended solids plots
├── Precipitation/                # Precipitation visualizations
├── Discharge/                    # River discharge plots
└── Wind/                         # Wind speed plots
```

---

## Key Parameters by Period

The analysis organizes all data by **water periods**, defined in notebook 1.

Time series metrics are calculated for each period:
- **Area** (km²): Flooded water surface area
- **TSS** (mg/L): Total suspended solids concentration  
- **Discharge** (m³/s): River discharge rate
- **Wind** (m/s): Mean wind speed
- **Precipitation** (mm): Accumulated rainfall per period

# Example Results


![Mosaic Time Series](.\time_series_txt.gif)

![TSS Model Time Series](.\TSS_time_series_txt.gif)
