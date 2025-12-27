# Atmospheric Correction with py6s for Sentinel-2 Time Series

This module provides tools for performing atmospheric correction on Landsat satellite imagery using py6s (a Python wrapper for the 6S radiative transfer model) integrated with Google Earth Engine.

## Overview

Atmospheric correction removes the effects of the atmosphere from satellite imagery, converting at-sensor radiance to surface reflectance. This is essential for accurate analysis of multispectral time series data.

## Prerequisites

- Docker installed and running
- Git
- ~5GB of disk space for Docker images

## Setup Instructions

### Step 1: Clone the Base Repository

Clone the Google Earth Engine Jupyter contrib repository:

```bash
git clone https://github.com/gee-community/ee-jupyter-contrib/
cd ee-jupyter-contrib
```

### Step 2: Update Docker Image

Replace the Docker image in the `docker/atmcorr-ee` directory with the custom image provided in the `docker_image` directory of this repository.

### Step 3: Build and Run Docker Container

Navigate to the `docker/atmcorr-ee` folder and build the Docker image:

```bash
docker build . -t atmcorr-ee
docker run -i -t -p 8888:8888 atmcorr-ee
```

This creates a containerized environment with all dependencies for atmospheric correction.

### Step 4: Clone Required Repositories

```bash
git clone https://github.com/samsammurphy/gee-atmcorr-S2
git clone https://github.com/larissavaladao/time_series_curuai.git
```

### Step 5: Set Up Working Directory

Move the correction module into the gee-atmcorr-S2 repository:

```bash
cd time_series_curuai
mv 1.py6sCorrection ../gee-atmcorr-S2
cd ../gee-atmcorr-S2/1.py6sCorrection
```

### Step 6: Run the Correction Notebook

Start the Jupyter notebook for atmospheric correction:

```bash
jupyter-notebook py6s_correction.ipynb --ip='*' --port=8888 --allow-root
```

Access the notebook through your browser at `http://localhost:8888`

## Files

- `py6s_correction.ipynb`: Main notebook for Sentinel-2 atmospheric correction
- `docker_image/`: Custom Docker configuration

## Notes

- The Docker container must be running in the background during Jupyter notebook execution
- Ensure port 8888 is available on your system
