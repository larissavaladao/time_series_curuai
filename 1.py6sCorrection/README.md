
## `1.py6sCorrection` — Atmospheric correction (Py6S)

This step performs atmospheric correction on Landsat imagery using the Py6S-based workflow included in the repository. It produces surface reflectance outputs used by later pipeline steps.

Prerequisites
- Python 3.8+ and Jupyter.
- use the provided Docker image.
- Google Earth Engine account for GEE-based scripts.
- Run inside the `atmcorr-ee` Docker container described below.

Quick start (Docker recommended)
1. Clone helper repos (if not already present):

```bash
git clone https://github.com/gee-community/ee-jupyter-contrib.git
git clone https://github.com/samsammurphy/gee-atmcorr-S2.git
git clone https://github.com/larissavaladao/time_series_curuai.git
```

2. Build and run the Docker image (runs Jupyter inside container):

```bash
cd ee-jupyter-contrib/atmcorr-ee
docker build . -t atmcorr-ee
docker run -it -p 8888:8888 atmcorr-ee
```

3. From inside the container (or locally if dependencies are installed):

```bash
# move or link this step into the atmcorr repo (only if needed)
mv ~/time_series_curuai/1.py6sCorrection ../gee-atmcorr-S2/ || true
cd ../gee-atmcorr-S2/1.py6sCorrection
jupyter-notebook py6s_correction.ipynb --ip='*' --port=8888 --allow-root
```

Running the notebook
- Open `py6s_correction.ipynb` and follow the notebook cells in order. Cells include sections to:
	- authenticate with Google Earth Engine (if used),
	- load input imagery, set correction parameters, and
	- export corrected surface reflectance rasters to local disk or Google Drive/Cloud Storage.

Inputs & outputs
- Inputs: Landsat TOA images (local or from GEE).
- Outputs: surface reflectance GeoTIFFs (saved to GEE asset storage as configured).

Tips
- If you use GEE, import earthengine package, authenticate and initiatialize your project before running the notebook.
- Adjust notebook paths and export targets at the top of the notebook to match your GEE account.
