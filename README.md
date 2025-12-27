# time_series_curuai — Implementation of the **Curuai time-series article**

This folder contains the code and notebooks used to implement the analyses presented in the **Curuai floodplain time-series article**. The project is organized as a sequential pipeline: directories are numbered in the order the steps should be run.

Where the article is implemented
- The article's code and notebooks are implemented across the numbered directories below. Run them in numerical order (1 → 6) to reproduce the workflow and results.


Directory (pipeline) overview
- `1.py6sCorrection`: Atmospheric correction using Py6S (prepare top-of-atmosphere to surface reflectance).
- `2.deglint_sampling`: Sun-glint removal / deglint sampling and tests for aquatic scenes.
- `3.manage_data`: Data management and preprocessing, stacking bands, organizing time-series and the main analysis notebooks.
- `4.SPM_model`: Suspended Particulate Matter (SPM) modelling and calibration notebooks.
- `5. parameters_by_period`: Estimation of model parameters per acquisition period and temporal stratification.
- `6.PCA_analysis`: Principal Component Analysis and exploratory analyses used in the article.
- `datasets`: Example or sample input datasets used to reproduce figures and model runs.

Usage
- Open the notebooks in order, starting at `1.py6sCorrection` and following the numbered directories. Each folder contains notebooks and helper scripts for that step.
- Typical environment: Python 3.8+ with Jupyter, geopandas, rasterio, numpy, pandas, scikit-learn, matplotlib (adjust packages as needed). See project-level `requirements.txt` or add a conda environment if desired.

Contributing & Reproducibility
- Notebooks are the primary deliverable; to reproduce results, prepare input data in `datasets/` (or point notebooks to your data paths) and run the pipeline sequentially.
- If you'd like, I can add a `requirements.txt` or a `environment.yml` listing exact dependencies.

Contact
- If something is unclear or you want the README translated or extended, tell me which language and what extra details to include.
