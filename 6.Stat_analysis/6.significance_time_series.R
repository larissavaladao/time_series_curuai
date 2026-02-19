# ============================================================================
# TIME SERIES SIGNIFICANCE TESTING - TREND ANALYSIS
# ============================================================================
# 
# PURPOSE
# Detect and classify temporal trends in environmental time series data for
# water quality monitoring across local and regional watersheds.
#
# ANALYTICAL APPROACH
# This script uses a hierarchical (cascading) testing strategy that tests for
# three types of trends in order of complexity:
#   1. LINEAR TRENDS      → Student's t-test (detects monotonic linear patterns)
#   2. MONOTONIC TRENDS   → Mann-Kendall test (detects any monotonic direction)
#   3. NON-MONOTONIC TRENDS → WAVK test (detects complex oscillating patterns)
#
# The script stops at the first statistically significant trend found (p < 0.05)
# and records that trend type, allowing efficient classification of all variables.
#
# VARIABLES ANALYZED
# - area_km2: Watershed surface area (km²)
# - TSS_mean: Total Suspended Solids (mg/L) - PRIMARY VARIABLE
# - precipitation: Rainfall amount (mm)
# - mean_discharge: Stream flow (m³/s)
# - anthropogenic_km2: Human-modified land area (km²)
# - natural_km2: Natural vegetation area (km²)
# - u_wind: Eastward wind component (m/s)
# - v_wind: Northward wind component (m/s)
#
# OUTPUT
# CSV files containing: variable name, p-value, and trend classification
# (linear | monotonic | non-monotonic | no-trend)
#
# DATASETS PROCESSED
# - df_merged_local.csv:      Complete cases, local watershed only
# - df_merged_regional.csv:   Complete cases, regional watershed
# - df_fillTSS_local.csv:     Interpolated-fill version, local watershed
# - df_fillTSS_regional.csv:  Interpolated-fill version, regional watershed
#
# ============================================================================

# ========== LIBRARY DEPENDENCIES ==========
# funtimes: Provides notrend_test() function for hierarchical trend testing
#           Includes Student's t-test, Mann-Kendall, and WAVK test implementations
# dplyr: Used for select() function to extract specific columns from data frame
library(funtimes)
library(dplyr)

# ============================================================================
# SECTION 1: DEFINE THE TIME SERIES SIGNIFICANCE ANALYSIS FUNCTION
# ============================================================================
# This function encapsulates the entire workflow for testing trends in a single
# dataset. It is designed to be called once for each watershed type × data
# version combination (local/regional, merged/filled).
#
# Function Parameters:
#   input_path:   Path to input CSV file containing time series data
#   output_dir:   Directory where trend results CSV will be saved
#   label_suffix: Identifier for this analysis ("local", "regional", etc.)
#
# Function Workflow:
#   1. Validate input file exists
#   2. Load data and format date column
#   3. Select analysis variables (area, TSS, precipitation, discharge, etc.)
#   4. For each variable: Apply cascading trend tests
#   5. Consolidate results into single data frame
#   6. Export results to CSV
#
# ============================================================================

ts_significance_analysis <- function(input_path, output_dir, label_suffix) {
  # ========== Set Random Seed for Reproducibility ==========
  # Some statistical tests (particularly WAVK) may involve random processes
  # Setting seed ensures consistent results across multiple script runs
  set.seed(666)
  
  message(paste("Processing dataset:", label_suffix))
  
  # ========== Validate and Create Output Directory ==========
  # Ensures output folder exists before attempting to write trend results
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # ========== Load Input Data ==========
  # Check file exists before attempting to load
  if (!file.exists(input_path)) {
    warning(paste("File not found:", input_path))
    return(NULL)
  }
  
  # Read CSV and convert to data frame
  dados <- as.data.frame(read.csv(input_path))
  
  # ========== Format Time Column ==========
  # Convert time_start column to Date class for proper temporal handling
  # (optional but good practice for time series analysis)
  dados$time_start <- as.Date(dados$time_start) 

  # ========== Select Analysis Variables ==========
  # Extract only columns needed for trend analysis
  # These 8 variables represent water quality, watershed characteristics, and climate factors
  selecao <- select(dados, area_km2,  TSS_mean, precipitation, mean_discharge, 
                    anthropogenic_km2, natural_km2, u_wind, v_wind)

  # ========== Initialize Results Storage ==========
  # Create empty list to accumulate results from each variable's trend test
  # Will be converted to data frame at end of function
  results_list <- list()

  # ========== BEGIN VARIABLE LOOP ==========
  # Iterate through each environmental variable (column) and test for trends
  for (var_name in colnames(selecao)) {
    
    # ========== Extract and Clean Variable Data ==========
    # Extract current variable as a vector
    x <- selecao[[var_name]]
    
    # Remove missing values (NAs)
    # The notrend_test() function cannot handle missing values, so they must be removed
    # This may reduce sample size but is necessary for test validity
    x <- x[!is.na(x)]
    
    # =========================================================================
    # STEP 1: TEST FOR LINEAR TREND (Student's t-test)
    # =========================================================================
    # This is the simplest trend type: a monotonic increase or decrease
    # Student's t-test evaluates whether the linear slope is statistically
    # significant (different from zero).
    #
    # Result interpretation:
    #   p < 0.05 → Significant linear trend detected
    #   p >= 0.05 → No significant linear trend (proceed to next test)
    #
    t_test_res <- notrend_test(x, test = "t")
    
    if (t_test_res$p.value <= 0.05) {
      # ========== RESULT: LINEAR TREND SIGNIFICANT ==========
      # If linear trend is significant at 5% level, record it and skip remaining tests
      # (no need to test for more complex trends if simple trend is significant)
      results_list[[var_name]] <- data.frame(
        variable = var_name, 
        P = t_test_res$p.value, 
        trend = "linear"
      )
      next 
    }
    
    # =========================================================================
    # STEP 2: TEST FOR MONOTONIC TREND (Mann-Kendall Test)
    # =========================================================================
    # If linear test failed, check for ANY monotonic trend (not necessarily linear)
    # The Mann-Kendall test detects whether values tend to increase (or decrease)
    # over time, without assuming a linear relationship.
    #
    # Advantage over t-test:
    #   - Non-parametric: doesn't assume normal distribution
    #   - Detects non-linear monotonic patterns (e.g., exponential growth)
    #
    # Result interpretation:
    #   p < 0.05 → Significant monotonic trend detected
    #   p >= 0.05 → No monotonic trend (proceed to next test)
    #
    mk_test_res <- notrend_test(x, test = "MK")
    
    if (mk_test_res$p.value <= 0.05) {
      # ========== RESULT: MONOTONIC TREND SIGNIFICANT ==========
      # If monotonic trend is significant, record it and skip the final test
      results_list[[var_name]] <- data.frame(
        variable = var_name, 
        P = mk_test_res$p.value, 
        trend = "monotonic"
      )
      next
    }
    
    # =========================================================================
    # STEP 3: TEST FOR NON-MONOTONIC TREND (WAVK Test)
    # =========================================================================
    # This is the most complex trend type: oscillating patterns that change direction
    # The WAVK (Wavelet-based test) detects any non-monotonic structures in the data
    # by analyzing different frequency components.
    #
    # Advantages:
    #   - Detects complex oscillations that are missed by simpler tests
    #   - Useful for climate/discharge data with seasonal cycles
    #   - "adaptive.selection" parameter auto-selects appropriate wavelet scales
    #
    # Result interpretation:
    #   p < 0.05 → Significant non-monotonic trend detected
    #   p >= 0.05 → No significant trend of any type (label as "no-trend")
    #
    wavk_test_res <- notrend_test(x, test = "WAVK", factor.length = "adaptive.selection")
    
    # ========== DETERMINE FINAL TREND CLASSIFICATION ==========
    if (wavk_test_res$p.value <= 0.05) {
      # If WAVK is significant, the variable shows non-monotonic trend behavior
      trend_result <- "non-monotonic"
    } else {
      # If linear, monotonic, AND non-monotonic tests all failed,
      # the variable shows no significant trend
      trend_result <- "no-trend"
    }
    
    # ========== RECORD RESULT FOR THIS VARIABLE ==========
    # Store the WAVK test results (final test in the cascade)
    # results_list will be combined with all other variable results later
    results_list[[var_name]] <- data.frame(
      variable = var_name, 
      P = wavk_test_res$p.value, 
      trend = trend_result
    )
  }

  # ========== CONSOLIDATE RESULTS ==========
  # Convert results_list (list of data frames) into single combined data frame
  # Each row represents one variable with its trend classification
  final_trend_df <- do.call(rbind, results_list)

  # ========== CLEAN UP ROW NAMES ==========
  # Reset automatic row names (1, 2, 3, ...) for cleaner output
  row.names(final_trend_df) <- NULL

  # ========== OUTPUT RESULTS ==========
  # Print results table to console for immediate review
  print(final_trend_df)
  
  # ========== SAVE RESULTS TO CSV ==========
  # Write trend results to CSV file for documentation and downstream analysis
  # Filename format: trends_{label_suffix}.csv
  # Row 1: variable name | p-value | trend classification
  write.csv(final_trend_df, file = file.path(output_dir, paste0("trends_", label_suffix, ".csv")))
  print(paste("Trend analysis completed for:", label_suffix))
}

# ============================================================================
# SECTION 2: DEFINE DATASETS AND EXECUTE ANALYSIS LOOP
# ============================================================================
# This section configures all datasets to be analyzed and executes the analysis
# function for each one. The script processes both watershed scales and both
# data versions (merged complete cases vs. interpolated-filled).
#
# Dataset Configuration Strategy:
#   Each dataset is defined as a list with:
#   - path: Full path to input CSV file
#   - outdir: Output directory for results CSV
#   - suffix: Label identifier for output file naming
#
# Four Dataset Combinations:
#   1. LOCAL WATERSHED, MERGED:    Complete cases, local area only
#   2. REGIONAL WATERSHED, MERGED: Complete cases, regional area
#   3. LOCAL WATERSHED, FILLED:    Interpolated TSS, local area
#   4. REGIONAL WATERSHED, FILLED: Interpolated TSS, regional area
#
# ============================================================================

# ========== DEFINE BASE PATH FOR DATA FILES ==========
# Parent directory containing all watershed-specific data folders
# NOTE: Update 'base_path' if your folder structure changes
base_path <- "C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df"

# ========== CONFIGURE ANALYSIS DATASETS ==========
# List of 4 datasets, each defined with input file, output directory, and label
#
# Dataset 1: MERGED + LOCAL
#   - Source: df_merged_local.csv (complete time series cases, local watershed only)
#   - Analysis: Tests for trends in undisturbed (gap-free) local data
#   - Output: Local Watershed/trends/trends_local.csv
#
# Dataset 2: MERGED + REGIONAL  
#   - Source: df_merged_regional.csv (complete cases, regional watershed)
#   - Analysis: Tests for trends in undisturbed regional data
#   - Output: Regional Watershed/trends/trends_regional.csv
#
# Dataset 3: FILLED + LOCAL
#   - Source: df_fillTSS_local.csv (interpolated TSS values, local watershed)
#   - Analysis: Tests for trends with missing TSS values reconstructed
#   - Output: Local Watershed Fill/trends/trends_local.csv
#
# Dataset 4: FILLED + REGIONAL
#   - Source: df_fillTSS_regional.csv (interpolated TSS values, regional watershed)
#   - Analysis: Tests for trends with missing TSS values reconstructed
#   - Output: Regional Watershed Fill/trends/trends_regional.csv
#
datasets <- list(
  list(
    path   = file.path(base_path, "df_merged_local.csv"),
    outdir = file.path(base_path, "Local Watershed/trends"),
    suffix = "local"
  ),
  list(
    path   = file.path(base_path, "df_merged_regional.csv"),
    outdir = file.path(base_path, "Regional Watershed/trends"),
    suffix = "regional"
  ),
  list(
    path   = file.path(base_path, "df_fillTSS_local.csv"),
    outdir = file.path(base_path, "Local Watershed Fill/trends"),
    suffix = "local"
  ),
  list(
    path   = file.path(base_path, "df_fillTSS_regional.csv"),
    outdir = file.path(base_path, "Regional Watershed Fill/trends"),
    suffix = "regional"
  )
)

# ========== EXECUTE ANALYSIS LOOP ==========
# Iterate through each dataset and apply the ts_significance_analysis function
# Each iteration tests all 8 environmental variables for trends
for (ds in datasets) {
  ts_significance_analysis(ds$path, ds$outdir, ds$suffix)
}
