# ============================================================================
# TIME SERIES SIGNIFICANCE TESTING - WATER-PERIOD-SPECIFIC TREND ANALYSIS
# ============================================================================
# 
# PURPOSE
# Detect and classify temporal trends in environmental time series data,
# separately for each hydrological water period. This allows comparison of
# trend behavior across different seasons of the hydrological cycle.
#
# KEY DIFFERENCE FROM BASE TREND ANALYSIS
# While the base analysis (6.significance_time_series.R) tests trends across
# the entire dataset, THIS script separates data by water period (R, HW, F, LW)
# and performs trend testing within each period. This reveals:
#   - Seasonal variation in trend patterns
#   - Which periods show significant changes
#   - Whether trends are consistent across hydrological phases
#
# WATER PERIOD DEFINITIONS
# R:  Rising water        - Period of stream level increase
# HW: High water          - Peak water period
# F:  Falling water       - Period of stream level decrease  
# LW: Low water           - Minimum water period
#
# HIERARCHICAL TESTING STRATEGY
# For each variable within each water period, tests three trend types:
#   1. LINEAR TRENDS      -> Student's t-test
#   2. MONOTONIC TRENDS   -> Mann-Kendall test
#   3. NON-MONOTONIC TRENDS -> WAVK test
#
# VARIABLES ANALYZED (within each period)
# - area_km2: Watershed surface area (km2)
# - TSS_mean: Total Suspended Solids (mg/L) - PRIMARY VARIABLE
# - precipitation: Rainfall amount (mm)
# - mean_discharge: Stream flow (m3/s)
# - anthropogenic_km2: Human-modified land area (km2)
# - natural_km2: Natural vegetation area (km2)
# - u_wind: Eastward wind component (m/s)
# - v_wind: Northward wind component (m/s)
#
# OUTPUT STRUCTURE
# Separate CSV files for each water_period within each dataset:
#   trends_{suffix}_{water_period}.csv
# Example: trends_local_High_Water.csv, trends_local_Low_Water.csv
#
# ============================================================================

# ========== LIBRARY DEPENDENCIES ==========
# funtimes: Provides notrend_test() function for hierarchical trend testing
#           Includes Student's t-test, Mann-Kendall, and WAVK implementations
# dplyr: Used for select(), filter(), any_of() functions for data manipulation
library(funtimes)
library(dplyr)

# ============================================================================
# SECTION 1: DEFINE THE WATER-PERIOD-SPECIFIC ANALYSIS FUNCTION
# ============================================================================
# This function implements the complete workflow for testing trends separately
# for each water period within a dataset. It uses a nested loop approach:
#   Outer loop:  Iterate through each water period in the data
#   Inner loop:  For each period, test all 8 variables for trends
#
# Function Parameters:
#   input_path:   Path to input CSV file (must have 'water_period' column)
#   output_dir:   Directory where period-specific trend results CSVs are saved
#   label_suffix: Identifier for this analysis ("local", "regional", etc.)
#
# Function Workflow:
#   1. Load data and validate 'water_period' column exists
#   2. Extract unique water periods from data
#   3. FOR EACH WATER PERIOD:
#      a. Filter data to current period only
#      b. Check data sufficiency (minimum 3 observations needed)
#      c. FOR EACH ENVIRONMENTAL VARIABLE:
#         - Apply cascading trend tests (linear -> monotonic -> non-monotonic)
#      d. Save period-specific results to CSV
#   4. Generate console messages showing progress
#
# ============================================================================

ts_significance_analysis <- function(input_path, output_dir, label_suffix) {
  # ========== Set Random Seed for Reproducibility ==========
  # Some statistical tests (particularly WAVK) may involve random processes
  # Setting seed ensures consistent results across multiple script runs
  set.seed(666)
  
  # ========== Display Progress Messages ==========
  # Print analysis status to console for user tracking
  message(paste("\n========================================"))
  message(paste("Starting Trend Analysis for:", label_suffix))
  
  # ========== Validate and Create Output Directory ==========
  # Ensures output folder exists before attempting to write results
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # ========== Load Input Data ==========
  # Check file exists before loading
  if (!file.exists(input_path)) {
    warning(paste("File not found:", input_path))
    return(NULL)
  }
  
  # Read CSV file and convert to data frame
  data <- as.data.frame(read.csv(input_path))
  
  # ========== Validate Water Period Column ==========
  # This script requires a 'water_period' column to function
  # If column is missing, skip this dataset with warning message
  if(!"water_period" %in% names(data)) {
    warning(paste("Skipping", label_suffix, "- 'water_period' column not found."))
    return(NULL)
  }
  
  # ========== Extract Unique Water Periods ==========
  # Identify all distinct water periods present in this dataset
  # Typically: R (Rising), HW (High Water), F (Falling), LW (Low Water)
  unique_periods <- unique(data$water_period)
  message(paste("Found periods:", paste(unique_periods, collapse = ", "))))

  # =========================================================================
  # BEGIN OUTER LOOP: ITERATE THROUGH EACH WATER PERIOD
  # =========================================================================
  # For each distinct water period found in the data, perform separate trend
  # analysis. This nesting structure allows comparison of trend patterns
  # across the hydrological cycle (Rising -> High -> Falling -> Low water).
  #
  for (period in unique_periods) {
    
    # ========== Prepare Period Identifier for Output Filenames ==========
    # Convert water period name to filename-safe format
    # Replaces spaces and special characters with underscores
    # Example: "High Water" -> "High_Water"
    clean_period_name <- gsub("[^A-Za-z0-9]", "_", period)
    message(paste("  Processing period:", period))
    
    # ========== FILTER DATA TO CURRENT WATER PERIOD ==========
    # Extract only rows matching the current period
    # dplyr::filter() preserves data frame structure
    period_data <- filter(data, water_period == period)
    
    # ========== VALIDATE PERIOD DATA SUFFICIENCY ==========
    # Check if period has enough observations for statistical testing
    # Most trend tests (t-test, Mann-Kendall, WAVK) need minimum 3-5 points
    # Skipping periods with <3 observations prevents statistical errors
    if (nrow(period_data) < 3) {
      message(paste("    Skipping - not enough observations (<3)."))
      next
    }
    
    # ========== SELECT VARIABLES TO TEST ==========
    # Extract only columns needed for analysis from the period data
    # Using any_of() allows graceful handling of missing columns
    # (important for robustness across different data versions)
    cols_to_test <- c("area_km2", "TSS_mean", "precipitation", "mean_discharge", 
                      "anthropogenic_km2", "natural_km2", "u_wind", "v_wind")
    
    selecao <- select(period_data, any_of(cols_to_test))
    
    # ========== Initialize Results Storage FOR THIS PERIOD ==========
    # Create empty list to accumulate trend test results for all variables
    # within this water period. Will be bound into data frame later.
    results_list <- list()
    
    # =========================================================================
    # BEGIN INNER LOOP: TEST EACH VARIABLE WITHIN CURRENT WATER PERIOD
    # =========================================================================
    # For each environmental variable in the selected columns, apply the
    # cascading trend tests (linear -> monotonic -> non-monotonic).
    #
    for (var_name in colnames(selecao)) {
      
      # ========== EXTRACT AND CLEAN VARIABLE DATA ==========
      # Pull current variable as numeric vector from data frame
      x <- selecao[[var_name]]
      
      # Remove missing values (NAs) - required for valid statistical testing
      # notrend_test() cannot handle NA values
      x <- x[!is.na(x)]
      
      # ========== VALIDATE VARIABLE DATA ==========
      # Skip testing if:
      #   - Variable has fewer than 2 unique values (constant)
      #   - Variable has fewer than 3 observations (below test minimum)
      # Both conditions prevent mathematical/statistical errors
      if (length(unique(x)) < 2 || length(x) < 3) {
        next 
      }
      
      # =====================================================================
      # STEP 1: TEST FOR LINEAR TREND (Student's t-test)
      # =====================================================================
      # Test for simple monotonic linear trend
      # tryCatch() wrapper prevents mathematical errors from crashing loop
      #   (e.g., if variable has zero variance or other edge cases)
      #
      # Result interpretation:
      #   p < 0.05 -> Significant linear trend detected
      #   p >= 0.05 -> No linear trend (proceed to next test)
      #
      t_test_res <- tryCatch(notrend_test(x, test = "t"), error = function(e) NULL)
      
      if (!is.null(t_test_res) && t_test_res$p.value <= 0.05) {
        # ========== RESULT: LINEAR TREND SIGNIFICANT ==========
        # Record result and skip remaining tests for this variable
        # Note: period column included for later grouping/filtering
        results_list[[var_name]] <- data.frame(
          variable = var_name, 
          P = t_test_res$p.value, 
          trend = "linear",
          period = period
        )
        next 
      }
      
      # =====================================================================
      # STEP 2: TEST FOR MONOTONIC TREND (Mann-Kendall Test)
      # =====================================================================
      # If linear test failed, check for any monotonic trend
      # Mann-Kendall detects directional changes (up/down) without 
      # assuming linear relationship
      #
      # Advantage over t-test:
      #   - Non-parametric: no normality assumption
      #   - Detects non-linear monotonic patterns
      #
      # Result interpretation:
      #   p < 0.05 -> Significant monotonic trend detected
      #   p >= 0.05 -> No monotonic trend (proceed to next test)
      #
      mk_test_res <- tryCatch(notrend_test(x, test = "MK"), error = function(e) NULL)
      
      if (!is.null(mk_test_res) && mk_test_res$p.value <= 0.05) {
        # ========== RESULT: MONOTONIC TREND SIGNIFICANT ==========
        results_list[[var_name]] <- data.frame(
          variable = var_name, 
          P = mk_test_res$p.value, 
          trend = "monotonic",
          period = period
        )
        next
      }
      
      # =====================================================================
      # STEP 3: TEST FOR NON-MONOTONIC TREND (WAVK Test)
      # =====================================================================
      # Most complex trend type: oscillating patterns with direction changes
      # WAVK (Wavelet-based test) detects non-monotonic structures through
      # frequency analysis
      #
      # Useful for water period analysis:
      #   - Seasonal discharge cycles within each period
      #   - Complex wind patterns
      #   - TSS fluctuations without directional trend
      #
      # Result interpretation:
      #   p < 0.05 -> Significant non-monotonic trend detected
      #   p >= 0.05 -> No trend of ANY type -> label as "no-trend"
      #
      wavk_test_res <- tryCatch(
        notrend_test(x, test = "WAVK", factor.length = "adaptive.selection"), 
        error = function(e) NULL
      )
      
      # ========== DETERMINE FINAL TREND CLASSIFICATION ==========
      if (!is.null(wavk_test_res)) {
        if (wavk_test_res$p.value <= 0.05) {
          # WAVK significant = non-monotonic trend behavior detected
          trend_result <- "non-monotonic"
        } else {
          # All three tests failed = no significant trend
          trend_result <- "no-trend"
        }
        
        # ========== RECORD WAVK RESULTS FOR THIS VARIABLE ==========
        results_list[[var_name]] <- data.frame(
          variable = var_name, 
          P = wavk_test_res$p.value, 
          trend = trend_result,
          period = period
        )
      }
    } # ========== END VARIABLE LOOP ==========
    
    # =========================================================================
    # CONSOLIDATE AND SAVE RESULTS FOR THIS WATER PERIOD
    # =========================================================================
    if (length(results_list) > 0) {
      # ========== COMBINE RESULTS INTO DATA FRAME ==========
      # Bind all variable results from this period into single data frame
      final_trend_df <- do.call(rbind, results_list)
      
      # ========== CLEAN UP ROW NAMES ==========
      # Reset automatic numbering for clean output
      row.names(final_trend_df) <- NULL
      
      # ========== GENERATE OUTPUT FILENAME ==========
      # Format: trends_{label_suffix}_{water_period}.csv
      # Example: trends_local_High_Water.csv, trends_regional_Low_Water.csv
      output_filename <- paste0("trends_", label_suffix, "_", clean_period_name, ".csv")
      
      # ========== SAVE PERIOD-SPECIFIC RESULTS ==========
      # Write results to CSV with period label for identification
      write.csv(final_trend_df, file = file.path(output_dir, output_filename), row.names = FALSE)
      
      message(paste("    Saved:", output_filename))
    } else {
      message("    No valid results to save for this period.")
    }
    
  } # ========== END WATER PERIOD LOOP ==========
  
  message(paste("Completed dataset:", label_suffix))
}

# ============================================================================
# SECTION 2: DEFINE DATASETS AND EXECUTE WATER-PERIOD-SPECIFIC ANALYSIS LOOP
# ============================================================================
# This section configures the datasets to be analyzed and executes the
# water-period-specific analysis function for each. Unlike the base analysis
# (which processes 4 dataset combinations), this script processes only
# the MERGED data versions (complete cases only) to avoid data distortion.
#
# Why only MERGED versions?
#   - Water periods are defined in the merged data based on discharge patterns
#   - Filled/interpolated data is less suitable for period-based analysis
#   - Using only merged data ensures cleaner period separation
#
# Dataset Configuration:
#   Each dataset defined as a list with:
#   - path: Input CSV file path (must contain 'water_period' column)
#   - outdir: Output directory for period-specific results CSVs
#   - suffix: Label identifier for output file naming
#
# ============================================================================

# ========== DEFINE BASE PATH FOR DATA FILES ==========
# Parent directory containing all watershed-specific data folders
# NOTE: Update 'base_path' if your folder structure changes
base_path <- "C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df"

# ========== CONFIGURE ANALYSIS DATASETS ==========
# List of 2 datasets (local and regional watersheds with MERGED data)
#
# Dataset 1: MERGED + LOCAL
#   - Source: df_merged_local.csv (complete time series, local watershed)
#   - Analysis: Trends for each water period (R, HW, F, LW) in local area
#   - Outputs: trends_local_Rising.csv, trends_local_High_Water.csv, etc.
#
# Dataset 2: MERGED + REGIONAL  
#   - Source: df_merged_regional.csv (complete cases, regional watershed)
#   - Analysis: Trends for each water period in regional area
#   - Outputs: trends_regional_Rising.csv, trends_regional_High_Water.csv, etc.
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
  )
)

# ========== EXECUTE ANALYSIS LOOP ==========
# For each dataset configuration, apply the ts_significance_analysis function
# This will automatically:
#   1. Extract unique water periods from the data
#   2. Analyze each period separately
#   3. Generate period-specific output CSVs
for (ds in datasets) {
  ts_significance_analysis(ds$path, ds$outdir, ds$suffix)
}

# ========== FINAL STATUS MESSAGE ==========
message("\nAll trend analyses finished.")