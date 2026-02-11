# avaliar series temporais ####

# libraries ######################
library(funtimes)
library(dplyr)

# 1. Define the Analysis Function ####
ts_significance_analysis <- function(input_path, output_dir, label_suffix) {
  
  message(paste("\n========================================"))
  message(paste("Starting Trend Analysis for:", label_suffix))
  
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Import Data
  if (!file.exists(input_path)) {
    warning(paste("File not found:", input_path))
    return(NULL)
  }
  
  data <- as.data.frame(read.csv(input_path))
  
  # Check if water_period exists
  if(!"water_period" %in% names(data)) {
    warning(paste("Skipping", label_suffix, "- 'water_period' column not found."))
    return(NULL)
  }
  
  # Get unique water periods
  unique_periods <- unique(data$water_period)
  message(paste("Found periods:", paste(unique_periods, collapse = ", ")))

  ######################################################################
  # BEGIN LOOP FOR EACH WATER PERIOD
  ######################################################################
  
  for (period in unique_periods) {
    
    # Clean period name for filename
    clean_period_name <- gsub("[^A-Za-z0-9]", "_", period)
    message(paste("  Processing period:", period))
    
    # Filter data for the current period
    period_data <- filter(data, water_period == period)
    
    # Check data sufficiency (need enough points for time series tests)
    # Most trend tests need at least 3-5 points, but more is better.
    if (nrow(period_data) < 3) {
      message(paste("    Skipping - not enough observations (<3)."))
      next
    }
    
    # Select columns to test
    # NOTE: Ensure these column names match your CSV exactly
    # We use 'any_of' to avoid crashing if a column is missing
    cols_to_test <- c("area_km2", "TSS_mean", "precipitation", "mean_discharge", 
                      "anthropogenic_km2", "natural_km2", "u_wind", "v_wind")
    
    selecao <- select(period_data, any_of(cols_to_test))
    
    # Initialize list for this period's results
    results_list <- list()
    
    # Loop through each variable (column)
    for (var_name in colnames(selecao)) {
      
      x <- selecao[[var_name]]
      x <- x[!is.na(x)] # Remove NAs
      
      # Skip if variable is constant or too short after NA removal
      if (length(unique(x)) < 2 || length(x) < 3) {
        next 
      }
      
      # ---------------------------------------------------------
      # STEP 1: Test for Linear Trend (Student's t-test)
      # ---------------------------------------------------------
      # Wrap in tryCatch to prevent loop crash on mathematical errors
      t_test_res <- tryCatch(notrend_test(x, test = "t"), error = function(e) NULL)
      
      if (!is.null(t_test_res) && t_test_res$p.value <= 0.05) {
        results_list[[var_name]] <- data.frame(
          variable = var_name, 
          P = t_test_res$p.value, 
          trend = "linear",
          period = period # Add period column
        )
        next 
      }
      
      # ---------------------------------------------------------
      # STEP 2: Test for Monotonic Trend (Mann-Kendall)
      # ---------------------------------------------------------
      mk_test_res <- tryCatch(notrend_test(x, test = "MK"), error = function(e) NULL)
      
      if (!is.null(mk_test_res) && mk_test_res$p.value <= 0.05) {
        results_list[[var_name]] <- data.frame(
          variable = var_name, 
          P = mk_test_res$p.value, 
          trend = "monotonic",
          period = period
        )
        next
      }
      
      # ---------------------------------------------------------
      # STEP 3: Test for Non-Monotonic Trend (WAVK)
      # ---------------------------------------------------------
      wavk_test_res <- tryCatch(
        notrend_test(x, test = "WAVK", factor.length = "adaptive.selection"), 
        error = function(e) NULL
      )
      
      if (!is.null(wavk_test_res)) {
        if (wavk_test_res$p.value <= 0.05) {
          trend_result <- "non-monotonic"
        } else {
          trend_result <- "no-trend"
        }
        
        results_list[[var_name]] <- data.frame(
          variable = var_name, 
          P = wavk_test_res$p.value, 
          trend = trend_result,
          period = period
        )
      }
    } # End variable loop
    
    # Save results for this period
    if (length(results_list) > 0) {
      final_trend_df <- do.call(rbind, results_list)
      row.names(final_trend_df) <- NULL
      
      # Define filename: trends_local_High_Water.csv
      output_filename <- paste0("trends_", label_suffix, "_", clean_period_name, ".csv")
      write.csv(final_trend_df, file = file.path(output_dir, output_filename), row.names = FALSE)
      
      message(paste("    Saved:", output_filename))
    } else {
      message("    No valid results to save for this period.")
    }
    
  } # End period loop
  
  message(paste("Completed dataset:", label_suffix))
}

# 2. Define Datasets and Run Loop ####
# NOTE: Update 'base_path' to your specific folder structure
base_path <- "C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df"

datasets <- list(
  list(
    # Input file for Local
    path   = file.path(base_path, "Local Watershed/filled_local.csv"),
    # Output folder for Local Trends
    outdir = file.path(base_path, "Local Watershed/trends"),
    # Suffix
    suffix = "local"
  ),
  list(
    # Input file for Regional
    path   = file.path(base_path, "Regional Watershed/filled_regional.csv"),
    # Output folder for Regional Trends
    outdir = file.path(base_path, "Regional Watershed/trends"),
    # Suffix
    suffix = "regional"
  )
)

# Run the analysis for each dataset
for (ds in datasets) {
  ts_significance_analysis(ds$path, ds$outdir, ds$suffix)
}

message("\nAll trend analyses finished.")