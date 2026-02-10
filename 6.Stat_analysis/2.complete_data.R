# Load libraries once at the start
library(readr)
library(dplyr)
library(mice)
library(tidyr)

# 1. Define the processing function ####
process_watershed_data <- function(input_path, output_dir, output_filename) {
  
  message(paste("Processing:", input_path))
  
  # Import data
  # Check if file exists to avoid errors
  if (!file.exists(input_path)) {
    warning(paste("File not found:", input_path))
    return(NULL)
  }
  
  data <- as.data.frame(read.csv(input_path))
  
  # --- Imputation Step (MICE) ---
  # Select columns for imputation
  data_for_imputation <- select(data, mean_discharge, natural_km2)
  
  # Run MICE (5 datasets, 100 iterations, pmm, seed 500)
  dados_imp <- mice(data_for_imputation, m = 5, maxit = 100, method = "pmm", seed = 500, printFlag = FALSE)
  
  # Use dataset #3
  dados_comp <- complete(dados_imp, 3)
  
  # Prepare final dataframe: remove old columns and X, attach imputed ones
  # Note: Ensuring we don't fail if X doesn't exist
  cols_to_remove <- c("mean_discharge", "natural_km2", "X")
  cols_to_remove <- cols_to_remove[cols_to_remove %in% names(data)]
  
  final_data <- select(data, -all_of(cols_to_remove))
  final_data$mean_discharge <- dados_comp$mean_discharge
  final_data$natural_km2 <- dados_comp$natural_km2
  
  # --- Regression Step (Fill Anthropogenic Area) ---
  # Remove duplicates by year and drop NAs to build the model
  dados_temp1 <- final_data[!duplicated(final_data$year), ]
  dados_temp <- dados_temp1 |> drop_na()
  
  # Create linear regression
  anthropicLm <- lm(anthropogenic_km2 ~ year, data = dados_temp)
  
  # Extract coefficients
  a <- coef(anthropicLm)[1] # Intercept
  b <- coef(anthropicLm)[2] # Slope (Year)
  
  # Fill NAs using the projection for the year 2024
  # (Based on your original code logic: a + b * 2024)
  final_data <- final_data |> 
    dplyr::mutate(anthropogenic_km2 = replace_na(anthropogenic_km2, a + (b * 2024)))
  
  # --- Save Output ---
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  output_full_path <- file.path(output_dir, output_filename)
  write.csv(final_data, file = output_full_path)
  
  message(paste("Saved processed file to:", output_full_path))
  message("------------------------------------------------")
}

# 2. Define your datasets (Input and Output paths) ####
# Adjust 'base_path' to match your local computer
base_path <- "C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df"

datasets <- list(
  list(
    input  = file.path(base_path, "df_merged_local.csv"),
    out_dir = file.path(base_path, "Local Watershed"),
    out_file = "filled_local.csv"
  ),
  list(
    input  = file.path(base_path, "df_merged_regional.csv"),
    out_dir = file.path(base_path, "Regional Watershed"),
    out_file = "filled_regional.csv"
  )
)

# 3. Run the loop ####
for (ds in datasets) {
  process_watershed_data(ds$input, ds$out_dir, ds$out_file)
}