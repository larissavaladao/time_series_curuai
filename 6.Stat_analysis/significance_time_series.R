# avaliar series temporais ####

# libraries ######################
library(funtimes)
library(dplyr)

# definir diretório de trabalho ##############################
# NOTE: Update this if you switch computers or folders
setwd("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df/Local Watershed")

# importar dados ##############################
dados <- as.data.frame(read.csv("filled_local.csv"))

# Ensure time formatting (optional but good practice)
# dados$time_start <- as.Date(dados$time_start) 

# selecionar colunas 
# Note: Ensure these column names match your CSV exactly
selecao <- select(dados, area_km2, TSS_mean, precipitation, mean_discharge, 
                  anthropogenic_km2, natural_km2, u_wind, v_wind)

# Initialize an empty list to store results
results_list <- list()

# Loop through each variable (column) in the selection
for (var_name in colnames(selecao)) {
  
  # Extract the vector for the current variable
  x <- selecao[[var_name]]
  
  # Remove NAs if any (notrend_test can fail with NAs)
  x <- x[!is.na(x)]
  
  # ---------------------------------------------------------
  # STEP 1: Test for Linear Trend (Student's t-test)
  # ---------------------------------------------------------
  t_test_res <- notrend_test(x, test = "t")
  
  if (t_test_res$p.value <= 0.05) {
    # If Linear is significant, record it and move to next variable
    results_list[[var_name]] <- data.frame(
      variable = var_name, 
      P = t_test_res$p.value, 
      trend = "linear"
    )
    next 
  }
  
  # ---------------------------------------------------------
  # STEP 2: Test for Monotonic Trend (Mann-Kendall)
  # ---------------------------------------------------------
  mk_test_res <- notrend_test(x, test = "MK")
  
  if (mk_test_res$p.value <= 0.05) {
    # If Monotonic is significant, record it and move to next variable
    results_list[[var_name]] <- data.frame(
      variable = var_name, 
      P = mk_test_res$p.value, 
      trend = "monotonic"
    )
    next
  }
  
  # ---------------------------------------------------------
  # STEP 3: Test for Non-Monotonic Trend (WAVK)
  # ---------------------------------------------------------
  wavk_test_res <- notrend_test(x, test = "WAVK", factor.length = "adaptive.selection")
  
  if (wavk_test_res$p.value <= 0.05) {
    # If WAVK is significant, it's non-monotonic
    trend_result <- "non-monotonic"
  } else {
    # If none were significant, there is no trend
    trend_result <- "no-trend"
  }
  
  # Record the final result (either non-monotonic or no-trend)
  results_list[[var_name]] <- data.frame(
    variable = var_name, 
    P = wavk_test_res$p.value, 
    trend = trend_result
  )
}

# Combine all results into one dataframe
final_trend_df <- do.call(rbind, results_list)

# Reset row names for clean look
row.names(final_trend_df) <- NULL

# View the results in R
print(final_trend_df)

# Write to CSV
write.csv(final_trend_df, "trends/trend_local.csv", row.names = FALSE)