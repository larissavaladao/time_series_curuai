# dados dos mosaicos com periodos de água definidos por cotas em Obidos ####
# definir diretório de trabalho ####
# NOTE: Update this path to your specific local directory before running
setwd("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df")

# import libraries ####
library(dplyr)
library(ggpubr)
library(moments)
library(corrplot)
library(psych)
library(ggplot2)

# import data ####
# NOTE: Ensure 'filled_data.csv' contains the 'water_period' column
data <- as.data.frame(read.csv("filled_data.csv"))

# Clean data
data <- select(data, -X, -band_count)

# Check if water_period exists
if(!"water_period" %in% names(data)) {
  stop("The column 'water_period' was not found in the dataframe.")
}

# Get unique water periods
unique_periods <- unique(data$water_period)
print(paste("Found the following periods:", paste(unique_periods, collapse = ", ")))

######################################################################
# BEGIN LOOP FOR EACH WATER PERIOD
######################################################################

for (period in unique_periods) {
  
  print(paste("Processing period:", period))
  
  # 1. Filter data for the current period
  period_data <- filter(data, water_period == period)
  
  # Check if there is enough data
  if (nrow(period_data) < 3) {
    print(paste("Skipping period", period, "- not enough observations."))
    next
  }
  
  # 2. Define dynamic filenames based on the period
  # Clean period name to avoid invalid characters in filenames
  clean_period_name <- gsub("[^A-Za-z0-9]", "_", period)
  
  pdf_filename <- paste0("local_plots_", clean_period_name, ".pdf")
  shapiro_csv_name <- paste0("shapiro_", clean_period_name, ".csv")
  corr_r_name <- paste0("corr_local_", clean_period_name, ".csv")
  corr_p_name <- paste0("corr_p_loc", clean_period_name, ".csv")
  corr_se_name <- paste0("corr_se_loc", clean_period_name, ".csv")
  corr_ci_name <- paste0("corr_ci_loc", clean_period_name, ".csv")
  corr_si_name <- paste0("corr_si_loc", clean_period_name, ".csv")

  # Open the PDF graphics device for this period
  pdf(pdf_filename, width = 8, height = 6)
  
  ################################################################################
  # Normality Tests & Plots
  ################################################################################
  
  # Function to plot to avoid repetitive code
  plot_normality <- function(df, col_name, title_text, x_lab) {
    # Check for NA
    clean_vec <- df[[col_name]][!is.na(df[[col_name]])]
    
    if(length(clean_vec) > 3) {
      # Tests
      print(paste("---", title_text, "---"))
      print(shapiro.test(clean_vec))
      print(skewness(clean_vec))
      
      # Plots
      print(ggqqplot(df[[col_name]], title=title_text))
      print(ggdensity(df, x = col_name, fill = "lightgray", title=title_text) +
              stat_overlay_normal_density(color = "red", linetype = "dashed"))
      boxplot(df[[col_name]], xlab=title_text, main=paste("Boxplot -", period))
    }
  }

  ### Run plots for variables
  plot_normality(period_data, "area_km2", "Surface Area (km2)", "Surface Area (km2)")
  plot_normality(period_data, "TSS_mean", "TSS", "TSS")
  plot_normality(period_data, "mean_loc_chirps", "Precipitation", "Precipitation")
  plot_normality(period_data, "mean_discharge", "Discharge", "Discharge")
  plot_normality(period_data, "u_wind_loc", "Wind Eastward", "Wind Eastward")
  plot_normality(period_data, "v_wind_loc", "Wind Northward", "Wind Northward")
  plot_normality(period_data, "anthropogenic_km2", "Anthropic Area", "Anthropic Area")
  plot_normality(period_data, "natural_km2", "Natural Area", "Natural Area")

  ################################################################################
  # Shapiro-Wilk Table Export
  ################################################################################
  
  vars <- c("area_km2", "TSS_mean", "TSS_max", "TSS_min", "mean_loc_chirps",
            "mean_discharge", "u_wind_loc", "v_wind_loc", "anthropogenic_km2", "natural_km2")
  
  shapiro_results <- lapply(vars, function(varname) {
    x <- period_data[[varname]]
    x <- x[!is.na(x)]
    n <- length(x)
    if (n < 3) {
      return(data.frame(variable = varname, W = NA_real_, p.value = NA_real_, 
                        normality = NA_character_, n = n, stringsAsFactors = FALSE))
    }
    
    sampled_n <- n
    if (n > 5000) {
      set.seed(123)
      x <- sample(x, 5000)
      sampled_n <- 5000
    }
    
    test <- shapiro.test(x)
    W <- unname(test$statistic)
    p <- unname(test$p.value)
    normality <- ifelse(is.na(p), NA_character_, ifelse(p >= 0.05, "normal", "not normal"))
    data.frame(variable = varname, W = W, p.value = p, normality = normality, n = sampled_n, stringsAsFactors = FALSE)
  })
  
  shapiro_df <- do.call(rbind, shapiro_results)
  shapiro_df$period <- period # Add period column for reference
  write.csv(shapiro_df, file = shapiro_csv_name, row.names = FALSE)
  
  ###############################################################################
  # CORRELATION
  ###############################################################################
  
  ## Graphical view (Kendall)
  # Defined specific plot function for correlation to save space
  plot_corr <- function(y_var, y_lab) {
    print(ggscatter(period_data, x = "TSS_mean", y = y_var, add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "kendall", 
                    title = paste("Kendall Corr (", period, ")"), 
                    xlab = "TSS", ylab = y_lab))
  }
  
  plot_corr("area_km2", "Surface Area")
  plot_corr("mean_loc_chirps", "Precipitation")
  plot_corr("mean_discharge", "Discharge")
  plot_corr("u_wind_loc", "Eastward Wind")
  plot_corr("v_wind_loc", "Northward Wind")
  plot_corr("anthropogenic_km2", "Anthropic Area")
  plot_corr("natural_km2", "Natural Area")
  
  ## Correlation Matrix ####
  
  data_select <- select(period_data, TSS_mean, area_km2, mean_loc_chirps,
                        mean_discharge, u_wind_loc, v_wind_loc, anthropogenic_km2)
  
  # Ensure we have numeric data and enough rows
  data_select <- na.omit(data_select)
  
  if(nrow(data_select) > 4) {
    # Using psych::corr.test instead of corTest
    cor_result <- psych::corr.test(data_select, method = "kendall", normal = FALSE)
    
    # Plot mixed correlation matrix
    corrplot::corrplot.mixed(cor_result$r, lower = "number", las = 1, 
                             title = paste("Correlation Matrix -", period), 
                             mar=c(0,0,2,0)) # Add margin for title
    
    # Save CSVs
    write.csv(cor_result$r, file = corr_r_name)
    write.csv(cor_result$p, file = corr_p_name)
    write.csv(cor_result$se, file = corr_se_name)
    write.csv(cor_result$ci, file = corr_ci_name)
    # corr.test does not return 'stars' directly, usually part of summary or print
    # We can create a simple significance matrix if needed, or rely on p-values.
    # If your version of 'corTest' was custom, this might differ.
  } else {
    print(paste("Not enough complete pairs for correlation matrix in period:", period))
  }
  
  # Close PDF for this period
  dev.off()
  print(paste("Finished processing:", period))
}

print("All periods processed.")

# Close the device to save plots as the pdf file
dev.off()
