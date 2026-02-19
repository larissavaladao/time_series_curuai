################################################################################
# Distribution & Correlation Analysis Script
# 
# Purpose: Analyze statistical distributions and correlations across environmental
# variables (TSS, precipitation, discharge, wind, land cover) for local and regional
# watersheds. Generates diagnostic plots, normality tests, and correlation matrices.
#
# Outputs:
# - PDF plots with Q-Q plots, density plots, and boxplots
# - Shapiro-Wilk normality test results (CSV)
# - Kendall correlation matrix and p-values (CSV)
# - Correlation significance indicators (CSV)
################################################################################

# Load required libraries
library(dplyr)        # Data manipulation
library(ggpubr)       # Publication-ready ggplot2 plots
library(moments)      # Skewness and kurtosis calculations
library(corrplot)     # Correlation matrix visualization
library(psych)        # Correlation testing with Kendall method
library(ggplot2)      # Data visualization

################################################################################
# SECTION 1: Define Main Analysis Function
################################################################################
# This function performs complete statistical analysis on watershed data:
# 1. Normality testing (Shapiro-Wilk, Kolmogorov-Smirnov, Skewness)
# 2. Q-Q and density plots with normal distribution overlay
# 3. Kendall correlation analysis (non-parametric, suitable for non-normal data)
# 4. Correlation matrix visualization and export

analyze_watershed_distribution <- function(input_path, output_dir, label_suffix) {
  
  # Print processing status message
  message(paste("Processing dataset:", label_suffix))
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # ========== Load Input Data ==========
  # Verify input file exists before attempting to read
  if (!file.exists(input_path)) {
    warning(paste("File not found:", input_path))
    return(NULL)
  }
  
  # Read CSV and convert to data frame
  data <- as.data.frame(read.csv(input_path))
  
  # Clean data by removing index and metadata columns that aren't needed for analysis
  cols_to_remove <- c("X", "band_count")
  data <- data[, !names(data) %in% cols_to_remove]
  
  # ========== Setup PDF Output ==========
  # Create filename with watershed label (local/regional, fill/no fill)
  pdf_filename <- file.path(output_dir, paste0(label_suffix, "_plots.pdf"))
  
  # Open PDF device for saving all plots (8x6 inches)
  pdf(pdf_filename, width = 8, height = 6)
  
  ################################################################################
  # SECTION 2: Normality Testing & Diagnostic Plots
  ################################################################################
  # Test if variables follow normal distribution using:
  # - Shapiro-Wilk test (parametric, powerful for small samples)
  # - Kolmogorov-Smirnov test (non-parametric)
  # - Skewness (measure of asymmetry)
  # Generate Q-Q plots, density plots, and boxplots for visual inspection
  
  # Define helper function to avoid code repetition for 8 variables
  plot_variable_diagnostics <- function(df, col_name, plot_title) {
    # Extract column as vector and remove missing values
    vec <- df[[col_name]]
    vec <- vec[!is.na(vec)]  # Remove NAs for statistical tests
    
    # Skip if insufficient data for testing (minimum 3 observations)
    if (length(vec) < 3) return()
    
    # ========== Print Normality Test Results to Console ==========
    message(paste("---", plot_title, "---"))
    
    # Shapiro-Wilk test: H0 = data are normally distributed (p >= 0.05 = normal)
    print(shapiro.test(vec))
    
    # Kolmogorov-Smirnov test: Compare to normal distribution with data mean/sd
    print(ks.test(vec, "pnorm", mean(vec), sd(vec)))
    
    # Print skewness: 0 = symmetric, >0 = right-skewed, <0 = left-skewed
    print(paste("Skewness:", skewness(vec)))
    
    # ========== Generate Diagnostic Plots ==========
    # Q-Q plot: Points on diagonal = normal distribution
    print(ggqqplot(df[[col_name]], title = plot_title))
    
    # Density plot with normal distribution overlay (red dashed line)
    # Compare histogram to theoretical normal curve
    print(ggdensity(df, x = col_name, fill = "lightgray", title = plot_title) +
            stat_overlay_normal_density(color = "red", linetype = "dashed"))
    
    # Boxplot: Shows median, quartiles, and outliers
    boxplot(df[[col_name]], xlab = plot_title, main = paste("Boxplot -", plot_title))
  }
  
  # ========== Run Diagnostics for All Variables ==========
  plot_variable_diagnostics(data, "area_km2", "Surface Area (km2)")
  plot_variable_diagnostics(data, "TSS_mean", "TSS")
  plot_variable_diagnostics(data, "precipitation", "Precipitation")
  plot_variable_diagnostics(data, "mean_discharge", "Discharge")
  plot_variable_diagnostics(data, "u_wind", "Wind Eastward")
  plot_variable_diagnostics(data, "v_wind", "Wind Northward")
  plot_variable_diagnostics(data, "anthropogenic_km2", "Anthropic Area")
  plot_variable_diagnostics(data, "natural_km2", "Natural Area")
  
  ################################################################################
  # SECTION 3: Export Normality Test Results Table
  ################################################################################
  # Create a comprehensive table of Shapiro-Wilk test statistics for all variables
  # Includes W-statistic, p-value, normality classification, and sample size
  
  # Define variables to test (includes TSS stats: min, mean, max, std.dev)
  vars <- c("area_km2", "TSS_mean", "TSS_max", "TSS_min", "precipitation",
            "mean_discharge", "u_wind", "v_wind", "anthropogenic_km2", "natural_km2")
  
  # Run Shapiro-Wilk test for each variable and compile results
  shapiro_results <- lapply(vars, function(varname) {
    # Check if variable exists in dataset
    if (!varname %in% names(data)) return(NULL)
    
    # Extract column and remove NAs
    x <- data[[varname]]
    x <- x[!is.na(x)]
    n <- length(x)
    
    # Return NA results if insufficient data (Shapiro requires >= 3)
    if (n < 3) {
      return(data.frame(variable = varname, W = NA, p.value = NA, normality = NA, n = n))
    }
    
    # Shapiro-Wilk test limitation: maximum 5000 observations
    # If sample > 5000, randomly sample 5000 for testing
    sampled_n <- n
    if (n > 5000) {
      set.seed(123)  # Set seed for reproducibility
      x <- sample(x, 5000)
      sampled_n <- 5000
    }
    
    # Perform Shapiro-Wilk test
    test <- shapiro.test(x)
    p <- test$p.value
    
    # Classify normality: p >= 0.05 = normal (fail to reject H0)
    normality <- ifelse(p >= 0.05, "normal", "not normal")
    
    # Return data frame with test results for this variable
    data.frame(variable = varname, W = test$statistic, p.value = p, 
               normality = normality, n = sampled_n, row.names = NULL)
  })
  
  # Combine results from all variables into single data frame
  shapiro_df <- do.call(rbind, shapiro_results)
  
  # Export Shapiro-Wilk results to CSV
  write.csv(shapiro_df, file = file.path(output_dir, paste0("shapiro_", label_suffix, ".csv")), row.names = FALSE)
  
  ################################################################################
  # SECTION 4: Correlation Analysis (Kendall Method)
  ################################################################################
  # Kendall's tau is a non-parametric correlation measure suitable for:
  # - Non-normally distributed data
  # - Monotonic relationships
  # - Small sample sizes
  # Values range from -1 (perfect inverse) to +1 (perfect positive correlation)
  
  # ========== 4.1: TSS vs Other Variables (Scatter Plots with Trend Lines) ==========
  # Define helper function to create scatter plots with Kendall correlation overlay
  plot_corr_scatter <- function(y_col, y_lab) {
    # Create scatter plot with:
    # - Linear regression line (with 95% CI)
    # - Kendall correlation coefficient
    print(ggscatter(data, x = "TSS_mean", y = y_col, add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "kendall", 
                    title = paste("Kendall Corr:", y_lab), xlab = "TSS", ylab = y_lab))
  }
  
  # Generate scatter plots for TSS vs each variable
  plot_corr_scatter("area_km2", "Surface Area")
  plot_corr_scatter("precipitation", "Precipitation")
  plot_corr_scatter("mean_discharge", "Discharge")
  plot_corr_scatter("u_wind", "Eastward Wind")
  plot_corr_scatter("v_wind", "Northward Wind")
  plot_corr_scatter("anthropogenic_km2", "Anthropic Area")
  plot_corr_scatter("natural_km2", "Natural Area")
  
  # ========== 4.2: Full Correlation Matrix (Kendall) ==========
  # Select variables for correlation matrix analysis
  data_select <- select(data, any_of(c("TSS_mean", "area_km2", "precipitation",
                                       "mean_discharge", "u_wind", "v_wind", 
                                       "anthropogenic_km2")))
  
  # Remove rows with any missing values (required for correlation matrix)
  data_select <- na.omit(data_select)
  
  # Proceed only if sufficient complete cases (n > 4)
  if (nrow(data_select) > 4) {
    # Calculate correlation matrix using psych::corr.test with Kendall method
    # normal = FALSE indicates data are not expected to be normally distributed
    cor_result <- psych::corr.test(data_select, method = "kendall", normal = FALSE)
    
    # ========== Plot Correlation Matrix ==========
    # Mixed plot: lower triangle = correlation coefficients, upper triangle = visualization
    corrplot::corrplot.mixed(cor_result$r, lower = "number", las = 1, 
                             title = paste("Correlation -", label_suffix), mar=c(0,0,2,0))
    
    # ========== Export Correlation Results to CSVs ==========
    # Save correlation coefficients (tau values)
    write.csv(cor_result$r,  file = file.path(output_dir, paste0("corr_", label_suffix, ".csv")))
    
    # Save p-values (significance of each correlation)
    write.csv(cor_result$p,  file = file.path(output_dir, paste0("corr_p_", label_suffix, ".csv")))
    
    # Save standard errors
    write.csv(cor_result$se, file = file.path(output_dir, paste0("corr_se_", label_suffix, ".csv")))
    
    # Save confidence intervals
    write.csv(cor_result$ci, file = file.path(output_dir, paste0("corr_ci_", label_suffix, ".csv")))
    
    # ========== Generate Significance Stars ==========
    # Create significance indicators: * = p<0.05, ** = p<0.01, *** = p<0.001
    # (psych::corr.test doesn't have $stars, so generate manually)
    p_mat <- cor_result$p
    stars_mat <- matrix("", nrow=nrow(p_mat), ncol=ncol(p_mat))
    stars_mat[p_mat < 0.05] <- "*"
    stars_mat[p_mat < 0.01] <- "**"
    stars_mat[p_mat < 0.001] <- "***"
    rownames(stars_mat) <- rownames(p_mat)
    colnames(stars_mat) <- colnames(p_mat)
    write.csv(stars_mat, file = file.path(output_dir, paste0("corr_si_", label_suffix, ".csv")))
    
    # Close PDF device (saves all plots to file)
    dev.off()
    
  } else {
    # Insufficient data warning
    warning("Not enough data for correlation matrix.")
  }
  
  # Print completion message
  message(paste("Completed:", label_suffix))
  message("------------------------------------------------")
}

################################################################################
# SECTION 5: Define Datasets and Execute Analysis
################################################################################
# Process four datasets:
# 1. Local watershed (complete cases only)
# 2. Regional watershed (complete cases only)
# 3. Local watershed (with interpolated missing values)
# 4. Regional watershed (with interpolated missing values)
#
# NOTE: Update 'base_path' if your repository folder structure is different

# Define base directory containing merged dataframes
base_path <- "C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df"

# Define all datasets to analyze as list of configuration objects
datasets <- list(
  # 1. Local watershed - complete cases only
  list(
    path   = file.path(base_path, "df_merged_local.csv"),
    outdir = file.path(base_path, "Local Watershed"),
    suffix = "local"
  ),
  
  # 2. Regional watershed - complete cases only
  list(
    path   = file.path(base_path, "df_merged_regional.csv"),
    outdir = file.path(base_path, "Regional Watershed"),
    suffix = "regional"
  ),
  
  # 3. Local watershed - with interpolated fill values
  list(
    path   = file.path(base_path, "df_fillTSS_local.csv"),
    outdir = file.path(base_path, "Local Watershed Fill"),
    suffix = "local"
  ),
  
  # 4. Regional watershed - with interpolated fill values
  list(
    path   = file.path(base_path, "df_fillTSS_regional.csv"),
    outdir = file.path(base_path, "Regional Watershed Fill"),
    suffix = "regional"
  )
)

################################################################################
# Execute Analysis Loop
################################################################################
# Process each dataset configuration through the analysis function
for (ds in datasets) {
  analyze_watershed_distribution(ds$path, ds$outdir, ds$suffix)
}