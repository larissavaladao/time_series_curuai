################################################################################
# Water Period-Specific Distribution & Correlation Analysis
#
# Purpose: Perform statistical analysis (normality tests and correlations) 
# separately for each hydrological water period (R, HW, F, LW). This allows
# examination of how relationships between environmental variables differ across
# seasonal conditions.
#
# Workflow:
# 1. Load regional/local watershed data with water_period classifications
# 2. For each dataset, extract unique water periods (R, HW, F, LW)
# 3. For each water period, perform:
#    - Shapiro-Wilk normality tests
#    - Q-Q plots, density plots, boxplots
#    - Kendall correlation analysis
#    - Correlation matrix visualization
# 4. Export separate results for each dataset x water period combination
#
# Outputs by water period:
# - PDF plots with diagnostic visualizations
# - Shapiro-Wilk normality test results (CSV)
# - Kendall correlation matrices, p-values, significance (CSV)
################################################################################

# Load required libraries
library(dplyr)        # Data manipulation and filtering
library(ggpubr)       # Publication-ready ggplot2 plots
library(moments)      # Skewness and kurtosis calculations
library(corrplot)     # Correlation matrix visualization
library(psych)        # Correlation testing with Kendall method
library(ggplot2)      # Data visualization

################################################################################
# SECTION 1: Define Master Analysis Function
################################################################################
# This function performs the complete workflow for a single dataset:
# - Loads data and extracts water periods  
# - For each water period: generates plots, tests normality, analyzes correlations
# - Exports all results to organized CSV and PDF files
# 
# Parameters:
#   input_path:  Path to merged watershed CSV (df_merged_local.csv, etc.)
#   output_dir:  Directory where results will be saved
#   file_prefix: Label for output files ('local' or 'regional')

analyze_periods_by_dataset <- function(input_path, output_dir, file_prefix) {
  
  # Print processing header to console
  message(paste("\n========================================"))
  message(paste("Starting analysis for:", file_prefix))
  message(paste("Reading file:", input_path))
  
  # ========== Input Validation ==========
  # Verify input file exists before attempting to read
  if (!file.exists(input_path)) {
    warning(paste("File not found:", input_path))
    return(NULL)
  }
  
  # ========== Setup Output Directory ==========
  # Create output directory if it doesn't exist (for PDFs and CSVs)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # ========== Load and Clean Data ==========
  # Read CSV and convert to data frame
  data <- as.data.frame(read.csv(input_path))
  
  # Remove index and metadata columns (X = row index, band_count = raster metadata)
  cols_to_remove <- c("X", "band_count")
  data <- data[, !names(data) %in% cols_to_remove]
  
  # ========== Validate Water Period Column ==========
  # Check if water_period column exists (required for period-specific filtering)
  if(!"water_period" %in% names(data)) {
    warning(paste("Skipping", file_prefix, "- 'water_period' column not found."))
    return(NULL)
  }
  
  # ========== Extract Unique Water Periods ==========
  # Get all unique water periods in the dataset (e.g., R, HW, F, LW)
  unique_periods <- unique(data$water_period)
  message(paste("Found periods:", paste(unique_periods, collapse = ", ")))
  
  ################################################################################
  # SECTION 2: Inner Loop - Analyze Each Water Period Separately
  ################################################################################
  # For each unique water period in the dataset, perform comprehensive analysis
  # This allows examination of seasonal variations in environmental relationships
  
  for (period in unique_periods) {
    
    # ========== Prepare Period-Specific Filename Components ==========
    # Replace special characters in period name for valid filenames
    # E.g., "High Water" -> "High_Water" or "R" -> "R"
    clean_period_name <- gsub("[^A-Za-z0-9]", "_", period)
    message(paste("  Processing period:", period, "->", clean_period_name))
    
    # ========== Filter Data for Specific Water Period ==========
    # Extract all observations where water_period matches current period
    period_data <- filter(data, water_period == period)
    
    # ========== Check Data Sufficiency ==========
    # Skip analysis if fewer than 3 observations (minimum for statistical tests)
    if (nrow(period_data) < 3) {
      message(paste("    Skipping - not enough observations (<3)."))
      next
    }
    
    # ========== Setup Output Filenames ==========
    # All files follow pattern: [prefix]_[type]_[period_name].[ext]
    # Examples:
    #   - plots_local_R.pdf
    #   - shapiro_local_High_Water.csv
    #   - corr_local_F.csv
    
    pdf_filename <- file.path(output_dir, paste0("plots_",file_prefix, "_", clean_period_name, ".pdf"))
    shapiro_csv_name <- file.path(output_dir, paste0("shapiro_",file_prefix, "_", clean_period_name, ".csv"))
    
    # Correlation filenames
    # Correlation output filenames with different result components
    corr_base <- file.path(output_dir, paste0("corr_",file_prefix))
    corr_r_name  <- paste0(corr_base, clean_period_name, ".csv")          # Correlation coefficients
    corr_p_name  <- paste0(corr_base, "_p_", clean_period_name, ".csv") # P-values
    corr_se_name <- paste0(corr_base, "_se_", clean_period_name, ".csv") # Standard errors
    corr_ci_name <- paste0(corr_base, "_ci_", clean_period_name, ".csv") # Confidence intervals
    corr_si_name <- paste0(corr_base, "_sig_", clean_period_name, ".csv") # Significance stars
    
    # ========== Open PDF for Period-Specific Plots ==========
    # All subsequent plot commands will write to this PDF device
    pdf(pdf_filename, width = 8, height = 6)
    
    # ========== SECTION 2A: Normality Testing & Diagnostic Plots ==========
    # Create helper function to generate diagnostic plots for each variable
    # Tests for normality using:
    #   - Q-Q plot: Points on 45-degree line indicate normal distribution
    #   - Density plot: Compares actual distribution to theoretical normal curve
    #   - Boxplot: Shows median, quartiles, and outliers
    
    plot_normality <- function(df, col_name, title_text) {
      # Extract column and remove missing values
      clean_vec <- df[[col_name]][!is.na(df[[col_name]])]
      
      # Only plot if sufficient data (> 3 observations)
      if(length(clean_vec) > 3) {
        # Q-Q plot: Visual assessment of normality (points should follow diagonal line)
        print(ggqqplot(df[[col_name]], title=title_text))
        
        # Density plot: Compare actual distribution (gray) to normal distribution (red dashed line)
        print(ggdensity(df, x = col_name, fill = "lightgray", title=title_text) +
                stat_overlay_normal_density(color = "red", linetype = "dashed"))
        
        # Boxplot: Shows median (line), quartiles (box), and potential outliers
        boxplot(df[[col_name]], xlab=title_text, main=paste("Boxplot -", period))
      }
    }
    
    # Generate diagnostic plots for all 8 variables in this water period
    # Each plot will be added to the PDF
    plot_normality(period_data, "area_km2", "Surface Area (km2)")
    plot_normality(period_data, "TSS_mean", "TSS")
    plot_normality(period_data, "precipitation", "Precipitation")
    plot_normality(period_data, "mean_discharge", "Discharge")
    plot_normality(period_data, "u_wind", "Wind Eastward")
    plot_normality(period_data, "v_wind", "Wind Northward")
    plot_normality(period_data, "anthropogenic_km2", "Anthropic Area")
    plot_normality(period_data, "natural_km2", "Natural Area")
    
    # ========== SECTION 2B: Shapiro-Wilk Normality Test Export ==========
    # Test null hypothesis: data are normally distributed
    # p >= 0.05 indicates normal distribution; p < 0.05 indicates deviation from normality
    # W-statistic: ranges 0-1, where 1 = perfectly normal
    
    # Define all variables to test (includes TSS statistics)
    vars <- c("area_km2", "TSS_mean", "TSS_max", "TSS_min", "precipitation",
              "mean_discharge", "u_wind", "v_wind", "anthropogenic_km2", "natural_km2")
    
    # Run Shapiro-Wilk test for each variable and compile results
    shapiro_results <- lapply(vars, function(varname) {
      # Skip if variable not in dataset
      if (!varname %in% names(period_data)) return(NULL)
      
      # Extract column and remove NAs
      x <- period_data[[varname]]
      x <- x[!is.na(x)]
      n <- length(x)
      
      # Skip if insufficient data (< 3 observations)
      if (n < 3) return(NULL)
      
      # Shapiro-Wilk limitation: maximum 5000 observations
      # If sample > 5000, use random sample for testing (record actual n)
      sampled_n <- n
      if (n > 5000) { 
        x <- sample(x, 5000)  # Random sample for test
        sampled_n <- 5000 
      }
      
      # Perform Shapiro-Wilk test
      test <- shapiro.test(x)
      p <- test$p.value
      
      # Classify normality: p >= 0.05 = normal, p < 0.05 = not normal
      normality <- ifelse(p >= 0.05, "normal", "not normal")
      
      # Return results as data frame row
      data.frame(variable = varname, W = test$statistic, p.value = p, 
                 normality = normality, n = sampled_n, period = period)
    })
    
    # Combine all test results into single data frame
    shapiro_df <- do.call(rbind, shapiro_results)
    
    # Export Shapiro-Wilk results to CSV
    if (!is.null(shapiro_df)) {
      write.csv(shapiro_df, file = shapiro_csv_name, row.names = FALSE)
    }
    
    # ========== SECTION 2C: Correlation Analysis & Visualization ==========
    # Kendall's tau is non-parametric, suitable for non-normal data and monotonic relationships
    # Values range from -1 (perfect negative) to +1 (perfect positive) correlation
    
    # \"\" 2C-1: TSS vs Other Variables (Individual Scatter Plots with Kendall Correlation)
    # Create scatter plot with linear regression line and Kendall correlation coefficient
    plot_corr <- function(y_var, y_lab) {
      # Check if variable exists and has sufficient data (> 3 non-missing)
      if(y_var %in% names(period_data) && sum(!is.na(period_data[[y_var]])) > 3) {
        print(ggscatter(period_data, x = "TSS_mean", y = y_var, add = "reg.line", conf.int = TRUE,
                        cor.coef = TRUE, cor.method = "kendall", 
                        title = paste("Kendall Corr (", period, ")"), 
                        xlab = "TSS", ylab = y_lab))
      }
    }
    
    # Generate scatter plots for TSS vs each environmental variable
    plot_corr("area_km2", "Surface Area")
    plot_corr("precipitation", "Precipitation")
    plot_corr("mean_discharge", "Discharge")
    plot_corr("u_wind", "Eastward Wind")
    plot_corr("v_wind", "Northward Wind")
    plot_corr("anthropogenic_km2", "Anthropic Area")
    plot_corr("natural_km2", "Natural Area")
    
    # \"\" 2C-2: Complete Correlation Matrix (7 Variables x 7 Variables)
    # Select key variables for correlation matrix analysis
    data_select <- select(period_data, any_of(c("TSS_mean", "area_km2", "precipitation",
                                                "mean_discharge", "u_wind", "v_wind", 
                                                "anthropogenic_km2")))
    
    # Remove rows with any missing values (correlation matrix requires complete cases)
    data_select <- na.omit(data_select)
    
    # Proceed only if sufficient complete pairs for correlation (n > 4)
    if(nrow(data_select) > 4) {
      # Calculate correlation matrix using Kendall's tau method
      # normal = FALSE: indicates data are not expected to be normally distributed
      cor_result <- psych::corr.test(data_select, method = "kendall", normal = FALSE)
      
      # ========== Plot Correlation Matrix ==========
      # Mixed plot: lower triangle = correlation coefficients, upper triangle = color intensity visualization
      corrplot::corrplot.mixed(cor_result$r, lower = "number", las = 1, 
                               title = paste("Correlation Matrix -", period), 
                               mar=c(0,0,2,0))
      
      # ========== Export Correlation Results to CSVs ==========
      # 1. Correlation coefficients (Kendall tau values)
      write.csv(cor_result$r, file = corr_r_name)
      
      # 2. P-values (significance of each correlation)
      write.csv(cor_result$p, file = corr_p_name)
      
      # 3. Standard errors
      write.csv(cor_result$se, file = corr_se_name)
      
      # 4. Confidence intervals
      write.csv(cor_result$ci, file = corr_ci_name)
      
      # ========== Generate Significance Stars ==========
      # Manually create significance indicators (psych::corr.test doesn't have $stars)
      # Stars: * = p<0.05, ** = p<0.01, *** = p<0.001
      p_mat <- cor_result$p
      stars_mat <- matrix("", nrow=nrow(p_mat), ncol=ncol(p_mat))
      stars_mat[p_mat < 0.05] <- "*"
      stars_mat[p_mat < 0.01] <- "**"
      stars_mat[p_mat < 0.001] <- "***"
      rownames(stars_mat) <- rownames(p_mat)
      colnames(stars_mat) <- colnames(p_mat)
      
      # Export significance matrix
      write.csv(stars_mat, file = corr_si_name)
      
      # Close PDF device (saves all plots for this period to file)
      dev.off()
      
    } else {
      # Insufficient complete cases for correlation matrix
      message(paste("    Not enough complete pairs for correlation matrix."))
    }
    
    
  }
}

}

################################################################################
# SECTION 3: Define Datasets and Execute Analysis Loop
################################################################################
# Outer loop: Process 4 datasets (2 watersheds x 2 versions: complete cases + filled)
# Inner loop (in function): For each dataset, analyze each water period separately
#
# Each configuration specifies:
#   path:   Input CSV file with merged watershed data
#   outdir: Output directory for results (PDFs and CSVs)
#   prefix: Label for output filenames ('local' or 'regional')
#
# NOTE: Update base_path if your repository folder structure is different

# Define base directory containing all merged dataframes
base_path <- "C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df"

# Define dataset configurations as list
# Each configuration will be processed through the analysis function
datasets <- list(
  # 1. Local watershed - complete cases only (missing values excluded)
  list(
    path   = file.path(base_path, "df_merged_local.csv"),
    outdir = file.path(base_path, "Local Watershed"),
    prefix = "local"
  ),
  
  # 2. Regional watershed - complete cases only (missing values excluded)
  list(
    path   = file.path(base_path, "df_merged_regional.csv"),
    outdir = file.path(base_path, "Regional Watershed"),
    prefix = "regional"
  ),
  
  # 3. Local watershed - with interpolated fill values for missing TSS/area data
  list(
    path   = file.path(base_path, "df_fillTSS_local.csv"),
    outdir = file.path(base_path, "Local Watershed Fill"),
    prefix = "local"
  ),
  
  # 4. Regional watershed - with interpolated fill values for missing TSS/area data
  list(
    path   = file.path(base_path, "df_fillTSS_regional.csv"),
    outdir = file.path(base_path, "Regional Watershed Fill"),
    prefix = "regional"
  )
)

################################################################################
# Execute Outer Loop: Process All Datasets
################################################################################
# For each dataset:
# 1. Call analyze_periods_by_dataset() function
# 2. Function extracts unique water periods and analyzes each separately
# 3. All results (plots, tests, correlations) saved to output directory

for (ds in datasets) {
  analyze_periods_by_dataset(ds$path, ds$outdir, ds$prefix)
}

# Print completion message
message("\n========================================")
message("All datasets and water periods processed.")
message("========================================")