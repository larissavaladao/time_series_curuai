# Load libraries once
library(dplyr)
library(ggpubr)
library(moments)
library(corrplot)
library(psych)
library(ggplot2)

# 1. Define the Analysis Function ####
analyze_watershed_distribution <- function(input_path, output_dir, label_suffix) {
  
  message(paste("Processing dataset:", label_suffix))
  
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
  
  # Clean data (remove X and band_count if they exist)
  cols_to_remove <- c("X", "band_count")
  data <- data[, !names(data) %in% cols_to_remove]
  
  # Define filenames based on the suffix (local/regional)
  pdf_filename <- file.path(output_dir, paste0(label_suffix, "_plots.pdf"))
  
  # Open PDF Device
  pdf(pdf_filename, width = 8, height = 6)
  
  ##############################################################################
  # Normality Tests & Plots (Helper Function)
  ##############################################################################
  
  # Helper to avoid repeating plot code 8 times
  plot_variable_diagnostics <- function(df, col_name, plot_title) {
    vec <- df[[col_name]]
    vec <- vec[!is.na(vec)] # remove NAs for tests
    
    if (length(vec) < 3) return()
    
    # Print text results to console
    message(paste("---", plot_title, "---"))
    print(shapiro.test(vec))
    # KS test with mean/sd from data
    print(ks.test(vec, "pnorm", mean(vec), sd(vec)))
    print(paste("Skewness:", skewness(vec)))
    
    # Plots
    print(ggqqplot(df[[col_name]], title = plot_title))
    print(ggdensity(df, x = col_name, fill = "lightgray", title = plot_title) +
            stat_overlay_normal_density(color = "red", linetype = "dashed"))
    boxplot(df[[col_name]], xlab = plot_title, main = paste("Boxplot -", plot_title))
  }
  
  # Run diagnostics for all variables
  plot_variable_diagnostics(data, "area_km2", "Surface Area (km2)")
  plot_variable_diagnostics(data, "TSS_mean", "TSS")
  plot_variable_diagnostics(data, "precipitation", "Precipitation")
  plot_variable_diagnostics(data, "mean_discharge", "Discharge")
  plot_variable_diagnostics(data, "u_wind", "Wind Eastward")
  plot_variable_diagnostics(data, "v_wind", "Wind Northward")
  plot_variable_diagnostics(data, "anthropogenic_km2", "Anthropic Area")
  plot_variable_diagnostics(data, "natural_km2", "Natural Area")
  
  ##############################################################################
  # Export Shapiro-Wilk Table
  ##############################################################################
  
  vars <- c("area_km2", "TSS_mean", "TSS_max", "TSS_min", "precipitation",
            "mean_discharge", "u_wind", "v_wind", "anthropogenic_km2", "natural_km2")
  
  shapiro_results <- lapply(vars, function(varname) {
    if (!varname %in% names(data)) return(NULL)
    
    x <- data[[varname]]
    x <- x[!is.na(x)]
    n <- length(x)
    
    if (n < 3) {
      return(data.frame(variable = varname, W = NA, p.value = NA, normality = NA, n = n))
    }
    
    # Sample if > 5000 (Shapiro limit)
    sampled_n <- n
    if (n > 5000) {
      set.seed(123)
      x <- sample(x, 5000)
      sampled_n <- 5000
    }
    
    test <- shapiro.test(x)
    p <- test$p.value
    normality <- ifelse(p >= 0.05, "normal", "not normal")
    
    data.frame(variable = varname, W = test$statistic, p.value = p, 
               normality = normality, n = sampled_n, row.names = NULL)
  })
  
  shapiro_df <- do.call(rbind, shapiro_results)
  write.csv(shapiro_df, file = file.path(output_dir, paste0("shapiro_", label_suffix, ".csv")), row.names = FALSE)
  
  ##############################################################################
  # Correlation Analysis
  ##############################################################################
  
  # 1. Scatter Plots (Kendall)
  # Helper for scatter plots
  plot_corr_scatter <- function(y_col, y_lab) {
    print(ggscatter(data, x = "TSS_mean", y = y_col, add = "reg.line", conf.int = TRUE,
                    cor.coef = TRUE, cor.method = "kendall", 
                    title = paste("Kendall Corr:", y_lab), xlab = "TSS", ylab = y_lab))
  }
  
  plot_corr_scatter("area_km2", "Surface Area")
  plot_corr_scatter("precipitation", "Precipitation")
  plot_corr_scatter("mean_discharge", "Discharge")
  plot_corr_scatter("u_wind", "Eastward Wind")
  plot_corr_scatter("v_wind", "Northward Wind")
  plot_corr_scatter("anthropogenic_km2", "Anthropic Area")
  plot_corr_scatter("natural_km2", "Natural Area")
  
  # 2. Correlation Matrix
  data_select <- select(data, any_of(c("TSS_mean", "area_km2", "precipitation",
                                       "mean_discharge", "u_wind", "v_wind", 
                                       "anthropogenic_km2")))
  
  # Remove NAs for correlation matrix
  data_select <- na.omit(data_select)
  
  if (nrow(data_select) > 4) {
    # Using psych::corr.test
    cor_result <- psych::corr.test(data_select, method = "kendall", normal = FALSE)
    
    # Plot Mixed
    corrplot::corrplot.mixed(cor_result$r, lower = "number", las = 1, 
                             title = paste("Correlation -", label_suffix), mar=c(0,0,2,0))
    
    # Save CSVs
    write.csv(cor_result$r,  file = file.path(output_dir, paste0("corr_", label_suffix, ".csv")))
    write.csv(cor_result$p,  file = file.path(output_dir, paste0("corr_p_", label_suffix, ".csv")))
    write.csv(cor_result$se, file = file.path(output_dir, paste0("corr_se_", label_suffix, ".csv")))
    write.csv(cor_result$ci, file = file.path(output_dir, paste0("corr_ci_", label_suffix, ".csv")))
    
    # Generate Stars (Significance) manually since corr.test object doesn't have $stars
    p_mat <- cor_result$p
    stars_mat <- matrix("", nrow=nrow(p_mat), ncol=ncol(p_mat))
    stars_mat[p_mat < 0.05] <- "*"
    stars_mat[p_mat < 0.01] <- "**"
    stars_mat[p_mat < 0.001] <- "***"
    rownames(stars_mat) <- rownames(p_mat)
    colnames(stars_mat) <- colnames(p_mat)
    write.csv(stars_mat, file = file.path(output_dir, paste0("corr_si_", label_suffix, ".csv")))
    
    dev.off()# Close PDF
    
  } else {
    # dev.off()
    warning("Not enough data for correlation matrix.")
  }
  
  message(paste("Completed:", label_suffix))
  message("------------------------------------------------")
}

# 2. Define Datasets and Run Loop ####
# NOTE: Update 'base_path' to your specific folder structure
base_path <- "C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df"

datasets <- list(
  list(
    path   = file.path(base_path, "df_merged_local.csv"),
    outdir = file.path(base_path, "Local Watershed"),
    suffix = "local"
  ),
  list(
    path   = file.path(base_path, "df_merged_regional.csv"),
    outdir = file.path(base_path, "Regional Watershed"),
    suffix = "regional"
  ),
  list(
    path   = file.path(base_path, "df_fillTSS_local.csv"),
    outdir = file.path(base_path, "Local Watershed Fill"),
    suffix = "local"
  ),
  list(
    path   = file.path(base_path, "df_fillTSS_regional.csv"),
    outdir = file.path(base_path, "Regional Watershed Fill"),
    suffix = "regional"
  )
)

# Run the analysis for each dataset
for (ds in datasets) {
  analyze_watershed_distribution(ds$path, ds$outdir, ds$suffix)
}

