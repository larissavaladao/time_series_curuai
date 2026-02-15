# Load libraries once
library(dplyr)
library(ggpubr)
library(moments)
library(corrplot)
library(psych)
library(ggplot2)

# 1. Define the Master Function ####
analyze_periods_by_dataset <- function(input_path, output_dir, file_prefix) {
  
  message(paste("\n========================================"))
  message(paste("Starting analysis for:", file_prefix))
  message(paste("Reading file:", input_path))
  
  # Check if file exists
  if (!file.exists(input_path)) {
    warning(paste("File not found:", input_path))
    return(NULL)
  }
  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Read Data
  data <- as.data.frame(read.csv(input_path))
  
  # Clean data
  cols_to_remove <- c("X", "band_count")
  data <- data[, !names(data) %in% cols_to_remove]
  
  # Check if water_period exists
  if(!"water_period" %in% names(data)) {
    warning(paste("Skipping", file_prefix, "- 'water_period' column not found."))
    return(NULL)
  }
  
  # Get unique water periods
  unique_periods <- unique(data$water_period)
  message(paste("Found periods:", paste(unique_periods, collapse = ", ")))
  
  ######################################################################
  # BEGIN LOOP FOR EACH WATER PERIOD (Inner Loop)
  ######################################################################
  
  for (period in unique_periods) {
    
    # Clean period name for filename (remove spaces, etc.)
    clean_period_name <- gsub("[^A-Za-z0-9]", "_", period)
    message(paste("  Processing period:", period, "->", clean_period_name))
    
    # Filter data
    period_data <- filter(data, water_period == period)
    
    # Check data sufficiency
    if (nrow(period_data) < 3) {
      message(paste("    Skipping - not enough observations (<3)."))
      next
    }
    
    # Define Dynamic Filenames
    # Format: [dataset]_[filetype]_[period].ext
    # Example: local_plots_High_Water.pdf
    
    pdf_filename <- file.path(output_dir, paste0("plots_",file_prefix, "_", clean_period_name, ".pdf"))
    shapiro_csv_name <- file.path(output_dir, paste0("shapiro_",file_prefix, "_", clean_period_name, ".csv"))
    
    # Correlation filenames
    corr_base <- file.path(output_dir, paste0("corr_",file_prefix))
    corr_r_name  <- paste0(corr_base, clean_period_name, ".csv")
    corr_p_name  <- paste0(corr_base, "_p_", clean_period_name, ".csv")
    corr_se_name <- paste0(corr_base, "_se_", clean_period_name, ".csv")
    corr_ci_name <- paste0(corr_base, "_ci_", clean_period_name, ".csv")
    corr_si_name <- paste0(corr_base, "_sig_", clean_period_name, ".csv")
    
    # Open PDF
    pdf(pdf_filename, width = 8, height = 6)
    
    # --- Normality Tests & Plots (Helper Function) ---
    plot_normality <- function(df, col_name, title_text) {
      clean_vec <- df[[col_name]][!is.na(df[[col_name]])]
      if(length(clean_vec) > 3) {
        # Prints go to console/log
        # Plots go to PDF
        print(ggqqplot(df[[col_name]], title=title_text))
        print(ggdensity(df, x = col_name, fill = "lightgray", title=title_text) +
                stat_overlay_normal_density(color = "red", linetype = "dashed"))
        boxplot(df[[col_name]], xlab=title_text, main=paste("Boxplot -", period))
      }
    }
    
    plot_normality(period_data, "area_km2", "Surface Area (km2)")
    plot_normality(period_data, "TSS_mean", "TSS")
    plot_normality(period_data, "precipitation", "Precipitation")
    plot_normality(period_data, "mean_discharge", "Discharge")
    plot_normality(period_data, "u_wind", "Wind Eastward")
    plot_normality(period_data, "v_wind", "Wind Northward")
    plot_normality(period_data, "anthropogenic_km2", "Anthropic Area")
    plot_normality(period_data, "natural_km2", "Natural Area")
    
    # --- Shapiro-Wilk Export ---
    vars <- c("area_km2", "TSS_mean", "TSS_max", "TSS_min", "precipitation",
              "mean_discharge", "u_wind", "v_wind", "anthropogenic_km2", "natural_km2")
    
    shapiro_results <- lapply(vars, function(varname) {
      if (!varname %in% names(period_data)) return(NULL)
      x <- period_data[[varname]]
      x <- x[!is.na(x)]
      n <- length(x)
      if (n < 3) return(NULL)
      
      sampled_n <- n
      if (n > 5000) { x <- sample(x, 5000); sampled_n <- 5000 }
      
      test <- shapiro.test(x)
      p <- test$p.value
      normality <- ifelse(p >= 0.05, "normal", "not normal")
      data.frame(variable = varname, W = test$statistic, p.value = p, 
                 normality = normality, n = sampled_n, period = period)
    })
    
    shapiro_df <- do.call(rbind, shapiro_results)
    if (!is.null(shapiro_df)) {
      write.csv(shapiro_df, file = shapiro_csv_name, row.names = FALSE)
    }
    
    # --- Correlation ---
    # Graphical (Kendall)
    plot_corr <- function(y_var, y_lab) {
      if(y_var %in% names(period_data) && sum(!is.na(period_data[[y_var]])) > 3) {
        print(ggscatter(period_data, x = "TSS_mean", y = y_var, add = "reg.line", conf.int = TRUE,
                        cor.coef = TRUE, cor.method = "kendall", 
                        title = paste("Kendall Corr (", period, ")"), 
                        xlab = "TSS", ylab = y_lab))
      }
    }
    
    plot_corr("area_km2", "Surface Area")
    plot_corr("precipitation", "Precipitation")
    plot_corr("mean_discharge", "Discharge")
    plot_corr("u_wind", "Eastward Wind")
    plot_corr("v_wind", "Northward Wind")
    plot_corr("anthropogenic_km2", "Anthropic Area")
    plot_corr("natural_km2", "Natural Area")
    
    # Matrix
    data_select <- select(period_data, any_of(c("TSS_mean", "area_km2", "precipitation",
                                                "mean_discharge", "u_wind", "v_wind", 
                                                "anthropogenic_km2")))
    data_select <- na.omit(data_select)
    
    if(nrow(data_select) > 4) {
      cor_result <- psych::corr.test(data_select, method = "kendall", normal = FALSE)
      
      corrplot::corrplot.mixed(cor_result$r, lower = "number", las = 1, 
                               title = paste("Correlation Matrix -", period), 
                               mar=c(0,0,2,0))
      
      write.csv(cor_result$r, file = corr_r_name)
      write.csv(cor_result$p, file = corr_p_name)
      write.csv(cor_result$se, file = corr_se_name)
      write.csv(cor_result$ci, file = corr_ci_name)
      # Generate Stars (Significance) manually since corr.test object doesn't have $stars
      p_mat <- cor_result$p
      stars_mat <- matrix("", nrow=nrow(p_mat), ncol=ncol(p_mat))
      stars_mat[p_mat < 0.05] <- "*"
      stars_mat[p_mat < 0.01] <- "**"
      stars_mat[p_mat < 0.001] <- "***"
      rownames(stars_mat) <- rownames(p_mat)
      colnames(stars_mat) <- colnames(p_mat)
      write.csv(stars_mat, file = corr_si_name)
      
      dev.off()# Close PDF
    } else {
      message(paste("    Not enough complete pairs for correlation matrix."))
    }
    
    
  }
}

# 2. Define Datasets and Run Loop (Outer Loop) ####
# Update base_path to your folder structure
base_path <- "C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df"

datasets <- list(
  list(
    # Input file for Local
    path   = file.path(base_path, "df_merged_local.csv"),
    # Output folder for Local
    outdir = file.path(base_path, "Local Watershed"),
    # Prefix for filenames (e.g. "local_plots_...")
    prefix = "local"
  ),
  list(
    # Input file for Regional
    path   = file.path(base_path, "df_merged_regional.csv"),
    # Output folder for Regional
    outdir = file.path(base_path, "Regional Watershed"),
    # Prefix for filenames (e.g. "regional_plots_...")
    prefix = "regional"
  ),
  list(
    # Input file for Local
    path   = file.path(base_path, "df_fillTSS_local.csv"),
    # Output folder for Local
    outdir = file.path(base_path, "Local Watershed Fill"),
    # Prefix for filenames (e.g. "local_plots_...")
    prefix = "local"
  ),
  list(
    # Input file for Regional
    path   = file.path(base_path, "df_fillTSS_regional.csv"),
    # Output folder for Regional
    outdir = file.path(base_path, "Regional Watershed Fill"),
    # Prefix for filenames (e.g. "regional_plots_...")
    prefix = "regional"
  )
)

# Run the outer loop
for (ds in datasets) {
  analyze_periods_by_dataset(ds$path, ds$outdir, ds$prefix)
}

message("All datasets and periods processed.")