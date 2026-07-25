# Importar bibliotecas ####
library(dplyr)
library(FactoMineR)
library(factoextra)
library(ggplot2)

pca_analysis <- function(input_path, output_dir, label_suffix){
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
  data$water_period <- as.factor(data$water_period)
  
  # Clean data by removing index and metadata columns that aren't needed for analysis
  data_select <- select(data, TSS_mean, area_km2, mean_discharge,
                        precipitation, u_wind, v_wind, 
                        anthropogenic_km2)
  message(paste("Data Summary:", summary(data_select)))
  
  # Calculate PCA
  res.pca <- PCA(data_select,
                 graph = FALSE,
                 scale.unit = TRUE)
  
  eing <- res.pca$eig
  message(paste("Eigenvalues:"))
  print(eing)
  write.csv(eing, file = file.path(output_dir, paste0("pca_eing", label_suffix, ".csv")))
  
  var_coordinates <- res.pca$var$coord
  message(paste("Variables coordinates:", var_coordinates))
  write.csv(var_coordinates, file = file.path(output_dir, paste0("pca_coord", label_suffix, ".csv")))
  
  var_correlation <- res.pca$var$cor
  message(paste("Correlation between variables and axes:", var_correlation))
  write.csv(var_correlation, file = file.path(output_dir, paste0("pca_corr", label_suffix, ".csv")))
  
  var_cos2 <- res.pca$var$cos2
  message(paste("Square cosine:", var_cos2))
  write.csv(var_cos2, file = file.path(output_dir, paste0("pca_cos2", label_suffix, ".csv")))
  
  var_contrib <- res.pca$var$contrib
  message(paste("Contributions:", var_contrib))
  write.csv(var_contrib, file = file.path(output_dir, paste0("pca_contrib", label_suffix, ".csv")))
  
  # Plot Scree Plot
  fviz_eig(res.pca, addlabels = TRUE, title = "", labelsize = 2.5, barfill = "steelblue", barcolor = "steelblue") +
    theme(
      text = element_text(size = 6.5, color = "black"),
      axis.title = element_text(size = 6.5, color = "black"),
      axis.text = element_text(size = 6.5, color = "black")
    )
  
  ggsave(
    paste0("pca_scree_", label_suffix, ".jpg"),
    plot = last_plot(),
    path = output_dir,
    dpi = 300,
    width = 4.9,
    height = 3.2,
    units = "in"
  )
  
  # Plot Biplot
  fviz_pca_biplot(
    res.pca,
    geom.ind = c("point"),
    geom.var = c("arrow", "text"),
    labelsize = 0,
    repel = TRUE,
    habillage = data$water_period,
    palette = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728'),
    col.var = "black",          # Sets variable arrows and variable text labels to black
    addEllipses = TRUE,
    ellipse.level = 0.9,
    alpha.ind = 0.8,
    title = ''
  ) +
    theme(
      text = element_text(size = 6.5, color = "black"),
      axis.title = element_text(size = 6.5, color = "black"),
      axis.text = element_text(size = 6.5, color = "black"),
      legend.text = element_text(size = 6.5, color = "black"),
      legend.title = element_text(size = 6.5, color = "black")
    )
  
  ggsave(
    paste0("pca_", label_suffix, ".jpg"),
    plot = last_plot(),
    path = output_dir,
    dpi = 400,
    width = 3,
    height = 3.2,
    units = "in"
  )
}

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
  )
)

################################################################################
# Execute Analysis Loop
################################################################################
for (ds in datasets) {
  pca_analysis(ds$path, ds$outdir, ds$suffix)
}
