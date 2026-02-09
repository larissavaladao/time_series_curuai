setwd("C:/Users/l_v_v/Documents/GitHub/time_series_curuai+/datasets/Parameters Time series/merged_df_cota_lulc")
library(readr)

data_select <- as.data.frame(read.csv("filled_data_cota_lulc.csv"))
data_select<-select(data_select, area_km2  , mean_SPM  , mean_precipitation,
                    mean_discharge  ,mean_u__wind,mean_v__wind)

head(data_select)
summary(data_select)

#fazer graficos - blbiotecas necess?rias
library(ggplot2)
library(grid)
library(gtable)
library(dplyr)
library(ggpmisc)
library("cowplot")
library(mice)
library(factoextra)
library(corrplot)
library(FactoMineR)


#definir o nome dos reservat?rios como fator
#data_select$water_period <- as.factor(data_select$water_period)

pca <- prcomp(data_select, scale = T)

pca
summary(pca) #autovalores
round(pca$rotation, 3) #autovetores


# Biplot
biplot(pca,
       pc.biplot = T,
       cex = 0.5,
       main = "PCA")
abline(h = 0, lwd = 1, lty = 2, col = "blue")
abline(v = 0, lwd = 1, lty = 2, col = "blue")

#calcular pca
res.pca1 <- PCA(data_select,graph = F)
#Visualize eigenvalues (scree plot). Show the percentage of variances explained 
#by each principal component.
fviz_eig(res.pca1,addlabels = TRUE)
#Graph of variables. Positive correlated variables point to the same side of the 
#plot. Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(res.pca1,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#358705", "#9a4402", "#ff0000"),
             repel = TRUE,     # Avoid text overlapping
             title = "")

#ggsave(
 #   'PCA_otcantins.jpg',
  #  plot = last_plot(),
  #  path = "C:/Users/l_v_v/Desktop/teste",
  #  dpi = 300,
  #  width = 3.5,
  #  height = 3.5,
  #  units =  c("in")
#)
# Eigenvalues
eig.val <- get_eigenvalue(res.pca1)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca1)
res.var$contrib        # Contributions to the PCs
res.var$cor




##############################################################################
#DADOS MENSAIS
setwd("C:/Users/l_v_v/Documents/GitHub/py6s_harmonize_sample/datasets/Parameters Time series/merged_df")
library(readr)

data_select <- as.data.frame(read.csv("filled_data.csv"))
data_select<-select(data_select, area_km2  , mean_SPM  , mean_precipitation,
                    discharge_mean ,mean_u__wind,mean_v__wind,water_period)

head(data_select)
summary(data_select)

#fazer graficos - blbiotecas necess?rias
library(ggplot2)
library(grid)
library(gtable)
library(dplyr)
library(ggpmisc)
library("cowplot")
library(mice)
library(factoextra)
library(corrplot)
library(FactoMineR)


#definir o nome dos reservat?rios como fator
data_select$water_period <- as.factor(data_select$water_period)

pca <- prcomp(data_select, scale = T)

pca
summary(pca) #autovalores
round(pca$rotation, 3) #autovetores


# Biplot
biplot(pca,
       pc.biplot = T,
       cex = 0.5,
       main = "PCA")
abline(h = 0, lwd = 1, lty = 2, col = "blue")
abline(v = 0, lwd = 1, lty = 2, col = "blue")

#calcular pca
res.pca1 <- PCA(data_select,graph = F)
#Visualize eigenvalues (scree plot). Show the percentage of variances explained 
#by each principal component.
fviz_eig(res.pca1,addlabels = TRUE)
#Graph of variables. Positive correlated variables point to the same side of the 
#plot. Negative correlated variables point to opposite sides of the graph.
fviz_pca_var(res.pca1,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#358705", "#9a4402", "#ff0000"),
             repel = TRUE,     # Avoid text overlapping
             title = "")

#ggsave(
#   'PCA_otcantins.jpg',
#  plot = last_plot(),
#  path = "C:/Users/l_v_v/Desktop/teste",
#  dpi = 300,
#  width = 3.5,
#  height = 3.5,
#  units =  c("in")
#)
# Eigenvalues
eig.val <- get_eigenvalue(res.pca1)
eig.val

# Results for Variables
res.var <- get_pca_var(res.pca1)
res.var$contrib        # Contributions to the PCs
res.var$cor
