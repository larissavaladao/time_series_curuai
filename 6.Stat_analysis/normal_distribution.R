# dados dos mosaicos com periodos de água definidos por cotas em Obidos ####
#definir diretório de trabalho 
setwd("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df")

# import data
library(readr)
library(dplyr)
library(tidyr)
library(ggpubr)
library(moments)

# import data
data <- as.data.frame(
  read.csv(
    "C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df/filled_data.csv"
    ))

head(data)
summary(data)
data <- select(data,-X,-band_count)

#testes preliminares #####################################
#tanto Shapiro Wilk quanto Korogonov tem resultados parecidos - para testar normalidade
##testar normalidade#####

#Area

shapiro.test(data$area_km2) 
ks.test(data$area_km2, "pnorm", mean(data$area_km2), sd(data$area_km2))
ggqqplot(data$area_km2)
# Distribution of CONT variable
ggdensity(data, x = "area_km2", fill = "lightgray", title = "Area") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data$area_km2)
skewness(data$area_km2)

#TSS mean

shapiro.test(data$TSS_mean) 
ks.test(data$TSS_mean, "pnorm", mean(data$TSS_mean), sd(data$TSS_mean))
ggqqplot(data$TSS_mean)
# # Distribution of CONT variable
ggdensity(data, x = "TSS_mean", fill = "lightgray", title = "TSS") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data$TSS_mean)
skewness(data$TSS_mean)

#TSS max

shapiro.test(data$TSS_max)
ks.test(data$TSS_max, "pnorm", mean(data$TSS_max), sd(data$TSS_max))
ggqqplot(data$TSS_max)
# Distribution of CONT variable
ggdensity(data, x = "TSS_max", fill = "lightgray", title = "TSS") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data$TSS_max)
skewness(data$TSS_max)

#TSS min

shapiro.test(data$TSS_min) 
ks.test(data$TSS_min, "pnorm", mean(data$TSS_min), sd(data$TSS_min))
ggqqplot(data$TSS_min)
# Distribution of CONT variable
ggdensity(data, x = "TSS_min", fill = "lightgray", title = "TSS") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data$TSS_min)
skewness(data$TSS_min)

#Precipitation

shapiro.test(data$mean_loc_chirps) 
ks.test(data$mean_loc_chirps, "pnorm", mean(data$mean_loc_chirps), sd(data$mean_loc_chirps))
ggqqplot(data$mean_loc_chirps)
# Distribution of CONT variable
ggdensity(data, x = "mean_loc_chirps", fill = "lightgray", title = "Precipitation") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data$mean_loc_chirps)
skewness(data$mean_loc_chirps)

# discharge

shapiro.test(data$mean_discharge)
ks.test(data$mean_discharge, "pnorm", mean(data$mean_discharge), sd(data$mean_discharge))
ggqqplot(data$mean_discharge)
# Distribution of CONT variable
ggdensity(data, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data$mean_discharge)
skewness(data$mean_discharge)

#wind U

shapiro.test(data$u_wind_loc)
ks.test(data$u_wind_loc, "pnorm", mean(data$u_wind_loc), sd(data$u_wind_loc))
ggqqplot(data$u_wind_loc)
# Distribution of CONT variable
ggdensity(data, x = "u_wind_loc", fill = "lightgray", title = "Wind U") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data$u_wind_loc)
skewness(data$u_wind_loc)

# wind V

shapiro.test(data$v_wind_loc)
ks.test(data$v_wind_loc, "pnorm", mean(data$v_wind_loc), sd(data$v_wind_loc))
ggqqplot(data$v_wind_loc)
# Distribution of CONT variable
ggdensity(data, x = "v_wind_loc", fill = "lightgray", title = "Wind V") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data$v_wind_loc)
skewness(data$v_wind_loc)

#Anthropogenic area

shapiro.test(data$anthropogenic_km2)
ks.test(data$anthropogenic_km2, "pnorm", mean(data$anthropogenic_km2), sd(data$anthropogenic_km2))
ggqqplot(data$anthropogenic_km2)
# Distribution of CONT variable
ggdensity(data, x = "anthropogenic_km2", fill = "lightgray", title = "anthropogenic_km2") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data$anthropogenic_km2)
skewness(data$anthropogenic_km2)

#Natural area

shapiro.test(data$natural_km2)
ks.test(data$natural_km2, "pnorm", mean(data$natural_km2), sd(data$natural_km2))
ggqqplot(data$natural_km2)
# Distribution of CONT variable
ggdensity(data, x = "natural_km2", fill = "lightgray", title = "natural_km2") +
  stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data$natural_km2)
skewness(data$natural_km2)

# #calcular correlacaoo##############################################################
# #pacotes
library(corrplot)
library(tidyverse)
library(lmtest)
library(psych)

# #testar se a covari?ncia ? linear - verificar scatter plot
# #mean TSS
library(corrplot)
library(tidyverse)
library(lmtest)
library(psych)

# #max TSS
ggscatter(data, x = 'TSS_max', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max',
          ylab = 'area_km2') 

ggscatter(data, x = 'TSS_max', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max',
          ylab = 'mean_loc_chirps')

ggscatter(data, x = 'TSS_max', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max',
          ylab = 'mean_discharge')

ggscatter(data, x = 'TSS_max', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max',
          ylab = 'u_wind_loc')

ggscatter(data, x = 'TSS_max', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max',
          ylab = 'v_wind_loc')

ggscatter(data, x = 'TSS_max', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max',
          ylab = 'anthropogenic_km2')

# #min TSS
ggscatter(data, x = 'TSS_min', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min',
          ylab = 'area_km2')

ggscatter(data, x = 'TSS_min', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min',
          ylab = 'mean_loc_chirps')

ggscatter(data, x = 'TSS_min', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min',
          ylab = 'mean_discharge')

ggscatter(data, x = 'TSS_min', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min',
          ylab = 'u_wind_loc')

ggscatter(data, x = 'TSS_min', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min',
          ylab = 'v_wind_loc')

ggscatter(data, x = 'TSS_min', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min',
          ylab = 'anthropogenic_km2')

# #mean TSS
ggscatter(data, x = 'TSS_mean', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
          ylab = 'area_km2')

ggscatter(data, x = 'TSS_mean', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
          ylab = 'mean_loc_chirps')

ggscatter(data, x = 'TSS_mean', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
          ylab = 'mean_discharge')

ggscatter(data, x = 'TSS_mean', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
          ylab = 'u_wind_loc')

ggscatter(data, x = 'TSS_mean', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
          ylab = 'v_wind_loc')

ggscatter(data, x = 'TSS_mean', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
          ylab = 'anthropogenic_km2')

ggscatter(data, x = 'TSS_mean', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
          ylab = 'natural_km2')


# #matriz de corelacao############################################################

data_select <- select(data, TSS_mean, area_km2, mean_loc_chirps, 
        mean_discharge, u_wind_loc, v_wind_loc,  anthropogenic_km2)

cor_result<- corTest(data_select, method = 'kendall', normal=FALSE)

corrplot::corrplot.mixed(cor_result$r, lower = 'number', las = 1)


write.csv(cor_result$r, file = "corr_local.csv")
write.csv(cor_result$p, file = "corr_p_local.csv")
write.csv(cor_result$se, file = "corr_se_local.csv")
write.csv(cor_result$ci, file = "corr_ci_local.csv")
# #max TSS
# cor.test(data$TSS_max, data$area_km2, method = 'kendall')
# cor.test(data$TSS_max, data$mean_loc_chirps, method = 'kendall')
# cor.test(data$TSS_max, data$mean_discharge, method = 'kendall')
# cor.test(data$TSS_max, data$u_wind_loc, method = 'kendall')
# cor.test(data$TSS_max, data$v_wind_loc, method = 'kendall')
# cor.test(data$TSS_max, data$anthropogenic_km2, method = 'kendall')

# #min TSS
# cor.test(data$TSS_min, data$area_km2, method = 'kendall')
# cor.test(data$TSS_min, data$mean_loc_chirps, method = 'kendall')
# cor.test(data$TSS_min, data$mean_discharge, method = 'kendall')
# cor.test(data$TSS_min, data$u_wind_loc, method = 'kendall')
# cor.test(data$TSS_min, data$v_wind_loc, method = 'kendall')
# cor.test(data$TSS_min, data$anthropogenic_km2, method = 'kendall')





# #DADOS Separados por periodos##################################################################################
# data <- as.data.frame(read.csv("filled_data_cota_lulc.csv"))
# data <- select(data,-X,-class_name)
# data$water_period <- as.factor(data$water_period)

# # checking which values are from the period
# data_LW <- filter(data,water_period=='LW') 
# # data_LW
# summary(data_LW)

# data_HW<- filter(data,water_period=='HW') 
# # data_HW
# summary(data_HW)

# data_R<- filter(data,water_period=='R') 
# # data_R
# summary(data_R)

# data_F<- filter(data,water_period=='F') 
# # data_F
# summary(data_F)

# data_transition<- filter(data, water_period=='F' | water_period=='R') 
# # data_transition
# summary(data_transition)

# data_LW_R <- filter(data,water_period=='LW' | water_period=='R') 
# # data_LW_R
# summary(data_LW_R)

# data_HW_F<- filter(data,water_period=='HW'| water_period=='F') 
# # data_HW_F
# summary(data_HW_F)


# #testes preliminares #####################################

# # tanto Shapiro Wilk quanto Korogonov tem resultados parecidos - para testar normalidade
# #testar normalidade 
# ##Area####
# #LW
# shapiro.test(data_LW$area_km2) 
# ks.test(data_LW$area_km2, "pnorm", mean(data_LW$area_km2), sd(data_LW$area_km2))
# ggqqplot(data_LW$area_km2)
# # Distribution of CONT variable
# ggdensity(data_LW, x = "area_km2", fill = "lightgray", title = "Area LW") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW$area_km2)
# skewness(data_LW$area_km2)

# #HW
# shapiro.test(data_HW$area_km2) 
# ks.test(data_HW$area_km2, "pnorm", mean(data_HW$area_km2), sd(data_HW$area_km2))
# ggqqplot(data_HW$area_km2)
# # Distribution of CONT variable
# ggdensity(data_HW, x = "area_km2", fill = "lightgray", title = "Area HW") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW$area_km2)
# skewness(data_HW$area_km2)

# #R
# shapiro.test(data_R$area_km2) 
# ks.test(data_R$area_km2, "pnorm", mean(data_R$area_km2), sd(data_R$area_km2))
# ggqqplot(data_R$area_km2)
# # Distribution of CONT variable
# ggdensity(data_R, x = "area_km2", fill = "lightgray", title = "Area R") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_R$area_km2)
# skewness(data_R$area_km2)

# #F
# shapiro.test(data_F$area_km2) 
# ks.test(data_F$area_km2, "pnorm", mean(data_F$area_km2), sd(data_F$area_km2))
# ggqqplot(data_F$area_km2)
# # Distribution of CONT variable
# ggdensity(data_F, x = "area_km2", fill = "lightgray", title = "Area F") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_F$area_km2)
# skewness(data_F$area_km2)

# #transition
# shapiro.test(data_transition$area_km2) 
# ks.test(data_transition$area_km2, "pnorm", mean(data_transition$area_km2), sd(data_transition$area_km2))
# ggqqplot(data_transition$area_km2)
# # Distribution of CONT variable
# ggdensity(data_transition, x = "area_km2", fill = "lightgray", title = "Area transition") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_transition$area_km2)
# skewness(data_transition$area_km2)

# #LW_R
# shapiro.test(data_LW_R$area_km2) 
# ks.test(data_LW_R$area_km2, "pnorm", mean(data_LW_R$area_km2), sd(data_LW_R$area_km2))
# ggqqplot(data_LW_R$area_km2)
# # Distribution of CONT variable
# ggdensity(data_LW_R, x = "area_km2", fill = "lightgray", title = "Area LW_R") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW_R$area_km2)
# skewness(data_LW_R$area_km2)

# #data_HW_F
# shapiro.test(data_HW_F$area_km2) 
# ks.test(data_HW_F$area_km2, "pnorm", mean(data_HW_F$area_km2), sd(data_HW_F$area_km2))
# ggqqplot(data_HW_F$area_km2)
# # Distribution of CONT variable
# ggdensity(data_HW_F, x = "area_km2", fill = "lightgray", title = "Area HW_F") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW_F$area_km2)
# skewness(data_HW_F$area_km2)

# ##TSS mean########
# #LW
# shapiro.test(data_LW$TSS_mean) 
# ks.test(data_LW$TSS_mean, "pnorm", mean(data_LW$TSS_mean), sd(data_LW$TSS_mean))
# ggqqplot(data_LW$TSS_mean)
# # Distribution of CONT variable
# ggdensity(data_LW, x = "TSS_mean", fill = "lightgray", title = "TSS LW") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW$TSS_mean)
# skewness(data_LW$TSS_mean)

# #HW
# shapiro.test(data_HW$TSS_mean) 
# ks.test(data_HW$TSS_mean, "pnorm", mean(data_HW$TSS_mean), sd(data_HW$TSS_mean))
# ggqqplot(data_HW$TSS_mean)
# # Distribution of CONT variable
# ggdensity(data_HW, x = "TSS_mean", fill = "lightgray", title = "TSS HW") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW$TSS_mean)
# skewness(data_HW$TSS_mean)

# #R
# shapiro.test(data_R$TSS_mean) 
# ks.test(data_R$TSS_mean, "pnorm", mean(data_R$TSS_mean), sd(data_R$TSS_mean))
# ggqqplot(data_R$TSS_mean)
# # Distribution of CONT variable
# ggdensity(data_R, x = "TSS_mean", fill = "lightgray", title = "TSS R") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_R$TSS_mean)
# skewness(data_R$TSS_mean)

# #F
# shapiro.test(data_F$TSS_mean) 
# ks.test(data_F$TSS_mean, "pnorm", mean(data_F$TSS_mean), sd(data_F$TSS_mean))
# ggqqplot(data_F$TSS_mean)
# # Distribution of CONT variable
# ggdensity(data_F, x = "TSS_mean", fill = "lightgray", title = "TSS F") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_F$TSS_mean)
# skewness(data_F$TSS_mean)

# #transition
# shapiro.test(data_transition$TSS_mean) 
# ks.test(data_transition$TSS_mean, "pnorm", mean(data_transition$TSS_mean), sd(data_transition$TSS_mean))
# ggqqplot(data_transition$TSS_mean)
# # Distribution of CONT variable
# ggdensity(data_transition, x = "TSS_mean", fill = "lightgray", title = "TSS transition") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_transition$TSS_mean)
# skewness(data_transition$TSS_mean)

# #LW_R
# shapiro.test(data_LW_R$TSS_mean) 
# ks.test(data_LW_R$TSS_mean, "pnorm", mean(data_LW_R$TSS_mean), sd(data_LW_R$TSS_mean))
# ggqqplot(data_LW_R$TSS_mean)
# # Distribution of CONT variable
# ggdensity(data_LW_R, x = "TSS_mean", fill = "lightgray", title = "TSS LW_R") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW_R$TSS_mean)
# skewness(data_LW_R$TSS_mean)

# #HW_F
# shapiro.test(data_HW_F$TSS_mean) 
# ks.test(data_HW_F$TSS_mean, "pnorm", mean(data_HW_F$TSS_mean), sd(data_HW_F$TSS_mean))
# ggqqplot(data_HW_F$TSS_mean)
# # Distribution of CONT variable
# ggdensity(data_HW_F, x = "TSS_mean", fill = "lightgray", title = "TSS HW_F") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW_F$TSS_mean)
# skewness(data_HW_F$TSS_mean)

# ##TSS max########
# #LW
# shapiro.test(data_LW$TSS_max) 
# ks.test(data_LW$TSS_max, "pnorm", mean(data_LW$TSS_max), sd(data_LW$TSS_max))
# ggqqplot(data_LW$TSS_max)
# # Distribution of CONT variable
# ggdensity(data_LW, x = "TSS_max", fill = "lightgray", title = "TSS LW") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW$TSS_max)
# skewness(data_LW$TSS_max)

# #HW
# shapiro.test(data_HW$TSS_max) 
# ks.test(data_HW$TSS_max, "pnorm", mean(data_HW$TSS_max), sd(data_HW$TSS_max))
# ggqqplot(data_HW$TSS_max)
# # Distribution of CONT variable
# ggdensity(data_HW, x = "TSS_max", fill = "lightgray", title = "TSS HW") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW$TSS_max)
# skewness(data_HW$TSS_max)

# #R
# shapiro.test(data_R$TSS_max) 
# ks.test(data_R$TSS_max, "pnorm", mean(data_R$TSS_max), sd(data_R$TSS_max))
# ggqqplot(data_R$TSS_max)
# # Distribution of CONT variable
# ggdensity(data_R, x = "TSS_max", fill = "lightgray", title = "TSS R") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_R$TSS_max)
# skewness(data_R$TSS_max)

# #F
# shapiro.test(data_F$TSS_max) 
# ks.test(data_F$TSS_max, "pnorm", mean(data_F$TSS_max), sd(data_F$TSS_max))
# ggqqplot(data_F$TSS_max)
# # Distribution of CONT variable
# ggdensity(data_F, x = "TSS_max", fill = "lightgray", title = "TSS F") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_F$TSS_max)
# skewness(data_F$TSS_max)

# #transition
# shapiro.test(data_transition$TSS_max) 
# ks.test(data_transition$TSS_max, "pnorm", mean(data_transition$TSS_max), sd(data_transition$TSS_max))
# ggqqplot(data_transition$TSS_max)
# # Distribution of CONT variable
# ggdensity(data_transition, x = "TSS_max", fill = "lightgray", title = "TSS transition") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_transition$TSS_max)
# skewness(data_transition$TSS_max)

# #LW_R
# shapiro.test(data_LW_R$TSS_max) 
# ks.test(data_LW_R$TSS_max, "pnorm", mean(data_LW_R$TSS_max), sd(data_LW_R$TSS_max))
# ggqqplot(data_LW_R$TSS_max)
# # Distribution of CONT variable
# ggdensity(data_LW_R, x = "TSS_max", fill = "lightgray", title = "TSS LW_R") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW_R$TSS_max)
# skewness(data_LW_R$TSS_max)

# #HW_F
# shapiro.test(data_HW_F$TSS_max) 
# ks.test(data_HW_F$TSS_max, "pnorm", mean(data_HW_F$TSS_max), sd(data_HW_F$TSS_max))
# ggqqplot(data_HW_F$TSS_max)
# # Distribution of CONT variable
# ggdensity(data_HW_F, x = "TSS_max", fill = "lightgray", title = "TSS HW_F") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW_F$TSS_max)
# skewness(data_HW_F$TSS_max)

# #TSS min########
# #LW
# shapiro.test(data_LW$TSS_min) 
# ks.test(data_LW$TSS_min, "pnorm", mean(data_LW$TSS_min), sd(data_LW$TSS_min))
# ggqqplot(data_LW$TSS_min)
# # Distribution of CONT variable
# ggdensity(data_LW, x = "TSS_min", fill = "lightgray", title = "TSS LW") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW$TSS_min)
# skewness(data_LW$TSS_min)

# #HW
# shapiro.test(data_HW$TSS_min) 
# ks.test(data_HW$TSS_min, "pnorm", mean(data_HW$TSS_min), sd(data_HW$TSS_min))
# ggqqplot(data_HW$TSS_min)
# # Distribution of CONT variable
# ggdensity(data_HW, x = "TSS_min", fill = "lightgray", title = "TSS HW") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW$TSS_min)
# skewness(data_HW$TSS_min)

# #R
# shapiro.test(data_R$TSS_min) 
# ks.test(data_R$TSS_min, "pnorm", mean(data_R$TSS_min), sd(data_R$TSS_min))
# ggqqplot(data_R$TSS_min)
# # Distribution of CONT variable
# ggdensity(data_R, x = "TSS_min", fill = "lightgray", title = "TSS R") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_R$TSS_min)
# skewness(data_R$TSS_min)

# #F
# shapiro.test(data_F$TSS_min) 
# ks.test(data_F$TSS_min, "pnorm", mean(data_F$TSS_min), sd(data_F$TSS_min))
# ggqqplot(data_F$TSS_min)
# # Distribution of CONT variable
# ggdensity(data_F, x = "TSS_min", fill = "lightgray", title = "TSS F") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_F$TSS_min)
# skewness(data_F$TSS_min)

# #transition
# shapiro.test(data_transition$TSS_min) 
# ks.test(data_transition$TSS_min, "pnorm", mean(data_transition$TSS_min), sd(data_transition$TSS_min))
# ggqqplot(data_transition$TSS_min)
# # Distribution of CONT variable
# ggdensity(data_transition, x = "TSS_min", fill = "lightgray", title = "TSS transition") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_transition$TSS_min)
# skewness(data_transition$TSS_min)

# #LW_R
# shapiro.test(data_LW_R$TSS_min) 
# ks.test(data_LW_R$TSS_min, "pnorm", mean(data_LW_R$TSS_min), sd(data_LW_R$TSS_min))
# ggqqplot(data_LW_R$TSS_min)
# # Distribution of CONT variable
# ggdensity(data_LW_R, x = "TSS_min", fill = "lightgray", title = "TSS LW_R") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW_R$TSS_min)
# skewness(data_LW_R$TSS_min)

# #HW_F
# shapiro.test(data_HW_F$TSS_min) 
# ks.test(data_HW_F$TSS_min, "pnorm", mean(data_HW_F$TSS_min), sd(data_HW_F$TSS_min))
# ggqqplot(data_HW_F$TSS_min)
# # Distribution of CONT variable
# ggdensity(data_HW_F, x = "TSS_min", fill = "lightgray", title = "TSS HW_F") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW_F$TSS_min)
# skewness(data_HW_F$TSS_min)

# ##Precipitation ####
# #LW
# shapiro.test(data_LW$mean_loc_chirps) 
# ks.test(data_LW$mean_loc_chirps, "pnorm", mean(data_LW$data_LW), sd(data_LW$mean_loc_chirps))
# ggqqplot(data_LW$mean_loc_chirps)
# # Distribution of CONT variable
# ggdensity(data_LW, x = "mean_loc_chirps", fill = "lightgray", title = "Precipitation LW") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW$mean_loc_chirps)
# skewness(data_LW$mean_loc_chirps)

# #HW
# shapiro.test(data_HW$mean_loc_chirps) 
# ks.test(data_HW$mean_loc_chirps, "pnorm", mean(data_HW$mean_loc_chirps), sd(data_HW$mean_loc_chirps))
# ggqqplot(data_HW$mean_loc_chirps)
# # Distribution of CONT variable
# ggdensity(data_HW, x = "mean_loc_chirps", fill = "lightgray", title = "Precipitation") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW$mean_loc_chirps)
# skewness(data_HW$mean_loc_chirps)

# #R
# shapiro.test(data_R$mean_loc_chirps) 
# ks.test(data_R$mean_loc_chirps, "pnorm", mean(data_R$mean_loc_chirps), sd(data_R$mean_loc_chirps))
# ggqqplot(data_R$mean_loc_chirps)
# # Distribution of CONT variable
# ggdensity(data_R, x = "mean_loc_chirps", fill = "lightgray", title = "Precipitation") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_R$mean_loc_chirps)
# skewness(data_R$mean_loc_chirps)

# #F
# shapiro.test(data_F$mean_loc_chirps) 
# ks.test(data_F$mean_loc_chirps, "pnorm", mean(data_F$mean_loc_chirps), sd(data_F$mean_loc_chirps))
# ggqqplot(data_F$mean_loc_chirps)
# # Distribution of CONT variable
# ggdensity(data_F, x = "mean_loc_chirps", fill = "lightgray", title = "Precipitation") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_F$mean_loc_chirps)
# skewness(data_F$mean_loc_chirps)

# #transition
# shapiro.test(data_transition$mean_loc_chirps) 
# ks.test(data_transition$mean_loc_chirps, "pnorm", mean(data_transition$mean_loc_chirps), sd(data_transition$mean_loc_chirps))
# ggqqplot(data_transition$mean_loc_chirps)
# # Distribution of CONT variable
# ggdensity(data_transition, x = "mean_loc_chirps", fill = "lightgray", title = "Precipitation") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_transition$mean_loc_chirps)
# skewness(data_transition$mean_loc_chirps)

# #LW_R
# shapiro.test(data_LW_R$mean_loc_chirps) 
# ks.test(data_LW_R$mean_loc_chirps, "pnorm", mean(data_LW_R$mean_loc_chirps), sd(data_LW_R$mean_loc_chirps))
# ggqqplot(data_LW_R$mean_loc_chirps)
# # Distribution of CONT variable
# ggdensity(data_LW_R, x = "mean_loc_chirps", fill = "lightgray", title = "Precipitation") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW_R$mean_loc_chirps)
# skewness(data_LW_R$mean_loc_chirps)

# #HW_F
# shapiro.test(data_HW_F$mean_loc_chirps) 
# ks.test(data_HW_F$mean_loc_chirps, "pnorm", mean(data_HW_F$mean_loc_chirps), sd(data_HW_F$mean_loc_chirps))
# ggqqplot(data_HW_F$mean_loc_chirps)
# # Distribution of CONT variable
# ggdensity(data_HW_F, x = "mean_loc_chirps", fill = "lightgray", title = "Precipitation") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW_F$mean_loc_chirps)
# skewness(data_HW_F$mean_loc_chirps)


# ##discharge#############################
# #LW
# shapiro.test(data_LW$mean_discharge) 
# ks.test(data_LW$mean_discharge, "pnorm", mean(data_LW$mean_discharge), sd(data_LW$mean_discharge))
# ggqqplot(data_LW$mean_discharge)
# # Distribution of CONT variable
# ggdensity(data_LW, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW$mean_discharge)
# skewness(data_LW$mean_discharge)

# #HW
# shapiro.test(data_HW$mean_discharge) 
# ks.test(data_HW$mean_discharge, "pnorm", mean(data_HW$mean_discharge), sd(data_HW$mean_discharge))
# ggqqplot(data_HW$mean_discharge)
# # Distribution of CONT variable
# ggdensity(data_HW, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW$mean_discharge)
# skewness(data_HW$mean_discharge)

# #R
# shapiro.test(data_R$mean_discharge) 
# ks.test(data_R$mean_discharge, "pnorm", mean(data_R$mean_discharge), sd(data_R$mean_discharge))
# ggqqplot(data_R$mean_discharge)
# # Distribution of CONT variable
# ggdensity(data_R, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_R$mean_discharge)
# skewness(data_R$mean_discharge)

# #F
# shapiro.test(data_F$mean_discharge) 
# ks.test(data_F$mean_discharge, "pnorm", mean(data_F$mean_discharge), sd(data_F$mean_discharge))
# ggqqplot(data_F$mean_discharge)
# # Distribution of CONT variable
# ggdensity(data_F, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_F$mean_discharge)
# skewness(data_F$mean_discharge)

# #transition
# shapiro.test(data_transition$mean_discharge) 
# ks.test(data_transition$mean_discharge, "pnorm", mean(data_transition$mean_discharge), sd(data_transition$mean_discharge))
# ggqqplot(data_transition$mean_discharge)
# # Distribution of CONT variable
# ggdensity(data_transition, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_transition$mean_discharge)
# skewness(data_transition$mean_discharge)

# #LW_R
# shapiro.test(data_LW_R$mean_discharge) 
# ks.test(data_LW_R$mean_discharge, "pnorm", mean(data_LW_R$mean_discharge), sd(data_LW_R$mean_discharge))
# ggqqplot(data_LW_R$mean_discharge)
# # Distribution of CONT variable
# ggdensity(data_LW_R, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW_R$mean_discharge)
# skewness(data_LW_R$mean_discharge)

# #HW_F
# shapiro.test(data_HW_F$mean_discharge) 
# ks.test(data_HW_F$mean_discharge, "pnorm", mean(data_HW_F$mean_discharge), sd(data_HW_F$mean_discharge))
# ggqqplot(data_HW_F$mean_discharge)
# # Distribution of CONT variable
# ggdensity(data_HW_F, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW_F$mean_discharge)
# skewness(data_HW_F$mean_discharge)

# ##wind U###############
# #LW
# shapiro.test(data_LW$u_wind_loc) 
# ks.test(data_LW$u_wind_loc, "pnorm", mean(data_LW$u_wind_loc), sd(data_LW$u_wind_loc))
# ggqqplot(data_LW$u_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_LW, x = "u_wind_loc", fill = "lightgray", title = "Wind U") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW$u_wind_loc)
# skewness(data_LW$u_wind_loc)

# #HW
# shapiro.test(data_HW$u_wind_loc) 
# ks.test(data_HW$u_wind_loc, "pnorm", mean(data_HW$u_wind_loc), sd(data_HW$u_wind_loc))
# ggqqplot(data_HW$u_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_HW, x = "u_wind_loc", fill = "lightgray", title = "Wind U") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW$u_wind_loc)
# skewness(data_HW$u_wind_loc)

# #R
# shapiro.test(data_R$u_wind_loc) 
# ks.test(data_R$u_wind_loc, "pnorm", mean(data_R$u_wind_loc), sd(data_R$u_wind_loc))
# ggqqplot(data_R$u_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_R, x = "u_wind_loc", fill = "lightgray", title = "Wind U") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_R$u_wind_loc)
# skewness(data_R$u_wind_loc)

# #F
# shapiro.test(data_F$u_wind_loc) 
# ks.test(data_F$u_wind_loc, "pnorm", mean(data_F$u_wind_loc), sd(data_F$u_wind_loc))
# ggqqplot(data_F$u_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_F, x = "u_wind_loc", fill = "lightgray", title = "Wind U") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_F$u_wind_loc)
# skewness(data_F$u_wind_loc)

# #transition
# shapiro.test(data_transition$u_wind_loc) 
# ks.test(data_transition$u_wind_loc, "pnorm", mean(data_transition$u_wind_loc), sd(data_transition$u_wind_loc))
# ggqqplot(data_transition$u_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_transition, x = "u_wind_loc", fill = "lightgray", title = "Wind U") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_transition$u_wind_loc)
# skewness(data_transition$u_wind_loc)

# #LW_R
# shapiro.test(data_LW_R$u_wind_loc) 
# ks.test(data_LW_R$u_wind_loc, "pnorm", mean(data_LW_R$u_wind_loc), sd(data_LW_R$u_wind_loc))
# ggqqplot(data_LW_R$u_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_LW_R, x = "u_wind_loc", fill = "lightgray", title = "Wind U") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW_R$u_wind_loc)
# skewness(data_LW_R$u_wind_loc)

# #HW_F
# shapiro.test(data_HW_F$u_wind_loc) 
# ks.test(data_HW_F$u_wind_loc, "pnorm", mean(data_HW_F$u_wind_loc), sd(data_HW_F$u_wind_loc))
# ggqqplot(data_HW_F$u_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_HW_F, x = "u_wind_loc", fill = "lightgray", title = "Wind U") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW_F$u_wind_loc)
# skewness(data_HW_F$u_wind_loc)

# ##wind V####################
# #LW
# shapiro.test(data_LW$v_wind_loc) 
# ks.test(data_LW$v_wind_loc, "pnorm", mean(data_LW$v_wind_loc), sd(data_LW$v_wind_loc))
# ggqqplot(data_LW$v_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_LW, x = "v_wind_loc", fill = "lightgray", title = "Wind V") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW$v_wind_loc)
# skewness(data_LW$v_wind_loc)

# #HW
# shapiro.test(data_HW$v_wind_loc) 
# ks.test(data_HW$v_wind_loc, "pnorm", mean(data_HW$v_wind_loc), sd(data_HW$v_wind_loc))
# ggqqplot(data_HW$v_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_HW, x = "v_wind_loc", fill = "lightgray", title = "Wind V") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW$v_wind_loc)
# skewness(data_HW$v_wind_loc)

# #R
# shapiro.test(data_R$v_wind_loc) 
# ks.test(data_R$v_wind_loc, "pnorm", mean(data_R$v_wind_loc), sd(data_R$v_wind_loc))
# ggqqplot(data_R$v_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_R, x = "v_wind_loc", fill = "lightgray", title = "Wind V") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_R$v_wind_loc)
# skewness(data_R$v_wind_loc)

# #F
# shapiro.test(data_F$v_wind_loc) 
# ks.test(data_F$v_wind_loc, "pnorm", mean(data_F$v_wind_loc), sd(data_F$v_wind_loc))
# ggqqplot(data_F$v_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_F, x = "v_wind_loc", fill = "lightgray", title = "Wind V") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_F$v_wind_loc)
# skewness(data_F$v_wind_loc)

# #transition
# shapiro.test(data_transition$v_wind_loc) 
# ks.test(data_transition$v_wind_loc, "pnorm", mean(data_transition$v_wind_loc), sd(data_transition$v_wind_loc))
# ggqqplot(data_transition$v_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_transition, x = "v_wind_loc", fill = "lightgray", title = "Wind V") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_transition$v_wind_loc)
# skewness(data_transition$v_wind_loc)

# #LW_R
# shapiro.test(data_LW_R$v_wind_loc) 
# ks.test(data_LW_R$v_wind_loc, "pnorm", mean(data_LW_R$v_wind_loc), sd(data_LW_R$v_wind_loc))
# ggqqplot(data_LW_R$v_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_LW_R, x = "v_wind_loc", fill = "lightgray", title = "Wind V") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW_R$v_wind_loc)
# skewness(data_LW_R$v_wind_loc)

# #HW_F
# shapiro.test(data_HW_F$v_wind_loc) 
# ks.test(data_HW_F$v_wind_loc, "pnorm", mean(data_HW_F$v_wind_loc), sd(data_HW_F$v_wind_loc))
# ggqqplot(data_HW_F$v_wind_loc)
# # Distribution of CONT variable
# ggdensity(data_HW_F, x = "v_wind_loc", fill = "lightgray", title = "Wind V") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW_F$v_wind_loc)
# skewness(data_HW_F$v_wind_loc)

# ##anthropic km2 ####################
# #LW
# shapiro.test(data_LW$anthropogenic_km2) 
# ks.test(data_LW$anthropogenic_km2, "pnorm", mean(data_LW$anthropogenic_km2), sd(data_LW$anthropogenic_km2))
# ggqqplot(data_LW$anthropogenic_km2)
# # Distribution of CONT variable
# ggdensity(data_LW, x = "anthropogenic_km2", fill = "lightgray", title = "anthropic area") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW$anthropogenic_km2)
# skewness(data_LW$anthropogenic_km2)

# #HW
# shapiro.test(data_HW$anthropogenic_km2) 
# ks.test(data_HW$anthropogenic_km2, "pnorm", mean(data_HW$anthropogenic_km2), sd(data_HW$anthropogenic_km2))
# ggqqplot(data_HW$anthropogenic_km2)
# # Distribution of CONT variable
# ggdensity(data_HW, x = "anthropogenic_km2", fill = "lightgray", title = "anthropic area") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW$anthropogenic_km2)
# skewness(data_HW$anthropogenic_km2)

# #R
# shapiro.test(data_R$anthropogenic_km2) 
# ks.test(data_R$anthropogenic_km2, "pnorm", mean(data_R$anthropogenic_km2), sd(data_R$anthropogenic_km2))
# ggqqplot(data_R$anthropogenic_km2)
# # Distribution of CONT variable
# ggdensity(data_R, x = "anthropogenic_km2", fill = "lightgray", title = "anthropic area") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_R$anthropogenic_km2)
# skewness(data_R$anthropogenic_km2)

# #F
# shapiro.test(data_F$anthropogenic_km2) 
# ks.test(data_F$anthropogenic_km2, "pnorm", mean(data_F$anthropogenic_km2), sd(data_F$anthropogenic_km2))
# ggqqplot(data_F$anthropogenic_km2)
# # Distribution of CONT variable
# ggdensity(data_F, x = "anthropogenic_km2", fill = "lightgray", title = "anthropic area") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_F$anthropogenic_km2)
# skewness(data_F$anthropogenic_km2)

# #transition
# shapiro.test(data_transition$anthropogenic_km2) 
# ks.test(data_transition$anthropogenic_km2, "pnorm", mean(data_transition$anthropogenic_km2), sd(data_transition$anthropogenic_km2))
# ggqqplot(data_transition$anthropogenic_km2)
# # Distribution of CONT variable
# ggdensity(data_transition, x = "anthropogenic_km2", fill = "lightgray", title = "anthropic area") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_transition$anthropogenic_km2)
# skewness(data_transition$anthropogenic_km2)

# #LW_R
# shapiro.test(data_LW_R$anthropogenic_km2) 
# ks.test(data_LW_R$anthropogenic_km2, "pnorm", mean(data_LW_R$anthropogenic_km2), sd(data_LW_R$anthropogenic_km2))
# ggqqplot(data_LW_R$anthropogenic_km2)
# # Distribution of CONT variable
# ggdensity(data_LW_R, x = "anthropogenic_km2", fill = "lightgray", title = "anthropic area") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_LW_R$anthropogenic_km2)
# skewness(data_LW_R$anthropogenic_km2)

# #HW_F
# shapiro.test(data_HW_F$anthropogenic_km2) 
# ks.test(data_HW_F$anthropogenic_km2, "pnorm", mean(data_HW_F$anthropogenic_km2), sd(data_HW_F$anthropogenic_km2))
# ggqqplot(data_HW_F$anthropogenic_km2)
# # Distribution of CONT variable
# ggdensity(data_HW_F, x = "anthropogenic_km2", fill = "lightgray", title = "anthropic area") +
#     stat_overlay_normal_density(color = "red", linetype = "dashed")
# boxplot(data_HW_F$anthropogenic_km2)
# skewness(data_HW_F$anthropogenic_km2)

# #calcular correlacao##############################################################
# #pacotes
# library(corrplot)
# library(tidyverse)
# library(lmtest)
# library(psych)

# #testar se a covari?ncia ? linear - verificar scatter plot
# ##mean TSS#############
# #LW
# ggscatter(data_LW, x = 'TSS_mean', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
#           ylab = 'area_km2') 

# ggscatter(data_LW, x = 'TSS_mean', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_LW, x = 'TSS_mean', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_discharge') 

# ggscatter(data_LW, x = 'TSS_mean', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_LW, x = 'TSS_mean', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_LW, x = 'TSS_mean', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'anthropogenic_km2') 

# #HW
# ggscatter(data_HW, x = 'TSS_mean', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
#           ylab = 'area_km2') 

# ggscatter(data_HW, x = 'TSS_mean', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_HW, x = 'TSS_mean', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_discharge') 

# ggscatter(data_HW, x = 'TSS_mean', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_HW, x = 'TSS_mean', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_HW, x = 'TSS_mean', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'anthropogenic_km2') 

# #R
# ggscatter(data_R, x = 'TSS_mean', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
#           ylab = 'area_km2') 

# ggscatter(data_R, x = 'TSS_mean', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_R, x = 'TSS_mean', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_discharge') 

# ggscatter(data_R, x = 'TSS_mean', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_R, x = 'TSS_mean', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_R, x = 'TSS_mean', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'anthropogenic_km2') 

# #F
# ggscatter(data_F, x = 'TSS_mean', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
#           ylab = 'area_km2') 

# ggscatter(data_F, x = 'TSS_mean', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_F, x = 'TSS_mean', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_discharge') 

# ggscatter(data_F, x = 'TSS_mean', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_F, x = 'TSS_mean', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_F, x = 'TSS_mean', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'anthropogenic_km2') 

# #transition
# ggscatter(data_transition, x = 'TSS_mean', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
#           ylab = 'area_km2') 

# ggscatter(data_transition, x = 'TSS_mean', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_transition, x = 'TSS_mean', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_discharge') 

# ggscatter(data_transition, x = 'TSS_mean', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_transition, x = 'TSS_mean', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_transition, x = 'TSS_mean', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'anthropogenic_km2') 

# #LW_R
# ggscatter(data_LW_R, x = 'TSS_mean', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
#           ylab = 'area_km2') 

# ggscatter(data_LW_R, x = 'TSS_mean', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_LW_R, x = 'TSS_mean', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_discharge') 

# ggscatter(data_LW_R, x = 'TSS_mean', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_LW_R, x = 'TSS_mean', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_LW_R, x = 'TSS_mean', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'anthropogenic_km2') 

# #HW_F
# ggscatter(data_HW_F, x = 'TSS_mean', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean',
#           ylab = 'area_km2') 

# ggscatter(data_HW_F, x = 'TSS_mean', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_HW_F, x = 'TSS_mean', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'mean_discharge') 

# ggscatter(data_HW_F, x = 'TSS_mean', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_HW_F, x = 'TSS_mean', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_HW_F, x = 'TSS_mean', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'anthropogenic_km2') 

# ##min TSS#######
# #LW
# ggscatter(data_LW, x = 'TSS_min', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min',
#           ylab = 'area_km2') 

# ggscatter(data_LW, x = 'TSS_min', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_LW, x = 'TSS_min', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_discharge') 

# ggscatter(data_LW, x = 'TSS_min', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_LW, x = 'TSS_min', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_LW, x = 'TSS_min', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'anthropogenic_km2') 

# #HW
# ggscatter(data_HW, x = 'TSS_min', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min',
#           ylab = 'area_km2') 

# ggscatter(data_HW, x = 'TSS_min', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_HW, x = 'TSS_min', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_discharge') 

# ggscatter(data_HW, x = 'TSS_min', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_HW, x = 'TSS_min', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_HW, x = 'TSS_min', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'anthropogenic_km2') 

# #R
# ggscatter(data_R, x = 'TSS_min', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min',
#           ylab = 'area_km2') 

# ggscatter(data_R, x = 'TSS_min', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_R, x = 'TSS_min', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_discharge') 

# ggscatter(data_R, x = 'TSS_min', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_R, x = 'TSS_min', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_R, x = 'TSS_min', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'anthropogenic_km2') 

# #F
# ggscatter(data_F, x = 'TSS_min', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min',
#           ylab = 'area_km2') 

# ggscatter(data_F, x = 'TSS_min', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_F, x = 'TSS_min', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_discharge') 

# ggscatter(data_F, x = 'TSS_min', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_F, x = 'TSS_min', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_F, x = 'TSS_min', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'anthropogenic_km2') 

# #transition
# ggscatter(data_transition, x = 'TSS_min', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min',
#           ylab = 'area_km2') 

# ggscatter(data_transition, x = 'TSS_min', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_transition, x = 'TSS_min', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_discharge') 

# ggscatter(data_transition, x = 'TSS_min', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_transition, x = 'TSS_min', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_transition, x = 'TSS_min', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'anthropogenic_km2') 

# #LW_R
# ggscatter(data_LW_R, x = 'TSS_min', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min',
#           ylab = 'area_km2') 

# ggscatter(data_LW_R, x = 'TSS_min', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_LW_R, x = 'TSS_min', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_discharge') 

# ggscatter(data_LW_R, x = 'TSS_min', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_LW_R, x = 'TSS_min', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_LW_R, x = 'TSS_min', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'anthropogenic_km2') 

# #HW_F
# ggscatter(data_HW_F, x = 'TSS_min', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min',
#           ylab = 'area_km2') 

# ggscatter(data_HW_F, x = 'TSS_min', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_HW_F, x = 'TSS_min', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_discharge') 

# ggscatter(data_HW_F, x = 'TSS_min', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_HW_F, x = 'TSS_min', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_HW_F, x = 'TSS_min', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'anthropogenic_km2') 

# ##max TSS#############
# #LW
# ggscatter(data_LW, x = 'TSS_max', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max',
#           ylab = 'area_km2') 

# ggscatter(data_LW, x = 'TSS_max', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_LW, x = 'TSS_max', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'mean_discharge') 

# ggscatter(data_LW, x = 'TSS_max', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_LW, x = 'TSS_max', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_LW, x = 'TSS_max', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'anthropogenic_km2') 

# #HW
# ggscatter(data_HW, x = 'TSS_max', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max',
#           ylab = 'area_km2') 

# ggscatter(data_HW, x = 'TSS_max', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_HW, x = 'TSS_max', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'mean_discharge') 

# ggscatter(data_HW, x = 'TSS_max', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_HW, x = 'TSS_max', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_HW, x = 'TSS_max', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'anthropogenic_km2') 

# #R
# ggscatter(data_R, x = 'TSS_max', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max',
#           ylab = 'area_km2') 

# ggscatter(data_R, x = 'TSS_max', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_R, x = 'TSS_max', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'mean_discharge') 

# ggscatter(data_R, x = 'TSS_max', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_R, x = 'TSS_max', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_R, x = 'TSS_max', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'anthropogenic_km2') 

# #F
# ggscatter(data_F, x = 'TSS_max', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'pearson', xlab = 'TSS_max',
#           ylab = 'area_km2') 

# ggscatter(data_F, x = 'TSS_max', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_F, x = 'TSS_max', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'mean_discharge') 

# ggscatter(data_F, x = 'TSS_max', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_F, x = 'TSS_max', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'pearson', xlab = 'TSS_max', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_F, x = 'TSS_max', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'pearson', xlab = 'TSS_max', 
#           ylab = 'anthropogenic_km2') 

# #transition
# ggscatter(data_transition, x = 'TSS_max', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max',
#           ylab = 'area_km2') 

# ggscatter(data_transition, x = 'TSS_max', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_transition, x = 'TSS_max', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'mean_discharge') 

# ggscatter(data_transition, x = 'TSS_max', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_transition, x = 'TSS_max', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_transition, x = 'TSS_max', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'anthropogenic_km2') 

# #LW_R
# ggscatter(data_LW_R, x = 'TSS_max', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max',
#           ylab = 'area_km2') 

# ggscatter(data_LW_R, x = 'TSS_max', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_LW_R, x = 'TSS_max', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'mean_discharge') 

# ggscatter(data_LW_R, x = 'TSS_max', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_LW_R, x = 'TSS_max', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'pearson', xlab = 'TSS_max', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_LW_R, x = 'TSS_max', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'pearson', xlab = 'TSS_max', 
#           ylab = 'anthropogenic_km2') 

# #HW_F
# ggscatter(data_HW_F, x = 'TSS_max', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_max',
#           ylab = 'area_km2') 

# ggscatter(data_HW_F, x = 'TSS_min', y = 'mean_loc_chirps', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_loc_chirps') 

# ggscatter(data_HW_F, x = 'TSS_min', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'mean_discharge') 

# ggscatter(data_HW_F, x = 'TSS_min', y = 'u_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_min', 
#           ylab = 'u_wind_loc') 

# ggscatter(data_HW_F, x = 'TSS_mean', y = 'v_wind_loc', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'v_wind_loc') 

# ggscatter(data_HW_F, x = 'TSS_mean', y = 'anthropogenic_km2', add = 'reg.line', conf.int = TRUE,
#           cor.coef = TRUE, cor.method = 'kendall', xlab = 'TSS_mean', 
#           ylab = 'anthropogenic_km2') 

# #matriz de corelacao############################################################
# #LW
# data_LW<-select(data_LW, TSS_mean  , area_km2  ,  mean_loc_chirps,
#                     mean_discharge,anthropogenic_km2,u_wind_loc,v_wind_loc,TSS_max,TSS_min)
# correlation_LW_sp <- cor(data_LW, method = 'kendall')
# corrplot::corrplot.mixed(correlation_LW_sp, upper = 'ellipse', lower = 'number', las = 1)
# correlation_LW_pea <- cor(data_LW, method = 'pearson')
# corrplot::corrplot.mixed(correlation_LW_pea, upper = 'ellipse', lower = 'number', las = 1)

# #HW
# data_HW<-select(data_HW, TSS_mean  , area_km2  ,  mean_loc_chirps,
#                 mean_discharge,anthropogenic_km2,u_wind_loc,v_wind_loc,TSS_max,TSS_min)
# correlation_HW_sp <- cor(data_HW, method = 'kendall')
# corrplot::corrplot.mixed(correlation_HW_sp, upper = 'ellipse', lower = 'number', las = 1)
# correlation_HW_pea <- cor(data_HW, method = 'pearson')
# corrplot::corrplot.mixed(correlation_HW_pea, upper = 'ellipse', lower = 'number', las = 1)

# #R
# data_R<-select(data_R, TSS_mean  , area_km2  ,  mean_loc_chirps,
#                 mean_discharge,anthropogenic_km2,u_wind_loc,v_wind_loc,TSS_max,TSS_min)
# correlation_R_sp <- cor(data_R, method = 'kendall')
# corrplot::corrplot.mixed(correlation_R_sp, upper = 'ellipse', lower = 'number', las = 1)
# correlation_R_pea <- cor(data_R, method = 'pearson')
# corrplot::corrplot.mixed(correlation_R_pea, upper = 'ellipse', lower = 'number', las = 1)

# #F
# data_F<-select(data_F, TSS_mean  , area_km2  ,  mean_loc_chirps,
#                 mean_discharge,anthropogenic_km2,u_wind_loc,v_wind_loc,TSS_max,TSS_min)
# correlation_F_sp <- cor(data_F, method = 'kendall')
# corrplot::corrplot.mixed(correlation_F_sp, upper = 'ellipse', lower = 'number', las = 1)
# correlation_F_pea <- cor(data_F, method = 'pearson')
# corrplot::corrplot.mixed(correlation_F_pea, upper = 'ellipse', lower = 'number', las = 1)

# #transition
# data_transition<-select(data_transition, TSS_mean  , area_km2  ,  mean_loc_chirps,
#                 mean_discharge,anthropogenic_km2,u_wind_loc,v_wind_loc,TSS_max,TSS_min)
# correlation_transition_sp <- cor(data_transition, method = 'kendall')
# corrplot::corrplot.mixed(correlation_transition_sp, upper = 'ellipse', lower = 'number', las = 1)
# correlation_transition_pea <- cor(data_transition, method = 'pearson')
# corrplot::corrplot.mixed(correlation_transition_pea, upper = 'ellipse', lower = 'number', las = 1)

# #LW_R
# data_LW_R<-select(data_LW_R, TSS_mean  , area_km2  ,  mean_loc_chirps,
#                 mean_discharge,anthropogenic_km2,u_wind_loc,v_wind_loc,TSS_max,TSS_min)
# correlation_LW_R_sp <- cor(data_LW_R, method = 'kendall')
# corrplot::corrplot.mixed(correlation_LW_R_sp, upper = 'ellipse', lower = 'number', las = 1)
# correlation_LW_R_pea <- cor(data_LW_R, method = 'pearson')
# corrplot::corrplot.mixed(correlation_LW_R_pea, upper = 'ellipse', lower = 'number', las = 1)

# #HW_F
# data_HW_F<-select(data_HW_F, TSS_mean  , area_km2  ,  mean_loc_chirps,
#                 mean_discharge,anthropogenic_km2,u_wind_loc,v_wind_loc,TSS_max,TSS_min)
# correlation_HW_F_sp <- cor(data_HW_F, method = 'kendall')
# corrplot::corrplot.mixed(correlation_HW_F_sp, upper = 'ellipse', lower = 'number', las = 1)
# correlation_HW_F_pea <- cor(data_HW_F, method = 'pearson')
# corrplot::corrplot.mixed(correlation_HW_F_pea, upper = 'ellipse', lower = 'number', las = 1)

# write.csv(correlation_LW_sp, file = "correlationLW_sp.csv")
# write.csv(correlation_LW_pea, file = "correlationLW_pea.csv")
# write.csv(correlation_HW_sp, file = "correlationHW_sp.csv")
# write.csv(correlation_HW_pea, file = "correlationHW_pea.csv")
# write.csv(correlation_R_sp, file = "correlationR_sp.csv")
# write.csv(correlation_R_pea, file = "correlationR_pea.csv")
# write.csv(correlation_F_sp, file = "correlationF_sp.csv")
# write.csv(correlation_F_pea, file = "correlationF_pea.csv")
# write.csv(correlation_transition_sp, file = "correlationtransition_sp.csv")
# write.csv(correlation_transition_pea, file = "correlationtransition_pea.csv")
# write.csv(correlation_LW_R_sp, file = "correlationR_sp.csv")
# write.csv(correlation_LW_R_pea, file = "correlationR_pea.csv")
# write.csv(correlation_HW_F_sp, file = "correlationHW_F_sp.csv")
# write.csv(correlation_HW_F_pea, file = "correlationHW_F_pea.csv")

# #verificar se as correlacoes sao significativas##################################
# ### fazer para o dataset escolhiodo
# cor.test(data$TSS_mean, data$area_km2, method = 'kendall')
# cor.test(data$TSS_mean, data$mean_loc_chirps, method = 'kendall')
# cor.test(data$TSS_mean, data$mean_discharge, method = 'kendall')
# cor.test(data$TSS_mean, data$u_wind_loc, method = 'kendall')
# cor.test(data$TSS_mean, data$v_wind_loc, method = 'kendall')



