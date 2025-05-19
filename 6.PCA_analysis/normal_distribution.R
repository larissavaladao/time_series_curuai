# dados dos mosaicos com periodos de água definidos por cotas em Obidos ####

#definir diretório de trabalho e importar dados ##################

#definir diret?rio de trabalho e importar dados ####
setwd("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df")
# setwd("C:/Users/LarissaVieiraValadão/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df")

library(readr)
library(dplyr)
data <- as.data.frame(read.csv("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df/df_merged_cota_lulc.csv"))
# data <- as.data.frame(read.csv("C:/Users/LarissaVieiraValadão/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df/df_merged_cota_lulc.csv"))
head(data)
summary(data)

#imputar valores que estao faltando ###################
#500 iterations of predictive mean mapping for imputing 
#5 datasets
library(mice)
library(dplyr)


#imputar dados faltantes faltantes
selecao <- select(data, mean_u__wind  , mean_v__wind  , anthropic_km2, area_km2,mean_SPM,mean_precipitation)
dados_imp <- mice(selecao, m = 5, maxit = 100, method = 'pmm', seed = 500)
dados_comp <- complete(dados_imp, 5)#numero do dataset cujas imputa??es vc quer usar
dados_comp


selecao <- select(data, -X,-mean_u__wind,-mean_v__wind,-anthropic_km2)

selecao$mean_u__wind <- dados_comp$mean_u__wind
selecao$mean_v__wind <- dados_comp$mean_v__wind
selecao$anthropic_km2 <- dados_comp$anthropic_km2

write.csv(selecao, file = "filled_data_cota_lulc.csv")


#ler os arquivos ja substituidos para calcular as correlacoes e normalidade ###########################
#todos os periodos de agua incluidos
data_select <- as.data.frame(read.csv("filled_data_cota_lulc.csv"))
data_select <- select(data_select,-X,-class_name)
head(data_select)
summary(data_select)

#testes preliminares #####################################
library(ggpubr)
library(moments)


#tanto Shapiro Wilk quanto Korogonov tem resultados parecidos - para testar normalidade
##testar normalidade#####
#Area
shapiro.test(data_select$area_km2) #dados normais
ks.test(data_select$area_km2, "pnorm", mean(data_select$area_km2), sd(data_select$area_km2))
ggqqplot(data_select$area_km2)
# Distribution of CONT variable
ggdensity(data_select, x = "area_km2", fill = "lightgray", title = "Area") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$area_km2)
skewness(data_select$area_km2)

#SPM mean
shapiro.test(data_select$mean_SPM) #dados normais
ks.test(data_select$mean_SPM, "pnorm", mean(data_select$mean_SPM), sd(data_select$mean_SPM))
ggqqplot(data_select$mean_SPM)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_SPM", fill = "lightgray", title = "SPM") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_SPM)
skewness(data_select$mean_SPM)

#SPM max
shapiro.test(data_select$max_SPM) #dados normais
ks.test(data_select$max_SPM, "pnorm", mean(data_select$max_SPM), sd(data_select$max_SPM))
ggqqplot(data_select$max_SPM)
# Distribution of CONT variable
ggdensity(data_select, x = "max_SPM", fill = "lightgray", title = "SPM") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$max_SPM)
skewness(data_select$max_SPM)

#SPM min
shapiro.test(data_select$min_SPM) #dados normais
ks.test(data_select$min_SPM, "pnorm", mean(data_select$min_SPM), sd(data_select$min_SPM))
ggqqplot(data_select$min_SPM)
# Distribution of CONT variable
ggdensity(data_select, x = "min_SPM", fill = "lightgray", title = "SPM") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$min_SPM)
skewness(data_select$min_SPM)

#Precipitation
shapiro.test(data_select$mean_precipitation) #dados normais
ks.test(data_select$mean_precipitation, "pnorm", mean(data_select$mean_precipitation), sd(data_select$mean_precipitation))
ggqqplot(data_select$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_precipitation", fill = "lightgray", title = "Precipitation") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_precipitation)
skewness(data_select$mean_precipitation)

# discharge
shapiro.test(data_select$mean_discharge) #dados normais
ks.test(data_select$mean_discharge, "pnorm", mean(data_select$mean_discharge), sd(data_select$mean_discharge))
ggqqplot(data_select$mean_discharge)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_discharge)
skewness(data_select$mean_discharge)

#wind U
shapiro.test(data_select$mean_u__wind) #dados normais
ks.test(data_select$mean_u__wind, "pnorm", mean(data_select$mean_u__wind), sd(data_select$mean_u__wind))
ggqqplot(data_select$mean_u__wind)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_u__wind", fill = "lightgray", title = "Wind U") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_u__wind)
skewness(data_select$mean_u__wind)

#wind V
shapiro.test(data_select$mean_v__wind) #dados normais
ks.test(data_select$mean_v__wind, "pnorm", mean(data_select$mean_v__wind), sd(data_select$mean_v__wind))
ggqqplot(data_select$mean_v__wind)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_v__wind", fill = "lightgray", title = "Wind V") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_v__wind)
skewness(data_select$mean_v__wind)

#anthropic_km2
shapiro.test(data_select$anthropic_km2) #dados normais
ks.test(data_select$anthropic_km2, "pnorm", mean(data_select$anthropic_km2), sd(data_select$anthropic_km2))
ggqqplot(data_select$anthropic_km2)
# Distribution of CONT variable
ggdensity(data_select, x = "anthropic_km2", fill = "lightgray", title = "anthropic_km2") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$anthropic_km2)
skewness(data_select$anthropic_km2)

#calcular correlacaoo##############################################################
#pacotes
library(corrplot)
library(tidyverse)
library(lmtest)
library(psych)

#testar se a covari?ncia ? linear - verificar scatter plot
#mean SPM
ggscatter(data_select, x = 'mean_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear
ggscatter(data_select, x = 'mean_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'anthropic_km2') #cov linear

#max SPM
ggscatter(data_select, x = 'max_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_select, x = 'max_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_select, x = 'max_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_select, x = 'max_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_select, x = 'max_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_v__wind') #cov linear
ggscatter(data_select, x = 'max_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'anthropic_km2') #cov linear

#min SPM
ggscatter(data_select, x = 'min_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_select, x = 'min_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_select, x = 'min_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_select, x = 'min_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_select, x = 'min_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_v__wind') #cov linear
ggscatter(data_select, x = 'min_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'anthropic_km2') #cov linear

#matriz de corelacao############################################################

data_select<-select(data_select, mean_SPM  , area_km2,  mean_precipitation,
                    mean_discharge ,mean_u__wind,mean_v__wind,anthropic_km2,max_SPM  ,min_SPM  ,)
correlation <- cor(data_select, method = 'spearman')

corrplot::corrplot.mixed(correlation, upper = 'ellipse', lower = 'number', las = 1)


write.csv(correlation, file = "correlation_cota_lulc.csv")

#verificar se as correlacoes sao significativas##################################
#mean SPM
cor.test(data_select$mean_SPM, data_select$area_km2, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_precipitation, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_discharge, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_u__wind, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_v__wind, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$anthropic_km2, method = 'spearman')

#max SPM
cor.test(data_select$max_SPM, data_select$area_km2, method = 'spearman')
cor.test(data_select$max_SPM, data_select$mean_precipitation, method = 'spearman')
cor.test(data_select$max_SPM, data_select$mean_discharge, method = 'spearman')
cor.test(data_select$max_SPM, data_select$mean_u__wind, method = 'spearman')
cor.test(data_select$max_SPM, data_select$mean_v__wind, method = 'spearman')
cor.test(data_select$max_SPM, data_select$anthropic_km2, method = 'spearman')

#min SPM
cor.test(data_select$min_SPM, data_select$area_km2, method = 'spearman')
cor.test(data_select$min_SPM, data_select$mean_precipitation, method = 'spearman')
cor.test(data_select$min_SPM, data_select$mean_discharge, method = 'spearman')
cor.test(data_select$min_SPM, data_select$mean_u__wind, method = 'spearman')
cor.test(data_select$min_SPM, data_select$mean_v__wind, method = 'spearman')
cor.test(data_select$min_SPM, data_select$anthropic_km2, method = 'spearman')





#DADOS Separados por periodos##################################################################################
data_select <- as.data.frame(read.csv("filled_data_cota_lulc.csv"))
data_select <- select(data_select,-X,-class_name)
data_select$water_period <- as.factor(data_select$water_period)

# checking which values are from the period
data_LW <- filter(data_select,water_period=='LW') 
# data_LW
summary(data_LW)

data_HW<- filter(data_select,water_period=='HW') 
# data_HW
summary(data_HW)

data_R<- filter(data_select,water_period=='R') 
# data_R
summary(data_R)

data_F<- filter(data_select,water_period=='F') 
# data_F
summary(data_F)

data_transition<- filter(data_select, water_period=='F' | water_period=='R') 
# data_transition
summary(data_transition)

data_LW_R <- filter(data_select,water_period=='LW' | water_period=='R') 
# data_LW_R
summary(data_LW_R)

data_HW_F<- filter(data_select,water_period=='HW'| water_period=='F') 
# data_HW_F
summary(data_HW_F)


#testes preliminares #####################################

# tanto Shapiro Wilk quanto Korogonov tem resultados parecidos - para testar normalidade
#testar normalidade 
##Area####
#LW
shapiro.test(data_LW$area_km2) #dados normais
ks.test(data_LW$area_km2, "pnorm", mean(data_LW$area_km2), sd(data_LW$area_km2))
ggqqplot(data_LW$area_km2)
# Distribution of CONT variable
ggdensity(data_LW, x = "area_km2", fill = "lightgray", title = "Area LW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW$area_km2)
skewness(data_LW$area_km2)

#HW
shapiro.test(data_HW$area_km2) #dados normais
ks.test(data_HW$area_km2, "pnorm", mean(data_HW$area_km2), sd(data_HW$area_km2))
ggqqplot(data_HW$area_km2)
# Distribution of CONT variable
ggdensity(data_HW, x = "area_km2", fill = "lightgray", title = "Area HW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW$area_km2)
skewness(data_HW$area_km2)

#R
shapiro.test(data_R$area_km2) #dados normais
ks.test(data_R$area_km2, "pnorm", mean(data_R$area_km2), sd(data_R$area_km2))
ggqqplot(data_R$area_km2)
# Distribution of CONT variable
ggdensity(data_R, x = "area_km2", fill = "lightgray", title = "Area R") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_R$area_km2)
skewness(data_R$area_km2)

#F
shapiro.test(data_F$area_km2) #dados normais
ks.test(data_F$area_km2, "pnorm", mean(data_F$area_km2), sd(data_F$area_km2))
ggqqplot(data_F$area_km2)
# Distribution of CONT variable
ggdensity(data_F, x = "area_km2", fill = "lightgray", title = "Area F") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_F$area_km2)
skewness(data_F$area_km2)

#transition
shapiro.test(data_transition$area_km2) #dados normais
ks.test(data_transition$area_km2, "pnorm", mean(data_transition$area_km2), sd(data_transition$area_km2))
ggqqplot(data_transition$area_km2)
# Distribution of CONT variable
ggdensity(data_transition, x = "area_km2", fill = "lightgray", title = "Area transition") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_transition$area_km2)
skewness(data_transition$area_km2)

#LW_R
shapiro.test(data_LW_R$area_km2) #dados normais
ks.test(data_LW_R$area_km2, "pnorm", mean(data_LW_R$area_km2), sd(data_LW_R$area_km2))
ggqqplot(data_LW_R$area_km2)
# Distribution of CONT variable
ggdensity(data_LW_R, x = "area_km2", fill = "lightgray", title = "Area LW_R") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW_R$area_km2)
skewness(data_LW_R$area_km2)

#data_HW_F
shapiro.test(data_HW_F$area_km2) #dados normais
ks.test(data_HW_F$area_km2, "pnorm", mean(data_HW_F$area_km2), sd(data_HW_F$area_km2))
ggqqplot(data_HW_F$area_km2)
# Distribution of CONT variable
ggdensity(data_HW_F, x = "area_km2", fill = "lightgray", title = "Area HW_F") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW_F$area_km2)
skewness(data_HW_F$area_km2)

##SPM mean########
#LW
shapiro.test(data_LW$mean_SPM) #dados normais
ks.test(data_LW$mean_SPM, "pnorm", mean(data_LW$mean_SPM), sd(data_LW$mean_SPM))
ggqqplot(data_LW$mean_SPM)
# Distribution of CONT variable
ggdensity(data_LW, x = "mean_SPM", fill = "lightgray", title = "SPM LW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW$mean_SPM)
skewness(data_LW$mean_SPM)

#HW
shapiro.test(data_HW$mean_SPM) #dados normais
ks.test(data_HW$mean_SPM, "pnorm", mean(data_HW$mean_SPM), sd(data_HW$mean_SPM))
ggqqplot(data_HW$mean_SPM)
# Distribution of CONT variable
ggdensity(data_HW, x = "mean_SPM", fill = "lightgray", title = "SPM HW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW$mean_SPM)
skewness(data_HW$mean_SPM)

#R
shapiro.test(data_R$mean_SPM) #dados normais
ks.test(data_R$mean_SPM, "pnorm", mean(data_R$mean_SPM), sd(data_R$mean_SPM))
ggqqplot(data_R$mean_SPM)
# Distribution of CONT variable
ggdensity(data_R, x = "mean_SPM", fill = "lightgray", title = "SPM R") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_R$mean_SPM)
skewness(data_R$mean_SPM)

#F
shapiro.test(data_F$mean_SPM) #dados normais
ks.test(data_F$mean_SPM, "pnorm", mean(data_F$mean_SPM), sd(data_F$mean_SPM))
ggqqplot(data_F$mean_SPM)
# Distribution of CONT variable
ggdensity(data_F, x = "mean_SPM", fill = "lightgray", title = "SPM F") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_F$mean_SPM)
skewness(data_F$mean_SPM)

#transition
shapiro.test(data_transition$mean_SPM) #dados normais
ks.test(data_transition$mean_SPM, "pnorm", mean(data_transition$mean_SPM), sd(data_transition$mean_SPM))
ggqqplot(data_transition$mean_SPM)
# Distribution of CONT variable
ggdensity(data_transition, x = "mean_SPM", fill = "lightgray", title = "SPM transition") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_transition$mean_SPM)
skewness(data_transition$mean_SPM)

#LW_R
shapiro.test(data_LW_R$mean_SPM) #dados normais
ks.test(data_LW_R$mean_SPM, "pnorm", mean(data_LW_R$mean_SPM), sd(data_LW_R$mean_SPM))
ggqqplot(data_LW_R$mean_SPM)
# Distribution of CONT variable
ggdensity(data_LW_R, x = "mean_SPM", fill = "lightgray", title = "SPM LW_R") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW_R$mean_SPM)
skewness(data_LW_R$mean_SPM)

#HW_F
shapiro.test(data_HW_F$mean_SPM) #dados normais
ks.test(data_HW_F$mean_SPM, "pnorm", mean(data_HW_F$mean_SPM), sd(data_HW_F$mean_SPM))
ggqqplot(data_HW_F$mean_SPM)
# Distribution of CONT variable
ggdensity(data_HW_F, x = "mean_SPM", fill = "lightgray", title = "SPM HW_F") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW_F$mean_SPM)
skewness(data_HW_F$mean_SPM)

##SPM max########
#LW
shapiro.test(data_LW$max_SPM) #dados normais
ks.test(data_LW$max_SPM, "pnorm", mean(data_LW$max_SPM), sd(data_LW$max_SPM))
ggqqplot(data_LW$max_SPM)
# Distribution of CONT variable
ggdensity(data_LW, x = "max_SPM", fill = "lightgray", title = "SPM LW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW$max_SPM)
skewness(data_LW$max_SPM)

#HW
shapiro.test(data_HW$max_SPM) #dados normais
ks.test(data_HW$max_SPM, "pnorm", mean(data_HW$max_SPM), sd(data_HW$max_SPM))
ggqqplot(data_HW$max_SPM)
# Distribution of CONT variable
ggdensity(data_HW, x = "max_SPM", fill = "lightgray", title = "SPM HW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW$max_SPM)
skewness(data_HW$max_SPM)

#R
shapiro.test(data_R$max_SPM) #dados normais
ks.test(data_R$max_SPM, "pnorm", mean(data_R$max_SPM), sd(data_R$max_SPM))
ggqqplot(data_R$max_SPM)
# Distribution of CONT variable
ggdensity(data_R, x = "max_SPM", fill = "lightgray", title = "SPM R") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_R$max_SPM)
skewness(data_R$max_SPM)

#F
shapiro.test(data_F$max_SPM) #dados normais
ks.test(data_F$max_SPM, "pnorm", mean(data_F$max_SPM), sd(data_F$max_SPM))
ggqqplot(data_F$max_SPM)
# Distribution of CONT variable
ggdensity(data_F, x = "max_SPM", fill = "lightgray", title = "SPM F") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_F$max_SPM)
skewness(data_F$max_SPM)

#transition
shapiro.test(data_transition$max_SPM) #dados normais
ks.test(data_transition$max_SPM, "pnorm", mean(data_transition$max_SPM), sd(data_transition$max_SPM))
ggqqplot(data_transition$max_SPM)
# Distribution of CONT variable
ggdensity(data_transition, x = "max_SPM", fill = "lightgray", title = "SPM transition") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_transition$max_SPM)
skewness(data_transition$max_SPM)

#LW_R
shapiro.test(data_LW_R$max_SPM) #dados normais
ks.test(data_LW_R$max_SPM, "pnorm", mean(data_LW_R$max_SPM), sd(data_LW_R$max_SPM))
ggqqplot(data_LW_R$max_SPM)
# Distribution of CONT variable
ggdensity(data_LW_R, x = "max_SPM", fill = "lightgray", title = "SPM LW_R") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW_R$max_SPM)
skewness(data_LW_R$max_SPM)

#HW_F
shapiro.test(data_HW_F$max_SPM) #dados normais
ks.test(data_HW_F$max_SPM, "pnorm", mean(data_HW_F$max_SPM), sd(data_HW_F$max_SPM))
ggqqplot(data_HW_F$max_SPM)
# Distribution of CONT variable
ggdensity(data_HW_F, x = "max_SPM", fill = "lightgray", title = "SPM HW_F") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW_F$max_SPM)
skewness(data_HW_F$max_SPM)

#SPM min########
#LW
shapiro.test(data_LW$min_SPM) #dados normais
ks.test(data_LW$min_SPM, "pnorm", mean(data_LW$min_SPM), sd(data_LW$min_SPM))
ggqqplot(data_LW$min_SPM)
# Distribution of CONT variable
ggdensity(data_LW, x = "min_SPM", fill = "lightgray", title = "SPM LW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW$min_SPM)
skewness(data_LW$min_SPM)

#HW
shapiro.test(data_HW$min_SPM) #dados normais
ks.test(data_HW$min_SPM, "pnorm", mean(data_HW$min_SPM), sd(data_HW$min_SPM))
ggqqplot(data_HW$min_SPM)
# Distribution of CONT variable
ggdensity(data_HW, x = "min_SPM", fill = "lightgray", title = "SPM HW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW$min_SPM)
skewness(data_HW$min_SPM)

#R
shapiro.test(data_R$min_SPM) #dados normais
ks.test(data_R$min_SPM, "pnorm", mean(data_R$min_SPM), sd(data_R$min_SPM))
ggqqplot(data_R$min_SPM)
# Distribution of CONT variable
ggdensity(data_R, x = "min_SPM", fill = "lightgray", title = "SPM R") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_R$min_SPM)
skewness(data_R$min_SPM)

#F
shapiro.test(data_F$min_SPM) #dados normais
ks.test(data_F$min_SPM, "pnorm", mean(data_F$min_SPM), sd(data_F$min_SPM))
ggqqplot(data_F$min_SPM)
# Distribution of CONT variable
ggdensity(data_F, x = "min_SPM", fill = "lightgray", title = "SPM F") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_F$min_SPM)
skewness(data_F$min_SPM)

#transition
shapiro.test(data_transition$min_SPM) #dados normais
ks.test(data_transition$min_SPM, "pnorm", mean(data_transition$min_SPM), sd(data_transition$min_SPM))
ggqqplot(data_transition$min_SPM)
# Distribution of CONT variable
ggdensity(data_transition, x = "min_SPM", fill = "lightgray", title = "SPM transition") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_transition$min_SPM)
skewness(data_transition$min_SPM)

#LW_R
shapiro.test(data_LW_R$min_SPM) #dados normais
ks.test(data_LW_R$min_SPM, "pnorm", mean(data_LW_R$min_SPM), sd(data_LW_R$min_SPM))
ggqqplot(data_LW_R$min_SPM)
# Distribution of CONT variable
ggdensity(data_LW_R, x = "min_SPM", fill = "lightgray", title = "SPM LW_R") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW_R$min_SPM)
skewness(data_LW_R$min_SPM)

#HW_F
shapiro.test(data_HW_F$min_SPM) #dados normais
ks.test(data_HW_F$min_SPM, "pnorm", mean(data_HW_F$min_SPM), sd(data_HW_F$min_SPM))
ggqqplot(data_HW_F$min_SPM)
# Distribution of CONT variable
ggdensity(data_HW_F, x = "min_SPM", fill = "lightgray", title = "SPM HW_F") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW_F$min_SPM)
skewness(data_HW_F$min_SPM)

##Precipitation ####
#LW
shapiro.test(data_LW$mean_precipitation) #dados normais
ks.test(data_LW$mean_precipitation, "pnorm", mean(data_LW$data_LW), sd(data_LW$mean_precipitation))
ggqqplot(data_LW$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_LW, x = "mean_precipitation", fill = "lightgray", title = "Precipitation LW") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW$mean_precipitation)
skewness(data_LW$mean_precipitation)

#HW
shapiro.test(data_HW$mean_precipitation) #dados normais
ks.test(data_HW$mean_precipitation, "pnorm", mean(data_HW$mean_precipitation), sd(data_HW$mean_precipitation))
ggqqplot(data_HW$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_HW, x = "mean_precipitation", fill = "lightgray", title = "Precipitation") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW$mean_precipitation)
skewness(data_HW$mean_precipitation)

#R
shapiro.test(data_R$mean_precipitation) #dados normais
ks.test(data_R$mean_precipitation, "pnorm", mean(data_R$mean_precipitation), sd(data_R$mean_precipitation))
ggqqplot(data_R$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_R, x = "mean_precipitation", fill = "lightgray", title = "Precipitation") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_R$mean_precipitation)
skewness(data_R$mean_precipitation)

#F
shapiro.test(data_F$mean_precipitation) #dados normais
ks.test(data_F$mean_precipitation, "pnorm", mean(data_F$mean_precipitation), sd(data_F$mean_precipitation))
ggqqplot(data_F$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_F, x = "mean_precipitation", fill = "lightgray", title = "Precipitation") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_F$mean_precipitation)
skewness(data_F$mean_precipitation)

#transition
shapiro.test(data_transition$mean_precipitation) #dados normais
ks.test(data_transition$mean_precipitation, "pnorm", mean(data_transition$mean_precipitation), sd(data_transition$mean_precipitation))
ggqqplot(data_transition$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_transition, x = "mean_precipitation", fill = "lightgray", title = "Precipitation") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_transition$mean_precipitation)
skewness(data_transition$mean_precipitation)

#LW_R
shapiro.test(data_LW_R$mean_precipitation) #dados normais
ks.test(data_LW_R$mean_precipitation, "pnorm", mean(data_LW_R$mean_precipitation), sd(data_LW_R$mean_precipitation))
ggqqplot(data_LW_R$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_LW_R, x = "mean_precipitation", fill = "lightgray", title = "Precipitation") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW_R$mean_precipitation)
skewness(data_LW_R$mean_precipitation)

#HW_F
shapiro.test(data_HW_F$mean_precipitation) #dados normais
ks.test(data_HW_F$mean_precipitation, "pnorm", mean(data_HW_F$mean_precipitation), sd(data_HW_F$mean_precipitation))
ggqqplot(data_HW_F$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_HW_F, x = "mean_precipitation", fill = "lightgray", title = "Precipitation") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW_F$mean_precipitation)
skewness(data_HW_F$mean_precipitation)


##discharge#############################
#LW
shapiro.test(data_LW$mean_discharge) #dados normais
ks.test(data_LW$mean_discharge, "pnorm", mean(data_LW$mean_discharge), sd(data_LW$mean_discharge))
ggqqplot(data_LW$mean_discharge)
# Distribution of CONT variable
ggdensity(data_LW, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW$mean_discharge)
skewness(data_LW$mean_discharge)

#HW
shapiro.test(data_HW$mean_discharge) #dados normais
ks.test(data_HW$mean_discharge, "pnorm", mean(data_HW$mean_discharge), sd(data_HW$mean_discharge))
ggqqplot(data_HW$mean_discharge)
# Distribution of CONT variable
ggdensity(data_HW, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW$mean_discharge)
skewness(data_HW$mean_discharge)

#R
shapiro.test(data_R$mean_discharge) #dados normais
ks.test(data_R$mean_discharge, "pnorm", mean(data_R$mean_discharge), sd(data_R$mean_discharge))
ggqqplot(data_R$mean_discharge)
# Distribution of CONT variable
ggdensity(data_R, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_R$mean_discharge)
skewness(data_R$mean_discharge)

#F
shapiro.test(data_F$mean_discharge) #dados normais
ks.test(data_F$mean_discharge, "pnorm", mean(data_F$mean_discharge), sd(data_F$mean_discharge))
ggqqplot(data_F$mean_discharge)
# Distribution of CONT variable
ggdensity(data_F, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_F$mean_discharge)
skewness(data_F$mean_discharge)

#transition
shapiro.test(data_transition$mean_discharge) #dados normais
ks.test(data_transition$mean_discharge, "pnorm", mean(data_transition$mean_discharge), sd(data_transition$mean_discharge))
ggqqplot(data_transition$mean_discharge)
# Distribution of CONT variable
ggdensity(data_transition, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_transition$mean_discharge)
skewness(data_transition$mean_discharge)

#LW_R
shapiro.test(data_LW_R$mean_discharge) #dados normais
ks.test(data_LW_R$mean_discharge, "pnorm", mean(data_LW_R$mean_discharge), sd(data_LW_R$mean_discharge))
ggqqplot(data_LW_R$mean_discharge)
# Distribution of CONT variable
ggdensity(data_LW_R, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW_R$mean_discharge)
skewness(data_LW_R$mean_discharge)

#HW_F
shapiro.test(data_HW_F$mean_discharge) #dados normais
ks.test(data_HW_F$mean_discharge, "pnorm", mean(data_HW_F$mean_discharge), sd(data_HW_F$mean_discharge))
ggqqplot(data_HW_F$mean_discharge)
# Distribution of CONT variable
ggdensity(data_HW_F, x = "mean_discharge", fill = "lightgray", title = "Discharge") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW_F$mean_discharge)
skewness(data_HW_F$mean_discharge)

##wind U###############
#LW
shapiro.test(data_LW$mean_u__wind) #dados normais
ks.test(data_LW$mean_u__wind, "pnorm", mean(data_LW$mean_u__wind), sd(data_LW$mean_u__wind))
ggqqplot(data_LW$mean_u__wind)
# Distribution of CONT variable
ggdensity(data_LW, x = "mean_u__wind", fill = "lightgray", title = "Wind U") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW$mean_u__wind)
skewness(data_LW$mean_u__wind)

#HW
shapiro.test(data_HW$mean_u__wind) #dados normais
ks.test(data_HW$mean_u__wind, "pnorm", mean(data_HW$mean_u__wind), sd(data_HW$mean_u__wind))
ggqqplot(data_HW$mean_u__wind)
# Distribution of CONT variable
ggdensity(data_HW, x = "mean_u__wind", fill = "lightgray", title = "Wind U") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW$mean_u__wind)
skewness(data_HW$mean_u__wind)

#R
shapiro.test(data_R$mean_u__wind) #dados normais
ks.test(data_R$mean_u__wind, "pnorm", mean(data_R$mean_u__wind), sd(data_R$mean_u__wind))
ggqqplot(data_R$mean_u__wind)
# Distribution of CONT variable
ggdensity(data_R, x = "mean_u__wind", fill = "lightgray", title = "Wind U") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_R$mean_u__wind)
skewness(data_R$mean_u__wind)

#F
shapiro.test(data_F$mean_u__wind) #dados normais
ks.test(data_F$mean_u__wind, "pnorm", mean(data_F$mean_u__wind), sd(data_F$mean_u__wind))
ggqqplot(data_F$mean_u__wind)
# Distribution of CONT variable
ggdensity(data_F, x = "mean_u__wind", fill = "lightgray", title = "Wind U") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_F$mean_u__wind)
skewness(data_F$mean_u__wind)

#transition
shapiro.test(data_transition$mean_u__wind) #dados normais
ks.test(data_transition$mean_u__wind, "pnorm", mean(data_transition$mean_u__wind), sd(data_transition$mean_u__wind))
ggqqplot(data_transition$mean_u__wind)
# Distribution of CONT variable
ggdensity(data_transition, x = "mean_u__wind", fill = "lightgray", title = "Wind U") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_transition$mean_u__wind)
skewness(data_transition$mean_u__wind)

#LW_R
shapiro.test(data_LW_R$mean_u__wind) #dados normais
ks.test(data_LW_R$mean_u__wind, "pnorm", mean(data_LW_R$mean_u__wind), sd(data_LW_R$mean_u__wind))
ggqqplot(data_LW_R$mean_u__wind)
# Distribution of CONT variable
ggdensity(data_LW_R, x = "mean_u__wind", fill = "lightgray", title = "Wind U") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW_R$mean_u__wind)
skewness(data_LW_R$mean_u__wind)

#HW_F
shapiro.test(data_HW_F$mean_u__wind) #dados normais
ks.test(data_HW_F$mean_u__wind, "pnorm", mean(data_HW_F$mean_u__wind), sd(data_HW_F$mean_u__wind))
ggqqplot(data_HW_F$mean_u__wind)
# Distribution of CONT variable
ggdensity(data_HW_F, x = "mean_u__wind", fill = "lightgray", title = "Wind U") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW_F$mean_u__wind)
skewness(data_HW_F$mean_u__wind)

##wind V####################
#LW
shapiro.test(data_LW$mean_v__wind) #dados normais
ks.test(data_LW$mean_v__wind, "pnorm", mean(data_LW$mean_v__wind), sd(data_LW$mean_v__wind))
ggqqplot(data_LW$mean_v__wind)
# Distribution of CONT variable
ggdensity(data_LW, x = "mean_v__wind", fill = "lightgray", title = "Wind V") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW$mean_v__wind)
skewness(data_LW$mean_v__wind)

#HW
shapiro.test(data_HW$mean_v__wind) #dados normais
ks.test(data_HW$mean_v__wind, "pnorm", mean(data_HW$mean_v__wind), sd(data_HW$mean_v__wind))
ggqqplot(data_HW$mean_v__wind)
# Distribution of CONT variable
ggdensity(data_HW, x = "mean_v__wind", fill = "lightgray", title = "Wind V") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW$mean_v__wind)
skewness(data_HW$mean_v__wind)

#R
shapiro.test(data_R$mean_v__wind) #dados normais
ks.test(data_R$mean_v__wind, "pnorm", mean(data_R$mean_v__wind), sd(data_R$mean_v__wind))
ggqqplot(data_R$mean_v__wind)
# Distribution of CONT variable
ggdensity(data_R, x = "mean_v__wind", fill = "lightgray", title = "Wind V") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_R$mean_v__wind)
skewness(data_R$mean_v__wind)

#F
shapiro.test(data_F$mean_v__wind) #dados normais
ks.test(data_F$mean_v__wind, "pnorm", mean(data_F$mean_v__wind), sd(data_F$mean_v__wind))
ggqqplot(data_F$mean_v__wind)
# Distribution of CONT variable
ggdensity(data_F, x = "mean_v__wind", fill = "lightgray", title = "Wind V") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_F$mean_v__wind)
skewness(data_F$mean_v__wind)

#transition
shapiro.test(data_transition$mean_v__wind) #dados normais
ks.test(data_transition$mean_v__wind, "pnorm", mean(data_transition$mean_v__wind), sd(data_transition$mean_v__wind))
ggqqplot(data_transition$mean_v__wind)
# Distribution of CONT variable
ggdensity(data_transition, x = "mean_v__wind", fill = "lightgray", title = "Wind V") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_transition$mean_v__wind)
skewness(data_transition$mean_v__wind)

#LW_R
shapiro.test(data_LW_R$mean_v__wind) #dados normais
ks.test(data_LW_R$mean_v__wind, "pnorm", mean(data_LW_R$mean_v__wind), sd(data_LW_R$mean_v__wind))
ggqqplot(data_LW_R$mean_v__wind)
# Distribution of CONT variable
ggdensity(data_LW_R, x = "mean_v__wind", fill = "lightgray", title = "Wind V") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW_R$mean_v__wind)
skewness(data_LW_R$mean_v__wind)

#HW_F
shapiro.test(data_HW_F$mean_v__wind) #dados normais
ks.test(data_HW_F$mean_v__wind, "pnorm", mean(data_HW_F$mean_v__wind), sd(data_HW_F$mean_v__wind))
ggqqplot(data_HW_F$mean_v__wind)
# Distribution of CONT variable
ggdensity(data_HW_F, x = "mean_v__wind", fill = "lightgray", title = "Wind V") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW_F$mean_v__wind)
skewness(data_HW_F$mean_v__wind)

##anthropic km2 ####################
#LW
shapiro.test(data_LW$anthropic_km2) #dados normais
ks.test(data_LW$anthropic_km2, "pnorm", mean(data_LW$anthropic_km2), sd(data_LW$anthropic_km2))
ggqqplot(data_LW$anthropic_km2)
# Distribution of CONT variable
ggdensity(data_LW, x = "anthropic_km2", fill = "lightgray", title = "anthropic area") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW$anthropic_km2)
skewness(data_LW$anthropic_km2)

#HW
shapiro.test(data_HW$anthropic_km2) #dados normais
ks.test(data_HW$anthropic_km2, "pnorm", mean(data_HW$anthropic_km2), sd(data_HW$anthropic_km2))
ggqqplot(data_HW$anthropic_km2)
# Distribution of CONT variable
ggdensity(data_HW, x = "anthropic_km2", fill = "lightgray", title = "anthropic area") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW$anthropic_km2)
skewness(data_HW$anthropic_km2)

#R
shapiro.test(data_R$anthropic_km2) #dados normais
ks.test(data_R$anthropic_km2, "pnorm", mean(data_R$anthropic_km2), sd(data_R$anthropic_km2))
ggqqplot(data_R$anthropic_km2)
# Distribution of CONT variable
ggdensity(data_R, x = "anthropic_km2", fill = "lightgray", title = "anthropic area") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_R$anthropic_km2)
skewness(data_R$anthropic_km2)

#F
shapiro.test(data_F$anthropic_km2) #dados normais
ks.test(data_F$anthropic_km2, "pnorm", mean(data_F$anthropic_km2), sd(data_F$anthropic_km2))
ggqqplot(data_F$anthropic_km2)
# Distribution of CONT variable
ggdensity(data_F, x = "anthropic_km2", fill = "lightgray", title = "anthropic area") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_F$anthropic_km2)
skewness(data_F$anthropic_km2)

#transition
shapiro.test(data_transition$anthropic_km2) #dados normais
ks.test(data_transition$anthropic_km2, "pnorm", mean(data_transition$anthropic_km2), sd(data_transition$anthropic_km2))
ggqqplot(data_transition$anthropic_km2)
# Distribution of CONT variable
ggdensity(data_transition, x = "anthropic_km2", fill = "lightgray", title = "anthropic area") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_transition$anthropic_km2)
skewness(data_transition$anthropic_km2)

#LW_R
shapiro.test(data_LW_R$anthropic_km2) #dados normais
ks.test(data_LW_R$anthropic_km2, "pnorm", mean(data_LW_R$anthropic_km2), sd(data_LW_R$anthropic_km2))
ggqqplot(data_LW_R$anthropic_km2)
# Distribution of CONT variable
ggdensity(data_LW_R, x = "anthropic_km2", fill = "lightgray", title = "anthropic area") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_LW_R$anthropic_km2)
skewness(data_LW_R$anthropic_km2)

#HW_F
shapiro.test(data_HW_F$anthropic_km2) #dados normais
ks.test(data_HW_F$anthropic_km2, "pnorm", mean(data_HW_F$anthropic_km2), sd(data_HW_F$anthropic_km2))
ggqqplot(data_HW_F$anthropic_km2)
# Distribution of CONT variable
ggdensity(data_HW_F, x = "anthropic_km2", fill = "lightgray", title = "anthropic area") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_HW_F$anthropic_km2)
skewness(data_HW_F$anthropic_km2)

#calcular correlacao##############################################################
#pacotes
library(corrplot)
library(tidyverse)
library(lmtest)
library(psych)

#testar se a covari?ncia ? linear - verificar scatter plot
##mean SPM#############
#LW
ggscatter(data_LW, x = 'mean_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_LW, x = 'mean_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_LW, x = 'mean_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_LW, x = 'mean_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_LW, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_LW, x = 'mean_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'anthropic_km2') #cov linear

#HW
ggscatter(data_HW, x = 'mean_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_HW, x = 'mean_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_HW, x = 'mean_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_HW, x = 'mean_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_HW, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_HW, x = 'mean_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'anthropic_km2') #cov linear

#R
ggscatter(data_R, x = 'mean_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_R, x = 'mean_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_R, x = 'mean_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_R, x = 'mean_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_R, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_R, x = 'mean_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'anthropic_km2') #cov linear

#F
ggscatter(data_F, x = 'mean_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_F, x = 'mean_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_F, x = 'mean_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_F, x = 'mean_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_F, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_F, x = 'mean_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'anthropic_km2') #cov linear

#transition
ggscatter(data_transition, x = 'mean_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_transition, x = 'mean_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_transition, x = 'mean_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_transition, x = 'mean_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_transition, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_transition, x = 'mean_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'anthropic_km2') #cov linear

#LW_R
ggscatter(data_LW_R, x = 'mean_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_LW_R, x = 'mean_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_LW_R, x = 'mean_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_LW_R, x = 'mean_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_LW_R, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_LW_R, x = 'mean_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'mean_SPM', 
          ylab = 'anthropic_km2') #cov linear

#HW_F
ggscatter(data_HW_F, x = 'mean_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_HW_F, x = 'mean_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_HW_F, x = 'mean_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_HW_F, x = 'mean_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_HW_F, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_HW_F, x = 'mean_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'anthropic_km2') #cov linear

##min SPM#######
#LW
ggscatter(data_LW, x = 'min_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_LW, x = 'min_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_LW, x = 'min_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_LW, x = 'min_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_LW, x = 'min_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_LW, x = 'min_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'anthropic_km2') #cov linear

#HW
ggscatter(data_HW, x = 'min_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_HW, x = 'min_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_HW, x = 'min_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_HW, x = 'min_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_HW, x = 'min_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_HW, x = 'min_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'anthropic_km2') #cov linear

#R
ggscatter(data_R, x = 'min_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_R, x = 'min_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_R, x = 'min_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_R, x = 'min_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_R, x = 'min_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_R, x = 'min_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'anthropic_km2') #cov linear

#F
ggscatter(data_F, x = 'min_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_F, x = 'min_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_F, x = 'min_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_F, x = 'min_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_F, x = 'min_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_F, x = 'min_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'anthropic_km2') #cov linear

#transition
ggscatter(data_transition, x = 'min_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_transition, x = 'min_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_transition, x = 'min_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_transition, x = 'min_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_transition, x = 'min_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_transition, x = 'min_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'anthropic_km2') #cov linear

#LW_R
ggscatter(data_LW_R, x = 'min_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_LW_R, x = 'min_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_LW_R, x = 'min_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_LW_R, x = 'min_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_LW_R, x = 'min_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_LW_R, x = 'min_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'anthropic_km2') #cov linear

#HW_F
ggscatter(data_HW_F, x = 'min_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_HW_F, x = 'min_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_HW_F, x = 'min_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_HW_F, x = 'min_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_HW_F, x = 'min_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_HW_F, x = 'min_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'anthropic_km2') #cov linear

##max SPM#############
#LW
ggscatter(data_LW, x = 'max_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_LW, x = 'max_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_LW, x = 'max_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_LW, x = 'max_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_LW, x = 'max_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_LW, x = 'max_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'anthropic_km2') #cov linear

#HW
ggscatter(data_HW, x = 'max_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_HW, x = 'max_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_HW, x = 'max_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_HW, x = 'max_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_HW, x = 'max_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_HW, x = 'max_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'anthropic_km2') #cov linear

#R
ggscatter(data_R, x = 'max_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_R, x = 'max_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_R, x = 'max_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_R, x = 'max_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_R, x = 'max_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_R, x = 'max_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'anthropic_km2') #cov linear

#F
ggscatter(data_F, x = 'max_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'max_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_F, x = 'max_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_F, x = 'max_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_F, x = 'max_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_F, x = 'max_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'max_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_F, x = 'max_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'max_SPM', 
          ylab = 'anthropic_km2') #cov linear

#transition
ggscatter(data_transition, x = 'max_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_transition, x = 'max_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_transition, x = 'max_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_transition, x = 'max_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_transition, x = 'max_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_transition, x = 'max_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'anthropic_km2') #cov linear

#LW_R
ggscatter(data_LW_R, x = 'max_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_LW_R, x = 'max_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_LW_R, x = 'max_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_LW_R, x = 'max_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_LW_R, x = 'max_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'max_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_LW_R, x = 'max_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'pearson', xlab = 'max_SPM', 
          ylab = 'anthropic_km2') #cov linear

#HW_F
ggscatter(data_HW_F, x = 'max_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_HW_F, x = 'min_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_HW_F, x = 'min_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_HW_F, x = 'min_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_HW_F, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_HW_F, x = 'mean_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'anthropic_km2') #cov linear

#matriz de corelacao############################################################
#LW
data_LW<-select(data_LW, mean_SPM  , area_km2  ,  mean_precipitation,
                    mean_discharge,anthropic_km2,mean_u__wind,mean_v__wind,max_SPM,min_SPM)
correlation_LW_sp <- cor(data_LW, method = 'spearman')
corrplot::corrplot.mixed(correlation_LW_sp, upper = 'ellipse', lower = 'number', las = 1)
correlation_LW_pea <- cor(data_LW, method = 'pearson')
corrplot::corrplot.mixed(correlation_LW_pea, upper = 'ellipse', lower = 'number', las = 1)

#HW
data_HW<-select(data_HW, mean_SPM  , area_km2  ,  mean_precipitation,
                mean_discharge,anthropic_km2,mean_u__wind,mean_v__wind,max_SPM,min_SPM)
correlation_HW_sp <- cor(data_HW, method = 'spearman')
corrplot::corrplot.mixed(correlation_HW_sp, upper = 'ellipse', lower = 'number', las = 1)
correlation_HW_pea <- cor(data_HW, method = 'pearson')
corrplot::corrplot.mixed(correlation_HW_pea, upper = 'ellipse', lower = 'number', las = 1)

#R
data_R<-select(data_R, mean_SPM  , area_km2  ,  mean_precipitation,
                mean_discharge,anthropic_km2,mean_u__wind,mean_v__wind,max_SPM,min_SPM)
correlation_R_sp <- cor(data_R, method = 'spearman')
corrplot::corrplot.mixed(correlation_R_sp, upper = 'ellipse', lower = 'number', las = 1)
correlation_R_pea <- cor(data_R, method = 'pearson')
corrplot::corrplot.mixed(correlation_R_pea, upper = 'ellipse', lower = 'number', las = 1)

#F
data_F<-select(data_F, mean_SPM  , area_km2  ,  mean_precipitation,
                mean_discharge,anthropic_km2,mean_u__wind,mean_v__wind,max_SPM,min_SPM)
correlation_F_sp <- cor(data_F, method = 'spearman')
corrplot::corrplot.mixed(correlation_F_sp, upper = 'ellipse', lower = 'number', las = 1)
correlation_F_pea <- cor(data_F, method = 'pearson')
corrplot::corrplot.mixed(correlation_F_pea, upper = 'ellipse', lower = 'number', las = 1)

#transition
data_transition<-select(data_transition, mean_SPM  , area_km2  ,  mean_precipitation,
                mean_discharge,anthropic_km2,mean_u__wind,mean_v__wind,max_SPM,min_SPM)
correlation_transition_sp <- cor(data_transition, method = 'spearman')
corrplot::corrplot.mixed(correlation_transition_sp, upper = 'ellipse', lower = 'number', las = 1)
correlation_transition_pea <- cor(data_transition, method = 'pearson')
corrplot::corrplot.mixed(correlation_transition_pea, upper = 'ellipse', lower = 'number', las = 1)

#LW_R
data_LW_R<-select(data_LW_R, mean_SPM  , area_km2  ,  mean_precipitation,
                mean_discharge,anthropic_km2,mean_u__wind,mean_v__wind,max_SPM,min_SPM)
correlation_LW_R_sp <- cor(data_LW_R, method = 'spearman')
corrplot::corrplot.mixed(correlation_LW_R_sp, upper = 'ellipse', lower = 'number', las = 1)
correlation_LW_R_pea <- cor(data_LW_R, method = 'pearson')
corrplot::corrplot.mixed(correlation_LW_R_pea, upper = 'ellipse', lower = 'number', las = 1)

#HW_F
data_HW_F<-select(data_HW_F, mean_SPM  , area_km2  ,  mean_precipitation,
                mean_discharge,anthropic_km2,mean_u__wind,mean_v__wind,max_SPM,min_SPM)
correlation_HW_F_sp <- cor(data_HW_F, method = 'spearman')
corrplot::corrplot.mixed(correlation_HW_F_sp, upper = 'ellipse', lower = 'number', las = 1)
correlation_HW_F_pea <- cor(data_HW_F, method = 'pearson')
corrplot::corrplot.mixed(correlation_HW_F_pea, upper = 'ellipse', lower = 'number', las = 1)

write.csv(correlation_LW_sp, file = "correlationLW_sp.csv")
write.csv(correlation_LW_pea, file = "correlationLW_pea.csv")
write.csv(correlation_HW_sp, file = "correlationHW_sp.csv")
write.csv(correlation_HW_pea, file = "correlationHW_pea.csv")
write.csv(correlation_R_sp, file = "correlationR_sp.csv")
write.csv(correlation_R_pea, file = "correlationR_pea.csv")
write.csv(correlation_F_sp, file = "correlationF_sp.csv")
write.csv(correlation_F_pea, file = "correlationF_pea.csv")
write.csv(correlation_transition_sp, file = "correlationtransition_sp.csv")
write.csv(correlation_transition_pea, file = "correlationtransition_pea.csv")
write.csv(correlation_LW_R_sp, file = "correlationR_sp.csv")
write.csv(correlation_LW_R_pea, file = "correlationR_pea.csv")
write.csv(correlation_HW_F_sp, file = "correlationHW_F_sp.csv")
write.csv(correlation_HW_F_pea, file = "correlationHW_F_pea.csv")

#verificar se as correlacoes sao significativas##################################
### fazer para o dataset escolhiodo
cor.test(data_select$mean_SPM, data_select$area_km2, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_precipitation, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_discharge, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_u__wind, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_v__wind, method = 'spearman')


