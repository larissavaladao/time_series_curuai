# dados dos mosaicos com periodos de água definidos por cotas em Obidos ####

#definir diretório de trabalho e importar dados ##################

#definir diret?rio de trabalho e importar dados ####
setwd("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df")
# setwd("C:/Users/LarissaVieiraValadão/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df")

library(readr)
library(dplyr)
data <- as.data.frame(read.csv("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df/df_merged_cota_lulcN2.csv"))
# data <- as.data.frame(read.csv("C:/Users/LarissaVieiraValadão/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df/df_merged_cota_lulcN2.csv"))
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
dados_comp <- complete(dados_imp, 3)#numero do dataset cujas imputa??es vc quer usar
dados_comp


selecao <- select(data, -X,-mean_u__wind,-mean_v__wind,-anthropic_km2)

selecao$mean_u__wind <- dados_comp$mean_u__wind
selecao$mean_v__wind <- dados_comp$mean_v__wind
selecao$anthropic_km2 <- dados_comp$anthropic_km2

write.csv(selecao, file = "filled_data_cota_lulcN2.csv")


#ler os arquivos ja substituidos para calcular as correlacoes e normalidade ###########################
#todos os periodos de agua incluidos
data_select <- as.data.frame(read.csv("filled_data_cota_lulcN2.csv"))
data_select <- select(data_select,-X,-class_name)
head(data_select)
summary(data_select)

#testes preliminares #####################################
library(ggpubr)
library(moments)


#tanto Shapiro Wilk quanto Korogonov tem resultados parecidos - para testar normalidade
##testar normalidade#####

#Precipitation
shapiro.test(data_select$mean_precipitation) #dados normais
ks.test(data_select$mean_precipitation, "pnorm", mean(data_select$mean_precipitation), sd(data_select$mean_precipitation))
ggqqplot(data_select$mean_precipitation)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_precipitation", fill = "lightgray", title = "Precipitation") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_precipitation)
skewness(data_select$mean_precipitation)


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

ggscatter(data_select, x = 'mean_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_precipitation') #cov linear

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

ggscatter(data_select, x = 'max_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'max_SPM', 
          ylab = 'mean_precipitation') #cov linear


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

ggscatter(data_select, x = 'min_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'min_SPM', 
          ylab = 'mean_precipitation') #cov linear

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


write.csv(correlation, file = "correlation_cota_lulcN2.csv")

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
data_select <- as.data.frame(read.csv("filled_data_cota_lulcN2.csv"))
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
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_LW, x = 'mean_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_LW, x = 'mean_SPM', y = 'mean_discharge', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_discharge') #cov linear

ggscatter(data_LW, x = 'mean_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_LW, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_LW, x = 'mean_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
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
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_HW, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear

ggscatter(data_HW, x = 'mean_SPM', y = 'anthropic_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'anthropic_km2') #cov linear

#R
ggscatter(data_R, x = 'mean_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM',
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
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM',
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


