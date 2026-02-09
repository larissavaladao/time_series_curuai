##################################################################################
### DADOS MENSAIS

library(readr)
data <- as.data.frame(read.csv("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df/df_merged.csv"))
head(data)
summary(data)

#imputar valores que est?o faltando n?o vai ser usado por hora###################
#500 iterations of predictive mean mapping for imputing 
#5 datasets
library(mice)
library(dplyr)


#imputar dados faltantes faltantes
selecao <- select(data, area_km2  , mean_SPM  , mean_precipitation,
                  discharge_mean ,mean_u__wind,mean_v__wind)
dados_imp <- mice(selecao, m = 5, maxit = 100, method = 'pmm', seed = 500)
dados_comp <- complete(dados_imp, 4)#numero do dataset cujas imputa??es vc quer usar
dados_comp


selecao <- select(data, -discharge_mean)

selecao$discharge_mean <- dados_comp$discharge_mean

write.csv(selecao, file = "filled_data.csv")

####################################################################################
#ler os arquivos j? substitu?dos para calcular as correla??es e normalidade
#reservatorios
data_select <- as.data.frame(read.csv("filled_data.csv"))
head(data_select)
summary(data_select)

#testes preliminares#############################################################
library(ggpubr)
library(moments)


# tanto Shapiro Wilk quanto Korogonov tem resultados parecidos - para testar normalidade
#testar normalidade 
#Area
shapiro.test(data_select$area_km2) #dados normais
ks.test(data_select$area_km2, "pnorm", mean(data_select$area_km2), sd(data_select$area_km2))
ggqqplot(data_select$area_km2)
# Distribution of CONT variable
ggdensity(data_select, x = "area_km2", fill = "lightgray", title = "Area") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$area_km2)
skewness(data_select$area_km2)

#SPM
shapiro.test(data_select$mean_SPM) #dados normais
ks.test(data_select$mean_SPM, "pnorm", mean(data_select$mean_SPM), sd(data_select$mean_SPM))
ggqqplot(data_select$mean_SPM)
# Distribution of CONT variable
ggdensity(data_select, x = "mean_SPM", fill = "lightgray", title = "SPM") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$mean_SPM)
skewness(data_select$mean_SPM)

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
shapiro.test(data_select$discharge_mean) #dados normais
ks.test(data_select$discharge_mean, "pnorm", mean(data_select$discharge_mean), sd(data_select$discharge_mean))
ggqqplot(data_select$discharge_mean)
# Distribution of CONT variable
ggdensity(data_select, x = "discharge_mean", fill = "lightgray", title = "Discharge") +
    stat_overlay_normal_density(color = "red", linetype = "dashed")
boxplot(data_select$discharge_mean)
skewness(data_select$discharge_mean)

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

#calcular correla??o##############################################################
#pacotes
library(corrplot)
library(tidyverse)
library(lmtest)
library(psych)

#testar se a covari?ncia ? linear - verificar scatter plot
#Serra da Mesa
ggscatter(data_select, x = 'mean_SPM', y = 'area_km2', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM',
          ylab = 'area_km2') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_precipitation', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_precipitation') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'discharge_mean', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'discharge_mean') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_u__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_u__wind') #cov linear

ggscatter(data_select, x = 'mean_SPM', y = 'mean_v__wind', add = 'reg.line', conf.int = TRUE,
          cor.coef = TRUE, cor.method = 'spearman', xlab = 'mean_SPM', 
          ylab = 'mean_v__wind') #cov linear

#matriz de corela??o############################################################

data_select<-select(data_select, area_km2  , mean_SPM  , mean_precipitation,
                    discharge_mean ,mean_u__wind,mean_v__wind)
correlation <- cor(data_select, method = 'spearman')

corrplot::corrplot.mixed(correlation, upper = 'ellipse', lower = 'number', las = 1)


write.csv(correlation, file = "correlation.csv")

#verificar se as correla??es s?o significativas##################################

cor.test(data_select$mean_SPM, data_select$area_km2, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_precipitation, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$discharge_mean, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_u__wind, method = 'spearman')
cor.test(data_select$mean_SPM, data_select$mean_v__wind, method = 'spearman')

