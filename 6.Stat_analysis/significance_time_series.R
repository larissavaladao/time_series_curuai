#avaliar significancia das tendencias se necessário

#libraries######################
library(funtimes)
library(dplyr)
library(Kendall)
library(tseries)
#definir diret?rio de trabalho ##############################
setwd("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df")
setwd("C:/Users/LarissaVieiraValadão/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df")

#importar dados ##############################
dados <- as.data.frame(read.csv("filled_data_cota_lulc.csv"))
dados$water_period <- as.factor(dados$water_period)
dados$time_start <- as.Date(dados$time_start)
dados$time_finish <- as.Date(dados$time_finish)

# Create a new column based on condition#############
dados <- dados %>% mutate(agg_water_period = ifelse(water_period == 'LW'|water_period == 'R', "LW_R", "HW_F"))
dados$agg_water_period <- as.factor(dados$agg_water_period)


str(dados)
summary(dados)


#Dados Completos ###################
#selecionar colunas 
selecao<- select(dados,area_km2,mean_SPM, mean_precipitation,mean_discharge,anthropic_km2,
                 mean_u__wind,mean_v__wind)
#teste da série
apply(selecao, 2, function(x) notrend_test(x)$p.value)
apply(selecao, 2, function(x) notrend_test(x, test = "MK")$p.value)
apply(selecao, 2, function(x) notrend_test(x,test = "WAVK", 
                                           factor.length = "adaptive.selection")$p.value)

#Dados por periodo de agua ###################
# LW ####
#selecionar colunas 
selecao<- select(dados,area_km2,mean_SPM, mean_precipitation,mean_discharge,anthropic_km2,
                 mean_u__wind,mean_v__wind,water_period)

selecao <- filter(selecao,water_period=='LW') 

selecao<-select(selecao,-water_period)
head(selecao)
summary(selecao)

#teste da série
apply(selecao, 2, function(x) notrend_test(x)$p.value)
apply(selecao, 2, function(x) notrend_test(x, test = "MK")$p.value)
apply(selecao, 2, function(x) notrend_test(x,test = "WAVK", 
                                           factor.length = "adaptive.selection")$p.value)
# HW ####
#selecionar colunas 
selecao<- select(dados,area_km2,mean_SPM, mean_precipitation,mean_discharge,anthropic_km2,
                 mean_u__wind,mean_v__wind,water_period)

selecao <- filter(selecao,water_period=='HW') 

selecao<-select(selecao,-water_period)
head(selecao)
summary(selecao)

#teste da série
apply(selecao, 2, function(x) notrend_test(x)$p.value)
apply(selecao, 2, function(x) notrend_test(x, test = "MK")$p.value)
apply(selecao, 2, function(x) notrend_test(x,test = "WAVK", 
                                           factor.length = "adaptive.selection")$p.value)

# R ####
#selecionar colunas 
selecao<- select(dados,area_km2,mean_SPM, mean_precipitation,mean_discharge,anthropic_km2,
                 mean_u__wind,mean_v__wind,water_period)

selecao <- filter(selecao,water_period=='R') 

selecao<-select(selecao,-water_period)
head(selecao)
summary(selecao)

#teste da série
apply(selecao, 2, function(x) notrend_test(x)$p.value)
apply(selecao, 2, function(x) notrend_test(x, test = "MK")$p.value)
apply(selecao, 2, function(x) notrend_test(x,test = "WAVK", 
                                           factor.length = "adaptive.selection")$p.value)

# F ####
#selecionar colunas 
selecao<- select(dados,area_km2,mean_SPM, mean_precipitation,mean_discharge,anthropic_km2,
                 mean_u__wind,mean_v__wind,water_period)

selecao <- filter(selecao,water_period=='F') 

selecao<-select(selecao,-water_period)
head(selecao)
summary(selecao)

#teste da série
apply(selecao, 2, function(x) notrend_test(x)$p.value)
apply(selecao, 2, function(x) notrend_test(x, test = "MK")$p.value)
apply(selecao, 2, function(x) notrend_test(x,test = "WAVK", 
                                           factor.length = "adaptive.selection")$p.value)

# LW_R ####
#selecionar colunas 
selecao<- select(dados,area_km2,mean_SPM, mean_precipitation,mean_discharge,anthropic_km2,
                 mean_u__wind,mean_v__wind,agg_water_period)

selecao <- filter(selecao,agg_water_period=='LW_R') 

selecao<-select(selecao,-agg_water_period)
head(selecao)
summary(selecao)

#teste da série
apply(selecao, 2, function(x) notrend_test(x)$p.value)
apply(selecao, 2, function(x) notrend_test(x, test = "MK")$p.value)
apply(selecao, 2, function(x) notrend_test(x,test = "WAVK", 
                                           factor.length = "adaptive.selection")$p.value)

# HW_F ####
#selecionar colunas 
selecao<- select(dados,area_km2,mean_SPM, mean_precipitation,mean_discharge,anthropic_km2,
                 mean_u__wind,mean_v__wind,agg_water_period)

selecao <- filter(selecao,agg_water_period=='HW_F') 

selecao<-select(selecao,-agg_water_period)
head(selecao)
summary(selecao)

#teste da série
apply(selecao, 2, function(x) notrend_test(x)$p.value)
apply(selecao, 2, function(x) notrend_test(x, test = "MK")$p.value)
apply(selecao, 2, function(x) notrend_test(x,test = "WAVK", 
                                           factor.length = "adaptive.selection")$p.value)
