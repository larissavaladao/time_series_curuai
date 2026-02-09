# dados dos mosaicos com periodos de água definidos por cotas em Obidos ####
#definir diretório de trabalho 
setwd("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df")

# import libraries and data
library(readr)
library(dplyr)
data <- as.data.frame(read.csv("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df/df_merged_local.csv"))

head(data)
summary(data)

# #imputar valores que estao faltando ###################
#500 iterations of predictive mean mapping for imputing
# #5 datasets
library(mice)
library(tidyr)


# #imputar dados faltantes faltantes
selecao <- select(data, mean_discharge,natural_km2)
dados_imp <- mice(selecao, m = 5, maxit = 100, method = 'pmm', seed = 500)
dados_comp <- complete(dados_imp, 3)#numero do dataset cujas imputa??es vc quer usar
summary(dados_comp)

selecao <- select(data, -mean_discharge, -natural_km2)
selecao$mean_discharge <- dados_comp$mean_discharge
# selecao$anthropogenic_km2 <- dados_comp$anthropogenic_km2
selecao$natural_km2 <- dados_comp$natural_km2
summary(selecao)

dados_temp1 <- selecao[!duplicated(selecao$year), ]
dados_temp1
dados_temp <- dados_temp1 %>% drop_na()

# ## fazer regressão lm para completar dados de area antropizada
anthropicLm = lm(anthropogenic_km2~year, data = dados_temp) #Create the linear regression
summary(anthropicLm)
a <- summary(anthropicLm)$coefficients[1,1] #Review the results

b <- summary(anthropicLm)$coefficients[2,1] #Review the results

selecao <- selecao %>% dplyr::mutate(anthropogenic_km2 = replace_na(anthropogenic_km2, a+(b*2024)))
tail(selecao)


write.csv(selecao, file = "filled_data.csv")