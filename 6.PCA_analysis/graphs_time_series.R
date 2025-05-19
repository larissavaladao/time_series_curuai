#definir diret?rio de trabalho e importar dados ####
setwd("C:/Users/l_v_v/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df")
setwd("C:/Users/LarissaVieiraValadão/Documents/GitHub/time_series_curuai/datasets/Parameters Time series/merged_df")

library(readr)
library(dplyr)

dados <- as.data.frame(read.csv("filled_data_cota_lulc.csv"))
dados <- select(dados,-X,-class_name,-stdDev_SPM)

head(dados)


#fazer graficos - blbiotecas necessarias
library(ggplot2)
library(grid)
library(gtable)
library(ggpmisc)
library("cowplot")



#graficos com ggplot seguem o modelo:
#ggplot(data, aes(x=x,y=y)) + geom_how to render()

#definir o nome dos periodos como fator e datas######
dados$water_period <- as.factor(dados$water_period)
dados$time_start <- as.Date(dados$time_start)
dados$time_finish <- as.Date(dados$time_finish)

# Create a new column based on condition
dados <- dados %>% mutate(agg_water_period = ifelse(water_period == 'LW'|water_period == 'R', "LW_R", "HW_F"))
dados$agg_water_period <- as.factor(dados$agg_water_period)


str(dados)
summary(dados)

#definir formula usada 
my.formula <- y ~ x

########################################################################
#grafico de variacaoo de area por tempo para todos os periodos#####
ggplot(data = dados, 
       mapping = aes(time_start, area_km2)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=TRUE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    labs(y="Surface Water Area (km²)",x="Date")+
    theme(legend.position="none",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))
# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de area por periodos######

ggplot(data = dados, 
       mapping = aes(time_start, area_km2, 
                     color = water_period)) +
     stat_poly_eq(formula = my.formula, 
                  aes(label = paste(stat(eq.label))), 
                  parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                  label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Surface Water Area (km²)",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de variacao de area por periodos agregados em 2####################

ggplot(data = dados, 
       mapping = aes(time_start, area_km2, 
                     color = agg_water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(agg_water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Surface Water Area (km²)",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

########################################################################
#grafico de variacaoo de SPM medio por tempo para todos os periodos#####
ggplot(data = dados, 
       mapping = aes(time_start, mean_SPM)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=TRUE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    labs(y="Mean SPM (mg/L)",x="Date")+
    theme(legend.position="none",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))
# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de SPM medio por periodos######

ggplot(data = dados, 
       mapping = aes(time_start, mean_SPM, 
                     color = water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Mean SPM (mg/L)",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de variacao de SPM medio por periodos agregados em 2####################

ggplot(data = dados, 
       mapping = aes(time_start, mean_SPM, 
                     color = agg_water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(agg_water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Mean SPM (mg/L)",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

########################################################################
#grafico de variacao de SPM maximo por tempo para todos os periodos#####
ggplot(data = dados, 
       mapping = aes(time_start, max_SPM)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=TRUE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    labs(y="Max SPM (mg/L)",x="Date")+
    theme(legend.position="none",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))
# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de SPM maximo por periodos######

ggplot(data = dados, 
       mapping = aes(time_start, max_SPM, 
                     color = water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Max SPM (mg/L)",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de variacao de SPM maximo por periodos agregados em 2####################

ggplot(data = dados, 
       mapping = aes(time_start, max_SPM, 
                     color = agg_water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(agg_water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Max SPM (mg/L)",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

########################################################################
#grafico de variacao de SPM minimo por tempo para todos os periodos#####
ggplot(data = dados, 
       mapping = aes(time_start, min_SPM)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=TRUE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    labs(y="Min SPM (mg/L)",x="Date")+
    theme(legend.position="none",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))
# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de SPM minimo por periodos######

ggplot(data = dados, 
       mapping = aes(time_start, min_SPM, 
                     color = water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Min SPM (mg/L)",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de variacao de SPM minimo por periodos agregados em 2####################

ggplot(data = dados, 
       mapping = aes(time_start, min_SPM, 
                     color = agg_water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(agg_water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Min SPM (mg/L)",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

########################################################################
#grafico de variacao de precipitação media por tempo para todos os periodos#####
ggplot(data = dados, 
       mapping = aes(time_start, mean_precipitation)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=TRUE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    labs(y="Mean Acc Precipitation (mm/water period)",x="Date")+
    theme(legend.position="none",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))
# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de precipitação media por periodos######

ggplot(data = dados, 
       mapping = aes(time_start, mean_precipitation, 
                     color = water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Mean Acc Precipitation (mm/water period)",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de variacao de precipitação media por periodos agregados em 2####################

ggplot(data = dados, 
       mapping = aes(time_start, mean_precipitation, 
                     color = agg_water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(agg_water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Mean Acc Precipitation (mm/water period)",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

########################################################################
#grafico de variacao de vazao media por tempo para todos os periodos#####
ggplot(data = dados, 
       mapping = aes(time_start, mean_discharge)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=TRUE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    labs(y="Mean Discharge ()",x="Date")+
    theme(legend.position="none",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))
# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de vazao media por periodos######

ggplot(data = dados, 
       mapping = aes(time_start, mean_discharge, 
                     color = water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Mean Discharge ()",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de variacao de vazao media por periodos agregados em 2####################

ggplot(data = dados, 
       mapping = aes(time_start, mean_discharge, 
                     color = agg_water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(agg_water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Mean Discharge ()",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

########################################################################
#grafico de variacao de mean_u__wind  por tempo para todos os periodos#####
ggplot(data = dados, 
       mapping = aes(time_start, mean_u__wind)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=TRUE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    labs(y="Mean U Wind ()",x="Date")+
    theme(legend.position="none",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))
# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de mean_u__wind por periodos######

ggplot(data = dados, 
       mapping = aes(time_start, mean_u__wind, 
                     color = water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Mean U Wind ()",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de variacao de mean_u__wind por periodos agregados em 2####################

ggplot(data = dados, 
       mapping = aes(time_start, mean_u__wind, 
                     color = agg_water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(agg_water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Mean U Wind ()",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

########################################################################
#grafico de variacao de mean_v__wind  por tempo para todos os periodos#####
ggplot(data = dados, 
       mapping = aes(time_start, mean_v__wind)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=TRUE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    labs(y="Mean V Wind ()",x="Date")+
    theme(legend.position="none",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))
# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de mean_v__wind por periodos######

ggplot(data = dados, 
       mapping = aes(time_start, mean_v__wind, 
                     color = water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Mean V Wind ()",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de variacao de mean_v__wind por periodos agregados em 2####################

ggplot(data = dados, 
       mapping = aes(time_start, mean_v__wind, 
                     color = agg_water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(agg_water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Mean V Wind ()",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

########################################################################
#grafico de variacao de anthropic_km2  por tempo para todos os periodos#####
ggplot(data = dados, 
       mapping = aes(time_start, anthropic_km2)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=TRUE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    labs(y="Anthropic Area (km2)",x="Date")+
    theme(legend.position="none",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))
# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de anthropic_km2 por periodos######

ggplot(data = dados, 
       mapping = aes(time_start, anthropic_km2, 
                     color = water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Anthropic Area (km2)",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#grafico de variacao de anthropic_km2 por periodos agregados em 2####################

ggplot(data = dados, 
       mapping = aes(time_start, anthropic_km2, 
                     color = agg_water_period)) +
    stat_poly_eq(formula = my.formula, 
                 aes(label = paste(stat(eq.label))), 
                 parse = TRUE, size = 2.7, color = 'black', label.x = "left",
                 label.y = "top")+
    geom_smooth(method = 'lm', se = FALSE, linewidth=0.3, linetype=1,formula =my.formula) + #regress?o linear
    #geom_smooth(se=FALSE) +  #regress?o "loess"
    geom_point() +   #inclui os pontos
    facet_grid(rows = vars(agg_water_period), #separa subgraficos por periodo
               scales = "free")+ #deixa livre as escalas dos eixos x e y
    labs(y="Anthropic Area (km2)",x="Date")+
    theme(legend.position="bottom",
          axis.title=element_text(size=8, face = 'bold'),
          axis.text.x=element_text(size=7.5),
          axis.text.y=element_text(size=7.5),
          strip.text.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.background = element_rect(fill = '#F1F1F1',
                                          colour = 'gray',
                                          linetype = 'solid',
                                          inherit.blank = FALSE))

# ggsave(
#     'reservoir_area.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 3,
#     height = 4,
#     units =  c("in")
# )

#ajustar alinhamento dos gr?ficos com pacote gtable
#transformar em Grob - incluindo dados da bacia tocantins toda
# g1 <- ggplotGrob(precipitacao)
# g2 <- ggplotGrob(afluencia)
# g3 <- ggplotGrob(defluencia)
# g4 <- ggplotGrob(evaporacao)
# 
# g5 <- ggplotGrob(urbano)
# g6 <- ggplotGrob(natural)
# g7 <- ggplotGrob(agricultura)
# g8 <- ggplotGrob(pastagem)
# 
# #juntar por linhas
# gC1 <- cbind(g5, g6)
# gC2 <- cbind(g7, g8)
# gR <- rbind(gC1,gC2)
# #ajustar tamanho m?ximo
# g$widths <- unit.pmax(g1$widths,g2$widths, g3$widths,g4$widths)
# #novap?gina em branco
# grid.newpage()
# #desenhar graficos na pagina
# grid.draw(gR)
# 
# #################################################################################
# require(ggplot2)
# #bacia tocantins
# tocantins <- as.data.frame(read_excel("novos_dados.xlsx", 
#                                       sheet = "graf_tocantins"))
# 
# #definir o nome dos reservat?rios como fator
# tocantins$parametro <- as.factor(tocantins$parametro)
# #formula usada na regress?o
# my.formula <- y ~ x
# #divis?o dos dados em duas d?cadas para serem usados na regress?o
# dadosAno1 = filter(tocantins, ano < 2011)
# dadosAno2 = filter(tocantins, ano > 2009)
# #grafico de varia??o de parametros por tempo para a bacia
# #op??o de mudar a regress?o ou colocar grafico de linhas commented out
# 
# ggplot(data = tocantins, 
#        mapping = aes(ano, valor, 
#                      color = parametro)) +
#     geom_vline(xintercept=2010, color='gray')+
#     geom_point() +
#     #geom_line() +
#     geom_smooth(data=dadosAno1,method = 'lm', se = FALSE, linewidth=0.3, linetype=1) + #regress?o linear
#     # stat_poly_eq(data=dadosAno1,formula = my.formula, 
#     #              aes(label = paste(stat(eq.label))), 
#     #              parse = TRUE, size = 3, color = 'black', label.x = "left",
#     #              label.y = "top")+
#     geom_smooth(data=dadosAno2,method = 'lm', se = FALSE, linewidth=0.3, linetype=1) +
#     # stat_poly_eq(data=dadosAno2,formula = my.formula, 
#     #              aes(label = paste(stat(eq.label))), 
#     #              parse = TRUE, size = 3, color = 'black', label.x = "right",
#     #              label.y = "top")+
#     facet_grid(rows = vars(parametro), #separa subgraficos por reservatorio
#                scales = "free")+ #deixa livre as escalas dos eixos x e y
#     labs(x="Year", y = element_blank())+
#     scale_y_continuous(label=comma)+
#     theme(legend.position="none",
#           axis.title=element_text(size=8,face="bold"),
#           axis.text.x=element_text(size=7.5),
#           axis.text.y=element_text(size=7.5),
#           strip.text.y = element_text(angle = 0, size = 7.5),
#           panel.grid.major.x = element_blank(),
#           panel.grid.minor.x = element_blank(),
#           panel.grid.major.y = element_blank(),
#           panel.grid.minor.y = element_blank(),
#           panel.background = element_rect(fill = '#F1F1F1',
#                                           colour = 'gray',
#                                           linetype = 'solid',
#                                           inherit.blank = FALSE)) 
# 
# ggsave(
#     'tocantins_watershed.jpg',
#     plot = last_plot(),
#     path = "C:/Users/larissa.valadao/Desktop",
#     dpi = 300,
#     width = 5,
#     height = 6,
#     units =  c("in")
# )
# #dev.print( device = jpeg, filename = "tocantinsATT1.jpg",  width = 500,  height = 700)
# 
# #op??o de divis?o em dois gr?ficos A e B - n?o usado
# #de 2000 at? 2010
# dec1 <- ggplot(data = dadosAno1, 
#                mapping = aes(ano, valor, 
#                              color = parametro)) +
#     scale_x_continuous(breaks=c(2000,2002,2004,2006,2008,2010),labels = c(2000,2002,2004,2006,2008,2010))+
#     #stat_poly_eq(formula = my.formula, 
#     #             aes(label = paste(stat(eq.label))), 
#     #             parse = TRUE, size = 3, color = 'black', label.x = "center",
#     #             label.y = "top")+
#     geom_point() +
#     #geom_smooth(se = FALSE) +
#     geom_smooth(method = 'lm', se = FALSE) + #regress?o linear
#     facet_grid(rows = vars(parametro), #separa subgraficos por reservatorio
#                scales = "free")+ #deixa livre as escalas dos eixos x e y
#     labs(subtitle='2000 to 2010',x="Year", y = element_blank())+
#     theme(legend.position="none",
#           axis.text.x=element_text(size=8),
#           axis.text.y=element_text(size=8),
#           strip.text.y = element_blank(),
#           panel.grid.major.x = element_blank(),
#           panel.grid.minor.x = element_blank(),
#           panel.grid.major.y = element_blank(),
#           panel.grid.minor.y = element_blank()) 
# 
# dec1
# 
# #de 2010 at? 2020
# 
# dadosAno2$ano <- as.integer(dadosAno2$ano)
# dec2 <- ggplot(data = dadosAno2, 
#                mapping = aes(ano, valor, 
#                              color = parametro)) +
#     scale_x_continuous(breaks=c(2010,2012,2014,2016,2018,2020),labels = c(2010,2012,2014,2016,2018,2020))+
#     #stat_poly_eq(formula = my.formula, 
#     #             aes(label = paste(stat(eq.label))), 
#     #             parse = TRUE, size = 3, color = 'black', label.x = "center",
#     #             label.y = "top")+
#     geom_point() +
#     #geom_smooth(se = FALSE) +
#     geom_smooth(method = 'lm', se = FALSE) + #regress?o linear
#     facet_grid(rows = vars(parametro), #separa subgraficos por reservatorio
#                scales = "free")+ #deixa livre as escalas dos eixos x e y
#     labs(subtitle='    2010 to 2020',x="Year", y = element_blank())+
#     theme(legend.position="none",
#           axis.text.x=element_text(size=8),
#           axis.text.y=element_text(size=8),
#           strip.text.y = element_text(angle = 0, size = 8.8),
#           panel.grid.major.x = element_blank(),
#           panel.grid.minor.x = element_blank(),
#           panel.grid.major.y = element_blank(),
#           panel.grid.minor.y = element_blank()) 
# 
# dec2
# #separa em duas colunas
# plot_grid(dec1, dec2, labels=c("A", "B"), ncol = 2, nrow = 1, rel_widths = c(0.4255, 0.5745))
# 
# ## dados por reservat?rio n?o vai ser usado
# #Serra da Mesa#########################################################################
# #filtro dos dados de serra da mesa
# require(ggpmisc)
# my.formula <- y ~ x
# SM <- dados %>% filter(Reservoir == '1. Serra da Mesa')
# SMg <- ggplot(data = SM) + #defini??o do gr?fico geral
#     xlim(c(2000,2020)) #intervalo de tempo
# 
# #gr?fico de area
# SM_A<- SMg + geom_point(mapping = aes(Year, Area)) + 
#     labs(title = "Serra da Mesa") +  #titulo do gr?fico 
#     geom_smooth(mapping = aes(Year, Area), 
#                 method = 'loess', 
#                 color = 'red',
#                 se = FALSE) 
# SM_A
# #grafico de precipita??o
# SM_Prec <- SMg + 
#     geom_point(mapping = aes(Year, Precipitation)) +
#     geom_smooth(mapping = aes(Year, Precipitation), 
#                 method = 'lm', 
#                 color = "green",
#                 se=FALSE) +
#     stat_poly_eq(formula = my.formula, 
#                  aes(label = paste(stat(eq.label))), 
#                  parse = TRUE, size = 2.7, color = 'black', label.x = "center",
#                  label.y = "top")+
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank()) 
# 
# #gr?fico de defluencia
# SM_D <- SMg + 
#     geom_point(mapping = aes(Year, Outflow)) +
#     geom_smooth(mapping = aes(Year, Outflow), 
#                 method = 'loess', 
#                 color = "blue",
#                 se=FALSE) 
# SM_D 
# 
# #gr?fico de vazao por evapora??o liquida
# SM_Lq <- SMg + 
#     geom_point(mapping = aes(Year, Liq_evaporation)) +
#     geom_smooth(mapping = aes(Year, Liq_evaporation), 
#                 method = 'lm', 
#                 color = "purple",
#                 se=FALSE) +
#     stat_poly_eq(formula = my.formula, 
#                  aes(label = paste(stat(eq.label))), 
#                  parse = TRUE, size = 2.7, color = 'black', label.x = "center",
#                  label.y = "top")+
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# 
# #gr?fico de percentual de classe urbana 
# SM_Ub <- SMg + 
#     geom_point(mapping = aes(Year, Urban)) +
#     geom_smooth(mapping = aes(Year, Urban), 
#                 method = 'lm', 
#                 color = "black",
#                 se=FALSE) +
#     stat_poly_eq(formula = my.formula, 
#                  aes(label = paste(stat(eq.label))), 
#                  parse = TRUE, size = 2.7, color = 'black', label.x = "center",
#                  label.y = "top")+
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #gr?fico de percentual de classe natural 
# SM_N <- SMg + 
#     geom_point(mapping = aes(Year, Natural)) +
#     geom_smooth(mapping = aes(Year, Natural), 
#                 method = 'lm', 
#                 color = "brown",
#                 se=FALSE) +
#     stat_poly_eq(formula = my.formula, 
#                  aes(label = paste(stat(eq.label))), 
#                  parse = TRUE, size = 2.7, color = 'black', label.x = "center",
#                  label.y = "top")+
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #gr?fico de percentual de classe agricultura 
# SM_Ag <- SMg + 
#     geom_point(mapping = aes(Year, Agriculture)) +
#     geom_smooth(mapping = aes(Year, Agriculture), 
#                 method = 'lm', 
#                 color = "pink",
#                 se=FALSE) +
#     stat_poly_eq(formula = my.formula, 
#                  aes(label = paste(stat(eq.label))), 
#                  parse = TRUE, size = 2.7, color = 'black', label.x = "center",
#                  label.y = "top")+
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #gr?fico de percentual de classe pastagem 
# SM_P <- SMg + 
#     geom_point(mapping = aes(Year, Pasture)) +
#     geom_smooth(mapping = aes(Year, Pasture), 
#                 method = 'lm', 
#                 color = "yellow",
#                 se=FALSE) 
# 
# #ajustar alinhamento dos gr?ficos com pacote gtable
# #transformar em Grob - incluindo dados da bacia tocantins toda
# g1 <- ggplotGrob(SM_A)
# g2 <- ggplotGrob(SM_Prec)
# g3 <- ggplotGrob(SM_AF)
# g4 <- ggplotGrob(SM_D)
# g5 <- ggplotGrob(SM_Lq)
# g6 <- ggplotGrob(SM_Ub)
# g7 <- ggplotGrob(SM_N)
# g8 <- ggplotGrob(SM_Ag)
# g9 <- ggplotGrob(SM_P)
# 
# #juntar por linhas
# g <- rbind(g1, g2, g3, g4, g5, g6,g7,g8,g9,  size = "first")
# #ajustar tamanho m?ximo
# g$widths <- unit.pmax(g1$widths,g2$widths, g3$widths,g4$widths, g5$widths, 
#                       g6$widths, g7$widths, g8$widths, g9$widths)
# #novap?gina em branco
# grid.newpage()
# #desenhar graficos na pagina
# grid.draw(g)
# 
# #Cana Brava#######################################################################
# #filtrar por reservat?rio
# CB <- dados %>% filter(Reservoir == '2. Cana Brava')
# #base dos graficos
# CBg <- ggplot(data = CB)  + 
#     xlim(c(2003,2020))
# 
# #area
# CB_A<- CBg + 
#     geom_point(mapping = aes(Year, Area )) + 
#     labs(title = "Cana Brava") +
#     geom_smooth(mapping = aes(Year, Area ), 
#                 method = 'loess', 
#                 color = 'red',
#                 se=FALSE) 
# CB_A
# #defluencia
# CB_D <- CBg + 
#     geom_point(mapping = aes(Year, Outflow)) +
#     geom_smooth(mapping = aes(Year, Outflow), 
#                 method = 'loess', 
#                 color = "blue",
#                 se=F) 
# CB_D
# #precipitacao
# CB_Prec <- CBg + 
#     geom_point(mapping = aes(ano, precipitacao)) +
#     geom_smooth(mapping = aes(ano, precipitacao), 
#                 method = 'lm', 
#                 color = "green") +
#     # geom_smooth(mapping = aes(ano, precipitacao), 
#     #             color = "green") +
#     # geom_line(mapping = aes(ano, precipitacao), 
#     #           color = "green") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# # % classe antropica
# CB_Ant <- CBg + 
#     geom_point(mapping = aes(ano, perc_antropico)) +
#     geom_smooth(mapping = aes(ano, perc_antropico), 
#                 method = 'lm', 
#                 color = "black") +
#     # geom_smooth(mapping = aes(ano, perc_antropico),  
#     #             color = "black")+
#     # geom_line(mapping = aes(ano, perc_antropico),  
#     #           color = "black") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #ajustar alinhamento dos graficos na pagina
# g1 <- ggplotGrob(CB_A)
# g2 <- ggplotGrob(CB_D)
# g3 <- ggplotGrob(CB_Prec)
# g3TOC <- ggplotGrob(TOC_Prec)
# g4 <- ggplotGrob(CB_Ant)
# g4TOC <- ggplotGrob(TOC_Ant)
# g <- rbind(g1, g2, g3, g3TOC, g4, g4TOC,  size = "first")
# g$widths <- unit.pmax(g1$widths,g2$widths, g3$widths,g3TOC$widths, g4$widths, 
#                       g4TOC$widths)
# grid.newpage()
# grid.draw(g)
# 
# #S?o Salvador#######################################################################
# #filtrar dados por reservat?rio
# SS <- dados %>% filter(reservatorio == '3SS')
# 
# #base do grafico
# SSg <- ggplot(data = SS) +  
#     xlim(c(2010,2020))
# 
# #area
# SS_A<- SSg + 
#     geom_point(mapping = aes(ano, area)) + 
#     labs(title = "S?o Salvador") +
#     geom_smooth(mapping = aes(ano, area), 
#                 method = 'lm', 
#                 color = 'red') +
#     # geom_smooth(mapping = aes(ano, area), 
#     #             = 'red') +
#     # geom_line(mapping = aes(ano, area), 
#     #           color = 'red') +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #defluencia
# SS_D <- SSg + 
#     geom_point(mapping = aes(ano, defluencia)) +
#     geom_smooth(mapping = aes(ano, defluencia), 
#                 method = 'lm', 
#                 color = "blue") +
#     # geom_smooth(mapping = aes(ano, defluencia), 
#     #             color = "blue") +
#     # geom_line(mapping = aes(ano, defluencia), 
#     #           color = "blue") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# SS_Prec <- SSg + 
#     geom_point(mapping = aes(ano, precipitacao)) +
#     geom_smooth(mapping = aes(ano, precipitacao), 
#                 method = 'lm', 
#                 color = "green") +
#     # geom_smooth(mapping = aes(ano, precipitacao), 
#     #             color = "green") +
#     # geom_line(mapping = aes(ano, precipitacao), 
#     #           color = "green") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank()) 
# 
# # %classe antropica
# SS_Ant <- SSg + 
#     geom_point(mapping = aes(ano, perc_antropico)) +
#     geom_smooth(mapping = aes(ano, perc_antropico), 
#                 method = 'lm', 
#                 color = "black") +
#     # geom_smooth(mapping = aes(ano, perc_antropico),  
#     #             color = "black")+
#     # geom_line(mapping = aes(ano, perc_antropico),  
#     #           color = "black") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #ajustar alinhamento dos graficos
# g1 <- ggplotGrob(SS_A)
# g2 <- ggplotGrob(SS_D)
# g3 <- ggplotGrob(SS_Prec)
# g3TOC <- ggplotGrob(TOC_Prec)
# g4 <- ggplotGrob(SS_Ant)
# g4TOC <- ggplotGrob(TOC_Ant)
# g <- rbind(g1, g2, g3, g3TOC, g4, g4TOC,  size = "first")
# g$widths <- unit.pmax(g1$widths,g2$widths, g3$widths,g3TOC$widths, g4$widths, 
#                       g4TOC$widths)
# grid.newpage()
# grid.draw(g)
# 
# #Peixe Angical#######################################################################
# #filtrar opr reservat?rio
# PA<- dados %>% filter(reservatorio == '4PA')
# #base do gr?fico
# PAg <- ggplot(data = PA) +  
#     xlim(c(2006,2020)) #intervalo do gr?fico no eixo x
# 
# #area
# PA_A<- PAg + 
#     geom_point(mapping = aes(ano, area)) + 
#     labs(title = "Peixe Angical") +
#     geom_smooth(mapping = aes(ano, area), 
#                 method = 'lm', 
#                 color = 'red') +
#     # geom_smooth(mapping = aes(ano, area), 
#     #             color = 'red') +
#     # geom_line(mapping = aes(ano, area), 
#     #           color = 'red') +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #defluencia
# PA_D <- PAg + 
#     geom_point(mapping = aes(ano, defluencia)) +
#     geom_smooth(mapping = aes(ano, defluencia), 
#                 method = 'lm', 
#                 color = "blue") +
#     # geom_smooth(mapping = aes(ano, defluencia), 
#     #             color = "blue") +
#     # geom_line(mapping = aes(ano, defluencia), 
#     #           color = "blue") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #precipitacao
# PA_Prec <- PAg + 
#     geom_point(mapping = aes(ano, precipitacao)) +
#     geom_smooth(mapping = aes(ano, precipitacao), 
#                 method = 'lm', 
#                 color = "green") +
#     # geom_smooth(mapping = aes(ano, precipitacao), 
#     #             color = "green") +
#     # geom_line(mapping = aes(ano, precipitacao), 
#     #           color = "green") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank()) 
# 
# # %classe antr?pica
# PA_Ant <- PAg + 
#     geom_point(mapping = aes(ano, perc_antropico)) +
#     geom_smooth(mapping = aes(ano, perc_antropico), 
#                 method = 'lm', 
#                 color = "black") +
#     # geom_smooth(mapping = aes(ano, perc_antropico),  
#     #             color = "black")+
#     # geom_line(mapping = aes(ano, perc_antropico),  
#     #           color = "black") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #ajustar alinhamento dos graficos na pagina
# g1 <- ggplotGrob(PA_A)
# g2 <- ggplotGrob(PA_D)
# g3 <- ggplotGrob(PA_Prec)
# g3TOC <- ggplotGrob(TOC_Prec)
# g4 <- ggplotGrob(PA_Ant)
# g4TOC <- ggplotGrob(TOC_Ant)
# g <- rbind(g1, g2, g3, g3TOC, g4, g4TOC,  size = "first")
# g$widths <- unit.pmax(g1$widths,g2$widths, g3$widths,g3TOC$widths, g4$widths, 
#                       g4TOC$widths)
# grid.newpage()
# grid.draw(g)
# 
# #Lajeado#######################################################################
# #filtrar por reservat?rio
# L<- dados %>% filter(reservatorio == '5L')
# #base do gr?fico
# Lg <- ggplot(data = L) +  
#     xlim(c(2003,2020)) #intervalo do grafico no eixo X
# 
# #area
# L_A<- Lg + 
#     geom_point(mapping = aes(ano, area)) + 
#     labs(title = "Lajeado") +
#     geom_smooth(mapping = aes(ano, area), 
#                 method = 'lm', 
#                 color = 'red') +
#     # geom_smooth(mapping = aes(ano, area), 
#     #             color = 'red') +
#     # geom_line(mapping = aes(ano, area), 
#     #           color = 'red') +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #defluencia
# L_D <- Lg + 
#     geom_point(mapping = aes(ano, defluencia)) +
#     geom_smooth(mapping = aes(ano, defluencia), 
#                 method = 'lm', 
#                 color = "blue") +
#     # geom_smooth(mapping = aes(ano, defluencia), 
#     #             color = "blue") +
#     # geom_line(mapping = aes(ano, defluencia), 
#     #           color = "blue") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #precipitacao
# L_Prec <- Lg + 
#     geom_point(mapping = aes(ano, precipitacao)) +
#     geom_smooth(mapping = aes(ano, precipitacao), 
#                 method = 'lm', 
#                 color = "green") +
#     # geom_smooth(mapping = aes(ano, precipitacao), 
#     #             color = "green") +
#     # geom_line(mapping = aes(ano, precipitacao), 
#     #           color = "green") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank()) 
# 
# # %classe antropica
# L_Ant <- Lg + 
#     geom_point(mapping = aes(ano, perc_antropico)) +
#     geom_smooth(mapping = aes(ano, perc_antropico), 
#                 method = 'lm', 
#                 color = "black")+ 
#     # geom_smooth(mapping = aes(ano, perc_antropico),  
#     #             color = "black") +
#     # geom_line(mapping = aes(ano, perc_antropico),  
#     #           color = "black") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #ajustar alinhamento na p?gina
# g1 <- ggplotGrob(L_A)
# g2 <- ggplotGrob(L_D)
# g3 <- ggplotGrob(L_Prec)
# g3TOC <- ggplotGrob(TOC_Prec)
# g4 <- ggplotGrob(L_Ant)
# g4TOC <- ggplotGrob(TOC_Ant)
# g <- rbind(g1, g2, g3, g3TOC, g4, g4TOC,  size = "first")
# g$widths <- unit.pmax(g1$widths,g2$widths, g3$widths,g3TOC$widths, g4$widths, g4TOC$widths)
# grid.newpage()
# grid.draw(g)
# 
# #Estreito#######################################################################
# #filtrar por reservat?rio
# E<- dados %>% filter(reservatorio == '6E')
# #grafico base
# Eg <- ggplot(data = E) +  
#     xlim(c(2012,2020)) #intervalo do eixo x
# 
# #area
# E_A<- Eg + 
#     geom_point(mapping = aes(ano, area)) + 
#     labs(title = "Estreito") +
#     geom_smooth(mapping = aes(ano, area), 
#                 method = 'lm', 
#                 color = 'red') +
#     # geom_smooth(mapping = aes(ano, area), 
#     #             color = 'red') +
#     # geom_line(mapping = aes(ano, area), 
#     #           color = 'red') +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #defluencia
# E_D <- Eg + 
#     geom_point(mapping = aes(ano, defluencia)) +
#     geom_smooth(mapping = aes(ano, defluencia), 
#                 method = 'lm', 
#                 color = "blue") +
#     # geom_smooth(mapping = aes(ano, defluencia), 
#     #             color = "blue") +
#     # geom_line(mapping = aes(ano, defluencia), 
#     #           color = "blue") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# E_Prec <- Eg + 
#     geom_point(mapping = aes(ano, precipitacao)) +
#     geom_smooth(mapping = aes(ano, precipitacao),
#                 method = 'lm', 
#                 color = "green") +
#     # geom_smooth(mapping = aes(ano, precipitacao), 
#     #             color = "green") +
#     # geom_line(mapping = aes(ano, precipitacao), 
#     #           color = "green") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank()) 
# 
# # %classe antropica
# E_Ant <- Eg + 
#     geom_point(mapping = aes(ano, perc_antropico)) +
#     geom_smooth(mapping = aes(ano, perc_antropico), 
#                 method = 'lm', 
#                 color = "black") +
#     # geom_smooth(mapping = aes(ano, perc_antropico),  
#     #             color = "black") +
#     # geom_line(mapping = aes(ano, perc_antropico),  
#     #           color = "black") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #ajustar alinhamento do gr?fico
# g1 <- ggplotGrob(E_A)
# g2 <- ggplotGrob(E_D)
# g3 <- ggplotGrob(E_Prec)
# g3TOC <- ggplotGrob(TOC_Prec)
# g4 <- ggplotGrob(E_Ant)
# g4TOC <- ggplotGrob(TOC_Ant)
# g <- rbind(g1, g2, g3, g3TOC, g4, g4TOC,  size = "first")
# g$widths <- unit.pmax(g1$widths,g2$widths, g3$widths,g3TOC$widths, g4$widths, 
#                       g4TOC$widths)
# grid.newpage()
# grid.draw(g)
# 
# #Tucurui#######################################################################
# #filtrar por reservat?rio
# Tuc<- dados %>% filter(reservatorio == '7Tuc')
# #gr?fico base
# Tucg <- ggplot(data = Tuc) +  
#     xlim(c(2000,2020)) #intervalo de tempo do eixo x
# 
# #area
# Tuc_A<- Tucg + 
#     geom_point(mapping = aes(ano, area)) + 
#     labs(title = "Tucuru?") +
#     geom_smooth(mapping = aes(ano, area), 
#                 method = 'lm', 
#                 color = 'red') +
#     # geom_smooth(mapping = aes(ano, area), 
#     #             color = 'red') +
#     # geom_line(mapping = aes(ano, area), 
#     #           color = 'red') +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #defluencia
# Tuc_D <- Tucg + 
#     geom_point(mapping = aes(ano, defluencia)) +
#     geom_smooth(mapping = aes(ano, defluencia), 
#                 method = 'lm', 
#                 color = "blue") +
#     # geom_smooth(mapping = aes(ano, defluencia), 
#     #             color = "blue") +
#     # geom_line(mapping = aes(ano, defluencia), 
#     #           color = "blue") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #precipitacao
# Tuc_Prec <- Tucg + 
#     geom_point(mapping = aes(ano, precipitacao)) +
#     geom_smooth(mapping = aes(ano, precipitacao), 
#                 method = 'lm', 
#                 color = "green") +
#     # geom_smooth(mapping = aes(ano, precipitacao), 
#     #             color = "green") +
#     # geom_line(mapping = aes(ano, precipitacao), c
#     #           olor = "green") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank()) 
# 
# # %classe antropica
# Tuc_Ant <- Tucg + 
#     geom_point(mapping = aes(ano, perc_antropico)) +
#     geom_smooth(mapping = aes(ano, perc_antropico), 
#                 method = 'lm', 
#                 color = "black") + 
#     # geom_smooth(mapping = aes(ano, perc_antropico),  
#     #             color = "black")
#     # geom_line(mapping = aes(ano, perc_antropico),  
#     #           color = "black") +
#     theme(axis.title.x=element_blank(),
#           axis.text.x=element_blank(),
#           axis.ticks.x=element_blank())
# 
# #ajustar alinhamento na pagina
# g1 <- ggplotGrob(Tuc_A)
# g2 <- ggplotGrob(Tuc_D)
# g3 <- ggplotGrob(Tuc_Prec)
# g3TOC <- ggplotGrob(TOC_Prec)
# g4 <- ggplotGrob(Tuc_Ant)
# g4TOC <- ggplotGrob(TOC_Ant)
# g <- rbind(g1, g2, g3, g3TOC, g4, g4TOC,  size = "first")
# g$widths <- unit.pmax(g1$widths,g2$widths, g3$widths,g3TOC$widths, g4$widths, g4TOC$widths)
# grid.newpage()
# grid.draw(g)