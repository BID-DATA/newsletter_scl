# Codigo general para graficar indicadores: funciones, leer antes de cada capítulo
# Autor: Maria Reyes Retana
# Divsion: SCL-SCL: Investigacion

##### Librerias #####

#install indicators from SCLData
#devtools::install_github("EL-BID/idbsocialdataR", force = TRUE)

library(ggplot2)
library(tidyverse)
library(haven)
library(ggrepel)
library(extrafont)
library(geofacet)
library(scales)
library(RColorBrewer)
library(scldataR)
library(idbsocialdataR)
library(dplyr)

##### Preliminares y colores #####

#Define colors
colors_pal <- c('#17406D','#0F6FC6','#5FF3CB', '#009DD9','#FEA300', '#A5C249','#176A7B','#0BD0D9',
                '#10CF9B', '#FA5F00','#C8DA92','#CC0066')

countries_color <- sample(colors_pal, 12, replace = FALSE)

countries_color_prom <- c("ARG"= "#17406D", "BOL" = "#17406D","CHL" = "#17406D",
                          "COL" = "#17406D", "CRI" = "#17406D", "ECU" = "#17406D",
                          "MEX" = "#17406D", "PER" = "#17406D", "PRY" = "#17406D", 
                          "SLV" = "#17406D", "LAC" = "#009DD9", "BRA" = "#17406D", 
                          "URY" = "#17406D", "VEN" = "#17406D", "GTM" = "#17406D", "NIC" ="#17406D" , "DOM" ="#17406D" , 
                          "HND" = "#17406D", "PAN" = "#17406D")

# agregar región BID

isoalpha3 <- c("Promedio", "JAM", "SUR", "BHS")
country_name_en <- c("Promedio", "Jamaica", "Suriname", "Bahamas")
region_bid <- c("Promedio", "Caribe", "Caribe", "Caribe")

prom <- data.frame(isoalpha3, country_name_en, region_bid)

paises <- query_indicator(indicator = 'pobreza', 
                          yearstart = 2006, 
                          yearend =  2020) %>% 
  select(isoalpha3, country_name_en) %>% 
  distinct() %>% 
  mutate(region_bid = case_when(isoalpha3 %in% c("ARG", "BRA", "PRY", "CHL", "URY") ~ "Cono Sur", 
                                isoalpha3 %in% c("BOL", "COL", "ECU", "PER", "VEN") ~ "Grupo Andino", 
                                isoalpha3 %in% c("BHS", "BRB", "GUY", "JAM", "SUR", "TTO") ~ "Caribe", 
                                isoalpha3 %in% c("HTI", "MEX", "PAN", "DOM", "BLZ", "CRI", "SLV", "GTM", "HND", "NIC") ~ "Centroamerica", 
                                TRUE ~ "NO")) %>% 
  rbind(prom)
  

colors_regiones <- sample(colors_pal, 5, replace = FALSE)

##### Función para generar promedios #####

grouped_mean <- function(.data, .summary_var, ...) {
  .summary_var <- enquo(.summary_var)
  
  .data %>%
    group_by(...) %>%
    mutate(value = mean(!!.summary_var)) %>% 
    mutate(isoalpha3 = "Promedio", 
           country_name_en = "Promedio", 
           country_name_es = "Average", 
           source = "scldata", 
           se = NA_character_, 
           cv = NA_character_, 
           sample = NA_character_, 
           source_es = "scldata", 
           source_en = 'scldata') %>% 
    arrange(source, year) %>% 
    distinct()
  
}

##### Función gráfica cambios promedios ya con datos #####

ggfun_prom <- function(dat,  x.var, y.var) 
  {
  ggp_prom <- ggplot(dat, aes(x = fct_reorder(x.var, y.var),  fill = x.var)) +
    geom_col(aes(y = y.var)) +
    geom_text(data = dat,
              aes(y =  y.var, label = comma(y.var, accuracy = 0.1)),  
              family = 'Century Gothic', fontface = "bold", angle = 90) +
    labs(title = str_wrap(dat$label_es, 50), y = dat$valuetype) +
    theme(legend.position="none", 
          axis.title.y = element_text(color = "#000f1c", face = "bold", family = 'Century Gothic'),
          axis.title.x = element_blank(), 
          legend.title=element_blank(),panel.background = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          legend.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
    scale_fill_manual(values = countries_color_prom)
  
  print(ggp_prom)
}

ggfun_prom_porc <- function(dat,  x.var, y.var) 
{
  ggp_prom <- ggplot(dat, aes(x = fct_reorder(x.var, y.var),  fill = x.var)) +
    geom_col(aes(y = y.var)) +
    geom_text(data = dat,
              aes(y =  y.var, label = percent(y.var, accuracy = 0.1)),  
              family = 'Century Gothic', fontface = "bold", angle = 90) +
    labs(title = str_wrap(dat$label_es, 50), y = dat$valuetype) +
    theme(legend.position="none", 
          axis.title.y = element_text(color = "#000f1c", face = "bold", family = 'Century Gothic'),
          axis.title.x = element_blank(), 
          legend.title=element_blank(),panel.background = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          legend.text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          text = element_text(size = 8, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
    scale_fill_manual(values = countries_color_prom)
  
  print(ggp_prom)
}

##### Cambios evolución general #####

ggfun_evo <- function(dat, x.var, y.var, group){
  ggp_evo <- ggplot(data = dat, aes(x = x.var, 
                                    y = y.var,
                                    group = group)) +
    geom_line(size=1, color = c('#009DD9'))+
    geom_point(size = 1.5,color = c('#009DD9')) +
    geom_label_repel(aes( y = y.var, label = ifelse(year == "2020" | year == "2021" | year == "2006" |year == "2005", percent(y.var, .1), "")),
              fontface = "bold", family = 'Century Gothic',box.padding = 0.5, size = 2)+
    theme(legend.position="bottom", 
          axis.title.y = element_text(color = "#000f1c", face = "bold", family = 'Century Gothic'),
          axis.title.x = element_blank(), 
          legend.title=element_blank(), panel.background = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.text = element_text(size = 7, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          legend.text = element_text(size = 7, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          text = element_text(size = 5, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
    scale_y_continuous(labels = percent) 
  
  print(ggp_evo)
}

##### Gráfica una categoría con dos o tres niveles #####

ggfun_one <- function(dat, x.var, y.var, nivel) {
 
ggp_one <- ggplot(dat, aes(x = fct_reorder(x.var, y.var),  fill = nivel)) +
 geom_col(aes(y = y.var), position = "dodge") +
 geom_text(data = dat,
          aes(y =  y.var+.005, label = comma(y.var, accuracy = 0.1)),  
           family = 'Century Gothic', fontface = "bold", 
          position = position_dodge(width = .9), angle = 90) +
  labs(title = str_wrap(dat$label_es, 50), y = dat$valuetype) +
  theme(legend.position="bottom", 
        axis.title.y = element_text(color = "#000f1c", face = "bold", 
                                    family = 'Century Gothic'),
        axis.title.x = element_blank(), 
        legend.title=element_blank(),panel.background = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 8, color = "#000f1c", face = "bold", 
                                 family = 'Century Gothic'),
        legend.text = element_text(size = 8, color = "#000f1c", face = "bold",
                                   family = 'Century Gothic'),
        text = element_text(size = 8, color = "#000f1c", face = "bold", 
                            family = 'Century Gothic')) +
  scale_fill_manual(values = colors_pal) +
  scale_y_continuous(labels = comma)
  
  print(ggp_one)
  
}

##### Cambios por edad  y promedio #####

ggfun_mul <- function(ggplot, data){
  
ggp_mul<- ggplot +
    theme(legend.position="right",
          axis.title.y = element_text(color = "#000f1c", face = "bold", family = 'Century Gothic'),
          axis.title.x = element_blank(),
          legend.title=element_blank(),panel.background = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          legend.text = element_text(size = 10, color = "#000f1c", face = "bold", family = 'Century Gothic'),
          text = element_text(size = 14, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
    scale_color_brewer(palette = "Purples") +
    scale_y_continuous(labels = comma) +
    scale_fill_manual(values = countries_color_prom) +
    geom_hline(yintercept = 0)
    
print(ggp_mul)

}
  