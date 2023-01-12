##### Revisión de dato mensual SCLData - Enero 2023

##### Contexto #####

# El dato de este mes será elaborado para el blog de abierto al público. En este 
# Script haré un paso a paso de cómo utilizar el paquete de SCLData en R (idbsocialdataR)

# idbsocialdata es un conjunto de librerías en R, Python y Stata que facilita la consulta 
# de indicadores sociales que produce el sector social del Banco Inter-Americano de Desarrollo
# El paquete inicialmente fue elaborado en R y traducido a Python. 

#### preliminares ######

#si no tenemos instalado nuestro paquete lo instalamos

#devtools::install_github("EL-BID/idbsocialdataR") 


# definimos las librerías que vamos a usar
library(tidyverse) # tidyverse para la estructura del código
library(idbsocialdataR) # librería para consultar indicadores sociales
library(scales) # para las escalas de la gráfica que haremos

#define colors
colors_pal <- c('#17406D','#A5C249','#5FF3CB', '#009DD9')

#### Leer datos SCLData ####

# Como el paquete tiene una cantidad significativa de indicadores podemos con la 
# siguiente función buscar todos los indicadores disponibles por tema y colección.

#query dictionary of indicators
dictionary <- idbsocialdataR:::query_dictionary() %>% select(collection, indicator, label_es, theme_es)

# Decidimos que queremos consultar por ejemplo la pobreza de Colombia, México y Bolivia.
# Para la población indígena, afrodescendiente y no indígena ni afrodescendiente. 
# Para el periodo de 2005 a 2020

# lo hacemos con la función query_indicator() indicando los siguientes parámetros.

pobreza <- query_indicator(indicator = 'pobreza', 
                           countries = 'COL,MEX,BOL', 
                           categories = 'ethnicity', 
                           yearstart = '2005', 
                           yearend = '2020') 

# tenemos una tabla con los datos disponibles para los argumentos que pedimos.
# ahorita podemos pasar a analizar los datos, así que haremos una gráfica sencilla
# para evaluar la evolución de este indicador de forma gráfica. 

##### Gráficas ####

query_plot <- pobreza %>% 
  # cambios de formato para que la gráfica salga bien
  mutate(year = as.numeric(year), 
         country_name_es = ifelse(str_detect(country_name_es, "Bolivia"), "Bolivia", country_name_es)) %>% 
  # defino mi gráfica
  ggplot() +
  # defino el tipo de gráfica = línea, eje x = año, eje y = pobreza, agrupado por etinicidad
  geom_line(aes(x = year, y = value, colour = ethnicity), size = .8) +
  # divido por país
  facet_wrap(~country_name_es) +
  # defino la escala de colores
  scale_color_manual(values = colors_pal)+
  # quiero el eje y como porcentaje
  scale_y_continuous(labels = label_percent(accuracy = .1))+
  theme_minimal() +
  # defino título y subtitulo
  labs(title = "Pobreza en América Latina por etnicidad", 
       caption = "Fuente: SCLData. Banco Inter-Americano de Desarrollo") +
  # defino posición, tamaño y fuente de mis títulos y leyendas.
  theme(legend.position="right",
        axis.title.y = element_text(color = "#000f1c", face = "bold", family = 'Century Gothic'),
        axis.title.x = element_blank(),
        legend.title=element_blank(),panel.background = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 7, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        legend.text = element_text(size = 7, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        text = element_text(size = 7, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        plot.background = element_rect(fill = "white")) 

# Save plot
ggsave(plot = query_plot, file = "Output/pobreza-race.jpg", height = 7, width = 10, units = "cm")
