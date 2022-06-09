##### Revisión de dato mensual SCLData

#### preliminares ######

source("Indicadores funciones.R")

options(scipen = 999)

#### Leer datos de proyección pobreza ####

pobreza_proyeccion <- read.csv("Input/Poverty_inequality_2021_0422_CRF.csv")

##### Gráficas ####

pobreza_lac <- pobreza_proyeccion %>% 
  dplyr::filter(pais == "LAC" & var == "pov50") %>%  
  pivot_longer(y1997:y2021, names_to = "year") %>% 
  mutate(year = str_sub(year, 2L, 5L)) %>% 
  mutate(value = case_when(value>1 ~ value/100, 
                           TRUE ~ value))

gg_poverty_lac <- ggfun_evo(pobreza_lac, pobreza_lac$year, pobreza_lac$value, pobreza_lac$pais) +
  xlab("Año") +
  ylab("Pporcentaje") +
  ggtitle(label = "Evolución de la pobreza en América Latina") +
  labs(caption = "Fuente: Encuestas de hogares de América Latina armonizadas. Disponibles en SCLData.
                  Nota: 1. El valor para 2021 es una proyección que se calcula utilizando el dato de 2020 y la tasa de crecimiento proyectada.
                  2. Para el cálculo de la pobreza siempre se utilizan los mismos 18 países. 
                  Para los años que no tienen información la pobreza se interpola linealmente o se proyecta con el método descrito en 1.")
  
print(gg_poverty_lac)

ggsave(plot = gg_poverty_lac, file = "Output/Poverty_evolution.jpg", height = 7, width = 10, units = "cm")
