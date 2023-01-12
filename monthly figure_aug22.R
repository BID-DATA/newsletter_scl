##### Revisión de dato mensual SCLData - Agosto

#### preliminares ######

# add functions to do graphs
source("Indicadores funciones.R")

#remove scientific notation
options(scipen = 999)

#define colors
colors_sex <- c('#176A7B', '#C8DA92')

#### Leer datos SCLData ####

#query dictionary of indicators
dictionary <- idbsocialdataR:::query_dictionary() %>% select(collection, indicator, label_es, label_en)

# query indicator from package
tasa_patrones <- idbsocialdataR::query_indicator(indicator = 'tasa_patrones', 
                                                     categories = 'sex,ethnicity,age') 

##### Gráficas ####

table_evo <- tasa_patrones %>% 
  # change woman - women/man- men
  mutate(sex = case_when(sex == "woman" ~ "women", 
                         sex == "man" ~ "men", 
                         TRUE ~ sex)) %>% 
  # change ethnicity names for graph
  mutate(ethnicity= case_when(ethnicity == "Afro" ~ "Afro-Descendant", 
                              ethnicity == "Indi" ~ "Indigenous Population",
                              ethnicity == "Otro" ~ "Non-AD/IP",
                              TRUE ~ ethnicity)) %>% 
  # filte rpopulation 25_64
  filter(age == "25_64")

plot_aux <- table_evo %>%
  # keep only countries with Afro-descendant population in their surveys
  filter(isoalpha3 %in% c('BRA', 'COL', 'ECU', 'URY')) %>%
  # filter only non-AD/IP and AD population
  filter(sex != "Total" & ethnicity != "Total" & ethnicity!="Indigenous Population")

# use function from Indicadores funciones
plot_evo <- gg_evo_eth(plot_aux, plot_aux$year, plot_aux$value, plot_aux$ethnicity) +
  facet_grid(sex~isoalpha3, scales = "free_y", switch = "y") +
  labs(title = "Employers in the main activity (in%), by race", 
       size = 4)

print(plot_evo)

# Save plot
ggsave(plot = plot_evo, file = "Output/Employers-race.jpg", height = 10, width = 12, units = "cm")
