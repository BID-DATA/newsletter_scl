##### Revisión de dato mensual SCLData - Julio

#### preliminares ######

library(tidyverse)
library(idbsocialdataR)
library(padr)
library(lubridate)
library(imputeTS)

options(scipen = 999)

colors_sex <- c('#176A7B', '#C8DA92')

source('Indicadores funciones.R')

#### Leer datos SCLData ####

# Dictionary indicators
dictionary <- idbsocialdataR:::query_dictionary() %>% select(collection, indicator, label_es, label_en)

# Query participation rate for all countries
tasa_participacion <- scldataR::query_indicator(indicator = c('tasa_participacion_PHC', 'tasa_participacion'),
                                                countries = 'All',
                                                categories = 'sex,area,age', 
                                                yearstart = 1960,
                                                yearend = 2021) 


##### Función fill #####
#Create function to fill all years/countries
query_indicator_fill <- function(dat, yearstart, yearend, model='kalman', forcast=1){
  
  data_raw <- dat %>%
    rename(y=value, ds=dt) 
  
  data <- data_raw %>%
    group_by(isoalpha3) %>% 
    mutate(n = n()) %>%
    filter(n>4) %>% 
    select(indicator, label_en, label_es, 
           collection_es,collection_en,
           theme_es,theme_en,
           isoalpha3,
           country_name_en,country_name_es,
           ds, y, valuetype) %>% 
    mutate(ds = floor_date(ds, 'monthly')) %>%
    padr:::pad(group = c('isoalpha3','country_name_en',
                         'country_name_es', 'source'),
               by = 'ds',
               start_val=ymd(str_c(yearstart,'-01-01')),
               end_val = ymd(str_c(yearend,'-01-01'))) %>% 
    fill_by_prevalent(indicator, label_en, label_es,
                      collection_es,collection_en,
                      theme_es,theme_en, valuetype) %>% 
    mutate(year = lubridate::year(ds))
  
  if (model =='prophet') {
    nest_data <- data %>% 
      dplyr::group_by(isoalpha3) %>%
      nest() %>% 
      mutate(m = map(data, prophet, changepoint.prior.scale = .40)) %>%  
      mutate(future = map(m, make_future_dataframe, periods = forcast,freq = 'year')) %>% 
      mutate(forecast = map2(m, future, predict))
    
    predictions <- nest_data %>% 
      unnest(forecast) %>% 
      select(ds, isoalpha3, yhat) %>% ungroup()
    
    data_predicted <- data %>%
      full_join(predictions, on=c("ds", "isoalpha3")) %>% 
      mutate(predicted = ifelse(is.na(y),TRUE, FALSE),
             value =ifelse(is.na(y),yhat, y)) %>% 
      select(ds, isoalpha3,
             label_es, label_en,
             collection_es,collection_en,
             theme_es, theme_en,
             value, valuetype, predicted)
    
    data_out <- data_predicted %>% 
      left_join(countries, on='isoalpha3') %>% 
      mutate(value = ifelse(value>1,1,value)) %>% 
      select(ds, isoalpha3,
             label_es, label_en,
             collection_es,collection_en,
             theme_es, theme_en,
             value,valuetype, predicted)
    
  } else if (model =='kalman'){
    
    data_out <- data %>% 
      mutate(predicted = ifelse(is.na(y),TRUE, FALSE),
             yhat =  na_kalman(y, model = "StructTS", smooth = T, nit = -1),
             value = ifelse(is.na(y), yhat, y)) %>% 
      select(ds, isoalpha3,
             label_es, label_en,
             collection_es,collection_en,
             theme_es, theme_en,
             value, valuetype, predicted, year, indicator)
  }
  data_out <- data_out %>%
    return(data_out)
}

##### Elaboración data LAC #####
#Define period
yearstart='1995'
yearend='2020'

#Select only countries that have censuses for all decades (to have comparable sample)
paises_censos <- c('ARG', 'BRA', 'CHL', 'COL', 'ECU', 'GTM', 'MEX', 'PAN', 'PRY', 'VEN', 'TTO')


# Filter calculations for women
tasa_part_woman <- tasa_participacion %>% 
  filter(age == "Total") %>% 
  filter(source != "IPUMS") %>% 
  select(dt, iddate, year, idgeo, isoalpha3, source, indicator, sex, area, value, se, cv, sample, label_en, label_es, collection_es,collection_en,
         theme_es,theme_en, valuetype, country_name_en,country_name_es) %>% 
 # rbind(tasa_participacion_old) %>% 
  filter(sex == "woman" & area == "Total") 

#Filter calculations for men
tasa_part_man <- tasa_participacion %>% 
  filter(age == "Total") %>% 
  filter(source != "IPUMS") %>% 
  select(dt, iddate, year, idgeo, isoalpha3, source, indicator, sex, area, value, se, cv, sample, label_en, label_es, collection_es,collection_en,
         theme_es,theme_en, valuetype, country_name_en,country_name_es) %>% 
#  rbind(tasa_participacion_old) %>% 
  filter(sex == "man" & area == "Total")

# Use created function to fill all years - women
tasa_part_filled_woman <- query_indicator_fill(dat = tasa_part_woman, yearstart = '1995', 
                                               yearend = '2020', model = 'kalman') %>% 
  mutate(sex = "woman") 

# Use created function to fill all years  - men
tasa_part_filled <- query_indicator_fill(dat = tasa_part_man, yearstart = '1995', 
                                         yearend = '2020', model = 'kalman') %>% 
  mutate(sex = "man") %>% 
# Bind women's dataframe
  rbind(tasa_part_filled_woman) %>% 
  select(-c(predicted))

# Filter only selected countries
tasa_part_hs <- tasa_part_filled %>% 
  filter(isoalpha3 %in% paises_censos) %>% 
  ungroup() %>%
  distinct() %>% 
  rbind(grouped_mean(., value, sex, year) %>% 
          select(-c('se', 'cv', 'sample', 'country_name_en', 'country_name_es', 'source_es', 'source_en', 'source'))) %>% 
  mutate(source = "Harmonized Household Surveys", 
         value = value/100) %>% 
  select(-ds)

#Select only censuses 
tasa_part_lac <- tasa_participacion %>%
  filter(source == "IPUMS" & area == "Total" & sex != "Total") %>% 
  filter(age == "15_64") %>% 
  select(-dt) %>%  
  filter(isoalpha3 %in% paises_censos) %>% 
  filter(year>=1970 & year<2010) %>% 
  filter(!(isoalpha3 == "MEX" & source == "IPUMS" & (year == "2005" | year == "1995"))) %>% 
  mutate(digit = as.numeric(str_sub(year, -1L)),
         year = ifelse(source == "IPUMS", ifelse(digit>5, plyr::round_any(year,10, f=ceiling), plyr::round_any(year,10, f=floor)), year)) %>% 
  select(indicator, source, label_en, label_es, 
         collection_es,collection_en,
         theme_es,theme_en,
         isoalpha3, value, sex, year, valuetype) %>% 
  rbind(grouped_mean(., value, sex, year) %>% 
          select(-c('se', 'cv', 'sample', 'country_name_en', 'country_name_es', 'source_es', 'source_en'))) %>% 
  # Bind household surveys data frame
  rbind(tasa_part_hs) %>% 
  group_by(isoalpha3, sex, indicator) %>% 
  mutate(label = case_when(year == max(year) ~ as.character(percent(value, accuracy = .1)), 
                           year == min(year) ~ as.character(percent(value, accuracy = .1)),
                           year == mean(year) ~ as.character(percent(value, accuracy = .1)),
                           year == 2019 ~ as.character(percent(value, accuracy = .1))))
# Reshape data
tasa_part_table <- tasa_part_lac %>% 
  pivot_wider(names_from = sex)

##### Gráficas ####

# Graph LAC series using censuses + household surveys
 tasa_part_lac_plot <- tasa_part_lac %>% 
  filter(isoalpha3 == "Promedio") %>% 
  # Define plot
  ggplot(aes(x = year, y = value)) +
  # Define line annd point layer add color
  geom_line(aes(color = indicator)) +
  geom_point(aes(color = indicator), size = .5) +
  # Add labels
  geom_label_repel(aes(x=year, y = value, label = label),
                   min.segment.length = unit(0, 'lines'), 
                   max.overlaps = 8, size = 2, family = 'Century Gothic') +
  # Make panels by sex
  facet_wrap(~sex) +
  # Add title
  labs(title = str_wrap(paste("Global participation rate (Censuses and Household Surveys)", "Latin America", sep = " - "), 70), 
       caption = str_wrap("Source: Integrated Public Use Microdata Series (IPUMS). 
                  Harmonized Household Surveys.
                  LAC average is computed as a simple average. 
                  For censuses we group available censuses by decade, and then compute LAC's average as a simple average for each decade with the available countries.
                  To have a balanced sample between countries, we use the same 11 countries throughout the period; using a lineal interpolation to complete missing points between years."),200) +
  # Add format
  theme(legend.position="bottom", 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.title=element_blank(),panel.background = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 6, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        legend.text = element_text(size = 6, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        panel.grid.major = element_line(colour = "#D3D3D3"),
        text = element_text(size = 6, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
  # Assign colors
  scale_color_manual(values = colors_sex) +
  # Change y axis to percent
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))

#Save plot
ggsave(plot = tasa_part_lac_plot, file = "Output/Participation_rate_lac.jpg", height = 8, width = 10, units = "cm")
