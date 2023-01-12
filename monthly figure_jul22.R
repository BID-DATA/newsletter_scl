##### Revisión de dato mensual SCLData - Julio

#### preliminares ######

source("Indicadores funciones.R")

options(scipen = 999)

colors_sex <- c('#176A7B', '#C8DA92')

#### Leer datos SCLData ####

dictionary <- idbsocialdataR:::query_dictionary() %>% select(collection, indicator, label_es, label_en)

tasa_desocupacion <- idbsocialdataR::query_indicator(indicator = 'tasa_desocupacion', 
                                                     categories = 'sex')

##### Gráficas ####

tasa_desocupacion_table <- tasa_desocupacion %>% 
  filter(sample>30) %>% 
  mutate(year = case_when(year == 2017 & isoalpha3 == "CHL" ~ 2019, 
                          year == 2018 & isoalpha3 == "MEX" ~2019, 
                          TRUE ~ year)) %>% 
  group_by(isoalpha3, sex) %>% 
  filter(sex!= 'Total') %>% 
  filter(year!= 2020) %>% 
  filter(year !=2021) %>% 
  slice(which(year == max(year))) %>% 
  ungroup() %>% 
  select(year, isoalpha3, sex, value, label_en, description_en) %>% 
  spread(sex, value) %>% 
  arrange(isoalpha3, year) %>% 
  mutate(gap = (man - woman)*100,
         gap_pos = woman +(gap/200)) 

tasa_desocupacion_plot <- ggplot(tasa_desocupacion_table %>%
                                group_by(isoalpha3) %>% 
                                slice(which.max(year)) %>% 
                                arrange(man)) +
  geom_segment(aes(x = fct_reorder(isoalpha3, woman), xend=fct_reorder(isoalpha3, woman), y=man, yend = woman), color = "dark grey") +
  geom_point(aes(x=isoalpha3, y=man, color = "man"),  size=1.5, alpha = .8) +
  geom_text(data  = tasa_desocupacion_table %>%
              group_by(isoalpha3) %>% 
              slice(which.min(year)),
            aes(x=isoalpha3, y=gap_pos, label=comma(gap,accuracy = .1)),
            family = 'Century Gothic', 
            nudge_x = .3,
            size = 2) +
  geom_point(aes(x=isoalpha3, y=woman, color = "woman"),  size=2, alpha = .8) +
  labs(title = "Difference in unemployment rate in Latin American countries by gender",
       caption = "Source: Inter-American Development Bank. Household Surveys Indicators of Latin America and the Caribbean. 
                  Gap is computed as the difference between male's and female's value in percentage points.
                  Figures are from the last available period before COVID.") +
  theme(legend.position="bottom", 
        axis.title.y = element_blank(),
        axis.title.x = element_blank(), 
        legend.title=element_blank(),panel.background = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.text = element_text(size = 6, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        legend.text = element_text(size = 6, color = "#000f1c", face = "bold", family = 'Century Gothic'),
        panel.grid.major = element_line(colour = "#D3D3D3"),
        text = element_text(size = 6, color = "#000f1c", face = "bold", family = 'Century Gothic')) +
  scale_color_manual(values = colors_sex) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  coord_flip()
  
print(tasa_desocupacion_plot)

ggsave(plot = tasa_desocupacion_plot, file = "Output/Unemployment_rate_gender.jpg", height = 10, width = 12, units = "cm")
