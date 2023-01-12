**SCL Data - Data Ecosystem Working Group**

[![IDB Logo](https://scldata.iadb.org/assets/iadb-7779368a000004449beca0d4fc6f116cc0617572d549edf2ae491e9a17f63778.png)](https://scldata.iadb.org)

# Indicador mensual de SCLData
Este repositorio se utiliza para calcular y almacenar indicadores sociales de forma mensual. Para el cálculo se utilizan principalmente los datos de [SCLData](https://scldata.iadb.org/) a través del paquete [idbsocialdataR](https://github.com/EL-BID/idbsocialdataR).

## Descripción

Este repositorio contiene los scripts para el cálculo de los indicadores presentados en el newsletter mensual del Sector Social (SCL) del Banco Inter-Americano de Desarrollo (BID). 
El repositorio se actualiza mensualmente añadiendo el último dato calculado.

## Requisitos

**Paquetes necesarios para su funcionamiento**

- idbsocialdataR
- tidyverse

## Contribución

Hay dos formas de contribuir con nosotros. 

1. Si encuentras algún dato o pregunta que te parezca relevante para el desarrollo de América Latina y el Caribe puedes mandarlo como [Issue](https://github.com/BID-DATA/newsletter_scl/issues) y lo incluiremos en una de las siguientes ediciones.
2. Podrías ir más allá y construir algún indicador relevante para la región, utilizando nuestro paquete de R y mandar tu script para que lo revisemos. 

## Indicadores mensuales

En esta sección presentamos el resumen de todos los indicadores construidos hasta el momento. 

| Mes | Código | Indicador | SCLData
| :---: | :--- | :--- | :--- | 
| Junio 22 |[Script Junio 2022](https://github.com/BID-DATA/newsletter_scl/blob/main/monthly%20figure_june22.R)|[Evolución pobreza América Latina (1997 vs 2021)](https://github.com/BID-DATA/newsletter_scl/blob/main/Output/Poverty_evolution.jpg)|[Link al indicador](https://scldata.iadb.org/en/public/query-builder?collections=Household+Socio-Economic+Surveys&indicators=pobreza) | 
| Julio 22 |[Script Julio 2022](https://github.com/BID-DATA/newsletter_scl/blob/main/monthly%20figure_jul22.R)|[Tasa de desocupación por género (último dato disponible)](https://github.com/BID-DATA/newsletter_scl/blob/main/Output/Unemployment_rate_gender.jpg)|[Link al indicador](https://scldata.iadb.org/es/public/query-builder?indicators=tasa_desocupacion&categories=sex) | 
| Agosto 22 |[Script Agosto 2022](https://github.com/BID-DATA/newsletter_scl/blob/main/monthly%20figure_aug22.R)|[Tasa de empleadores en ocupación principal por raza)](https://github.com/BID-DATA/newsletter_scl/blob/main/Output/Employers-race.jpg)|[Link al indicador](https://scldata.iadb.org/es/public/query-builder?indicators=tasa_desocupacion&categories=sex) | 
| Septiembre 22 |[Script Septiembre 2022](https://github.com/BID-DATA/newsletter_scl/blob/main/monthly%20figure_sept22.R)|[Tasa de participación por género (1990-2020)](https://github.com/BID-DATA/newsletter_scl/blob/main/Output/Participation_rate_lac.jpg)|[Link al indicador](https://scldata.iadb.org/es/public/query-builder?indicators=tasa_participacion&categories=sex) | 
| Enero 23 | [Script Enero 2023](https://github.com/BID-DATA/newsletter_scl/blob/main/monthly%20figure_jan23.R)|[Pobreza por etnicidad (2005-2021)](https://github.com/BID-DATA/newsletter_scl/blob/main/Output/pobreza-race.jpg) | [Link al indicador](https://scldata.iadb.org/es/public/query-builder?indicators=pobreza&categories=ethnicity&countries=BOL%2CCOL%2CMEX) |

