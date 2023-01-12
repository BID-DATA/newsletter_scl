# -*- coding: utf-8 -*-
"""
Created on Thu Jan 12 10:14:26 2023

@author: MARIAREY
"""

##### Contexto #####

# El dato de este mes será elaborado para el blog de abierto al público. En este
# Script haré un paso a paso de cómo utilizar el paquete de SCLData en python (idbsocialdatapy)

# idbsocialdata es un conjunto de librerías en R, Python y Stata que facilita la consulta
# de indicadores sociales que produce el sector social del Banco Inter-Americano de Desarrollo
# El paquete inicialmente fue elaborado en R y traducido a Python.
# En el blog de abierto al público platicamos brevemente la historia de traducción del paquete.
# En este script enseño el paso a paso del uso del paquete en Python.

##### Preliminares #####

# En python el paquete debe de ser instalado en la consola/terminal a través de pip
# Esto lo puedes hacer con este comando: pip install idbsocialdatapy

# Importamos las librerías/paquetes que vamos a usar

import idbsocialdatapy as idb  # nuestro paquete para consultar indicadores
import pandas as pd  # análisis de datos

###### Metadata ######

# como nuestro paquete tiene más de 250 indicadores, primero consultamos el diccionario
# para determinar que es lo que queremos estudiar.

diccionario = idb.query_dictionary()

##### Data #####

# ahora que ya tenemos decidimos que queremos estudiar.
# En este caso nos interesa la tasa de pobreza en Bolivia, Colombia y México.
# Para la población indígena, afrodescendiente y no indígena ni afrodescendiente.
# Para el periodo de 2005 a 2020

# lo hacemos con la función query_indicator() indicando los siguientes parámetros.

pobreza = idb.query_indicator(
    indicator="pobreza",  # define indicador
    categories="ethnicity",  # define categoría
    countries="COL,MEX,BOL",  # definir país
    yearstart="2005",  # inicio
    yearend="2021"), #fin