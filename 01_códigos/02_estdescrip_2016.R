#------------------------------------------------------------------------------#
# Proyecto:                   Índice de Progreso Social 
# Objetivo:                   Estimar estadística descriptiva de 2016
#
# Encargadas:     
# Correos:                    
# 
# Fecha de creación:          15 de septiembre de 2021
# Última actualización:       15 de septiembre de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
require(pacman)
p_load(tidyverse, dplyr, googledrive, googlesheets4, beepr)

# Desactiva notación científica
options(scipen=999)

# Vaciar espacio de trabajo 
rm(list=ls())

# 1. Importar datos ------------------------------------------------------------