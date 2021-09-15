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
# Activar las credenciales de google
googledrive::drive_auth("regimedina19@gmail.com")
googlesheets4::gs4_auth("regimedina19@gmail.com")

# Verificar credenciales (dirección de correo de intersecta) 
googledrive::drive_user()
googlesheets4::gs4_user() 

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1859408845")[1, 2])

# Función para importar de manera más corta desde drive
imp_dv <- function(x, y){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x), sheet = y)}

# 1.2. Cargar datos ------------------------------------------------------------

# Catálogo de bases de datos 
imp_dv(v_id, "99_pobtot")
