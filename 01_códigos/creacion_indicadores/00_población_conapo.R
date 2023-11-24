#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Proyecciones de población CONAPO
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          27 de octubre de 2021
# Última actualización:       06 de octubre de 2022
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
require(pacman)
p_load(readxl, tidyverse, dplyr, googledrive, 
       googlesheets4, lubridate, janitor, beepr)

# Desactiva notación científica
options(scipen=999)

# Vaciar espacio de trabajo 
rm(list=ls())

# Colores MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados

# Activar las credenciales de google
v_usuaria <- "regina"
# v_usuaria <- "katia"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x))}


# Funciones de importación y exportación
paste_inp <- function(x){paste0("02_datos_crudos/", x)}
paste_out <- function(x){paste0("05_infobites/"   , x)}


# 1. Cargar datos --------------------------------------------------------------

# Cargar proyecciones de  CONAPO (preprocesadas por Dr. Alarid - PADECI)

load(paste_inp("df_pop_state.RData"))

# 2. Procesar datos ------------------------------------------------------------

df_data <- df_pop_state                                             %>% 
    # Desagrupar para poder seleccionar variables sin que meta STATE
    ungroup()                                                       %>% 
    # Seleccionar variables de interés y renombrar 
    select(anio = year, cve_ent = CVE_GEO, pob_tot = population)    %>% 
    # Filtrar y dejar solo años de interés 
    filter(anio %in% c(2010:2025))                                  %>% 
    # Cambiar el identificador de la entidad para que sea compatible con el 
    # resto de bases y así tenga numeración "01" en vez de "1"
    mutate(cve_ent = str_pad(cve_ent, width = 2, pad = "0")) %>% 
    arrange(anio, cve_ent)


# 3. Guardar en drive ----------------------------------------------------------

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1859408845")[1, 2])

# Guardar en la base en el Drive
googlesheets4::range_write(ss = v_id, data = df_data,
                           sheet = "99_pobtot")

# FIN. -------------------------------------------------------------------------
