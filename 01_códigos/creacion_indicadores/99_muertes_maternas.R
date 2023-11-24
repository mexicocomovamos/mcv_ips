#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Mortalidad materna
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          24 de noviembre de 2021
# Última actualización:       24 de noviembre de 2022
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
# rm(list=ls())

# Colores MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados

# Funciones para directorio 
paste_inp <- function(x){paste0("02_datos_crudos/01_01_mortalidad/"    , x)}
paste_out <- function(x){paste0("02_bases_procesadas/00_01_mortalidad/", x)}

# ---- Activar las credenciales de google
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


# 1. Importar datos ------------------------------------------------------------

# Defunciones maternas totales
df_crudo_mort <- read_excel(
    paste_inp("INEGI_exporta_muertes_maternas_totales.xlsx"), skip = 4)

# Defunciones para calcular la razón de la mortalidad materna 
df_crudo    <- read_excel(paste_inp("INEGI_exporta_defunción_materna.xlsx"), 
                          skip = 3)
# Nacimientos (vivos)
df_crudo_nac <- read_excel(paste_inp("INEGI_exporta_nacimientos.xlsx"), 
                          skip = 4)

# 2. Procesamiento de datos ----------------------------------------------------

## 2.1. Muertes maternas totales -----------------------------------------------
# Guardar nombres de las variables
v_names     <- names(df_crudo)

# Renombrar y cambiar formato 
df_muertes_totales <- df_crudo_mort     %>% 
    rename(anio_ocurrencia = 1)         %>% 
    mutate_all(~as.numeric(str_remove_all(., ",")))          %>% 
    filter(anio_ocurrencia >= 1990)     %>% 
    pivot_longer(
        cols = -anio_ocurrencia, 
        names_to = "anio_registro", 
        values_to = "total"
    ) %>% 
    group_by(anio_ocurrencia) %>% 
    summarise(muertes_maternas_totales = sum(total, na.rm = T))

## 2.2. Razón de muerte materna ------------------------------------------------
# Guardar nombres de las variables
v_names     <- names(df_crudo)

# Renombrar y cambiar formato
df_razon_muerte_materna   <-  df_crudo          %>% 
    rename(
        # Distinguir entre año de registro y año de ocurrencia
        anio_registro   = v_names[1], 
        anio_ocurrencia = v_names[2],
        # Columna con nivel nacional
        Nacional        = v_names[3])           %>% 
    # Cambiar a formato largo 
    pivot_longer(
        cols      = -c(anio_registro, anio_ocurrencia), 
        names_to  = "entidad", 
        values_to = "total")                    %>% 
    # Cambiar a variable numérica 
    mutate(total = as.numeric(str_remove_all(total, ","))) %>% 
    # Agrupar por año de ocurrencia y estimar totales 
    group_by(entidad, anio_ocurrencia)          %>% 
    summarise(muertes_razon_mm = sum(total, na.rm = TRUE)) %>% 
    ungroup()                                   %>% 
    # Agregar variables de identificación 
    filter(anio_ocurrencia %in% c(1990:2022), 
           entidad == "Nacional") %>% 
    mutate(anio_ocurrencia = as.numeric(anio_ocurrencia))


## 2.3. Nacidos vivos ----------------------------------------------------------

# Procesar datos de nacimientos 
v_names         <- names(df_crudo_nac)

df_nacimientos  <- df_crudo_nac                                         %>% 
    rename( 
        # Distinguir entre año de registro y año de ocurrencia  
        anio_registro   = v_names[1],   
        anio_ocurrencia = v_names[2],   
        # Columna con nivel nacional    
        Nacional        = v_names[3])                                   %>% 
    pivot_longer(   
        cols      = -c(anio_registro, anio_ocurrencia),     
        names_to  = "entidad",  
        values_to = "total")                                            %>% 
    mutate(nacimientos = as.numeric(str_remove_all(total, ",")))        %>% 
    group_by(anio_ocurrencia, entidad)                                  %>% 
    summarise(nacimientos = sum(nacimientos, na.rm = TRUE))             %>% 
    filter(anio_ocurrencia %in% c(1990:2022), entidad == "Nacional")    %>% 
    mutate(anio_ocurrencia = as.numeric(anio_ocurrencia))

    

## 2.4. Pegar bases ------------------------------------------------------------

df_muertes_maternas <- df_nacimientos %>% 
    full_join(df_razon_muerte_materna, by = c("anio_ocurrencia", "entidad"))   %>% 
    full_join(df_muertes_totales, by = c("anio_ocurrencia")) %>% 
    select(entidad, anio_ocurrencia, muertes_maternas_totales, muertes_razon_mm, nacimientos)

# 3. Guardar base --------------------------------------------------------------

openxlsx::write.xlsx(df_muertes_maternas, paste_out("df_muertes_maternas.xlsx"))

# FIN. -------------------------------------------------------------------------
