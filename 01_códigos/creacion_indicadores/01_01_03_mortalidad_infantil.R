#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Mortalidad infantil
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          27 de octubre de 2021
# Última actualización:       26 de octubre de 2022
#------------------------------------------------------------------------------#

# Fuente de mortalidad: https://www.inegi.org.mx/sistemas/olap/proyectos/bd/continuas/mortalidad/mortalidadgeneral.asp?s=est&c=11144&proy=mortgral_mg#
# Fuente para nacimientos: https://www.inegi.org.mx/sistemas/olap/proyectos/bd/continuas/natalidad/nacimientos.asp?s=est&c=23699&proy=nat_nac


# Tener cuidado a la hora de importar los tabulados para que queden: 
# Año de registro como primera columana, y año de registro como segunda, 
# y las entidades como el resto de las columnas. 

# Nota: Cuando se importan los tabulados del INEGI, se descargan de una manera
# dañada. Para solucionar el problema hay que abrirlos y volverlos a guardar
# con extensión .xlsx en vez de .xls. Esto se hace de manera manual y luego se
# importan a R. 

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

# Vectores para directorio 
inp <- "02_datos_crudos/01_01_mortalidad/"


# Funciones para directorio 
paste_inp <- function(x){paste0("02_datos_crudos/01_01_mortalidad/"    , x)}
paste_out <- function(x){paste0("02_bases_procesadas/00_01_mortalidad/", x)}


# Activar las credenciales de google
v_usuaria <- "regina"
# v_usuaria <- "katia"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x))}


# 1. Importar datos ------------------------------------------------------------

# Muertes infantiles 
df_crudo <- read_excel(
    paste_inp("INEGI_exporta_defunciones_infantiles.xlsx"), skip = 4)

# Nacimientos 
df_crudo_nac <- read_excel(paste_inp("INEGI_exporta_nacimientos.xlsx"), skip = 4)

# 2. Procesamiento de datos ----------------------------------------------------

# Guardar nombres de las variables
v_names     <- names(df_crudo)

# Renombrar y cambiar formato
df_limpio   <- df_crudo                         %>% 
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
    summarise(total = sum(total, na.rm = TRUE)) %>% 
    ungroup()                                   %>% 
    rename(anio = anio_ocurrencia)              %>% 
    # Agregar variables de identificación 
    mutate(
        id_dimension = "01", 
        id_indicador = "03") %>% 
    filter(anio %in% c(1990:2022))



# Renombrar entidades
df_entidad  <- df_limpio        %>% 
    left_join(
        readxl::read_excel("02_datos_crudos/00_cve_ent.xlsx") %>% 
            rename(ent = entidad, entidad = entidad_comp)
    ) %>% 
    # Seleccionar variables finales 
    select(cve_ent, entidad_abr_m, anio, id_dimension, id_indicador, total)


# Procesar datos de nacimientos 
v_names         <- names(df_crudo_nac)

df_nacimientos  <- df_crudo_nac                                     %>% 
    rename(
        # Distinguir entre año de registro y año de ocurrencia
        anio_registro   = v_names[1], 
        anio_ocurrencia = v_names[2],
        # Columna con nivel nacional
        Nacional        = v_names[3])                               %>% 
    pivot_longer(
        cols      = -c(anio_registro, anio_ocurrencia), 
        names_to  = "entidad", 
        values_to = "total")                                        %>% 
    mutate(nacimientos = as.numeric(str_remove_all(total, ",")))    %>% 
    rename(anio = anio_ocurrencia)                                  %>% 
    group_by(anio, entidad)                                         %>% 
    summarise(nacimientos = sum(nacimientos, na.rm = TRUE))         %>% 
    filter(anio %in% c(1990:2022))                                  %>% 
    left_join(
        readxl::read_excel("02_datos_crudos/00_cve_ent.xlsx") %>% 
            rename(ent = entidad, entidad = entidad_comp)
    )

# Agregar número de nacimientos  
df_final    <- df_entidad                                       %>% 
    left_join(df_nacimientos, 
              by = c("cve_ent", "entidad_abr_m", "anio"))       %>% 
    filter(anio %in% 2000:2021)                                 %>% 
    mutate(         
        indicador_value = total*100000/nacimientos)             %>% 
    arrange(anio, cve_ent)                                      %>% 
    # Seleccionar variables finales 
    select(
        cve_ent, entidad_abr_m, anio, id_dimension, id_indicador, indicador_value) %>% 
    drop_na(cve_ent)


# 2. Guardar -------------------------------------------------------------------

## 2.1. Guardar copia con procesamiento anterior -------------------------------

# # Base con año de registro (se guardaba con el código previo)
# df_prev_mortalidad_infantil <- df_final 
# 
# save(df_prev_mortalidad_infantil, 
#      file = paste_out("df_prev_mortalidad_infantil.RData"))

# Base con año de ocurrencia 
df_post_mortalidad_infantil <- df_final 

save(df_post_mortalidad_infantil, 
     file = paste_out("df_post_mortalidad_infantil.RData"))

## 2.2. Guardar en drive -------------------------------------------------------

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1128387096")[1, 2])

# Guardar en la base en el Drive
googlesheets4::range_write(ss = v_id, data = df_final,
    sheet = "01_03_mortalidad_infantil")

# FIN. -------------------------------------------------------------------------
