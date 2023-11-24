#------------------------------------------------------------------------------#
# Proyecto:             ÍNDICE DE PROGRESO SOCIAL
# Objetivo:             Hogares con titulo de propiedad (%)
#                       
# 
# Encargada:            Sandra Martínez Peña     
# Correos:              sandra@mexicocomovamos.mx
# 
# Fecha de creación:    28 de septiembre de 2023
# Última actualización: 28 de septiembre de 2023
#------------------------------------------------------------------------------#

#Creado tomando como base en el código elaborado por Axel Gómez (axel@mexicocomovamos.mx)

# Fuente: Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH). 2022 
#https://www.inegi.org.mx/programas/enigh/nc/2022/

#Descripción de la base de datos - https://www.inegi.org.mx/app/biblioteca/ficha.html?upc=889463901242

#Pregunta textual: "¿Esta vivienda cuenta con escrituras o título de propiedad?"
#1 A nombre del dueño
#2 A nombre de otra persona
#3 No tiene escrituras
#4 No sabe

# 0. Configuración inicial -----------------------------------------------------

# Especificar zona horaria e idioma
Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
require(pacman)
p_load(readxl, tidyverse, dplyr, googledrive, 
       googlesheets4, lubridate, janitor, beepr, foreign, srvyr)

# Desactiva notación científica
options(scipen=999)

# Vaciar espacio de trabajo 
rm(list=ls())

# Funciones para directorio 
paste_inp <- function(x){paste0("02_datos_crudos/03_35_hogares_titulo_propiedad/", x)}

# Activar las credenciales de google
v_usuaria <- "sandra"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 


# 1. Importar datos ------------------------------------------------------------

df_crudo_viv <- read_csv(paste_inp("viviendas.csv"))
df_crudo_hog <- read_csv(paste_inp("hogares.csv"))

# 2. Procesamiento de datos ----------------------------------------------------

#Escrituras
df_viv <- df_crudo_viv %>%
    mutate(escrituras_2 = case_when(
        #1 - a nombre del dueño
        escrituras == 1 ~ 1,
        #2 - a nombre de otra persona
        escrituras == 2 ~ 1,
        #3- no tiene escrituras
        escrituras == 3 ~ 0,
        #4 - no sabe
        escrituras == 4 ~ 3
    ))

#Base provisional viviendad
df_viv <- df_viv %>%
    select(folioviv, escrituras, escrituras_2, factor)

#Unir base vivienda y hogares con base en "folioviv"
df_data_join <- df_crudo_hog %>%
    left_join(df_viv, by=c("folioviv", "factor"))


df_data <- df_data_join                                                         %>%
    #Obtener la clave de entidad de la variable folioviv
    mutate(cve_ent = case_when(nchar(folioviv)==9 ~ substr(folioviv,1,1),
                               TRUE ~substr(folioviv,1,2)),
           #Generar identificador abreviado 
           entidad_abr_m = case_when(
               cve_ent == "01" ~ "AGS", 
               cve_ent == "02" ~ "BC",
               cve_ent == "03" ~ "BCS",
               cve_ent == "04" ~ "CAMP",
               cve_ent == "05" ~ "COAH",
               cve_ent == "06" ~ "COL", 
               cve_ent == "07" ~ "CHPS",
               cve_ent == "08" ~ "CHIH",
               cve_ent == "09" ~ "CDMX",
               cve_ent == "10" ~ "DGO",
               cve_ent == "11" ~ "GTO",
               cve_ent == "12" ~ "GRO", 
               cve_ent == "13" ~ "HGO",
               cve_ent == "14" ~ "JAL",
               cve_ent == "15" ~ "MEX",
               cve_ent == "16" ~ "MICH",
               cve_ent == "17" ~ "MOR", 
               cve_ent == "18" ~ "NAY",
               cve_ent == "19" ~ "NL",
               cve_ent == "20" ~ "OAX",
               cve_ent == "21" ~ "PUE",
               cve_ent == "22" ~ "QRO", 
               cve_ent == "23" ~ "QROO",
               cve_ent == "24" ~ "SLP",
               cve_ent == "25" ~ "SIN",
               cve_ent == "26" ~ "SON",
               cve_ent == "27" ~ "TAB", 
               cve_ent == "28" ~ "TAM",
               cve_ent == "29" ~ "TLAX",
               cve_ent == "30" ~ "VER",
               cve_ent == "31" ~ "YUC",
               cve_ent == "32" ~ "ZAC")) %>% 
    # Agregar identificador del indicador
    mutate(id_dimension = "03",
           id_indicador = "35", 
           anio = 2022)  

#Hogares con escrituras
df_escrituras <- df_data                                               %>% 
    drop_na(escrituras)                                                %>% 
    filter(escrituras_2 < 2)                                           %>% 
    as_survey_design(weights = factor)                                 %>% 
    group_by(cve_ent, entidad_abr_m, anio, id_dimension, id_indicador) %>% 
    summarise(
        indicador_value = survey_mean(escrituras_2, na.rm = T)*100
    )                                                                  %>% 
    select(-ends_with("se"))                  


# 3. Guardar en drive ----------------------------------------------------------

# Obtener identificador de la base del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=39812926")[1, 2])

# Guardar en la base de Drive
googlesheets4::sheet_append(ss = v_id, data = df_escrituras,
                            sheet = "03_35_hogares_titulo_propiedad")

# FIN. -------------------------------------------------------------------------