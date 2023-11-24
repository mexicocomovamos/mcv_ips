#------------------------------------------------------------------------------#
# Proyecto:             ÍNDICE DE PROGRESO SOCIAL
# Objetivo:             Calcular hogares que usan focos ahorradores (%)
#                       
# 
# Encargada:            Sandra Martínez Peña     
# Correos:              sandra@mexicocomovamos.mx
# 
# Fecha de creación:    27 de septiembre de 2023
# Última actualización: 27 de septiembre de 2023
#------------------------------------------------------------------------------#

#Creado tomando como base en el código elaborado por Axel Gómez (axel@mexicocomovamos.mx)

# Fuente: Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH). 2022 
#https://www.inegi.org.mx/programas/enigh/nc/2022/


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
paste_inp <- function(x){paste0("02_datos_crudos/02_34_focos_ahorradores/", x)}

# Activar las credenciales de google
v_usuaria <- "sandra"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 


# 1. Importar datos ------------------------------------------------------------

df_crudo <- read_csv(paste_inp("viviendas.csv"))


# 2. Procesamiento de datos ----------------------------------------------------

df_data <-df_crudo                                                         %>%
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
               cve_ent == "32" ~ "ZAC"))                             %>% 
    # Agregar identificador del indicador
    mutate(id_dimension = "02",
           id_indicador = "34", 
           anio = 2022)  

#Generamos la variable de  focos ahorradores
df_focos <-df_data                                                    %>%
    mutate(focos = case_when(focos_ahor >= 1 ~ 1,
                             focos_ahor == 0 ~ 0))

#Focos ahorradores por entidad federativa
df_focos <- df_focos                                                  %>% 
    drop_na(focos)                                                    %>% 
    as_survey_design(weights = factor)                                %>% 
    group_by(cve_ent, entidad_abr_m, anio, id_dimension,id_indicador) %>% 
    summarise(
        indicador_value = survey_mean(focos, na.rm = T)*100
    )                                                                 %>% 
    select(-ends_with("se"))                    


# 3. Guardar en drive ----------------------------------------------------------

# Obtener identificador de la base del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1556525340")[1, 2])

# Guardar en la base de Drive
googlesheets4::sheet_append(ss = v_id, data = df_focos,
                            sheet = "02_34_uso_focos_ahorradores")

# FIN. -------------------------------------------------------------------------
