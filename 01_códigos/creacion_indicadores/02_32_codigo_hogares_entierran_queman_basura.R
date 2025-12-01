
#------------------------------------------------------------------------------#
# Proyecto:             ÍNDICE DE PROGRESO SOCIAL
# Objetivo:             Calcular hogares que entierran o queman basura (%)
#                       
# 
# Encargada:            Sandra Martínez Peña     
# Correos:              sandra@mexicocomovamos.mx
# 
# Fecha de creación:    15 de septiembre de 2025
# Última actualización: 15 de septiembre de 2025
#------------------------------------------------------------------------------#

#Creado tomando como base en el código elaborado por Axel Gómez (axel@mexicocomovamos.mx)

# Fuente: Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH). 2024 
#https://www.inegi.org.mx/programas/enigh/nc/2024/#microdatos
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

# Funciones para directorio - se jalan los datos de esta carpeta
paste_inp <- function(x){paste0("02_datos_crudos/03_35_hogares_titulo_propiedad/", x)}

# Activar las credenciales de google
v_usuaria <- "sandra"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 


# 1. Importar datos ------------------------------------------------------------

# Función para procesar un año específico
procesar_anio_basura <- function(anio) {
    
    # Leer datos de viviendas
    df_viviendas <- read_csv(paste_inp(paste0("viviendas_", anio, ".csv")))
    
    # Leer datos de concentrado hogar
    df_hogares <- read_csv(paste_inp(paste0("concentradohogar_", anio, ".csv")))
    
    # 2. Procesamiento de datos ----------------------------------------------------
    
    # Procesar viviendas
    df_viviendas_proc <- df_viviendas %>%
        mutate(
            folioviv = str_pad(folioviv, width = 10, side = "left", pad = "0"),
            eli_basura = case_when(
                eli_basura == 1 ~ "La recoge un camión o carrito de basura",
                eli_basura == 2 ~ "La tiran en el basurero público",
                eli_basura == 3 ~ "La tiran en un contenedor o depósito",
                eli_basura == 4 ~ "La queman",
                eli_basura == 5 ~ "La entierran",
                eli_basura == 6 ~ "La tiran en un terreno baldío o calle",
                eli_basura == 7 ~ "La tiran en barranca o grieta",
                eli_basura == 8 ~ "La tiran al río, lago o mar"
            )
        ) %>%
        select(folioviv, eli_basura)
    
    # Procesar hogares
    df_hogares_proc <- df_hogares %>%
        mutate(
            folioviv = str_pad(folioviv, width = 10, side = "left", pad = "0"),
            ID = paste(folioviv, foliohog, sep = ".")
        ) %>%
        select(folioviv, foliohog, factor, upm, est_dis, ID)
    
    # Combinar datos
    df_data <- left_join(df_viviendas_proc, df_hogares_proc, by = "folioviv") %>%
        # Obtener la clave de entidad de la variable folioviv
        mutate(cve_ent = case_when(nchar(folioviv)==9 ~ substr(folioviv,1,1),
                                   TRUE ~substr(folioviv,1,2)),
               # Generar identificador abreviado 
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
        mutate(id_dimension = "02",
               id_indicador = "32", 
               anio = anio)
    
    # Generar variable de hogares que queman/entierran basura
    df_basura <- df_data %>%
        mutate(queman_entierran = case_when(
            eli_basura %in% c("La queman", "La entierran") ~ 1,
            TRUE ~ 0
        ))
    
    # Hogares que queman/entierran basura nacional 
    df_basura_nac <- df_basura %>% 
        drop_na(queman_entierran) %>% 
        as_survey_design(weights = factor) %>% 
        group_by(anio, id_dimension, id_indicador) %>% 
        summarise(
            indicador_value = survey_mean(queman_entierran, na.rm = T)*100
        ) %>% 
        select(-ends_with("se")) %>% 
        mutate(
            cve_ent = "00",
            entidad_abr_m = "Nacional"
        )
    
    # Hogares que queman/entierran basura por entidad federativa
    df_basura_ent <- df_basura %>% 
        drop_na(queman_entierran) %>% 
        as_survey_design(weights = factor) %>% 
        group_by(cve_ent, entidad_abr_m, anio, id_dimension, id_indicador) %>% 
        summarise(
            indicador_value = survey_mean(queman_entierran, na.rm = T)*100
        ) %>% 
        select(-ends_with("se"))
    
    # Unir datos nacionales y por entidad
    df_basura_final <- bind_rows(df_basura_ent, df_basura_nac)
    
    return(df_basura_final)
}

# Procesar para 2024
anios <- c(2024)
df_basura_completo <- map_dfr(anios, procesar_anio_basura)

# 3. Guardar en drive ----------------------------------------------------------

# Obtener identificador de la base del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1556525340")[1, 2])

# Guardar en la base de Drive
googlesheets4::sheet_append(ss = v_id, data = df_basura_completo,
                            sheet = "02_32_enterrar_quemar_basura")

# FIN. -------------------------------------------------------------------------




