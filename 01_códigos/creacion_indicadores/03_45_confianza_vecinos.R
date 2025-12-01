#------------------------------------------------------------------------------#
# Proyecto:             ÍNDICE DE PROGRESO SOCIAL
# Objetivo:             Confianza en los vecinos (% de la población)
#                       
# 
# Encargada:            Sandra Martínez Peña     
# Correos:              sandra@mexicocomovamos.mx
# 
# Fecha de creación:    13 de septiembre de 2023
# Última actualización: 13 de septiembre de 2023
#------------------------------------------------------------------------------#

# Fuente: Encuesta Nacional de Victimización y Percepción sobre Seguridad Pública
#(ENVIPE) 2022 https://www.inegi.org.mx/programas/envipe/2022/
#También se encuentra en tabulados (VI Desempeño Institucional) 

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

# Vectores para directorio 
inp <- "02_datos_crudos/03_45_confianza_vecinos/"

# Activar las credenciales de google
v_usuaria <- "sandra"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# 1. Importar datos ------------------------------------------------------------
df_crudo <- read_csv(paste0(inp,"TPer_Vic1.csv"), 
                     locale = locale(encoding = "ISO-8859-1"))

# 2. Procesamiento de datos ----------------------------------------------------

# limpiar
df_crudo     <- clean_names(df_crudo)

df_data <- df_crudo                                                         %>%
  select(cve_ent,
         entidad_abr_m = nom_ent,
         #pregunta
         ap5_2_1, 
         factor = fac_ele)                                                  %>%
    mutate(factor = as.numeric(factor))                                     %>% 
  #Filtramos para no seleccionar respuestas "No aplica = 5 y "NS/NR = 9"
  filter(!ap5_2_1 %in% c(5,9))                                              %>% 
    #ponderador 
  as_survey_design(weights = factor)                                        %>% 
  group_by(entidad_abr_m, ap5_2_1, cve_ent)                                 %>% 
    summarise(n= survey_total())                                            %>% 
  ungroup()                                                                 %>% 
  group_by(entidad_abr_m, cve_ent)                                          %>% 
  #calcular proporción
  mutate(prop_respuesta = n/sum(n))                                         %>% 
  #Mantener sólo respuestas "Mucha" y "Alguna" 
  filter(ap5_2_1 %in% c(1,2))                                               %>% 
  #proporción Mucha y Alguna 
  summarise(indicador_value = formatC(sum(prop_respuesta*100), 
                                      digits = 1,
                                      format = "f"
                                      ))                                    %>%
  # Agregar identificador del indicador
  mutate(id_dimension = "03",
         id_indicador = "45", 
         anio = 2024)                                                       %>%
  # Generar identificador abreviado 
  mutate(
    entidad_abr_m = case_when(
      cve_ent == "00" ~ "Nacional", 
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
      cve_ent == "32" ~ "ZAC"))                                             %>% 
  # Seleccionar variables finales 
  select(cve_ent, entidad_abr_m, anio, id_dimension, id_indicador, indicador_value) 

df_data
# 3. Guardar en drive ----------------------------------------------------------

# Obtener identificador de la base del IPS 
v_id <- as.character(
  googledrive::drive_get(
    "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1168077137")[1, 2])

# Guardar en la base de Drive
googlesheets4::sheet_append(ss = v_id, data = df_data,
                           sheet = "03_45_confianza_vecinos")

# FIN. -------------------------------------------------------------------------

