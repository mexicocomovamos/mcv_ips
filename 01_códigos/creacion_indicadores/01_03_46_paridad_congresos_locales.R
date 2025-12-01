#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Paridad de género en congresos locales
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          27 de septiembre de 2021
# Última actualización:       10 de septiembre de 2024
#------------------------------------------------------------------------------#

# Fuente: en esta liga se comparte la fuente de datos para las elecciones 2024: 
#https://docs.google.com/spreadsheets/d/1eC3rZ2IrEJTA7R92m3iBZ2EN7nZhUjpygPerxu1ZF8w/edit?usp=sharing

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
require(pacman)
p_load(readxl, tidyverse, dplyr, googledrive, googlesheets4, lubridate, janitor, beepr)

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
inp <- "02_datos_crudos/03_46_paridad_congresos_locales/"

# Activar las credenciales de google
v_usuaria <- "sandra"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x))}
# 1. Procesamiento de datos ----------------------------------------------------

# Ya fue actualizado hasta 2024
df_crudo    <- read_excel(paste0(inp, "paridad_congresos_locales_nuevo.xlsx"))

df_crudo <- df_crudo %>% 
    arrange(cve_ent, anio)

#openxlsx::write.xlsx(df_crudo, paste0(inp, "paridad_congresos_locales2.xlsx"))

# Procesamiento de la info 
df_limpio   <- df_crudo         %>% 
    arrange(cve_ent, anio)      %>% 
    mutate(
        cve_ent = str_pad(cve_ent, 2, pad = "0"), 
        # indicador_value = abs((num_hombres/num_mujeres)-1), 
        indicador_value = num_mujeres/(num_hombres+num_mujeres), 
        id_dimension = "03", 
        id_indicador = "46")   %>% 
    distinct(cve_ent, ent, anio, id_dimension, id_indicador, indicador_value) %>% 
    # Seleccionar variables finales 
    select(cve_ent, entidad_abr_m = ent, anio, id_dimension, id_indicador, indicador_value) %>% 
    filter(anio >= 2015) %>% 
    distinct()

# Ver entidades y años faltantes 
table(df_limpio$entidad_abr_m, df_limpio$anio)
table(df_limpio$cve_ent, df_limpio$anio)


# 2. Guardar en drive ----------------------------------------------------------

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1128387096")[1, 2])

# Guardar en la base en el Drive
googlesheets4::range_write(ss = v_id, data = df_limpio,
    sheet = "03_46_participacion_mujeres_congresos_locales")


# FIN. -------------------------------------------------------------------------

