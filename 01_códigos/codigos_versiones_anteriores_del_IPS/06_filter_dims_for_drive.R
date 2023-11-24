#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Mortalidad materna
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          19 de noviembre de 2021
# Última actualización:       19 de noviembre de 2021
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

# Vectores para directorio 
inp <- "03_ips_clean/"

# Activar las credenciales de google
googledrive::drive_auth("regimedina19@gmail.com")
googlesheets4::gs4_auth("regimedina19@gmail.com")

# googledrive::drive_auth("katia@mexicocomovamos.mx")
# googlesheets4::gs4_auth("katia@mexicocomovamos.mx")

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x))}


# 1. Importar datos ------------------------------------------------------------

# Dimensiones del IPS
df_crudo <- read_excel(paste0(inp, "00_IPS_COMPLETE_LONG.xlsx"))


# 2. Filtrar por dimensiones para google_sheet
df_dim1 <- df_crudo %>% 
    filter(id_dim == "01", anio == 2020, cve_ent != "00") %>% 
    mutate(cve_ent = as.numeric(cve_ent))

df_dim2 <- df_crudo %>% 
    filter(id_dim == "02", anio == 2020, cve_ent != "00")  %>% 
    mutate(cve_ent = as.numeric(cve_ent))

df_dim3 <- df_crudo %>% 
    filter(id_dim == "03", anio == 2020, cve_ent != "00")  %>% 
    mutate(cve_ent = as.numeric(cve_ent))



# 2. Guardar en drive ----------------------------------------------------------

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1oMCDwF8T0pfuuBLMN5BkNS7lBiZF4d0KxDVs76VkPac/edit#gid=0")[1, 2])

# Guardar en la base en el Drive
googlesheets4::range_write(ss = v_id, data = df_dim1,
    sheet = "04_mapa_dim1")

googlesheets4::range_write(ss = v_id, data = df_dim2,
    sheet = "05_mapa_dim2")

googlesheets4::range_write(ss = v_id, data = df_dim3,
    sheet = "06_mapa_dim3")


# FIN. -------------------------------------------------------------------------
