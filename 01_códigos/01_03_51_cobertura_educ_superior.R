#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Cobertura educación superior
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          27 de septiembre de 2021
# Última actualización:       27 de septiembre de 2021
#------------------------------------------------------------------------------#

# Fuente: https://www.planeacion.sep.gob.mx/principalescifras/
# Cada año corresponde a aquel en que terminó el ciclo escolar.  
# La cobertura incluye los posgrados 

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
inp <- "02_datos_crudos/03_53_paridad_educ/"

# Activar las credenciales de google
googledrive::drive_auth("regimedina19@gmail.com")
googlesheets4::gs4_auth("regimedina19@gmail.com")

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x))}

# 1. Procesamiento de datos ----------------------------------------------------

# Vectores de texto
v_tipo      <- c("edusuperior_")

v_time      <- c(
    "2009-2010", "2010-2011", "2011-2012", "2012-2013", 
    "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
    "2018-2019", "2019-2020", "2020-2021")

v_formato   <- c(".xlsx")

## 1.1. Limpieza de ensayo -----------------------------------------------------

# Cargar datos 
df_crudo    <- read_excel(paste0(inp, v_tipo[1], v_time[7], v_formato), skip = 4)
v_names     <- names(df_crudo)

# Renombrar variables
df_estudiantes <- df_crudo                                  %>%
    slice(1:33)                                             %>% 
    select(v_names[1], v_names[3])                          %>%
    rename(                 
        entidad       = v_names[1],                   
        estudiantes   = v_names[3])                         %>% 
    mutate(anio       = 2015)

## 1.2. Limpieza en bucle ------------------------------------------------------

# Vectores de texto
v_formato   <- c(".xlsx")
v_years     <- c(2010:2021)

# Base vacía 
df_unida <- data.frame(entidad = NA, estudiantes = NA, anio = NA)

for(i in 1:length(v_time)){
    
    df_crudo    <- read_excel(paste0(inp, v_tipo[1], v_time[i], v_formato), skip = 4)
    v_names     <- names(df_crudo)
    
    x <- ifelse(v_time[i] %in% v_time[1:5], 3, 4) 
    
    df_estudiantes <- df_crudo                                  %>%
        slice(1:33)                                             %>% 
        select(v_names[1],  v_names[x])                         %>%
        rename(
            entidad       = v_names[1], 
            estudiantes   = v_names[x])                         %>% 
        mutate(anio       = v_years[i])
    
    df_unida <- df_unida %>% bind_rows(df_estudiantes)
    
}

# View(df_unida)

## 1.3.  Limpieza final --------------------------------------------------------

# Guardar todos los nombres de entidades federativas para homologarlos 
v_entidad   <- unique(df_unida$entidad)

df_limpio    <- df_unida                            %>% 
    filter(!is.na(entidad))                         %>% 
    select(entidad, anio, estudiantes)              %>% 
    # Agregar identificador del indicador
    mutate(
        id_dimension = "03", 
        id_indicador = "51")                        %>% 
    # Generar identificador numérico 
    mutate(
        cve_ent = case_when(
            entidad == v_entidad[2]  ~ "01", 
            entidad == v_entidad[3]  ~ "02", 
            entidad == v_entidad[4]  ~ "03", 
            entidad == v_entidad[5]  ~ "04",
            entidad == v_entidad[8]  ~ "05", 
            entidad == v_entidad[9]  ~ "06", 
            entidad == v_entidad[6]  ~ "07", 
            entidad == v_entidad[7]  ~ "08", 
            entidad == v_entidad[10] ~ "09", 
            entidad == v_entidad[11] ~ "10", 
            entidad == v_entidad[12] ~ "11", 
            entidad == v_entidad[13] ~ "12", 
            entidad == v_entidad[14] ~ "13", 
            entidad == v_entidad[15] ~ "14", 
            entidad == v_entidad[16] ~ "15", 
            entidad == v_entidad[17] ~ "16", # Michoacán 
            entidad == v_entidad[18] ~ "17", 
            entidad == v_entidad[19] ~ "18", 
            entidad == v_entidad[20] ~ "19", # Nuevo León
            entidad == v_entidad[21] ~ "20", 
            entidad == v_entidad[22] ~ "21", 
            entidad == v_entidad[23] ~ "22", 
            entidad == v_entidad[24] ~ "23", 
            entidad == v_entidad[25] ~ "24", 
            entidad == v_entidad[26] ~ "25", 
            entidad == v_entidad[27] ~ "26", 
            entidad == v_entidad[28] ~ "27", 
            entidad == v_entidad[29] ~ "28", 
            entidad == v_entidad[30] ~ "29", 
            entidad == v_entidad[31] ~ "30", 
            entidad == v_entidad[32] ~ "31", 
            entidad == v_entidad[33] ~ "32", 
            entidad == v_entidad[34] ~ "00", # Nacional
            entidad == v_entidad[35] ~ "15", # Estado de México
            entidad == v_entidad[36] ~ "16", # Michoacán 
            entidad == v_entidad[37] ~ "19", # Nuevo León
            entidad == v_entidad[38] ~ "22", # Querétaro
            entidad == v_entidad[39] ~ "24", # San Luis Potosí 
            entidad == v_entidad[40] ~ "30", # Veracruz
            entidad == v_entidad[41] ~ "31", # Yucatán 
            entidad == v_entidad[42] ~ "09" # Ciudad de México
        )) %>% 
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
            cve_ent == "32" ~ "ZAC")) %>%
    # Seleccionar variables finales 
    select(cve_ent, entidad_abr_m, anio, id_dimension, id_indicador, estudiantes)

# Agregar población en edad de educación superior 
load("02_datos_crudos/df_pop_state_age.RData")

# Filtrar por años y edades de interés
df_pop <- df_pop_state_age              %>% 
    filter(year > 2009 & year < 2022)   %>% 
    filter(age  > 17   & age  < 24)     %>%  # Este rango de edad da lo más similar a lo reportado por la SEP
    group_by(year, CVE_GEO, state)      %>% 
    summarise(pop = sum(population))    %>% 
    ungroup()                           %>% 
    mutate(cve_ent = str_pad(CVE_GEO, 2, pad = "0"))    %>% 
    select(cve_ent, anio = year, pop)

df_final <- df_limpio                                   %>% 
    left_join(df_pop, by = c("cve_ent", "anio"))        %>%
    mutate(indicador_value = estudiantes/pop)           %>% 
    # Seleccionar variables finales 
    select(
        cve_ent, entidad_abr_m, anio, id_dimension, id_indicador, indicador_value)


# 2. Guardar en drive ----------------------------------------------------------

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1128387096")[1, 2])


# Guardar en la base en el Drive
googlesheets4::range_write(ss = v_id, data = df_final,
    sheet = "03_51_cobertura_educacion_superior")

# FIN. -------------------------------------------------------------------------

