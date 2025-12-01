#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Cobertura educación superior
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          27 de septiembre de 2021
# Última actualización:       10 de octubre    de 2022
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

# Vectores para directorio 
inp <- "02_datos_crudos/03_53_paridad_educ/"

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

# Vectores de texto
v_tipo      <- c("edusuperior_")

v_time      <- c(
    "2009-2010", "2010-2011", "2011-2012", "2012-2013", 
    "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
    "2017-2018", "2018-2019", "2019-2020", "2020-2021", 
    "2021-2022", "2022-2023", "2023-2024", "2024-2025")

v_formato   <- c(".xlsx")

## 1.1. Limpieza de ensayo -----------------------------------------------------

# Cargar datos 
df_crudo    <- read_excel(paste0(inp, v_tipo[1], v_time[7], v_formato), skip = 4)
v_names     <- names(df_crudo)

# Renombrar variables
df_estudiantes <- df_crudo                                  %>%
    slice(1:33)                                             %>% 
    select(v_names[1], v_names[4])                          %>%
    rename(                 
        entidad       = v_names[1], 
        estudiantes   = v_names[4])                         %>% 
    mutate(anio       = 2015)

## 1.2. Limpieza en bucle ------------------------------------------------------

# Vectores de texto
v_formato   <- c(".xlsx")
v_years     <- c(2009:2025)

# Base vacía 
df_unida <- data.frame()

for(i in 1:length(v_time)){
    
    # Cambiar el número de renglones que se omiten a partir del ciclo 2022-2023
    v_anio <- as.numeric(str_sub(v_time[i], 1, 4))
    
    if(v_anio >= 2022) {
        df_crudo    <- read_excel(paste0(inp, v_tipo[1], v_time[i], v_formato), skip = 5)
        
    } else {
        df_crudo    <- read_excel(paste0(inp, v_tipo[1], v_time[i], v_formato), skip = 4)
        
    }    
    
    v_names     <- names(df_crudo)
    
    #x <- ifelse(v_time[i] %in% v_time[1:5], 3, #nota: aquí revisar la base nueva y el orden de las columnas 
     #           ifelse(v_time[i] %in% v_time[15], 3,4)) 
    
    x <- ifelse(v_anio >= 2023, 3,  # 2023-2024: columna 3
                ifelse(v_anio >= 2014 & v_anio <= 2022, 4,  # 2014-2022: columna 4
                       3)) #nota: aquí revisar la base nueva y el orden de las columnas 
    
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
            entidad == v_entidad[1]  ~ "01",
            entidad == v_entidad[2]  ~ "02", 
            entidad == v_entidad[3]  ~ "03", 
            entidad == v_entidad[4]  ~ "04", 
            entidad == v_entidad[5]  ~ "07", # Chiapas
            entidad == v_entidad[8]  ~ "06", # Colima
            entidad == v_entidad[9]  ~ "09", # CDMX
            entidad == v_entidad[6]  ~ "08", # Chihuahua
            entidad == v_entidad[7]  ~ "05", # Coahulia
            entidad == v_entidad[10] ~ "10", 
            entidad == v_entidad[11] ~ "11", 
            entidad == v_entidad[12] ~ "12", 
            entidad == v_entidad[13] ~ "13", 
            entidad == v_entidad[14] ~ "14", 
            entidad == v_entidad[15] ~ "15", 
            entidad == v_entidad[16] ~ "16", # Michoacán 
            entidad == v_entidad[17] ~ "17", 
            entidad == v_entidad[18] ~ "18", 
            entidad == v_entidad[19] ~ "19", # Nuevo León
            entidad == v_entidad[20] ~ "20", 
            entidad == v_entidad[21] ~ "21", 
            entidad == v_entidad[22] ~ "22", 
            entidad == v_entidad[23] ~ "23", 
            entidad == v_entidad[24] ~ "24", 
            entidad == v_entidad[25] ~ "25", 
            entidad == v_entidad[26] ~ "26", 
            entidad == v_entidad[27] ~ "27", 
            entidad == v_entidad[28] ~ "28", 
            entidad == v_entidad[29] ~ "29", 
            entidad == v_entidad[30] ~ "30", 
            entidad == v_entidad[31] ~ "31", 
            entidad == v_entidad[32] ~ "32", 
            entidad == v_entidad[33] ~ "00", # Nacional
            entidad == v_entidad[34] ~ "15", # Estado de México
            entidad == v_entidad[35] ~ "16", # Michoacán 
            entidad == v_entidad[36] ~ "19", # Nuevo León
            entidad == v_entidad[37] ~ "22", # Querétaro
            entidad == v_entidad[38] ~ "24", # San Luis Potosí 
            entidad == v_entidad[39] ~ "30", # Veracruz
            entidad == v_entidad[40] ~ "31", # Yucatán 
            entidad == v_entidad[41] ~ "09",  # Ciudad de México
            entidad == v_entidad[42] ~ "09",  # Ciudad de México
            entidad == v_entidad[43] ~ "15", # Estado de México
            entidad == v_entidad[44] ~ "16", # Michoacán 
            entidad == v_entidad[45] ~ "19", # Nuevo León
            entidad == v_entidad[46] ~ "22", # Querétaro
            entidad == v_entidad[47] ~ "24", # San Luis Potosí 
            entidad == v_entidad[48] ~ "31" # Yucatán 
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


# Controles de calidad
# View(df_limpio)
table(df_limpio$entidad)
table(df_limpio$anio)

# Agregar población en edad de educación superior 
load("02_datos_crudos/df_pop_state_age.RData")

# Filtrar por años y edades de interés
df_pop <- df_pop_state_age              %>% 
    filter(year > 2008 & year < 2025)   %>% 
    filter(age  > 17   & age  < 24)     %>%  # Este rango de edad da lo más similar a lo reportado por la SEP
    group_by(year, CVE_GEO, state)      %>% 
    #summarise(pop = sum(population))    %>% 
    summarise(pop = sum(pop_tot))    %>% 
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

