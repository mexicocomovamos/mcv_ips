#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Matriculación de preescolar
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          27 de septiembre de 2021
# Última actualización:       17 de agosto     de 2023
#------------------------------------------------------------------------------#

# Fuente: https://www.planeacion.sep.gob.mx/principalescifras/
# Cada año corresponde a aquel en que terminó el ciclo escolar.  

# Rango de edad para preescolar: 3 a 5

# Nota: Las cifras del ciclo 2022-2023 segúan siendo preliminares al 17 de 
# agosto de 2023. Procurar repetir la actualización de estos años con las cifras 
# preliminares. 

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
inp <- "02_datos_crudos/02_17_matriculación_preescolar/"

# Activar las credenciales de google
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

# 1. Procesamiento de datos ----------------------------------------------------

# Vectores de texto
v_time      <- c(
    "2009-2010", "2010-2011", "2011-2012", "2012-2013", 
    "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
    "2018-2019", "2019-2020", "2020-2021", "2021-2022", 
    "2022-2023")

v_formato   <- c(".xlsx")

## 1.1. Limpieza de ensayo -----------------------------------------------------

# Cargar datos 
df_crudo    <- read_excel(paste0(inp, v_time[8], v_formato), skip = 4)
v_names     <- names(df_crudo)

# Renombrar variables
df_estudiantes <- df_crudo                                  |>
    slice(1:33)                                             |> 
    select(v_names[1], v_names[3])                          |>
    rename(                 
        entidad       = v_names[1],                   
        estudiantes   = v_names[3])                         |> 
    mutate(anio       = 2010)

## 1.2. Limpieza en bucle ------------------------------------------------------

# Vectores de texto
v_time      <- c(
    "2009-2010", "2010-2011", "2011-2012", "2012-2013", 
    "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
    "2017-2018", "2018-2019", "2019-2020", "2020-2021", 
    "2021-2022", "2022-2023")

v_formato   <- c(".xlsx")
v_years     <- c(2009:2023)

# Base vacía 
df_unida <- data.frame(entidad = NA, estudiantes = NA, anio = NA)

for(i in 1:length(v_time)){
    
    # Cambiar el número de renglones que se omiten a partir del ciclo 2022-2023
    v_anio <- as.numeric(str_sub(v_time[i], 1, 4))
    
    if(v_anio >= 2022) {
        df_crudo    <- read_excel(paste0(inp, v_time[i], v_formato), skip = 5)
        
    } else {
        df_crudo    <- read_excel(paste0(inp, v_time[i], v_formato), skip = 4)
        
    }
    
    # Guardar nombres de las columnas
    v_names     <- names(df_crudo)
    
    # Limpiar bases
    df_estudiantes <- df_crudo                                  |>
        slice(1:33)                                             |> 
        select(v_names[1],  v_names[3])                         |>
        rename(
            entidad       = v_names[1], 
            estudiantes   = v_names[3])                         |> 
        mutate(anio       = v_years[i])                         |> 
        mutate(estudiantes = as.numeric(estudiantes))           |> 
        drop_na(estudiantes)
    
    # Pegar serie de tiempo 
    df_unida <- df_unida |> bind_rows(df_estudiantes)
    
}

# View(df_unida)

## 1.3.  Limpieza final --------------------------------------------------------

# Guardar todos los nombres de entidades federativas para homologarlos 
v_entidad   <- unique(df_unida$entidad)

df_limpio    <- df_unida                            |> 
    filter(!is.na(entidad))                         |> 
    select(entidad, anio, estudiantes)              |> 
    # Agregar identificador del indicador
    mutate(
        id_dimension = "02", 
        id_indicador = "17")                        |> 
    mutate(
        entidad = ifelse(
            entidad == "DISTRITO FEDERAL", 
            "CIUDAD DE MÉXICO",
            ifelse(entidad == "TOTAL", "NACIONAL", entidad)),
        entidad = ifelse(str_starts(entidad, "OAX"), "OAXACA", entidad),
        entidad = ifelse(str_starts(entidad, "MICH"), "MICHOACÁN DE OCAMPO", entidad),
        id_dimension = "02", 
        id_indicador = "19")                        |> 
    # Generar identificador numérico 
    left_join(
        readxl::read_excel("02_datos_crudos/00_cve_ent.xlsx") |> 
            rename(ent = entidad, entidad = entidad_comp) |> 
            mutate(entidad = toupper(entidad))
    ) |> 
    # Seleccionar variables finales 
    select(cve_ent, entidad_abr_m, anio, id_dimension, id_indicador, estudiantes)

unique(df_limpio$entidad_abr_m)
table(is.na(df_limpio$entidad_abr_m))

# Procesar información de población
load("02_datos_crudos/df_pop_state_age.Rdata") # Población total por entidad y edad

df_pop <- df_pop_state_age                          |> 
    filter(age %in% 3:5)                            |> 
    group_by(state, CVE_GEO, year)                  |> 
    summarise(pob_tot = sum(population))            |> 
    ungroup()                                       |> 
    mutate(
        cve_ent = str_pad(CVE_GEO, 2, pad = "0"), 
        grupo   = "Población de 3 a 5 años")        |> 
    select(anio = year, cve_ent, state, grupo, pob_tot)

# Agregar población 
df_final    <- df_limpio                            |> 
    left_join(df_pop, by = c("cve_ent", "anio"))    |> 
    filter(anio < 2023)                             |> 
    mutate(
        indicador_value = estudiantes*100/pob_tot)  |> 
    # Seleccionar variables finales 
    select(
        cve_ent, entidad_abr_m, anio, id_dimension, id_indicador, indicador_value)

table(df_final$entidad_abr_m, df_final$anio)

# 2. Guardar en drive ----------------------------------------------------------

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1859408845")[1, 2])

# Guardar en la base en el Drive
googlesheets4::range_write(ss = v_id, data = df_final,
    sheet = "02_17_matriculacion_preescolar")

# FIN. -------------------------------------------------------------------------
