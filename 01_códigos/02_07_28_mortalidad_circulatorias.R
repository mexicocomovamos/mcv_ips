#------------------------------------------------------------------------------#
# Proyecto:             ÍNDICE DE PROGRESO SOCIAL
# Objetivo:             Mortalidad por enfermedades circulatorias
#                       Enfermedades del sistema circulatorio (I00-I99)
#
# Encargada:            Regina Isabel Medina Rosales     
# Correos:              regimedina19@gmail.com
# 
# Fecha de creación:    27 de octubre de 2021
# Última actualización: 27 de octubre de 2021
#------------------------------------------------------------------------------#

# Fuente: https://www.inegi.org.mx/sistemas/olap/proyectos/bd/continuas/mortalidad/mortalidadgeneral.asp?s=est&c=11144&proy=mortgral_mg#

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


# 1. Importar datos ------------------------------------------------------------

df_crudo <- read_excel(paste0(inp, "INEGI_exporta_27_10_2021_14_40_37.xlsx"), 
    skip = 4)


# 2. Procesamiento de datos ----------------------------------------------------

# Guardar nombres de las variables
v_names     <- names(df_crudo)

# Renombrar y cambiar formato
df_limpio   <- df_crudo         %>% 
    rename(
        anio      = v_names[1], 
        Nacional  = v_names[2]) %>% 
    # Quitar fila irrelevante
    filter(!is.na(anio))        %>% 
    # Cambiar a formato largo 
    pivot_longer(
        cols      = -c(anio), 
        names_to  = "entidad", 
        values_to = "total")    %>% 
    # Agregar variables de identificación 
    mutate(
        id_dimension = "01", 
        id_indicador = "04", 
        total = as.numeric(str_remove_all(total, ",")))


# Guardar nombres de las entidades
v_entidad   <- unique(df_limpio$entidad)

# Renombrar entidades
df_entidad  <- df_limpio        %>% 
    # Valores de entidades federativas 
    mutate(
        cve_ent = case_when(
            entidad == v_entidad[1]  ~ "00", # Nacional
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
            entidad == v_entidad[33] ~ "32")) %>% 
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
            cve_ent == "32" ~ "ZAC"))  %>% 
    # Seleccionar variables finales 
    select(cve_ent, entidad_abr_m, anio, id_dimension, id_indicador, total)

# Procesar información de población
load("02_datos_crudos/df_pop_state_age.Rdata") # Población total por entidad y edad

df_pop <- df_pop_state_age                          %>% 
    # filter(age %in% 0)                              %>% 
    group_by(state, CVE_GEO, year)                  %>% 
    summarise(pob_tot = sum(population))            %>% 
    ungroup()                                       %>% 
    mutate(
        year    = as.character(year), 
        cve_ent = str_pad(CVE_GEO, 2, pad = "0"), 
        grupo   = "Población menor a 1 año")        %>% 
    select(anio = year, cve_ent, state, grupo, pob_tot)

# Agregar población 
df_final    <- df_entidad                           %>% 
    left_join(df_pop, by = c("cve_ent", "anio"))    %>% 
    filter(anio %in% 2000:2021)                     %>% 
    mutate(
        indicador_value = total*100000/pob_tot)     %>% 
    arrange(anio, cve_ent)                          %>% 
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
    sheet = "02_28_mortalidad_circulatorias")

# FIN. -------------------------------------------------------------------------

