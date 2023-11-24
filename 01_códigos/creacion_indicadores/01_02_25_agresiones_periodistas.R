#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Procesar datos de mecanismos de participación
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          20 de septiembre de 2021
# Última actualización:       8 de agosoto de 2023
#------------------------------------------------------------------------------#

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
inp <- "02_datos_crudos/02_25_agresiones_periodistas/"

# Activar las credenciales de google
#googledrive::drive_auth("regimedina19@gmail.com")
#googlesheets4::gs4_auth("regimedina19@gmail.com")

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x))}

# 1. Cargar datos --------------------------------------------------------------

# Base histórica de 2007 a 2014
df_crudo0714    <- read_excel(paste0(inp, "2007-2015_fechas.xlsx"))

# Base 2015 
df_crudo15  <- read_excel(paste0(inp, "Agresiones 2015 - public.xlsx"))

# Base 2018
df_crudo18  <- read_excel(paste0(inp, "RESPALDO BASE DE DATOS 2018 - public.xlsx"))

# Base 2019 
df_crudo19  <- read_excel(paste0(inp, "CIFRAS 2019 - public.xlsx"))

# Base 2020
df_crudo20  <- read_excel(paste0(inp, "Agresiones 2020_Final.xlsx"))

# Base 2021
df_crudo21  <- read_excel(paste0(inp, "Agresiones 2021_Final.xlsx"), sheet = 2)

# Base 2022
df_crudo22  <- read_excel(paste0(inp, "Corte FINAL Agresiones 30 de enero 2023 PUBLIC.xlsx"))


# Población total por entidad
df_pop      <- imp_dv("1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1859408845")

# 2. Procesamiento -------------------------------------------------------------

# Limpiar cada base por separado 
v_names <- names(df_crudo0714)

df_0714 <- df_crudo0714                                 %>% 
    mutate(
        Fecha  = as.Date(as.numeric(Fecha), origin = "1899-12-30"), 
        anio   = year(Fecha), 
        Estado = ifelse(
            Estado == "Saltillo", "Coahuila", Estado))  %>% 
    filter(!is.na(Fecha), anio > 2009)                  %>%
    group_by(Estado, anio)                              %>% 
    summarise(total = n())                              %>% 
    select(Estado, anio, total)


df_15 <- df_crudo15                                     %>% 
    group_by(Estado)                                    %>% 
    summarise(total = n())                              %>% 
    mutate(anio = 2015)                                 %>% 
    select(Estado, anio, total)


df_18 <- df_crudo18                                     %>% 
    group_by(event_state_properties_state_name)         %>% 
    summarise(total = n())                              %>% 
    mutate(anio = 2018)                                 %>% 
    select(Estado = event_state_properties_state_name, anio, total)


df_19 <- df_crudo19                                     %>% 
    group_by(event_state_properties_state_name)         %>% 
    summarise(total = n())                              %>% 
    mutate(anio = 2019)                                 %>% 
    select(Estado = event_state_properties_state_name, anio, total)

df_20 <- df_crudo20                                     %>% 
    group_by(`Entidad Federativa`)                      %>% 
    summarise(total = n())                              %>% 
    mutate(anio = 2020)                                 %>% 
    select(Estado = `Entidad Federativa`, anio, total)

df_21 <- df_crudo21                                     %>% 
    group_by(event_state_properties_state_name)         %>% 
    summarise(total = n())                              %>% 
    mutate(anio = 2021)                                 %>% 
    select(Estado = event_state_properties_state_name, anio, total)

df_22 <- df_crudo22                                     %>% 
    group_by(`Entidad Federativa`)                      %>% 
    summarise(total = n())                              %>% 
    mutate(anio = 2022)                                 %>% 
    select(Estado = `Entidad Federativa`, anio, total)


# Unir todos los años disponibles 
df_unida    <- df_0714                                  %>% 
    bind_rows(df_15, df_18, df_19, df_20, df_21, df_22) %>% 
    # Agregar nivel nacional 
    filter(!(Estado %in% c("Extranjero", "Exterior")))  %>% 
    group_by(anio) %>% 
    bind_rows(
        summarise_all(.), 
        ~if(is.numeric(.) sum(.) else ("Nacional"))
        )


# Homologar nombres e identificadores de las entidades
v_entidad   <- unique(df_unida$Estado)

df_entidad  <- df_unida             %>% 
    mutate(
        cve_ent = case_when(
            Estado == v_entidad[1]   ~ "01", 
            Estado == v_entidad[2]   ~ "02", 
            Estado == v_entidad[3]   ~ "03", 
            Estado == v_entidad[4]   ~ "04", 
            Estado == v_entidad[7]   ~ "05", 
            Estado == v_entidad[8]   ~ "06", 
            Estado == v_entidad[5]   ~ "07", 
            Estado == v_entidad[6]   ~ "08", 
            Estado == v_entidad[9]   ~ "09", # Ciudad de México
            Estado == v_entidad[10]  ~ "09", # Ciudad de México
            Estado == v_entidad[11]  ~ "10", 
            Estado == v_entidad[13]  ~ "11", 
            Estado == v_entidad[14]  ~ "12", 
            Estado == v_entidad[15]  ~ "13", 
            Estado == v_entidad[16]  ~ "14", 
            Estado == v_entidad[12]  ~ "15", 
            Estado == v_entidad[17]  ~ "16", 
            Estado == v_entidad[18]  ~ "17", 
            Estado == v_entidad[19]  ~ "18", 
            Estado == v_entidad[20]  ~ "19", 
            Estado == v_entidad[21]  ~ "20", 
            Estado == v_entidad[22]  ~ "21", 
            Estado == v_entidad[23]  ~ "22", # Querétaro
            Estado == v_entidad[24]  ~ "22", # Querétaro
            Estado == v_entidad[25]  ~ "23", 
            Estado == v_entidad[26]  ~ "24", 
            Estado == v_entidad[27]  ~ "25", 
            Estado == v_entidad[28]  ~ "26", 
            Estado == v_entidad[29]  ~ "27", 
            Estado == v_entidad[30]  ~ "28", 
            Estado == v_entidad[31]  ~ "29", 
            Estado == v_entidad[32]  ~ "30", 
            Estado == v_entidad[33]  ~ "31", 
            Estado == v_entidad[34]  ~ "32", 
            Estado == v_entidad[35]  ~ "09", # Ciudad de México
            Estado == v_entidad[36]  ~ "00"  # Nacional
        ))                              %>% 
    select(cve_ent, anio, total)        %>% 
    ungroup()                           %>% 
    group_by(cve_ent, anio)             %>% 
    summarise(total = sum(total))       %>% 
    ungroup()

# Las entidades tienen frecuencia distinta (no en todos los años hay incidentes)
table(df_entidad$cve_ent)

# Expandir base para que incluya todas las combinaciones posibles (entidad, año)
df_expanded <- df_entidad               %>% 
    complete(cve_ent, anio = full_seq(anio, period = 1), fill = list(total = 0))

#Filtrar año 2017 ya que no se incluye en la base de datos
# Remove rows with a specific year (e.g., 2022)
df_filtered <- df_expanded %>%
    filter(anio != 2017)

# Ahora todas las entidades tienen los años registrados (con 0 incidentes)
table(df_expanded$cve_ent)

# Limpieza final 
df_limpia <- df_filtered                            %>% 
    # Agregar abreviatura de la entidad
    mutate(
        entidad_abr_m = case_when(
            cve_ent == "00" ~ "Nacional", 
            cve_ent == "01" ~ "AGS", 
            cve_ent == "02" ~ "BC" ,
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
            cve_ent == "32" ~ "ZAC"))               %>% 
    # Agregar identificadores de dimensión e indicador
    mutate(id_dimension = 02, id_indicador = 35)    %>% 
    # Estimar tasa por cada 100,000 habitantes 
    left_join(df_pop, by = c("cve_ent", "anio"))    %>% 
    mutate(indicador_value = total*100000/pob_tot)  %>% 
    # Ordernar y seleccionar variables finales
    arrange(anio, cve_ent)                          %>% 
    select(cve_ent, entidad_abr_m, anio, id_dimension, 
        id_indicador, indicador_value)


# 2. Guardar en drive ----------------------------------------------------------

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1185997937")[1, 2])

# Guardar en la base en el Drive
googlesheets4::range_write(ss = v_id, data = df_limpia,
    sheet = "02_25_tasa_agresion_periodistas")

# FIN. -------------------------------------------------------------------------
