#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Procesar datos de mecanismos de participación
#
# Encargadas:     
# Correos:                    katia@mexicocomovamos.mx | regimedina19@gmail.com
# 
# Fecha de creación:          09 de septiembre de 2021
# Última actualización:       14 de septiembre de 2021
#------------------------------------------------------------------------------#

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
require(pacman)
p_load(tidyverse, dplyr, googledrive, googlesheets4, beepr)

# Desactiva notación científica
options(scipen=999)

# Vaciar espacio de trabajo 
rm(list=ls())

# Colores MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados

# 1. Importar datos ------------------------------------------------------------

# 1.1. Preparación paquetería google drive -------------------------------------

# Activar las credenciales de google
    googledrive::drive_auth("regimedina19@gmail.com")
    googlesheets4::gs4_auth("regimedina19@gmail.com")

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1128387096")[1, 2])

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x))}

# 1.1. Cargar datos de drive ---------------------------------------------------

# Catálogos 
df_ent   <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1Yn5H5C6mH-5rPMaKNVDmXgiAnfMpmqADVAvB7wPTRFQ/edit#gid=546787975")
df_mec   <- imp_dv("1dInh-AHCo3WanzOw51nZXzZxCwAnKRdz6nx9Lrk7hXE/edit#gid=2077764669")

# Mecanismos de participación en formato largo 
df_2011r <- imp_dv("18nEHJNNKaFm8uEIIu5-pLkPDGPjDjj_MkFetw_SAn8Y/edit#gid=398684941")
df_2012r <- imp_dv("1bvKvt6Pr7OeOo5p5oAJj8xh-zCJmdiS4o_E4eMthBrM/edit#gid=842281077")
df_2013r <- imp_dv("1d3nano2akraBzgeAtLsCjPjHdbNlvJWnU5_KDJw2n9c/edit#gid=68691302")
df_2014r <- imp_dv("15BvL2zV17-vCnYiC9_GnHNwkjghJ7bQOxlj3LTw2i0Y/edit#gid=1084159780")
df_2015r <- imp_dv("16vTqD-bL37oeF3IIwiFfx0EegIE2VX9CUGzrJzjgsNo/edit#gid=613925185")
df_2016r <- imp_dv("1xfefRqquMMAL-ADMt-F40ty7ZZoaqiFMUX52VE71Uf0/edit#gid=1636399023")

# Mecanismos de participación en formato ancho
df_2017r <- imp_dv("1nzpPSHuk3yo06KQrsat4Y3iZcSqKHjdFjBfiQ9M5Q9A/edit#gid=302818891")
df_2018r <- imp_dv("1DRRvhuNBuU_U3JoIxSUoNUz1AVdKVQkmPI_NM_j0RHc/edit#gid=1687519434")
df_2019r <- imp_dv("1ed9fT9tkRzw14V8CeSlCHVZpz6TtRhOJeT6bPocum8w/edit#gid=1657469613")
df_2020r <- imp_dv("1gMDSBlPQqN-cdhYShAyAdN6-eD3qE8XmF3gxGWcgIJ8/edit#gid=526210988")

# 2. Procesar datos ------------------------------------------------------------

# Añadir variable con etiqueta de año 
df_2011 <- df_2011r %>% mutate(year = 2011)
df_2012 <- df_2012r %>% mutate(year = 2012)
df_2013 <- df_2013r %>% mutate(year = 2013)
df_2014 <- df_2014r %>% mutate(year = 2014)
df_2015 <- df_2015r %>% mutate(year = 2015)
df_2016 <- df_2016r %>% mutate(year = 2016)

# Unir todas las bases con formato largo de 2011 a 2016
df_long1116 <- df_2011                                  %>% 
    rbind(df_2012, df_2013, df_2014, df_2015, df_2016)  %>% 
    group_by(entidad, year)                             %>% 
    summarise(mec_part = n())                           %>%
    ungroup()                                           %>%
    group_by(year) %>% 
    mutate(p_mec = mec_part/ifelse(
        year %in% c(2011:2014), 10, max(mec_part)))     %>% 
    ungroup()                                           %>% 
    mutate_all(as.character) 


# ¿A partir de 2015 los mecanismos de participación ciudadana se disparan
# porque ahora vienen por tema?

# Procesar bases con formato ancho 
df_2017 <- df_2017r                                     %>% 
    select(-no_aplic)                                   %>%
    mutate_all(as.character)                            %>% 
    # Pasar a formato largo 
    pivot_longer(
        cols      = starts_with("mecpar"), 
        names_to  = "mecanismo", 
        values_to = "valor")                            %>% 
    # Añadir etiqueta de año y cambiar valores a 0
    mutate(
        year      = 2017, 
        valor     = ifelse(valor == 1, 1, 0))
     

df_2018 <- df_2018r                                     %>% 
    select(-no_aplic)                                   %>%
    mutate_all(as.character)                            %>% 
    pivot_longer(
        cols      = starts_with("mecpar"), 
        names_to  = "mecanismo", 
        values_to = "valor")                            %>% 
    mutate(
        year      = 2018, 
        valor     = ifelse(valor == 1, 1, 0))


df_2019 <- df_2019r                                     %>% 
    select(-no_aplic)                                   %>%
    mutate_all(as.character)                            %>% 
    pivot_longer(
        cols      = starts_with("mecpar"), 
        names_to  = "mecanismo", 
        values_to = "valor")                            %>% 
    mutate(
        year      = 2019, 
        valor     = ifelse(valor == 1, 1, 0))

df_2020 <- df_2020r                                     %>% 
    select(-no_aplic)                                   %>%
    mutate_all(as.character)                            %>% 
    pivot_longer(
        cols      = starts_with("mecpar"), 
        names_to  = "mecanismo", 
        values_to = "valor")                            %>% 
    mutate(
        year      = 2020, 
        valor     = ifelse(valor == 1, 1, 0))



# Unir todas las nuevas bases con formato largo de 2017 a 2020
df_long1720 <- df_2017                                  %>% 
    rbind(df_2018, df_2019, df_2020)                    %>% 
    mutate(
        valor = as.numeric(valor))                      %>% 
    group_by(entidad, year)                             %>% 
    summarise(mec_part = sum(valor, na.rm = T))         %>% 
    mutate(p_mec     = round(mec_part/(16*23), 5))


# Unir todos los años y agregar los nombres de las entidades
df_mecanismos <- df_long1116                            %>% 
    rbind(df_long1720)                                  %>% 
    mutate(entidad = as.double(entidad))                %>% 
    left_join(df_ent, by = c("entidad" = "ENTIDAD"))    %>% 
    arrange(entidad, year)                              %>% 
    mutate(
        cve_ent = str_pad(entidad, 2, pad = "0"), 
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
            cve_ent == "32" ~ "ZAC"), 
        id_dimension = "03", 
        id_indicador = "38")    %>% 
    select(
        cve_ent, entidad_abr_m, anio = year, id_dimension, 
        id_indicador, indicador_value = p_mec)
    
# View(df_mecanismos)


# 3. Revisar y guardar datos ---------------------------------------------------
    # View(df_long1116)
    # View(df_long1720)
    # View(df_mecanismos)

str(df_mecanismos)

# Guardar en la base en el Drive
googlesheets4::range_write(ss = v_id, data = df_mecanismos,
    sheet = "03_38_participación_ciudadana_gobierno")

# FIN. -------------------------------------------------------------------------
