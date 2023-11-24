#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Procesar datos de paridad de género en educación
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          20 de septiembre de 2021
# Última actualización:       10 de octubre    de 2022
#------------------------------------------------------------------------------#

# Fuente: https://www.planeacion.sep.gob.mx/principalescifras/
# Cada año corresponde a aquel en que terminó el ciclo escolar.  

# Para educación superior se toman todas as categorías del portal
# Para posgrado se toma solo la categoría de posgrados

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
require(pacman)
p_load(readxl, tidyverse, dplyr, googledrive, googlesheets4, janitor, beepr)

# Desactiva notación científica
options(scipen=999)

# Vaciar espacio de trabajo 
rm(list=ls())

# Colores MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados

# Activar las credenciales de google
v_usuaria <- "regina"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))


# Vectores para directorio 
inp <- "02_datos_crudos/03_53_paridad_educ/"

# 1. Procesamiento de datos ----------------------------------------------------

# Vectores de texto
v_tipo      <- c("edusuperior_", "posgrado_")
v_time      <- c(
                "2009-2010", "2010-2011", "2011-2012", "2012-2013", 
                "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
                "2018-2019", "2019-2020", "2020-2021", "2021-2022", 
                "2022-2023")

v_formato   <- c(".xlsx")


## 1.1. Limpieza de ensayo -----------------------------------------------------

# # Posgrados 
# df_crudo    <- read_excel(paste0(inp, v_tipo[2], v_time[1], v_formato), skip = 4)
# v_names     <- names(df_crudo)
# 
# df_posgrado <- df_crudo                                     %>%
#     slice(1:33)                                             %>% 
#     select(v_names[1], v_names[3], v_names[4], v_names[5])  %>%
#     rename(
#         entidad = v_names[1], 
#         posgrado_total   = v_names[5], 
#         posgrado_hombres = v_names[6],
#         posgrado_mujeres = v_names[7])  
# 
# # Licenciatura 
# df_crudo    <- read_excel(paste0(inp, v_tipo[1], v_time[1], v_formato), skip = 4)
# v_names     <- names(df_crudo)
# 
# # Todos 
# df_limpio   <- df_crudo                                     %>%
#     slice(1:33)                                             %>% 
#     select(v_names[1], v_names[3], v_names[4], v_names[5])  %>%
#     rename(
#         entidad = v_names[1], 
#         superior_total   = v_names[3], 
#         superior_hombres = v_names[4],
#         superior_mujeres = v_names[5])                      %>%
#     left_join(df_posgrado, by = "entidad")                  %>% 
#     # Estimar estudiantes de licenciatura (sin posgrado)     
#     mutate(
#         licenciatura_total   = superior_total   - posgrado_total  , 
#         licenciatura_hombres = superior_hombres - posgrado_hombres, 
#         licenciatura_mujeres = superior_mujeres - posgrado_mujeres) %>% 
#     # Cambiar a formato largo 
#     pivot_longer(
#         cols      = -c(entidad), 
#         names_to  = c("nivel", "grupo"),
#         names_sep = "_", 
#         values_to = "total") %>% 
#     pivot_wider(
#         names_from  = grupo, 
#         values_from = total) %>% 
#     # Estimar el valor del indicador 
#     mutate(
#         indicador_value = abs((hombres/mujeres)-1), 
#         anio = 2010
#     )

## 1.2. Limpieza en bucle ------------------------------------------------------

# El formato cambia a partir del ciclo 2014-2015, se agrega la columna 
# de número de instituciones. Con janitor se hace la selección por el nombre 
# de la variable en vez de su posición. 


# Vectores de texto
v_tipo      <- c("edusuperior_", "posgrado_")
v_time      <- c(
    "2009-2010", "2010-2011", "2011-2012", "2012-2013", 
    "2013-2014", "2014-2015", "2015-2016", "2016-2017", 
    "2017-2018", "2018-2019", "2019-2020", "2020-2021", 
    "2021-2022", "2022-2023")

v_formato   <- c(".xlsx")
v_years     <- c(2009:2023)

# Base vacía 
df_unida <- data.frame()

for(i in 1:length(v_time)){

    # Cambiar el número de renglones que se omiten a partir del ciclo 2022-2023
    v_anio <- as.numeric(str_sub(v_time[i], 1, 4))
    
    if(v_anio >= 2022) {
        df_crudo    <- read_excel(paste0(inp, v_tipo[2], v_time[i], v_formato), skip = 5)
        
    } else {
        df_crudo    <- read_excel(paste0(inp, v_tipo[2], v_time[i], v_formato), skip = 4)
        
    }
    
    # Guardar nombres de las columnas
    v_names     <- names(df_crudo)
    
    # Limpiar bases    
    df_posgrado <- df_crudo                                     %>%
        slice(1:33)                                             %>% 
        janitor::clean_names() %>% 
        select(entidad, alumnos, alumnos_mujeres, alumnos_hombres)  %>%
        rename(
            posgrado_total   = alumnos, 
            posgrado_hombres = alumnos_hombres,
            posgrado_mujeres = alumnos_mujeres)  
    
    # Licenciatura 
    if(v_anio >= 2022) {
        df_crudo    <- read_excel(paste0(inp, v_tipo[1], v_time[i], v_formato), skip = 5)
        
    } else {
        df_crudo    <- read_excel(paste0(inp, v_tipo[1], v_time[i], v_formato), skip = 4)
        
    }    
    
    # Guardar nombres de las columnas
    v_names     <- names(df_crudo)
    
    # Todos 
    df_limpio   <- df_crudo                                     %>%
        janitor::clean_names()                                  %>% 
        select(entidad, alumnos, alumnos_mujeres, alumnos_hombres)  %>%
        rename(
            superior_total   = alumnos, 
            superior_hombres = alumnos_hombres,
            superior_mujeres = alumnos_mujeres)                 %>%
        left_join(df_posgrado, by = "entidad")                  %>% 
        # Estimar estudiantes de licenciatura (sin posgrado)     
        mutate(
            licenciatura_total   = superior_total   - posgrado_total  , 
            licenciatura_hombres = superior_hombres - posgrado_hombres, 
            licenciatura_mujeres = superior_mujeres - posgrado_mujeres) %>% 
        # Cambiar a formato largo 
        pivot_longer(
            cols      = -c(entidad), 
            names_to  = c("nivel", "grupo"),
            names_sep = "_", 
            values_to = "total") %>% 
        pivot_wider(
            names_from  = grupo, 
            values_from = total) %>% 
        # Estimar el valor del indicador 
        mutate(
            # indicador_value = abs((hombres/mujeres)-1), 
            indicador_value = (mujeres/(hombres+mujeres))*100, 
            anio = v_years[i]
        )
    
    df_unida <- df_unida %>% bind_rows(df_limpio) %>% drop_na(indicador_value)
}




# Controles de calidad
# View(df_unida)
table(df_unida$entidad)
table(df_unida$nivel)
table(df_unida$anio)

# # Ver valores faltantes
# sum(is.na(df_unida$indicador_value))
# df_na <- df_unida %>% filter(is.na(indicador_value))

# Verificar tendencia a través del tiempo
# ggplot(df_unida %>% filter(entidad == "TOTAL", nivel == "posgrado"),
#        aes(x = anio, y = indicador_value)) +
#     geom_point() +
#     geom_line()



# Hay diferencias en cómo se escriben las entidades, pero ninguna en el mismo año 
# View(table(df_unida$entidad, df_unida$anio))


## 1.3.  Limpieza final --------------------------------------------------------

# NOTA: Hasta el ciclo 2021-2022 había 42 nombres de entidades federativas, 
# revisar que la codificación se siga realizando de forma correcta y contemple 
# todos los valoes. 

# Guardar todos los nombres de entidades federativas para homologarlos 
v_entidad   <- unique(df_unida$entidad)

df_final    <- df_unida                             %>% 
    filter(!is.na(entidad))                         %>% 
    select(entidad, anio, nivel, indicador_value)   %>% 
    # Agregar identificador del indicador
    mutate(
        id_dimension = "03", 
        id_indicador = ifelse(
              nivel == "posgrado", "53", "54"))     %>% 
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
            entidad == v_entidad[41] ~ "09"  # Ciudad de México
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
    select(cve_ent, entidad_abr_m, anio, nivel, id_dimension, id_indicador, indicador_value)


# Controles de calidad
# View(df_final)
table(df_final$entidad)
table(df_final$nivel)
table(df_final$anio)


# Filtrar para cada nivel 
df_posgrado     <- df_final %>% filter(nivel == "posgrado")     %>% select(-nivel)
df_licenciatura <- df_final %>% filter(nivel == "licenciatura") %>% select(-nivel)

# str(df_posgrado)
# str(df_licenciatura)

# View(df_posgrado)
# View(df_licenciatura)

# 2. Guardar en drive ----------------------------------------------------------

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1128387096")[1, 2])


# Guardar en la base en el Drive
googlesheets4::range_write(ss = v_id, data = df_posgrado,
    sheet = "03_53_paridad_género_posgrado")

googlesheets4::range_write(ss = v_id, data = df_licenciatura,
    sheet = "03_54_paridad_género_licenciatura")

# FIN. -------------------------------------------------------------------------
