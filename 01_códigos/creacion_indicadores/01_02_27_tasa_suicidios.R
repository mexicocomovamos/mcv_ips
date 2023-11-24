#------------------------------------------------------------------------------#
# Proyecto:             ÍNDICE DE PROGRESO SOCIAL
# Objetivo:             Suicidios (Tasa de homicidios por cada 100mil habitantes)
#
# 
# Encargada:            Regina Isabel Medina Rosales
# Autor:                Samantha Contreras Guerrero
# Correos:              regina@mexicocomovamos.mx , sammcontr@gmail.com
# 
# Fecha de creación:    9 de Agosto de 2023
# Última actualización: 11 de julio de 2023
#------------------------------------------------------------------------------#

# Fuente:https://www.inegi.org.mx/app/tabulados/interactivos/?px=Mental_07&bd=Salud

# 0. Configuración inicial -----------------------------------------------------

# Especificar zona horaria e idioma
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

# Funciones para directorio 
paste_inp <- function(x){paste0("02_datos_crudos/02_07_tasa_suicidios/"    , x)}
paste_out <- function(x){paste0("02_bases_procesadas/00_02_tasa_suicidios/", x)}


# Activar las credenciales de google
googledrive::drive_auth("samantha@mexicocomovamos.mx")
googlesheets4::gs4_auth("samantha@mexicocomovamos.mx")

#Usuarios

#v_usuaria <- "regina"

#googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
#googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))


# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x))}


# 1. Importar datos ------------------------------------------------------------

df_crudo    <- read_excel(paste_inp("INEGI_exporta_tasa_defunciones_suicidio.xlsx"), skip = 5)

# 2. Procesamiento de datos ----------------------------------------------------

# Guardar nombres de las variables
v_names     <- names(df_crudo)

#En este caso eliminar la primer columna innecesaria
df_crudo <- df_crudo[,-1]

# Renombrar y cambiar formato
df_limpio   <- df_crudo                       %>% 
    rename(
        # Renombrar columna de año y nacional
        anio      = v_names[2],
        # Columna con nivel nacional
        Nacional  = v_names[3])             %>% 
    # Cambiar a formato largo 
    pivot_longer(
        cols      = -c(anio), 
        names_to  = "entidad", 
        values_to = "total")                      %>% 
    # Cambiar a variable numérica 
    mutate(total = as.numeric(str_remove_all(total, ","))) %>% 
    # filter(anio_ocurrencia != "Total")          %>% 
    # Agrupar por año de ocurrencia y estimar totales 
    group_by(entidad, anio)          %>% 
    summarise(total = sum(total, na.rm = TRUE)) %>% 
    ungroup()                                   %>% 
    rename(anio = anio)              %>% 
    # Agregar variables de identificación 
    mutate(
        id_dimension = "01", 
        id_indicador = "12") %>% 
    filter(anio %in% c(1990:2021)) 

# Renombrar entidades
df_entidad  <- df_limpio %>% 
    left_join(
        readxl::read_excel("02_datos_crudos/00_cve_ent.xlsx") %>% 
            rename(ent = entidad, entidad = entidad_comp), 
        by = join_by(entidad)
    ) %>% 
    # Seleccionar variables finales 
    select(
        entidad,	cve_ent,	entidad_abr_m,
        anio,	id_dimension,	id_indicador, total)


# Procesar información de población
load("02_datos_crudos/df_pop_state_age.Rdata") 
# Población total por entidad y edad

df_pop <- df_pop_state_age                          %>% 
    group_by(state, CVE_GEO, year)                  %>% 
    summarise(pob_tot = sum(population))            %>% 
    ungroup()                                       %>% 
    mutate(
        year    = as.character(year), 
        cve_ent = str_pad(CVE_GEO, 2, pad = "0"), 
        grupo   = "Población total")                %>% 
    select(anio = year, cve_ent, state, grupo, pob_tot)

# Agregar población 
df_final    <- df_entidad                           %>% 
    left_join(df_pop, by = c("cve_ent", "anio"))    %>% 
    filter(anio %in% 2000:2022)                     %>% 
    mutate(
        indicador_value = total*100000/pob_tot)     %>% 
    arrange(anio, cve_ent)                          %>% 
    # Seleccionar variables finales 
    select(
        entidad,	cve_ent,	entidad_abr_m,
        anio,	id_dimension,	id_indicador,	indicador_value) %>% 
    drop_na(cve_ent)

# 3. Guardar -------------------------------------------------------------------

## 3.1. Guardar copia con procesamiento anterior -------------------------------

# Base con año de ocurrencia 
df_tasa_suicidios <- df_final 

save(df_tasa_suicidios, 
     file = paste_out("df_tasa_suicidios.RData"))

## 3.2. Guardar en drive -------------------------------------------------------

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=2062630409")[1, 2])

# Guardar en la base en el Drive
googlesheets4::range_write(ss = v_id, data = df_final,
                           sheet = "02_27_tasa_suicidios")

# FIN. -------------------------------------------------------------------------

