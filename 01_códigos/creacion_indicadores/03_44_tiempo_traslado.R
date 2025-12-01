##------------------------------------------------------------------------------#
# Proyecto:             ÍNDICE DE PROGRESO SOCIAL
# Objetivo:             Porcentaje de la población ocupada que tarda más de 
#                       2 horas a su trabajo 
#                       
# 
# Encargada:            Sandra Martínez Peña     
# Correos:              sandra@mexicocomovamos.mx
# 
# Fecha de creación:    7 de noviembre de 2025
# Última actualización: 7 de noviembre de 2025
#------------------------------------------------------------------------------#

#Datos: ENUT, https://www.inegi.org.mx/programas/enut/2009/#microdatos
#Pregunta: 
#Para condición de ocupación es 
#Pregunta 4.1 Durante la semana pasada, ¿usted trabajó? Sí = 1, No = 2
#Pregunta 4.2 Aunque ya me dijo que no trabajó, ¿la semana pasada ...
#ayudó a trabajar en las tierras o en un negocio de algún familiar u otra persona? .
#Pregunta: 4.4 Durante la semana pasada, ¿cuánto tiempo utilizó en trasladarse (ida y vuelta) a (SU TRABAJO o RESPUESTA DE 4.2) ...
#p4_4_2: Horas de lunes a viernes (tiempo de traslado en horas)
#P4_4_4  Horas de sábado  a domingo

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
require(pacman)
p_load(tidyverse, dplyr, foreign, srvyr, janitor)

# Desactiva notación científica
options(scipen=999)

# Vaciar espacio de trabajo 
rm(list=ls())

# Funciones para directorio 
paste_inp <- function(x){paste0("02_datos/enut_2024_bd_csv/", x)}
paste_out <- function(x){paste0("03_datos_limpios/", x)}

# Activar las credenciales de google
v_usuaria <- "sandra"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# 1. Importar datos ------------------------------------------------------------

#previo a 2024 eran dbf 
#df_dem <- foreign::read.dbf(paste_inp("TSDem.dbf")) %>% 
# janitor::clean_names()

#df_mod1 <- foreign::read.dbf(paste_inp("TModulo1.dbf")) %>% 
# janitor::clean_names()

#versión 2024 
df_dem <- read_csv(paste_inp("tsdem.csv")) %>% 
    janitor::clean_names()

df_mod1 <- read_csv(paste_inp("tmodulo.csv")) %>% 
    janitor::clean_names()

# 2. Procesamiento de datos ----------------------------------------------------

# Crear llave
df_dem <- df_dem %>% 
    mutate(llave = paste0(upm_dis, viv_sel, hogar, n_ren))

df_mod1 <- df_mod1 %>% 
    mutate(llave = paste0(upm_dis, viv_sel, hogar, n_ren))

# Procesar datos en módulo 1
df_mod1_proc <- df_mod1 %>% 
    mutate(
        p5_1 = as.numeric(as.character(p5_1)),
        p5_2 = as.numeric(as.character(p5_2)),
        # p5_4_1 = as.numeric(as.character(p5_4_1)),  # Horas de lunes a viernes, versión 2014
        p5_9_1 = as.numeric(as.character(p5_9_1)), #versión 2024  
        #  p5_4_3 = as.numeric(as.character(p5_4_3)),  # Horas de sábado a domingo, versión 2014 
        p5_9_3 = as.numeric(as.character(p5_9_3)), #versión 2024 
        # Crear variable de población ocupada
        # Ocupada = trabajó (p4_1 == 1) O ayudó a trabajar (p4_2 <= 4)
        # Las opciones 1-6 en p4_2 representan trabajo (versión 2024) - revisar con cada levantamiento las opciones
        ocupada = case_when(
            p5_1 == 1 ~ 1,           # Trabajó
            p5_2 <= 6 & !is.na(p5_2) ~ 1,  # Ayudó a trabajar (opciones 1-5) #OJO! verificar opciones con cada levantamiento
            TRUE ~ 0                 # No ocupada
        ),
        # Convertir variable de tiempo a numérico (horas totales de lunes a viernes)
        # p4_4_2 viene como texto "00", "01", "10", etc. o NA
        # "01" = 1 hora total, "10" = 10 horas totales
        tiempo_traslado_horas_semana = case_when(
            is.na(p5_9_1) ~ NA_real_,
            TRUE ~ as.numeric(as.character(p5_9_1))
        ),
        # Convertir variable de tiempo sábado a domingo a numérico (horas totales de sábado a domingo)
        tiempo_traslado_horas_fin_semana = case_when(
            is.na(p5_9_3) ~ NA_real_,
            TRUE ~ as.numeric(as.character(p5_9_3))
        ),
        # Sumar horas totales de lunes a viernes + sábado a domingo
        tiempo_traslado_horas_total_semana = case_when(
            !is.na(tiempo_traslado_horas_semana) & !is.na(tiempo_traslado_horas_fin_semana) ~ tiempo_traslado_horas_semana + tiempo_traslado_horas_fin_semana,
            !is.na(tiempo_traslado_horas_semana) ~ tiempo_traslado_horas_semana,
            !is.na(tiempo_traslado_horas_fin_semana) ~ tiempo_traslado_horas_fin_semana,
            TRUE ~ NA_real_
        ),
        # Calcular horas por día considerando los días disponibles:
        # - Si tiene lunes a viernes Y sábado y domingo → divide entre 7
        # - Si solo tiene lunes a viernes → divide entre 5
        # - Si solo tiene sábado y domingo → divide entre 2
        tiempo_traslado_horas_dia_total = case_when(
            !is.na(tiempo_traslado_horas_semana) & !is.na(tiempo_traslado_horas_fin_semana) ~ tiempo_traslado_horas_total_semana / 7,  # Tiene ambos → divide entre 7
            !is.na(tiempo_traslado_horas_semana) ~ tiempo_traslado_horas_semana / 5,  # Solo lunes a viernes → divide entre 5
            !is.na(tiempo_traslado_horas_fin_semana) ~ tiempo_traslado_horas_fin_semana / 2,  # Solo sábado y domingo → divide entre 2
            TRUE ~ NA_real_
        ),
        # Verificar que tenga tiempo de traslado válido (no NA y >= 0)
        tiene_tiempo_valido_total = !is.na(tiempo_traslado_horas_dia_total) & tiempo_traslado_horas_dia_total >= 0,
        # Indicador: tarda 2 horas o más AL DÍA (>= 2 horas por día) - toda la semana (lunes a domingo)
        mas_2_horas = case_when(
            ocupada == 1 & tiene_tiempo_valido_total == TRUE & tiempo_traslado_horas_dia_total >= 2 ~ 1,
            ocupada == 1 & tiene_tiempo_valido_total == TRUE & tiempo_traslado_horas_dia_total < 2 ~ 0,
            TRUE ~ NA_real_
        )
    ) %>% 
    # Filtrar solo población ocupada con tiempo de traslado válido
    filter(ocupada == 1 & tiene_tiempo_valido_total == TRUE) %>% 
    drop_na(mas_2_horas) %>% 
    select(llave, mas_2_horas, fac_per)

# Unir bases

df_data <- df_dem %>% 
    left_join(df_mod1_proc, by = "llave") %>% 
    # Extraer clave de estado de los primeros dos dígitos
    mutate(
        cve_ent = case_when(
            nchar(as.character(control)) == 9 ~ substr(as.character(control), 1, 1),
            TRUE ~ substr(as.character(control), 1, 2)
        ),
        factor = as.numeric(fac_per)
    ) %>% 
    # Filtrar solo quienes tienen mas_2_horas
    filter(!is.na(mas_2_horas))

# Población ocupada que tarda 2 horas o más
# Calcular sobre población ocupada con tiempo válido
# El porcentaje es: (ocupados que tardan >= 2 horas) / (ocupados con tiempo válido) * 100
df_resultados <- df_data %>% 
    as_survey_design(weights = factor) %>% 
    group_by(cve_ent) %>% 
    summarise(
        indicador_value = survey_mean(mas_2_horas, na.rm = T) * 100,
        n = unweighted(n())
    ) %>% 
    select(-ends_with("se")) %>% 
    mutate(
        ent = as.numeric(cve_ent),
        entidad = case_when(
            ent == 1 ~ "Aguascalientes",
            ent == 2 ~ "Baja California",
            ent == 3 ~ "Baja California Sur",
            ent == 4 ~ "Campeche",
            ent == 5 ~ "Coahuila",
            ent == 6 ~ "Colima",
            ent == 7 ~ "Chiapas",
            ent == 8 ~ "Chihuahua",
            ent == 9 ~ "Ciudad de México",
            ent == 10 ~ "Durango",
            ent == 11 ~ "Guanajuato",
            ent == 12 ~ "Guerrero",
            ent == 13 ~ "Hidalgo",
            ent == 14 ~ "Jalisco",
            ent == 15 ~ "México",
            ent == 16 ~ "Michoacán",
            ent == 17 ~ "Morelos",
            ent == 18 ~ "Nayarit",
            ent == 19 ~ "Nuevo León",
            ent == 20 ~ "Oaxaca",
            ent == 21 ~ "Puebla",
            ent == 22 ~ "Querétaro",
            ent == 23 ~ "Quintana Roo",
            ent == 24 ~ "San Luis Potosí",
            ent == 25 ~ "Sinaloa",
            ent == 26 ~ "Sonora",
            ent == 27 ~ "Tabasco",
            ent == 28 ~ "Tamaulipas",
            ent == 29 ~ "Tlaxcala",
            ent == 30 ~ "Veracruz",
            ent == 31 ~ "Yucatán",
            ent == 32 ~ "Zacatecas",
            TRUE ~ "No especificado"
        ),
        entidad_abr_m = case_when(
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
            cve_ent == "32" ~ "ZAC"
        ),
        #id indicador
        id_dimension = "03",
        id_indicador = "44", 
        anio = 2024
    ) %>% 
    arrange(ent)

# Calcular valor nacional
df_nacional <- df_data %>% 
    as_survey_design(weights = factor) %>% 
    summarise(
        indicador_value = survey_mean(mas_2_horas, na.rm = T) * 100,
        n = unweighted(n())
    ) %>% 
    select(-ends_with("se")) %>% 
    mutate(
        cve_ent = "00",
        ent = 0,
        entidad = "Nacional",
        entidad_abr_m = "Nacional",
        id_dimension = "03",
        id_indicador = "44", 
        anio = 2024
    )

# Combinar resultados por estado con valor nacional
df_resultados <- df_resultados %>% 
    bind_rows(df_nacional)

df_resultados <- df_resultados %>% select(cve_ent, entidad_abr_m, anio, id_dimension, id_indicador, indicador_value)

# 3. Guardar resultados en drive ----------------------------------------------------------

# Obtener identificador de la base del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=39812926")[1, 2])

# Guardar en la base de Drive
googlesheets4::sheet_append(ss = v_id, data = df_resultados,
                            sheet = "03_44_tiempo_traslado")


# FIN. -------------------------------------------------------------------------
