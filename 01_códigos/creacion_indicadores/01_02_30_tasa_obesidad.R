#------------------------------------------------------------------------------#
# Proyecto:               PREVALENCIA DE OBESIDAD
# Objetivo:               Base de datos prevalencia de Obesidad de persona menores de 60 años 
#
# Encargadas:             Samantha Contreras 
# Correos:                samantha@mexicocomovamos.mx 
# 
# Fecha de creación:      9 de octubre de 2023
# Última actualización:   9 de octubre de 2023
#------------------------------------------------------------------------------#

# FUENTE: 
# - Encuesta Nacional de Salud y Nutrición - ENSANUT
# --- Cuestionario de antropometría y tensión arterial


# 0. Configuración inicial -----------------------------------------------------
Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
# Cargar paquetería 
require(pacman)
require(srvyr)
p_load(readr, readxl, ggplot2, tidyverse, dplyr, writexl, beepr)
#if(!require("srvyr")) install.packages("srvyr") & require("srvyr")

# Vaciar espacio de trabajo 
rm(list=ls())

# Colores MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados

# Funciones de importación y exportación
paste_inp <- function(x){paste0("02_datos_crudos/02_30_tasa_obesidad/", x)}
paste_out <- function(x){paste0("02_bases_procesadas/"           , x)}

# Activar las credenciales de google
v_usuaria <- "samantha"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))


# 1. Cargar datos --------------------------------------------------------------

# Base 2020
df_20<- read_csv(paste_inp("antropometria_ensanut2020_w.csv"))

# Base 2021 
df_21  <- read_csv(paste_inp("ensaantro21_entrega_w_17_12_2021.csv"))

# Base 2022
df_22 <- read_delim(paste_inp("ensaantro2022_entrega_w.csv"), 
                      delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","), 
                    trim_ws = TRUE)

# 2. Procesar datos ------------------------------------------------------------

# Convertir variables a numérico, cambiar nombre y seleccionar variables 
#(nota: verificar nombre de variables).

df_20_selected <- df_20 %>%
    mutate(
        peso1 = as.numeric(sub(",", ".", as.character(an01_1))),
        peso2 = as.numeric(sub(",", ".", as.character(an01_2))),
        estatura1 = as.numeric(sub(",", ".", as.character(an04_01))),
        estatura2 = as.numeric(sub(",", ".", as.character(an04_02)))
    )%>%
    select(FOLIO_INT, ENTIDAD, DESC_ENT, peso1, peso2, estatura1, 
           estatura2, ponde_g)

df_21_selected <- df_21 %>%
    mutate(
        peso1 = as.numeric(sub(",", ".", as.character(an01_1))),
        peso2 = as.numeric(sub(",", ".", as.character(an01_2))),
        estatura1 = as.numeric(sub(",", ".", as.character(an04_1))),
        estatura2 = as.numeric(sub(",", ".", as.character(an04_2)))
    )%>%
    select(FOLIO_INT, entidad, desc_ent, peso1, peso2, estatura1, 
           estatura2, ponde_f)

df_22_selected <- df_22 %>%
    mutate(
        peso1 = as.numeric(sub(",", ".", as.character(an01_1))),
        peso2 = as.numeric(sub(",", ".", as.character(an01_2))),
        estatura1 = as.numeric(sub(",", ".", as.character(an04_1))),
        estatura2 = as.numeric(sub(",", ".", as.character(an04_2)))
    )%>%
    select(FOLIO_INT, entidad, desc_ent, peso1, peso2, estatura1, 
           estatura2, ponde_f)

#Hacer promedio de mediciones de peso y estatura
df_20_means <- df_20_selected %>%
    mutate(
        peso = (peso1 + peso2) / 2,
        estatura = (estatura1 + estatura2) / 2
    ) %>%
    select(-peso1, -peso2, -estatura1, -estatura2)

df_21_means <- df_21_selected %>%
    mutate(
        peso = (peso1 + peso2) / 2,
        estatura = (estatura1 + estatura2) / 2
    ) %>%
    select(-peso1, -peso2, -estatura1, -estatura2)
df_22_means <- df_22_selected %>%
    mutate(
        peso = (peso1 + peso2) / 2,
        estatura = (estatura1 + estatura2) / 2
    ) %>%
    select(-peso1, -peso2, -estatura1, -estatura2)

# Calcular IMC (Índice de Masa Corporal)
df_20_IMC <- df_20_means %>% mutate(IMC = peso / (estatura / 100)^2)
df_21_IMC <- df_21_means %>% mutate(IMC = peso / (estatura / 100)^2)
df_22_IMC <- df_22_means %>% mutate(IMC = peso / (estatura / 100)^2)

# Contar filas
nrow(df_20_IMC)
nrow(df_21_IMC)
nrow(df_22_IMC)

#Borrar las líneas con NaN
df_drop20 <- drop_na(df_20_IMC, c("IMC"))
df_drop21 <- drop_na(df_21_IMC, c("IMC"))
df_drop22 <- drop_na(df_22_IMC, c("IMC"))

# Contar filas
nrow(df_drop20)
nrow(df_drop21)
nrow(df_drop22)

# Crear un diagrama de caja para 'IMC'
ggplot(df_drop20, aes(y = IMC)) +
    geom_boxplot()
ggplot(df_drop21, aes(y = IMC)) +
    geom_boxplot()
ggplot(df_drop22, aes(y = IMC)) +
    geom_boxplot()

# Calcular Q1 y Q3
Q1_20 <- quantile(df_drop20$IMC, 0.25)
Q3_20 <- quantile(df_drop20$IMC, 0.75)

Q1_21 <- quantile(df_drop21$IMC, 0.25)
Q3_21 <- quantile(df_drop21$IMC, 0.75)

Q1_22 <- quantile(df_drop22$IMC, 0.25)
Q3_22 <- quantile(df_drop22$IMC, 0.75)

# Calcular el rango intercuartílico (IQR)
IQR_20 <- Q3_20 - Q1_20
IQR_21 <- Q3_21 - Q1_21
IQR_22 <- Q3_22 - Q1_22


# Identificar valores atípicos
limite_inferior_20 <- Q1_20 - 1.5 * IQR_20
limite_superior_20 <- Q3_20 + 1.5 * IQR_20

limite_inferior_21 <- Q1_21 - 1.5 * IQR_21
limite_superior_21 <- Q3_21 + 1.5 * IQR_21

limite_inferior_22 <- Q1_22 - 1.5 * IQR_22
limite_superior_22 <- Q3_22 + 1.5 * IQR_22

# Filtrar valores atípicos
datos_filtrados_20 <- df_drop20 %>% filter(IMC >= limite_inferior_20 & IMC <= limite_superior_20)
datos_filtrados_21 <- df_drop21 %>% filter(IMC >= limite_inferior_21 & IMC <= limite_superior_21)
datos_filtrados_22 <- df_drop22 %>% filter(IMC >= limite_inferior_22 & IMC <= limite_superior_22)

# Crear otro diagrama de caja para 'IMC' filtrado
ggplot(datos_filtrados_20, aes(y = IMC)) +
    geom_boxplot()
ggplot(datos_filtrados_21, aes(y = IMC)) +
    geom_boxplot()
ggplot(datos_filtrados_22, aes(y = IMC)) +
    geom_boxplot()

# Contar filas en datos filtrados
nrow(datos_filtrados_20)
nrow(datos_filtrados_21)
nrow(datos_filtrados_22)


# Agrupar por entidad y contar cuántos veces un IMC es igual o mayor a 30
df_20_IMCcount_byentidad <- datos_filtrados_20 %>%
    group_by(ENTIDAD, DESC_ENT) %>%
    summarise(indicador_value = sum(IMC >= 30))
df_21_IMCcount_byentidad <- datos_filtrados_21 %>%
    group_by(entidad, desc_ent) %>%
    summarise(indicador_value = sum(IMC >= 30))
df_22_IMCcount_byentidad <- datos_filtrados_22 %>%
    group_by(entidad, desc_ent) %>%
    summarise(indicador_value = sum(IMC >= 30))

# Definir diseño de muestra utilizando srvyr
designed_df_20 <- datos_filtrados_20 %>% 
    as_survey_design(weights = ponde_g, strata = "DESC_ENT")
designed_df_21 <- datos_filtrados_21 %>% 
    as_survey_design(weights = ponde_f, strata = "desc_ent")
designed_df_22 <- datos_filtrados_22 %>% 
    as_survey_design(weights = ponde_f, strata = "desc_ent")


# Calcular el porcentaje de IMC >= 30 en cada estrato, ponderado
result_ponde_20 <- designed_df_20 %>% 
    group_by(ENTIDAD, DESC_ENT) %>% 
    summarise(indicador_value = survey_mean(ifelse(IMC >= 30, 1, 0), vartype = "ci", na.rm = TRUE)) %>% 
    bind_rows(designed_df_20 %>% 
    summarise(ENTIDAD = "00", DESC_ENT = "00 NACIONAL", indicador_value = survey_mean(ifelse(IMC >= 30, 1, 0), vartype = "ci", na.rm = TRUE)))
result_ponde_21 <- designed_df_21 %>% 
    group_by(entidad, desc_ent) %>% 
    summarise(indicador_value = survey_mean(ifelse(IMC >= 30, 1, 0), vartype = "ci", na.rm = TRUE)) %>% 
    bind_rows(designed_df_21 %>% 
    summarise(entidad = "00", desc_ent = "00 NACIONAL", indicador_value = survey_mean(ifelse(IMC >= 30, 1, 0), vartype = "ci", na.rm = TRUE)))
result_ponde_22 <- designed_df_22 %>% 
    group_by(entidad, desc_ent) %>% 
    summarise(indicador_value = survey_mean(ifelse(IMC >= 30, 1, 0), vartype = "ci", na.rm = TRUE)) %>% 
    bind_rows(designed_df_22 %>% 
                  summarise(entidad = "00", desc_ent = "00 NACIONAL", indicador_value = survey_mean(ifelse(IMC >= 30, 1, 0), vartype = "ci", na.rm = TRUE)))

# Renombrar y agregar anio
result_ponde_20s <- rename(result_ponde_20, entidad = ENTIDAD, desc_ent = DESC_ENT)
result_ponde_20s <- mutate(result_ponde_20s, anio = 2020)
result_ponde_21s <- mutate(result_ponde_21, anio = 2021)
result_ponde_22s <- mutate(result_ponde_22, anio = 2022)

# Combine the data frames
df_unida <- bind_rows(result_ponde_20s, result_ponde_21s, result_ponde_22s)

# Guardar todos los nombres de entidades federativas para homologarlos 
v_entidad   <- unique(df_unida$entidad)

df_limpio    <- df_unida                            |> 
    filter(!is.na(entidad))                         |> 
    select(entidad, anio, indicador_value)          |> 
    # Agregar identificador del indicador
    mutate(
        id_dimension = "02", 
        id_indicador = "30")                        |> 
    # Generar identificador numérico 
    mutate(
        cve_ent = case_when(
            entidad == v_entidad[1]  ~ "01", 
            entidad == v_entidad[2]  ~ "02", 
            entidad == v_entidad[3]  ~ "03", 
            entidad == v_entidad[4]  ~ "04",
            entidad == v_entidad[5]  ~ "05", 
            entidad == v_entidad[6]  ~ "06", 
            entidad == v_entidad[7]  ~ "07", 
            entidad == v_entidad[8]  ~ "08", 
            entidad == v_entidad[9] ~ "09", 
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
            entidad == v_entidad[34] ~ "09", # Ciudad de México
            entidad == v_entidad[35] ~ "16", # Michoacán 
            entidad == v_entidad[36] ~ "20"  # Nuevo León
        )) |> 
    # Generar identificador abreviado 
    mutate(
        entidad_abr_m = case_when(
            entidad == "00" ~ "Nacional", 
            entidad == "01" ~ "AGS", 
            entidad == "02" ~ "BC",
            entidad == "03" ~ "BCS",
            entidad == "04" ~ "CAMP",
            entidad == "05" ~ "COAH",
            entidad == "06" ~ "COL", 
            entidad == "07" ~ "CHPS",
            entidad == "08" ~ "CHIH",
            entidad == "09" ~ "CDMX",
            entidad == "10" ~ "DGO",
            entidad == "11" ~ "GTO",
            entidad == "12" ~ "GRO", 
            entidad == "13" ~ "HGO",
            entidad == "14" ~ "JAL",
            entidad == "15" ~ "MEX",
            entidad == "16" ~ "MICH",
            entidad == "17" ~ "MOR", 
            entidad == "18" ~ "NAY",
            entidad == "19" ~ "NL",
            entidad == "20" ~ "OAX",
            entidad == "21" ~ "PUE",
            entidad == "22" ~ "QRO", 
            entidad == "23" ~ "QROO",
            entidad == "24" ~ "SLP",
            entidad == "25" ~ "SIN",
            entidad == "26" ~ "SON",
            entidad == "27" ~ "TAB", 
            entidad == "28" ~ "TAM",
            entidad == "29" ~ "TLAX",
            entidad == "30" ~ "VER",
            entidad == "31" ~ "YUC",
            entidad == "32" ~ "ZAC")) |> 
    ungroup() %>%
    # Seleccionar variables finales 
    select(cve_ent, entidad_abr_m, anio, id_dimension, id_indicador, indicador_value)

## 2.1. Guardar copia con procesamiento anterior -------------------------------
df_limpios <- df_limpio %>%
    mutate(indicador_value = indicador_value * 100)

df_tasa_obesidad <- df_limpios

#save(df_tasa_obesidad,
#     file = paste_out("01_02_30_tasa_obesidad.RData"))

## 2.2. Guardar en drive -------------------------------------------------------

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit?pli=1#gid=1660013474")[1, 2])

# Guardar en la base en el Drive
googlesheets4::range_write(ss = v_id, data = df_tasa_obesidad,
                           sheet = "02_30_tasa_obesidad",
                           range = "A68",  #líne68, columna A
                           col_names = FALSE)  # Ignorar el header del df

# FIN. -------------------------------------------------------------------------
