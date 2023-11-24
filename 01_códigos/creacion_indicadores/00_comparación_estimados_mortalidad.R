#------------------------------------------------------------------------------#
# Proyecto:             ÍNDICE DE PROGRESO SOCIAL
# Objetivo:             Comparar estimaciones de mortalidad
#
# Encargada:            Regina Isabel Medina Rosales     
# Correos:              regina@mexicocomovamos.mx
# 
# Fecha de creación:    25 de octubre de 2022
# Última actualización: 26 de octubre de 2022
#------------------------------------------------------------------------------#

# En este código se contratan las estimaciones de mortalidad del IPS cuando 
# se toma el año de registro de la defunción contra el año de ocurrencia. 

# En la publicación de 2021 se tomaron datos con año de registro y en la de
# 2022 se reeemplzaron esas estimaciones con las de año de ocurrencia. 

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

# Funciones para directorio 
paste_inp <- function(x){paste0("02_datos_crudos/01_01_mortalidad/"    , x)}
paste_out <- function(x){paste0("02_bases_procesadas/00_01_mortalidad/", x)}

# 1. Cargar datos --------------------------------------------------------------

# ---- Cargar bases con estimaciones previas (publicación de 2021)
load(paste_out("df_prev_mortalidad_materna.RData"))
load(paste_out("df_prev_mortalidad_infantil.RData"))
load(paste_out("df_prev_mortalidad_infecciosa.RData"))
load(paste_out("df_prev_mortalidad_circulatorias.RData"))
load(paste_out("df_prev_mortalidad_diabetes.RData"))

# ---- Cargar bases con nuevas estimaciones (publicación de 2022)

load(paste_out("df_post_mortalidad_materna.RData"))
load(paste_out("df_post_mortalidad_infantil.RData"))
load(paste_out("df_post_mortalidad_infecciosa.RData"))
load(paste_out("df_post_mortalidad_circulatorias.RData"))
load(paste_out("df_post_mortalidad_diabetes.RData"))


# 2. Procesar datos ------------------------------------------------------------

# Unir todas las bases
df_mortalidad <- df_prev_mortalidad_materna                     %>% 
    mutate(tipo = "Materna", previo = indicador_value)          %>% 
    select(-indicador_value) %>% 
    full_join(
        df_post_mortalidad_materna %>% 
            mutate(tipo = "Materna", post = indicador_value) %>% 
            select(-indicador_value)
    ) %>% 
    # Mortalidad infantil 
    full_join(
        df_prev_mortalidad_infantil %>% 
            mutate(tipo = "Infantil", previo = indicador_value)  %>% 
            select(-indicador_value)
    ) %>% 
    full_join(
        df_post_mortalidad_infantil %>% 
            mutate(tipo = "Infantil", post = indicador_value) %>% 
            select(-indicador_value)
    ) %>% 
    # Enfermedades infecciosas 
    full_join(
        df_prev_mortalidad_infecciosa %>% 
            mutate(tipo = "Infecciosas", previo = indicador_value) %>% 
            select(-indicador_value)
    ) %>% 
    full_join(
        df_post_mortalidad_infecciosa %>% 
            mutate(tipo = "Infecciosas", post = indicador_value) %>% 
            select(-indicador_value)
    ) %>% 
    # Enfermedades circulatorias
    full_join(
        df_prev_mortalidad_circulatorias %>% 
            mutate(tipo = "Circulatorias", previo = indicador_value) %>% 
            select(-indicador_value)
    ) %>% 
    full_join(
        df_post_mortalidad_circulatorias %>% 
            mutate(tipo = "Circulatorias", post = indicador_value) %>% 
            select(-indicador_value)
    ) %>% 
    # Diabetes
    full_join(
        df_prev_mortalidad_diabetes %>% 
            mutate(tipo = "Diabetes", previo = indicador_value) %>% 
            select(-indicador_value)
    ) %>% 
    full_join(
        df_post_mortalidad_diabetes %>% 
            mutate(tipo = "Diabetes", post = indicador_value) %>% 
            select(-indicador_value)
    ) %>% 
    pivot_longer(
        cols = c(previo, post), 
        names_to = "tipo_indicador", 
        values_to = "indicador"
    ) %>% 
    select(anio, cve_ent, entidad_abr_m, contains("id_"), everything())


    
# 3. Visualizar ----------------------------------------------------------------

## 3.1. Nivel nacional ---------------------------------------------------------

df_data <- df_mortalidad %>% 
    filter(cve_ent == "00") %>% 
    mutate(id = paste(anio, tipo, tipo_indicador, sep = "_"), 
           tipo_indicador = if_else(
               tipo_indicador == "post", "Año de ocurrencia", "Año de registro")) 

ggplot(
    # Datos
    df_data, 
    # Coordenadas
       aes(x = anio, y = indicador, color = tipo_indicador, group = id)) +
    facet_wrap(~tipo, scales = "free_y") +
    # Geoms
    geom_line() +
    geom_point() +
    # Etiquetas
    labs(
        title = "Comparación entre estimaciones de mortalidad", 
        subtitle = "Tomando año de registro vs año de ocurrencia", 
        x = "\nAño del IPS", 
        y = "Tasa de defunciones por cada 100,000\n", 
        color = ""
    ) +
    # Tema 
    theme_bw() +
    theme(
        axis.text.x = element_text(angle = 90), 
        legend.position = "top"
    )

# FIN. -------------------------------------------------------------------------