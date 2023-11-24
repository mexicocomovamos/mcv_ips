#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Estimar PIB per cápita por entidad
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          26 de octubre  de 2022
# Última actualización:       1 de noviembre de 2022
#------------------------------------------------------------------------------#

# Fuente API del INEGI: https://www.inegi.org.mx/servicios/api_indicadores.html 

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
require(pacman)
p_load(readxl, inegiR, tidyverse, dplyr,
       googledrive, googlesheets4, lubridate, janitor, beepr)

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
inp <- "02_datos_crudos/"

# # Activar las credenciales de google
# v_usuaria <- "regina"
# # v_usuaria <- "katia"
# 
# googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
# googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
# 
# 
# # Verificar credenciales 
# googledrive::drive_user()
# googlesheets4::gs4_user() 
# 
# # Función para importar de manera más corta desde drive
# imp_dv <- function(x){
#     googlesheets4::read_sheet(
#         paste0("https://docs.google.com/spreadsheets/d/", x))}

# ----- Funciones de importación y exportación
paste_inp <- function(x){paste0("03_ips_clean/", x)}
paste_fig <- function(x){paste0(
    "05_infobites/00_en_cifras_ips/02_componentes/", x)}


# 1. Importar datos ------------------------------------------------------------

## 1.1. PIB Nacional -----------------------------------------------------------

# # Token para API del INEGI
v_token_inegi   <- "682ad7f9-19fe-47f0-abec-e4c2ab2f2948"
# # source(paste_code("00_token.R"))
# 
# # Identificadores de bases en la API del INEGI 
# v_desest        <- "493911"     # ID para serie desestacionalizada
# 
# # Base desestacionalizada
# df_gdp_des_raw  <- inegi_series(serie    = v_desest,
#                                 token    = v_token_inegi,
#                                 database = "BIE",
#                                 as_tt    = TRUE)

## 1.2. PIB Estatal ------------------------------------------------------------

# Estos son los códigos del PIB anual (van de 472080 a 472111, nacional: 472079)
v_ids <- c(
    "472080", "472081", "472082", "472083", "472084", "472085", "472086",
    "472087", "472088", "472089", "472090", "472091", "472092", "472093",
    "472094", "472095", "472096", "472097", "472098", "472099", "472100",
    "472101", "472102", "472103", "472104", "472105", "472106", "472107",
    "472108", "472109", "472110", "472111", "472079" # Nacional
)


## 1.3. Población CONAPO -------------------------------------------------------

load("02_datos_crudos/df_pop_state.Rdata") # Población total por entidad y edad

# ----- Funciones de importación y exportación
paste_inp <- function(x){paste0("03_ips_clean/", x)}
paste_fig <- function(x){paste0(
    "05_infobites/00_en_cifras_ips/02_componentes/", x)}


## 1.4. Catálogo entidades -----------------------------------------------------

df_entidades    <- read_excel("02_datos_crudos/00_cve_ent.xlsx")

v_entidades_abr <- df_entidades$entidad_abr_m
v_entidades_id  <- df_entidades$cve_ent

# 2. Procesamiento de datos ----------------------------------------------------

## 2.1. Bucle para entidades ---------------------------------------------------
### 2.1.1. PIBE con actividades petroleras -------------------------------------
# Crear data frame vacío para ir uniendo las bases procesadas
df_gdp_desest <- data.frame()

# Loop de limpieza de las bases procesadas 
# i = 1
for(i in 1:length(v_ids)){
    
    # Imprimir entidad 
    print(paste(i))
    
    # Importar base
    df_gdp_raw  <- inegi_series(
        serie    = v_ids[i],
        token    = v_token_inegi, 
        database = "BIE", 
        as_tt    = F) # Cambié aquí el argumento para poder importar
    
    # Procesar datos 
    df_gdp <- df_gdp_raw                            %>% 
        mutate(
            anio = as.numeric(date), 
            cve_ent       = v_entidades_id[i], 
            entidad_abr_m = v_entidades_abr[i]
            ) %>% 
        select(anio, cve_ent, entidad_abr_m, value_pib = values)
    
    # Unir en una sola base larga 
    df_gdp_desest <- df_gdp_desest %>% 
        bind_rows(df_gdp) %>% 
        filter(!is.na(cve_ent))
}

### 2.1.2. PIBE sin actividades petroleras -------------------------------------

# Juve: Descarga automatica: 
curl::curl_download(url = "https://www.inegi.org.mx/contenidos/temas/economia/pib/pibent/tabulados/ori/tabulados_pibent.zip",
    destfile =  "02_datos_crudos/00_PIBE_juve/pib_estatal.zip")
zip::unzip("02_datos_crudos/00_PIBE_juve/pib_estatal.zip", 
           exdir = "02_datos_crudos/00_PIBE_juve/nuevos")
v_ids = str_c(ifelse(str_length(0:32) == 1, yes = str_c("0", 0:32), no = 0:32),
              "_",
    list.files("02_datos_crudos/00_PIBE_juve/nuevos/", 
                         pattern = str_c(42:74, collapse = "|")))

# v_ids
# v_ids <- c(
#     "00_PIBE_42.xlsx",
#     "01_PIBE_43.xlsx",
#     "02_PIBE_44.xlsx",
#     "03_PIBE_45.xlsx",
#     "04_PIBE_46.xlsx",
#     "05_PIBE_47.xlsx",
#     "06_PIBE_48.xlsx",
#     "07_PIBE_49.xlsx",
#     "08_PIBE_50.xlsx",
#     "09_PIBE_51.xlsx",
#     "10_PIBE_52.xlsx",
#     "11_PIBE_53.xlsx",
#     "12_PIBE_54.xlsx",
#     "13_PIBE_55.xlsx",
#     "14_PIBE_56.xlsx",
#     "15_PIBE_57.xlsx",
#     "16_PIBE_58.xlsx",
#     "17_PIBE_59.xlsx",
#     "18_PIBE_60.xlsx",
#     "19_PIBE_61.xlsx",
#     "20_PIBE_62.xlsx",
#     "21_PIBE_63.xlsx",
#     "22_PIBE_64.xlsx",
#     "23_PIBE_65.xlsx",
#     "24_PIBE_66.xlsx",
#     "25_PIBE_67.xlsx",
#     "26_PIBE_68.xlsx",
#     "27_PIBE_69.xlsx",
#     "28_PIBE_70.xlsx",
#     "29_PIBE_71.xlsx",
#     "30_PIBE_72.xlsx",
#     "31_PIBE_73.xlsx",
#     "32_PIBE_74.xlsx"
# )

df_gdp_desest_sin_petrolera <- data.frame()
# i = 1
for(i in 1:length(v_ids)){
    
    print(v_ids[i])
    # tempo <- readxl::read_excel(paste0("02_datos_crudos/00_PIBE/", v_ids[i]), skip = 4) %>% 
    tempo <- readxl::read_excel(paste0("02_datos_crudos/00_PIBE_juve/nuevos/", v_ids[i] %>% str_remove_all(pattern = "^\\d\\d\\_")
                                       ), skip = 4) %>% 
        janitor::clean_names() %>% 
        filter(concepto == "Total" | concepto == "21-1 - Minería petrolera") %>% 
        distinct(concepto, .keep_all = T) %>% 
        mutate_at(
            vars(starts_with("x")),
            ~as.numeric(.)
        ) %>% 
        pivot_longer(
            starts_with("x"),
            names_to = "anio",
            names_prefix = "x",
            values_to = "value"
        ) %>% 
        mutate(
            cve_ent = str_sub(v_ids[i], 1, 2),
            anio = str_remove_all(anio, "[a-z]"),
            value = as.numeric(value)
        ) %>% 
        arrange(anio) %>% 
        group_by(anio) %>% 
        mutate(value_pib_sin_petrolera = value-lead(value)) %>% 
        drop_na(value_pib_sin_petrolera) %>% 
        select(cve_ent, anio, value_pib_sin_petrolera) %>% 
        glimpse
    
    df_gdp_desest_sin_petrolera <- bind_rows(df_gdp_desest_sin_petrolera, tempo)
    
    rm(tempo)
    
}


## 2.2. Limpiar base unida -----------------------------------------------------

df_pib <- df_gdp_desest                         %>% 
    filter(anio > 2002)                         %>% 
    # Etiquetar la serie
    mutate(tipo = "Serie desestacionalizada")   %>%
    bind_rows(
        df_gdp_desest_sin_petrolera %>% 
            mutate(anio = as.numeric(anio),
                   tipo = "Sin actividad petrolera") %>% 
            rename(value_pib = value_pib_sin_petrolera) %>% 
            left_join(df_entidades)
    ) %>% 
    arrange(cve_ent, anio)                      %>% 
    # Unir datos poblacionales
    left_join(
        df_pop_state %>% 
            mutate(cve_ent = str_pad(CVE_GEO, 2, pad = "0")) %>% 
            select(anio = year, cve_ent, state, population)
    ) %>% 
    # Estimar pib per cápita 
    mutate(value_pib_pc = value_pib/population*1000000) %>% 
    select(anio, cve_ent, entidad_abr_m, tipo, value_pib, pob = population, value_pib_pc)

openxlsx::write.xlsx(df_pib, "02_datos_crudos/03_pib_per_capita.xlsx")

## 2.3. Porcentaje de la población ---------------------------------------------

# Cargar IPS procesado
df_ips <- read_excel(paste_inp("08_ips_ranking.xlsx"))

# View(df_ips)

# Agregar datos poblacionales 
df_ips_pib_per_cap <- df_ips %>% 
    # Dejar solo una observación por cada entidad (IPS global)
    filter(id == "00") %>% 
    # Pegar datos de población 
    left_join(
        df_pop_state %>% 
            mutate(cve_ent = str_pad(CVE_GEO, 2, pad = "0")) %>% 
            select(anio = year, cve_ent, state, population)
    ) %>% 
    # Estimar población por rangos del PIB 
    group_by(grupo_pib) %>% 
    summarise(pob = sum(population)) %>% 
    ungroup() %>% 
    mutate(pob_porcentaje = pob/sum(pob))  %>% 
    rename(pob_total = pob)

openxlsx::write.xlsx(df_ips_pib_per_cap, 
                     file = "02_bases_procesadas/00_pob_pib_per_capita.xlsx")

# FIN. -------------------------------------------------------------------------

