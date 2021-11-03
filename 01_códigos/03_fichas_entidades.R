#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Fichas por entidades 
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          29 de octubre   de 2021
# Última actualización:       03 de noviembre de 2021
#------------------------------------------------------------------------------#

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
inp_pib <- "02_datos_crudos/"
inp_ips <- "03_ips_clean/"

# Activar las credenciales de google
googledrive::drive_auth("regimedina19@gmail.com")
googlesheets4::gs4_auth("regimedina19@gmail.com")

# googledrive::drive_auth("katia@mexicocomovamos.mx")
# googlesheets4::gs4_auth("katia@mexicocomovamos.mx")

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x))}


# 1. Importar datos ------------------------------------------------------------

# PIB per cápita 
df_pibr <- read_excel(paste0(inp_pib, "03_pib_per_capita.xlsx"))

# Puntajes de las dimensiones del IPS 
df_ipsr <- read_excel(paste0(inp_ips, "00_IPS_COMPLETE_LONG.xlsx"))

# Puntajes de los indicadores (componentes) específicos del IPS
df_ips_indicadoresr <- read_excel(paste0(inp_ips, "01_ips_long.xlsx"))

v
# Clasificar grupos de entidades según el PIB (tres grupos)
df_pib  <- df_pibr                                          %>% 
    filter(cve_ent != "00", tipo_pib != "PIB per cápita")   %>% 
    arrange(desc(value_pib))                                %>% 
    mutate(
        grupo_pib = case_when(
                                 value_pib >  150000 ~ "PIB per cápita alto" , 
            value_pib >= 95000 & value_pib <= 150000 ~ "PIB per cápita medio", 
            value_pib <  95000                       ~ "PIB per cápita bajo"), 
        ranking_pib = rank(desc(value_pib)))

table(df_pib$grupo_pib)

# Unir IPS con PIB y hacer rankings (agregado a nivel dimensión)
df_ips <- df_ipsr                                           %>% 
    left_join(df_pib, by  = c("cve_ent"))                   %>% 
    # Dejar solo datos del año más reciente           
    filter(cve_ent != "00", anio == 2020)                   %>% 
    # Variables nacionales 
    group_by(anio, id_dim)                                  %>% 
    mutate(
        dim_mean_nacional = mean(dim_value), 
        dim_rank_nacional = rank(desc(dim_value)))          %>% 
    # Variables grupales (según grupo del pib de la entidad)            
    group_by(anio, grupo_pib, id_dim)                       %>% 
    mutate(
        dim_mean_grupal   = mean(dim_value), 
        dim_rank_grupal   = rank(desc(dim_value)), 
        dim_fort_deb_gr   = case_when(
            dim_value > (mean(dim_value) + sd(dim_value)) ~ "Desempeño superior", 
            dim_value < (mean(dim_value) - sd(dim_value)) ~ "Desempeño inferior", 
            T ~ "Desempeño esperado"), 
        dim_name = case_when(
            id_dim == "01" ~ "Necesidades Humanas Básicas", 
            id_dim == "02" ~ "Fundamentos del Bienestar", 
            id_dim == "03" ~ "Oportunidades", 
            id_dim == "00" ~ "Índice de Progreso Social"), 
        nivel = "Dimensión")                                %>% 
    ungroup() %>% 
    select(
        anio, cve_ent, entidad_abr_m, nivel, id = id_dim, name = dim_name, 
        value = dim_value, ips_mean_nacional = dim_mean_nacional, 
        ips_rank_nacional = dim_rank_nacional, grupo_pib, value_pib, ranking_pib, 
        ips_mean_grupal = dim_mean_grupal, ips_rank_grupal = dim_rank_grupal, 
        ips_fort_deb_gr = dim_fort_deb_gr)

# Controles de calidad
table(df_ips$dim_rank_nacional)
table(df_ips$dim_rank_grupal)
table(df_ips$dim_fort_deb_gr)

# Unir IPS con PIB y hacer rankings (agregado a nivel indicador/componente)
df_ips_indicadores <- df_ips_indicadoresr                   %>% 
    left_join(df_pib, by = c("cve_ent"))                    %>% 
    rename(ind_value     = indicador_value)                 %>% 
    # Dejar solo datos del año más reciente                 
    filter(cve_ent != "00", anio == 2020)                   %>% 
    # Varibales nacionales              
    group_by(anio, id_dim_ind)                              %>% 
    mutate(
        ind_mean_nacional = mean(ind_value), 
        ind_rank_nacional = rank(
            desc(ind_value),ties.method = "min"))           %>% 
    # Variables grupales (según grupo del pib)
    group_by(anio, id_dim_ind, grupo_pib)                   %>% 
    mutate(
        ind_mean_grupal   = mean(ind_value), 
        ind_rank_grupal   = rank(desc(ind_value),ties.method = "min"), 
        ind_fort_deb_gr   = case_when(
            ind_value > (mean(ind_value) + sd(ind_value)) ~ "Desempeño superior", 
            ind_value < (mean(ind_value) - sd(ind_value)) ~ "Desempeño inferior", 
            T ~ "Desempeño esperado"))                      %>% 
    ungroup() %>% 
    # Agregar nombre de los indicadores
    mutate(
        ind_name = case_when(
            id_dim_ind == "0101" ~ "Carencia por acceso a la alimentación", 
            id_dim_ind == "0102" ~ "Mortalidad materna (razón de muerte materna)", 
            id_dim_ind == "0103" ~ "Mortalidad infantil",
            id_dim_ind == "0104" ~ "Mortalidad por enfermedades infecciosas", 
            id_dim_ind == "0105" ~ "Hogares con disponibilidad de agua dentro de la vivienda", 
            id_dim_ind == "0106" ~ "Hogares con dotación diaria de agua",
            id_dim_ind == "0107" ~ "Hogares con servicio sanitario exclusivo para la vivienda", 
            id_dim_ind == "0108" ~ "Hogares con paredes material frágil", 
            id_dim_ind == "0109" ~ "Hogares con piso de tierra",
            id_dim_ind == "0110" ~ "Hogares que cocinan con leña o carbón", 
            id_dim_ind == "0111" ~ "Hogares en hacinamiento", 
            id_dim_ind == "0112" ~ "Homicidios (Tasa de homicidios por cada 100 mil habitantes)",
            id_dim_ind == "0113" ~ "Peligrosidad de accidentes de tránsito", 
            id_dim_ind == "0114" ~ "Crimen violento", 
            id_dim_ind == "0115" ~ "Crimen organizado",
            id_dim_ind == "0116" ~ "Percepción de inseguridad en la entidad", 
            id_dim_ind == "0217" ~ "Matriculación educación preescolar", 
            id_dim_ind == "0218" ~ "Analfabetismo",
            id_dim_ind == "0219" ~ "Matriculación educación primaria", 
            id_dim_ind == "0220" ~ "Matriculación educación secundaria", 
            id_dim_ind == "0221" ~ "Paridad de género en educación secundaria",
            id_dim_ind == "0222" ~ "Usuarios de telefonía móvil", 
            id_dim_ind == "0223" ~ "Hogares con computadoras", 
            id_dim_ind == "0224" ~ "Hogares con conexión a internet",
            id_dim_ind == "0225" ~ "Tasa de agresión a periodistas", 
            id_dim_ind == "0226" ~ "Esperanza de vida", 
            id_dim_ind == "0227" ~ "Tasa de suicidios",
            id_dim_ind == "0228" ~ "Mortalidad por enfermedades circulatorias", 
            id_dim_ind == "0229" ~ "Mortalidad por diabetes", 
            id_dim_ind == "0230" ~ "Tasa de obesidad",
            id_dim_ind == "0231" ~ "Grado de presión del agua", 
            id_dim_ind == "0232" ~ "Enterrar o quemar basura", 
            id_dim_ind == "0233" ~ "Satisfacción con áreas verdes",
            id_dim_ind == "0234" ~ "Uso de focos ahorradores", 
            id_dim_ind == "0335" ~ "Hogares con titulo de propiedad", 
            id_dim_ind == "0336" ~ "Participación electoral",
            id_dim_ind == "0337" ~ "Interacción con gobierno electrónico", 
            id_dim_ind == "0338" ~ "Participación ciudadana en el gobierno", 
            id_dim_ind == "0339" ~ "Percepción de corrupción en instituciones que imparten justicia",
            id_dim_ind == "0340" ~ "Jóvenes de 15 a 24 años que no estudian ni trabajan", 
            id_dim_ind == "0341" ~ "Embarazo adolescente", 
            id_dim_ind == "0342" ~ "Incidencia de corrupción",
            id_dim_ind == "0343" ~ "Informalidad laboral", 
            id_dim_ind == "0344" ~ "Porcentaje de la población ocupada que tarda más de dos horas en el traslado a su trabajo", 
            id_dim_ind == "0345" ~ "Confianza en los vecinos",
            id_dim_ind == "0346" ~ "Paridad de género en congresos locales", 
            id_dim_ind == "0347" ~ "Inclusión personas LGBT+ (% que aprueba el matrimonio igualitario)", 
            id_dim_ind == "0348" ~ "Tasa de analfabetización en personas indígenas",
            id_dim_ind == "0349" ~ "Tasa de analfabetización en personas con discapacidad", 
            id_dim_ind == "0350" ~ "Absorción en eduación superior", 
            id_dim_ind == "0351" ~ "Cobertura educación superior",
            id_dim_ind == "0352" ~ "Escolaridad promedio mujeres", 
            id_dim_ind == "0353" ~ "Paridad de género en posgrado", 
            id_dim_ind == "0354" ~ "Paridad de género en licenciatura",
            id_dim_ind == "0355" ~ "Posgrados nacionales de calidad"), 
        nivel = "Indicador") %>% 
    select(
        anio, cve_ent, entidad_abr_m, nivel, id = id_dim_ind, name = ind_name, 
        value = ind_value, ips_mean_nacional = ind_mean_nacional, 
        ips_rank_nacional = ind_rank_nacional, grupo_pib, value_pib, ranking_pib, 
        ips_mean_grupal = ind_mean_grupal, ips_rank_grupal = ind_rank_grupal, 
        ips_fort_deb_gr = ind_fort_deb_gr)


# Controles de calidad
table(df_ips_indicadores$ind_rank_nacional)
table(df_ips_indicadores$ind_rank_grupal)
table(df_ips_indicadores$ind_fort_deb_gr)

# Unir base de dimensiones e indicadores 

df_ranking_ips <- df_ips %>% 
    bind_rows(df_ips_indicadores) %>% 
    arrange(cve_ent)

# 3. Guardar -------------------------------------------------------------------

openxlsx::write.xlsx(df_ranking_ips, "03_ips_clean/08_ips_ranking.xlsx")

# FIN. -------------------------------------------------------------------------