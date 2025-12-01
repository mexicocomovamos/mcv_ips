#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Aprobación de matrimonio igualitario 
#
# Encargada:                  Sandra Martínez Peña   
# Correos:                    sandra@mexicocomovamos.com
# 
# Fecha de creación:          7 de noviembre de 2025
# Última actualización:       7 de noviembre de 2025
#------------------------------------------------------------------------------#

# Fuente: LAPOP
#NOTA: no se agrega la base al repo para no saturar, se puede descargar de esta liga
#Descargar base de datos: http://datasets.americasbarometer.org/database/index.php?freeUser=true
#cuestionario: http://datasets.americasbarometer.org/database/files/ABMex2023-Mexico-Questionnaire-V9.2.3.0-Spa-230511-W.pdf
#La pregunta de interés es la D6 "¿Con qué firmeza aprueba o desaprueba que las parejas del mismo sexo puedan tener el derecho a casarse?"
#La escala va de 1 Desaprueba firmemente a 10 Aprueba firmemente


# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Paquetes ----
library(pacman)

p_load(lubridate, zoo, scales, ggrepel, tidyverse, dplyr, ggplot2, httr, jsonlite, sf, mxmaps, leaflet, ggpubr, ggalluvial, ggflags,countrycode, survey)


v_usuaria <- "sandra"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# 1. Leer datos y obtener nombre de estado

d <- haven::read_dta("02_datos/MEX_2023_LAPOP_AmericasBarometer_v1.0_w.dta") %>% #modificar a directorio propio
    mutate(estado = as_factor(prov)) %>% 
    filter(!(d6 %in% c(888888, 988888))) #eliminamos no sabe / no responde 

# 2. Diseño muestral con wt
d_survey <- svydesign(
    ids = ~1,
    weights = ~wt,
    data = d
)

# 3. Porcentajes ponderados por estado
tabla_cruzada <- svytable(~estado + d6, d_survey)
porcentajes <- prop.table(tabla_cruzada, margin = 1) * 100

resultados <- as.data.frame(porcentajes) %>%
    pivot_wider(names_from = d6, values_from = Freq) %>%
    rename(Estado = estado)

# 4. Indicador por estado 
d_estados <- resultados %>%
    mutate(
        indicador_value = rowSums(across(`1`:`5`), na.rm = TRUE), #sumamos opciones de 1 a 5
        cve_ent = case_when(
            Estado == "Aguascalientes" ~ "01",
            Estado == "Baja California" ~ "02",
            Estado == "Baja California Sur" ~ "03",
            Estado == "Campeche" ~ "04",
            Estado == "Coahuila" ~ "05",
            Estado == "Colima" ~ "06",
            Estado == "Chiapas" ~ "07",
            Estado == "Chihuahua" ~ "08",
            Estado == "Distrito Federal" ~ "09",
            Estado == "Durango" ~ "10",
            Estado == "Guanajuato" ~ "11",
            Estado == "Guerrero" ~ "12",
            Estado == "Hidalgo" ~ "13",
            Estado == "Jalisco" ~ "14",
            Estado == "Estado de México" ~ "15",
            Estado == "Michoacán" ~ "16",
            Estado == "Morelos" ~ "17",
            Estado == "Nayarit" ~ "18",
            Estado == "Nuevo León" ~ "19",
            Estado == "Oaxaca" ~ "20",
            Estado == "Puebla" ~ "21",
            Estado == "Querétaro" ~ "22",
            Estado == "Quintana Roo" ~ "23",
            Estado == "San Luis Potosí" ~ "24",
            Estado == "Sinaloa" ~ "25",
            Estado == "Sonora" ~ "26",
            Estado == "Tabasco" ~ "27",
            Estado == "Tamaulipas" ~ "28",
            Estado == "Tlaxcala" ~ "29",
            Estado == "Veracruz" ~ "30",
            Estado == "Yucatán" ~ "31",
            Estado == "Zacatecas" ~ "32"
        ),
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
            cve_ent == "32" ~ "ZAC"
        ),
        id_dimension = "03",
        id_indicador = "47",
        anio = 2023
    ) %>%
    select(cve_ent, entidad_abr_m, anio, id_dimension, id_indicador, indicador_value)

# 5. Valor nacional
tabla_nacional <- svytable(~d6, d_survey)
porcentajes_nac <- prop.table(tabla_nacional) * 100
indicador_nacional <- sum(porcentajes_nac[c("1","2","3","4","5")]) #sumamos opciones 1 a 5 

df_nacional <- tibble(
    cve_ent = "00",
    entidad_abr_m = "Nacional",
    anio = 2023,
    id_dimension = "03",
    id_indicador = "47",
    indicador_value = indicador_nacional
)

# 6. Juntar nacional + estados
df_final <- bind_rows(df_nacional, d_estados)

df_final

#7- Juntar bases 

# Obtener identificador de la base del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=29386592")[1, 2])

# Guardar en la base de Drive - 03_47_inclusion_poblacion_indigena
googlesheets4::sheet_append(ss = v_id, data = df_final,
                            sheet = "03_47_inclusion_poblacion_gay")

# FIN. -------------------------------------------------------------------------
