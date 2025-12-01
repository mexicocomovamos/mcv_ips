#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Número de posgrados de calidad
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          10 de octubre de 2021
# Última actualización:       9 de octubre  de 2023
#------------------------------------------------------------------------------#

# Fuente 2022: Solicitud de acceso a la información con folio 330010922000496 
# Fuente 2023: Solicitud de acceso a la información con folio 330010923000734 
# Fuente 2024: Solicitud de acceso a la información con folio 330010924000525
# Fuente 2025: Solicitud de acceso a la información con folio 343235700002825

# El programa cambió en 2022
    # Antes: Programa Nacional de Posgrados de Calidad
    # Ahora Sistema Nacional de Posgrados de Conacyt
        #Debido al cambio de criterios para ingresar al SNP, el # de posgrados
        #incrementó de manera considerable en todas las entidades para 2022


# Nota: 
# - En 2023 entregaron la información en formato microdato, por lo que cambia 
# el flujo de procesamiento. Se dejó comentado el proceso de 2022.


# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
require(pacman)
p_load(readxl, tidyverse, dplyr, googledrive, googlesheets4, lubridate, janitor, beepr)

# Desactiva notación científica
options(scipen=999)

# Vaciar espacio de trabajo 
rm(list=ls())

# Vectores para directorio 
inp <- "02_datos_crudos/03_55_posgrados_calidad/"

# Activar las credenciales de google
v_usuaria <- "sandra"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# Función para importar de manera más corta desde drive
imp_dv <- function(x, ...){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x),
        ...
    )
}

# 1. Cargar datos --------------------------------------------------------------

# Datos de posgrados nacionales de calidad
#df_crudo <- read_excel(paste0(inp, "CA000-730-2022 - Anexo.xlsx"))
#Datos desde 2021 a 2023 del SNP:
#df_crudo  <- imp_dv("10I0C_qSfk0Ou8RVXZMBCPgmDumL1BlyUQ1W-IWUIaR0/edit#gid=0")
#Datos solicitados en 2024:
#df_crudo  <- imp_dv("1xHFp4pHJl_wPnKrcsJ2MkFW4nUZrLx1aSHwImduJBoM/edit?gid=0#gid=0")
#Datos solicitados en 2025: contienen info de 2021 a 2025 (segundo trimestre de 2025)
df_crudo  <- imp_dv("1JIndck15eMmv29Ms-8mh_30wyDjLqfIHJMKRfS2GT9w/edit?gid=0#gid=0", 
                    skip = 1, 
                    .name_repair = "minimal")

df_crudo <- df_crudo[-nrow(df_crudo),]

# Datos de población 
load("02_datos_crudos/df_pop_state.Rdata")

# 2. Procesamiento de datos ----------------------------------------------------

# Limpiar formato de base de población
df_pob <- df_pop_state %>% 
    ungroup() %>% 
    mutate(
        cve_ent = str_pad(CVE_GEO, width = 2, pad = "0")) |> 
    select(anio = year, cve_ent, pop = pop_tot)


#------- PROCESO 2022: Cambiar a formato largo y limpiar formato de datos y nombres 
#df_limpio <- df_crudo                                           |>
 #   rename(cve_ent = 1, entidad = 2)                            |>
    # Cambiar todas las columnas al mismo formato para poder hacer el pivot
  #  mutate_all(~as.character(.))                                |>
   # pivot_longer(cols      = -c(cve_ent, entidad),
    #             names_to  = "anio",
     #            values_to = "indicador")                       |>
    # Hacer NAs explícitos y cambiar a formato numérico
    #mutate(indicador = na_if(indicador, "-"))                   |>
    #mutate_at(.vars = c("anio", "indicador"), ~as.numeric(.))   |>
    # Limpiar formato del resto de variables
    #mutate(
    #    cve_ent = str_pad(cve_ent, width = 2, pad = "0"),
    #    id_dimension = "03",
    #    id_indicador = "55")

#--------- PROCESO  2024
 df_limpio <- df_crudo                       |> 
     rename(cve_ent = 1,entidad = 2)           |> 
     select(cve_ent, entidad, indicador = `2024`)            |> 
     mutate(anio = "2024") |> 
     #group_by(anio, entidad)                 |> 
     #summarise(indicador = n())              |>
             #id_dimension = "03",
             #id_indicador = "55")             |>
    # rename(entidad_abr_m = entidad)          |>
     # Ajuste abreviación de entidades
    # mutate(
     #    entidad_abr_m = if_else(
      #       entidad_abr_m == "COLI" , "COL", 
       #      ifelse(entidad_abr_m == "EMEX" , "MEX",
        #     ifelse(entidad_abr_m == "TAMP" , "TAM", entidad_abr_m)))) %>% 
     mutate(entidad_abr_m = recode(entidad,
                             "AGUASCALIENTES" = "AGS",
                             "BAJA CALIFORNIA" = "BC",
                             "BAJA CALIFORNIA SUR" = "BCS",
                             "CAMPECHE" = "CAMP",
                             "COAHUILA DE ZARAGOZA" = "COAH",
                             "COLIMA" = "COL",
                             "CHIAPAS" = "CHPS",
                             "CHIHUAHUA" = "CHIH",
                             "CIUDAD DE MEXICO" = "CDMX",
                             "DURANGO" = "DGO",
                             "GUANAJUATO" = "GTO",
                             "GUERRERO" = "GRO",
                             "HIDALGO" = "HGO",
                             "JALISCO" = "JAL",
                             "MEXICO" = "MEX",
                             "MICHOACAN DE OCAMPO" = "MICH",
                             "MORELOS" = "MOR",
                             "NAYARIT" = "NAY",
                             "NUEVO LEON" = "NL",
                             "OAXACA" = "OAX",
                             "PUEBLA" = "PUE",
                             "QUERETARO" = "QRO",
                             "QUINTANA ROO" = "QROO",
                             "SAN LUIS POTOSI" = "SLP",
                             "SINALOA" = "SIN",
                             "SONORA" = "SON",
                             "TABASCO" = "TAB",
                             "TAMAULIPAS" = "TAM",
                             "TLAXCALA" = "TLAX",
                             "VERACRUZ DE IGNACIO DE LA LLAVE" = "VER",
                             "YUCATAN" = "YUC",
                             "ZACATECAS" = "ZAC"), 
            id_dimension = "03",
            id_indicador = "55", 
            anio = as.numeric(anio)) %>% 
     # Limpiar formato del resto de variables
     mutate(
         cve_ent = case_when(
             entidad_abr_m == "AGS"  ~ "01",
             entidad_abr_m  == "BC"   ~ "02",
             entidad_abr_m  == "BCS"  ~ "03",
             entidad_abr_m  == "CDMX" ~ "09",
             entidad_abr_m  == "CHIH" ~ "08",
             entidad_abr_m  == "CHPS" ~ "07", 
             entidad_abr_m  == "COAH" ~ "05",
             entidad_abr_m  == "COL" ~ "06",
             entidad_abr_m  == "DGO"  ~ "10",
             entidad_abr_m  == "MEX" ~ "15",
             entidad_abr_m  == "GRO"  ~ "12",
             entidad_abr_m  == "GTO"  ~ "11",
             entidad_abr_m  == "HGO"  ~ "13",
             entidad_abr_m  == "JAL"  ~ "14",
             entidad_abr_m  == "MICH" ~ "16",
             entidad_abr_m  == "MOR"  ~ "17", 
             entidad_abr_m  == "NAY"  ~ "18", 
             entidad_abr_m  == "NL"   ~ "19",   
             entidad_abr_m  == "OAX"  ~ "20",  
             entidad_abr_m  == "PUE"  ~ "21", 
             entidad_abr_m  == "QRO"  ~ "22", 
             entidad_abr_m == "QROO" ~ "23", 
             entidad_abr_m  == "SIN"  ~ "25",  
             entidad_abr_m  == "SLP"  ~ "24",  
             entidad_abr_m  == "SON"  ~ "26",  
             entidad_abr_m  == "TAB"  ~ "27",  
             entidad_abr_m  == "TLAX" ~ "29", 
             entidad_abr_m  == "VER"  ~ "30", 
             entidad_abr_m  == "YUC"  ~ "31", 
             entidad_abr_m  == "ZAC"  ~ "32", 
             entidad_abr_m  == "CAMP" ~ "04", 
             entidad_abr_m  == "TAM" ~ "28"
         )) %>% 
     mutate(cve_ent = str_pad(cve_ent, width = 2, side = "left", pad = "0")) %>% 
     select(cve_ent, entidad_abr_m, anio, id_dimension, id_indicador, indicador)

# Agregar el total nacional y estimar indicador ponderado
df_data <- df_limpio                                            |> 
    group_by(anio, id_dimension, id_indicador)                  |> 
    summarise(indicador = sum(indicador, na.rm = T))            |> 
    mutate(cve_ent = "00", entidad_abr_m = "Nacional")          |>
    ungroup()                                                   |> 
    bind_rows(df_limpio)                                        |> 
    # Estimar indicador ponderado (tasa por cada 100,000 habitantes)
    left_join(df_pob)                                           |> 
    mutate(indicador_value = (indicador*100000/pop))            |>
    arrange(anio, cve_ent)                                      |> 
    # Seleccionar variables finales
    select(anio, cve_ent, entidad_abr_m,contains("id_"), indicador_value, drop_indicador_value = indicador)


# 3. Guardar en drive ----------------------------------------------------------

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1117300144")[1, 2])

# Guardar en la base en el Drive
googlesheets4::sheet_append(ss = v_id, data = df_data,
                           sheet = "03_55_posgrados_nacionales_calidad")

# FIN. -------------------------------------------------------------------------
