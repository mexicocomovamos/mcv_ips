------------------------------------------------------------------------------#
    # Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
    # Objetivo:                   Mortalidad materna
    #
    # Encargada:                  Regina Isabel Medina Rosales     
    # Correos:                    regimedina19@gmail.com
    # 
    # Fecha de creación:          27 de octubre de 2021
    # Última actualización:       26 de octubre de 2022
    #------------------------------------------------------------------------------#
    
    # Fuente de mortalidad:    https://www.inegi.org.mx/sistemas/olap/proyectos/bd/continuas/mortalidad/mortalidadgeneral.asp?s=est&c=11144&proy=mortgral_mg#
# Fuente para nacimientos: https://www.inegi.org.mx/sistemas/olap/proyectos/bd/continuas/natalidad/nacimientos.asp?s=est&c=23699&proy=nat_nac

# Nota: Cuando se importan los tabulados del INEGI, se descargan de una manera
# dañada. Para solucionar el problema hay que abrirlos y volverlos a guardar
# con extensión .xlsx en vez de .xls. Esto se hace de manera manual y luego se
# importan a R. 

# Nota del 25/10/2022 (previo a la publicación de datos 2021):
# En 2021, hice las estimaciones según el año de registro de las defunciones. 
# En 2022, cambió a defunciones según año de ocurrencia. Guardé una copia de 
# las estimaciones originales realizadas en 2021 para comparar con las nuevas
# estimaciones. 

# Nota del 26/10/2022:
# Hoy se publican los datos de mortalidad para 2021 y se realiza la estimación 
# del IPS con la nueva limpieza, tomando año de ocurrencia. 

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
require(pacman)
p_load(readxl, tidyverse, dplyr, googledrive, 
       googlesheets4, lubridate, janitor, beepr)

# Desactiva notación científica
options(scipen=999)

# Vaciar espacio de trabajo 
# rm(list=ls())

# Colores MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados

# Funciones para directorio 
paste_inp    <- function(x){paste0("02_datos_crudos/01_01_mortalidad/"    , x)}
paste_out    <- function (x){paste0("02_bases_procesadas/00_01_mortalidad/", x)}

# ---- Activar las credenciales de google
# v_usuaria <- "regina"
# v_usuaria <- "katia"
v_usuaria <- "axel"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

# Verificar credenciales
googledrive::drive_user()
googlesheets4::gs4_user()

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x))}


# 1. Importar datos ------------------------------------------------------------

# Defunciones para calcular la razón de la mortalidad materna
# df_crudo <- readxl::read_xlsx("02_datos_crudos/01_01_mortalidad/INEGI_exporta_MORT_MATERNA.xlsx", 
#                               skip = 3) %>% 
#     rename(cve_ent = `...1`, 
#            nom_ent = `...2`) %>% 
#     filter(!is.na(nom_ent)) %>% 
#     mutate(cve_ent = ifelse(is.na(cve_ent), yes = "00", no = cve_ent)) %>% 
#     pivot_longer(cols = 3:ncol(.)) %>% 
#     mutate(value = str_remove_all(value, pattern = ",") %>% as.numeric())

MORT_MATERNA <- readxl::read_xlsx("02_datos_crudos/01_01_mortalidad/MORTALIDAD_MATERNA_REGISTRADO_OCURRIDO.xlsx", sheet = 2) %>%
     pivot_longer(3:ncol(.)) %>%
     mutate(value = str_remove_all(value, pattern = ",") %>% as.numeric()) %>%
     group_by(name, ocurrencia) %>%
     summarise(value = sum(value, na.rm = T)) %>%
     rename(year = ocurrencia,
            mort_materna = value,
            nom_ent = name)

 # df_crudo_nac <-
 #     readxl::read_xlsx("02_datos_crudos/01_01_mortalidad/NACIDOS_VIVOS_2.xls", skip = 4) %>%
 #     pivot_longer(5:ncol(.)) %>%
 #     rename(registro = `...3`,
 #            ocurrencia = `name`) %>%
 #     select(-1) %>%
 #     rename(nom_ent = `...2`) %>%
 #     select(nom_ent, registro, ocurrencia, Total, value) %>%
 #     mutate(across(.cols = Total:value, .fns = function(x){as.numeric(str_remove_all(x, pattern = ","))})) %>%
 #     group_by(nom_ent, ocurrencia) %>%
 #     summarise(value = sum(value, na.rm = T))

 NAC_VIVOS <- readxl::read_xlsx("02_datos_crudos/01_01_mortalidad/NACIDOS_VIVOS.xlsx") %>%
     rename(nom_ent = Entidad) %>%
     mutate(cve_entidad = str_pad(cve_entidad, side = "left", pad = "0", width = 2)) %>%
     pivot_longer(cols = 3:ncol(.)) %>%
     mutate(value = str_remove_all(value, pattern = ",") %>% as.numeric()) %>%
     rename(cve_ent = cve_entidad,
            year = name,
            nac_vivos = value)

 # dx <- left_join(df_crudo %>% rename(muertes_maternas = value),
 #           df_crudo_nac %>% rename(nacidos_vivos = value)) %>%
 #     mutate(razon = muertes_maternas/(nacidos_vivos/100000)) %>%
 #     filter(as.numeric(cve_ent) <= 32)

 
 # Si se usa este procedimiento el próximo año cambiar tabla de 
 # nacidos vivos registrados a nacidos vivos ocurridos
 # En IPS 2015-2023 se uso de base pob CONAPO de edad 0 años
 dx <- left_join(MORT_MATERNA,NAC_VIVOS) %>%
     filter(year >= 2002) %>%
     mutate(razon = mort_materna/(nac_vivos/100000)) %>%
     filter(as.numeric(cve_ent) <= 32) %>%
     mutate(nom_ent = ifelse(nom_ent == "Total", yes = "Nacional", no = nom_ent)) %>%
     mutate(cve_ent = ifelse(cve_ent == "Nacional", yes = "00", no = cve_ent))

unique(dx$nom_ent)

catalogo_abreviaturas <- read_csv("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/Datos/cat_edos.csv")

datos <- left_join(dx, catalogo_abreviaturas) %>%
     mutate(id_dimension = "01") %>%
     mutate(id_indicador = "02") %>%
     select(cve_ent,
            entidad_abr_m,
            anio = year,
            id_dimension,
            id_indicador,
            indicador_valor = razon)

openxlsx::write.xlsx(datos %>% arrange(anio, as.numeric(cve_ent)),
                      "02_datos_crudos/mortalidad_materna_valores.xlsx")

plt <- datos %>%
     ggplot(aes(x = anio, y = indicador_valor, group = nom_ent)) +
     geom_line()

plotly::ggplotly(plt)