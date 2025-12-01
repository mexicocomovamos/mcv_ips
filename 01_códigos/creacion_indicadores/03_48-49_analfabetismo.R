#------------------------------------------------------------------------------#
# Proyecto:             ÍNDICE DE PROGRESO SOCIAL
# Objetivo:             Porcentaje analfabetismo en personas índígenas y personas
#                       con discapacidad
#                       
# 
# Encargada:            Sandra Martínez Peña     
# Correos:              sandra@mexicocomovamos.mx
# 
# Fecha de creación:    3 de octubre de 2023
# Última actualización: 3 de octubre de 2023
#------------------------------------------------------------------------------#

#Creado tomando como base en el código elaborado por Axel Gómez (axel@mexicocomovamos.mx)

# Fuente: Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH). 2022 
#https://www.inegi.org.mx/programas/enigh/nc/2022/

#NOTA: como el repo está muy pesado las bases utilizadas en este código (poblacion y viviendas)
#no se encuentran en el repo

#PREGUNTA ALFABETISMO: ¿(NOMBRE) sabe leer y escribir? 
#          1 - sí 
#          2 - no 

#PREGUNTA INDÍGENA: De acuerdo con la cultura de (NOMBRE), ¿ella (él) se considera indígena?
#         1 - sí 
#         2 - no 

#PREGUNTA DISCAPACIDAD - Por algún problema de nacimiento o de salud ¿cuánta 
# dificultad tiene (NOMBRE) para...
#          1 - No puede hacerlo 
#          2 - Lo hace con mucha dificultad 
#          3 - Lo hace con poca dificultad 
#          4 - No tiene dificultad 

# 0. Configuración inicial -----------------------------------------------------

# Especificar zona horaria e idioma
Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
require(pacman)
p_load(readxl, tidyverse, dplyr, googledrive, 
       googlesheets4, lubridate, janitor, beepr, foreign, srvyr)

# Desactiva notación científica
options(scipen=999)

# Vaciar espacio de trabajo 
rm(list=ls())

# Funciones para directorio 
paste_inp <- function(x){paste0("02_datos_crudos/03_48-49_analfabetismo/", x)}

# Activar las credenciales de google
v_usuaria <- "sandra"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user() 

# 1. Importar datos ------------------------------------------------------------

df_crudo_pob <- read_csv(paste_inp("poblacion.csv"))
df_crudo_viv <- read_csv(paste_inp("viviendas.csv"))

# 2. Procesamiento de datos ----------------------------------------------------

df_pob <- df_crudo_pob %>%
    #Obtener la clave de entidad de la variable folioviv
    mutate(cve_ent =
               case_when(nchar(folioviv) == 9 ~ substr(folioviv,1,1),
                         TRUE ~substr(folioviv,1,2)),
           # Generar identificador abreviado 
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
               cve_ent == "32" ~ "ZAC")) 

#------------Analfabetismo

df_pob <- df_pob %>%
    #Obtener la clave de entidad de la variable folioviv
    mutate(analfa = case_when(alfabetism == 2 ~ 1,
                              alfabetism == 1 ~ 0))

#verificar
prov <- df_pob %>%
    select(analfa, alfabetism)

#-------------Discapacidad
df_pob<-   df_pob  %>%
    mutate(discapacidad = case_when(#caminar, subir o bajar usando sus piernas
        disc_camin > 0 & disc_camin <= 3 ~ 1,
        disc_camin == 4 ~ 0,
        #ver (aunque use lentes)
        disc_ver > 0 & disc_ver <= 3 ~ 1,
        disc_ver == 4 ~ 0,
        #mover o usar brazos o manos
        disc_brazo > 0 & disc_brazo <= 3 ~ 1,
        disc_brazo == 4 ~ 0,
        #aprender, recordar o concentrarse
        disc_apren > 0 & disc_apren <= 3 ~ 1,
        disc_apren == 4 ~ 0,
        #escuchar  
        disc_oir > 0 & disc_oir <= 3 ~ 1,
        disc_oir == 4 ~ 0,
        #bañarse, vestirse o comer
        disc_vest > 0 & disc_vest <= 3 ~ 1,
        disc_vest == 4 ~ 0,
        #hablar o comunicarse
        disc_habla > 0 & disc_habla <= 3 ~ 1,
        disc_habla == 4 ~ 0,
        #realizar actividades diarias por problemas mentales/emocionales
        disc_acti > 0 & disc_acti <= 3 ~ 1,
        disc_acti == 4 ~ 0
    ))

#verificar
prov <- df_pob %>%
    select(disc_camin, disc_ver, disc_brazo, disc_apren, disc_oir, disc_vest, disc_habla,disc_acti, discapacidad)


#---------------indígena
df_pob <- df_pob %>%
    mutate(ind = case_when(hablaind == 1  ~ 1,
                           etnia == 1  ~ 1,
                           hablaind == 2  ~ 0,
                           etnia == 2  ~ 0))

#verificar
prov <- df_pob %>%
    select(hablaind, etnia, ind)

# ----------------Indígena y analfabeta
df_pob <- df_pob %>%
    #Obtener la clave de entidad de la variable folioviv
    mutate(ind_analfa = case_when(ind == 1 & analfa ==1  ~ 1,
                                  TRUE ~ 0))

#verificar
prov <- df_pob %>%
    select(analfa, ind, ind_analfa)


#------------------Discapacidad y analfabeta
df_pob<- df_pob %>%
    #Obtener la clave de entidad de la variable folioviv
    mutate(disc_analfa = case_when(discapacidad == 1 & analfa ==1  ~ 1,
                                   TRUE ~ 0))

prov <- df_pob %>%
    select(analfa, discapacidad, disc_analfa)


#-------------Merge con la tabla de vivienda y hogares
colnames(df_crudo_viv) <- tolower(colnames(df_crudo_viv))

prov <- df_crudo_viv                                          %>%
    select(folioviv, factor, tot_resid)                       %>%
    mutate(tot_resid = as.numeric(tot_resid))


base <- df_pob                                                 %>%
    left_join(prov,by=c("folioviv", "factor"))                 %>%
    mutate(factorp = factor/as.numeric(tot_resid),
           numren = as.numeric(numren),
           res = case_when(numren >=1  ~ 1,
                           TRUE ~ 0))


prov <- base                                                    %>%
    group_by(folioviv)                                          %>% 
    summarise(
        totalres= sum(res, na.rm = T))


base <- base                                                     %>%
    left_join(prov,by=c("folioviv"))                             %>%
    mutate(factorp2 = factor/totalres)


prov <- base                                                     %>%
    select(tot_resid, res, numren, totalres)




#-----------------Indigenas analfabetas

df_indigenas <- base                                              %>% 
    drop_na(ind_analfa)                                           %>% 
    filter(ind == 1)                                              %>% 
    as_survey_design(weights = factor)                            %>% 
    # Agregar identificador del indicador
    mutate(id_dimension = "03",
           id_indicador = "48", 
           anio = 2024)                                           %>% 
    group_by(cve_ent, entidad_abr_m, anio, 
             id_dimension, id_indicador)                          %>% 
    summarise(
        indicador_value = (survey_mean(ind_analfa, na.rm = T)*100)
    )                                                             %>% 
    select(-ends_with("se")) 


#-----------------Discapacitados analfabetas
df_discapacitados <- base                                          %>% 
    drop_na(discapacidad)                                          %>% 
    filter(discapacidad == 1)                                      %>% 
    as_survey_design(weights = factor)                             %>% 
    # Agregar identificador del indicador
    mutate(id_dimension = "03",
           id_indicador = "49", 
           anio = 2024)                                            %>% 
    group_by(cve_ent, entidad_abr_m, anio, 
             id_dimension, id_indicador)                           %>% 
    summarise(
        indicador_value = (survey_mean(disc_analfa, na.rm = T)*100)
    )                                                              %>% 
    select(-ends_with("se")) 



#-----------------Indigenas analfabetas - TOTAL NACIONAL
df_indigenas_nacional <- base                                              %>% 
    drop_na(ind_analfa)                                           %>% 
    filter(ind == 1)                                              %>% 
    as_survey_design(weights = factor)                            %>% 
    # Agregar identificador del indicador
    mutate(id_dimension = "03",
           id_indicador = "48", 
           anio = 2024,
           cve_ent = "00",
           entidad_abr_m = "Nacional")                            %>% 
    group_by(cve_ent, entidad_abr_m, anio, 
             id_dimension, id_indicador)                          %>% 
    summarise(
        indicador_value = (survey_mean(ind_analfa, na.rm = T)*100)
    )                                                             %>% 
    select(-ends_with("se")) 

#-----------------Discapacitados analfabetas - TOTAL NACIONAL
df_discapacitados_nacional <- base                                          %>% 
    drop_na(discapacidad)                                          %>% 
    filter(discapacidad == 1)                                      %>% 
    as_survey_design(weights = factor)                             %>% 
    # Agregar identificador del indicador
    mutate(id_dimension = "03",
           id_indicador = "49", 
           anio = 2024,
           cve_ent = "00",
           entidad_abr_m = "Nacional")                             %>% 
    group_by(cve_ent, entidad_abr_m, anio, 
             id_dimension, id_indicador)                           %>% 
    summarise(
        indicador_value = (survey_mean(disc_analfa, na.rm = T)*100)
    )                                                              %>% 
    select(-ends_with("se")) 




# 3. Guardar en drive ----------------------------------------------------------

# Obtener identificador de la base del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=29386592")[1, 2])

# Guardar en la base de Drive - 03_48_inclusion_poblacion_indigena
googlesheets4::sheet_append(ss = v_id, data = df_indigenas,
                            sheet = "03_48_inclusion_poblacion_indigena")

# Guardar en la base de Drive - 03_49_inclusion_poblacion_discapacidad
googlesheets4::sheet_append(ss = v_id, data = df_discapacitados,
                            sheet = "03_49_inclusion_poblacion_discapacidad")

# FIN. -------------------------------------------------------------------------