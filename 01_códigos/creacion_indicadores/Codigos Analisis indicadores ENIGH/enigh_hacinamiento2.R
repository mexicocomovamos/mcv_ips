#############################ENIGH########################################  

############################Hacinamiento##########################


##########Preparacion###############
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
### Setup ----
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notacion cientifica

# carga lista de librer?as que necesitaremos
library(foreign)
# librer?a que nos ayuda a leer las tablas en diferentes formatos
library(doBy)
# librer?a que nos permite ordenar los datos de la tabla seg?n el ingreso
library(reldist)
library(dplyr)
library(tidyr)
# carga la librer?a survey, ?sta nos sirve para el c?lculo del dise?o muestral
library(srvyr)
library(writexl)
library(tidyverse)

###########################2012###########################
# cargamos las bases de hogares y vivienda
setwd("J:/MCV/Pruebas/ENIGH/2012")

hogares <- read.dbf("./data/ncv_hogares_2012_concil_2010_dbf.dbf",as.is = T)

Viv <- read.dbf("./data/NCV_viviendas_2012_concil_2010.dbf",as.is = T)


colnames(Viv) <- tolower(colnames(Viv))

Viv<-Viv%>%
  #Creamos indicador de hacinamiento
  mutate(hac=case_when((tot_resid / num_cuarto) > 2.5 ~ 1,
                       TRUE ~ 0))


#Base provisional
prov <- Viv %>%
  select(folioviv, hac, factor_viv)


#Pegado base vivienda y hogares
base <- hogares %>%
  left_join(prov,by=c("folioviv"))

base<-base%>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(cve_ent=
           case_when(nchar(folioviv)==9 ~ substr(folioviv,1,1),
                     TRUE ~substr(folioviv,1,2)),
         #Creamos nombre de entidad
         nom_ent= case_when(cve_ent==01 ~ "Aguascalientes",
                            cve_ent==02 ~ "Baja California",
                            cve_ent==03 ~ "Baja California Sur",
                            cve_ent==04 ~ "Campeche",
                            cve_ent==05 ~ "Coahuila de Zaragoza",
                            cve_ent==06 ~ "Colima",
                            cve_ent==07 ~ "Chiapas",
                            cve_ent==08 ~ "Chihuahua",
                            cve_ent==09 ~ "Ciudad de M?xico",
                            cve_ent==10 ~ "Durango",
                            cve_ent==11 ~ "Guanajuato",
                            cve_ent==12 ~ "Guerrero",
                            cve_ent==13 ~ "Hidalgo",
                            cve_ent==14 ~ "Jalisco",
                            cve_ent==15 ~ "M?xico",
                            cve_ent==16 ~ "Michoac?n de Ocampo",
                            cve_ent==17 ~ "Morelos",
                            cve_ent==18 ~ "Nayarit",
                            cve_ent==19 ~ "Nuevo Le?n",
                            cve_ent==20 ~ "Oaxaca",
                            cve_ent==21 ~ "Puebla",
                            cve_ent==22 ~ "Quer?taro",
                            cve_ent==23 ~ "Quintana Roo",
                            cve_ent==24 ~ "San Luis Potos?",
                            cve_ent==25 ~ "Sinaloa",
                            cve_ent==26 ~ "Sonora",
                            cve_ent==27 ~ "Tabasco",
                            cve_ent==28 ~ "Tamaulipas",
                            cve_ent==29 ~ "Tlaxcala",
                            cve_ent==30 ~ "Veracruz de Ignacio de la Llave",
                            cve_ent==31 ~ "Yucat?n",
                            cve_ent==32 ~ "Zacatecas"))

#Hogares con hacinamiento
hacinamiento <- base%>% 
  drop_na(hac) %>% 
  as_survey_design(weights = factor_hog) %>% 
  #group_by(cve_ent) %>% 
  summarise(
    hac = survey_mean(hac, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(hacinamiento,"./resultados/hacinamiento.xlsx")

###########################2014###########################
# cargamos las bases de poblaci?n y vivienda
setwd("J:/MCV/Pruebas/ENIGH/2014")

hogares <- read.dbf("./data/ncv_hogares_2014_concil_2010_dbf.dbf",as.is = T)

Viv <- read.dbf("./data/NCV_vivi_2014_concil_2010.dbf",as.is = T)

colnames(Viv) <- tolower(colnames(Viv))

Viv<-Viv%>%
  #Creamos indicador de hacinamiento
  mutate(hac=case_when((tot_resid / num_cuarto) > 2.5 ~ 1,
                       TRUE ~ 0))


#Base provisional
prov <- Viv %>%
  select(folioviv, hac, factor_viv)


#Pegado base vivienda y hogares
base <- hogares %>%
  left_join(prov,by=c("folioviv"))

base<-base%>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(cve_ent=
           case_when(nchar(folioviv)==9 ~ substr(folioviv,1,1),
                     TRUE ~substr(folioviv,1,2)),
         #Creamos nombre de entidad
         nom_ent= case_when(cve_ent==01 ~ "Aguascalientes",
                            cve_ent==02 ~ "Baja California",
                            cve_ent==03 ~ "Baja California Sur",
                            cve_ent==04 ~ "Campeche",
                            cve_ent==05 ~ "Coahuila de Zaragoza",
                            cve_ent==06 ~ "Colima",
                            cve_ent==07 ~ "Chiapas",
                            cve_ent==08 ~ "Chihuahua",
                            cve_ent==09 ~ "Ciudad de M?xico",
                            cve_ent==10 ~ "Durango",
                            cve_ent==11 ~ "Guanajuato",
                            cve_ent==12 ~ "Guerrero",
                            cve_ent==13 ~ "Hidalgo",
                            cve_ent==14 ~ "Jalisco",
                            cve_ent==15 ~ "M?xico",
                            cve_ent==16 ~ "Michoac?n de Ocampo",
                            cve_ent==17 ~ "Morelos",
                            cve_ent==18 ~ "Nayarit",
                            cve_ent==19 ~ "Nuevo Le?n",
                            cve_ent==20 ~ "Oaxaca",
                            cve_ent==21 ~ "Puebla",
                            cve_ent==22 ~ "Quer?taro",
                            cve_ent==23 ~ "Quintana Roo",
                            cve_ent==24 ~ "San Luis Potos?",
                            cve_ent==25 ~ "Sinaloa",
                            cve_ent==26 ~ "Sonora",
                            cve_ent==27 ~ "Tabasco",
                            cve_ent==28 ~ "Tamaulipas",
                            cve_ent==29 ~ "Tlaxcala",
                            cve_ent==30 ~ "Veracruz de Ignacio de la Llave",
                            cve_ent==31 ~ "Yucat?n",
                            cve_ent==32 ~ "Zacatecas"))

#Hogares con hacinamiento
hacinamiento <- base%>% 
  drop_na(hac) %>% 
  as_survey_design(weights = factor_hog) %>% 
  #group_by(cve_ent) %>% 
  summarise(
    hac = survey_mean(hac, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(hacinamiento,"./resultados/hacinamiento.xlsx")

###########################2016###########################
# cargamos las bases de poblaci?n y vivienda
setwd("J:/MCV/Pruebas/ENIGH/2016")

hogares <- read.dbf("./data/hogares.dbf",as.is = T)

Viv <- read.dbf("./data/viviendas.dbf",as.is = T)

colnames(Viv) <- tolower(colnames(Viv))

Viv<-Viv%>%
  #Creamos indicador de hacinamiento
  mutate(hac=case_when((tot_resid / num_cuarto) > 2.5 ~ 1,
                       TRUE ~ 0))


#Base provisional
prov <- Viv %>%
  select(folioviv, hac, factor)


#Pegado base vivienda y hogares
base <- hogares %>%
  left_join(prov,by=c("folioviv"))

base<-base%>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(cve_ent=
           case_when(nchar(folioviv)==9 ~ substr(folioviv,1,1),
                     TRUE ~substr(folioviv,1,2)),
         #Creamos nombre de entidad
         nom_ent= case_when(cve_ent==01 ~ "Aguascalientes",
                            cve_ent==02 ~ "Baja California",
                            cve_ent==03 ~ "Baja California Sur",
                            cve_ent==04 ~ "Campeche",
                            cve_ent==05 ~ "Coahuila de Zaragoza",
                            cve_ent==06 ~ "Colima",
                            cve_ent==07 ~ "Chiapas",
                            cve_ent==08 ~ "Chihuahua",
                            cve_ent==09 ~ "Ciudad de M?xico",
                            cve_ent==10 ~ "Durango",
                            cve_ent==11 ~ "Guanajuato",
                            cve_ent==12 ~ "Guerrero",
                            cve_ent==13 ~ "Hidalgo",
                            cve_ent==14 ~ "Jalisco",
                            cve_ent==15 ~ "M?xico",
                            cve_ent==16 ~ "Michoac?n de Ocampo",
                            cve_ent==17 ~ "Morelos",
                            cve_ent==18 ~ "Nayarit",
                            cve_ent==19 ~ "Nuevo Le?n",
                            cve_ent==20 ~ "Oaxaca",
                            cve_ent==21 ~ "Puebla",
                            cve_ent==22 ~ "Quer?taro",
                            cve_ent==23 ~ "Quintana Roo",
                            cve_ent==24 ~ "San Luis Potos?",
                            cve_ent==25 ~ "Sinaloa",
                            cve_ent==26 ~ "Sonora",
                            cve_ent==27 ~ "Tabasco",
                            cve_ent==28 ~ "Tamaulipas",
                            cve_ent==29 ~ "Tlaxcala",
                            cve_ent==30 ~ "Veracruz de Ignacio de la Llave",
                            cve_ent==31 ~ "Yucat?n",
                            cve_ent==32 ~ "Zacatecas"))

#Hogares con hacinamiento
hacinamiento <- base%>% 
  drop_na(hac) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(cve_ent) %>% 
  summarise(
    hac = survey_mean(hac, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(hacinamiento,"./resultados/hacinamiento.xlsx")

###########################2018###########################
# cargamos las bases de poblaci?n y vivienda
setwd("J:/MCV/Pruebas/ENIGH/2018")

hogares <- read.dbf("./data/hogares.dbf",as.is = T)

Viv <- read.dbf("./data/viviendas.dbf",as.is = T)

colnames(Viv) <- tolower(colnames(Viv))

Viv<-Viv%>%
  #Creamos indicador de hacinamiento
  mutate(hac=case_when((tot_resid / num_cuarto) > 2.5 ~ 1,
                       TRUE ~ 0))


#Base provisional
prov <- Viv %>%
  select(folioviv, hac, factor)


#Pegado base vivienda y hogares
base <- hogares %>%
  left_join(prov,by=c("folioviv"))

base<-base%>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(cve_ent=
           case_when(nchar(folioviv)==9 ~ substr(folioviv,1,1),
                     TRUE ~substr(folioviv,1,2)),
         #Creamos nombre de entidad
         nom_ent= case_when(cve_ent==01 ~ "Aguascalientes",
                            cve_ent==02 ~ "Baja California",
                            cve_ent==03 ~ "Baja California Sur",
                            cve_ent==04 ~ "Campeche",
                            cve_ent==05 ~ "Coahuila de Zaragoza",
                            cve_ent==06 ~ "Colima",
                            cve_ent==07 ~ "Chiapas",
                            cve_ent==08 ~ "Chihuahua",
                            cve_ent==09 ~ "Ciudad de M?xico",
                            cve_ent==10 ~ "Durango",
                            cve_ent==11 ~ "Guanajuato",
                            cve_ent==12 ~ "Guerrero",
                            cve_ent==13 ~ "Hidalgo",
                            cve_ent==14 ~ "Jalisco",
                            cve_ent==15 ~ "M?xico",
                            cve_ent==16 ~ "Michoac?n de Ocampo",
                            cve_ent==17 ~ "Morelos",
                            cve_ent==18 ~ "Nayarit",
                            cve_ent==19 ~ "Nuevo Le?n",
                            cve_ent==20 ~ "Oaxaca",
                            cve_ent==21 ~ "Puebla",
                            cve_ent==22 ~ "Quer?taro",
                            cve_ent==23 ~ "Quintana Roo",
                            cve_ent==24 ~ "San Luis Potos?",
                            cve_ent==25 ~ "Sinaloa",
                            cve_ent==26 ~ "Sonora",
                            cve_ent==27 ~ "Tabasco",
                            cve_ent==28 ~ "Tamaulipas",
                            cve_ent==29 ~ "Tlaxcala",
                            cve_ent==30 ~ "Veracruz de Ignacio de la Llave",
                            cve_ent==31 ~ "Yucat?n",
                            cve_ent==32 ~ "Zacatecas"))

#Hogares con hacinamiento
hacinamiento <- base%>% 
  drop_na(hac) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(cve_ent) %>% 
  summarise(
    hac = survey_mean(hac, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(hacinamiento,"./resultados/hacinamiento.xlsx")


###########################2020###########################
# cargamos las bases de poblaci?n y vivienda
setwd("J:/MCV/Pruebas/ENIGH/2020")

hogares <- read.dbf("./data/hogares.dbf",as.is = T)

Viv <- read.dbf("./data/viviendas.dbf",as.is = T)

colnames(Viv) <- tolower(colnames(Viv))

Viv<-Viv%>%
  #Creamos indicador de hacinamiento
  mutate(hac=case_when((tot_resid / num_cuarto) > 2.5 ~ 1,
                       TRUE ~ 0))


#Base provisional
prov <- Viv %>%
  select(folioviv, hac, factor)


#Pegado base vivienda y hogares
base <- hogares %>%
  left_join(prov,by=c("folioviv"))

base<-base%>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(cve_ent=
           case_when(nchar(folioviv)==9 ~ substr(folioviv,1,1),
                     TRUE ~substr(folioviv,1,2)),
         #Creamos nombre de entidad
         nom_ent= case_when(cve_ent==01 ~ "Aguascalientes",
                            cve_ent==02 ~ "Baja California",
                            cve_ent==03 ~ "Baja California Sur",
                            cve_ent==04 ~ "Campeche",
                            cve_ent==05 ~ "Coahuila de Zaragoza",
                            cve_ent==06 ~ "Colima",
                            cve_ent==07 ~ "Chiapas",
                            cve_ent==08 ~ "Chihuahua",
                            cve_ent==09 ~ "Ciudad de M?xico",
                            cve_ent==10 ~ "Durango",
                            cve_ent==11 ~ "Guanajuato",
                            cve_ent==12 ~ "Guerrero",
                            cve_ent==13 ~ "Hidalgo",
                            cve_ent==14 ~ "Jalisco",
                            cve_ent==15 ~ "M?xico",
                            cve_ent==16 ~ "Michoac?n de Ocampo",
                            cve_ent==17 ~ "Morelos",
                            cve_ent==18 ~ "Nayarit",
                            cve_ent==19 ~ "Nuevo Le?n",
                            cve_ent==20 ~ "Oaxaca",
                            cve_ent==21 ~ "Puebla",
                            cve_ent==22 ~ "Quer?taro",
                            cve_ent==23 ~ "Quintana Roo",
                            cve_ent==24 ~ "San Luis Potos?",
                            cve_ent==25 ~ "Sinaloa",
                            cve_ent==26 ~ "Sonora",
                            cve_ent==27 ~ "Tabasco",
                            cve_ent==28 ~ "Tamaulipas",
                            cve_ent==29 ~ "Tlaxcala",
                            cve_ent==30 ~ "Veracruz de Ignacio de la Llave",
                            cve_ent==31 ~ "Yucat?n",
                            cve_ent==32 ~ "Zacatecas"))

#Hogares con hacinamiento
hacinamiento <- base%>% 
  drop_na(hac) %>% 
  as_survey_design(weights = factor) %>% 
  #group_by(cve_ent) %>% 
  summarise(
    hac = survey_mean(hac, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(hacinamiento,"./resultados/hacinamiento.xlsx")


###########################2022###########################
# cargamos las bases de poblaci?n y vivienda
# setwd("J:/MCV/Pruebas/ENIGH/2020")

# hogares <- read.dbf("./data/hogares.dbf",as.is = T)
# Viv <- read.dbf("./data/viviendas.dbf",as.is = T)
hogares <- read_csv("/Volumes/Extreme\ SSD/DATASETS/INEGI\ -\ ENIGH/2022/hogares.csv")
Viv <- read_csv("/Volumes/Extreme\ SSD/DATASETS/INEGI\ -\ ENIGH/2022/viviendas.csv")

colnames(Viv) <- tolower(colnames(Viv))

Viv<-Viv%>%
    #Creamos indicador de hacinamiento
    mutate(hac=case_when((tot_resid / num_cuarto) > 2.5 ~ 1,
                         TRUE ~ 0))

#Base provisional
prov <- Viv %>%
    select(folioviv, hac)

#Pegado base vivienda y hogares
base <- hogares %>%
    left_join(prov,by=c("folioviv"))

base<-base%>%
    #Obtener la clave de entidad de la variable folioviv
    mutate(cve_ent=
               case_when(nchar(folioviv)==9 ~ substr(folioviv,1,1),
                         TRUE ~substr(folioviv,1,2)),
           #Creamos nombre de entidad
           nom_ent= case_when(cve_ent==01 ~ "Aguascalientes",
                              cve_ent==02 ~ "Baja California",
                              cve_ent==03 ~ "Baja California Sur",
                              cve_ent==04 ~ "Campeche",
                              cve_ent==05 ~ "Coahuila de Zaragoza",
                              cve_ent==06 ~ "Colima",
                              cve_ent==07 ~ "Chiapas",
                              cve_ent==08 ~ "Chihuahua",
                              cve_ent==09 ~ "Ciudad de M?xico",
                              cve_ent==10 ~ "Durango",
                              cve_ent==11 ~ "Guanajuato",
                              cve_ent==12 ~ "Guerrero",
                              cve_ent==13 ~ "Hidalgo",
                              cve_ent==14 ~ "Jalisco",
                              cve_ent==15 ~ "M?xico",
                              cve_ent==16 ~ "Michoac?n de Ocampo",
                              cve_ent==17 ~ "Morelos",
                              cve_ent==18 ~ "Nayarit",
                              cve_ent==19 ~ "Nuevo Le?n",
                              cve_ent==20 ~ "Oaxaca",
                              cve_ent==21 ~ "Puebla",
                              cve_ent==22 ~ "Quer?taro",
                              cve_ent==23 ~ "Quintana Roo",
                              cve_ent==24 ~ "San Luis Potos?",
                              cve_ent==25 ~ "Sinaloa",
                              cve_ent==26 ~ "Sonora",
                              cve_ent==27 ~ "Tabasco",
                              cve_ent==28 ~ "Tamaulipas",
                              cve_ent==29 ~ "Tlaxcala",
                              cve_ent==30 ~ "Veracruz de Ignacio de la Llave",
                              cve_ent==31 ~ "Yucat?n",
                              cve_ent==32 ~ "Zacatecas"))

names(base)

#Hogares con hacinamiento
hacinamiento <- base%>% 
    drop_na(hac) %>% 
    as_survey_design(
        # id=upm,
        # strata=est_dis,
        weights=factor, 
        # weights = factor
        ) %>% 
    group_by(cve_ent) %>%
    summarise(
        hac = survey_mean(hac, na.rm = T)
    ) %>% 
    select(-ends_with("se")) 

# Conteo... corroboramos
hacinamiento = base %>% 
    select(nom_ent, cve_ent, hac, factor) %>% 
    group_by(cve_ent, hac) %>% 
    summarise(suma = sum(factor)) %>% 
    pivot_wider(id_cols = "cve_ent", 
                names_from = "hac", 
                values_from = "suma") %>% 
    mutate(pp = 100*(`1`/(`1` + `0`)))

hacinamiento_nac <- base %>% 
    select(nom_ent, hac, factor) %>% 
    group_by(hac) %>% 
    summarise(suma = sum(factor)) %>% 
    mutate(cve_ent = "00") %>% 
    pivot_wider(id_cols = "cve_ent", 
                names_from = "hac", 
                values_from = "suma") %>% 
    mutate(pp = 100*(`1`/(`1` + `0`)))
hacinamiento_nac$pp

# Guardamos
write_xlsx(hacinamiento,"02_datos_crudos/hacinamiento.xlsx")
