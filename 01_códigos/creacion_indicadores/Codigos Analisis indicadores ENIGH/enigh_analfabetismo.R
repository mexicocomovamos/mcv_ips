#############################ENIGH########################################  

############################Alfabetismo##########################


##########Preparacion###############
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
### Setup ----
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notacion cientifica

# carga lista de librerías que necesitaremos
library(foreign)
# librería que nos ayuda a leer las tablas en diferentes formatos
library(doBy)
# librería que nos permite ordenar los datos de la tabla según el ingreso
library(reldist)
library(dplyr)
library(tidyr)
# carga la librería survey, ésta nos sirve para el cálculo del diseño muestral
library(srvyr)
library(writexl)

###########################2016###########################
# cargamos las bases de población y vivienda
setwd("J:/MCV/Pruebas/ENIGH/2016")

Pob<- read.dbf("./data/poblacion.dbf",as.is = T)

Viv <- read.dbf("./data/viviendas.dbf",as.is = T)


#Creamos las variables sobre población indígena y población discapacitada

Pob<-Pob%>%
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
                            cve_ent==09 ~ "Ciudad de México",
                            cve_ent==10 ~ "Durango",
                            cve_ent==11 ~ "Guanajuato",
                            cve_ent==12 ~ "Guerrero",
                            cve_ent==13 ~ "Hidalgo",
                            cve_ent==14 ~ "Jalisco",
                            cve_ent==15 ~ "México",
                            cve_ent==16 ~ "Michoacán de Ocampo",
                            cve_ent==17 ~ "Morelos",
                            cve_ent==18 ~ "Nayarit",
                            cve_ent==19 ~ "Nuevo León",
                            cve_ent==20 ~ "Oaxaca",
                            cve_ent==21 ~ "Puebla",
                            cve_ent==22 ~ "Querétaro",
                            cve_ent==23 ~ "Quintana Roo",
                            cve_ent==24 ~ "San Luis Potosí",
                            cve_ent==25 ~ "Sinaloa",
                            cve_ent==26 ~ "Sonora",
                            cve_ent==27 ~ "Tabasco",
                            cve_ent==28 ~ "Tamaulipas",
                            cve_ent==29 ~ "Tlaxcala",
                            cve_ent==30 ~ "Veracruz de Ignacio de la Llave",
                            cve_ent==31 ~ "Yucatán",
                            cve_ent==32 ~ "Zacatecas"))

#Analfabetismo
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(analfa = case_when(alfabetism == 2 ~ 1,
                            alfabetism == 1 ~ 0))

#Merge con la tabla de vivienda y hogares
colnames(Viv) <- tolower(colnames(Viv))

prov <- Viv %>%
  select(folioviv, factor, tot_resid)%>%
  mutate(tot_resid = as.numeric(tot_resid))


base <- Pob %>%
  left_join(prov,by=c("folioviv"))


#Analfabetismo
analfabetismo <- base%>% 
  drop_na(analfa) %>% 
  as_survey_design(weights = factor) %>% 
  #group_by(cve_ent) %>% 
  summarise(
    analfabetismo = survey_mean(analfa, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(analfabetismo,"./resultados/analfabetismo.xlsx")

###########################2018###########################
# cargamos las bases de población y vivienda
setwd("J:/MCV/Pruebas/ENIGH/2018")

Pob<- read.dbf("./data/poblacion.dbf",as.is = T)

Viv <- read.dbf("./data/viviendas.dbf",as.is = T)


#Creamos las variables sobre población indígena y población discapacitada

Pob<-Pob%>%
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
                            cve_ent==09 ~ "Ciudad de México",
                            cve_ent==10 ~ "Durango",
                            cve_ent==11 ~ "Guanajuato",
                            cve_ent==12 ~ "Guerrero",
                            cve_ent==13 ~ "Hidalgo",
                            cve_ent==14 ~ "Jalisco",
                            cve_ent==15 ~ "México",
                            cve_ent==16 ~ "Michoacán de Ocampo",
                            cve_ent==17 ~ "Morelos",
                            cve_ent==18 ~ "Nayarit",
                            cve_ent==19 ~ "Nuevo León",
                            cve_ent==20 ~ "Oaxaca",
                            cve_ent==21 ~ "Puebla",
                            cve_ent==22 ~ "Querétaro",
                            cve_ent==23 ~ "Quintana Roo",
                            cve_ent==24 ~ "San Luis Potosí",
                            cve_ent==25 ~ "Sinaloa",
                            cve_ent==26 ~ "Sonora",
                            cve_ent==27 ~ "Tabasco",
                            cve_ent==28 ~ "Tamaulipas",
                            cve_ent==29 ~ "Tlaxcala",
                            cve_ent==30 ~ "Veracruz de Ignacio de la Llave",
                            cve_ent==31 ~ "Yucatán",
                            cve_ent==32 ~ "Zacatecas"))

#Analfabetismo
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(analfa = case_when(alfabetism == 2 ~ 1,
                            alfabetism == 1 ~ 0))

#Merge con la tabla de vivienda y hogares
colnames(Viv) <- tolower(colnames(Viv))

prov <- Viv %>%
  select(folioviv, factor, tot_resid)%>%
  mutate(tot_resid = as.numeric(tot_resid))


base <- Pob %>%
  left_join(prov,by=c("folioviv"))


#Analfabetismo
analfabetismo <- base%>% 
  drop_na(analfa) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(cve_ent) %>% 
  summarise(
    analfabetismo = survey_mean(analfa, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(analfabetismo,"./resultados/analfabetismo.xlsx")



