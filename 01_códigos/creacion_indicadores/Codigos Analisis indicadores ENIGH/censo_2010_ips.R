#############################Censo#######################################  

############################Prueba##########################


##########Preparacion###############
rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
### Setup ----
Sys.setlocale("LC_ALL", "Spanish_Mexico.1252") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notacion cientifica

#Cargamos librerias

library(dplyr)
# carga la librería foreign la cual nos auxiliará para leer los datos en diferentes formatos (.dbf, .sav, etc.)
library(foreign)
# carga la librería survey, ésta nos sirve para el cálculo del diseño muestral
library(srvyr)
# carga la librería doBy que nos permite ordenar los datos de la tabla según el ingreso

#install.packages("doBy")
library(doBy)
# carga la librería reldist, ésta incluye la función para el cálculo del GINI
#install.packages("reldist")
library(reldist)

library("writexl")


###############################Internet###############################
#Definimos el directorio
setwd("J:/MCV/Pruebas/Censo/2010")

# abrimos la tabla concentradohogar
aguascalientes<- read.dbf("./data/Aguascalientes/Viviendas_01.dbf",as.is = T)
bc<- read.dbf("./data/Baja California/Viviendas_02.dbf",as.is = T)
bcs<- read.dbf("./data/Baja California Sur/Viviendas_03.dbf",as.is = T)
campeche<- read.dbf("./data/Campeche/Viviendas_04.dbf",as.is = T)
coahuila<- read.dbf("./data/Coahuila/Viviendas_05.dbf",as.is = T)
colima<- read.dbf("./data/Colima/Viviendas_06.dbf",as.is = T)
chiapas<- read.dbf("./data/Chiapas/Viviendas_07.dbf",as.is = T)
chihuhua<- read.dbf("./data/Chihuahua/Viviendas_08.dbf",as.is = T)
df<- read.dbf("./data/DF/Viviendas_09.dbf",as.is = T)
durango<- read.dbf("./data/Durango/Viviendas_10.dbf",as.is = T)
guanajuato<- read.dbf("./data/Guanajuato/Viviendas_11.dbf",as.is = T)
guerrero<- read.dbf("./data/guerrero/Viviendas_12.dbf",as.is = T)
hidalgo<- read.dbf("./data/Hidalgo/Viviendas_13.dbf",as.is = T)
jalisco<- read.dbf("./data/Jalisco/Viviendas_14.dbf",as.is = T)
mexico<- read.dbf("./data/Mexico/viviendas_15.dbf",as.is = T)
michoacan<- read.dbf("./data/Michoacan/Viviendas_16.dbf",as.is = T)
morelos<- read.dbf("./data/Morelos/Viviendas_17.dbf",as.is = T)
nayarit<- read.dbf("./data/Nayarit/Viviendas_18.dbf",as.is = T)
nl<- read.dbf("./data/Nuevo Leon/Viviendas_19.dbf",as.is = T)
oaxaca <- read.dbf("./data/Oaxaca/Viviendas_20.dbf",as.is = T)
puebla<- read.dbf("./data/Puebla/Viviendas_21.dbf",as.is = T)
queretaro<- read.dbf("./data/Queretaro/Viviendas_22.dbf",as.is = T)
qr<- read.dbf("./data/Quintana Roo/Viviendas_23.dbf",as.is = T)
slp<- read.dbf("./data/San Luis Potosi/Viviendas_24.dbf",as.is = T)
sinaloa<- read.dbf("./data/Sinaloa/Viviendas_25.dbf",as.is = T)
sonora<- read.dbf("./data/Sonora/Viviendas_26.dbf",as.is = T)
tabasco<- read.dbf("./data/Tabasco/Viviendas_27.dbf",as.is = T)
tamaulipas<- read.dbf("./data/Tamaulipas/Viviendas_28.dbf",as.is = T)
tlaxcala<- read.dbf("./data/Tlaxcala/Viviendas_29.dbf",as.is = T)
veracruz<- read.dbf("./data/Veracruz/Viviendas_30.dbf",as.is = T)
yucatan<- read.dbf("./data/Yucatan/Viviendas_31.dbf",as.is = T)
zacatecas<- read.dbf("./data/Zacatecas/Viviendas_32.dbf",as.is = T)

mexico <- mexico %>% 
            rename(TAM_LOC =tam_loc)

hogar <- rbind(aguascalientes,bc,bcs,campeche,coahuila,colima, chiapas, chihuhua, 
               df, durango, guanajuato, guerrero, hidalgo, jalisco, mexico, michoacan,
               morelos, nayarit,nl, oaxaca, puebla, queretaro,qr,slp,sinaloa, sonora,
               tabasco,tamaulipas, tlaxcala, veracruz,yucatan, zacatecas)



#Computadora

hogar<-hogar %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(compu = case_when(COMPU== 3 ~ 1,
                           COMPU == 4 ~ 0
                              
  ))

prov <-hogar %>%
  select(compu, COMPU, ENT)

#Hogares con COMPU
compun <- hogar%>% 
  drop_na(compu) %>% 
  as_survey_design(weights = FACTOR) %>% 
  #group_by(ENT) %>% 
  summarise(
    compu = mean(compu, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 


#Hogares con COMPU
compu <- hogar%>% 
  drop_na(compu) %>% 
  as_survey_design(weights = FACTOR) %>% 
  group_by(ENT) %>% 
  summarise(
    compu = mean(compu, na.rm = T)
  ) %>% 
  select(-ends_with("se"))  

write_xlsx(compu,"./resultados/compu_estados.xlsx")


#Internet
hogar<-hogar %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(internet = case_when(INTERNET == 1 ~ 1,
                              INTERNET == 2 ~ 0
                              
  ))

prov <-hogar %>%
  select(internet, INTERNET, ENT)

#Hogares con internet
internetn <- hogar%>% 
  drop_na(internet) %>% 
  as_survey_design(weights = FACTOR) %>% 
  #group_by(ENT) %>% 
  summarise(
    internet = mean(internet, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 


#Hogares con internet
internet <- hogar%>% 
  drop_na(internet) %>% 
  as_survey_design(weights = FACTOR) %>% 
  group_by(ENT) %>% 
  summarise(
    internet = mean(internet, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(internet,"./resultados/internet_estados.xlsx")

