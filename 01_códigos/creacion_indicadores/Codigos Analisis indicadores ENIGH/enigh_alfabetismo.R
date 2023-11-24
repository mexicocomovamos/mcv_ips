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

###########################2010###########################
# cargamos las bases de población y vivienda
setwd("J:/MCV/Pruebas/ENIGH/2010")

Pob<- read.dbf("./data/NCV_Poblacion_2010_concil_2010_DBF.dbf",as.is = T)

Viv <- read.dbf("./data/NCV_Hogares_2010_concil_2010_DBF.dbf",as.is = T)


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

#Anallfabetismo
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(analfa = case_when(alfabe == 2 ~ 1,
                              alfabe == 1 ~ 0))

prov <- Pob %>%
          select(analfa, alfabe)

#Discapacidad
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(discapacidad = case_when(disc1 > 0 & disc1 <= 7 ~ 1,
                                  disc2 > 0 & disc2 <= 7 ~ 1,
                                  disc3 > 0 & disc3 <= 7 ~ 1,
                                  disc4 > 0 & disc4 <= 7 ~ 1,
                                  disc5 > 0 & disc5 <= 7 ~ 1,
                                  disc6 > 0 & disc6 <= 7 ~ 1,
                                  disc7 > 0 & disc7 <= 7 ~ 1,
                                  disc1 == 8 ~ 0))

prov <- Pob %>%
  select(disc1, disc2, disc3, disc4, disc5, disc6, disc7, discapacidad)


#indígena
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(ind = case_when(hablaind == 1  ~ 1,
                         etnia == 1  ~ 1,
                         hablaind == 2  ~ 0,
                         etnia == 2  ~ 0))

prov <- Pob %>%
  select(hablaind, etnia, ind)

#Ind y analfabeta
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(indal = case_when(ind == 1 & analfa ==1  ~ 1,
                        TRUE ~ 0))

prov <- Pob %>%
  select(analfa, ind, indal)


#Disc y analfabeta
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(discal = case_when(discapacidad == 1 & analfa ==1  ~ 1,
                           TRUE ~ 0))

prov <- Pob %>%
  select(analfa, discapacidad, discal)


#Merge con la tabla de vivienda y hogares
prov <- Viv %>%
          select(folioviv, foliohog, factor, residentes)


base <- Pob %>%
          left_join(prov,by=c("folioviv","foliohog"))%>%
          mutate(factorp = factor/residentes,
                 numren = as.numeric(numren),
                 res = case_when(numren >=1  ~ 1,
                                 TRUE ~ 0))


prov <- base %>%
  group_by(folioviv,foliohog) %>% 
  summarise(
    totalres= sum(res, na.rm = T))
  

base <- base %>%
  left_join(prov,by=c("folioviv","foliohog"))%>%
  mutate(factorp2 = factor/totalres)


prov <- base %>%
  select(residentes, res, numren, totalres)




#Indigenas analfabetas
indigenas <- base%>% 
  drop_na(indal) %>% 
  filter(ind == 1) %>% 
  as_survey_design(weights = factorp2) %>% 
  group_by(cve_ent) %>% 
  summarise(
    ind = survey_mean(indal, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(indigenas,"./resultados/indigenas_analfabetas.xlsx")


#Discapacitados analfabetas
discapacitados <- base%>% 
  drop_na(discapacidad) %>% 
  filter(discapacidad == 1) %>% 
  as_survey_design(weights = factorp2) %>% 
  group_by(cve_ent) %>% 
  summarise(
    disc = survey_mean(discal, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(discapacitados,"./resultados/discapacidad_analfabetas.xlsx")


###########################2012###########################
# cargamos las bases de población y vivienda
setwd("J:/MCV/Pruebas/ENIGH/2012")

Pob<- read.dbf("./data/ncv_poblacion_2012_concil_2010_dbf.dbf",as.is = T)

Viv <- read.dbf("./data/NCV_viviendas_2012_concil_2010.dbf",as.is = T)


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

prov <- Pob %>%
  select(analfa, alfabetism)

#Discapacidad
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(discapacidad = case_when(disc1 > 0 & disc1 <= 7 ~ 1,
                                  disc2 > 0 & disc2 <= 7 ~ 1,
                                  disc3 > 0 & disc3 <= 7 ~ 1,
                                  disc4 > 0 & disc4 <= 7 ~ 1,
                                  disc5 > 0 & disc5 <= 7 ~ 1,
                                  disc6 > 0 & disc6 <= 7 ~ 1,
                                  disc7 > 0 & disc7 <= 7 ~ 1,
                                  disc1 == 8 ~ 0))

prov <- Pob %>%
  select(disc1, disc2, disc3, disc4, disc5, disc6, disc7, discapacidad)


#indígena
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(ind = case_when(hablaind == 1  ~ 1,
                         etnia == 1  ~ 1,
                         hablaind == 2  ~ 0,
                         etnia == 2  ~ 0))

prov <- Pob %>%
  select(hablaind, etnia, ind)

#Ind y analfabeta
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(indal = case_when(ind == 1 & analfa ==1  ~ 1,
                           TRUE ~ 0))

prov <- Pob %>%
  select(analfa, ind, indal)


#Disc y analfabeta
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(discal = case_when(discapacidad == 1 & analfa ==1  ~ 1,
                            TRUE ~ 0))

prov <- Pob %>%
  select(analfa, discapacidad, discal)


#Merge con la tabla de vivienda y hogares
colnames(Viv) <- tolower(colnames(Viv))

prov <- Viv %>%
  select(folioviv, factor_viv, tot_resid)%>%
  mutate(tot_resid = as.numeric(tot_resid))


base <- Pob %>%
  left_join(prov,by=c("folioviv"))%>%
  mutate(tot_resid = as.numeric(tot_resid)) %>%
  mutate(factorp = factor_viv/as.numeric(tot_resid),
         numren = as.numeric(numren),
         res = case_when(numren >=1  ~ 1,
                         TRUE ~ 0))


prov <- base %>%
  group_by(folioviv) %>% 
  summarise(
    totalres= sum(res, na.rm = T))


base <- base %>%
  left_join(prov,by=c("folioviv"))%>%
  mutate(factorp2 = factor_viv/totalres)


prov <- base %>%
  select(tot_resid, res, numren, totalres)




#Indigenas analfabetas
indigenas <- base%>% 
  drop_na(indal) %>% 
  filter(ind == 1) %>% 
  as_survey_design(weights = factorp2) %>% 
  group_by(cve_ent) %>% 
  summarise(
    ind = survey_mean(indal, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(indigenas,"./resultados/indigenas_analfabetas.xlsx")


#Discapacitados analfabetas
discapacitados <- base%>% 
  drop_na(discapacidad) %>% 
  filter(discapacidad == 1) %>% 
  as_survey_design(weights = factorp2) %>% 
  #group_by(cve_ent) %>% 
  summarise(
    disc = survey_mean(discal, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(discapacitados,"./resultados/discapacidad_analfabetas.xlsx")


###########################2014###########################
# cargamos las bases de población y vivienda
setwd("J:/MCV/Pruebas/ENIGH/2014")

Pob<- read.dbf("./data/ncv_poblacion_2014_concil_2010_dbf.dbf",as.is = T)

Viv <- read.dbf("./data/NCV_vivi_2014_concil_2010.dbf",as.is = T)


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

prov <- Pob %>%
  select(analfa, alfabetism)

#Discapacidad
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(discapacidad = case_when(disc1 > 0 & disc1 <= 7 ~ 1,
                                  disc2 > 0 & disc2 <= 7 ~ 1,
                                  disc3 > 0 & disc3 <= 7 ~ 1,
                                  disc4 > 0 & disc4 <= 7 ~ 1,
                                  disc5 > 0 & disc5 <= 7 ~ 1,
                                  disc6 > 0 & disc6 <= 7 ~ 1,
                                  disc7 > 0 & disc7 <= 7 ~ 1,
                                  disc1 == 8 ~ 0))

prov <- Pob %>%
  select(disc1, disc2, disc3, disc4, disc5, disc6, disc7, discapacidad)


#indígena
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(ind = case_when(hablaind == 1  ~ 1,
                         etnia == 1  ~ 1,
                         hablaind == 2  ~ 0,
                         etnia == 2  ~ 0))

prov <- Pob %>%
  select(hablaind, etnia, ind)

#Ind y analfabeta
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(indal = case_when(ind == 1 & analfa ==1  ~ 1,
                           TRUE ~ 0))

prov <- Pob %>%
  select(analfa, ind, indal)


#Disc y analfabeta
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(discal = case_when(discapacidad == 1 & analfa ==1  ~ 1,
                            TRUE ~ 0))

prov <- Pob %>%
  select(analfa, discapacidad, discal)


#Merge con la tabla de vivienda y hogares
colnames(Viv) <- tolower(colnames(Viv))

prov <- Viv %>%
  select(folioviv, factor_viv, tot_resid)%>%
  mutate(tot_resid = as.numeric(tot_resid))


base <- Pob %>%
  left_join(prov,by=c("folioviv"))%>%
  mutate(tot_resid = as.numeric(tot_resid)) %>%
  mutate(factorp = factor_viv/as.numeric(tot_resid),
         numren = as.numeric(numren),
         res = case_when(numren >=1  ~ 1,
                         TRUE ~ 0))


prov <- base %>%
  group_by(folioviv) %>% 
  summarise(
    totalres= sum(res, na.rm = T))


base <- base %>%
  left_join(prov,by=c("folioviv"))%>%
  mutate(factorp2 = factor_viv/totalres)


prov <- base %>%
  select(tot_resid, res, numren, totalres)




#Indigenas analfabetas
indigenas <- base%>% 
  drop_na(indal) %>% 
  filter(ind == 1) %>% 
  as_survey_design(weights = factorp2) %>% 
  group_by(cve_ent) %>% 
  summarise(
    ind = survey_mean(indal, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(indigenas,"./resultados/indigenas_analfabetas.xlsx")


#Discapacitados analfabetas
discapacitados <- base%>% 
  drop_na(discapacidad) %>% 
  filter(discapacidad == 1) %>% 
  as_survey_design(weights = factorp2) %>% 
  group_by(cve_ent) %>% 
  summarise(
    disc = survey_mean(discal, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(discapacitados,"./resultados/discapacidad_analfabetas.xlsx")

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

prov <- Pob %>%
  select(analfa, alfabetism)

#Discapacidad
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(discapacidad = case_when(disc1 > 0 & disc1 <= 7 ~ 1,
                                  disc2 > 0 & disc2 <= 7 ~ 1,
                                  disc3 > 0 & disc3 <= 7 ~ 1,
                                  disc4 > 0 & disc4 <= 7 ~ 1,
                                  disc5 > 0 & disc5 <= 7 ~ 1,
                                  disc6 > 0 & disc6 <= 7 ~ 1,
                                  disc7 > 0 & disc7 <= 7 ~ 1,
                                  disc1 == 8 ~ 0))

prov <- Pob %>%
  select(disc1, disc2, disc3, disc4, disc5, disc6, disc7, discapacidad)


#indígena
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(ind = case_when(hablaind == 1  ~ 1,
                         etnia == 1  ~ 1,
                         hablaind == 2  ~ 0,
                         etnia == 2  ~ 0))

prov <- Pob %>%
  select(hablaind, etnia, ind)

#Ind y analfabeta
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(indal = case_when(ind == 1 & analfa ==1  ~ 1,
                           TRUE ~ 0))

prov <- Pob %>%
  select(analfa, ind, indal)


#Disc y analfabeta
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(discal = case_when(discapacidad == 1 & analfa ==1  ~ 1,
                            TRUE ~ 0))

prov <- Pob %>%
  select(analfa, discapacidad, discal)


#Merge con la tabla de vivienda y hogares
colnames(Viv) <- tolower(colnames(Viv))

prov <- Viv %>%
  select(folioviv, factor, tot_resid)%>%
  mutate(tot_resid = as.numeric(tot_resid))


base <- Pob %>%
  left_join(prov,by=c("folioviv"))%>%
  mutate(tot_resid = as.numeric(tot_resid)) %>%
  mutate(factorp = factor/as.numeric(tot_resid),
         numren = as.numeric(numren),
         res = case_when(numren >=1  ~ 1,
                         TRUE ~ 0))


prov <- base %>%
  group_by(folioviv) %>% 
  summarise(
    totalres= sum(res, na.rm = T))


base <- base %>%
  left_join(prov,by=c("folioviv"))%>%
  mutate(factorp2 = factor/totalres)


prov <- base %>%
  select(tot_resid, res, numren, totalres)




#Indigenas analfabetas
indigenas <- base%>% 
  drop_na(indal) %>% 
  filter(ind == 1) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(cve_ent) %>% 
  summarise(
    ind = survey_mean(indal, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(indigenas,"./resultados/indigenas_analfabetas.xlsx")


#Discapacitados analfabetas
discapacitados <- base%>% 
  drop_na(discapacidad) %>% 
  filter(discapacidad == 1) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(cve_ent) %>% 
  summarise(
    disc = survey_mean(discal, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(discapacitados,"./resultados/discapacidad_analfabetas.xlsx")


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

prov <- Pob %>%
  select(analfa, alfabetism)

#Discapacidad
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(discapacidad = case_when(disc1 > 0 & disc1 <= 7 ~ 1,
                                  disc2 > 0 & disc2 <= 7 ~ 1,
                                  disc3 > 0 & disc3 <= 7 ~ 1,
                                  disc4 > 0 & disc4 <= 7 ~ 1,
                                  disc5 > 0 & disc5 <= 7 ~ 1,
                                  disc6 > 0 & disc6 <= 7 ~ 1,
                                  disc7 > 0 & disc7 <= 7 ~ 1,
                                  disc1 == 8 ~ 0))

prov <- Pob %>%
  select(disc1, disc2, disc3, disc4, disc5, disc6, disc7, discapacidad)


#indígena
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(ind = case_when(hablaind == 1  ~ 1,
                         etnia == 1  ~ 1,
                         hablaind == 2  ~ 0,
                         etnia == 2  ~ 0))

prov <- Pob %>%
  select(hablaind, etnia, ind)

#Ind y analfabeta
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(indal = case_when(ind == 1 & analfa ==1  ~ 1,
                           TRUE ~ 0))

prov <- Pob %>%
  select(analfa, ind, indal)


#Disc y analfabeta
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(discal = case_when(discapacidad == 1 & analfa ==1  ~ 1,
                            TRUE ~ 0))

prov <- Pob %>%
  select(analfa, discapacidad, discal)


#Merge con la tabla de vivienda y hogares
colnames(Viv) <- tolower(colnames(Viv))

prov <- Viv %>%
  select(folioviv, factor, tot_resid)%>%
  mutate(tot_resid = as.numeric(tot_resid))


base <- Pob %>%
  left_join(prov,by=c("folioviv"))%>%
  mutate(tot_resid = as.numeric(tot_resid)) %>%
  mutate(factorp = factor/as.numeric(tot_resid),
         numren = as.numeric(numren),
         res = case_when(numren >=1  ~ 1,
                         TRUE ~ 0))


prov <- base %>%
  group_by(folioviv) %>% 
  summarise(
    totalres= sum(res, na.rm = T))


base <- base %>%
  left_join(prov,by=c("folioviv"))%>%
  mutate(factorp2 = factor/totalres)


prov <- base %>%
  select(tot_resid, res, numren, totalres)




#Indigenas analfabetas
indigenas <- base%>% 
  drop_na(indal) %>% 
  filter(ind == 1) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(cve_ent) %>% 
  summarise(
    ind = survey_mean(indal, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(indigenas,"./resultados/indigenas_analfabetas.xlsx")


#Discapacitados analfabetas
discapacitados <- base%>% 
  drop_na(discapacidad) %>% 
  filter(discapacidad == 1) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(cve_ent) %>% 
  summarise(
    disc = survey_mean(discal, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(discapacitados,"./resultados/discapacidad_analfabetas.xlsx")

###########################2020###########################
# cargamos las bases de población y vivienda
setwd("J:/MCV/Pruebas/ENIGH/2020")

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

prov <- Pob %>%
  select(analfa, alfabetism)

#Discapacidad
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(discapacidad = case_when(disc_camin > 0 & disc_camin <= 3 ~ 1,
                                  disc_camin == 4 ~ 0,
                                  disc_ver > 0 & disc_ver <= 3 ~ 1,
                                  disc_ver == 4 ~ 0,
                                  disc_brazo > 0 & disc_brazo <= 3 ~ 1,
                                  disc_brazo == 4 ~ 0,
                                  disc_apren > 0 & disc_apren <= 3 ~ 1,
                                  disc_apren == 4 ~ 0,
                                  disc_oir > 0 & disc_oir <= 3 ~ 1,
                                  disc_oir == 4 ~ 0,
                                  disc_vest > 0 & disc_vest <= 3 ~ 1,
                                  disc_vest == 4 ~ 0,
                                  disc_habla > 0 & disc_habla <= 3 ~ 1,
                                  disc_habla == 4 ~ 0,
                                  disc_acti > 0 & disc_acti <= 3 ~ 1,
                                  disc_acti == 4 ~ 0
                                  ))

prov <- Pob %>%
  select(disc_camin, disc_ver, disc_brazo, disc_apren, disc_oir, disc_vest, disc_habla,disc_acti, discapacidad)


#indígena
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(ind = case_when(hablaind == 1  ~ 1,
                         etnia == 1  ~ 1,
                         hablaind == 2  ~ 0,
                         etnia == 2  ~ 0))

prov <- Pob %>%
  select(hablaind, etnia, ind)

#Ind y analfabeta
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(indal = case_when(ind == 1 & analfa ==1  ~ 1,
                           TRUE ~ 0))

prov <- Pob %>%
  select(analfa, ind, indal)


#Disc y analfabeta
Pob<-Pob %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(discal = case_when(discapacidad == 1 & analfa ==1  ~ 1,
                            TRUE ~ 0))

prov <- Pob %>%
  select(analfa, discapacidad, discal)


#Merge con la tabla de vivienda y hogares
colnames(Viv) <- tolower(colnames(Viv))

prov <- Viv %>%
  select(folioviv, factor, tot_resid)%>%
  mutate(tot_resid = as.numeric(tot_resid))


base <- Pob %>%
  left_join(prov,by=c("folioviv"))%>%
  mutate(tot_resid = as.numeric(tot_resid)) %>%
  mutate(factorp = factor/as.numeric(tot_resid),
         numren = as.numeric(numren),
         res = case_when(numren >=1  ~ 1,
                         TRUE ~ 0))


prov <- base %>%
  group_by(folioviv) %>% 
  summarise(
    totalres= sum(res, na.rm = T))


base <- base %>%
  left_join(prov,by=c("folioviv"))%>%
  mutate(factorp2 = factor/totalres)


prov <- base %>%
  select(tot_resid, res, numren, totalres)




#Indigenas analfabetas
indigenas <- base%>% 
  drop_na(indal) %>% 
  filter(ind == 1) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(cve_ent) %>% 
  summarise(
    ind = survey_mean(indal, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(indigenas,"./resultados/indigenas_analfabetas.xlsx")


#Discapacitados analfabetas
discapacitados <- base%>% 
  drop_na(discapacidad) %>% 
  filter(discapacidad == 1) %>% 
  as_survey_design(weights = factor) %>% 
  group_by(cve_ent) %>% 
  summarise(
    disc = survey_mean(discal, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(discapacitados,"./resultados/discapacidad_analfabetas.xlsx")

