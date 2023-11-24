#############################ENDUTIH#######################################  

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
setwd("J:/MCV/Pruebas/ENDUTIH/2020")

# abrimos la tabla concentradohogar
hogar<- read.dbf("./data/ti20hog.dbf",as.is = T)


hogar<-hogar %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(internet = case_when(P4_4 == 1 ~ 1,
                              P4_4 == 2 ~ 0
                          
  ))

prov <-hogar %>%
  select(internet, P4_4, ENT)

#Hogares con internet
internetn <- hogar%>% 
  drop_na(internet) %>% 
  as_survey_design(weights = FAC_HOG) %>% 
  #group_by(ENT) %>% 
  summarise(
    internet = survey_mean(internet, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 


#Hogares con internet
internet <- hogar%>% 
  drop_na(internet) %>% 
  as_survey_design(weights = FAC_HOG) %>% 
  group_by(ENT) %>% 
  summarise(
    internet = survey_mean(internet, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(internet,"./resultados/internet_estados.xlsx")


#Definimos el directorio
setwd("J:/MCV/Pruebas/ENDUTIH/2018")

# abrimos la tabla concentradohogar
hogar<- read.dbf("./data/tic_2018_hogares.dbf",as.is = T)


hogar<-hogar %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(internet = case_when(P4_4 == 1 ~ 1,
                              P4_4 == 2 ~ 0
                              
  ))

prov <-hogar %>%
  select(internet, P4_4, ENT)

#Internet
internetn <- hogar%>% 
  drop_na(internet) %>% 
  as_survey_design(weights = FAC_HOG) %>% 
  #group_by(ENT) %>% 
  summarise(
    internet = survey_mean(internet, na.rm = T)
  ) %>% 
  select(-ends_with("se"))

internet <- hogar%>% 
  drop_na(internet) %>% 
  as_survey_design(weights = FAC_HOG) %>% 
  group_by(ENT) %>% 
  summarise(
    internet = survey_mean(internet, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(internet,"./resultados/internet_estados.xlsx")

#Definimos el directorio
setwd("J:/MCV/Pruebas/ENDUTIH/2017")

# abrimos la tabla concentradohogar
hogar<- read.dbf("./data/tic_2017_hogares.dbf",as.is = T)


hogar<-hogar %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(internet = case_when(P4_4 == 1 ~ 1,
                              P4_4 == 2 ~ 0
                              
  ))

prov <-hogar %>%
  select(internet, P4_4, ENT)

#Internet
internetn <- hogar%>% 
  drop_na(internet) %>% 
  as_survey_design(weights = FAC_HOG) %>% 
  #group_by(ENT) %>% 
  summarise(
    internet = survey_mean(internet, na.rm = T)
  ) %>% 
  select(-ends_with("se"))

internet <- hogar%>% 
  drop_na(internet) %>% 
  as_survey_design(weights = FAC_HOG) %>% 
  group_by(ENT) %>% 
  summarise(
    internet = survey_mean(internet, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(compu,"./resultados/compu_estados.xlsx")


#Definimos el directorio
setwd("J:/MCV/Pruebas/ENDUTIH/2018")

# abrimos la tabla concentradohogar
hogar<- read.dbf("./data/tic_2018_hogares.dbf",as.is = T)

# hogar<-hogar %>%
#       mutate(compu = case_when(P4_2_1 == 1 & P4_2_2==1 ~ 1,
#                                P4_2_1 == 1 & P4_2_2==2 ~ 1,
#                                P4_2_1 == 2 & P4_2_2==1 ~ 1,
#                                P4_2_1 == 2 & P4_2_2==2 ~ 0))

hogar<-hogar %>%
  mutate(compu = case_when(P4_2_1 == 1 & P4_2_2==1 & P4_2_3==1 ~ 1,
                           P4_2_1 == 1 & P4_2_2==2 & P4_2_3==1~ 1,
                           P4_2_1 == 1 & P4_2_2==1 & P4_2_3==2 ~ 1,
                           P4_2_1 == 1 & P4_2_2==2 & P4_2_3==2 ~ 1,
                           P4_2_1 == 2 & P4_2_2==1 & P4_2_3==1 ~ 1,
                           P4_2_1 == 2 & P4_2_2==1 & P4_2_3==2 ~ 1,
                           P4_2_1 == 2 & P4_2_2==2 & P4_2_3==1 ~ 1,
                           P4_2_1 == 2 & P4_2_2==2 & P4_2_3==2~ 0
                           ))

prov <-hogar %>%
  select(compu, P4_2_1, P4_2_2,P4_2_3, ENT)

#Hogares con computadora
compu<- hogar%>% 
  drop_na(compu) %>% 
  as_survey_design(weights = FAC_HOG) %>% 
  #group_by(ENT) %>% 
  summarise(
    compu = survey_mean(compu, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 


#Hogares con computadora
compu<- hogar%>% 
  drop_na(compu) %>% 
  as_survey_design(weights = FAC_HOG) %>% 
  group_by(ENT) %>% 
  summarise(
    compu = survey_mean(compu, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(compu,"./resultados/compu_estados.xlsx")


#Definimos el directorio
setwd("J:/MCV/Pruebas/ENDUTIH/2017")

# abrimos la tabla concentradohogar
hogar<- read.dbf("./data/tic_2017_hogares.dbf",as.is = T)

# hogar<-hogar %>%
#       mutate(compu = case_when(P4_2_1 == 1 & P4_2_2==1 ~ 1,
#                                P4_2_1 == 1 & P4_2_2==2 ~ 1,
#                                P4_2_1 == 2 & P4_2_2==1 ~ 1,
#                                P4_2_1 == 2 & P4_2_2==2 ~ 0))

hogar<-hogar %>%
  mutate(compu = case_when(P4_2_1 == 1 & P4_2_2==1 & P4_2_3==1 ~ 1,
                           P4_2_1 == 1 & P4_2_2==2 & P4_2_3==1~ 1,
                           P4_2_1 == 1 & P4_2_2==1 & P4_2_3==2 ~ 1,
                           P4_2_1 == 1 & P4_2_2==2 & P4_2_3==2 ~ 1,
                           P4_2_1 == 2 & P4_2_2==1 & P4_2_3==1 ~ 1,
                           P4_2_1 == 2 & P4_2_2==1 & P4_2_3==2 ~ 1,
                           P4_2_1 == 2 & P4_2_2==2 & P4_2_3==1 ~ 1,
                           P4_2_1 == 2 & P4_2_2==2 & P4_2_3==2~ 0
  ))

prov <-hogar %>%
  select(compu, P4_2_1, P4_2_2,P4_2_3, ENT)

#Hogares con computadora
compu<- hogar%>% 
  drop_na(compu) %>% 
  as_survey_design(weights = FAC_HOG) %>% 
  #group_by(ENT) %>% 
  summarise(
    compu = survey_mean(compu, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 


#Hogares con computadora
compu<- hogar%>% 
  drop_na(compu) %>% 
  as_survey_design(weights = FAC_HOG) %>% 
  group_by(ENT) %>% 
  summarise(
    compu = survey_mean(compu, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(compu,"./resultados/compu_estados.xlsx")

###############################tm###############################
#Definimos el directorio
setwd("J:/MCV/Pruebas/ENDUTIH/2020")

# abrimos la tabla concentradohogar
usuarios <- read.dbf("./data/ti20usu2.dbf",as.is = T)


usuarios <-usuarios %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(tm = case_when(P8_1 == 1 ~ 1,
                        P8_1 == 2 ~ 0
                            ))

# usuarios <-usuarios %>%
#   #Obtener la clave de entidad de la variable folioviv
#   mutate(tm = case_when(P8_1 == 1 & P8_4_2 == 1  ~ 1,
#                         P8_1 == 1 & P8_4_1 == 1  ~ 0,
#                         P8_4_2 == 2 ~ 0
#   ))
# 
# usuarios <-usuarios %>%
#   #Obtener la clave de entidad de la variable folioviv
#   mutate(tm = case_when(P8_4_1 == 1 | P8_4_2 == 1  ~ 1,
#                         P8_4_1 == 1 | P8_4_2 == 2  ~ 0,
#                         P8_4_1 == 2 & P8_4_2 == 2  ~ 0
#                         
#   ))
# 

prov <-usuarios %>%
  select(tm, P8_1, P8_4_1, P8_4_2, ENT)

#usuarios con tm
tm <- usuarios %>% 
  drop_na(tm) %>% 
  as_survey_design(weights = FAC_PER) %>% 
  #group_by(ENT) %>% 
  summarise(
    tm = survey_mean(tm, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 



#usuarios con tm
tm <- usuarios %>% 
  drop_na(tm) %>% 
  as_survey_design(weights = FAC_PER) %>% 
  group_by(ENT) %>% 
  summarise(
    tm = survey_mean(tm, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(tm,"./resultados/tm_estados.xlsx")

#Definimos el directorio
setwd("J:/MCV/Pruebas/ENDUTIH/2018")

# abrimos la tabla concentradohogar
usuarios <- read.dbf("./data/tic_2018_usuarios.dbf",as.is = T)


usuarios <-usuarios %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(tm = case_when(P8_1 == 1 ~ 1,
                        P8_1 == 2 ~ 0
  ))



prov <-usuarios %>%
  select(tm, P8_1, P8_4_1, P8_4_2, ENT)

#usuarios con tm
tm <- usuarios %>% 
  drop_na(tm) %>% 
  as_survey_design(weights = FAC_PER) %>% 
  #group_by(ENT) %>% 
  summarise(
    tm = survey_mean(tm, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 



#usuarios con tm
tm <- usuarios %>% 
  drop_na(tm) %>% 
  as_survey_design(weights = FAC_PER) %>% 
  group_by(ENT) %>% 
  summarise(
    tm = survey_mean(tm, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(tm,"./resultados/tm_estados.xlsx")


#Definimos el directorio
setwd("J:/MCV/Pruebas/ENDUTIH/2017")

# abrimos la tabla concentradohogar
usuarios <- read.dbf("./data/tic_2017_usuarios.dbf",as.is = T)


usuarios <-usuarios %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(tm = case_when(P8_1 == 1 ~ 1,
                        P8_1 == 2 ~ 0
  ))



prov <-usuarios %>%
  select(tm, P8_1, P8_4_1, P8_4_2, ENT)

#usuarios con tm
tm <- usuarios %>% 
  drop_na(tm) %>% 
  as_survey_design(weights = FAC_PER) %>% 
  #group_by(ENT) %>% 
  summarise(
    tm = survey_mean(tm, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 



#usuarios con tm
tm <- usuarios %>% 
  drop_na(tm) %>% 
  as_survey_design(weights = FAC_PER) %>% 
  group_by(ENT) %>% 
  summarise(
    tm = survey_mean(tm, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(tm,"./resultados/tm_estados.xlsx")


#Definimos el directorio
setwd("J:/MCV/Pruebas/ENDUTIH/2016")

# abrimos la tabla concentradohogar
usuarios <- read.dbf("./data/tic_2016_usuarios.dbf",as.is = T)


usuarios <-usuarios %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(tm = case_when(P8_1 == 1 ~ 1,
                        P8_1 == 2 ~ 0
  ))



prov <-usuarios %>%
  select(tm, P8_1, P8_4_1, P8_4_2, ENT)

#usuarios con tm
tm <- usuarios %>% 
  drop_na(tm) %>% 
  as_survey_design(weights = FAC_PER) %>% 
  #group_by(ENT) %>% 
  summarise(
    tm = survey_mean(tm, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 



#usuarios con tm
tm <- usuarios %>% 
  drop_na(tm) %>% 
  as_survey_design(weights = FAC_PER) %>% 
  group_by(ENT) %>% 
  summarise(
    tm = survey_mean(tm, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(tm,"./resultados/tm_estados.xlsx")


#Definimos el directorio
setwd("J:/MCV/Pruebas/ENDUTIH/2015")

# abrimos la tabla concentradohogar
usuarios <- read.dbf("./data/tic_2015_usuarios.dbf",as.is = T)


usuarios <-usuarios %>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(tm = case_when(P8_1 == 1 ~ 1,
                        P8_1 == 2 ~ 0
  ))





#usuarios con tm
tm <- usuarios %>% 
  drop_na(tm) %>% 
  as_survey_design(weights = FAC_PER) %>% 
  #group_by(ENT) %>% 
  summarise(
    tm = survey_mean(tm, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 



#usuarios con tm
tm <- usuarios %>% 
  drop_na(tm) %>% 
  as_survey_design(weights = FAC_PER) %>% 
  group_by(ENT) %>% 
  summarise(
    tm = survey_mean(tm, na.rm = T)
  ) %>% 
  select(-ends_with("se")) 

write_xlsx(tm,"./resultados/tm_estados.xlsx")


