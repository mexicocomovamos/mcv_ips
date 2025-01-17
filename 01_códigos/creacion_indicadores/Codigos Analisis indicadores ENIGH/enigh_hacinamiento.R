# Librer�as ====
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, srvyr,
               kableExtra)

#Descarga de archivos
url<-"https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_viviendas_csv.zip"
##Creaci�n de directorio temporal
td<- tempdir()
# Descarga del archivo temporal
tf = tempfile(tmpdir=td, fileext=".zip")
download.file(url, tf)
# unzip
unzip(tf, files="viviendas.csv", exdir=td, 
      overwrite=TRUE)
fpath=file.path(td,"viviendas.csv")
unlink(td)

#Leer el archivo
enigh<-read.csv(fpath)%>%
  #Renombrar folioviv
  rename(folioviv=1)

enigh<-enigh%>%
  #Obtener la clave de entidad de la variable folioviv
  mutate(cve_ent=
           case_when(nchar(folioviv)==9 ~ substr(folioviv,1,1),
                     TRUE ~substr(folioviv,1,2)),
         #Creamos nombre de entidad
         nom_ent= case_when(cve_ent==1 ~ "Aguascalientes",
                            cve_ent==2 ~ "Baja California",
                            cve_ent==3 ~ "Baja California Sur",
                            cve_ent==4 ~ "Campeche",
                            cve_ent==5 ~ "Coahuila de Zaragoza",
                            cve_ent==6 ~ "Colima",
                            cve_ent==7 ~ "Chiapas",
                            cve_ent==8 ~ "Chihuahua",
                            cve_ent==9 ~ "Ciudad de M�xico",
                            cve_ent==10 ~ "Durango",
                            cve_ent==11 ~ "Guanajuato",
                            cve_ent==12 ~ "Guerrero",
                            cve_ent==13 ~ "Hidalgo",
                            cve_ent==14 ~ "Jalisco",
                            cve_ent==15 ~ "M�xico",
                            cve_ent==16 ~ "Michoac�n de Ocampo",
                            cve_ent==17 ~ "Morelos",
                            cve_ent==18 ~ "Nayarit",
                            cve_ent==19 ~ "Nuevo Le�n",
                            cve_ent==20 ~ "Oaxaca",
                            cve_ent==21 ~ "Puebla",
                            cve_ent==22 ~ "Quer�taro",
                            cve_ent==23 ~ "Quintana Roo",
                            cve_ent==24 ~ "San Luis Potos�",
                            cve_ent==25 ~ "Sinaloa",
                            cve_ent==26 ~ "Sonora",
                            cve_ent==27 ~ "Tabasco",
                            cve_ent==28 ~ "Tamaulipas",
                            cve_ent==29 ~ "Tlaxcala",
                            cve_ent==30 ~ "Veracruz de Ignacio de la Llave",
                            cve_ent==31 ~ "Yucat�n",
                            cve_ent==32 ~ "Zacatecas"))

enigh<-enigh%>%
  #Creamos indicador de hacinamiento
  mutate(hac=case_when((tot_resid / num_cuarto) > 2.5 ~ 1,
                       TRUE ~ 0))

#Definir dise�o muestral
mydesign <-enigh%>%
  as_survey_design(ids=upm,
                   strata=est_dis,
                   weights=factor)

#Viviendas con habitantes en condici�n de hacinamiento nacional
hacnal<-mydesign %>%
  filter(hac==1)%>%
  summarise(hac=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  mutate(hac_cv=
           hac_cv*100,
         #Formato de n�meros
         hac=format(hac,big.mark=","),
         hac_cv=round(hac_cv,2),
         hac_low=format(hac_low,big.mark=","),
         hac_upp=format(hac_upp,big.mark=","),
  )

#Tabla Nacional
hacnal%>%
  kable(caption=text_spec("M�xico. Viviendas con habitantes en condici�n de hacinamiento, 2018",
                          bold=T, color="black",font_size = 30),
        format="html",
        align = "c",
        col.names = c("Viviendas con  habitantes en condici�n de\n
                    hacinamiento",
                      "Coeficiente de variaci�n",
                      "L�mite inferior",
                      "L�mite superior"))%>%
  kable_styling(full_width = F, font_size = 20,
                html_font = "Montserrat Medium")%>%
  row_spec(0, bold = F, color = "black", background = "#9ebcda")%>%
  footnote(general = "@claudiodanielpc con informaci�n de INEGI. Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) 2018",
           general_title = "
Fuente: ")%>%
  #Salvar
  as_image(file="hacnal.png")


#Viviendas con habitantes en condici�n de hacinamiento por entidad federativa
hacent<-mydesign %>%
  filter(hac==1)%>%
  group_by(nom_ent)%>%
  summarise(hac=survey_total(
    vartype = c("cv", "ci"),
    level=0.95))%>%
  #Multiplicar coeficiente de variaci�n por 100
  mutate(hac_cv=
           hac_cv*100,
         #Formato de n�meros
         hac=format(hac,big.mark=","),
         hac_cv=round(hac_cv,2),
         hac_low=format(hac_low,big.mark=","),
         hac_upp=format(hac_upp,big.mark=","),
  )

#Tabla por entidad federativa
hacent%>%
  kable(caption=text_spec("Viviendas con habitantes en condici�n de hacinamiento por entidad federativa, 2018",
                          bold=T, color="black",font_size = 30),
        format="html",
        align = "c",
        col.names = c("Entidad",
                      "Viviendas con  habitantes 
en condici�n de\n
                    hacinamiento",
                      "Coeficiente de variaci�n",
                      "L�mite inferior",
                      "L�mite superior"))%>%
  kable_styling(full_width = F, font_size = 20,
                html_font = "Montserrat Medium")%>%
  row_spec(0, bold = F, color = "black", background = "#9ebcda")%>%
  footnote(general = "@claudiodanielpc con informaci�n de INEGI. Encuesta Nacional de Ingresos y Gastos de los Hogares (ENIGH) 2018",
           general_title = "
Fuente: ")%>%
  #Salvar
  as_image(file="hacent.png")