"https://www.inegi.org.mx/contenidos/productos/prod_serv/contenidos/espanol/bvinegi/productos/nueva_estruc/889463910626.pdf"

#00. Librerias ----
library(foreign)
library(survey)
library(srvyr)
library(tidyverse)

# Claves de estados: 
cat <- tibble::tribble(
  ~cve_ent,                  ~entidad, ~abbr, ~Region,
  "00",            "Nacional",   "Nacional", "Nacional",
  "01",      "Aguascalientes",        "AGS", "Centro-occidente",
  "02",     "Baja California",         "BC", "Noroeste",
  "03", "Baja California Sur",        "BCS", "Noroeste",
  "04",            "Campeche",       "CAMP", "Sur-sureste",
  "05",            "Coahuila",       "COAH", "Noreste",
  "06",              "Colima",        "COL", "Centro-occidente",
  "07",             "Chiapas",       "CHIS", "Sur-sureste",
  "08",           "Chihuahua",       "CHIH", "Noreste",
  "09",    "Ciudad de México",       "CDMX", "Centro",
  "10",             "Durango",        "DGO", "Noreste",
  "11",          "Guanajuato",        "GTO", "Centro-occidente",
  "12",            "Guerrero",        "GRO", "Sur-sureste",
  "13",             "Hidalgo",        "HGO", "Centro",
  "14",             "Jalisco",        "JAL", "Centro-occidente",
  "15",              "México",        "MEX", "Centro",
  "16",           "Michoacán",       "MICH", "Centro-occidente",
  "17",             "Morelos",        "MOR", "Centro",
  "18",             "Nayarit",        "NAY", "Centro-occidente",
  "19",          "Nuevo León",         "NL", "Noreste",
  "20",              "Oaxaca",        "OAX", "Sur-sureste",
  "21",              "Puebla",        "PUE", "Sur-sureste",
  "22",           "Querétaro",        "QRO", "Centro",
  "23",        "Quintana Roo",       "QROO", "Sur-sureste",
  "24",     "San Luis Potosí",        "SLP", "Centro-occidente",
  "25",             "Sinaloa",        "SIN", "Noroeste",
  "26",              "Sonora",        "SON", "Noroeste",
  "27",             "Tabasco",        "TAB", "Sur-sureste",
  "28",          "Tamaulipas",      "TAMPS", "Noreste",
  "29",            "Tlaxcala",       "TLAX", "Centro",
  "30",            "Veracruz",        "VER", "Sur-sureste",
  "31",             "Yucatán",        "YUC", "Sur-sureste",
  "32",           "Zacatecas",        "ZAC", "Centro-occidente"
)

# 2. Datos ----
## 2.1 Datos de concentrado hogar ----
datos <- tibble(year = c(2016, 
                         2018,
                         2020, 
                         2022), 
                concentrado_hogar = c(
                  "/Volumes/Extreme\ SSD/DATASETS/INEGI\ -\ ENIGH/2016/conjunto_de_datos_concentradohogar_enigh_2016_ns/conjunto_de_datos/conjunto_de_datos_concentradohogar_enigh_2016_ns.csv",
                  "/Volumes/Extreme\ SSD/DATASETS/INEGI\ -\ ENIGH/2018/conjunto_de_datos_concentradohogar_enigh_2018_ns/conjunto_de_datos/conjunto_de_datos_concentradohogar_enigh_2018_ns.csv", 
                  "/Volumes/Extreme SSD/DATASETS/INEGI - ENIGH/2020/conjunto_de_datos_concentradohogar_enigh_2020_ns/conjunto_de_datos/conjunto_de_datos_concentradohogar_enigh_2020_ns.csv",
                  "/Volumes/Extreme\ SSD/DATASETS/INEGI\ -\ ENIGH/2022/concentradohogar.csv"
                ), 
                hogares = c(
                  "/Volumes/Extreme SSD/DATASETS/INEGI - ENIGH/2016/conjunto_de_datos_hogares_enigh_2016_ns/conjunto_de_datos/conjunto_de_datos_hogares_enigh_2016_ns.csv",
                  "/Volumes/Extreme SSD/DATASETS/INEGI - ENIGH/2018/conjunto_de_datos_hogares_enigh_2018_ns/conjunto_de_datos/conjunto_de_datos_hogares_enigh_2018_ns.csv",
                  "/Volumes/Extreme SSD/DATASETS/INEGI - ENIGH/2020/conjunto_de_datos_hogares_enigh_2020_ns/conjunto_de_datos/conjunto_de_datos_hogares_enigh_2020_ns.csv",
                  "/Volumes/Extreme SSD/DATASETS/INEGI - ENIGH/2022/hogares.csv"
                ), 
                viviendas = c(
                  "/Volumes/Extreme SSD/DATASETS/INEGI - ENIGH/2016/conjunto_de_datos_viviendas_enigh_2016_ns/conjunto_de_datos/conjunto_de_datos_viviendas_enigh_2016_ns.csv",
                  "/Volumes/Extreme SSD/DATASETS/INEGI - ENIGH/2018/conjunto_de_datos_vivienda_enigh_2018_ns/conjunto_de_datos/conjunto_de_datos_viviendas_enigh_2018_ns.csv",
                  "/Volumes/Extreme SSD/DATASETS/INEGI - ENIGH/2020/conjunto_de_datos_viviendas_enigh_2020_ns/conjunto_de_datos/conjunto_de_datos_viviendas_enigh_2020_ns.csv",
                  "/Volumes/Extreme SSD/DATASETS/INEGI - ENIGH/2022/viviendas.csv"
                ))

anio_sel = 2016

indicador <- lapply(c(2016, 
         2018,
         2020, 
         2022), function(anio_sel){
           
    # Procesamiento:  
    Viv <- read_csv(datos$viviendas[datos$year == anio_sel]) %>% 
      mutate(folioviv = str_pad(folioviv, width = 10, side = "left", pad = "0")) %>% 
      select(folioviv, eli_basura) %>%
    mutate(eli_basura = case_when(eli_basura == 1 ~ "La recoge un camión o carrito de basura",
                                  eli_basura == 2 ~ "La tiran en el basurero público",
                                  eli_basura == 3 ~ "La tiran en un contenedor o depósito",
                                  eli_basura == 4 ~ "La queman",
                                  eli_basura == 5 ~ "La entierran",
                                  eli_basura == 6 ~ "La tiran en un terreno baldío o calle",
                                  eli_basura == 7 ~ "La tiran en barranca o grieta",
                                  eli_basura == 8 ~ "La tiran al río, lago o mar"))
    
    Hog <-  read.csv(datos$concentrado_hogar[datos$year == anio_sel]) %>% 
      mutate(folioviv = str_pad(folioviv, width = 10, side = "left", pad = "0")) %>% 
      select("folioviv","foliohog","factor","upm","est_dis") %>% 
      mutate(ID = paste(folioviv,foliohog,sep=".")) %>% 
      as_tibble()
    
    info <- left_join(Viv, Hog) %>% 
      mutate(cve_ent = str_extract(folioviv, "^\\d\\d")) %>% 
      left_join(cat)
      
    hogares_queman_entierran <- info %>% 
      as_survey_design(
        id=upm,
        strata=est_dis,
        weights=factor) %>%
      group_by(cve_ent, abbr, entidad, eli_basura) %>%
      srvyr::survey_count() %>% 
      filter(eli_basura %in% c("La entierran", "La queman")) %>% 
      ungroup() %>% 
      group_by(entidad) %>% 
      summarise(hogares_queman_entierran = sum(n))
    
    hogares_totales <- info %>% 
      as_survey_design(
        id=upm,
        strata=est_dis,
        weights=factor) %>%
      group_by(cve_ent, abbr, entidad) %>%
      srvyr::survey_count() %>% 
      select(-n_se)
    
    datos_totales <- left_join(hogares_queman_entierran, 
              hogares_totales) %>% 
      mutate(prop = 100*(hogares_queman_entierran/n)) %>% 
      mutate(anio = anio_sel) %>% 
      arrange(-prop)
    
    # Parte Nacional: 
    hogares_queman_entierran_nal <- info %>% 
      as_survey_design(
        id=upm,
        strata=est_dis,
        weights=factor) %>%
      group_by(eli_basura) %>%
      srvyr::survey_count() %>% 
      filter(eli_basura %in% c("La entierran", "La queman")) %>% 
      ungroup() %>% 
      summarise(hogares_queman_entierran = sum(n))
    
    hogares_totales_nal <- info %>% 
      as_survey_design(
        id=upm,
        strata=est_dis,
        weights=factor) %>%
      srvyr::survey_count() %>% 
      select(-n_se)
    
    prop_nacional = 100*(hogares_queman_entierran_nal/hogares_totales_nal)
    
    bd_final <- datos_totales %>% 
      mutate(id_dimension = "02",	
             id_indicador = "32") %>% 
      select(cve_ent, 
             entidad_abr_m = abbr, 
             anio, id_dimension, id_indicador, 
             indicador_value = prop) %>% 
      rbind.data.frame(tibble(cve_ent = "00", 
                   entidad_abr_m = "Nacional", 
                   anio = anio_sel, 
                   id_dimension = "02", 
                   id_indicador = "32", 
                   indicador_value = unlist(prop_nacional)))
    
  return(bd_final)

})

indicador2 = indicador %>% 
  do.call(rbind, .) %>% 
  arrange(anio, as.numeric(cve_ent))

openxlsx::write.xlsx(indicador2, 
                     "datos_limpios/indicador_entierran_queman_basura.xlsx")

