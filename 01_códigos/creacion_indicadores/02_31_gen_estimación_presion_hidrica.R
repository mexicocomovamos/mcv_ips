
# Datos de Estados descargados del Marco Geoestadñistico de INEGI
# Datos de Presión Hídrica descargados del SINA CONAGUA: https://sinav30.conagua.gob.mx:8080 Consultado el 22 de Octubre, 2024
# Se renombró la carpeta de descarga a Grado_presión_RHA_2022, y se ejecutó el código siguiente:

options(scipen = 999)

# Librerias:
library(tidyverse)
library(sf)

# Estimación ----
# Datos :
edos <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson")

# Abrimos los archivos shp de las carpetas: 
root <- ""
rh <- read_sf("Grado_presión_RHA_2022/m03_1vwgradopresionrhamPolygon.shp",
              options = "ENCODING=WINDOWS-1252") %>% st_transform(crs = 4326)

bd <- lapply(c(as.character(unique(rh$anio))),
  function(anio_sel){
    
  ph_sel <- rh %>% filter(anio == anio_sel)       
    
  lapply(edos$CVE_EDO, function(edo_sel){
    
    p <- st_intersection(ph_sel, edos %>%
                           filter(CVE_EDO == edo_sel)) %>%
      mutate(area = st_area(.),
             area2 = area/sum(area),
             presion_ponderada = area2*grado) %>%
      as_tibble() %>%
      select(-geometry) %>%
      summarise(CVE_GEO = edo_sel,
                year = anio_sel,
                presion = as.numeric(sum(presion_ponderada)))
    print(anio_sel)
    print(edo_sel)
    return(p)
  }) %>%
    do.call(rbind, .)
})

anio_sel1 = "2015"
bd0 <- lapply(c(as.character(unique(rh$anio))), function(anio_sel1){
               
               ph_sel <- rh %>% 
                 filter(anio == anio_sel1) %>%
                   mutate(area = st_area(.),
                          area2 = area/sum(area),
                          presion_ponderada = area2*grado) %>%
                   as_tibble() %>%
                   select(-geometry) %>%
                   summarise(CVE_GEO = "00",
                             year = anio_sel1,
                             presion = as.numeric(sum(presion_ponderada)))
                 print(anio_sel1)
                 return(ph_sel)
}) %>% do.call(rbind, .)
             

bd_estimacion <- bd %>%
  do.call(rbind, .) %>%
  rbind(bd0) %>% 
  mutate(no = "6.4.1",
         periodo = "1",
         nombre = "Grado de presión hídrica",
         desagregacion = "Estimación") %>%
  select(no,
         cve_ent = CVE_GEO,
         year,
         periodo,
         valor = presion,
         nombre,
         desagregacion) 

bd_estimacion %>% 
  filter(year == 2022) %>% 
  openxlsx::write.xlsx("2022.xlsx")

bd_estimacion %>% 
  select(no, year, cve_ent, valor) %>% 
  mutate(no = 302) %>% 
  openxlsx::write.xlsx("302.xlsx")

