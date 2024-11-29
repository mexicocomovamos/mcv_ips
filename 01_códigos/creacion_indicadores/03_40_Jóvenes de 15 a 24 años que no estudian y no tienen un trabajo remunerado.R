options(scipen = 999)

# Informalidad

# Librerias: ----
library(tidyverse)
library(srvyr)
library(presupuestoR)
library(ggh4x)
library(ggrepel)
library(ggtext)

cat_edos <- read_csv("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/Datos/cat_edos.csv") %>% 
  mutate(cve_ent = as.numeric(cve_ent))

# Funciones propias: 
nb <- function(x, n) prettyNum(format(round(x,digits = n),nsmall = n),big.mark = ",") %>% str_squish()

# Variables. 
archivos <- list.files("02_datos_limpios/rds", full.names = T)
trim_maximo <- str_extract(archivos[length(archivos)], pattern = "\\_\\d\\.") %>% str_remove_all(pattern = "\\.|\\_") %>% as.numeric()
year_maximo <- str_extract(archivos[length(archivos)], pattern = "\\_\\d+\\_") %>% str_remove_all(pattern = "\\.|\\_") %>% as.numeric()

# Loop de cálculo: 
a = archivos[76]
info_jnentr <- lapply(archivos, function(a){

    datos <- readRDS(a) 
    
    # Total jovenes 15-24 por entidad: 
    total <- datos %>% 
      filter(between(eda, 15, 24)) %>% 
      as_survey_design(weights = factor) %>% 
      group_by(ent, año, trimestre) %>% 
      survey_count() %>%
      mutate(CV = 100*(n_se/n)) %>% 
      arrange(-CV) %>% 
      rename(jovenes_15_24 = n) %>% 
      select(ent, jovenes_15_24) %>% 
      rbind(datos %>% 
              filter(between(eda, 15, 24)) %>% 
              as_survey_design(weights = factor) %>% 
              group_by(año, trimestre) %>% 
              survey_count() %>%
              rename(jovenes_15_24 = n) %>% 
              ungroup() %>% 
              mutate(ent = 0) %>% 
              select(año, trimestre, ent, jovenes_15_24))
    
    # Jovenes en la PNEA de 15-24 años que no son estudiantes:
    jnentr <- datos %>% 
      filter(pos_ocu == 4 | clase1 == 2) %>%
      # filter(pos_ocu)
      # filter(clase1 == 2) %>% # Seleccionamos a la PNEA
      filter(between(eda, 15, 24)) %>% 
      filter(c_inac5c != 1) %>%  # Quitamos a los estudiantes
      as_survey_design(weights = factor) %>% 
      group_by(ent,año, trimestre) %>% 
      survey_count() %>% 
      mutate(CV = 100*(n_se/n)) %>% 
      arrange(-CV) %>% 
      mutate(jnentr = n) %>% 
      select(ent, jnentr,año, trimestre) %>% 
      rbind(
        datos %>% 
          # filter(pos_ocu == 4 | clase1 == 2) %>% 
          # filter(pos_ocu)
          filter(clase1 == 2) %>% # Seleccionamos a la PNEA
          filter(between(eda, 15, 24)) %>% 
          filter(c_inac5c != 1) %>%  # Quitamos a los estudiantes
          as_survey_design(weights = factor) %>% 
          group_by(año, trimestre) %>% 
          survey_count() %>% 
          mutate(CV = 100*(n_se/n)) %>% 
          arrange(-CV) %>% 
          mutate(jnentr = n, 
                 ent = 0) %>% 
          select(ent, jnentr,año, trimestre)
      )
    
    tabla <- left_join(total, jnentr) %>% 
      ungroup() %>% 
      mutate(pp = 100*(jnentr/jovenes_15_24)) %>% 
      left_join(cat_edos, by = c("ent" = "cve_ent")) %>% 
      arrange(-pp) 
    
    print(a)
    return(tabla)

})

indicador_final <- info_jnentr %>% 
  do.call(rbind, .) %>% 
  rename(cve_ent = ent) %>% 
  filter(trimestre == 1) %>% 
  mutate(cve_ent = str_pad(cve_ent, side = "left", pad = "0", width = 2)) %>% 
  filter(año != 2024) %>% 
  mutate(id_dimension = "03", 
         id_indicador = "40") %>% 
  select(cve_ent, entidad_abr_m, anio = año, 
         id_dimension,id_indicador, 
         indicador_value = pp)
  
  
openxlsx::write.xlsx(indicador_final, 
                     "02_datos_limpios/indicadores/jovenes_15_24_no_remunerado_no_estudiantes.xlsx")
