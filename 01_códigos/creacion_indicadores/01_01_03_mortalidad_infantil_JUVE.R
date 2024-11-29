
library(tidyverse)

# Para sacar este indicador: 
# 1. Nos vamos al tabulado de INEGI de Defunciones Registradas
# 2. Seleccionamos • Defunciones infantiles
# 3. Seleccionamos año de ocurrencia y Estado de residencia habitual 
# 4. Nos vamos al tabulado
# 5. Pasamos al lado izquierdo las variables de los años y a las columnas la entidad de residencia habitual
# 6. Descargamos el tabulado
# 7. Corremos el código. 
# 8. Hacemos lo mismo para los nacimientos. 
# Ante la duda, abrir los archivos que ya están y ver que las descargas tengan la misma estructura. 

MORTALIDAD_BEBES <- readxl::read_xlsx("02_datos_crudos/01_01_mortalidad/MORTALIDAD_BEBES.xls", 
                                      skip = 4) %>% 
    rename(ocurrencia   = `...1`, 
           registro     = `...2`) %>% 
    pivot_longer(3:ncol(.)) %>% 
    mutate(value = str_remove_all(value, pattern = ",") %>% as.numeric()) %>% 
    filter(as.numeric(ocurrencia) >= 1992) %>% 
    group_by(ocurrencia, name) %>% 
    summarise(value = sum(value, na.rm = T)) %>% 
    mutate(name = ifelse(name == "Total", yes = "Nacional", no = name)) %>% 
    select(year = ocurrencia, nom_ent = name, mort_bebes = value)

NAC_VIVOS <- readxl::read_xlsx("02_datos_crudos/01_01_mortalidad/NACIDOS_VIVOS.xlsx") %>% 
    rename(nom_ent = Entidad) %>% 
    mutate(cve_entidad = str_pad(cve_entidad, side = "left", pad = "0", width = 2)) %>% 
    pivot_longer(cols = 3:ncol(.)) %>% 
    mutate(value = str_remove_all(value, pattern = ",") %>% as.numeric()) %>% 
    rename(cve_ent = cve_entidad, 
           year = name, 
           nac_vivos = value) %>% 
    mutate(nom_ent = ifelse(nom_ent == "Total", yes = "Nacional", no = nom_ent))

catalogo_abreviaturas <- read_csv("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/Datos/cat_edos.csv")

datos <- left_join(MORTALIDAD_BEBES, NAC_VIVOS) %>%
    ungroup() %>% 
    mutate(razon = mort_bebes/(nac_vivos/1000)) %>% 
    filter(as.numeric(cve_ent) <= 32) %>% 
    arrange(year, as.numeric(cve_ent)) %>% 
    mutate(id_dimension = "01", 
           id_indicador = "03") %>% 
    left_join(catalogo_abreviaturas) %>% 
    select(cve_ent, 
           entidad_abr_m, 
           anio = year, 
           id_dimension, 
           id_indicador, 
           indicador_value = razon)

openxlsx::write.xlsx(datos, "02_datos_crudos/01_01_03_mortalidad_infantil.xlsx")

