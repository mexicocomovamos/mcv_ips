library(tidyverse)

# Cargamos proyeccion de CONAPO: 
load("02_datos_crudos/df_pop_state.Rdata")
proy_conapo <- df_pop_state

catalogo_abreviaturas <- read_csv("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/Datos/cat_edos.csv")
# Cargamos las enfermedades. 
# Para esto, vamos a desdoblar el tabulado de INEGI de mortalidad, seleccionando: 
#     * Año de ocurrencia
#     * Entidad y municipio de ocurrencia
#     * Lista de enfermedades de la lista mexicana de enfermedades
# Y vamos a pasar el año de ocurrencia y la Entidad y municipio de ocurrencia a los renglones, y vamos a desdoblar todas las categorías de enfermedades
# Guardamos como csv, verificamos la estructura y seguimos corriendo el código: 

TODAS_ENFERMEDADES <- read_csv("02_datos_crudos/01_01_mortalidad/TODAS_ENFERMEDADES.csv", 
                               locale = locale(encoding = "WINDOWS-1252"), 
                               skip = 6)
names(TODAS_ENFERMEDADES)[c(1,2,3,4)] <- c("registro", "ocurrencia", "cve_ent", "nom_ent")

# Procesamiento: 
# names(TODAS_ENFERMEDADES) %>% 
#     writeLines()

# names(TODAS_ENFERMEDADES)

TODAS_ENFERMEDADES <- TODAS_ENFERMEDADES[,c(1:5, c(62:476))] %>% 
    filter(ocurrencia %in% c(1998:2023))

dx <- TODAS_ENFERMEDADES %>% 
    mutate(cve_ent = ifelse(nom_ent == "Total", yes = "00", no = cve_ent)) %>% 
    pivot_longer(cols = 6:ncol(.)) %>% 
    filter(name %in% 
               # "(E49) Accidentes de transporte"
               c("(49B) Accidentes de tráfico de vehículos de motor",
                 "(49C) Accidentes de otros vehículos de carretera")
           ) %>% 
    group_by(cve_ent, nom_ent, ocurrencia) %>% 
    summarise(value = sum(value, na.rm = T))

datos <- left_join(dx %>% mutate(year = ocurrencia %>% as.numeric()),
                   proy_conapo %>% 
                       mutate(cve_ent = str_pad(CVE_GEO, width = 2, side = "left", pad = "0")) %>% 
                       select(-CVE_GEO)) %>% 
    mutate(razon = value/(pop_tot/100000)) %>% 
    select(-nom_ent, -state) %>% 
    left_join(catalogo_abreviaturas) %>% 
    mutate(id_dimension = "01", 
           id_indicador = "13") %>% 
    ungroup() %>% 
    select(cve_ent,entidad_abr_m,anio=year,id_dimension,id_indicador,indicador_value=razon) %>% 
    filter(as.numeric(cve_ent) <= 32) %>% 
    arrange(anio, cve_ent)

openxlsx::write.xlsx(datos, "02_datos_crudos/01_04_13_mortalidad_transito.xlsx")
