
library(tidyverse)

load("02_datos_crudos/df_pop_state_age.Rdata")
# pop_babies <- df_crudo1 %>% 
#     filter()

load("02_datos_crudos/df_pop_state.Rdata")
pop <- df_crudo2 %>% rename(nom_ent = state) %>% 
    mutate(CVE_GEO = str_pad(CVE_GEO, side = "left", pad = "0", width = 2)) %>% 
    mutate(nom_ent = case_when(nom_ent == "Coahuila" ~ "Coahuila de Zaragoza", 
                               nom_ent == "Veracruz" ~ "Veracruz de Ignacio de la Llave", 
                               nom_ent == "Michoacán" ~ "Michoacán de Ocampo", 
                               T ~ nom_ent))

catalogo_abreviaturas <- read_csv("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/Datos/cat_edos.csv")
# unique( pop$state)
# pop$state
# unique(homicidios$`Entidad federativa de registro`)
# names(homicidios)
homicidios <- read_excel("02_datos_crudos/01_01_mortalidad/HOMICIDIOS.xlsx", 
                                       skip = 6) %>% 
    filter(!is.na(`2010`)) %>% 
    slice(-1) %>% 
    pivot_longer(2:ncol(.)) %>% 
    mutate(year = name %>% as.numeric(), 
           homicidios = value %>% as.numeric()) %>% 
    select(-c(name, value)) %>% 
    rename(nom_ent = `Entidad federativa de registro`) %>% 
    mutate(nom_ent = ifelse(nom_ent == "Total", yes = "República Mexicana",  no = nom_ent))
    
datos <- left_join(homicidios, pop) %>% 
    mutate(razon = homicidios/(pop_tot/100000)) %>% 
    arrange(year, as.numeric(CVE_GEO)) %>% 
    mutate(id_dimension = "01", 
           id_indicador = "12") %>% 
    rename(cve_ent = CVE_GEO) %>% 
    left_join(catalogo_abreviaturas) %>% 
    select(cve_ent,entidad_abr_m,anio = year,id_dimension,id_indicador,indicador_value = razon)

openxlsx::write.xlsx(datos, "02_datos_crudos/01_12_homicidios.xlsx")
