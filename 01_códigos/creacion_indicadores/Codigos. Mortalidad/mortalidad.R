# Librerias ----
library(tidyverse)
library(inegiR)

# Datos ----
cat_edos <- readxl::read_xlsx("catalogo_estatal.xlsx")
conapo_todos <- readxl::read_xlsx("0_Pob_Mitad_1950_2070.xlsx") %>% 
  mutate(cve_ent = str_pad(CVE_GEO, width = 2, side = "left", pad = "0")) 
conapo_babies <- conapo_todos %>% filter(EDAD == 0) %>% 
  group_by(AÑO, ENTIDAD, CVE_GEO) %>% 
  summarise(pop = sum(POBLACION, na.rm = T)) %>% 
  mutate(cve_ent = str_pad(CVE_GEO, width = 2, side = "left", pad = "0")) %>% 
  select(-CVE_GEO) %>% 
  ungroup() %>% 
  rename(anio = AÑO, 
         nom_ent = ENTIDAD)
conapo_pob_estatal <- readxl::read_xlsx("0_Pob_Mitad_1950_2070.xlsx") %>% 
  group_by(AÑO, ENTIDAD, CVE_GEO) %>% 
  summarise(pop = sum(POBLACION, na.rm = T)) %>% 
  mutate(cve_ent = str_pad(CVE_GEO, width = 2, side = "left", pad = "0")) %>% 
  select(-CVE_GEO) %>% 
  ungroup() %>% 
  rename(anio = AÑO, 
         nom_ent = ENTIDAD)
# lme <- 
  cie <- readxl::read_xlsx("CIE2.xlsx") %>% 
  pivot_longer(cols = 3:ncol(.)) %>% 
    mutate(anio = str_extract(name, "\\-\\d+$") %>% 
             str_remove(pattern = "\\-") %>% as.numeric(), 
           valor = str_remove_all(value, "\\,") %>% as.numeric(), 
           enfermedad = str_remove(name, "\\-\\d+$")) %>% 
    select(-name, -value)
  
lme <- readxl::read_xlsx("INEGI_LISTA_MEXICANA_ENFERMEDADES.xlsx") %>% 
    pivot_longer(cols = 3:ncol(.)) %>% 
    mutate(anio = str_extract(name, "\\-\\d+$") %>% 
             str_remove(pattern = "\\-") %>% as.numeric(), 
           valor = str_remove_all(value, "\\,") %>% as.numeric(), 
           enfermedad = str_remove(name, "\\-\\d+$")) %>% 
    select(-name, -value)

# - Mortalidad por enfermedades infecciosas (CIE) ----
cie$enfermedad %>% unique()
  
def_inf <- cie %>% 
  filter(enfermedad %in% c("Enfermedades infecciosas y parasitarias",
                           "U069 Enfermedad por el virus del Zika, sin especificación",
                           "Ciertas enfermedades infecciosas y parasitarias (A00-B99)", 
                           "Códigos relacionados a COVID-19 : U07.1, U07.2 y U10.9")) %>% 
  group_by(cve_ent,nom_ent,anio) %>% 
  summarise(valor = sum(valor, na.rm = T)) %>% 
  ungroup()

enfermedades_infecciosas <- def_inf %>% 
  left_join(conapo_pob_estatal, by = c("cve_ent", "anio")) %>% 
  mutate(tasa = valor/(pop/100000)) %>% 
  select(cve_ent, anio, indicador_value = tasa) %>% 
  mutate(id_dimension = "01", 
         id_indicador = "04") %>% 
  left_join(cat_edos) %>% 
  select(cve_ent,entidad_abr_m,anio,id_dimension,id_indicador,indicador_value) %>% 
  filter(!is.na(entidad_abr_m)) %>% 
  filter(!is.na(indicador_value))

openxlsx::write.xlsx(enfermedades_infecciosas, 
                     "resultados/enf_inf.xlsx")

# - Mortalidad por diabetes (CIE) ----

def_diabetes <- cie %>% 
  filter(enfermedad %in% c("Diabetes mellitus (E10-E14)")) %>% 
  group_by(cve_ent,nom_ent,anio) %>% 
  summarise(valor = sum(valor)) %>% 
  ungroup() %>% 
  filter(!is.na(valor))

diabetes <- def_diabetes %>% 
  left_join(conapo_pob_estatal, by = c("cve_ent", "anio")) %>% 
  mutate(tasa = valor/(pop/100000)) %>% 
  select(cve_ent, anio, indicador_value = tasa) %>% 
  mutate(id_dimension = "02", 
         id_indicador = "29") %>% 
  left_join(cat_edos) %>% 
  select(cve_ent,entidad_abr_m,anio,id_dimension,id_indicador,indicador_value) %>% 
  filter(!is.na(entidad_abr_m)) %>% 
  filter(!is.na(indicador_value))

diabetes %>%
  group_by(cve_ent, anio) %>%
  count() %>%
  arrange(-n)

openxlsx::write.xlsx(diabetes, 
                     "resultados/diabetes.xlsx")

# - Mortalidad por enfermedades circulatorias (CIE) ----
circ <- cie %>% 
  filter(enfermedad %in% c("Enfermedades del sistema circulatorio (I00-I99)")) %>% 
  group_by(cve_ent,nom_ent,anio) %>% 
  summarise(valor = sum(valor)) %>% 
  ungroup() %>% 
  filter(!is.na(valor)) %>% 
  filter(as.numeric(cve_ent) <= 32)

circ <- circ %>% 
  left_join(conapo_pob_estatal, by = c("cve_ent", "anio")) %>% 
  mutate(tasa = valor/(pop/100000)) %>% 
  select(cve_ent, anio, indicador_value = tasa) %>% 
  mutate(id_dimension = "02", 
         id_indicador = "28") %>% 
  left_join(cat_edos) %>% 
  select(cve_ent,entidad_abr_m,anio,id_dimension,id_indicador,indicador_value) %>% 
  filter(!is.na(entidad_abr_m)) %>% 
  filter(!is.na(indicador_value))

# circ %>% 
#   group_by(cve_ent, anio) %>% 
#   count() %>% 
#   arrange(-n)

openxlsx::write.xlsx(circ, 
                     "resultados/circulatorios.xlsx")

# Suicidios ----
# unique(lme$enfermedad)
sui <- lme %>% 
  filter(enfermedad %in% c("(E54) Lesiones autoinfligidas intencionalmente")) %>% 
  group_by(cve_ent,nom_ent,anio) %>% 
  summarise(valor = sum(valor)) %>% 
  ungroup()

suicidios <- sui %>% 
  left_join(conapo_pob_estatal, by = c("cve_ent", "anio")) %>% 
  mutate(tasa = valor/(pop/100000)) %>% 
  select(cve_ent, anio,  indicador_value = tasa) %>% 
  mutate(id_dimension = "02", 
         id_indicador = "27") %>% 
  left_join(cat_edos) %>% 
  select(cve_ent,entidad_abr_m,anio,id_dimension,id_indicador,indicador_value) %>% 
  filter(!is.na(entidad_abr_m)) %>% 
  filter(!is.na(indicador_value))

openxlsx::write.xlsx(suicidios, 
                     "resultados/suicidios.xlsx")

# Accidentes de transporte ----
transporte <- lme %>% 
  filter(enfermedad %in% c("(E49) Accidentes de transporte")) %>%
  group_by(cve_ent,nom_ent,anio) %>% 
  summarise(valor = sum(valor)) %>% 
  ungroup() %>% 
  filter(!is.na(valor)) %>% 
  left_join(conapo_pob_estatal, by = c("cve_ent", "anio")) %>% 
  mutate(tasa = valor/(pop/100000)) %>% 
  select(cve_ent, anio,  indicador_value = tasa) %>% 
  mutate(id_dimension = "01", 
         id_indicador = "13") %>% 
  left_join(cat_edos) %>% 
  select(cve_ent,entidad_abr_m,anio,id_dimension,id_indicador,indicador_value) %>% 
  filter(!is.na(entidad_abr_m)) %>% 
  filter(!is.na(indicador_value)) %>% 
  ungroup()

transporte %>% 
  group_by(cve_ent, anio) %>% 
  count() %>% 
  arrange(-n)

openxlsx::write.xlsx(transporte, 
                     "resultados/transporte.xlsx")

# Tasas por homicidio ----
hom <- readxl::read_xlsx("INEGI_exporta_HOMICIDIOS.xlsx", skip = 4) %>% 
  pivot_longer(cols = 3:ncol(.)) %>% 
  mutate(anio = str_extract(name, "\\d+$") %>% as.numeric(), 
         valor = str_remove_all(value, "\\,") %>% as.numeric()) %>% 
  select(-name, -value) %>% 
  left_join(conapo_pob_estatal, by = c("cve_ent", "anio")) %>% 
  mutate(tasa = valor/(pop/100000)) %>% 
  select(cve_ent, anio,indicador_value = tasa) %>% 
  mutate(id_dimension = "01", 
         id_indicador = "12") %>% 
  left_join(cat_edos) %>% 
  select(cve_ent,entidad_abr_m,anio,id_dimension,id_indicador,indicador_value) %>% 
  filter(!is.na(entidad_abr_m)) %>% 
  filter(!is.na(indicador_value))

openxlsx::write.xlsx(hom, 
                     "resultados/hom.xlsx")

# Mortalidad Infantil ----
inf <- readxl::read_xlsx("INEGI_exporta_INFANTILES.xlsx", skip = 4) %>% 
  pivot_longer(cols = 3:ncol(.)) %>% 
  mutate(anio = str_extract(name, "\\d+$"), 
         valor = str_remove_all(value, "\\,") %>% as.numeric()) %>% 
  select(-name, -value) %>% 
  filter(!is.na(valor)) %>% 
  filter(as.numeric(cve_ent) <= 32)

nac <- readxl::read_xlsx("INEGI_exporta_nacimientos.xlsx", skip = 4) %>% 
  pivot_longer(
    cols      = -c(anio_registro, anio_ocurrencia), 
    names_to  = "entidad", 
    values_to = "total")                    %>% 
  # Cambiar a variable numérica 
  mutate(nacimientos = as.numeric(str_remove_all(total, ","))) %>% 
  # Agrupar por año de ocurrencia y estimar totales 
  group_by(entidad, anio_ocurrencia)          %>% 
  summarise(nacimientos = sum(nacimientos, na.rm = TRUE)) %>% 
  ungroup()                                   %>% 
  rename(anio = anio_ocurrencia)              %>% 
  # Agregar variables de identificación 
  # mutate(
  #   id_dimension = "01", 
  #   id_indicador = "03") %>%
  filter(anio %in% c(1990:2022)) %>% 
  left_join(cat_edos)

nac$cve_ent[nac$entidad == "Total"] <- "00"
nac$entidad_abr_m[nac$entidad == "Total"] <- "Nacional"
nac$cve_ent[nac$entidad == "Coahuila de Zaragoza"] <- "05"
nac$entidad_abr_m[nac$entidad == "Coahuila de Zaragoza"] <- "COAH"
nac$entidad_abr_m[nac$entidad == "Michoacán de Ocampo"] <- "MICH"
nac$cve_ent[nac$entidad == "Michoacán de Ocampo"] <- "16"
nac$entidad_abr_m[nac$entidad == "Veracruz de Ignacio de la Llave"] <- "VER"
nac$cve_ent[nac$entidad == "Veracruz de Ignacio de la Llave"] <- "30"

nac <- conapo_babies %>% 
  mutate(anio = as.character(anio)) %>% 
  select(nom_ent, 
         anio,
         nacimientos = pop, 
         cve_ent) %>% 
  left_join(cat_edos) %>% 
  select(-nom_ent)

mort_inf <- left_join(inf, nac, by = c("anio", "cve_ent")) %>% 
  mutate(tasa = valor/(nacimientos/1000)) %>% 
  mutate(id_dimension = "01", 
         id_indicador = "03") %>% 
  select(cve_ent,entidad_abr_m,anio,id_dimension,id_indicador,indicador_value = tasa) %>% 
  filter(!is.na(entidad_abr_m)) 

openxlsx::write.xlsx(mort_inf, "resultados/mort_inf.xlsx")

# Mortalidad materna ----
mat <- readxl::read_xlsx("INEGI_exporta_MORT_MATERNA.xlsx", skip = 4) %>% 
  pivot_longer(cols = 3:ncol(.)) %>% 
  mutate(anio = str_extract(name, "\\d+$"), 
         valor = str_remove_all(value, "\\,") %>% as.numeric()) %>% 
  select(-name, -value) %>% 
  filter(!is.na(valor)) %>% 
  filter(as.numeric(cve_ent) <= 32)

mort_mat <- left_join(mat,nac, by = c("anio", "cve_ent")) %>% 
  mutate(tasa = valor/(nacimientos/100000))%>% 
  mutate(id_dimension = "01", 
         id_indicador = "02") %>%
  select(cve_ent,entidad_abr_m,anio,id_dimension,id_indicador,indicador_value = tasa)
  
# %>% 
#   select(cve_ent,entidad_abr_m,anio,id_dimension,id_indicador,indicador_value = tasa) %>% 
#   filter(!is.na(entidad_abr_m)) %>% 
#   filter(!is.na(indicador_value))

openxlsx::write.xlsx(mort_mat, 
                     "resultados/mort_mat.xlsx")

