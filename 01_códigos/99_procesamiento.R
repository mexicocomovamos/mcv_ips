#ENH ----
enh <- read.dbf(
  "~/downloads/vivienda.dbf 3", as.is = T
) %>% 
  janitor::clean_names()

table(enh$escrituras)
test <- enh %>% 
  mutate(
    cve_ent = str_sub(folioviv,1,2),
    escrituras = case_when(
      escrituras < 5 ~ "Usa",
      escrituras == 9 ~ NA_character_,
      T ~ "Otro"
    ),
    factor = factor_viv
  ) %>% 
  drop_na(escrituras) %>% 
  select(cve_ent, escrituras, factor) %>% 
  group_by(cve_ent, escrituras) %>% 
  summarise(tot_escrituras = sum(factor)) %>% 
  ungroup() %>% 
  group_by(cve_ent) %>% 
  mutate(
    tot = sum(tot_escrituras),
    prop_escrituras = round(tot_escrituras/tot*100,4)
  ) %>%
  filter(escrituras=="Usa") %>% 
  select(cve_ent, prop_escrituras)

openxlsx::write.xlsx(
  
  test,
  
  "~/desktop/99_ips.xlsx"
  
)

# LAPOP ----

lapop <- haven::read_dta("cide/tesis/r/tesis/LAPOP_2019.dta")

test <- lapop %>% 
  mutate(
    cve_ent = str_pad(as.numeric(prov), 2, "l", "0"),
    matrimonio = ifelse(
      as.numeric(d6) >= 6, "Aprueba", "Desaprueba"
    )
  ) %>% 
  drop_na(matrimonio) %>% 
  as_survey_design(weghts = wt) %>% 
  group_by(cve_ent, matrimonio) %>% 
  summarise(prop_matr = survey_mean(na.rm = T)) %>% 
  mutate(prop_matr = round(prop_matr*100,4)) %>% 
  filter(matrimonio == "Aprueba")

# Diputaciones ----
dip <- read.delim("downloads/diputaciones.csv",
                  sep ="|", strip.white = TRUE, skip = 5, encoding = "latin1"
)
table(dip$NOMBRE_ESTADO)


test <- dip %>% 
  janitor::clean_names() %>% 
  group_by(id_estado, nombre_estado) %>% 
  summarise(tot_votos = sum(as.numeric(total_votos_calculados), na.rm = T),
            lista_nom = sum(as.numeric(lista_nominal_casilla), na.rm = T)) %>% 
  ungroup() %>% 
  mutate(part_eletoral = round(tot_votos/lista_nom*100, 4)) 

# Censo ----
test <- viv_2020 %>% 
  mutate(
    cve_ent = str_pad(ent, 2, "l", "0"),
    cuadorm = ifelse(cuadorm == "99", NA, cuadorm),
    hacinamiento = case_when(
      as.numeric(numpers)/as.numeric(cuadorm) >= 2.5 ~ "Hacinamiento",
      T ~ "Otro"
    )
  ) %>% 
  drop_na(hacinamiento) %>% 
  select(cve_ent, hacinamiento, factor) %>% 
  group_by(cve_ent, hacinamiento) %>% 
  summarise(tot_hacinamiento = sum(factor)) %>% 
  ungroup() %>% 
  group_by(cve_ent) %>% 
  mutate(
    tot = sum(tot_hacinamiento),
    prop_hacinamiento = round(tot_hacinamiento/tot*100,4)
  ) %>%
  filter(hacinamiento=="Hacinamiento") %>% 
  select(cve_ent, prop_hacinamiento)

openxlsx::write.xlsx(
  
  test,
  
  "~/desktop/99_ips.xlsx"
  
)


# conagua ----
require(sf)
edos_mex <- read_sf("mx_ents_fed/mx_ents_fed.shp") 

conagua_vec <- c(
  "Grado_presion_RHA_2020",
  "Grado_presion_RHA_2019",
  "Grado_presion_RHA_2018",
  "Grado_presion_RHA_2017",
  "Grado_presion_RHA_2016",
  "Grado_presion_RHA_2015"
)

interesections_final <- data.frame()
for(i in 1:6){
  
  presion <- st_read(paste0("downloads/", conagua_vec[i], "/", conagua_vec[i], ".shp"))%>% 
    st_transform(crs = "WGS84")
  
  intersection <- sf::st_intersection(x = presion, y = edos_mex)
  
  intersection_area <- intersection %>% 
    mutate(area = sf::st_area(.)) %>% 
    group_by(ESTADO) %>% 
    mutate(area_estatal = sum(area, na.rm = T),
           por_area = (area*100)/area_estatal,
           porc_gp = porc_gp/100) %>% 
    as_tibble() %>% 
    select(-geometry)
  
  interesections_final <- bind_rows(
    interesections_final,
    intersection_area  %>% 
      mutate(porc_gp_pond = porc_gp*por_area) %>% 
      group_by(año, CVE_ENT, ESTADO) %>% 
      summarise(porc_gp_pond = sum(porc_gp_pond, na.rm = T)/100) 
  )
  
  
  
  
}


interesections_final %>% 
  mutate(porc_gp_pond = as.numeric(str_remove_all(porc_gp_pond, " [1]")))

openxlsx::write.xlsx(
  
  interesections_final %>% 
    mutate(porc_gp_pond = as.numeric(str_remove_all(porc_gp_pond, " [1]"))),
  
  "~/desktop/99_ips.xlsx"
  
)


# congresos locales ----
d <- haven::read_sav("downloads/bd.sav")


test <- d %>% 
  select(nedo, edo, añoinicio, numlegis, femtot) %>% 
  group_by(nedo, edo, añoinicio, numlegis) %>% 
  summarise(tot_fem = sum(femtot)) %>% 
  ungroup() %>% 
  mutate(prop_fem = round(tot_fem / as.numeric(numlegis) * 100, 4)) %>% 
  drop_na(prop_fem)