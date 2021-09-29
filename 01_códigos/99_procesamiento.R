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
d <- haven::read_sav("~/downloads/bd.sav")


test <- d %>% 
  select(nedo, edo, añoinicio, añofinal, numlegis, femtot, maletot) %>% 
  group_by(nedo, edo, añoinicio, añofinal, numlegis) %>% 
  summarise(tot_fem = sum(femtot),
            tot_hom = sum(maletot)) %>% 
  ungroup() %>% 
  mutate(paridad = abs((tot_hom / tot_fem)-1),
         cve_ent = str_pad(nedo, 2, "l", "0")) %>% 
  select(cve_ent, añoinicio, añofinal, paridad) %>% 
  drop_na(paridad) %>% 
  complete(cve_ent, añoinicio) %>% 
  fill(paridad, .direction = "down") %>% 
  select(cve_ent, añoinicio, paridad) 



openxlsx::write.xlsx(
  
  test %>% 
    bind_rows(
      test %>% 
        group_by(añoinicio) %>% 
        summarise(paridad = mean(paridad)) %>% 
        ungroup() %>% 
        mutate(cve_ent = "00")
    ) %>% 
    arrange(añoinicio, cve_ent) %>% 
    mutate(entidad_abr_m = case_when(
      cve_ent == "00" ~ "Nacional", 
      cve_ent == "01" ~ "AGS", 
      cve_ent == "02" ~ "BC",
      cve_ent == "03" ~ "BCS",
      cve_ent == "04" ~ "CAMP",
      cve_ent == "05" ~ "COAH",
      cve_ent == "06" ~ "COL", 
      cve_ent == "07" ~ "CHPS",
      cve_ent == "08" ~ "CHIH",
      cve_ent == "09" ~ "CDMX",
      cve_ent == "10" ~ "DGO",
      cve_ent == "11" ~ "GTO",
      cve_ent == "12" ~ "GRO", 
      cve_ent == "13" ~ "HGO",
      cve_ent == "14" ~ "JAL",
      cve_ent == "15" ~ "MEX",
      cve_ent == "16" ~ "MICH",
      cve_ent == "17" ~ "MOR", 
      cve_ent == "18" ~ "NAY",
      cve_ent == "19" ~ "NL",
      cve_ent == "20" ~ "OAX",
      cve_ent == "21" ~ "PUE",
      cve_ent == "22" ~ "QRO", 
      cve_ent == "23" ~ "QROO",
      cve_ent == "24" ~ "SLP",
      cve_ent == "25" ~ "SIN",
      cve_ent == "26" ~ "SON",
      cve_ent == "27" ~ "TAB", 
      cve_ent == "28" ~ "TAM",
      cve_ent == "29" ~ "TLAX",
      cve_ent == "30" ~ "VER",
      cve_ent == "31" ~ "YUC",
      cve_ent == "32" ~ "ZAC")),
  
  "~/desktop/99_ips.xlsx"
  
)
