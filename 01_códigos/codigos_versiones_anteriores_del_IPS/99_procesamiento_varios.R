Sys.setlocale("LC_TIME", "es_ES")

# Paquetes ----
if(!require("lubridate")) install.packages("lubridate") & require("lubridate")
if(!require("hot.deck")) install.packages("hot.deck") & require("hot.deck")
if(!require("zoo")) install.packages("zoo") & require("zoo")
if(!require("stringi")) install.packages("stringi") & require("stringi")
if(!require("gganimate")) install.packages("gganimate") & require("gganimate")
if(!require("gridExtra")) install.packages("gridExtra") & require("gridExtra")
if(!require("ggthemes")) install.packages("ggthemes") & require("ggthemes")
if(!require("hrbrthemes")) install.packages("hrbrthemes") & require("hrbrthemes")
if(!require("magick")) install.packages("magick") & require("magick")
if(!require("scales")) install.packages("scales") & require("scales")
if(!require("RColorBrewer")) install.packages("RColorBrewer") & require("RColorBrewer")
if(!require("foreign")) install.packages("foreign") & require("foreign")
if(!require("srvyr")) install.packages("srvyr") & require("srvyr")
if(!require("openxlsx")) install.packages("openxlsx") & require("openxlsx")
if(!require("ggalt")) install.packages("ggalt") & require("ggalt")
if(!require("mxmaps")) install.packages("mxmaps") & require("mxmaps")
if(!require("ggpubr")) install.packages("ggpubr") & require("ggpubr")

require(tidyverse)

d <- read.dbf(
  "downloads/TModulo1.DBF", as.is = T
) %>% 
  janitor::clean_names()


test <- d %>% 
  select(control, p4_4_2, p4_4_3, p4_4_4, p4_4_5, fac_per) %>% 
  mutate_at(vars(starts_with("p4")), ~as.numeric(.)) %>% 
  mutate(
    cve_ent = str_sub(control,1,2),
    hora_traslado = p4_4_2 + p4_4_4 + (p4_4_3/60) + (p4_4_5/60),
    hora_traslado = hora_traslado/7,
    hora_traslado_2 = ifelse(
      hora_traslado == 0, NA, ifelse(
        hora_traslado >= 2, "Mayor o igual a 2 hrs", "Menor a 2 hrs"
      )
    )
  ) %>% 
  drop_na(hora_traslado_2) %>% 
  as_survey_design(weights = fac_per) %>% 
  group_by(cve_ent, hora_traslado_2) %>% 
  summarise(hora_traslado_prop = survey_mean(na.rm = T))

openxlsx::write.xlsx(
  test %>% 
    mutate(hora_traslado_prop = round(hora_traslado_prop*100,4)) %>% 
    filter(hora_traslado_2 == "Mayor o igual a 2 hrs") %>% 
    select(cve_ent, hora_traslado_prop), "desktop/99_ips.xlsx"
)


d <- read.csv("downloads/TMODULO.csv") %>% 
  janitor::clean_names()

test <- d %>% 
  select(ent, p5_4_1, p5_4_2, p5_4_3, p5_4_4, fac_per) %>% 
  mutate_at(vars(starts_with("p5")), ~as.numeric(.)) %>% 
  mutate(
    cve_ent = str_pad(ent, 2, "l", "0"),
    hora_traslado = p5_4_1 + p5_4_3 + (p5_4_2/60) + (p5_4_4/60),
    hora_traslado = hora_traslado/7,
    hora_traslado_2 = ifelse(
      hora_traslado == 0, NA, ifelse(
        hora_traslado >= 2, "Mayor o igual a 2 hrs", "Menor a 2 hrs"
      )
    )
  ) %>% 
  drop_na(hora_traslado_2) %>% 
  as_survey_design(weights = fac_per) %>% 
  group_by(cve_ent, hora_traslado_2) %>% 
  summarise(hora_traslado_prop = survey_mean(na.rm = T))



openxlsx::write.xlsx(
  test %>% 
    mutate(hora_traslado_prop = round(hora_traslado_prop*100,4)) %>% 
    filter(hora_traslado_2 == "Mayor o igual a 2 hrs") %>% 
    select(cve_ent, hora_traslado_prop), "desktop/99_ips.xlsx"
)


# conapo ----
d <- read_csv("downloads/prot_san_sal_ev_proyecciones.csv",
              locale = locale(encoding = "ISO-8859-1")) %>% 
  janitor::clean_names()


d %>% glimpse



openxlsx::write.xlsx(
  d %>% 
    filter(ano > 2015, ano < 2022) %>% 
    mutate(
      cve_ent = str_pad(cve_geo, 2, "l", "0")
    ) %>% 
    select(cve_ent, ano, ev) %>% 
    arrange(ano), "desktop/99_ips.xlsx"
)

# nacimientos ----
d <- readxl::read_excel("downloads/nac_ado.xlsx", sheet = 2) %>% 
  janitor::clean_names()


openxlsx::write.xlsx(
  d %>%
    pivot_longer(
      cols = starts_with("x"),
      names_prefix = "x",
      names_to = "anio"
    ) %>% 
    arrange(anio, cve_ent) %>% 
    mutate(value = round(value,4)), "desktop/99_ips.xlsx"
)

# ensanut ----
d <- read_csv("downloads/integrantes_ensanut2020_w.csv",
              locale = locale(encoding = "ISO-8859-1")) %>% 
  janitor::clean_names()

test <- d %>% 
  select(entidad, starts_with("h0902"), ponde_f20) %>% 
  mutate_at(vars(starts_with("p5")), ~as.numeric(.)) %>% 
  mutate(
    cve_ent = str_pad(entidad, 2, "l", "0"),
    obesidad = case_when(
      h0902a == 2 | h0902b == 2 | h0902c == 2 | h0902d == 2 | h0902e == 2 ~ "Obesidad",
      T ~ "Sin obesidad"
    )
  ) %>% 
  drop_na(obesidad) %>% 
  as_survey_design(weights = ponde_f20) %>% 
  group_by(cve_ent, obesidad) %>% 
  summarise(hora_traslado_prop = survey_mean(na.rm = T))



d <- readxl::read_excel("downloads/ARCHIVO MAESTRO ENSANUT COMPLETO.xlsx")


# censos economicos ----
d <- read_csv("downloads/ce2019_ags.csv",
              locale = locale(encoding = "ISO-8859-1")) %>% 
  janitor::clean_names()



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
edos_mex <- read_sf("02_datos_crudos/mx_ents_fed/mx_ents_fed.shp") 

conagua_vec <- c(
    "Grado_presion_RHA_2020",
    "Grado_presion_RHA_2019",
    "Grado_presion_RHA_2018",
    "Grado_presion_RHA_2017",
    "Grado_presion_RHA_2016",
    "Grado_presion_RHA_2015",
    "Grado_presion_RHA_2021"
)

interesections_final <- data.frame()
for(i in 7){
    
    presion <- st_read(paste0("~/downloads/", conagua_vec[i], "/", conagua_vec[i], ".shp"))%>% 
        st_transform(crs = "WGS84")
    
    intersection <- sf::st_intersection(x = presion, y = edos_mex)
    
    intersection_area <- intersection %>% 
        mutate(area = sf::st_area(.)) %>% 
        group_by(nom_ent) %>% 
        mutate(area_estatal = sum(area, na.rm = T),
               por_area = (area*100)/area_estatal,
               porc_gp = porc_gp/100) %>% 
        as_tibble() %>% 
        select(-geometry)
    
    interesections_final <- bind_rows(
        interesections_final,
        intersection_area  %>% 
            mutate(porc_gp_pond = porc_gp*por_area) %>% 
            group_by(año, nom_ent) %>% 
            summarise(porc_gp_pond = sum(porc_gp_pond, na.rm = T)/100) 
    ) %>% 
    ungroup()
    
    
    
    
}


interesections_final %>% 
    rename(entidad = nom_ent) %>% 
    left_join(readxl::read_excel("02_datos_crudos/00_cve_ent.xlsx") %>% select(entidad, cve_ent)) %>% 
    mutate(porc_gp_pond = as.numeric(str_remove_all(porc_gp_pond, " [1]")))

openxlsx::write.xlsx(
    
    interesections_final %>% 
        rename(entidad = nom_ent) %>% 
        left_join(readxl::read_excel("02_datos_crudos/00_cve_ent.xlsx") %>% select(entidad, cve_ent)) %>% 
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
