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