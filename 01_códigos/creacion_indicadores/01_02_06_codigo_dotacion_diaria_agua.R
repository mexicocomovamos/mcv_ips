
# No hay bucles aquí.... hacer todo paso por paso.

library(tidyverse)
library(srvyr)

viv_2014 <- "2014/viviendas_enigh2014ncv/conjunto_de_datos/viviendas.csv"
viv_2016 <- "2016/conjunto_de_datos_viviendas_enigh_2016_ns/conjunto_de_datos/conjunto_de_datos_viviendas_enigh_2016_ns.csv"
viv_2018 <- "2018/conjunto_de_datos_vivienda_enigh_2018_ns/conjunto_de_datos/conjunto_de_datos_viviendas_enigh_2018_ns.csv"
viv_2020 <- "2020/conjunto_de_datos_viviendas_enigh_2020_ns/conjunto_de_datos/conjunto_de_datos_viviendas_enigh_2020_ns.csv"
viv_2022 <- "2022/viviendas.csv"

hog_2014 <- "2014/concentradohogar_enigh2014ncv/conjunto_de_datos/concentradohogar.csv"
hog_2016 <- "2016/conjunto_de_datos_concentradohogar_enigh_2016_ns/conjunto_de_datos/conjunto_de_datos_concentradohogar_enigh_2016_ns.csv"
hog_2018 <- "2018/conjunto_de_datos_concentradohogar_enigh_2018_ns/conjunto_de_datos/conjunto_de_datos_concentradohogar_enigh_2018_ns.csv"
hog_2020 <- "2020/conjunto_de_datos_concentradohogar_enigh_2020_ns/conjunto_de_datos/conjunto_de_datos_concentradohogar_enigh_2020_ns.csv"
hog_2022 <- "2022/concentradohogar.csv"

# 2014 ----
viviendas <- read_csv(viv_2014) %>%
  filter(dotac_agua == 1) %>%
  select(folioviv) %>%
  mutate(dotacion_diaria = T)

hogares <- read_csv(hog_2014) %>%
  mutate(entidad = str_extract(folioviv, "^\\d\\d")) %>%
  rename(factor = factor_hog) %>%
  select(folioviv, foliohog, factor, entidad) # 70301

dt_2014 <- left_join(hogares, viviendas) %>%
  mutate(dotacion_diaria = ifelse(is.na(dotacion_diaria), yes = F, no = T)) %>%
  as_survey_design(weights = factor) %>%
  group_by(entidad) %>%
  summarise(pp = 100*survey_mean(dotacion_diaria, na.rm = T)) %>%
  arrange(-pp) %>%
  mutate(year = 2014) %>%
  rbind(left_join(hogares, viviendas) %>%
          mutate(dotacion_diaria = ifelse(is.na(dotacion_diaria), yes = F, no = T)) %>%
          as_survey_design(weights = factor) %>%
          summarise(pp = 100*survey_mean(dotacion_diaria, na.rm = T)) %>%
          arrange(-pp) %>%
          mutate(year = 2014) %>%
          mutate(entidad = "00") %>%
          select(entidad,pp,pp_se,year)) %>%
  arrange(entidad)

# 2016 ----
viviendas <- read_csv(viv_2016) %>%
  filter(dotac_agua == 1) %>%
  select(folioviv) %>%
  mutate(dotacion_diaria = T)

hogares <- read_csv(hog_2016) %>%
  mutate(entidad = str_extract(folioviv, "^\\d\\d")) %>%
  select(folioviv, foliohog, factor, entidad) # 70301

dt_2016 <- left_join(hogares, viviendas) %>%
  mutate(dotacion_diaria = ifelse(is.na(dotacion_diaria), yes = F, no = T)) %>%
  as_survey_design(weights = factor) %>%
  group_by(entidad) %>%
  summarise(pp = 100*survey_mean(dotacion_diaria, na.rm = T)) %>%
  arrange(-pp) %>%
  mutate(year = 2016) %>%
  rbind(left_join(hogares, viviendas) %>%
      mutate(dotacion_diaria = ifelse(is.na(dotacion_diaria), yes = F, no = T)) %>%
      as_survey_design(weights = factor) %>%
      summarise(pp = 100*survey_mean(dotacion_diaria, na.rm = T)) %>%
      arrange(-pp) %>%
      mutate(year = 2016) %>%
      mutate(entidad = "00") %>%
      select(entidad,pp,pp_se,year)) %>%
  arrange(entidad)

# 2018 ----
viviendas <- read_csv(viv_2018) %>%
  filter(dotac_agua == 1) %>%
  select(folioviv) %>%
  mutate(dotacion_diaria = T)

hogares <- read_csv(hog_2018) %>%
  mutate(entidad = str_extract(folioviv, "^\\d\\d")) %>%
  select(folioviv, foliohog, factor, entidad) # 70301

dt_2018 <- left_join(hogares, viviendas) %>%
  mutate(dotacion_diaria = ifelse(is.na(dotacion_diaria), yes = F, no = T)) %>%
  as_survey_design(weights = factor) %>%
  group_by(entidad) %>%
  summarise(pp = 100*survey_mean(dotacion_diaria, na.rm = T)) %>%
  arrange(-pp) %>%
  mutate(year = 2018) %>%
  rbind(left_join(hogares, viviendas) %>%
          mutate(dotacion_diaria = ifelse(is.na(dotacion_diaria), yes = F, no = T)) %>%
          as_survey_design(weights = factor) %>%
          summarise(pp = 100*survey_mean(dotacion_diaria, na.rm = T)) %>%
          arrange(-pp) %>%
          mutate(year = 2018) %>%
          mutate(entidad = "00") %>%
          select(entidad,pp,pp_se,year)) %>%
  arrange(entidad)


# 2020 ----
viviendas <- read_csv(viv_2020) %>%
  filter(dotac_agua == 1) %>%
  select(folioviv) %>%
  mutate(dotacion_diaria = T)

hogares <- read_csv(hog_2020) %>%
  mutate(entidad = str_extract(folioviv, "^\\d\\d")) %>%
  select(folioviv, foliohog, factor, entidad) # 70301

dt_2020 <- left_join(hogares, viviendas) %>%
  mutate(dotacion_diaria = ifelse(is.na(dotacion_diaria), yes = F, no = T)) %>%
  as_survey_design(weights = factor) %>%
  group_by(entidad) %>%
  summarise(pp = 100*survey_mean(dotacion_diaria, na.rm = T)) %>%
  arrange(-pp) %>%
  mutate(year = 2020) %>%
  rbind(left_join(hogares, viviendas) %>%
          mutate(dotacion_diaria = ifelse(is.na(dotacion_diaria), yes = F, no = T)) %>%
          as_survey_design(weights = factor) %>%
          summarise(pp = 100*survey_mean(dotacion_diaria, na.rm = T)) %>%
          arrange(-pp) %>%
          mutate(year = 2020) %>%
          mutate(entidad = "00") %>%
          select(entidad,pp,pp_se,year)) %>%
  arrange(entidad)

# 2022 ----
viviendas <- read_csv(viv_2022) %>%
  filter(dotac_agua == 1) %>%
  select(folioviv) %>%
  mutate(dotacion_diaria = T)

hogares <- read_csv(hog_2022) %>%
  mutate(entidad = str_extract(folioviv, "^\\d\\d")) %>%
  select(folioviv, foliohog, factor, entidad) # 70301

dt_2022 <- left_join(hogares, viviendas) %>%
  mutate(dotacion_diaria = ifelse(is.na(dotacion_diaria), yes = F, no = T)) %>%
  as_survey_design(weights = factor) %>%
  group_by(entidad) %>%
  summarise(pp = 100*survey_mean(dotacion_diaria, na.rm = T)) %>%
  arrange(entidad) %>%
  mutate(year = 2022) %>%
  rbind(rbind(left_join(hogares, viviendas) %>%
                  mutate(dotacion_diaria = ifelse(is.na(dotacion_diaria), yes = F, no = T)) %>%
                  as_survey_design(weights = factor) %>%
                  summarise(pp = 100*survey_mean(dotacion_diaria, na.rm = T)) %>%
                  arrange(-pp) %>%
                  mutate(year = 2022) %>%
                  mutate(entidad = "00") %>%
                  select(entidad,pp,pp_se,year))) %>%
  arrange(entidad)

datos_final <- rbind(dt_2014, dt_2016, dt_2018, dt_2020, dt_2022)
openxlsx::write.xlsx(datos_final, "20241104_excel_hogares_dotacion_diaria_agua.xlsx")

