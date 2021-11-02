# 0. Configuración inicial ----

options(scipen=999)
Sys.setlocale("LC_TIME", "es_ES")

## 0.1. Paquetes ----
if(!require("lubridate")) install.packages("lubridate") & require("lubridate")
if(!require("readxl")) install.packages("readxl") & require("readxl")
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
if(!require("googledrive")) install.packages("googledrive") & require("googledrive")
if(!require("googlesheets4")) install.packages("googlesheets4") & require("googlesheets4")
if(!require("psych")) install.packages("psych") & require("psych")
if(!require("ggalluvial")) install.packages("ggalluvial") & require("ggalluvial")

# Tidyverse <3
require(tidyverse)

## 0.2 Colores MCV -----
mcv_discrete <- c(
    "#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41"
)

mcv_semaforo <- c(
    "#00b783", # verde
    "#E8D92E", # amarillo
    "#ffbd41", # naranja
    "#ff6260" # rojo
)

mcv_blacks <- c("black", "#D2D0CD", "#777777")

## 0.3. BD IPS ----
ips_2020 <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx") %>% 
    filter(anio == 2020) %>% 
    glimpse

ips_cambios_2019_2020 <-  readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx") %>% 
    filter(anio >2018)%>% 
    filter(!as.numeric(cve_ent) > 33) %>% 
    pivot_wider(
        names_from = "anio",
        names_prefix = "anio_",
        values_from = "dim_value"
    ) %>% 
    mutate(
        dif = anio_2020-anio_2019
    ) %>% 
    glimpse


# 1. Exceso de mortalidad ----
exceso_mortalidad <- readxl::read_excel("02_datos_crudos/01_exceso_mort_ent.xlsx") %>% 
    glimpse
d <- ips_cambios_2019_2020 %>% 
    filter(!cve_ent=="00") %>% 
    select(-contains("anio")) %>% 
    left_join(
        exceso_mortalidad %>% 
            select(cve_ent, exceso_mort)
    ) %>% 
    glimpse

ggplot(
    d,
    aes(
        x = exceso_mort,
        y = dif,
        label = entidad_abr_m
    )
) +
    geom_smooth(method = "lm") +
    geom_point() +
    ggrepel::geom_label_repel()+
    facet_wrap(~id_dim) 


stargazer::stargazer(
    lm(formula = dif~exceso_mort, data = d %>% filter(id_dim=="00")), 
    lm(formula = dif~exceso_mort, data = d %>% filter(id_dim=="01")), 
    lm(formula = dif~exceso_mort, data = d %>% filter(id_dim=="02")),
    lm(formula = dif~exceso_mort, data = d %>% filter(id_dim=="03")), type = "text"
)


# 2. Pobreza laboral ----
pob_lab <- readxl::read_excel("02_datos_crudos/02_04_pobreza_laboral_complete_long.xlsx") %>% 
    filter(periodo == "2020T3", sexo == "General") %>% 
    glimpse

d <- ips_2020 %>% 
    filter(!cve_ent=="00") %>% 
    select(-contains("anio")) %>% 
    left_join(
        pob_lab %>% 
            select(cve_ent, tlp)
    ) %>% 
    glimpse

ggplot(
    d,
    aes(
        x = tlp,
        y = dim_value,
        label = entidad_abr_m
    )
) +
    geom_smooth(method = "glm") +
    geom_point() +
    ggrepel::geom_label_repel()+
    facet_wrap(~id_dim) 
stargazer::stargazer(
    lm(formula = dim_value~tlp, data = d %>% filter(id_dim=="00")), 
    lm(formula = dim_value~tlp, data = d %>% filter(id_dim=="01")), 
    lm(formula = dim_value~tlp, data = d %>% filter(id_dim=="02")),
    lm(formula = dim_value~tlp, data = d %>% filter(id_dim=="03")), type = "text"
)

# 3. PIB per cápita ----
pib_p_cap <- readxl::read_excel("02_datos_crudos/03_pib_per_capita.xlsx") %>% 
    glimpse

d <- ips_2020 %>% 
    filter(!cve_ent=="00") %>% 
    select(-contains("anio")) %>% 
    left_join(
        pib_p_cap %>% 
            filter(tipo_pib=="PIB per cápita (sin actividad petrolera)") %>% 
            select(cve_ent, value_pib)
    ) %>% 
    glimpse

ggplot(
    d,
    aes(
        x = value_pib,
        y = dim_value,
        label = entidad_abr_m
    )
) +
    geom_smooth(method = "glm", formula = y ~ log(x)) +
    geom_point() +
    ggrepel::geom_label_repel()+
    facet_wrap(~id_dim) 
stargazer::stargazer(
    lm(formula = dim_value~log(value_pib), data = d %>% filter(id_dim=="00")), 
    lm(formula = dim_value~log(value_pib), data = d %>% filter(id_dim=="01")), 
    lm(formula = dim_value~log(value_pib), data = d %>% filter(id_dim=="02")),
    lm(formula = dim_value~log(value_pib), data = d %>% filter(id_dim=="03")), type = "text"
)


stargazer::stargazer(lm(formula = dim_value~exp(value_pib), data = d %>% filter(id_dim=="01")), type = "text")


