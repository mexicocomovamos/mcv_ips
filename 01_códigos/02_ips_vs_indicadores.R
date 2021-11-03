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
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    left_join(
        exceso_mortalidad %>% 
            select(cve_ent, exceso_mort)
    ) %>% 
    glimpse


titulo <- "Exceso de mortalidad vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "Exceso de mortalidad"
eje_y <- "Diferencia de puntaje estimado entre 2019 y 2020"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico.\n**No se muestra la dimensión 3. Oportunidades debido a que el modelo se estima como no significativo."

g <- 
ggplot(
    d %>% 
        filter(!id_dim=="03"),
    aes(
        x = exceso_mort,
        y = dif,
        label = entidad_abr_m
    )
) +
    geom_smooth(method = "lm", col = mcv_discrete[3]) +
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu")+
    facet_wrap(~dim, ncol = 2) +
    scale_x_continuous(labels = scales::percent)+
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        color = "",
        caption = nota_pie
    )   + 
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  
ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/03_modelos/01_exceso_mortalidad.png", 
       width = 23, height = 12, dpi = 100)


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
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    left_join(
        pob_lab %>% 
            select(cve_ent, tlp)
    ) %>% 
    glimpse

titulo <- "Pobreza laboral vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "% de la población en pobreza laboral"
eje_y <- "Puntaje 2020"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."

g <- 
ggplot(
    d,
    aes(
        x = tlp,
        y = dim_value,
        label = entidad_abr_m
    )
) +
    geom_smooth(method = "lm", col = mcv_discrete[3]) +
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu")+
    facet_wrap(~dim) +
    scale_x_continuous(labels = scales::percent)+
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        color = "",
        caption = nota_pie
    )   + 
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  
ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/03_modelos/02_pob_lab.png", 
       width = 23, height = 12, dpi = 100)

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
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    select(-contains("anio")) %>% 
    left_join(
        pib_p_cap %>% 
            filter(tipo_pib=="PIB per cápita (sin actividad petrolera)") %>% 
            select(cve_ent, value_pib)
    ) %>% 
    glimpse

titulo <- "PIB per cápita (sin petróleo) vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "PIB per cápita 2019 (pesos corrientes)"
eje_y <- "Puntaje 2020"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."

g <- 
ggplot(
    d,
    aes(
        x = value_pib,
        y = dim_value,
        label = entidad_abr_m
    )
) +
    geom_smooth(method = "lm", col = mcv_discrete[3], formula = y ~log(x)) +
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu")+
    facet_wrap(~dim) +
    scale_x_continuous(labels = scales::comma)+
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        color = "",
        caption = nota_pie
    )   + 
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/03_modelos/03_pib_p_cap.png", 
       width = 23, height = 12, dpi = 100)

stargazer::stargazer(
    lm(formula = dim_value~log(value_pib), data = d %>% filter(id_dim=="00")), 
    lm(formula = dim_value~log(value_pib), data = d %>% filter(id_dim=="01")), 
    lm(formula = dim_value~log(value_pib), data = d %>% filter(id_dim=="02")),
    lm(formula = dim_value~log(value_pib), data = d %>% filter(id_dim=="03")), type = "text"
)


stargazer::stargazer(lm(formula = dim_value~exp(value_pib), data = d %>% filter(id_dim=="01")), type = "text")


