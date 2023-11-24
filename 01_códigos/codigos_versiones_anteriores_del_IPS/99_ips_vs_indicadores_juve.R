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
d_ips_2021 <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx") %>% 
    filter(anio == 2021) %>% 
    glimpse

# 1. IMAIEF ----
d_imaief <- readRDS("/Users/juvenalcampos/Downloads/tabulados_aief/tabla_imaief_2.rds") %>% 
    filter(indice_vol_fisico == "Actividades secundarias") %>%
    filter(cve_ent != "00") %>% 
    filter(anio == 2021)

d <- d_ips_2021 %>% 
    filter(!cve_ent=="00") %>% 
    select(-contains("anio")) %>% 
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    left_join(
        d_imaief %>% 
            select(cve_ent, imaief = value)
    ) %>% 
    glimpse

# Se va a llamar IMAIEF, aunque haga despues referencia a sus demás componentes.
stargazer::stargazer(
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="00")), 
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="01")), 
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="02")),
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="03")), type = "text")


titulo <- "Indicador Mensual de la Actividad Industrial\npor Entidad Federativa (IMAIEF)."
subtitulo <- "IMAIEF 2021. Intervalos de confianza al 95%"
eje_x <- "\nIndicador. Base 2013 (2013 = 100)"
eje_y <- "Puntaje 2021"
nota_pie <- "\n*Para más información del modelo consultar Anexo estadístico."

g <- 
    ggplot(
        d,
        aes(x = imaief,
            y = dim_value,
            label = entidad_abr_m)
    ) +
    geom_smooth(method = "lm", col = mcv_discrete[3]
                # , formula = y ~ exp(x)
                ) +
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu")+
    facet_wrap(~dim, ncol = 2) +
    # scale_x_continuous(labels = scales::percent)+
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

g
g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/11_modelos/01_imaief_gral.png", 
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/01_imaief_gral.svg", 
       width = 23, height = 12, dpi = 200)

# 2. MINERIA ----
# unique(d_imaief$indice_vol_fisico)
d_imaief <- readRDS("/Users/juvenalcampos/Downloads/tabulados_aief/tabla_imaief_2.rds") %>% 
    filter(indice_vol_fisico == "21 - Minería") %>% 
    filter(cve_ent != "00") %>% 
    filter(anio == 2021)

d <- d_ips_2021 %>% 
    filter(!cve_ent=="00") %>% 
    select(-contains("anio")) %>% 
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    left_join(
        d_imaief %>% 
            select(cve_ent, imaief = value)
    ) %>% 
    glimpse

# Se va a llamar IMAIEF, aunque haga despues referencia a sus demás componentes.
stargazer::stargazer(
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="00")), 
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="01")), 
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="02")),
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="03")), type = "text")

titulo <- "Nivel de Actividad 21 - Minería"
subtitulo <- "IMAIEF 2021. Intervalos de confianza al 95%"
eje_x <- "\nIndicador. Base 2013 (2013 = 100)"
eje_y <- "Puntaje 2021"
nota_pie <- "\n*Para más información del modelo consultar Anexo estadístico."

g <- 
    ggplot(
        d,
        aes(x = imaief,
            y = dim_value,
            label = entidad_abr_m)
    ) +
    geom_smooth(method = "lm", col = mcv_discrete[3]
                # , formula = y ~ exp(x)
    ) +
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu")+
    facet_wrap(~dim, ncol = 2) +
    # scale_x_continuous(labels = scales::percent)+
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
g
g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/11_modelos/01_imaief_mineria.png", 
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/01_imaief_mineria.svg", 
       width = 23, height = 12, dpi = 200)

# 3. Manufactura ----
# unique(d_imaief$indice_vol_fisico)
d_imaief <- readRDS("/Users/juvenalcampos/Downloads/tabulados_aief/tabla_imaief_2.rds") %>% 
    filter(indice_vol_fisico == "31-33 - Industrias manufactureras") %>% 
    filter(cve_ent != "00") %>% 
    filter(anio == 2021)

d <- d_ips_2021 %>% 
    filter(!cve_ent=="00") %>% 
    select(-contains("anio")) %>% 
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    left_join(
        d_imaief %>% 
            select(cve_ent, imaief = value)
    ) %>% 
    glimpse

# Se va a llamar IMAIEF, aunque haga despues referencia a sus demás componentes.
stargazer::stargazer(
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="00")), 
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="01")), 
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="02")),
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="03")), type = "text")

titulo <- "Nivel de Actividad 31-33 - Industrias manufactureras"
subtitulo <- "IMAIEF 2021. Intervalos de confianza al 95%"
eje_x <- "\nIndicador. Base 2013 (2013 = 100)"
eje_y <- "Puntaje 2021"
nota_pie <- "\n*Para más información del modelo consultar Anexo estadístico."

g <- 
    ggplot(
        d,
        aes(x = imaief,
            y = dim_value,
            label = entidad_abr_m)
    ) +
    geom_smooth(method = "lm", col = mcv_discrete[3]
                # , formula = y ~ exp(x)
    ) +
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu")+
    facet_wrap(~dim, ncol = 2) +
    # scale_x_continuous(labels = scales::percent)+
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
g
g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/11_modelos/01_imaief_manufactura.png", 
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/01_imaief_manufactura.svg", 
       width = 23, height = 12, dpi = 200)

# 4. Construccion ----
# unique(d_imaief$indice_vol_fisico)
d_imaief <- readRDS("/Users/juvenalcampos/Downloads/tabulados_aief/tabla_imaief_2.rds") %>% 
    filter(indice_vol_fisico == "23 - Construcción") %>% 
    filter(cve_ent != "00") %>% 
    filter(anio == 2021)

d <- d_ips_2021 %>% 
    filter(!cve_ent=="00") %>% 
    select(-contains("anio")) %>% 
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    left_join(
        d_imaief %>% 
            select(cve_ent, imaief = value)
    ) %>% 
    glimpse

# Se va a llamar IMAIEF, aunque haga despues referencia a sus demás componentes.
stargazer::stargazer(
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="00")), 
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="01")), 
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="02")),
    lm(formula = dim_value~exp(imaief), data = d %>% filter(id_dim=="03")), type = "text")

titulo <- "Nivel de Actividad 23 - Construcción"
subtitulo <- "IMAIEF 2021. Intervalos de confianza al 95%"
eje_x <- "\nIndicador. Base 2013 (2013 = 100)"
eje_y <- "Puntaje 2021"
nota_pie <- "\n*Para más información del modelo consultar Anexo estadístico."

g <- 
    ggplot(
        d,
        aes(x = imaief,
            y = dim_value,
            label = entidad_abr_m)
    ) +
    geom_smooth(method = "lm", col = mcv_discrete[3]
                # , formula = y ~ exp(x)
    ) +
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu")+
    facet_wrap(~dim, ncol = 2) +
    # scale_x_continuous(labels = scales::percent)+
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
g
g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/11_modelos/01_imaief_construccion.png", 
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/01_imaief_construccion.svg", 
       width = 23, height = 12, dpi = 200)
