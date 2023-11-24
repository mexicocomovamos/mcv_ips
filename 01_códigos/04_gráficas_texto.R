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


# 1. IPS global ----
ips_glob <- readxl::read_excel("02_datos_crudos/04_ips_global.xlsx")

d <- ips_glob %>% 
    mutate(value = round(value, 1),
           name_comp  = str_wrap(name_comp, 25))

d_wide <- pivot_wider(d, names_from = "tipo", values_from = "value") %>%
    ungroup()

titulo <- "1. Índice de Progreso Social Global 2021"
subtitulo <- "Desagregación por componente"
eje_y <- ""
eje_x <- "Puntaje"

ggplot(d_wide) +
    geom_dumbbell(
        aes(y = reorder(name_comp,-ord), x = México, xend = Mundo, group = name_comp),
        col = mcv_blacks[2],
        size=3, 
    ) + 
    geom_text(
        data = d_wide %>% 
            mutate(mx_first = ifelse(México>Mundo, 1, 0)) %>% 
            filter(mx_first == 1),
        aes(y = reorder(name_comp,-ord),x = México, label = México),
        hjust = -0.2, show.legend = F, size = 7, family = "Ubuntu", col = mcv_discrete[1]
    ) +
    geom_text(
        data = d_wide %>% 
            mutate(mx_first = ifelse(México>Mundo, 1, 0)) %>% 
            filter(mx_first == 0),
        aes(y = reorder(name_comp,-ord),x = México, label = México),
        hjust = 1.2, show.legend = F, size = 7, family = "Ubuntu", col = mcv_discrete[1]
    )  + 
    geom_text(
        data = d_wide %>% 
            mutate(mx_first = ifelse(México>Mundo, 1, 0)) %>% 
            filter(mx_first == 1),
        aes(y = reorder(name_comp,-ord),x = Mundo, label = Mundo),
        hjust = 1.3, show.legend = F, size = 7, family = "Ubuntu", col = mcv_discrete[3]
    ) +
    geom_text(
        data = d_wide %>% 
            mutate(mx_first = ifelse(México>Mundo, 1, 0)) %>% 
            filter(mx_first == 0),
        aes(y = reorder(name_comp,-ord),x = Mundo, label = Mundo),
        hjust = -0.3, show.legend = F, size = 7, family = "Ubuntu", col = mcv_discrete[3]
    ) +
    geom_point(
        data = d,
        aes(y = reorder(name_comp, ord),x = value, color = tipo), size = 7
    ) +
    scale_color_manual(name = "", values = c(mcv_discrete[1], mcv_discrete[3]), guide = guide_legend(reverse = TRUE)) +
    scale_x_continuous(limits = c(30,100)) +
    theme_minimal() +
    labs(
        title = titulo,
        subtitle = subtitulo,
        color="", 
        y = eje_y,
        x = eje_x
    ) +
    theme(plot.title = element_text(size = 32, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,30,0)),
          plot.caption = element_text(size = 20),
          strip.text.x = element_text(size = 15),
          panel.grid.minor  = element_blank(),
          plot.margin= margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 25),
          axis.text.x = element_text(size = 25, vjust = 0.5),
          axis.text.y = element_text(size = 25),
          legend.text = element_text(size = 25),
          legend.position = "top")

ggsave(filename = "05_infobites/05_gráficas_texto/01_ips_global.svg", 
       width = 23, height = 12, dpi = 100)

# 2. IPS 2015-2020 ----
ips_final <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx")
titulo <- "2. Índice de Progreso Social"
subtitulo <- "2015 - 2020"
eje_x <- ""
eje_y <- "Años"

ggplot(data = 
           ips_final %>% 
           filter(!as.numeric(cve_ent) > 33) %>% 
           filter(id_dim == "00") %>% 
           mutate(dim = case_when(
               id_dim == "00" ~ "0. Índice de Progreso Social",
               id_dim == "01" ~ "1. Necesidades Básicas Humanas",
               id_dim == "02" ~ "2. Salud y Bienestar",
               T ~ "3. Oportunidades"
           )),
       aes(y = reorder(anio, -anio), 
           x = reorder(entidad_abr_m, as.numeric(cve_ent)),
           fill = dim_value)) +
    geom_tile(col = "white", show.legend = F) +
    geom_text(aes(label = round(dim_value,1)), family = "Ubuntu", size = 4) +
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1],midpoint = 62)  +
    guides(label = "none") +
    scale_x_discrete(position = "top")+
    # Etiquetas
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        color = ""
    )   + 
    coord_fixed() +
    theme(legend.position="bottom") +
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "top")  

ggsave(filename = "05_infobites/05_gráficas_texto/02_ips_mx.svg", 
       width = 23, height = 12, dpi = 100)

# 3. IPS ranking ----
d <- ips_final %>% 
    filter(!as.numeric(cve_ent) > 33) %>% 
    filter(!cve_ent == "00") %>% 
    group_by(anio, id_dim) %>% 
    arrange(desc(dim_value), .by_group = T) %>% 
    mutate(ranking = 1:32,
           ranking = str_pad(ranking, 2, "l", "0")) %>% 
    ungroup()

titulo <- "3. Ranking del Índice de Progreso Social"
subtitulo <- "2015 - 2020"
eje_x <- "Años"
eje_y <- "Ranking"

ggplot(
    d %>% 
        filter(id_dim == "00"), 
    aes(
        y = 100, 
        x = as.character(anio),
        stratum = ranking, 
        alluvium = cve_ent, 
        fill = as.numeric(ranking), 
        label = paste0(as.numeric(ranking), ". ", entidad_abr_m)
    )
)  +
    scale_x_discrete(expand = c(.1, .1)) +
    geom_flow() +
    geom_stratum(alpha = .7, show.legend = F) +
    geom_text(stat = "stratum", size = 4, family = "Ubuntu") +
    scale_fill_gradient2("", high = mcv_semaforo[4], mid = mcv_semaforo[2], low = mcv_semaforo[1],midpoint = 16)  +
    # Etiquetas
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        color = ""
    )   + 
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_blank(),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

ggsave(filename = "05_infobites/05_gráficas_texto/03_ips_mx.svg", 
       width = 23, height = 12, dpi = 100)

# 4. IPS vs exceso de mortalidad ----
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


titulo <- "4. Exceso de mortalidad vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "Exceso de mortalidad"
eje_y <- "Diferencia de puntaje estimado entre 2019 y 2020"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico.\n**No se muestra la dimensión 3. Oportunidades debido a que el modelo se estima como no significativo."

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
ggsave(filename = "05_infobites/05_gráficas_texto/04_ips_exceso_mort.svg", 
       width = 23, height = 12, dpi = 100)

# 5. Pobreza laboral ----
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

titulo <- "5. Pobreza laboral vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "% de la población en pobreza laboral"
eje_y <- "Puntaje 2020"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."

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
ggsave(filename = "05_infobites/05_gráficas_texto/05_ips_pob_lab.svg", 
       width = 23, height = 12, dpi = 100)

# 6. PIB per cápita ----
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

titulo <- "6. PIB per cápita (sin petróleo) vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "PIB per cápita 2019 (pesos corrientes)"
eje_y <- "Puntaje 2020"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."

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

ggsave(filename = "05_infobites/05_gráficas_texto/06_ips_pib_p_cap.svg", 
       width = 23, height = 12, dpi = 100)

# 7. Necesidades Básicas Humanas heatmap ----
titulo <- "7. Dim 1. Necesidades Básicas Humanas"
subtitulo <- "2015 - 2020"
eje_x <- ""
eje_y <- "Años"

ggplot(data = 
           ips_final %>% 
           filter(!as.numeric(cve_ent) > 33) %>% 
           filter(id_dim == "01") %>% 
           mutate(dim = case_when(
               id_dim == "00" ~ "0. Índice de Progreso Social",
               id_dim == "01" ~ "1. Necesidades Básicas Humanas",
               id_dim == "02" ~ "2. Salud y Bienestar",
               T ~ "3. Oportunidades"
           )),
       aes(y = reorder(anio, -anio), 
           x = reorder(entidad_abr_m, as.numeric(cve_ent)),
           fill = dim_value)) +
    geom_tile(col = "white", show.legend = F) +
    geom_text(aes(label = round(dim_value,1)), family = "Ubuntu", size = 4) +
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1],midpoint = 62)  +
    guides(label = "none") +
    scale_x_discrete(position = "top")+
    # Etiquetas
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        color = ""
    )   + 
    coord_fixed() +
    theme(legend.position="bottom") +
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "top")  

ggsave(filename = "05_infobites/05_gráficas_texto/07_nhb_heatmap.svg", 
       width = 23, height = 12, dpi = 100)

# 8. NHB ranking ----
titulo <- "8. Ranking de la Dim 1. Necesidades Humanas Básicas"
subtitulo <- "2015 - 2020"
eje_x <- "Años"
eje_y <- "Ranking"
d <- ips_final %>% 
    filter(!as.numeric(cve_ent) > 33) %>% 
    filter(!cve_ent == "00") %>% 
    group_by(anio, id_dim) %>% 
    arrange(desc(dim_value), .by_group = T) %>% 
    mutate(ranking = 1:32,
           ranking = str_pad(ranking, 2, "l", "0")) %>% 
    ungroup()

ggplot(
    d %>% 
        filter(id_dim == "01"), 
    aes(
        y = 100, 
        x = as.character(anio),
        stratum = ranking, 
        alluvium = cve_ent, 
        fill = as.numeric(ranking), 
        label = paste0(as.numeric(ranking), ". ", entidad_abr_m)
    )
)  +
    scale_x_discrete(expand = c(.1, .1)) +
    geom_flow() +
    geom_stratum(alpha = .7, show.legend = F) +
    geom_text(stat = "stratum", size = 4, family = "Ubuntu") +
    scale_fill_gradient2("", high = mcv_semaforo[4], mid = mcv_semaforo[2], low = mcv_semaforo[1],midpoint = 16)  +
    # Etiquetas
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        color = ""
    )   + 
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_blank(),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  
ggsave(filename = "05_infobites/05_gráficas_texto/08_nhb_ranking.svg", 
       width = 23, height = 12, dpi = 100)

# 9-12. Heatmaps componentes NHB ----
ips_comp <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx", sheet = 2) %>% 
    left_join(
        readxl::read_excel("02_datos_crudos/00_diccionario_componentes.xlsx")
    ) %>%
    select(ends_with("comp"), everything()) %>% 
    filter(str_starts(id_comp, "01")) %>% 
    mutate(id_comp = case_when(
        id_comp == "01_01comp" ~ "1",
        id_comp == "01_02comp" ~ "2",
        id_comp == "01_03comp" ~ "3",
        id_comp == "01_04comp" ~ "4",
        T ~ NA_character_
    )) %>% 
    glimpse

id_comp_vec <- unique(ips_comp$id_comp)



for(i in 1:length(id_comp_vec)){
    
    a <- ips_comp %>% 
        filter(id_comp == id_comp_vec[i])
    
    titulo <- paste0(
        i+8, ". Comp 1.", unique(a$id_comp), ". ", unique(a$name_comp)
    )
    
    subtitulo <- "2015 - 2020"
    eje_x <- ""
    eje_y <- "Años"
    
    ggplot(data = 
               a %>% 
               filter(!as.numeric(cve_ent) > 33),
           aes(y = reorder(anio, -anio), 
               x = reorder(entidad_abr_m, as.numeric(cve_ent)),
               fill = comp_value)) +
        geom_tile(col = "white", show.legend = F) +
        geom_text(aes(label = round(comp_value,1)), family = "Ubuntu", size = 4) +
        scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1],midpoint = 50)  +
        guides(label = "none") +
        scale_x_discrete(position = "top")+
        # Etiquetas
        labs(
            title = titulo, 
            subtitle = subtitulo, 
            x = eje_x, 
            y = eje_y, 
            color = ""
        )   + 
        coord_fixed() +
        theme(legend.position="bottom") +
        theme_minimal() +
        theme(
            plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
            plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
            plot.caption       = element_text(size = 20),
            plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
            strip.text.x       = element_text(size = 25, colour = "#777777"),
            panel.grid.minor   = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background   = element_rect(fill = "transparent", colour = NA),
            text               = element_text(family = "Ubuntu"),
            axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
            axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
            legend.position    = "top")  
    
    ggsave(filename = paste0("05_infobites/05_gráficas_texto/",str_pad(i+8,2,"l","0"), ".svg"), 
           width = 23, height = 12, dpi = 100)
    
}

# 13. Fundamentos del Bienestar (heatmap) ----
titulo <- "13. Dim 2. Fundamentos del Bienestar"
subtitulo <- "2015 - 2020"
eje_x <- ""
eje_y <- "Años"

ggplot(data = 
           ips_final %>% 
           filter(!as.numeric(cve_ent) > 33) %>% 
           filter(id_dim == "02") %>% 
           mutate(dim = case_when(
               id_dim == "00" ~ "0. Índice de Progreso Social",
               id_dim == "01" ~ "1. Necesidades Básicas Humanas",
               id_dim == "02" ~ "2. Salud y Bienestar",
               T ~ "3. Oportunidades"
           )),
       aes(y = reorder(anio, -anio), 
           x = reorder(entidad_abr_m, as.numeric(cve_ent)),
           fill = dim_value)) +
    geom_tile(col = "white", show.legend = F) +
    geom_text(aes(label = round(dim_value,1)), family = "Ubuntu", size = 4) +
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1],midpoint = 62)  +
    guides(label = "none") +
    scale_x_discrete(position = "top")+
    # Etiquetas
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        color = ""
    )   + 
    coord_fixed() +
    theme(legend.position="bottom") +
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "top")  

ggsave(filename = "05_infobites/05_gráficas_texto/13_fb_heatmap.svg", 
       width = 23, height = 12, dpi = 100)

# 14. FB (ranking) ----
titulo <- "14. Ranking de la Dim 2. Fundamentos del Bienestar"
subtitulo <- "2015 - 2020"
eje_x <- "Años"
eje_y <- "Ranking"

ggplot(
    ips_final %>% 
        filter(!as.numeric(cve_ent) > 33) %>% 
        filter(!cve_ent == "00") %>% 
        group_by(anio, id_dim) %>% 
        arrange(desc(dim_value), .by_group = T) %>% 
        mutate(ranking = 1:32,
               ranking = str_pad(ranking, 2, "l", "0")) %>% 
        ungroup() %>% 
        filter(id_dim == "02"), 
    aes(
        y = 100, 
        x = as.character(anio),
        stratum = ranking, 
        alluvium = cve_ent, 
        fill = as.numeric(ranking), 
        label = paste0(as.numeric(ranking), ". ", entidad_abr_m)
    )
)  +
    scale_x_discrete(expand = c(.1, .1)) +
    geom_flow() +
    geom_stratum(alpha = .7, show.legend = F) +
    geom_text(stat = "stratum", size = 4, family = "Ubuntu") +
    scale_fill_gradient2("", high = mcv_semaforo[4], mid = mcv_semaforo[2], low = mcv_semaforo[1],midpoint = 16)  +
    # Etiquetas
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        color = ""
    )   + 
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_blank(),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  
ggsave(filename = "05_infobites/05_gráficas_texto/14_fb_ranking.svg", 
       width = 23, height = 12, dpi = 100)

# 15-18. Heatmaps componentes FB ----
ips_comp <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx", sheet = 2) %>% 
    left_join(
        readxl::read_excel("02_datos_crudos/00_diccionario_componentes.xlsx")
    ) %>%
    select(ends_with("comp"), everything()) %>% 
    filter(str_starts(id_comp, "02")) %>% 
    mutate(id_comp = case_when(
        id_comp == "02_05comp" ~ "1",
        id_comp == "02_06comp" ~ "2",
        id_comp == "02_07comp" ~ "3",
        id_comp == "02_08comp" ~ "4",
        T ~ NA_character_
    )) %>% 
    glimpse

id_comp_vec <- unique(ips_comp$id_comp)



for(i in 1:length(id_comp_vec)){
    
    a <- ips_comp %>% 
        filter(id_comp == id_comp_vec[i])
    
    titulo <- paste0(
        i+14, ". Comp 2.", unique(a$id_comp), ". ", unique(a$name_comp)
    )
    
    subtitulo <- "2015 - 2020"
    eje_x <- ""
    eje_y <- "Años"
    
    ggplot(data = 
               a %>% 
               filter(!as.numeric(cve_ent) > 33),
           aes(y = reorder(anio, -anio), 
               x = reorder(entidad_abr_m, as.numeric(cve_ent)),
               fill = comp_value)) +
        geom_tile(col = "white", show.legend = F) +
        geom_text(aes(label = round(comp_value,1)), family = "Ubuntu", size = 4) +
        scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1],midpoint = 50)  +
        guides(label = "none") +
        scale_x_discrete(position = "top")+
        # Etiquetas
        labs(
            title = titulo, 
            subtitle = subtitulo, 
            x = eje_x, 
            y = eje_y, 
            color = ""
        )   + 
        coord_fixed() +
        theme(legend.position="bottom") +
        theme_minimal() +
        theme(
            plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
            plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
            plot.caption       = element_text(size = 20),
            plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
            strip.text.x       = element_text(size = 25, colour = "#777777"),
            panel.grid.minor   = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background   = element_rect(fill = "transparent", colour = NA),
            text               = element_text(family = "Ubuntu"),
            axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
            axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
            legend.position    = "top")  
    
    ggsave(filename = paste0("05_infobites/05_gráficas_texto/",str_pad(i+14,2,"l","0"), ".svg"), 
           width = 23, height = 12, dpi = 100)
    
}

# 19. Oportunidades heatmap ----
titulo <- "19. Dim 3. Oportunidades"
subtitulo <- "2015 - 2020"
eje_x <- ""
eje_y <- "Años"

ggplot(data = 
           ips_final %>% 
           filter(!as.numeric(cve_ent) > 33) %>% 
           filter(id_dim == "03") %>% 
           mutate(dim = case_when(
               id_dim == "00" ~ "0. Índice de Progreso Social",
               id_dim == "01" ~ "1. Necesidades Básicas Humanas",
               id_dim == "02" ~ "2. Salud y Bienestar",
               T ~ "3. Oportunidades"
           )),
       aes(y = reorder(anio, -anio), 
           x = reorder(entidad_abr_m, as.numeric(cve_ent)),
           fill = dim_value)) +
    geom_tile(col = "white", show.legend = F) +
    geom_text(aes(label = round(dim_value,1)), family = "Ubuntu", size = 4) +
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1],midpoint = 62)  +
    guides(label = "none") +
    scale_x_discrete(position = "top")+
    # Etiquetas
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        color = ""
    )   + 
    coord_fixed() +
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "top")  

ggsave(filename = "05_infobites/05_gráficas_texto/19_op_heatmap.svg", 
       width = 23, height = 12, dpi = 100)

# 20. Op (ranking) ----
titulo <- "20. Ranking de la Dim 3. Oportunidades"
subtitulo <- "2015 - 2020"
eje_x <- "Años"
eje_y <- "Ranking"

ggplot(
    ips_final %>% 
        filter(!as.numeric(cve_ent) > 33) %>% 
        filter(!cve_ent == "00") %>% 
        group_by(anio, id_dim) %>% 
        arrange(desc(dim_value), .by_group = T) %>% 
        mutate(ranking = 1:32,
               ranking = str_pad(ranking, 2, "l", "0")) %>% 
        ungroup() %>% 
        filter(id_dim == "03"), 
    aes(
        y = 100, 
        x = as.character(anio),
        stratum = ranking, 
        alluvium = cve_ent, 
        fill = as.numeric(ranking), 
        label = paste0(as.numeric(ranking), ". ", entidad_abr_m)
    )
)  +
    scale_x_discrete(expand = c(.1, .1)) +
    geom_flow() +
    geom_stratum(alpha = .7, show.legend = F) +
    geom_text(stat = "stratum", size = 4, family = "Ubuntu") +
    scale_fill_gradient2("", high = mcv_semaforo[4], mid = mcv_semaforo[2], low = mcv_semaforo[1],midpoint = 16)  +
    # Etiquetas
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        color = ""
    )   + 
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_blank(),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  
ggsave(filename = "05_infobites/05_gráficas_texto/20_op_ranking.svg", 
       width = 23, height = 12, dpi = 100)

# 21-24. Heatmaps componentes Op ----
ips_comp <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx", sheet = 2) %>% 
    left_join(
        readxl::read_excel("02_datos_crudos/00_diccionario_componentes.xlsx")
    ) %>%
    select(ends_with("comp"), everything()) %>% 
    filter(str_starts(id_comp, "03")) %>% 
    mutate(id_comp = case_when(
        id_comp == "03_09comp" ~ "1",
        id_comp == "03_10comp" ~ "2",
        id_comp == "03_11comp" ~ "3",
        id_comp == "03_12comp" ~ "4",
        T ~ NA_character_
    )) %>% 
    glimpse

id_comp_vec <- unique(ips_comp$id_comp)



for(i in 1:length(id_comp_vec)){
    
    a <- ips_comp %>% 
        filter(id_comp == id_comp_vec[i])
    
    titulo <- paste0(
        i+20, ". Comp 3.", unique(a$id_comp), ". ", unique(a$name_comp)
    )
    
    subtitulo <- "2015 - 2020"
    eje_x <- ""
    eje_y <- "Años"
    
    ggplot(data = 
               a %>% 
               filter(!as.numeric(cve_ent) > 33),
           aes(y = reorder(anio, -anio), 
               x = reorder(entidad_abr_m, as.numeric(cve_ent)),
               fill = comp_value)) +
        geom_tile(col = "white", show.legend = F) +
        geom_text(aes(label = round(comp_value,1)), family = "Ubuntu", size = 4) +
        scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1],midpoint = 50)  +
        guides(label = "none") +
        scale_x_discrete(position = "top")+
        # Etiquetas
        labs(
            title = titulo, 
            subtitle = subtitulo, 
            x = eje_x, 
            y = eje_y, 
            color = ""
        )   + 
        coord_fixed() +
        theme(legend.position="bottom") +
        theme_minimal() +
        theme(
            plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
            plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
            plot.caption       = element_text(size = 20),
            plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
            strip.text.x       = element_text(size = 25, colour = "#777777"),
            panel.grid.minor   = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.background   = element_rect(fill = "transparent", colour = NA),
            text               = element_text(family = "Ubuntu"),
            axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
            axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
            legend.position    = "top")  
    
    ggsave(filename = paste0("05_infobites/05_gráficas_texto/",str_pad(i+20,2,"l","0"), ".svg"), 
           width = 23, height = 12, dpi = 100)
    
}