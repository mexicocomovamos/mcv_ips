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

# 1. Acceso a atención médica (censo 2020) ----
d_censo_at_med <- tribble(
    ~cve_ent, ~tot_pob, ~tot_no_afiliada, ~tot_no_esp,
    "01",   1425607,   262088, 2380,
    "02",   3769020,   836317, 27438,
    "03",   798447,   129270, 5055,
    "04",   928363,   203304, 5382,
    "05",   3146771,   597373, 8690,
    "06",   731391,   123074, 2370,
    "07",   5543828,  1814782, 30383,
    "08",   3741869,   574108, 11467,
    "09",   9209944,  2502789, 18143,
    "10",   1832650,   461394, 4591,
    "11",   6166934,  1275190, 17083,
    "12",   3540685,   891961, 16720,
    "13",   3082841,   928550, 4918,
    "14",   8348151,  2452519, 59922,
    "15",   16992418,  5672574, 52167,
    "16",   4748846,  1784629, 9661,
    "17",   1971520,   552170, 2466,
    "18",   1235456,   273039, 1854,
    "19",   5784442,  1072664, 30313,
    "20",   4132148,  1215990, 11455,
    "21",   6583278,  1921945, 10351,
    "22",   2368467,   486467, 8840,
    "23",   1857985,   468580, 24077,
    "24",   2822255,   490284, 4519,
    "25",   3026943,   575115, 4313,
    "26",   2944840,   547727, 5634,
    "27",   2402598,   755484, 2025,
    "28",   3527735,   713859, 10469,
    "29",   1342977,   356641, 21737,
    "30",   8062579,  2228480, 8566,
    "31",   2320898,   505108, 5669,
    "32",   1622138,   326238, 2841
) %>% 
    mutate(
        tot_afiliada = tot_pob-tot_no_afiliada-tot_no_esp,
        prop_afiliada = tot_afiliada/tot_pob
    ) %>% 
    glimpse()

d <- d_ips_2021 %>% 
    filter(!cve_ent=="00") %>% 
    select(-contains("anio")) %>% 
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    left_join(
        d_censo_at_med%>% 
            select(cve_ent, prop_afiliada)
    ) %>% 
    glimpse


titulo <- "Acceso a atención médica vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "% de la población afiliada a al menos una institución de salud"
eje_y <- "Puntaje 2022"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."

g <- 
ggplot(
    d,
    aes(
        x = prop_afiliada,
        y = dim_value,
        label = entidad_abr_m
    )
) +
    geom_smooth(method = "lm", col = mcv_discrete[3], formula = y ~ exp(x)) +
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
g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/11_modelos/01_at_med.png", 
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/01_at_med.svg", 
       width = 23, height = 12, dpi = 200)


stargazer::stargazer(
    lm(formula = dim_value~exp(prop_afiliada), data = d %>% filter(id_dim=="00")), 
    lm(formula = dim_value~exp(prop_afiliada), data = d %>% filter(id_dim=="01")), 
    lm(formula = dim_value~exp(prop_afiliada), data = d %>% filter(id_dim=="02")),
    lm(formula = dim_value~exp(prop_afiliada), data = d %>% filter(id_dim=="03")), type = "text"
)


# 2. Pobreza laboral ----
d_pob_lab <- readxl::read_excel("02_datos_crudos/02_04_pobreza_laboral_complete_long.xlsx") %>% 
    filter(periodo == "2021T4", sexo == "General") %>% 
    glimpse

d <- d_ips_2021 %>% 
    filter(!cve_ent=="00") %>% 
    select(-contains("anio")) %>% 
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    left_join(
        d_pob_lab %>% 
            select(cve_ent, tlp)
    ) %>% 
    glimpse

titulo <- "Pobreza laboral vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "% de la población en pobreza laboral"
eje_y <- "Puntaje 2022"
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
g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/11_modelos/02_pob_lab.png", 
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/02_pob_lab.svg", 
       width = 23, height = 12, dpi = 200)

stargazer::stargazer(
    lm(formula = dim_value~tlp, data = d %>% filter(id_dim=="00")), 
    lm(formula = dim_value~tlp, data = d %>% filter(id_dim=="01")), 
    lm(formula = dim_value~tlp, data = d %>% filter(id_dim=="02")),
    lm(formula = dim_value~tlp, data = d %>% filter(id_dim=="03")), type = "text"
)

# 3. PIB per cápita ----
d_pib_p_cap <- readxl::read_excel("02_datos_crudos/03_pib_per_capita.xlsx") %>% 
    glimpse

d <- d_ips_2021 %>% 
    filter(!cve_ent=="00") %>% 
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    select(-contains("anio")) %>% 
    left_join(
        d_pib_p_cap %>% 
            filter(tipo=="Sin actividad petrolera", anio == 2020) %>% 
            select(cve_ent, value_pib = value_pib_pc)
    ) %>% 
    glimpse

titulo <- "PIB per cápita (sin petróleo) vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "PIB per cápita 2020 (pesos corrientes)"
eje_y <- "Puntaje 2022"
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

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/11_modelos/03_pib_p_cap.png", 
       width = 23, height = 12, dpi = 200)

ggsave(filename = "05_infobites/11_modelos/03_pib_p_cap.svg", 
       width = 23, height = 12, dpi = 200)


stargazer::stargazer(
    lm(formula = dim_value~log(value_pib), data = d %>% filter(id_dim=="00")), 
    lm(formula = dim_value~log(value_pib), data = d %>% filter(id_dim=="01")), 
    lm(formula = dim_value~log(value_pib), data = d %>% filter(id_dim=="02")),
    lm(formula = dim_value~log(value_pib), data = d %>% filter(id_dim=="03")), type = "text"
)



# 4. Ingresos laborales promedio ----
d_ingresos <- readRDS("02_datos_crudos/2021T4_ingresos_imputados.rds") %>% 
    mutate(cve_ent = str_pad(ent, 2, "l", "0")) %>% 
    select(cve_ent, ingreso_sm, factor) %>% 
    glimpse


d <- d_ips_2021 %>% 
    filter(!cve_ent=="00") %>% 
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    select(-contains("anio")) %>% 
    left_join(
        d_ingresos %>% 
            as_survey_design(weights = factor) %>% 
            group_by(cve_ent) %>% 
            summarise(ingreso_mean = survey_mean(ingreso_sm, na.rm = T)) %>% 
            ungroup() %>% 
            select(-ingreso_mean_se)
    ) %>% 
    glimpse

titulo <- "Ingresos laborales promedio vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "Ingresos laborales promedio (MXN)"
eje_y <- "Puntaje 2022"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."

g <- 
    ggplot(
        d,
        aes(
            x = ingreso_mean,
            y = dim_value,
            label = entidad_abr_m
        )
    ) +
    geom_smooth(method = "lm", col = mcv_discrete[3], formula = y ~log(x)) +
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu")+
    facet_wrap(~dim) +
    scale_x_continuous(labels = scales::dollar)+
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

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/11_modelos/04_ingresos_laborales_promedio.png", 
       width = 23, height = 12, dpi = 200)

ggsave(filename = "05_infobites/11_modelos/04_ingresos_laborales_promedio.svg", 
       width = 23, height = 12, dpi = 200)


stargazer::stargazer(
    lm(formula = dim_value~log(ingreso_mean), data = d %>% filter(id_dim=="00")), 
    lm(formula = dim_value~log(ingreso_mean), data = d %>% filter(id_dim=="01")), 
    lm(formula = dim_value~log(ingreso_mean), data = d %>% filter(id_dim=="02")),
    lm(formula = dim_value~log(ingreso_mean), data = d %>% filter(id_dim=="03")), type = "text"
)

# 5. Flujo comercial ----
d_flujo_com_prop_pibe <- 
    tribble(
        ~cve_ent,	~entidad_abr_m,	~flujo_comercial,	
        "01",	"AGS",	    23658098128,	
        "02",	"BC",	    226222782468,	
        "03",	"BCS",	    740261794,	    
        "04",	"CAMP",	    430075981,	
        "05",	"COAH",	    72006411437,	
        "06",	"COL",	    1010647543,	    
        "07",	"CHPS",	    627218877,	    
        "08",	"CHIH",	    309473584112,	
        "09",	"CDMX",	    564129920634,	    
        "10",	"DGO",	    7640667946,	    
        "11",	"GTO",	    63156605435,	
        "12",	"GRO",	    27419197,	    
        "13",	"HGO",	    2725360755,	    
        "14",	"JAL",	    149430065051,	
        "15",	"MEX",	    83207706154,	
        "16",	"MICH",	    13692090343,	
        "17",	"MOR",	    3804093637,	    
        "18",	"NAY",	    395268550,	    
        "19",	"NL",	    223455247881,	
        "20",	"OAX",	    436874434,	    
        "21",	"PUE",	    63274297537,	
        "22",	"QRO",	    61066881427,	
        "23",	"QROO",	    684326762,	    
        "24",	"SLP",	    37339284960,	
        "25",	"SIN",	    13380846273,	    
        "26",	"SON",	    56502540961,	
        "27",	"TAB",	    893256185,	
        "28",	"TAM",	    131239838962,	
        "29",	"TLAX",	    3356027181,	    
        "30",	"VER",	    8853272358,	
        "31",	"YUC",	    5550490571,	    
        "32",	"ZAC",	    3412495085	
    ) %>% 
    left_join(
        d_pib_p_cap %>% 
            filter(anio == 2020, !tipo == "Serie desestacionalizada") %>% 
            select(cve_ent, tipo, value_pib)
    ) %>% 
    mutate(flujo_comercial_prop = flujo_comercial/(value_pib*1000000)) %>% 
    glimpse


d <- d_ips_2021 %>% 
    filter(!cve_ent=="00") %>% 
    select(-contains("anio")) %>% 
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    left_join(
        d_flujo_com_prop_pibe %>% 
            select(cve_ent, flujo_comercial_prop)
    ) %>% 
    glimpse


titulo <- "Flujo comercial como % del PIBE vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "Flujo comercial como % del PIBE"
eje_y <- "Puntaje 2022"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."

g <- 
    ggplot(
        d,
        aes(
            x = flujo_comercial_prop,
            y = dim_value,
            label = entidad_abr_m
        )
    ) +
    geom_smooth(method = "lm", col = mcv_discrete[3], formula = y ~ log(x)) +
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

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/11_modelos/05_flujo_comercial.png", 
       width = 23, height = 12, dpi = 200)

ggsave(filename = "05_infobites/11_modelos/05_flujo_comercial.svg", 
       width = 23, height = 12, dpi = 200)


stargazer::stargazer(
    lm(formula = dim_value~log(flujo_comercial_prop), data = d %>% filter(id_dim=="00")), 
    lm(formula = dim_value~log(flujo_comercial_prop), data = d %>% filter(id_dim=="01")), 
    lm(formula = dim_value~log(flujo_comercial_prop), data = d %>% filter(id_dim=="02")),
    lm(formula = dim_value~log(flujo_comercial_prop), data = d %>% filter(id_dim=="03")), type = "text"
)

# 6. Escolaridad ----
load("~/MCV/ips_private/02_datos_crudos/df_enoe_reciente.RData")
d_escolaridad <- df_enoe_reciente %>% 
    select(cve_ent = ent, anios_esc, fac) %>% 
    mutate(cve_ent = str_pad(cve_ent, 2, "l", "0")) %>% 
    as_survey_design(weights = fac) %>% 
    group_by(cve_ent) %>% 
    summarise(escolaridad_promedio = survey_mean(anios_esc, na.rm = T, vartype = "ci")) %>% 
    glimpse


d <- d_ips_2021 %>% 
    filter(!cve_ent=="00") %>% 
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    select(-contains("anio")) %>% 
    left_join(
        d_escolaridad %>% 
            select(cve_ent, escolaridad_promedio)
    ) %>% 
    glimpse

titulo <- "Años de escolaridad promedio vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "Años de escolaridad promedio"
eje_y <- "Puntaje 2022"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."

g <- 
    ggplot(
        d,
        aes(
            x = escolaridad_promedio,
            y = dim_value,
            label = entidad_abr_m
        )
    ) +
    geom_smooth(method = "lm", col = mcv_discrete[3]) +
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

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/11_modelos/06_escolaridad.png", 
       width = 23, height = 12, dpi = 200)

ggsave(filename = "05_infobites/11_modelos/06_escolaridad.svg", 
       width = 23, height = 12, dpi = 200)


stargazer::stargazer(
    lm(formula = dim_value~escolaridad_promedio, data = d %>% filter(id_dim=="00")), 
    lm(formula = dim_value~escolaridad_promedio, data = d %>% filter(id_dim=="01")), 
    lm(formula = dim_value~escolaridad_promedio, data = d %>% filter(id_dim=="02")),
    lm(formula = dim_value~escolaridad_promedio, data = d %>% filter(id_dim=="03")), type = "text"
)

# 99. Estimación de coeficientes ----
data_complete <- d_ips_2021 %>% 
    filter(!cve_ent=="00") %>% 
    mutate(dim = ifelse(id_dim == "00", "0. Índice de Progreso Social", 
                        ifelse(id_dim == "01", "1. Necesidades Humanas Básicas", 
                               ifelse(id_dim == "02", "2. Fundamentos del Bienestar", "3. Oportunidades")))) %>% 
    select(-contains("anio")) %>% 
    left_join(
        d_censo_at_med %>% 
            select(cve_ent, prop_afiliada)
    ) %>% 
    left_join(
        d_pob_lab %>% 
            select(cve_ent, tlp)
    ) %>% 
    left_join(
        d_pib_p_cap %>% 
            filter(tipo=="Sin actividad petrolera", anio == 2020) %>% 
            select(cve_ent, value_pib_pc)
    ) %>% 
    left_join(
        d_ingresos %>% 
            as_survey_design(weights = factor) %>% 
            group_by(cve_ent) %>% 
            summarise(ingreso_mean = survey_mean(ingreso_sm, na.rm = T)) %>% 
            ungroup() %>% 
            select(cve_ent, ingreso_mean)
    ) %>% 
    left_join(
        d_escolaridad %>% 
            select(cve_ent, escolaridad_promedio)
    ) %>% 
    glimpse

stargazer::stargazer(
    lm(formula = dim_value~exp(prop_afiliada), data = data_complete %>% filter(id_dim=="00")), 
    lm(formula = dim_value~exp(prop_afiliada), data = data_complete %>% filter(id_dim=="01")), 
    lm(formula = dim_value~exp(prop_afiliada), data = data_complete %>% filter(id_dim=="02")),
    lm(formula = dim_value~exp(prop_afiliada), data = data_complete %>% filter(id_dim=="03")), type = "text"
)

stargazer::stargazer(
    lm(formula = dim_value~tlp, data = data_complete %>% filter(id_dim=="00")), 
    lm(formula = dim_value~tlp, data = data_complete %>% filter(id_dim=="01")), 
    lm(formula = dim_value~tlp, data = data_complete %>% filter(id_dim=="02")),
    lm(formula = dim_value~tlp, data = data_complete %>% filter(id_dim=="03")), type = "text"
)

stargazer::stargazer(
    lm(formula = dim_value~log(value_pib_pc), data = data_complete %>% filter(id_dim=="00")), 
    lm(formula = dim_value~log(value_pib_pc), data = data_complete %>% filter(id_dim=="01")), 
    lm(formula = dim_value~log(value_pib_pc), data = data_complete %>% filter(id_dim=="02")),
    lm(formula = dim_value~log(value_pib_pc), data = data_complete %>% filter(id_dim=="03")), type = "text"
)

stargazer::stargazer(
    lm(formula = dim_value~log(ingreso_mean), data = data_complete %>% filter(id_dim=="00")), 
    lm(formula = dim_value~log(ingreso_mean), data = data_complete %>% filter(id_dim=="01")), 
    lm(formula = dim_value~log(ingreso_mean), data = data_complete %>% filter(id_dim=="02")),
    lm(formula = dim_value~log(ingreso_mean), data = data_complete %>% filter(id_dim=="03")), type = "text"
)

stargazer::stargazer(
    lm(formula = dim_value~escolaridad_promedio, data = data_complete %>% filter(id_dim=="00")), 
    lm(formula = dim_value~escolaridad_promedio, data = data_complete %>% filter(id_dim=="01")), 
    lm(formula = dim_value~escolaridad_promedio, data = data_complete %>% filter(id_dim=="02")),
    lm(formula = dim_value~escolaridad_promedio, data = data_complete %>% filter(id_dim=="03")), type = "text"
)

# Mods Juve 13-03-2023 ----
d_ips_2021 <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx") %>% 
    filter(anio == 2021) %>% 
    glimpse

## 1. IMAIEF ----
# Imaief hasta noviembre del 2022. Actualizar si se usa más adelante. 
d_imaief <- readRDS("02_datos_crudos/tabla_imaief_2.rds") %>% 
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
eje_y <- "Puntaje 2022"
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

## 2. MINERIA ----
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
eje_y <- "Puntaje 2022"
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

## 3. Manufactura ----
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
eje_y <- "Puntaje 2022"
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

## 4. Construccion ----
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
eje_y <- "Puntaje 2022"
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
