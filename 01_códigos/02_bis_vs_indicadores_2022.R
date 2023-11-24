# 0. Configuración inicial ----
options(scipen=999)
Sys.setlocale("LC_TIME", "es_ES")

# 0.1. Paquetes ----
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

# 0.2 Colores MCV -----
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

# 0.3. BD IPS ----

# 01. Pobreza extrema ----
d_ips_2022 <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx") %>% 
    filter(anio == 2022) 

dimensiones = tibble(id_dim = c("00", "01", "02", "03"), 
                     nombre_dimensión = c("0. Índice de Progreso Social", 
                                          "1. Necesidades Humanas Básicas", 
                                          "2. Fundamentos del Bienestar", 
                                          "3. Oportunidades"))

pobreza_extrema_multidimensional <- tibble::tribble(
    ~anio, ~cve_ent,              ~entidad, ~value,
    2022L,     "01",      "Aguascalientes",    1.8,
    2022L,     "02",     "Baja California",    1.3,
    2022L,     "03", "Baja California Sur",    0.8,
    2022L,     "04",            "Campeche",    9.6,
    2022L,     "05",            "Coahuila",    1.8,
    2022L,     "06",              "Colima",    1.2,
    2022L,     "07",             "Chiapas",   28.2,
    2022L,     "08",           "Chihuahua",    2.1,
    2022L,     "09",    "Ciudad de México",    1.7,
    2022L,     "10",             "Durango",    6.3,
    2022L,     "11",          "Guanajuato",    3.2,
    2022L,     "12",            "Guerrero",   22.2,
    2022L,     "13",             "Hidalgo",    6.8,
    2022L,     "14",             "Jalisco",    2.1,
    2022L,     "15",              "México",      6,
    2022L,     "16",           "Michoacán",    7.5,
    2022L,     "17",             "Morelos",    5.8,
    2022L,     "18",             "Nayarit",    6.4,
    2022L,     "19",          "Nuevo León",    1.1,
    2022L,     "20",              "Oaxaca",   20.2,
    2022L,     "21",              "Puebla",   11.4,
    2022L,     "22",           "Querétaro",    1.7,
    2022L,     "23",        "Quintana Roo",    4.2,
    2022L,     "24",     "San Luis Potosí",    7.4,
    2022L,     "25",             "Sinaloa",    1.8,
    2022L,     "26",              "Sonora",    1.7,
    2022L,     "27",             "Tabasco",   11.2,
    2022L,     "28",          "Tamaulipas",    2.9,
    2022L,     "29",            "Tlaxcala",    6.8,
    2022L,     "30",            "Veracruz",   13.1,
    2022L,     "31",             "Yucatán",    5.6,
    2022L,     "32",           "Zacatecas",    5.1,
    2022L,      "00",            "Nacional",    7.1
    ) %>% 
    filter(!is.na(cve_ent)) %>% 
    filter(cve_ent != "00")

d <- left_join(d_ips_2022, pobreza_extrema_multidimensional) %>% 
    left_join(dimensiones)

titulo <- "Porcentaje de la población en pobreza extrema\nvs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "% de la población viviendo en pobreza extrema"
eje_y <- "Puntaje IPS 2022"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."
unidad <- "%"

d <- filter(d, !is.na(value))
m1.0 = lm(data = d %>% filter(id_dim == "00"), formula = dim_value ~ value)
m1.1 = lm(data = d %>% filter(id_dim == "01"), formula = dim_value ~ value)
m1.2 = lm(data = d %>% filter(id_dim == "02"), formula = dim_value ~ value)
m1.3 = lm(data = d %>% filter(id_dim == "03"), formula = dim_value ~ value)
stargazer::stargazer(
    m1.0, 
    m1.1, 
    m1.2, 
    m1.3,
    type = "text"
)

g <- 
    ggplot(d, aes(x = value,
            y = dim_value,
            label = entidad_abr_m)) +
    geom_smooth(method = "lm", 
                col = mcv_discrete[3],
                formula = y ~ x) + #exp(x)
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu") +
    facet_wrap(~nombre_dimensión, ncol = 2, scales = "free") +
    scale_x_continuous(expand = expansion(c(0.1, 0.1)), 
                       labels = scales::comma_format(suffix = unidad)) +
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
        strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")

ggsave(filename = "05_infobites/11_modelos/cruces_2022/01_Pobreza_extrema.png",
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/cruces_2022/01_Pobreza_extrema.svg",
       width = 23, height = 12, dpi = 200)

# 02. Pobreza ----
pobreza <- tibble::tribble(
    ~anio, ~cve_ent,              ~entidad, ~value,
    2022L,     "01",      "Aguascalientes",   23.7,
    2022L,     "02",     "Baja California",   13.4,
    2022L,     "03", "Baja California Sur",   13.3,
    2022L,     "04",            "Campeche",   45.1,
    2022L,     "05",            "Coahuila",   18.2,
    2022L,     "06",              "Colima",   20.5,
    2022L,     "07",             "Chiapas",   67.4,
    2022L,     "08",           "Chihuahua",   17.6,
    2022L,     "09",    "Ciudad de México",     24,
    2022L,     "10",             "Durango",   34.3,
    2022L,     "11",          "Guanajuato",     33,
    2022L,     "12",            "Guerrero",   60.4,
    2022L,     "13",             "Hidalgo",     41,
    2022L,     "14",             "Jalisco",   21.8,
    2022L,     "15",              "México",   42.9,
    2022L,     "16",           "Michoacán",   41.7,
    2022L,     "17",             "Morelos",   41.1,
    2022L,     "18",             "Nayarit",   29.3,
    2022L,     "19",          "Nuevo León",     16,
    2022L,     "20",              "Oaxaca",   58.4,
    2022L,     "21",              "Puebla",     54,
    2022L,     "22",           "Querétaro",   21.7,
    2022L,     "23",        "Quintana Roo",     27,
    2022L,     "24",     "San Luis Potosí",   35.5,
    2022L,     "25",             "Sinaloa",   21.6,
    2022L,     "26",              "Sonora",   21.7,
    2022L,     "27",             "Tabasco",   46.5,
    2022L,     "28",          "Tamaulipas",   26.8,
    2022L,     "29",            "Tlaxcala",   52.5,
    2022L,     "30",            "Veracruz",   51.7,
    2022L,     "31",             "Yucatán",   38.8,
    2022L,     "32",           "Zacatecas",   44.2,
    2022L,      "00",            "Nacional",   36.3
    ) %>% 
    filter(cve_ent != "00")

d <- left_join(d_ips_2022, pobreza) %>% 
    left_join(dimensiones)

titulo <- "Pobreza multidimensional vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "% de la población viviendo en pobreza"
eje_y <- "Puntaje 2022"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."
unidad <- "%"

modelo_02 <- lm(data = d, 
                formula = dim_value~value)
d <- filter(d, !is.na(value))
m2.0 = lm(data = d %>% filter(id_dim == "00"), formula = dim_value ~ value)
m2.1 = lm(data = d %>% filter(id_dim == "01"), formula = dim_value ~ value)
m2.2 = lm(data = d %>% filter(id_dim == "02"), formula = dim_value ~ value)
m2.3 = lm(data = d %>% filter(id_dim == "03"), formula = dim_value ~ value)
stargazer::stargazer(
    m2.0, 
    m2.1, 
    m2.2, 
    m2.3,
    type = "text"
)


g <- 
    ggplot(d, aes(x = value,
                  y = dim_value,
                  label = entidad_abr_m)) +
    geom_smooth(method = "lm", 
                col = mcv_discrete[3],
                formula = y ~ x) + #exp(x)
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu") +
    facet_wrap(~nombre_dimensión, ncol = 2, scales = "free") +
    scale_x_continuous(expand = expansion(c(0.1, 0.1)), 
                       labels = scales::comma_format(suffix = unidad)) +
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
        strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")

ggsave(filename = "05_infobites/11_modelos/cruces_2022/02_Pobreza.png",
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/cruces_2022/02_Pobreza.svg",
       width = 23, height = 12, dpi = 200)

# 03. Rezago educativo ----

rezago_educativo <- tibble::tribble(
    ~anio, ~cve_ent,              ~entidad,        ~value,
    2022L,     "01",      "Aguascalientes", 16.4997553615,
    2022L,     "02",     "Baja California", 15.5469917573,
    2022L,     "03", "Baja California Sur", 14.1839268163,
    2022L,     "04",            "Campeche", 20.3119739766,
    2022L,     "05",            "Coahuila", 13.4530474176,
    2022L,     "06",              "Colima", 15.6372697168,
    2022L,     "07",             "Chiapas",  31.106865473,
    2022L,     "08",           "Chihuahua", 16.0886351221,
    2022L,     "09",    "Ciudad de México",  9.6995338497,
    2022L,     "10",             "Durango", 17.8679746642,
    2022L,     "11",          "Guanajuato", 22.8444151898,
    2022L,     "12",            "Guerrero", 28.8151850926,
    2022L,     "13",             "Hidalgo", 19.9960407831,
    2022L,     "14",             "Jalisco", 20.4421801476,
    2022L,     "15",              "México", 16.3426017835,
    2022L,     "16",           "Michoacán", 28.9364163464,
    2022L,     "17",             "Morelos", 17.8979810057,
    2022L,     "18",             "Nayarit", 20.3331727325,
    2022L,     "19",          "Nuevo León", 13.5154573843,
    2022L,     "20",              "Oaxaca", 29.1302411198,
    2022L,     "21",              "Puebla", 21.8982822447,
    2022L,     "22",           "Querétaro", 17.6099503107,
    2022L,     "23",        "Quintana Roo", 16.5228559192,
    2022L,     "24",     "San Luis Potosí", 20.5576706963,
    2022L,     "25",             "Sinaloa", 17.0028300484,
    2022L,     "26",              "Sonora", 14.5653386273,
    2022L,     "27",             "Tabasco", 17.9337549636,
    2022L,     "28",          "Tamaulipas", 16.0955717679,
    2022L,     "29",            "Tlaxcala", 16.1423820524,
    2022L,     "30",            "Veracruz", 25.4997179778,
    2022L,     "31",             "Yucatán", 21.2200937684,
    2022L,     "32",           "Zacatecas", 20.7248875836,
    2022L,      "00",            "Nacional", 19.4407930087
    )  %>% 
    filter(cve_ent != "00")

d <- left_join(d_ips_2022, rezago_educativo) %>% 
    left_join(dimensiones)

titulo <- "Rezago educativo vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "% de la población en rezago educativo"
eje_y <- "Puntaje 2022"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."
unidad <- "%"

modelo_03 <- lm(data = d, 
                formula = dim_value~value)
d <- filter(d, !is.na(value))
m3.0 = lm(data = d %>% filter(id_dim == "00"), formula = dim_value ~ value)
m3.1 = lm(data = d %>% filter(id_dim == "01"), formula = dim_value ~ value)
m3.2 = lm(data = d %>% filter(id_dim == "02"), formula = dim_value ~ value)
m3.3 = lm(data = d %>% filter(id_dim == "03"), formula = dim_value ~ value)
stargazer::stargazer(
    m3.0, 
    m3.1, 
    m3.2, 
    m3.3,
    type = "text"
)

g <- 
    ggplot(d, aes(x = value,
                  y = dim_value,
                  label = entidad_abr_m)) +
    geom_smooth(method = "lm", 
                col = mcv_discrete[3],
                formula = y ~ x) + #exp(x)
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu") +
    facet_wrap(~nombre_dimensión, ncol = 2, scales = "free") +
    scale_x_continuous(expand = expansion(c(0.1, 0.1)), 
                       labels = scales::comma_format(suffix = unidad)) +
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
        strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")

ggsave(filename = "05_infobites/11_modelos/cruces_2022/03_Rezago_educativo.png",
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/cruces_2022/03_Rezago_educativo.svg",
       width = 23, height = 12, dpi = 200)


# 04. Carencia Salud ----
carencia_salud <- tibble::tribble(
    ~anio, ~cve_ent,              ~entidad,        ~value,
    2022L,     "01",      "Aguascalientes", 26.1939774447,
    2022L,     "02",     "Baja California", 28.4100970374,
    2022L,     "03", "Baja California Sur", 17.3366896881,
    2022L,     "04",            "Campeche", 34.5696286695,
    2022L,     "05",            "Coahuila",  19.677330459,
    2022L,     "06",              "Colima", 21.9358378526,
    2022L,     "07",             "Chiapas", 66.0662029381,
    2022L,     "08",           "Chihuahua", 21.4914432491,
    2022L,     "09",    "Ciudad de México", 28.7459230788,
    2022L,     "10",             "Durango", 30.1583886583,
    2022L,     "11",          "Guanajuato", 33.1530952596,
    2022L,     "12",            "Guerrero", 52.7394540254,
    2022L,     "13",             "Hidalgo", 50.4085911839,
    2022L,     "14",             "Jalisco", 37.1306730394,
    2022L,     "15",              "México", 44.1543658863,
    2022L,     "16",           "Michoacán", 51.2139308133,
    2022L,     "17",             "Morelos", 46.4475164485,
    2022L,     "18",             "Nayarit", 34.0885533881,
    2022L,     "19",          "Nuevo León", 22.8338969822,
    2022L,     "20",              "Oaxaca", 65.7087413548,
    2022L,     "21",              "Puebla", 48.2831532016,
    2022L,     "22",           "Querétaro", 30.4314691788,
    2022L,     "23",        "Quintana Roo", 32.3706942196,
    2022L,     "24",     "San Luis Potosí", 34.5504405357,
    2022L,     "25",             "Sinaloa", 26.5751232297,
    2022L,     "26",              "Sonora", 24.4289057351,
    2022L,     "27",             "Tabasco", 44.8000340667,
    2022L,     "28",          "Tamaulipas", 27.9477171391,
    2022L,     "29",            "Tlaxcala", 45.2308754194,
    2022L,     "30",            "Veracruz", 49.2190642128,
    2022L,     "31",             "Yucatán", 35.1272683145,
    2022L,     "32",           "Zacatecas", 36.7209758594,
    2022L,      "00",            "Nacional", 39.0912088798
    ) %>% 
    filter(cve_ent != "00")

d <- left_join(d_ips_2022, carencia_salud) %>% 
    left_join(dimensiones)


titulo <- "Carencia por acceso a servicios de salud vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "% de la población con carencia por acceso a servicios de salud"
eje_y <- "Puntaje IPS 2022"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."
unidad <- "%"

modelo_04 <- lm(data = d, 
                formula = dim_value~value)
d <- filter(d, !is.na(value))
m4.0 = lm(data = d %>% filter(id_dim == "00"), formula = dim_value ~ value)
m4.1 = lm(data = d %>% filter(id_dim == "01"), formula = dim_value ~ value)
m4.2 = lm(data = d %>% filter(id_dim == "02"), formula = dim_value ~ value)
m4.3 = lm(data = d %>% filter(id_dim == "03"), formula = dim_value ~ value)
stargazer::stargazer(
    m4.0, 
    m4.1, 
    m4.2, 
    m4.3,
    type = "text"
)

g <- 
    ggplot(d, aes(x = value,
                  y = dim_value,
                  label = entidad_abr_m)) +
    geom_smooth(method = "lm", 
                col = mcv_discrete[3],
                formula = y ~ x) + #exp(x)
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu") +
    facet_wrap(~nombre_dimensión, ncol = 2, scales = "free") +
    scale_x_continuous(expand = expansion(c(0.1, 0.1)), 
                       labels = scales::comma_format(suffix = unidad)) +
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
        strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")

ggsave(filename = "05_infobites/11_modelos/cruces_2022/04_Rezago_salud.png",
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/cruces_2022/04_Rezago_salud.svg",
       width = 23, height = 12, dpi = 200)

# 05. PIB Per cápita ----
pib_pc <- tibble::tribble(
              ~cve_ent, ~entidad_abr_m, ~anio, ~id_dimension, ~id_indicador, ~pibe_no_petrolero, ~value,
                  "09",         "CDMX", 2021L,            NA,            NA,        3480641.634,       375366.482,
                  "19",           "NL", 2021L,            NA,            NA,        1878945.949,      313319.2264,
                  "05",         "COAH", 2021L,            NA,            NA,         916488.292,      283492.3649,
                  "26",          "SON", 2021L,            NA,            NA,         768402.411,      254088.9748,
                  "22",          "QRO", 2021L,            NA,            NA,         575435.506,      233661.9339,
                  "02",           "BC", 2021L,            NA,            NA,         902183.918,       231928.358,
                  "08",         "CHIH", 2021L,            NA,            NA,         849904.013,      220645.9147,
                  "01",          "AGS", 2021L,            NA,            NA,         313012.725,      212551.3786,
                  "03",          "BCS", 2021L,            NA,            NA,         170817.905,      205351.4472,
                  "14",          "JAL", 2021L,            NA,            NA,        1694872.721,      197688.5984,
                  "06",          "COL", 2021L,            NA,            NA,         147323.198,      196873.2392,
                  "28",          "TAM", 2021L,            NA,            NA,         703153.767,      194137.6533,
                  "24",          "SLP", 2021L,            NA,            NA,         518970.792,      179836.3958,
                  "00",     "Nacional", 2021L,            NA,            NA,        22662788.35,      175703.7677,
                  "11",          "GTO", 2021L,            NA,            NA,        1094551.205,       173491.433,
                  "23",         "QROO", 2021L,            NA,            NA,         335768.163,      173370.1189,
                  "25",          "SIN", 2021L,            NA,            NA,          507200.92,      163775.7607,
                  "10",          "DGO", 2021L,            NA,            NA,         299509.138,      160149.3851,
                  "31",          "YUC", 2021L,            NA,            NA,         358034.386,      149754.9299,
                  "04",         "CAMP", 2021L,            NA,            NA,         130766.713,      138864.7497,
                  "27",          "TAB", 2021L,            NA,            NA,         331441.883,      135739.3164,
                  "32",          "ZAC", 2021L,            NA,            NA,         221358.406,      133718.7416,
                  "16",         "MICH", 2021L,            NA,            NA,         624604.655,      128450.8371,
                  "17",          "MOR", 2021L,            NA,            NA,          255983.55,      127464.4546,
                  "15",          "MEX", 2021L,            NA,            NA,         2124479.31,      122865.8358,
                  "18",          "NAY", 2021L,            NA,            NA,         155281.952,      122759.1301,
                  "13",          "HGO", 2021L,            NA,            NA,         384370.514,       121715.315,
                  "30",          "VER", 2021L,            NA,            NA,         976654.029,      119890.9856,
                  "21",          "PUE", 2021L,            NA,            NA,           783742.1,      115961.3384,
                  "29",         "TLAX", 2021L,            NA,            NA,         140575.914,      101819.7525,
                  "20",          "OAX", 2021L,            NA,            NA,         377687.016,       89470.9251,
                  "12",          "GRO", 2021L,            NA,            NA,         299411.978,      83208.54718,
                  "07",         "CHPS", 2021L,            NA,            NA,         341213.689,      58976.84958
              ) %>% 
    mutate(anio = 2022) %>% 
    filter(cve_ent != "00")

d <- left_join(d_ips_2022, pib_pc) %>% 
    left_join(dimensiones) %>% 
    filter(!is.na(value))

titulo <- "PIB per cápita no petrolero vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "Pib per cápita (pesos de 2018)"
eje_y <- "Puntaje 2022"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."
unidad <- ""

modelo_05 <- lm(data = d %>% filter(id_dim=="00"), 
                formula = dim_value~log(value))
d <- filter(d, !is.na(value))
m5.0 = lm(data = d %>% filter(id_dim == "00"), formula = dim_value ~ log(value))
m5.1 = lm(data = d %>% filter(id_dim == "01"), formula = dim_value ~ log(value))
m5.2 = lm(data = d %>% filter(id_dim == "02"), formula = dim_value ~ log(value))
m5.3 = lm(data = d %>% filter(id_dim == "03"), formula = dim_value ~ log(value))
stargazer::stargazer(
    m5.0, 
    m5.1, 
    m5.2, 
    m5.3,
    type = "text"
)

g <- 
    ggplot(d, aes(x = value,
                  y = dim_value,
                  label = entidad_abr_m)) +
    geom_smooth(method = "lm", 
                col = mcv_discrete[3],
                formula = y ~ log(x)) + #exp(x)
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu") +
    facet_wrap(~nombre_dimensión, ncol = 2, scales = "free") +
    scale_x_continuous(expand = expansion(c(0.1, 0.1)), 
                       labels = scales::comma_format(suffix = unidad)) +
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        color = "",
        caption = nota_pie
    ) + 
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")

ggsave(filename = "05_infobites/11_modelos/cruces_2022/05_PIB_pc.png",
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/cruces_2022/05_PIB_pc.svg",
       width = 23, height = 12, dpi = 200)


# 06. Proporción de exportaciones ----
pp_exp_pibe <- tibble::tribble(
     ~cve_ent,        ~ent_abr_m,   ~anio,         ~value,
    "00", "Nacional", 2021L, 33.60111199,
    "01",      "AGS", 2021L, 64.15261096,
    "02",       "BC", 2021L, 94.67532818,
    "03",      "BCS", 2021L, 4.092416492,
    "04",     "CAMP", 2021L, 65.21605538,
    "05",     "COAH", 2021L, 99.56703201,
    "06",      "COL", 2021L, 10.91124104,
    "07",     "CHPS", 2021L, 5.137109108,
    "08",     "CHIH", 2021L, 121.1538773,
    "09",     "CDMX", 2021L, 1.327268705,
    "10",      "DGO", 2021L, 18.56522634,
    "11",      "GTO", 2021L,  43.9705092,
    "12",      "GRO", 2021L,  6.76109166,
    "13",      "HGO", 2021L, 8.646596267,
    "14",      "JAL", 2021L, 23.96618896,
    "15",      "MEX", 2021L, 14.66858995,
    "16",     "MICH", 2021L, 20.21755208,
    "17",      "MOR", 2021L, 22.35158008,
    "18",      "NAY", 2021L, 2.903093432,
    "19",       "NL", 2021L, 39.23040834,
    "20",      "OAX", 2021L,  5.12234588,
    "21",      "PUE", 2021L, 33.27323779,
    "22",      "QRO", 2021L, 45.09072687,
    "23",     "QROO", 2021L, 0.153434095,
    "24",      "SLP", 2021L, 45.23967148,
    "25",      "SIN", 2021L, 10.28442295,
    "26",      "SON", 2021L, 45.45041203,
    "27",      "TAB", 2021L, 26.13335676,
    "28",      "TAM", 2021L, 81.36461778,
    "29",     "TLAX", 2021L, 24.51335763,
    "30",      "VER", 2021L, 12.00998232,
    "31",      "YUC", 2021L, 5.903322215,
    "32",      "ZAC", 2021L, 32.39225177)  %>% 
    mutate(anio = 2022) %>% 
    filter(cve_ent != "00")

d <- left_join(d_ips_2022, pp_exp_pibe) %>% 
    left_join(dimensiones)

titulo <- "Proporción de exportaciones con respecto al PIB de la Entidad\nvs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "Proporción de las exportaciones (%)"
eje_y <- "Puntaje 2022"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."
unidad <- ""

modelo_06 = lm(data = d, 
               formula = dim_value ~ log(value))

d <- filter(d, !is.na(value))
m6.0 = lm(data = d %>% filter(id_dim == "00"), formula = dim_value ~ log(value))
m6.1 = lm(data = d %>% filter(id_dim == "01"), formula = dim_value ~ log(value))
m6.2 = lm(data = d %>% filter(id_dim == "02"), formula = dim_value ~ log(value))
m6.3 = lm(data = d %>% filter(id_dim == "03"), formula = dim_value ~ log(value))
stargazer::stargazer(
    m6.0, 
    m6.1, 
    m6.2, 
    m6.3,
    type = "text")

g <- 
    ggplot(d, aes(x = value,
                  y = dim_value,
                  label = entidad_abr_m)) +
    geom_smooth(method = "lm", 
                col = mcv_discrete[3],
                formula = y ~ log(x)) + #exp(x)
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu") +
    facet_wrap(~nombre_dimensión, ncol = 2, scales = "free") +
    scale_x_continuous(expand = expansion(c(0.1, 0.1)), 
                       labels = scales::comma_format(suffix = unidad)) +
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
        strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")

ggsave(filename = "05_infobites/11_modelos/cruces_2022/06_pp_PIB_exportaciones.png",
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/cruces_2022/06_pp_PIB_exportaciones.svg",
       width = 23, height = 12, dpi = 200)

# 07 Inversión extranjera directa ----
ied <- tibble::tribble(
           ~cve_ent,               ~Estado, ~Contribución.en.el.periodo,      ~value,
                 1L,      "Aguascalientes",                 0.022594969, 2.259496852,
                 2L,     "Baja California",                 0.047012719, 4.701271883,
                 3L, "Baja California Sur",                 0.018198455, 1.819845533,
                 4L,            "Campeche",                 0.006578444,  0.65784443,
                 7L,             "Chiapas",                 0.005372724, 0.537272362,
                 8L,           "Chihuahua",                 0.050327563, 5.032756276,
                 9L,    "Ciudad de México",                  0.21052191,   21.052191,
                 5L,            "Coahuila",                 0.048050308,  4.80503077,
                 6L,              "Colima",                 0.002563638, 0.256363831,
                10L,             "Durango",                 0.010257785, 1.025778484,
                15L,    "Estado de México",                  0.07739032, 7.739031966,
                11L,          "Guanajuato",                  0.04355004,  4.35500403,
                12L,            "Guerrero",                 0.009931337, 0.993133666,
                13L,             "Hidalgo",                 0.009972834, 0.997283409,
                14L,             "Jalisco",                 0.062448884, 6.244888354,
                16L,           "Michoacán",                 0.011701514, 1.170151378,
                17L,             "Morelos",                 0.010487575, 1.048757546,
                18L,             "Nayarit",                 0.008287884, 0.828788358,
                19L,          "Nuevo León",                 0.099583078, 9.958307846,
                20L,              "Oaxaca",                  0.00687659, 0.687658982,
                21L,              "Puebla",                 0.028567501, 2.856750133,
                22L,           "Querétaro",                 0.031453751, 3.145375132,
                23L,        "Quintana Roo",                 0.012936787, 1.293678737,
                24L,     "San Luis Potosí",                 0.032614897, 3.261489726,
                25L,             "Sinaloa",                 0.016750384, 1.675038409,
                26L,              "Sonora",                 0.015151338, 1.515133809,
                27L,             "Tabasco",                 0.011505923, 1.150592268,
                28L,          "Tamaulipas",                 0.037178139, 3.717813906,
                29L,            "Tlaxcala",                 0.006564283, 0.656428274,
                30L,            "Veracruz",                 0.028270257, 2.827025737,
                31L,             "Yucatán",                 0.005771797, 0.577179713,
                32L,           "Zacatecas",                 0.011526372, 1.152637195
           ) %>% 
    mutate(cve_ent = str_pad(cve_ent, side = "left", pad = "0", width = "2")) %>% 
    mutate(anio = 2022) %>% 
    filter(cve_ent != "00")

d <- left_join(d_ips_2022, ied) %>% 
    left_join(dimensiones)

titulo <- "Participación en el flujo de inversión extranjera\ndirecta vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "Porcentaje de la participación en la Inversión Extranjera Directa (%)"
eje_y <- "Puntaje 2022"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."
unidad <- "%"

modelo_07 = lm(data = d, 
               formula = dim_value ~ value)
d <- filter(d, !is.na(value))
m7.0 = lm(data = d %>% filter(id_dim == "00"), formula = dim_value ~ value)
m7.1 = lm(data = d %>% filter(id_dim == "01"), formula = dim_value ~ value)
m7.2 = lm(data = d %>% filter(id_dim == "02"), formula = dim_value ~ value)
m7.3 = lm(data = d %>% filter(id_dim == "03"), formula = dim_value ~ value)
stargazer::stargazer(
    m7.0, 
    m7.1, 
    m7.2, 
    m7.3,
    type = "text")

g <- 
    ggplot(d, aes(x = value,
                  y = dim_value,
                  label = entidad_abr_m)) +
    geom_smooth(method = "lm", 
                col = mcv_discrete[3],
                formula = y ~ x) + #exp(x)
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu") +
    facet_wrap(~nombre_dimensión, ncol = 2, scales = "free") +
    scale_x_continuous(expand = expansion(c(0.1, 0.1)), 
                       labels = scales::comma_format(suffix = unidad)) +
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
        strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/02_BASE.pdf")

ggsave(filename = "05_infobites/11_modelos/cruces_2022/07_pp_IED.png",
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/cruces_2022/07_pp_IED.svg",
       width = 23, height = 12, dpi = 200)

# 08 Pobreza laboral ----
pob_laboral <- tibble::tribble(
    ~cve_ent,              ~entidad,       ~value_prop,      ~value, ~anio,
        "01",      "Aguascalientes", 0.388287543018237,  38.8287543, 2022L,
        "02",     "Baja California",  0.21101081702059,  21.1010817, 2022L,
        "03", "Baja California Sur", 0.156416436302782, 15.64164363, 2022L,
        "04",            "Campeche", 0.426861047031124,  42.6861047, 2022L,
        "05",            "Coahuila", 0.248496425610836, 24.84964256, 2022L,
        "06",              "Colima",  0.25818772464479, 25.81877246, 2022L,
        "07",             "Chiapas", 0.695874248827724, 69.58742488, 2022L,
        "08",           "Chihuahua", 0.241729882418216, 24.17298824, 2022L,
        "09",    "Ciudad de México", 0.356028462847713, 35.60284628, 2022L,
        "10",             "Durango", 0.361853685878135, 36.18536859, 2022L,
        "11",          "Guanajuato", 0.398451141146368, 39.84511411, 2022L,
        "12",            "Guerrero", 0.681736679520216, 68.17366795, 2022L,
        "13",             "Hidalgo", 0.438428803281639, 43.84288033, 2022L,
        "14",             "Jalisco", 0.251609400005989,    25.16094, 2022L,
        "15",              "México", 0.374029418714942, 37.40294187, 2022L,
        "16",           "Michoacán", 0.386433474927383, 38.64334749, 2022L,
        "17",             "Morelos", 0.505304644129494, 50.53046441, 2022L,
        "18",             "Nayarit", 0.309161499189756, 30.91614992, 2022L,
        "19",          "Nuevo León", 0.247573187944183, 24.75731879, 2022L,
        "20",              "Oaxaca", 0.624696578673014, 62.46965787, 2022L,
        "21",              "Puebla", 0.494196137206683, 49.41961372, 2022L,
        "22",           "Querétaro", 0.414593409112344, 41.45934091, 2022L,
        "23",        "Quintana Roo", 0.287979892487852, 28.79798925, 2022L,
        "24",     "San Luis Potosí", 0.433431109644112, 43.34311096, 2022L,
        "25",             "Sinaloa", 0.315465655427684, 31.54656554, 2022L,
        "26",              "Sonora", 0.307222835218296, 30.72228352, 2022L,
        "27",             "Tabasco", 0.463213429349734, 46.32134293, 2022L,
        "28",          "Tamaulipas", 0.337480482239549, 33.74804822, 2022L,
        "29",            "Tlaxcala", 0.479735764274102, 47.97357643, 2022L,
        "30",            "Veracruz", 0.546680223520277, 54.66802235, 2022L,
        "31",             "Yucatán", 0.347896327851374, 34.78963279, 2022L,
        "32",           "Zacatecas", 0.477974706154299, 47.79747062, 2022L,
         "00",            "Nacional", 0.401143025676388, 40.11430257, 2022L
    ) %>% 
    filter(cve_ent != "00")

d <- left_join(d_ips_2022, pob_laboral) %>% 
    left_join(dimensiones)

titulo <- "Pobreza laboral vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "Porcentaje de la población en pobreza laboral"
eje_y <- "Puntaje 2022"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."
unidad <- "%"

modelo_08 = lm(data = d, formula = dim_value ~ value)

d <- filter(d, !is.na(value))
m8.0 = lm(data = d %>% filter(id_dim == "00"), formula = dim_value ~ value)
m8.1 = lm(data = d %>% filter(id_dim == "01"), formula = dim_value ~ value)
m8.2 = lm(data = d %>% filter(id_dim == "02"), formula = dim_value ~ value)
m8.3 = lm(data = d %>% filter(id_dim == "03"), formula = dim_value ~ value)
stargazer::stargazer(
    m8.0, 
    m8.1, 
    m8.2, 
    m8.3,
    type = "text"
)

g <- 
    ggplot(d, aes(x = value,
                  y = dim_value,
                  label = entidad_abr_m)) +
    geom_smooth(method = "lm", 
                col = mcv_discrete[3],
                formula = y ~ x) + #exp(x)
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu") +
    facet_wrap(~nombre_dimensión, ncol = 2, scales = "free") +
    scale_x_continuous(expand = expansion(c(0.1, 0.1)), 
                       labels = scales::comma_format(suffix = unidad)) +
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
        strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")

ggsave(filename = "05_infobites/11_modelos/cruces_2022/09_pp_pobreza_laboral.png",
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/cruces_2022/08_pp_pobreza_laboral.svg",
       width = 23, height = 12, dpi = 200)


# 09. Ranking Anuncios Inversión ----
ranking_ips <- d_ips_2022 %>% 
    filter(cve_ent != "00") %>% 
    # filter(id_dim == "00") %>%
    group_by(id_dim) %>% 
    mutate(lugar_ips = rank(-dim_value)) %>% 
    arrange(lugar_ips) %>% 
    select(cve_ent, anio, lugar_ips, entidad_abr_m, id_dim) %>% 
    mutate(cve_ent = as.numeric(cve_ent)) %>% 
    left_join(dimensiones)
    
lugares_anuncios_ie <- tibble::tribble(
                           ~cve_ent, ~edo_anuncios,              ~nom_ent,  ~total_anuncios, ~lugar,
                                19L,         "NLE",          "Nuevo León", 44L,     1L,
                                 5L,         "COA",            "Coahuila", 40L,     2L,
                                22L,         "QUE",           "Querétaro", 28L,     3L,
                                15L,         "MEX",    "Estado de México", 25L,     4L,
                                11L,         "GUA",          "Guanajuato", 22L,     5L,
                                24L,         "SLP",     "San Luis Potosí", 13L,     6L,
                                 8L,         "CHH",           "Chihuahua", 12L,     7L,
                                 1L,         "AGU",      "Aguascalientes",  9L,     8L,
                                 2L,         "BCN",     "Baja California",  8L,     9L,
                                32L,         "ZAC",           "Zacatecas",  6L,    10L,
                                14L,         "JAL",             "Jalisco",  5L,    11L,
                                10L,         "DUR",             "Durango",  4L,    12L,
                                28L,         "TAM",          "Tamaulipas",  4L,    13L,
                                16L,         "MIC",           "Michoacán",  3L,    14L,
                                21L,         "PUE",              "Puebla",  3L,    15L,
                                30L,         "VER",            "Veracruz",  3L,    16L,
                                 9L,         "CMX",    "Ciudad de México",  2L,    17L,
                                13L,         "HID",             "Hidalgo",  2L,    18L,
                                26L,         "SON",              "Sonora",  2L,    19L,
                                31L,         "YUC",             "Yucatán",  2L,    20L,
                                17L,         "MOR",             "Morelos",  1L,    21L,
                                27L,         "TAB",             "Tabasco",  1L,    22L,
                                 3L,         "BCS", "Baja California Sur",  0L,     NA,
                                 4L,         "CAM",            "Campeche",  0L,     NA,
                                 7L,         "CHP",             "Chiapas",  0L,     NA,
                                 6L,         "COL",              "Colima",  0L,     NA,
                                12L,         "GRO",            "Guerrero",  0L,     NA,
                                18L,         "NAY",             "Nayarit",  0L,     NA,
                                20L,         "OAX",              "Oaxaca",  0L,     NA,
                                23L,         "ROO",        "Quintana Roo",  0L,     NA,
                                25L,         "SIN",             "Sinaloa",  0L,     NA,
                                29L,         "TLA",            "Tlaxcala",  0L,     NA) %>% 
    # filter(!is.na(lugar)) %>% 
    select(cve_ent, lugar_anuncios = lugar, total_anuncios)

lugares_anuncios_ie = lugares_anuncios_ie %>% 
    mutate(lugar_inverso = rank(total_anuncios, ties.method = "first"))

cruce_lugares <- left_join(ranking_ips, lugares_anuncios_ie) %>% 
    filter(!is.na(lugar_anuncios))

titulo <- "Ranking IPS y Ranking de Anuncios de Inversión"
subtitulo <- NULL
eje_x <- "Lugar en la medición del IPS"
eje_y <- "Lugar en total de anuncios de inversión"
nota_pie <- "No se registró ningún anuncio de inversión para los estados de <b>Oaxaca</b>, <b>Guerrero</b>, <b>Chiapas</b>, Colima, Nayarit, Quintana Roo, Sinaloa o Tlaxcala."
unidad <- ""

max(cruce_lugares$lugar_inverso)
max(cruce_lugares$lugar_anuncios)

g <- cruce_lugares %>% 
    ggplot(aes(x = lugar_ips,
               y = lugar_anuncios)) + 
    geom_hline(yintercept = 22/2, linetype = 2, color = "gray50") + 
    geom_vline(xintercept = 32/2, linetype = 2, color = "gray50") + 
    annotate(geom = "rect", 
             xmin = c(0), 
             xmax = c(16), 
             ymin = c(0), 
             ymax = c(11), 
             fill = c("blue"), 
             alpha = 0.3) +
    annotate(geom = "rect", 
             xmin = c(16), 
             xmax = c(32), 
             ymin = c(11), 
             ymax = c(22), 
             fill = c("red"), 
             alpha = 0.3) + 
    annotate(geom = "rect", 
             xmin = c(16), 
             xmax = c(32), 
             ymin = c(0), 
             ymax = c(11), 
             fill = c("yellow"), 
             alpha = 0.3) +
    annotate(geom = "rect", 
             xmin = c(0), 
             xmax = c(16), 
             ymin = c(11), 
             ymax = c(22), 
             fill = c("green"), 
             alpha = 0.3) +
    geom_point(col = mcv_discrete[1]) +
    facet_wrap(~nombre_dimensión, scales = "free") + 
    ggrepel::geom_text_repel(aes(label = entidad_abr_m), 
                             family = "Ubuntu") +
    scale_y_reverse() + 
    scale_x_reverse() + 
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
        strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  
    
g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/03_integralia.pdf")
    
# ggsave(filename = "05_infobites/11_modelos/cruces_2022/09_cruce_rankings_anuncios.png",
#        width = 23, height = 12, dpi = 200)
# ggsave(filename = "05_infobites/11_modelos/cruces_2022/09_cruce_rankings_anuncios.svg",
#        width = 23, height = 12, dpi = 200)

g <- cruce_lugares %>% 
    filter(id_dim == "00") %>% 
    ggplot(aes(x = lugar_ips,
               y = lugar_anuncios)) + 
    geom_hline(yintercept = 22/2, linetype = 2, color = "gray50") + 
    geom_vline(xintercept = 32/2, linetype = 2, color = "gray50") + 
    annotate(geom = "rect", 
             xmin = c(0), 
             xmax = c(16), 
             ymin = c(0), 
             ymax = c(11), 
             fill = c("blue"), 
             alpha = 0.3, 
             size = 8) +
    annotate(geom = "rect", 
             xmin = c(16), 
             xmax = c(32), 
             ymin = c(11), 
             ymax = c(22), 
             fill = c("red"), 
             alpha = 0.3, 
             size = 8) + 
    annotate(geom = "rect", 
             xmin = c(16), 
             xmax = c(32), 
             ymin = c(0), 
             ymax = c(11), 
             fill = c("yellow"), 
             alpha = 0.3, 
             size = 8) +
    annotate(geom = "rect", 
             xmin = c(0), 
             xmax = c(16), 
             ymin = c(11), 
             ymax = c(22), 
             fill = c("green"), 
             alpha = 0.3, 
             size = 8) +
    annotate(geom = "label", 
             x = c(32, 16, 16, 32), 
             y = c(12, 12, 1, 1), 
             label = c("Bajo Progreso social/Pocos anuncios de inversión",
                       "Alto progreso social/Pocos anuncios de inversión", 
                       "Alto progreso social/Muchos anuncios de inversión", 
                       "Bajo progreso social/Muchos anuncios de inversión"
                       ), 
             family = "Ubuntu", 
             fontface = "bold", 
             hjust = -0.02, 
             size = 8
             ) +
    geom_point(fill = mcv_discrete[1], 
               color = "white", 
               stroke = 2,
               size = 6, 
               pch = 21) +
    ggrepel::geom_text_repel(aes(label = entidad_abr_m), 
                             family = "Ubuntu", 
                             size = 8) +
    geom_segment(aes(x = 10, y = 21, xend = 1, yend = 21), 
                 arrow = arrow(length = unit(1.1, "cm")), 
                 color = "red", linewidth = 10) + 
    geom_segment(aes(x = 31, y = 11, xend = 31, yend = 2), 
                 arrow = arrow(length = unit(1.1, "cm")), 
                 color = "red", linewidth = 10) + 
    annotate(geom = "text",
             label = c("Más progreso social", "Más anuncios de inversión"),
             x = c(10, 31), 
             y = c(21, 11), 
             size = 6,
             fontface = "bold",
             family = "Ubuntu",
             color = "white", 
             angle = c("0", "90"), hjust =-0.05) + 
    scale_y_reverse(expand = expansion(c(0.01,0)), breaks = c(1:22)) + 
    scale_x_reverse(expand = expansion(c(0,0)), breaks = c(1:32)) + 
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        caption = nota_pie,
        color = ""
    )   + 
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
        plot.caption       = ggtext::element_markdown(size = 20),
        plot.margin        = margin(2, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/03_integralia.pdf")

ggsave(filename = "05_infobites/11_modelos/cruces_2022/09_01_cruce_rankings_anuncios.png",
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/cruces_2022/09_01_cruce_rankings_anuncios.svg",
       width = 23, height = 12, dpi = 200)

# 10. IED Absolutos ----

ied_absolutos <- tibble::tribble(
    ~cve_ent,               ~Estado, ~flujo_2022, ~acum_15_22,      ~value,
    1L,      "Aguascalientes",       598.3,      7214.6,  "2.71%",
    2L,     "Baja California",     1873.54,    16268.48,  "6.11%",
    3L, "Baja California Sur",      813.64,     6476.49,  "2.43%",
    4L,            "Campeche",       40.24,     1836.31,  "0.69%",
    7L,             "Chiapas",      186.74,     1811.23,  "0.68%",
    8L,           "Chihuahua",      1868.6,    17147.33,  "6.44%",
    9L,    "Ciudad de México",    11182.44,    78435.46, "29.45%",
    5L,            "Coahuila",      872.81,    14546.33,  "5.46%",
    6L,              "Colima",       95.06,      878.82,  "0.33%",
    10L,             "Durango",       565.5,     3872.63,  "1.45%",
    15L,    "Estado de México",     2292.57,    25209.03,  "9.47%",
    11L,          "Guanajuato",     1570.57,    14749.44,  "5.54%",
    12L,            "Guerrero",      245.94,     3148.57,  "1.18%",
    13L,             "Hidalgo",      412.74,     3494.22,  "1.31%",
    14L,             "Jalisco",     2977.87,    22599.77,  "8.49%",
    16L,           "Michoacán",      166.86,      3465.8,  "1.30%",
    17L,             "Morelos",       81.13,     2972.08,  "1.12%",
    18L,             "Nayarit",      474.45,     3173.95,  "1.19%",
    19L,          "Nuevo León",     4429.24,    35396.26, "13.29%",
    20L,              "Oaxaca",      157.54,      2166.3,  "0.81%",
    21L,              "Puebla",      797.12,     9222.72,  "3.46%",
    22L,           "Querétaro",      734.69,     9867.46,  "3.71%",
    23L,        "Quintana Roo",      456.14,     4380.32,  "1.64%",
    24L,     "San Luis Potosí",      329.61,     9368.51,  "3.52%",
    25L,             "Sinaloa",      870.86,     6227.32,  "2.34%",
    26L,              "Sonora",      464.18,     4989.14,  "1.87%",
    27L,             "Tabasco",       27.26,     3145.52,  "1.18%",
    28L,          "Tamaulipas",      979.24,    11886.95,  "4.46%",
    29L,            "Tlaxcala",      275.03,     2327.11,  "0.87%",
    30L,            "Veracruz",        0.01,     7558.33,  "2.84%",
    31L,             "Yucatán",      547.37,     2662.76,  "1.00%",
    32L,           "Zacatecas",         8.7,     3118.86,  "1.17%"
) %>% 
    mutate(cve_ent = str_pad(cve_ent, side = "left", pad = "0", width = "2")) %>% 
    mutate(value = str_remove_all(value, "%") %>% as.numeric()) %>% 
    select(cve_ent, value = flujo_2022)

d <- left_join(d_ips_2022, ied_absolutos) %>%
    left_join(dimensiones)

titulo <- "Inversión extranjera directa vs Índice de Progreso Social"
subtitulo <- "Intervalos de confianza al 95%"
eje_x <- "Inversión Extranjera Directa (MDD)"
eje_y <- "Puntaje 2022"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."
unidad <- ""

modelo_10 = lm(data = d, formula = dim_value ~ value)
d <- filter(d, !is.na(value))
m10.0 = lm(data = d %>% filter(id_dim == "00"), formula = dim_value ~ log(value))
m10.1 = lm(data = d %>% filter(id_dim == "01"), formula = dim_value ~ log(value))
m10.2 = lm(data = d %>% filter(id_dim == "02"), formula = dim_value ~ log(value))
m10.3 = lm(data = d %>% filter(id_dim == "03"), formula = dim_value ~ log(value))
stargazer::stargazer(
    m10.0, 
    m10.1, 
    m10.2, 
    m10.3,
    type = "text"
)

g <- 
    ggplot(d, aes(x = value,
                  y = dim_value,
                  label = entidad_abr_m)) +
    geom_smooth(method = "lm", 
                col = mcv_discrete[3],
                formula = y ~ x) + #exp(x)
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu") +
    facet_wrap(~nombre_dimensión, ncol = 2, scales = "free") +
    scale_x_continuous(expand = expansion(c(0.1, 0.1)), 
                       labels = scales::comma_format(suffix = unidad)) +
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
        plot.margin        = margin(1, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/02_BASE.pdf")

ggsave(filename = "05_infobites/11_modelos/cruces_2022/10_IED_absolutos.png",
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/cruces_2022/10_IED_absolutos.svg",
       width = 23, height = 12, dpi = 200)


# 11. Anuncios absolutos ----
lugares_anuncios_ie_abs <-  tibble::tribble(
    ~cve_ent, ~edo_anuncios,              ~nom_ent,  ~total_anuncios, ~lugar,
    19L,         "NLE",          "Nuevo León", 44L,     1L,
    5L,         "COA",            "Coahuila", 40L,     2L,
    22L,         "QUE",           "Querétaro", 28L,     3L,
    15L,         "MEX",    "Estado de México", 25L,     4L,
    11L,         "GUA",          "Guanajuato", 22L,     5L,
    24L,         "SLP",     "San Luis Potosí", 13L,     6L,
    8L,         "CHH",           "Chihuahua", 12L,     7L,
    1L,         "AGU",      "Aguascalientes",  9L,     8L,
    2L,         "BCN",     "Baja California",  8L,     9L,
    32L,         "ZAC",           "Zacatecas",  6L,    10L,
    14L,         "JAL",             "Jalisco",  5L,    11L,
    10L,         "DUR",             "Durango",  4L,    12L,
    28L,         "TAM",          "Tamaulipas",  4L,    13L,
    16L,         "MIC",           "Michoacán",  3L,    14L,
    21L,         "PUE",              "Puebla",  3L,    15L,
    30L,         "VER",            "Veracruz",  3L,    16L,
    9L,         "CMX",    "Ciudad de México",  2L,    17L,
    13L,         "HID",             "Hidalgo",  2L,    18L,
    26L,         "SON",              "Sonora",  2L,    19L,
    31L,         "YUC",             "Yucatán",  2L,    20L,
    17L,         "MOR",             "Morelos",  1L,    21L,
    27L,         "TAB",             "Tabasco",  1L,    22L,
    3L,         "BCS", "Baja California Sur",  0L,     NA,
    4L,         "CAM",            "Campeche",  0L,     NA,
    7L,         "CHP",             "Chiapas",  0L,     NA,
    6L,         "COL",              "Colima",  0L,     NA,
    12L,         "GRO",            "Guerrero",  0L,     NA,
    18L,         "NAY",             "Nayarit",  0L,     NA,
    20L,         "OAX",              "Oaxaca",  0L,     NA,
    23L,         "ROO",        "Quintana Roo",  0L,     NA,
    25L,         "SIN",             "Sinaloa",  0L,     NA,
    29L,         "TLA",            "Tlaxcala",  0L,     NA) %>% 
    rename(value = total_anuncios) %>% 
    select(cve_ent, value) %>% 
    mutate(cve_ent = str_pad(cve_ent, side = "left", pad = "0", width = "2"))

d <- left_join(d_ips_2022, lugares_anuncios_ie_abs) %>%
    left_join(dimensiones)

titulo <- "Anuncios de Inversión vs Índice de Progreso Social"
subtitulo <- NULL
eje_y <- "Puntaje en el Índice de Progreso Social"
eje_x <- "Total de anuncios de inversión"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."
unidad <- ""

modelo_11 = lm(data = d, formula = dim_value ~ value)
d <- filter(d, !is.na(value))
m11.0 = lm(data = d %>% filter(id_dim == "00"), formula = dim_value ~ value)
m11.1 = lm(data = d %>% filter(id_dim == "01"), formula = dim_value ~ value)
m11.2 = lm(data = d %>% filter(id_dim == "02"), formula = dim_value ~ value)
m11.3 = lm(data = d %>% filter(id_dim == "03"), formula = dim_value ~ value)
stargazer::stargazer(
    m11.0, 
    m11.1, 
    m11.2, 
    m11.3,
    type = "text"
)

g <-
    ggplot(d, aes(x = value,
                  y = dim_value,
                  label = entidad_abr_m)) +
    geom_smooth(method = "lm", 
                col = mcv_discrete[3],
                formula = y ~ x) + #exp(x)
    geom_point(col = mcv_discrete[1]) +
    ggrepel::geom_text_repel(family = "Ubuntu") +
    facet_wrap(~nombre_dimensión, ncol = 2, scales = "free") +
    scale_x_continuous(expand = expansion(c(0.1, 0.1)), 
                       labels = scales::comma_format(suffix = unidad)) +
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
        plot.margin        = margin(2, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/03_integralia.pdf")

ggsave(filename = "05_infobites/11_modelos/cruces_2022/11_anuncios_absolutos.png",
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/cruces_2022/11_anuncios_absolutos.svg",
       width = 23, height = 12, dpi = 200)

# 12. Ranking IPS vs Ranking IED ----

lugar_ied <- ied_absolutos %>% 
    mutate(lugar_ied = rank(-value, ties.method = "first"))

ranking_ips <- d_ips_2022 %>% 
    filter(cve_ent != "00") %>% 
    filter(id_dim == "00") %>%
    group_by(id_dim) %>% 
    mutate(lugar_ips = rank(-dim_value)) %>% 
    arrange(lugar_ips) %>% 
    select(cve_ent, anio, lugar_ips, entidad_abr_m, id_dim) %>% 
    left_join(dimensiones)

cruce_lugares <- left_join(ranking_ips, lugar_ied) 

titulo <- "Ranking IPS y Ranking de Inversión extranjera directa"
subtitulo <- NULL
eje_x <- "Lugar en la medición del IPS"
eje_y <- "Lugar en montos de IED (2023)"
nota_pie <- "*Para más información del modelo consultar Anexo estadístico."
unidad <- ""

g <- cruce_lugares %>% 
    filter(id_dim == "00") %>% 
    ggplot(aes(x = lugar_ips,
               y = lugar_ied)) + 
    geom_hline(yintercept = 32/2, linetype = 2, color = "gray50") + 
    geom_vline(xintercept = 32/2, linetype = 2, color = "gray50") + 
    annotate(geom = "rect", 
             xmin = c(0), 
             xmax = c(16), 
             ymin = c(0), 
             ymax = c(16), 
             fill = c("blue"), 
             alpha = 0.3, 
             size = 8) +
    annotate(geom = "rect", 
             xmin = c(16), 
             xmax = c(32), 
             ymin = c(16), 
             ymax = c(32), 
             fill = c("red"), 
             alpha = 0.3, 
             size = 8) + 
    annotate(geom = "rect", 
             xmin = c(16), 
             xmax = c(32), 
             ymin = c(0), 
             ymax = c(16), 
             fill = c("yellow"), 
             alpha = 0.3, 
             size = 8) +
    annotate(geom = "rect", 
             xmin = c(0), 
             xmax = c(16), 
             ymin = c(16), 
             ymax = c(32), 
             fill = c("green"), 
             alpha = 0.3, 
             size = 8) +
    annotate(geom = "label", 
             x = c(32,
                   9,
                   16,
                   32), 
             y = c(17,
                   17,
                   1,
                   1), 
             label = c("Bajo Progreso social/Poca inversión",
                       "Alto progreso social/Poca inversión", 
                       "Alto progreso social/Mucha inversión", 
                       "Bajo progreso social/Mucha inversión"
             ), 
             family = "Ubuntu", 
             fontface = "bold", 
             hjust = -0.02, 
             size = 8
    ) +
    geom_point(fill = mcv_discrete[1], 
               color = "white", 
               stroke = 2,
               size = 6, 
               pch = 21) +
    ggrepel::geom_text_repel(aes(label = entidad_abr_m), 
                             family = "Ubuntu", 
                             size = 8) +
    # Linea abajo 
    geom_segment(aes(x = 10, y = 31, xend = 1, yend = 31),
                 arrow = arrow(length = unit(1.1, "cm")),
                 color = "red", linewidth = 10) +
    # Linea izquierda
    geom_segment(aes(x = 31,xend = 31, y = 15, yend = 3),
                 arrow = arrow(length = unit(1.1, "cm")),
                 color = "red", linewidth = 10) +
    annotate(geom = "text",
             label = c("Más progreso social", "Mayor IED recibida"),
             x = c(10, 31), 
             y = c(31, 15), 
             size = 6,
             fontface = "bold",
             family = "Ubuntu",
             color = "white", 
             angle = c("0", "90"), hjust =-0.05) + 
    scale_y_reverse(expand = expansion(c(0.01,0)), breaks = c(1:32)) + 
    scale_x_reverse(expand = expansion(c(0.01,0)), breaks = c(1:32)) + 
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
        plot.margin        = margin(2.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/02_BASE.pdf")

ggsave(filename = "05_infobites/11_modelos/cruces_2022/12_cruce_rankings_ied.png",
       width = 23, height = 12, dpi = 200)
ggsave(filename = "05_infobites/11_modelos/cruces_2022/12_cruce_rankings_ied.svg",
       width = 23, height = 12, dpi = 200)

# 13 Cambio PIB per cápita vs. Cambio IPS. ----

pib_pc <- readxl::read_xlsx("02_datos_crudos/bd_pib_pc.xlsx") %>% 
    select(cve_ent, entidad_abr_m, indicador_value, anio)
ranking <- readxl::read_xlsx("03_ips_clean/08_ips_ranking.xlsx") %>% 
    select(cve_ent, grupo_pib) %>% unique()
d_ips_2022 <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx") %>% 
    filter(id_dim == "00") %>% 
    left_join(ranking) %>% 
    mutate(grupo_pib = ifelse(is.na(grupo_pib), "Nacional", grupo_pib))


## 2022-2018- ---

cambio_ips <- d_ips_2022 %>% 
    filter(anio %in% c(2019, 2021)) %>% 
    pivot_wider(id_cols = c(cve_ent,entidad_abr_m,grupo_pib), 
                values_from = dim_value, 
                names_from = anio) %>% 
    mutate(cambio_IPS = `2021`-`2019`) %>% 
    select(cve_ent, entidad_abr_m, cambio_IPS,grupo_pib) %>% 
    mutate(grupo_pib = factor(grupo_pib, levels = c("Nacional",
                                                       "PIB per cápita alto",
                                                       "PIB per cápita medio",
                                                       "PIB per cápita bajo" )))
cambio_pib_pc <- pib_pc %>% 
    filter(anio %in% c(2019, 2021)) %>% 
    pivot_wider(id_cols = c(cve_ent,entidad_abr_m), 
                values_from = indicador_value, 
                names_from = anio) %>% 
    mutate(cambio_PIB = 100*((`2021`-`2019`)/`2019`)) %>% 
    select(cve_ent, entidad_abr_m, cambio_PIB)

datos_cambios <- left_join(cambio_ips, cambio_pib_pc)

g = datos_cambios %>% 
    ggplot(aes(x = cambio_IPS, 
               y = cambio_PIB, 
               color = grupo_pib)) + 
    annotate(geom = "label", 
             x = c(min(datos_cambios$cambio_IPS), 
                   min(datos_cambios$cambio_IPS),
                   max(datos_cambios$cambio_IPS),
                   max(datos_cambios$cambio_IPS)), 
             y = c(min(datos_cambios$cambio_PIB), 
                   max(datos_cambios$cambio_PIB),
                   min(datos_cambios$cambio_PIB), 
                   2
                   # max(datos_cambios$cambio_PIB)
                   ), 
             label = c("Caída PIB per cápita\nCaída puntaje IPS",
                       "Aumento PIB per cápita\nCaída puntaje IPS", 
                       "Caída PIB per cápita\nAumento puntaje IPS",
                       "Aumento PIB per cápita\nAumento puntaje IPS"),
             hjust = c(0.1, 0.1, 1, 1),
             color = "red", 
             fontface = "bold", 
             family = "Ubuntu", 
             size = 6) +
    geom_hline(yintercept = 0, linetype = 2, linewidth = 1, color = "gray50") + 
    geom_vline(xintercept = 0, linetype = 2, linewidth = 1, color = "gray50") +
    geom_point(size = 5, 
               pch = 21, 
               # color = mcv_discrete[1], 
               fill = "white", stroke = 2) + 
    ggrepel::geom_text_repel(aes(label = entidad_abr_m), 
                             fontface = "bold", 
                             color = "gray20",
                             size = 8, 
                             family = "Ubuntu") + 
    scale_y_continuous(labels = scales::comma_format(prefix = "")) + 
    scale_x_continuous(labels = scales::comma_format(prefix = "", suffix = ".0")) + 
    scale_color_manual(values = mcv_discrete[c(4,3,2,5)]) +
    # "#6950d8" "#3CEAFA" "#00b783" "#ff6260" "#ffaf84" "#ffbd41"
    labs(title = "Cambio en la calificación IPS vs\ncambio absoluto en el PIB per cápita, 2019-2021", 
         y = "Cambio porcentual en el Pib per cápita 2019-2021", 
         x = "Cambio en el puntaje del Índice de Progreso Social 2019-2021", 
         color = NULL) +
    theme_minimal() +
        theme(
            plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
            plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
            plot.caption       = element_text(size = 20),
            plot.margin        = margin(1, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
            strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
            panel.background   = element_rect(fill = "transparent", colour = NA),
            text               = element_text(family = "Ubuntu"),
            axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.text.x        = element_text(family = "Ubuntu", size = 30, colour = "#777777"),
            axis.text.y        = element_text(family = "Ubuntu", size = 30, colour = "#777777"),
            legend.text        = element_text(family = "Ubuntu", size = 20, colour = "black"),
            axis.line = element_line(),
            legend.position    = "bottom")  + 
    guides(color = guide_legend(ncol = 4))
    
    g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
    
    ggsave(filename = "05_infobites/11_modelos/cruces_2022/13_cambio_ips_pibPC_2019_2021.png",
           width = 23, height = 12, dpi = 200)
    ggsave(filename = "05_infobites/11_modelos/cruces_2022/13_cambio_ips_pibPC_2019_2021.svg",
           width = 23, height = 12, dpi = 200)
    

# Cambio 2015-2021
    cambio_ips <- d_ips_2022 %>% 
        filter(anio %in% c(2015, 2021)) %>% 
        pivot_wider(id_cols = c(cve_ent,entidad_abr_m,grupo_pib), 
                    values_from = dim_value, 
                    names_from = anio) %>% 
        mutate(cambio_IPS = `2021`-`2015`) %>% 
        select(cve_ent, entidad_abr_m, cambio_IPS,grupo_pib) %>% 
        mutate(grupo_pib = factor(grupo_pib, levels = c("Nacional",
                                                        "PIB per cápita alto",
                                                        "PIB per cápita medio",
                                                        "PIB per cápita bajo" )))
    
    cambio_pib_pc <- pib_pc %>% 
        filter(anio %in% c(2015, 2021)) %>% 
        pivot_wider(id_cols = c(cve_ent,entidad_abr_m), 
                    values_from = indicador_value, 
                    names_from = anio) %>% 
        mutate(cambio_PIB = 100*((`2021`-`2015`)/`2015`)) %>% 
        select(cve_ent, entidad_abr_m, cambio_PIB)
    
    datos_cambios <- left_join(cambio_ips, cambio_pib_pc)
    
    g = datos_cambios %>% 
        ggplot(aes(x = cambio_IPS, 
                   y = cambio_PIB, 
                   color = grupo_pib)) + 
        annotate(geom = "label", 
                 x = c(min(datos_cambios$cambio_IPS), 
                       min(datos_cambios$cambio_IPS),
                       max(datos_cambios$cambio_IPS),
                       max(datos_cambios$cambio_IPS)), 
                 y = c(min(datos_cambios$cambio_PIB), 
                       max(datos_cambios$cambio_PIB),
                       min(datos_cambios$cambio_PIB), 
                       2
                       # max(datos_cambios$cambio_PIB)
                       ), 
                 label = c("Caída PIB per cápita\nCaída puntaje IPS",
                           "Aumento PIB per cápita\nCaída puntaje IPS", 
                           "Caída PIB per cápita\nAumento puntaje IPS",
                           "Aumento PIB per cápita\nAumento puntaje IPS"),
                 hjust = c(0.1, 0.1, 1, 1),
                 color = "red", 
                 fontface = "bold", 
                 family = "Ubuntu", 
                 size = 6) +
        geom_hline(yintercept = 0, linetype = 2, linewidth = 1, color = "gray50") + 
        geom_vline(xintercept = 0, linetype = 2, linewidth = 1, color = "gray50") +
        geom_point(size = 5, 
                   pch = 21, 
                   # color = mcv_discrete[1], 
                   fill = "white", stroke = 2) + 
        ggrepel::geom_text_repel(aes(label = entidad_abr_m), 
                                 fontface = "bold", 
                                 size = 8, 
                                 color = "gray50",
                                 family = "Ubuntu") + 
        scale_y_continuous(labels = scales::comma_format(prefix = "")) + 
        scale_x_continuous(labels = scales::comma_format(prefix = "", suffix = ".0")) + 
        scale_color_manual(values = mcv_discrete[c(4,3,2,5)]) +
        labs(title = "Cambio en la calificación IPS vs\ncambio absoluto en el PIB per cápita, 2015-2021", 
             y = "Cambio porcentual en el Pib per cápita 2015-2021",
             color = NULL,
             x = "Cambio en el puntaje del Índice de Progreso Social 2015-2021") +
        theme_minimal() +
        theme(
            plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
            plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
            plot.caption       = element_text(size = 20),
            plot.margin        = margin(1, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
            strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
            panel.background   = element_rect(fill = "transparent", colour = NA),
            text               = element_text(family = "Ubuntu"),
            axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.text.x        = element_text(family = "Ubuntu", size = 30, colour = "#777777"),
            axis.text.y        = element_text(family = "Ubuntu", size = 30, colour = "#777777"),
            legend.text        = element_text(family = "Ubuntu", size = 20, colour = "black"),
            axis.line = element_line(),
            legend.position    = "bottom")  + 
        guides(color = guide_legend(ncol = 4))
    
    g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
    
    ggsave(filename = "05_infobites/11_modelos/cruces_2022/13_cambio_ips_pibPC_2015_2021.png",
           width = 23, height = 12, dpi = 200)
    ggsave(filename = "05_infobites/11_modelos/cruces_2022/13_cambio_ips_pibPC_2015_2021.svg",
           width = 23, height = 12, dpi = 200)
    

# Cambio 2015-2018
    cambio_ips <- d_ips_2022 %>% 
        filter(anio %in% c(2015, 2019)) %>% 
        pivot_wider(id_cols = c(cve_ent,entidad_abr_m, grupo_pib), 
                    values_from = dim_value, 
                    names_from = anio) %>% 
        mutate(cambio_IPS = `2019`-`2015`) %>% 
        select(cve_ent, entidad_abr_m, cambio_IPS,grupo_pib) %>% 
        mutate(grupo_pib = factor(grupo_pib, levels = c("Nacional",
                                                        "PIB per cápita alto",
                                                        "PIB per cápita medio",
                                                        "PIB per cápita bajo" )))
    
    cambio_pib_pc <- pib_pc %>% 
        filter(anio %in% c(2015, 2019)) %>% 
        pivot_wider(id_cols = c(cve_ent,entidad_abr_m), 
                    values_from = indicador_value, 
                    names_from = anio) %>% 
        mutate(cambio_PIB = 100*((`2019`-`2015`)/`2015`)) %>% 
        select(cve_ent, entidad_abr_m, cambio_PIB)
    
    datos_cambios <- left_join(cambio_ips, cambio_pib_pc)
    
    g =
        datos_cambios %>% 
        ggplot(aes(x = cambio_IPS, 
                   y = cambio_PIB, 
                   color = grupo_pib)) + 
        annotate(geom = "label", 
                 x = c(min(datos_cambios$cambio_IPS), 
                       min(datos_cambios$cambio_IPS),
                       max(datos_cambios$cambio_IPS),
                       max(datos_cambios$cambio_IPS)), 
                 y = c(min(datos_cambios$cambio_PIB), 
                       2,
                       # max(datos_cambios$cambio_PIB),
                       min(datos_cambios$cambio_PIB), 
                       max(datos_cambios$cambio_PIB)), 
                 label = c("Caída PIB per cápita\nCaída puntaje IPS",
                           "Aumento PIB per cápita\nCaída puntaje IPS", 
                           "Caída PIB per cápita\nAumento puntaje IPS",
                           "Aumento PIB per cápita\nAumento puntaje IPS"),
                 hjust = c(0.1, 0.1, 1, 1),
                 color = "red", 
                 fontface = "bold", 
                 family = "Ubuntu", 
                 size = 6) +
        geom_hline(yintercept = 0, linetype = 2, linewidth = 1, color = "gray50") + 
        geom_vline(xintercept = 0, linetype = 2, linewidth = 1, color = "gray50") +
        geom_point(size = 5, 
                   pch = 21, 
                   # color = mcv_discrete[1], 
                   fill = "white", stroke = 2) + 
        ggrepel::geom_text_repel(aes(label = entidad_abr_m), 
                                 fontface = "bold", 
                                 color = "gray50",
                                 size = 8, 
                                 family = "Ubuntu") + 
        scale_y_continuous(labels = scales::comma_format(prefix = "")) + 
        scale_x_continuous(labels = scales::comma_format(prefix = "", suffix = ".0")) + 
        scale_color_manual(values = mcv_discrete[c(4,3,2,5)]) +
        labs(title = "Cambio en la calificación IPS vs\ncambio absoluto en el PIB per cápita, 2015 - 2019", 
             y = "Cambio porcentual en el Pib per cápita 2015-2019", 
             color = NULL,
             x = "Cambio en el puntaje del Índice de Progreso Social 2015-2019") +
        theme_minimal() +
        theme(
            plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
            plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
            plot.caption       = element_text(size = 20),
            plot.margin        = margin(1, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
            strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
            panel.background   = element_rect(fill = "transparent", colour = NA),
            text               = element_text(family = "Ubuntu"),
            axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.text.x        = element_text(family = "Ubuntu", size = 30, colour = "#777777"),
            axis.text.y        = element_text(family = "Ubuntu", size = 30, colour = "#777777"),
            legend.text        = element_text(family = "Ubuntu", size = 20, colour = "black"),
            axis.line = element_line(),
            legend.position    = "bottom")  + 
        guides(color = guide_legend(ncol = 4))
    
    g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
    
    ggsave(filename = "05_infobites/11_modelos/cruces_2022/13_cambio_ips_pibPC_2015_2019.png",
           width = 23, height = 12, dpi = 200)
    ggsave(filename = "05_infobites/11_modelos/cruces_2022/13_cambio_ips_pibPC_2015_2019.svg",
           width = 23, height = 12, dpi = 200)
    

# 14. Barras de cambio ----
nb <- function(x, n = 1) prettyNum(format(round(x, n),nsmall = n), big.mark = ",")    
anios <- c(2019, 2021)

gen_grafica_simple_cambio <- function(anios){

cambio_ips <- d_ips_2022 %>% 
        filter(anio %in% anios) %>% 
        pivot_wider(id_cols = c(cve_ent,entidad_abr_m,grupo_pib), 
                    values_from = dim_value, 
                    names_from = anio) %>% 
    mutate(entidad_abr_m = ifelse(entidad_abr_m == "Nacional", 
                                  yes = "<b style = 'color:#ff6260;'>Nacional</b>", 
                                  no = entidad_abr_m))

names(cambio_ips)[c(4,5)] <- c("anio_1", "anio_2")    

cambio_ips <- cambio_ips %>% 
    mutate(cambio_ips_pp = anio_2-anio_1
           # , 
           # cambio_ips_pp = 100*(cambio_ips/anio_1)
           )

g = cambio_ips %>% 
    ggplot(aes(x = reorder(entidad_abr_m, 
                           cambio_ips_pp), 
               y = cambio_ips_pp)) + 
    geom_col(aes(fill = sign(cambio_ips_pp) %>% factor())) + 
    geom_hline(yintercept = 0, linetype = 1, linewidth = 1.1, color = "gray10") + 
    geom_text(aes(label = str_c(nb(cambio_ips_pp), ""),
                  hjust = sign(cambio_ips_pp) %>% factor(), 
                  color = sign(cambio_ips_pp) %>% factor()), 
              size = 10, 
              family = "Ubuntu",
              fontface = "bold",
              vjust = 0.5,
              angle = 90) + 
    labs(y = str_c("Variación ", str_c(anios, collapse = "-"), " (puntaje IPS)"), 
         x = NULL, 
         title = "Variación valores IPS", 
         subtitle = str_c("Periodo: ", str_c(anios, collapse = "-"))) +
    scale_y_continuous(expand = expansion(c(0.25, 0.3)), 
                       labels = comma_format(suffix = ".0")) +
    scale_fill_manual(values = c(mcv_discrete[c(4,3)])) + 
    scale_color_manual(values = c(mcv_discrete[c(4,3)])) + 
    scale_discrete_manual(aesthetics = "hjust", values = c(1.1, -0.1)) +
    theme_minimal() + 
    theme(legend.position = "none", 
          # text = element_text(family = "Ubuntu"),
          # plot.title.position = "plot", 
          plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
          plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
          plot.caption       = element_text(size = 20),
          plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
          panel.background   = element_rect(fill = "transparent", colour = NA),
          text               = element_text(family = "Ubuntu"),
          axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
          axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
          axis.text.x        = ggtext::element_markdown(family = "Ubuntu", size = 25, colour = "#777777", angle = 90, hjust = 1, vjust = 0.5),
          axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
          legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"), 
          panel.grid.major = element_line(linetype = 2)
          )

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = str_c("05_infobites/11_modelos/cruces_2022/cambio_",str_c(anios, collapse = "-"),".png"),
       width = 23, height = 12, dpi = 200)
ggsave(filename = str_c("05_infobites/11_modelos/cruces_2022/cambio_", str_c(anios, collapse = "-"), ".svg"),
       width = 23, height = 12, dpi = 200)

}

# Generando las gráficas
gen_grafica_simple_cambio(anios = c(2015, 2019))
gen_grafica_simple_cambio(anios = c(2019, 2021))
gen_grafica_simple_cambio(anios = c(2019, 2022))
gen_grafica_simple_cambio(anios = c(2015, 2022))

