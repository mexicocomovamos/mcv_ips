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



# 1. Mundo vs México ----
ips_glob <- readxl::read_excel("02_datos_crudos/04_ips_global.xlsx")

d <- ips_glob %>% 
    mutate(value = round(value, 1),
           name_comp  = str_wrap(name_comp, 25))

d_wide <- pivot_wider(d, names_from = "tipo", values_from = "value") %>%
    ungroup()

titulo <- "Índice de Progreso Social Global 2021"
subtitulo <- "Desagregación por componente"
eje_y <- ""
eje_x <- "Puntaje"
g <- 
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

ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/04_ips_global/01_ips_global.png", 
       width = 23, height = 12, dpi = 100)
