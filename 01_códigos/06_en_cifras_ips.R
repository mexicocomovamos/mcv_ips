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

# Colores MCV -----
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

## 0.2. Tokens ----
google_token <- "AIzaSyDF4E80nih1fBMPg785wYO-ruAKgJiEdW0"

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1859408845")[1, 2])
1
# Función para importar de manera más corta desde drive
imp_dv <- function(x, y){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x), sheet = y)}

# 1. Datos ----
ips_complete <- data.frame()
i = 58
for(i in 3:58){
    
    ips_tempo <- readxl::read_excel("02_datos_crudos/00_IPS_bd.xlsx", sheet = i) %>% 
        # imp_dv(v_id, i) %>% 
        mutate_at(
            vars(c(contains("value"),contains("anio"))),
            ~as.numeric(str_remove_all(as.character(.),"[[:blank:]]"))
        ) %>% 
        mutate_at(
            vars(c(starts_with("id"), starts_with("cve"))),
            ~as.character(str_pad(.,2,"left","0"))
        ) %>% 
        filter(anio %in% c(2015:2022))
    
    
    ips_complete <- bind_rows(ips_complete, ips_tempo)
    
    # Sys.sleep(2.5) 
    
}

ips_complete_filt <- ips_complete %>% 
    mutate(id_dim_ind = paste0(id_dimension, id_indicador)) %>% 
    select(cve_ent:anio, id_dim_ind, id_dimension:indicador_value) %>% 
    left_join(
        imp_dv(v_id, 2) %>% 
            select(id_dimension, id_componente,  id_indicador, direccion, indicador, indicador_name, datos_actualizados) 
    ) %>% 
    select(-datos_actualizados) %>% 
    drop_na(cve_ent) %>% 
    mutate(id_unique = paste(id_dimension, id_componente,  id_indicador, sep = "_")) %>% 
    as_tibble()
1
# 2. Heatmaps ----
# v_id_dim_ind <- unique(ips_complete_filt$id_dim_ind)
v_id_dim_ind <- unique(ips_complete_filt$id_dim_ind)
eje_y <- ""
eje_x <- "Años"

i = 1

# CUCHAREO: Se multiplicó x 100 el valor del indicador 51: 
ips_complete_filt$indicador_value[ips_complete_filt$id_indicador == "51"] <- 100*(ips_complete_filt$indicador_value[ips_complete_filt$id_indicador == "51"])

dicc_numeros <- ips_complete_filt %>% 
    select(id_dimension, id_indicador, id_componente) %>% 
    group_by(id_componente) %>% 
    unique() %>% 
    group_by(id_dimension, id_componente) %>% 
    mutate(id_para_grafica = str_c("0", rank(as.numeric(id_indicador))))

ips_complete_filt <- ips_complete_filt %>% 
    left_join(dicc_numeros)

i = 1
for(i in 1:length(v_id_dim_ind)){
    
    a <- ips_complete_filt %>% 
        filter(id_dim_ind == v_id_dim_ind[i]) %>% 
        # filter(direccion == 1) %>% 
        filter(anio >= 2014) %>% 
        glimpse
    print(paste0(v_id_dim_ind[i], " - ", unique(a$indicador)))
    v_cols_plot <- rev(mcv_semaforo)
    titulo <- str_c(unique(a$id_dimension), 
                    ".", 
                    unique(a$id_componente), 
                    ".", 
                    unique(a$id_para_grafica), 
                    " ", 
                    unique(a$indicador_name))
        
    subtitulo <- paste0(min(a$anio), " - ", max(a$anio))
    
    ggplot(data = 
               a %>% 
               filter(!as.numeric(cve_ent) > 33),
           aes(x = as.factor(anio), 
               y = reorder(entidad_abr_m, -as.numeric(cve_ent)),
               fill = (indicador_value)*direccion)) +
        geom_tile(col = "white") +
        geom_text(aes(label = round(abs(indicador_value),2)), family = "Ubuntu", size = 4) +
        scale_fill_gradientn(colours = v_cols_plot)+
        guides(label = "none") +
        # scale_x_discrete(position = "top")+
        # Etiquetas
        labs(
            title = str_wrap(titulo, 25), 
            subtitle = subtitulo, 
            x = eje_x, 
            y = eje_y, 
            color = ""
        )   + 
        coord_fixed() +
        theme_minimal() +
        theme(
            plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8", hjust = 0.5),
            plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", hjust = 0.5),
            plot.caption       = element_text(size = 20),
            panel.grid.minor   = element_blank(),
            panel.grid.major.x = element_blank(),
            plot.title.position = "plot",
            panel.background   = element_rect(fill = "transparent", colour = NA),
            text               = element_text(family = "Ubuntu"),
            axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
            axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            legend.position    = "none")  
    
    
    ggsave(filename = paste0("05_infobites/00_en_cifras_ips/", unique(a$id_unique), "_", str_replace_all(tolower(trimws(str_remove_all(unique(a$indicador), "[[:punct:]]"))), " ", "_"), ".png"), 
           width = 12, height = 23, dpi = 200)
    
    ggsave(filename = paste0("05_infobites/00_en_cifras_ips/", unique(a$id_unique), "_", str_replace_all(tolower(trimws(str_remove_all(unique(a$indicador), "[[:punct:]]"))), " ", "_"), ".svg"), 
           width = 12, height = 23, dpi = 200)
    
}

# # ## manual ----
# i = 35
# i = 43
# i = 3

# Indicador 56
a <- ips_complete_filt %>%
    filter(id_indicador == "56") %>%
    filter(anio >= 2014)

print(paste0(v_id_dim_ind[i], " - ", 
             unique(a$indicador) %>% str_replace_all("toneladas", "miles de toneladas")
             ))
v_cols_plot <- rev(mcv_semaforo)
titulo <- str_c("02.04.05 ", unique(a$indicador_name) %>% str_replace_all("toneladas", "miles de toneladas"))
# 
subtitulo <- paste0(min(a$anio), " - ", max(a$anio))
ggplot(data =
           a, #%>%
           # filter(as.numeric(cve_ent) > 0)   %>%
           # bind_rows(
           #     a %>%
           #         filter(as.numeric(cve_ent) > 0) %>%
           #         group_by(anio, direccion, indicador, indicador_name) %>%
           #         summarise(indicador_value = mean(indicador_value)) %>%
           #         ungroup() %>%
           #         mutate(cve_ent = "00", entidad_abr_m = "Nacional")
           # ),
       aes(x = as.factor(anio),
           y = reorder(entidad_abr_m, -as.numeric(cve_ent)),
           fill = ((indicador_value)*direccion))) +
    geom_tile(col = "white") +
    geom_text(aes(label = scales::comma(round(indicador_value/1000, 0))), family = "Ubuntu", size = 6) +
    scale_fill_gradientn(colours = v_cols_plot)+
    guides(label = "none") +
    # scale_x_discrete(position = "top")+
    # Etiquetas
    labs(
        title = str_wrap(titulo, 25),
        subtitle = subtitulo,
        x = eje_x,
        y = eje_y,
        color = ""
    )   +
    #coord_fixed() +
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8", hjust = 0.5),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", hjust = 0.5),
        plot.caption       = element_text(size = 20),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        plot.title.position = "plot",
        legend.position    = "none")

ggsave(filename = paste0("05_infobites/00_en_cifras_ips/", unique(a$id_unique), "_", str_replace_all(tolower(trimws(str_remove_all(unique(a$indicador), "[[:punct:]]"))), " ", "_"), ".png"), 
       width = 12, height = 23, dpi = 200)

ggsave(filename = paste0("05_infobites/00_en_cifras_ips/", unique(a$id_unique), "_", str_replace_all(tolower(trimws(str_remove_all(unique(a$indicador), "[[:punct:]]"))), " ", "_"), ".svg"), 
       width = 12, height = 23, dpi = 200)





a <- ips_complete_filt %>%
    filter(id_indicador == "42") %>%
    filter(anio >= 2014)

print(paste0(v_id_dim_ind[i], " - ", 
             unique(a$indicador) %>% str_replace_all("toneladas", "miles de toneladas")
))
v_cols_plot <- rev(mcv_semaforo)
titulo <- str_c("03.10.03 ", unique(a$indicador_name))
# 
subtitulo <- paste0(min(a$anio), " - ", max(a$anio))
ggplot(data =
           a, #%>%
       # filter(as.numeric(cve_ent) > 0)   %>%
       # bind_rows(
       #     a %>%
       #         filter(as.numeric(cve_ent) > 0) %>%
       #         group_by(anio, direccion, indicador, indicador_name) %>%
       #         summarise(indicador_value = mean(indicador_value)) %>%
       #         ungroup() %>%
       #         mutate(cve_ent = "00", entidad_abr_m = "Nacional")
       # ),
       aes(x = as.factor(anio),
           y = reorder(entidad_abr_m, -as.numeric(cve_ent)),
           fill = ((indicador_value)*direccion))) +
    geom_tile(col = "white") +
    geom_text(aes(label = scales::comma(round(indicador_value, 0))), family = "Ubuntu", size = 6) +
    scale_fill_gradientn(colours = v_cols_plot)+
    guides(label = "none") +
    # scale_x_discrete(position = "top")+
    # Etiquetas
    labs(
        title = str_wrap(titulo, 25),
        subtitle = subtitulo,
        x = eje_x,
        y = eje_y,
        color = ""
    )   +
    #coord_fixed() +
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8", hjust = 0.5),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", hjust = 0.5),
        plot.caption       = element_text(size = 20),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        plot.title.position = "plot",
        legend.position    = "none")

ggsave(filename = paste0("05_infobites/00_en_cifras_ips/", unique(a$id_unique), "_", str_replace_all(tolower(trimws(str_remove_all(unique(a$indicador), "[[:punct:]]"))), " ", "_"), ".png"), 
       width = 12, height = 23, dpi = 200)

ggsave(filename = paste0("05_infobites/00_en_cifras_ips/", unique(a$id_unique), "_", str_replace_all(tolower(trimws(str_remove_all(unique(a$indicador), "[[:punct:]]"))), " ", "_"), ".svg"), 
       width = 12, height = 23, dpi = 200)

# ggsave(filename = paste0("05_infobites/00_en_cifras_ips/", unique(a$id_unique), "_", str_replace_all(tolower(trimws(str_remove_all(unique(a$indicador), "[[:punct:]]"))), " ", "_"), ".png"),
#        width = 12, height = 23, dpi = 200)
# 
# ggsave(filename = paste0("05_infobites/00_en_cifras_ips/", unique(a$id_unique), "_", str_replace_all(tolower(trimws(str_remove_all(unique(a$indicador), "[[:punct:]]"))), " ", "_"), ".svg"),
#        width = 12, height = 23, dpi = 200)
# 
# 
# 
# i = 19
# a <- ips_complete_filt %>%
#     filter(id_dim_ind == v_id_dim_ind[i]) %>%
#     # filter(anio >= 2016) %>%
#     glimpse
# print(paste0(v_id_dim_ind[i], " - ", unique(a$indicador)))
# v_cols_plot <-
#     if(unique(a$direccion)>0){mcv_semaforo}else{rev(mcv_semaforo)}
# titulo <- unique(a$indicador_name)
# 
# subtitulo <- paste0(min(a$anio), " - ", max(a$anio))
# 
# ggplot(data =
#            a,
#        aes(x = as.factor(anio),
#            y = reorder(entidad_abr_m, -as.numeric(cve_ent)),
#            fill = (indicador_value)*direccion)) +
#     geom_tile(col = "white") +
#     geom_text(aes(label = scales::comma(round(indicador_value,2))), family = "Ubuntu", size = 4) +
#     scale_fill_gradientn(colours = v_cols_plot)+
#     guides(label = "none") +
#     # scale_x_discrete(position = "top")+
#     # Etiquetas
#     labs(
#         title = str_wrap(titulo, 25),
#         subtitle = subtitulo,
#         x = eje_x,
#         y = eje_y,
#         color = ""
#     )   +
#     coord_fixed() +
#     theme_minimal() +
#     theme(
#         plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8", hjust = 0.5),
#         plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", hjust = 0.5),
#         plot.caption       = element_text(size = 20),
#         panel.grid.minor   = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.background   = element_rect(fill = "transparent", colour = NA),
#         text               = element_text(family = "Ubuntu"),
#         axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
#         axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
#         axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
#         axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
#         # plot.title.position = "plot",
#         legend.position    = "none")
# 
# ggsave(filename = paste0("05_infobites/00_en_cifras_ips/", unique(a$id_dim_ind), "_", str_replace_all(tolower(trimws(str_remove_all(unique(a$indicador), "[[:punct:]]"))), " ", "_"), ".png"),
#        width = 12, height = 23, dpi = 200)
# 
# ggsave(filename = paste0("05_infobites/00_en_cifras_ips/", unique(a$id_dim_ind), "_", str_replace_all(tolower(trimws(str_remove_all(unique(a$indicador), "[[:punct:]]"))), " ", "_"), ".svg"),
#        width = 12, height = 23, dpi = 200)
# 
