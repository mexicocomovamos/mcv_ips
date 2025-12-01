#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Crear heatmaps para los componentes del IPS
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          19 de octubre de 2022
# Última actualización:       19 de octubre de 2022
#------------------------------------------------------------------------------#

# Para correr correctamente, este código requiere la base 
# 08_02_ips_ranking_series.xlsx que se produce en el códgio "03_fichas_entidades"

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# ----- Cargar paquetería 
require(pacman)
p_load(readxl, tidyverse, dplyr, googledrive, 
       googlesheets4, lubridate, janitor, beepr)

# ----- Desactiva notación científica
options(scipen=999)

# ----- Vaciar espacio de trabajo 
rm(list=ls())

# ----- Colores MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados


# ----- Funciones de importación y exportación
paste_inp <- function(x){paste0("03_ips_clean/", x)}
paste_fig <- function(x){paste0(
    "05_infobites/00_en_cifras_ips/02_componentes/", x)}

# ---- Configuración para usar google drive

# Activar credenciales
v_usuaria <- "regina"
# v_usuaria <- "katia"

# googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
# googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))

# Verificar credenciales
googledrive::drive_user()
googlesheets4::gs4_user()


# Función para importar de manera más corta desde drive
imp_dv <- function(x, y){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x), sheet = y)}

# 1. Cargar datos --------------------------------------------------------------

# ---- Bases en repositorio de GitHub
# Dimensiones
# df_ips <- read_excel(paste_inp("00_IPS_COMPLETE_LONG.xlsx"))
# df_ips <- read_excel(paste_inp("00_IPS_COMPLETE_WIDE.xlsx"))


# Variables
# df_ips <- read_excel(paste_inp("01_ips_long.xlsx"))

# Dimensiones, componentes e indicadores (variables)
dicc <- read_excel(paste_inp("08_02_ips_ranking_series.xlsx")) %>% 
    select(id, name) %>% 
    unique()
df_ips <- readxl::read_xlsx("03_ips_clean/00_IPS_COMPLETE_WIDE.xlsx", sheet = 2) %>% 
    pivot_longer(cols = 4:ncol(.)) %>%
    mutate(id = str_remove(name, "comp") %>% str_remove("\\_")) %>% 
    select(-name) %>% 
    left_join(dicc, by = "id")

# 2. Preprocesamiento de datos -------------------------------------------------

# # Comparar (uso la base con formato ancho)
# df_cambio <- df_ips %>% 
#     select(-c("01", "02", "03")) %>% 
#     pivot_wider(
#         names_from = anio, 
#         names_prefix = "a",
#         values_from = "00") %>% 
#     mutate(diff_2021_2019 = a2021-a2019)
# 
# openxlsx::write.xlsx(df_cambio, file = "02_bases_procesadas/04_cambio2019_2021.xlsx")
# 
# # Filtrar base y dejar solo los componentes del año más reciente 

df_componentes <- df_ips     %>%
    filter(!(cve_ent %in% c(98, 99)))

# 
# # Vectores
v_componentes       <- unique(df_componentes$name)
v_id_componentes    <- unique(df_componentes$id)

# 3. Heatmaps ------------------------------------------------------------------

## 3.0. Configuración figuras --------------------------------------------------

# ---- Tema 
tema <- theme_minimal() +
    theme(
        plot.title        = element_text(size = 40, face = "bold", colour = "#6950D8"),
        plot.subtitle     = element_text(size = 35, colour = "#777777", margin=margin(0,0,10,0)),
        plot.margin       = margin(0.3, 0.3, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
        plot.caption      = element_text(size = 20),
        strip.text.x      = element_text(size = 15),
        panel.grid.minor  = element_blank(),
        panel.grid.major  = element_blank(),
        panel.background  = element_rect(fill = "transparent",colour = NA),
        text              = element_text(family = "Ubuntu"),
        axis.title.x      = element_blank(),
        axis.title.y      = element_text(size = 25),
        axis.text.x       = element_text(size = 25, angle = 90, vjust = 0.5),
        axis.text.y       = element_text(size = 25),
        legend.text       = element_text(size = 15),
        legend.key.size   = unit(0.7, "cm"),
        legend.position   = "top")


# ---- Vectores de texto 
v_empty  <- ""

# ---- Colores 
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados

## 3.1. Ejemplo ----------------------------------------------------------------

# ---- Dejar solo componente de interés 
df_data <- df_componentes               %>% 
    filter(name == v_componentes[1])

# ---- Guardar vectores de texto dinámico (títulos )
v_name      <- unique(df_data$name)
v_id        <- unique(df_data$id)

v_years     <- paste0(min(df_data$anio), " - ", max(df_data$anio))


# v_cols_plot <- 
#     if(unique(a$direccion)>0){mcv_semaforo}else{rev(mcv_semaforo)}

v_cols_plot <- mcv_semaforo



## 3.2. Bucle ------------------------------------------------------------------

i = 3
for(i in 1:length(v_componentes)){
    
    # Imprimir vuelta 
    print(v_componentes[i])
    
    # ---- Dejar solo componente de esta vuelta
    df_data <- df_componentes               %>% 
        filter(name == v_componentes[i])
    
    # ---- Guardar vectores de texto dinámico (títulos )
    v_name      <- unique(df_data$name)
    v_id        <- unique(df_data$id)
    v_years     <- paste0(min(df_data$anio), " - ", max(df_data$anio))
    
    
    # ---- Establecer escala de colores
    # v_cols_plot <- 
    #     if(unique(a$direccion)>0){mcv_semaforo}else{rev(mcv_semaforo)}
    
    v_cols_plot <- rev(mcv_semaforo)
    
    # ---- Heatmap 
    ggplot(
        # Datos
        df_data,
        # Coordenadas
        aes(x = as.factor(anio), 
            y = reorder(entidad_abr_m, -as.numeric(cve_ent)), 
            fill = (value))) +
        coord_fixed() +
        # Geoms
        geom_tile(col = "white") +
        geom_text(aes(label = round(value, 2)), 
                  family = "Ubuntu",
                  fontface = "bold",
                  size = 4) +
        # Etiquetas
        labs(
            title = str_wrap(str_c(str_extract(v_id, "^\\d\\d"),".", str_extract(v_id, "\\d\\d$"), " ", v_name), 
                              25), 
            subtitle = v_years, 
            x = "Años", 
            y = v_empty, 
            color = v_empty
        ) +
        # Escalas
        scale_fill_gradientn(colours = v_cols_plot)+
        # Tema
        tema +
        theme(plot.title = element_text(hjust = 0.5), 
              axis.ticks = element_blank(),
              plot.subtitle = element_text(hjust = 0.5)) + 
        guides(label = "none", fill = "none")
    
    # ---- Guardar figura 
    # Nombre de la figura 
    v_fig <-  paste0(
        # v_id,
        str_c(str_extract(v_id, "^\\d\\d"), "_", str_extract(v_id, "\\d\\d$"), "_", "00"),
        "_", str_replace_all
        (tolower(trimws(str_remove_all(v_name, "[[:punct:]]"))), " ", "_"))
    
    # Formato .png 
    ggsave(filename = str_c("05_infobites/00_en_cifras_ips/", paste0(v_fig, ".png")), 
        device = "png", type = "cairo",  # Activar estas opciones solo para windows
        width = 12, height = 23, dpi = 200)
    
    # Formato .svg 
    ggsave(filename = str_c("05_infobites/00_en_cifras_ips/", paste0(v_fig, ".svg")), 
        width = 12, height = 23, dpi = 200)
    # paste_fig(paste0(v_fig, ".svg")), 
}

# FIN. -------------------------------------------------------------------------
