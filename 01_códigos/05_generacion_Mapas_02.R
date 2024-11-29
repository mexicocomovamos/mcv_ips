#------------------------------------------------------------------------------#
# Proyecto:               ÍNDICE DE PROGRESO SOCIAL
# Objetivo:               Procesar datos para creación de mapas en DW
#
# Encargada:              Regina Isabel Medina Rosales     
# Correos:                regimedina19@gmail.com
# 
# Fecha de creación:      1 de noviembre de 2022
# Última actualización:   3 de noviembre de 2022
#------------------------------------------------------------------------------#



# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# ----- Cargar paquetería 
require(pacman)
p_load(readxl, tidyverse, dplyr, googledrive, mxmaps, 
       googlesheets4, lubridate, janitor, beepr, sf, s2)

require(extrafont)

loadfonts(device="pdf")
loadfonts(device="postscript")
# ----- Desactiva notación científica
options(scipen=999)

# ----- Vaciar espacio de trabajo 
rm(list=ls())

# ----- Colores MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados





# 1. Cargar datos --------------------------------------------------------------

# ---- Bases en repositorio de GitHub
# Dimensiones
df_ips <- read_excel(paste_inp("00_IPS_COMPLETE_LONG.xlsx"))

# Variables
# df_ips <- read_excel(paste_inp("01_ips_long.xlsx"))

# Dimensiones, componentes e indicadores (variables)
df_ips_raw <- read_excel(paste_inp("08_02_ips_ranking_series.xlsx"))

# 2. Procesar datos ------------------------------------------------------------

## 2.0. Catálogos de nombres ---------------------------------------------------

# Crear catálogo de nombres de entidades en ISO para Data Wraper
df_iso <- dplyr::tribble(
    ~cve_ent,	~iso_entidad,
    "01",		"MX-AGU",
    "02",		"MX-BCN",
    "03",		"MX-BCS",
    "04",		"MX-CAM",
    "05",		"MX-COA",
    "06",		"MX-COL",
    "07",		"MX-CHP",
    "08",		"MX-CHH",
    "09",		"MX-CMX",
    "10",		"MX-DUR",
    "12",		"MX-GRO",
    "11",		"MX-GUA",
    "13",		"MX-HID",
    "14",		"MX-JAL",
    "15",		"MX-MEX",
    "16",		"MX-MIC",
    "17",		"MX-MOR",
    "18",		"MX-NAY",
    "19",		"MX-NLE",
    "20",		"MX-OAX",
    "21",		"MX-PUE",
    "22",		"MX-QUE",
    "23",		"MX-ROO",
    "24",		"MX-SLP",
    "25",		"MX-SIN",
    "26",		"MX-SON",
    "27",		"MX-TAB",
    "28",		"MX-TAM",
    "29",		"MX-TLA",
    "30",		"MX-VER",
    "31",		"MX-YUC",
    "32",		"MX-ZAC",
    "33",		"MX-NAC")


## 2.1. IPS general ------------------------------------------------------------

#### 2.1.1. Data Wrapper -------------------------------------------------------

# ---- Dar formato 
df_ips <- df_ips_raw %>% 
    # Dejar solo puntaje global de IPS en 2021 
    filter(
        id == "00", 
        anio == 2023) %>% 
    left_join(df_iso) %>% 
    mutate(
        id_dim  = as.numeric(id), 
        cve_ent = as.numeric(cve_ent), 
        ord     = cve_ent
           ) %>% 
    select(cve_ent, anio, iso_entidad, id_dim, dim_value = value, 
           ranking = ips_rank_nacional, entidad_abr_m, ord)



#### 2.1.2. Mapa png -----------------------------------------------------------

# ---- Preparar los datos
# Pegar coordenadas geográficas con datos del IPS
df_mxmap <- mxstate.map         %>% 
    rename(cve_ent = region)    %>%
    left_join(
        df_ips %>% mutate(cve_ent = str_pad(cve_ent, 2, pad = "0"))
    ) 

# Dar formato de geometría 
tempo <- df_mxmap %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    group_by(cve_ent) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") 

# Catálogo de etiquetas por entidad
# mxmap_labs <- df_mxmap %>% 
#     distinct(cve_ent, entidad_abr_m, dim_value) %>% 
#     bind_cols(
#         as.data.frame(rgeos::gCentroid(as(tempo, "Spatial"), byid = TRUE))
#     )

# Catálogo de etiquetas por entidad
sf_use_s2(FALSE)

centroide <- tempo %>% 
    # st_make_valid() %>% 
    # as.data.frame(st_centroid(geometry) %>% 
    mutate(
        centroid = st_centroid(geometry),
        long = st_coordinates(centroid)[, 1],  
        lat = st_coordinates(centroid)[, 2])

mxmap_labs <- df_mxmap %>% 
    distinct(cve_ent, entidad_abr_m, dim_value) %>% 
    left_join(
        centroide)

sf_use_s2(T) 
# ---- Visualización del mapa 
g <- ggplot(
    # Datos
    df_mxmap,
    # Coordenadas
    aes(long, lat, group=group, fill = dim_value)) +
    coord_map() +
    # Polígonos
    geom_polygon(color = "black", size = .2, show.legend = F, alpha = 0.8) +
    geom_label(data = mxmap_labs,
               aes(long, lat, label = paste0(entidad_abr_m, "\n[", round(dim_value, 2), "]"), group = cve_ent), alpha = .4,
               family = "Ubuntu", show.legend = F) +
    # Etiquetas
    labs(title = unique(df_mxmap$region)) +
    # Escalas
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 64.4) +
    # Tema
    theme_void() +
    theme(
        plot.title = element_text(size = 40, family = "Ubuntu", face = "bold", 
                                  colour = mcv_blacks[3], hjust = 0.5)) 

# ---- Guardar mapa
ggsave(
    filename = paste0("05_infobites/10_mapas/00-00_ips_global.svg"), 
    width = 23, height = 12, dpi = 200
)

ggsave(
    filename = paste0("05_infobites/10_mapas/00-00_ips_global.png"), 
    device = "png", type = "cairo",  
    width = 23, height = 12, dpi = 200
)



## 2.2. Dimensión 1 ------------------------------------------------------------

#### 2.2.1. Data Wrapper -------------------------------------------------------

# ---- Dar formato 
df_dim1 <- df_ips_raw %>% 
    # Dejar solo puntaje global de IPS en 2021 
    filter(
        id == "01", 
        anio == 2023) %>% 
    left_join(df_iso) %>% 
    mutate(
        id_dim  = as.numeric(id), 
        cve_ent = as.numeric(cve_ent), 
        ord     = cve_ent
    ) %>% 
    select(cve_ent, anio, iso_entidad, id_dim, dim_value = value, 
           ranking = ips_rank_nacional, entidad_abr_m, ord)



#### 2.2.2. Mapa png -----------------------------------------------------------

# ---- Preparar los datos
# Pegar coordenadas geográficas con datos del IPS
df_mxmap <- mxstate.map         %>% 
    rename(cve_ent = region)    %>%
    left_join(
        df_dim1 %>% mutate(cve_ent = str_pad(cve_ent, 2, pad = "0"))
    ) 

# Dar formato de geometría 
tempo <- df_mxmap %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    group_by(cve_ent) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") 

# Catálogo de etiquetas por entidad
# mxmap_labs <- df_mxmap %>% 
#     distinct(cve_ent, entidad_abr_m, dim_value) %>% 
#     bind_cols(
#         as.data.frame(rgeos::gCentroid(as(tempo, "Spatial"), byid = TRUE))
#     )

# Catálogo de etiquetas por entidad
sf_use_s2(FALSE)

centroide <- tempo %>% 
    # st_make_valid() %>% 
    # as.data.frame(st_centroid(geometry) %>% 
    mutate(
        centroid = st_centroid(geometry),
        long = st_coordinates(centroid)[, 1],  
        lat = st_coordinates(centroid)[, 2])

mxmap_labs <- df_mxmap %>% 
    distinct(cve_ent, entidad_abr_m, dim_value) %>% 
    left_join(
        centroide)

sf_use_s2(T) 
# ---- Visualización del mapa 
g <- ggplot(
    # Datos
    df_mxmap,
    # Coordenadas
    aes(long, lat, group=group, fill = dim_value)) +
    coord_map() +
    # Polígonos
    geom_polygon(color = "black", size = .2, show.legend = F, alpha = 0.8) +
    geom_label(data = mxmap_labs,
               aes(long, lat, label = paste0(entidad_abr_m, "\n[", round(dim_value, 2), "]"), group = cve_ent), alpha = .4,
               family = "Ubuntu", show.legend = F) +
    # Etiquetas
    labs(title = unique(df_mxmap$region)) +
    # Escalas
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 64.4) +
    # Tema
    theme_void() +
    theme(
        plot.title = element_text(size = 40, family = "Ubuntu", face = "bold", 
                                  colour = mcv_blacks[3], hjust = 0.5)) 

# ---- Guardar mapa
ggsave(
    filename = paste0("05_infobites/10_mapas/00-01_ips_dim1.svg"), 
    width = 23, height = 12, dpi = 200
)

ggsave(
    filename = paste0("05_infobites/10_mapas/00-01_ips_dim1.png"), 
    device = "png", type = "cairo",  
    width = 23, height = 12, dpi = 200
)



## 2.3. Dimensión 2 ------------------------------------------------------------

#### 2.3.1. Data Wrapper -------------------------------------------------------

# ---- Dar formato 
df_dim2 <- df_ips_raw %>% 
    # Dejar solo puntaje global de IPS en 2021 
    filter(
        id == "02", 
        anio == 2023) %>% 
    left_join(df_iso) %>% 
    mutate(
        id_dim  = as.numeric(id), 
        cve_ent = as.numeric(cve_ent), 
        ord     = cve_ent
    ) %>% 
    select(cve_ent, anio, iso_entidad, id_dim, dim_value = value, 
           ranking = ips_rank_nacional, entidad_abr_m, ord)



#### 2.3.2. Mapa png -----------------------------------------------------------

# ---- Preparar los datos
# Pegar coordenadas geográficas con datos del IPS
df_mxmap <- mxstate.map         %>% 
    rename(cve_ent = region)    %>%
    left_join(
        df_dim2 %>% mutate(cve_ent = str_pad(cve_ent, 2, pad = "0"))
    ) 

# Dar formato de geometría 
tempo <- df_mxmap %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    group_by(cve_ent) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") 

# # Catálogo de etiquetas por entidad
# mxmap_labs <- df_mxmap %>% 
#     distinct(cve_ent, entidad_abr_m, dim_value) %>% 
#     bind_cols(
#         as.data.frame(rgeos::gCentroid(as(tempo, "Spatial"), byid = TRUE))
#     )

# Catálogo de etiquetas por entidad
sf_use_s2(FALSE)

centroide <- tempo %>% 
    # st_make_valid() %>% 
    # as.data.frame(st_centroid(geometry) %>% 
    mutate(
        centroid = st_centroid(geometry),
        long = st_coordinates(centroid)[, 1],  
        lat = st_coordinates(centroid)[, 2])

mxmap_labs <- df_mxmap %>% 
    distinct(cve_ent, entidad_abr_m, dim_value) %>% 
    left_join(
        centroide)

sf_use_s2(T) 
# ---- Visualización del mapa 
g <- ggplot(
    # Datos
    df_mxmap,
    # Coordenadas
    aes(long, lat, group=group, fill = dim_value)) +
    coord_map() +
    # Polígonos
    geom_polygon(color = "black", size = .2, show.legend = F, alpha = 0.8) +
    geom_label(data = mxmap_labs,
               aes(long, lat, label = paste0(entidad_abr_m, "\n[", round(dim_value, 2), "]"), group = cve_ent), alpha = .4,
               family = "Ubuntu", show.legend = F) +
    # Etiquetas
    labs(title = unique(df_mxmap$region)) +
    # Escalas
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 64.4) +
    # Tema
    theme_void() +
    theme(
        plot.title = element_text(size = 40, family = "Ubuntu", face = "bold", 
                                  colour = mcv_blacks[3], hjust = 0.5)) 

# ---- Guardar mapa
ggsave(
    filename = paste0("05_infobites/10_mapas/00-02_ips_dim2.svg"), 
    width = 23, height = 12, dpi = 200
)

ggsave(
    filename = paste0("05_infobites/10_mapas/00-02_ips_dim2.png"), 
    device = "png", type = "cairo",  
    width = 23, height = 12, dpi = 200
)



## 2.4. Dimensión 3 ------------------------------------------------------------

#### 2.4.1. Data Wrapper -------------------------------------------------------

# ---- Dar formato 
df_dim3 <- df_ips_raw %>% 
    # Dejar solo puntaje global de IPS en 2021 
    filter(
        id == "03", 
        anio == 2023) %>% 
    left_join(df_iso) %>% 
    mutate(
        id_dim  = as.numeric(id), 
        cve_ent = as.numeric(cve_ent), 
        ord     = cve_ent
    ) %>% 
    select(cve_ent, anio, iso_entidad, id_dim, dim_value = value, 
           ranking = ips_rank_nacional, entidad_abr_m, ord)



#### 2.4.2. Mapa png -----------------------------------------------------------

# ---- Preparar los datos
# Pegar coordenadas geográficas con datos del IPS
df_mxmap <- mxstate.map         %>% 
    rename(cve_ent = region)    %>%
    left_join(
        df_dim3 %>% mutate(cve_ent = str_pad(cve_ent, 2, pad = "0"))
    ) 

# Dar formato de geometría 
tempo <- df_mxmap %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    group_by(cve_ent) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") 

# Catálogo de etiquetas por entidad
# mxmap_labs <- df_mxmap %>% 
#     distinct(cve_ent, entidad_abr_m, dim_value) %>% 
#     bind_cols(
#         as.data.frame(rgeos::gCentroid(as(tempo, "Spatial"), byid = TRUE))
#     )

# Catálogo de etiquetas por entidad
sf_use_s2(FALSE)

centroide <- tempo %>% 
    # st_make_valid() %>% 
    # as.data.frame(st_centroid(geometry) %>% 
    mutate(
        centroid = st_centroid(geometry),
        long = st_coordinates(centroid)[, 1],  
        lat = st_coordinates(centroid)[, 2])

mxmap_labs <- df_mxmap %>% 
    distinct(cve_ent, entidad_abr_m, dim_value) %>% 
    left_join(
        centroide)

sf_use_s2(T) 
# ---- Visualización del mapa 
g <- ggplot(
    # Datos
    df_mxmap,
    # Coordenadas
    aes(long, lat, group=group, fill = dim_value)) +
    coord_map() +
    # Polígonos
    geom_polygon(color = "black", size = .2, show.legend = F, alpha = 0.8) +
    geom_label(data = mxmap_labs,
               aes(long, lat, label = paste0(entidad_abr_m, "\n[", round(dim_value, 2), "]"), group = cve_ent), alpha = .4,
               family = "Ubuntu", show.legend = F) +
    # Etiquetas
    labs(title = unique(df_mxmap$region)) +
    # Escalas
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 64.4) +
    # Tema
    theme_void() +
    theme(
        plot.title = element_text(size = 40, family = "Ubuntu", face = "bold", 
                                  colour = mcv_blacks[3], hjust = 0.5)) 

# ---- Guardar mapa
ggsave(
    filename = paste0("05_infobites/10_mapas/00-03_ips_dim3.svg"), 
    width = 23, height = 12, dpi = 200
)

ggsave(
    filename = paste0("05_infobites/10_mapas/00-03_ips_dim3.png"), 
    device = "png", type = "cairo",  
    width = 23, height = 12, dpi = 200
)



# FIN. -------------------------------------------------------------------------
