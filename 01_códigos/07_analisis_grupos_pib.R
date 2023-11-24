#------------------------------------------------------------------------------#
# Proyecto:               ÍNDICE DE PROGRESO SOCIAL
# Objetivo:               Análisis por grupos del PIB (fichas, cambios y mapas)
#
# Encargada:              Regina Isabel Medina Rosales     
# Correos:                regimedina19@gmail.com
# 
# Fecha de creación:      2 de noviembre de 2022
# Última actualización:   2 de noviembre de 2022
#------------------------------------------------------------------------------#

# Fuente API del INEGI: https://www.inegi.org.mx/servicios/api_indicadores.html 

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
# devtools::install_github("munichrocker/DatawRappr")

require(pacman)
p_load(readxl, inegiR, tidyverse, dplyr, DatawRappr, mxmaps, sf, 
       googledrive, googlesheets4, lubridate, janitor, beepr)

require(extrafont)

loadfonts(device="pdf")
loadfonts(device="postscript")

# Desactiva notación científica
options(scipen=999)

# Vaciar espacio de trabajo 
rm(list=ls())

# Colores MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados

# Vectores para directorio 
inp <- "02_datos_crudos/"

# # Activar las credenciales de google
# v_usuaria <- "regina"
# v_usuaria <- "katia"
v_usuaria <- "axel"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))


# Verificar credenciales
googledrive::drive_user()
googlesheets4::gs4_user()

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x))}

# ----- Funciones de importación y exportación
paste_inp <- function(x){paste0("03_ips_clean/"       , x)}
paste_out <- function(x){paste0("02_bases_procesadas/", x)}
paste_fig <- function(x){paste0("05_infobites/12_grupos_pib/", x)}

# 1. Importar datos ------------------------------------------------------------

# ---- Población por entidad
#load(("02_datos_crudos/df_pop_state.RData"))

# ---- IPS 2022
df_ips2022 <- read_excel(paste_inp("08_ips_ranking.xlsx"))

# ---- Bases históricas
# Dimensiones
df_ips <- read_excel(paste_inp("00_IPS_COMPLETE_LONG.xlsx"))

# Variables
# df_ips <- read_excel(paste_inp("01_ips_long.xlsx"))

# Dimensiones, componentes e indicadores (variables)
# df_ips <- read_excel(paste_inp("08_02_ips_ranking_series.xlsx"))

# 2. Procesar datos ------------------------------------------------------------

## 2.0. Población --------------------------------------------------------------
#############OJO############33
#No es necesario hacer la tabla de población


# Dar formato compatible con bases del IPS
df_pob <- df_pop_state                                              %>% 
    # Desagrupar para poder seleccionar variables sin que meta STATE
    ungroup()                                                       %>% 
    # Seleccionar variables de interés y renombrar 
    select(anio = year, cve_ent = CVE_GEO, pob_tot = population)    %>% 
    # Filtrar y dejar solo años de interés 
    filter(anio %in% c(2010:2025))                                  %>% 
    # Cambiar el identificador de la entidad para que sea compatible con el 
    # resto de bases y así tenga numeración "01" en vez de "1"
    mutate(cve_ent = str_pad(cve_ent, width = 2, pad = "0"))        %>% 
    arrange(anio, cve_ent)                                          %>% 
    filter(anio == 2021)

# Población nacional total
v_pob_nacional <- df_pob$pob_tot[df_pob$cve_ent == "00"]


## 2.1. Fichas por grupos del PIB ----------------------------------------------

# ---- Identificar entidades en cada grupo 
# Identificar entidades según su grupo del PIB
df_registro_pib <- df_ips2022                       %>% 
    distinct(cve_ent, entidad_abr_m, grupo_pib)

# Extraer entidades en cada grupo del PIB
v_pib_alto  <- df_registro_pib$entidad_abr_m[str_detect(df_registro_pib$grupo_pib, "alto")]
v_pib_medio <- df_registro_pib$entidad_abr_m[str_detect(df_registro_pib$grupo_pib, "medio")]
v_pib_bajo  <- df_registro_pib$entidad_abr_m[str_detect(df_registro_pib$grupo_pib, "bajo")]


# ---- Entidades con PIB per cápita alto
df_pib_alto <- df_ips2022           %>%
    filter(
        entidad_abr_m %in% v_pib_alto,  # Dejar solo entidades que pertenecen al grupo
        id == "00"                # Solo IPS
        ) %>% 
    # Obtener estadísticas descriptivas 
    mutate(
        # ent_etiqueta    = paste(), 
        promedio_grupal = mean(value), 
        max_ips         = max(value), 
        min_ips         = min(value)
    ) %>% 
    #left_join(df_pob) %>% 
    #mutate(pob_grupal = sum(pob_tot)) %>% , pob_grupal
    select(
        grupo_pib, anio, cve_ent, entidad_abr_m, value, ips_rank_nacional, 
        promedio_grupal, max_ips, min_ips)


# ---- Entidades con PIB per cápita medio
df_pib_medio <- df_ips2022          %>%
    filter(
        entidad_abr_m %in% v_pib_medio, # Dejar solo entidades que pertenecen al grupo
        id == "00"                # Solo IPS
    ) %>% 
    # Obtener estadísticas descriptivas 
    mutate(
        # ent_etiqueta    = paste(), 
        promedio_grupal = mean(value), 
        max_ips         = max(value), 
        min_ips         = min(value)
    ) %>% 
    #left_join(df_pob) %>% 
    #mutate(pob_grupal = sum(pob_tot)) %>% , pob_grupal
    select(
        grupo_pib, anio, cve_ent, entidad_abr_m, value, ips_rank_nacional, 
        promedio_grupal, max_ips, min_ips)

# ---- Entidades con PIB per cápita alto
df_pib_bajo <- df_ips2022           %>%
    filter(
        entidad_abr_m %in% v_pib_bajo,  # Dejar solo entidades que pertenecen al grupo
        id == "00"                # Solo IPS
    ) %>% 
    # Obtener estadísticas descriptivas 
    mutate(
        # ent_etiqueta    = paste(), 
        promedio_grupal = mean(value), 
        max_ips         = max(value), 
        min_ips         = min(value)
    ) %>% 
    #left_join(df_pob) %>% 
    #mutate(pob_grupal = sum(pob_tot)) %>% , pob_grupal
    select(
        grupo_pib, anio, cve_ent, entidad_abr_m, value, ips_rank_nacional, 
        promedio_grupal, max_ips, min_ips)

# ---- Armar información para fichas 
df_fichas_grupos_pib <- tribble(
    ~grupo    , ~dato      , ~valor, 
    # Grupo de entidades con PIB alto
    "PIB alto" , "Entidades"            , paste(v_pib_alto, sep = ","), 
    "PIB alto" , "IPS promedio"         , unique(df_pib_alto$promedio_grupal), 
    "PIB alto" , "Entidad con mejor IPS", df_pib_alto$entidad_abr_m[df_pib_alto$value == max(df_pib_alto$value)], 
    "PIB alto" , "IPS"                  , df_pib_alto$value[df_pib_alto$value == max(df_pib_alto$value)], 
    "PIB alto" , "Ranking"              , df_pib_alto$ips_rank_nacional[df_pib_alto$value == max(df_pib_alto$value)], 
    "PIB alto" , "Entidad con peor IPS" , df_pib_alto$entidad_abr_m[df_pib_alto$value == min(df_pib_alto$value)], 
    "PIB alto" , "IPS"                  , df_pib_alto$value[df_pib_alto$value == min(df_pib_alto$value)], 
    "PIB alto" , "Ranking"              , df_pib_alto$ips_rank_nacional[df_pib_alto$value == min(df_pib_alto$value)], 
    #"PIB alto" , "Total de habitantes"  , unique(df_pib_alto$pob_grupal),
    #"PIB alto" , "Porcentaje de habitantes", unique(df_pib_alto$pob_grupal)/v_pob_nacional,
    # Grupo de entidades con PIB medio
    "PIB medio", "Entidades"            , paste(v_pib_medio, sep = ","), 
    "PIB medio", "IPS promedio"         , unique(df_pib_medio$promedio_grupal), 
    "PIB medio", "Entidad con mejor IPS", df_pib_medio$entidad_abr_m[df_pib_medio$value == max(df_pib_medio$value)], 
    "PIB medio", "IPS"                  , df_pib_medio$value[df_pib_medio$value == max(df_pib_medio$value)], 
    "PIB medio", "Ranking"              , df_pib_medio$ips_rank_nacional[df_pib_medio$value == max(df_pib_medio$value)], 
    "PIB medio", "Entidad con peor IPS" , df_pib_medio$entidad_abr_m[df_pib_medio$value == min(df_pib_medio$value)], 
    "PIB medio", "IPS"                  , df_pib_medio$value[df_pib_medio$value == min(df_pib_medio$value)], 
    "PIB medio", "Ranking"              , df_pib_medio$ips_rank_nacional[df_pib_medio$value == min(df_pib_medio$value)], 
    #"PIB medio", "Total de habitantes"  , unique(df_pib_medio$pob_grupal),
    #"PIB medio", "Porcentaje de habitantes", unique(df_pib_medio$pob_grupal)/v_pob_nacional,
    # Grupo de entidades con PIB bajo
    "PIB bajo" , "Entidades"            , paste(v_pib_bajo, sep = ","), 
    "PIB bajo" , "IPS promedio"         , unique(df_pib_bajo$promedio_grupal), 
    "PIB bajo" , "Entidad con mejor IPS", df_pib_bajo$entidad_abr_m[df_pib_bajo$value == max(df_pib_bajo$value)], 
    "PIB bajo" , "IPS"                  , df_pib_bajo$value[df_pib_bajo$value == max(df_pib_bajo$value)], 
    "PIB bajo" , "Ranking"              , df_pib_bajo$ips_rank_nacional[df_pib_bajo$value == max(df_pib_bajo$value)], 
    "PIB bajo" , "Entidad con peor IPS" , df_pib_bajo$entidad_abr_m[df_pib_bajo$value == min(df_pib_bajo$value)], 
    "PIB bajo" , "IPS"                  , df_pib_bajo$value[df_pib_bajo$value == min(df_pib_bajo$value)], 
    "PIB bajo" , "Ranking"              , df_pib_bajo$ips_rank_nacional[df_pib_bajo$value == min(df_pib_bajo$value)], 
    #"PIB bajo" , "Total de habitantes"  , unique(df_pib_bajo$pob_grupal),
    #"PIB bajo" , "Porcentaje de habitantes", unique(df_pib_bajo$pob_grupal)/v_pob_nacional,
    
)

# Añadir población y estimar promedios por grupo 

# ---- Guardar base
openxlsx::write.xlsx(df_fichas_grupos_pib, 
                     file = paste_out("02_fichas_grupos_pib.xlsx"))

## 2.2. Diferencias temporales grupos PIB --------------------------------------

# ---- Tema 
tema <- theme_minimal() +
    theme(
        plot.title        = element_text(size = 40, face = "bold", colour = "#6950D8"),
        plot.subtitle     = element_text(size = 35, colour = "#777777", margin=margin(0,0,10,0)),
        plot.margin       = margin(0.3, 0.3, 1.5, 0.3, "cm"), # margin(top,right, bottom,left)
        plot.caption      = element_text(size = 20),
        strip.text.x      = element_text(size = 15),
        panel.grid.minor  = element_blank(),
        panel.background  = element_rect(fill = "transparent",colour = NA),
        text              = element_text(family = "Ubuntu"),
        axis.title.x      = element_blank(),
        axis.title.y      = element_text(size = 25),
        axis.text.x       = element_text(size = 25, angle = 90, vjust = 0.5),
        axis.text.y       = element_text(size = 25),
        legend.text       = element_text(size = 20, family = "Ubuntu"),
        legend.key.size   = unit(0.7, "cm"),
        legend.position   = "top")

# Hacer comparaciones entre:
# 2015 y 2018 
# 2015 y 2021 
# 2018 y 2021
# 2020 y 2021

df_ips_grupos_pib <- df_ips                 %>% 
    # Eliminar datos innecesarios
    filter(
        !(entidad_abr_m %in% c("Utopía", "Distopía")),
        cve_ent != "00")                    %>% 
    # Agegar grupo del PIB al que pertenecen las entidades
    left_join(df_registro_pib)              %>% 
    # Estimar el promedio anual por dimensión y grupo del PIB
    group_by(anio, id_dim, grupo_pib)       %>% 
    summarise(promedio = mean(dim_value))   %>% 
    # Cambiar a formato ancho 
    pivot_wider(
        names_from  = anio, 
        names_prefix = "value_", 
        values_from = promedio 
    ) %>% 
    # Estimar diferencias a través del tiempo 
    mutate(
        diff_2019_2015 = value_2019-value_2015, 
        diff_2022_2015 = value_2022-value_2015,
        diff_2022_2019 = value_2022-value_2019,
        diff_2022_2021 = value_2022-value_2021,
        diff_2021_2019 = value_2021-value_2019,
    ) %>% 
    arrange(grupo_pib, id_dim) %>% 
    select(grupo_pib, id_dim, everything())

# ---- Guardar base
openxlsx::write.xlsx(df_ips_grupos_pib, 
                     file = paste_out("02_cambios_grupos_pib.xlsx"))

# ---- Visualizar diferencias 
# 2015-2019
# 2019-2021
# 2019-2022

# Procesamiento de datos 
df_data <- df_ips_grupos_pib %>% 
    select(grupo_pib, id_dim, starts_with("diff")) %>% 
    pivot_longer(
        cols = -c("grupo_pib", "id_dim"), 
        names_to = "periodo", 
        values_to = "diff") %>% 
    filter(periodo %in% c("diff_2019_2015", "diff_2021_2019",
                          "diff_2022_2021")) %>%
    mutate(
        periodo = case_when(
            periodo == "diff_2019_2015" ~ "Cambio 2015-2019", 
            periodo == "diff_2021_2019" ~ "Cambio 2019-2021",
            periodo == "diff_2022_2021" ~ "Cambio 2021-2022"), 
        grupo_pib = factor(grupo_pib, levels = c("PIB per cápita bajo", 
                                                 "PIB per cápita medio", 
                                                 "PIB per cápita alto"))
        ) %>% 
    filter(id_dim == "00")
    
# ---- Visualización con énfasis en PIB Bajo 
g <- ggplot(
    # Datos
    df_data, 
    # Coordenadas
    aes(x = grupo_pib, y = diff, fill = periodo)) +
    # Geoms
    geom_col(
        position = position_dodge2(), 
        width = 0.4, 
        alpha = if_else(str_detect(df_data$grupo_pib, "alto"), 1, 0.55)) +
    #geom_col(
###        position = position_dodge2(), 
 #       width = 0.4, 
  #      alpha = if_else(str_detect(df_data$grupo_pib, "medio"), 1, 0.55)) +
    geom_text(aes
              (label = if_else(diff < 0, round(diff,2), NA)), 
              position = position_dodge2(0.4), 
              vjust = -1.1, 
              family = "Ubuntu", size = 10, color = "white", fontface = "bold"
    ) +
    geom_text(aes
              (label = if_else(diff > 0, round(diff,2), NA)), 
              position = position_dodge2(0.4), 
              vjust = 1.1, 
              family = "Ubuntu", size = 10, color = "white", fontface = "bold"
    ) +
    # Etiquetas
    labs(
        title    = "Ganancias y pérdidas entre 2015 y 2022", 
        subtitle = "Índice de Progreso Social", 
        y = "Cambio en el puntaje del IPS", 
        fill = ""
    ) +
    # Escalas 
    scale_fill_manual(values = c(mcv_semaforo[1], mcv_semaforo[4], mcv_semaforo[2])) +
    scale_x_discrete(position = "top") +
    scale_y_continuous(limits = c(-2.5, 4), breaks = seq(-2,4,2))+
    # Tema 
    tema +
    theme(
        axis.text.x = element_text(angle = 0), 
        legend.text = element_text(size = 25, family = "Ubuntu"),
        )

# ---- Guardar figura
ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")

ggsave(
    filename = paste_fig("03_01_diferencias_temporales.png"), 
    width = 23, height = 12, dpi = 100)

ggsave(
    filename = paste_fig("03_01_diferencias_temporales.svg"), 
    width = 23, height = 12, dpi = 100)


# ---- Visualización con énfasis en PIB MEDIO 
g <- ggplot(
    # Datos
    df_data, 
    # Coordenadas
    aes(x = grupo_pib, y = diff, fill = periodo)) +
    # Geoms
    geom_col(
        position = position_dodge2(), 
        width = 0.4, 
        alpha = if_else(str_detect(df_data$grupo_pib, "medio"), 1, 0.55)) +
    geom_text(aes
              (label = if_else(diff < 0, round(diff,2), NA)), 
              position = position_dodge2(0.4), 
              vjust = -1.1, 
              family = "Ubuntu", size = 10, color = "white", fontface = "bold"
    ) +
    geom_text(aes
              (label = if_else(diff > 0, round(diff,2), NA)), 
              position = position_dodge2(0.4), 
              vjust = 1.1, 
              family = "Ubuntu", size = 10, color = "white", fontface = "bold"
    ) +
    # Etiquetas
    labs(
        title    = "Ganancias y pérdidas entre 2015 y 2022", 
        subtitle = "Índice de Progreso Social", 
        y = "Cambio en el puntaje del IPS", 
        fill = ""
    ) +
    # Escalas 
    scale_fill_manual(values = c(mcv_semaforo[1], mcv_semaforo[4], mcv_semaforo[2])) +
    scale_x_discrete(position = "top") +
    scale_y_continuous(limits = c(-2.5, 4), breaks = seq(-2,4,2))+
    # Tema 
    tema +
    theme(
        axis.text.x = element_text(angle = 0), 
        legend.text = element_text(size = 25, family = "Ubuntu"),
    )

# ---- Guardar figura
ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")

ggsave(
    filename = paste_fig("03_02_diferencias_temporales.png"), 
    width = 23, height = 12, dpi = 100)

ggsave(
    filename = paste_fig("03_02_diferencias_temporales.svg"), 
    width = 23, height = 12, dpi = 100)


# ---- Visualización con énfasis en PIB ALTO 
g <- ggplot(
    # Datos
    df_data, 
    # Coordenadas
    aes(x = grupo_pib, y = diff, fill = periodo)) +
    # Geoms
    geom_col(
        position = position_dodge2(), 
        width = 0.4, 
        alpha = if_else(str_detect(df_data$grupo_pib, "bajo"), 1, 0.55)) +
    geom_text(aes
              (label = if_else(diff < 0, round(diff,2), NA)), 
              position = position_dodge2(0.4), 
              vjust = -1.1, 
              family = "Ubuntu", size = 10, color = "white", fontface = "bold"
    ) +
    geom_text(aes
              (label = if_else(diff > 0, round(diff,2), NA)), 
              position = position_dodge2(0.4), 
              vjust = 1.1, 
              family = "Ubuntu", size = 10, color = "white", fontface = "bold"
    ) +
    # Etiquetas
    labs(
        title    = "Ganancias y pérdidas entre 2015 y 2022", 
        subtitle = "Índice de Progreso Social", 
        y = "Cambio en el puntaje del IPS", 
        fill = ""
    ) +
    # Escalas 
    scale_fill_manual(values = c(mcv_semaforo[1], mcv_semaforo[4], mcv_semaforo[2])) +
    scale_x_discrete(position = "top") +
    scale_y_continuous(limits = c(-2.5, 4), breaks = seq(-2,4,2))+
    # Tema 
    tema +
    theme(
        axis.text.x = element_text(angle = 0), 
        legend.text = element_text(size = 25, family = "Ubuntu"),
    )

# ---- Guardar figura
ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")

ggsave(
    filename = paste_fig("03_03_diferencias_temporales.png"), 
    width = 23, height = 12, dpi = 100)

ggsave(
    filename = paste_fig("03_03_diferencias_temporales.svg"), 
    width = 23, height = 12, dpi = 100)

# 3. Mapas R -------------------------------------------------------------------

## 3.1. PIB ALTO ---------------------------------------------------------------

# Pegar coordenadas geográficas con datos del IPS
df_mxmap <- mxstate.map         %>% 
    rename(cve_ent = region)    %>%
    left_join(df_pib_alto) 


# Dar formato de geometría 
tempo <- df_mxmap %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    group_by(cve_ent) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") 

# Catálogo de etiquetas por entidad
mxmap_labs <- df_mxmap %>% 
    distinct(cve_ent, entidad_abr_m, value) %>% 
    bind_cols(
        as.data.frame(rgeos::gCentroid(as(tempo, "Spatial"), byid = TRUE))
    )

# ---- Visualización del mapa 
ggplot(
    # Datos
    df_mxmap,
    # Coordenadas
    aes(long, lat, group=group, fill = value)) +
    coord_map() +
    # Polígonos
    geom_polygon(color = "black", size = .2, show.legend = F, alpha = 0.8) +
    geom_label(
        data = mxmap_labs,
        aes(x, y, group = cve_ent,
            label = if_else(
                !(entidad_abr_m %in% v_pib_alto), NA_character_,
                paste0(entidad_abr_m, "\n[", round(value, 1), "]"))
            # label = paste0(entidad_abr_m, "\n[", round(value, 2), "]")
            ), 
        alpha = .4, family = "Ubuntu", show.legend = F) +
    # Etiquetas
    labs(title = unique(df_mxmap$region)) +
    # Escalas
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 65) +
    # Tema
    theme_void() +
    theme(
        plot.title = element_text(size = 40, family = "Ubuntu", face = "bold", 
                                  colour = mcv_blacks[3], hjust = 0.5)) 


# ---- Guardar mapa
ggsave(
    filename = paste0("05_infobites/10_mapas/00-04_ips_pib_alto.png"), 
    width = 23, height = 12, dpi = 200
)

ggsave(
    filename = paste0("05_infobites/10_mapas/00-04_ips_pib_alto.svg"), 
    # device = "png", type = "cairo",  
    width = 23, height = 12, dpi = 200
)


## 3.2 PIB MEDIO ---------------------------------------------------------------

# Pegar coordenadas geográficas con datos del IPS
df_mxmap <- mxstate.map         %>% 
    rename(cve_ent = region)    %>%
    left_join(df_pib_medio) 


# Dar formato de geometría 
tempo <- df_mxmap %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    group_by(cve_ent) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") 

# Catálogo de etiquetas por entidad
mxmap_labs <- df_mxmap %>% 
    distinct(cve_ent, entidad_abr_m, value) %>% 
    bind_cols(
        as.data.frame(rgeos::gCentroid(as(tempo, "Spatial"), byid = TRUE))
    )

# ---- Visualización del mapa 
ggplot(
    # Datos
    df_mxmap,
    # Coordenadas
    aes(long, lat, group=group, fill = value)) +
    coord_map() +
    # Polígonos
    geom_polygon(color = "black", size = .2, show.legend = F, alpha = 0.8) +
    geom_label(
        data = mxmap_labs,
        aes(x, y, group = cve_ent,
            label = if_else(
                !(entidad_abr_m %in% v_pib_medio), NA_character_,
                paste0(entidad_abr_m, "\n[", round(value, 1), "]"))
            # label = paste0(entidad_abr_m, "\n[", round(value, 2), "]")
        ), 
        alpha = .4, family = "Ubuntu", show.legend = F) +
    # Etiquetas
    labs(title = unique(df_mxmap$region)) +
    # Escalas
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 65) +
    # Tema
    theme_void() +
    theme(
        plot.title = element_text(size = 40, family = "Ubuntu", face = "bold", 
                                  colour = mcv_blacks[3], hjust = 0.5)) 


# ---- Guardar mapa
ggsave(
    filename = paste0("05_infobites/10_mapas/00-05_ips_pib_medio.png"), 
    width = 23, height = 12, dpi = 200
)

ggsave(
    filename = paste0("05_infobites/10_mapas/00-05_ips_pib_medio.svg"), 
    # device = "png", type = "cairo",  
    width = 23, height = 12, dpi = 200
)




## 3.3 PIB BAJO ----------------------------------------------------------------

# Pegar coordenadas geográficas con datos del IPS
df_mxmap <- mxstate.map         %>% 
    rename(cve_ent = region)    %>%
    left_join(df_pib_bajo) 


# Dar formato de geometría 
tempo <- df_mxmap %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
    group_by(cve_ent) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON") 

# Catálogo de etiquetas por entidad
mxmap_labs <- df_mxmap %>% 
    distinct(cve_ent, entidad_abr_m, value) %>% 
    bind_cols(
        as.data.frame(rgeos::gCentroid(as(tempo, "Spatial"), byid = TRUE))
    )

# ---- Visualización del mapa 
ggplot(
    # Datos
    df_mxmap,
    # Coordenadas
    aes(long, lat, group=group, fill = value)) +
    coord_map() +
    # Polígonos
    geom_polygon(color = "black", size = .2, show.legend = F, alpha = 0.8) +
    geom_label(
        data = mxmap_labs,
        aes(x, y, group = cve_ent,
            label = if_else(
                !(entidad_abr_m %in% v_pib_bajo), NA_character_,
                paste0(entidad_abr_m, "\n[", round(value, 1), "]"))
            # label = paste0(entidad_abr_m, "\n[", round(value, 2), "]")
        ), 
        alpha = .4, family = "Ubuntu", show.legend = F) +
    # Etiquetas
    labs(title = unique(df_mxmap$region)) +
    # Escalas
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 65) +
    # Tema
    theme_void() +
    theme(
        plot.title = element_text(size = 40, family = "Ubuntu", face = "bold", 
                                  colour = mcv_blacks[3], hjust = 0.5)) 


# ---- Guardar mapa
ggsave(
    filename = paste0("05_infobites/10_mapas/00-06_ips_pib_bajo.png"), 
    width = 23, height = 12, dpi = 200
)

ggsave(
    filename = paste0("05_infobites/10_mapas/00-06_ips_pib_bajo.svg"), 
    # device = "png", type = "cairo",  
    width = 23, height = 12, dpi = 200
)




## 4. Mapas DW -----------------------------------------------------------------

# Crear catálogo de nombres de entidades en ISO para Data Wrapper
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
    "32",		"MX-ZAC")

# ---- Crear base para mapa de PIB alto 
df_mapa_alto <- df_pib_alto             %>% 
    full_join(df_iso, by = "cve_ent")   %>% 
    mutate(
        value = round(value, 1), 
        id_dim = 0, 
        cve_ent = as.numeric(cve_ent))  %>% 
    arrange(cve_ent)                    %>% 
    # Seleccionar con nombres y orden de años anteriores 
    select(
        cve_ent, anio, iso_entidad, id_dim, value, 
        ranking = ips_rank_nacional, entidad_abr_m)

# ---- Crear base para mapa de PIB medio 
df_mapa_medio <- df_pib_medio           %>% 
    full_join(df_iso, by = "cve_ent")   %>% 
    mutate(
        value = round(value, 1), 
        id_dim = 0, 
        cve_ent = as.numeric(cve_ent))  %>% 
    arrange(cve_ent)                    %>% 
    # Seleccionar con nombres y orden de años anteriores 
    select(
        cve_ent, anio, iso_entidad, id_dim, value, 
        ranking = ips_rank_nacional, entidad_abr_m)

# ---- Crear base para mapa de PIB bajo 
df_mapa_bajo <- df_pib_bajo             %>% 
    full_join(df_iso, by = "cve_ent")   %>% 
    mutate(
        value = round(value, 1), 
        id_dim = 0, 
        cve_ent = as.numeric(cve_ent))  %>% 
    arrange(cve_ent)                    %>% 
    # Seleccionar con nombres y orden de años anteriores 
    select(
        cve_ent, anio, iso_entidad, id_dim, value, 
        ranking = ips_rank_nacional, entidad_abr_m)

# ---- Guardar bases en Drive 
# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1oMCDwF8T0pfuuBLMN5BkNS7lBiZF4d0KxDVs76VkPac/edit#gid=0")[1, 2])

# Guardar en la base en el Drive
googlesheets4::range_write(ss = v_id, data = df_mapa_alto , sheet = "12_ing_altos" )
googlesheets4::range_write(ss = v_id, data = df_mapa_medio, sheet = "13_ing_medios")
googlesheets4::range_write(ss = v_id, data = df_mapa_bajo , sheet = "14_ing_bajos" )

#---- Actualizar mapas en Data Wrapper

# Declarar token personal 
dw_token <-  "yx9Rab0qt6TpoEJxJogZsmhn1djSOcaKCGzMifCGl1PRE0rvwqPSaMRQhOV1GAXf" # Regina
datawrapper_auth(api_key = dw_token)

# Actualizar mapas
DatawRappr::dw_publish_chart(chart_id = "UP9Jo", api_key = dw_token) # PIB Alto
DatawRappr::dw_publish_chart(chart_id = "wJHXX", api_key = dw_token) # PIB Medio
DatawRappr::dw_publish_chart(chart_id = "11Yen", api_key = dw_token) # PIB Bajo

# FIN. -------------------------------------------------------------------------

