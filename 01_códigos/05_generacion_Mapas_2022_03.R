
# Librerias ----
library(tidyverse)
library(sf)
library(leaflet)
library(ggpubr)

mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
isDark <- function(colr) { (sum( col2rgb(colr) * c(299, 587,114))/1000 < 123) }

# Datos ----
shp <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson") %>% 
    rename(cve_ent = CVE_EDO) %>% 
    mutate(ctrd_x = (st_centroid(.) %>% st_coordinates())[,1],
           ctrd_y = (st_centroid(.) %>% st_coordinates())[,2])

df_ips = readxl::read_xlsx("03_ips_clean/08_ips_ranking.xlsx")

df_ips

dimensiones <- c("00", "01", "02", "03")
d = "01"

dimensiones_t = tibble(id = c("00", "01", "02", "03"), 
                     nombre_dimensión = c("Índice de Progreso Social", 
                                          "1. Necesidades Humanas Básicas", 
                                          "2. Fundamentos del Bienestar", 
                                          "3. Oportunidades"))

d = "01"
lapply(dimensiones, function(d){
        
        mapa <- shp %>% 
            left_join(df_ips) %>% 
            left_join(dimensiones_t) %>% 
            filter(id == d)
            
        
        # Correccion EDOMEX: 
        # Correccion EDOMEX: 
        mapa$ctrd_x[mapa$ENTIDAD == "México"] <- -99.9
        mapa$ctrd_y[mapa$ENTIDAD == "México"] <- 19.0
        mapa$ctrd_y[mapa$ENTIDAD == "Morelos"] <- 18.6
        mapa$ctrd_y[mapa$ENTIDAD == "Tlaxcala"] <- 19.35
        # Corrección GTO
        mapa$ctrd_y[mapa$ENTIDAD == "Guanajuato"] <- 21.2
        # Corrección PUE
        mapa$ctrd_y[mapa$ENTIDAD == "Puebla"] <- 18.27
        
        paleta <- colorNumeric(domain = mapa$value, 
                               palette = mcv_semaforo %>% rev())
        
        minimapa <- mapa %>% 
            filter(ENTIDAD %in% c("Ciudad de México", 
                                  "México",
                                  "Morelos", 
                                  "Hidalgo", 
                                  "Querétaro de Arteaga", 
                                  "Tlaxcala", 
                                  "Puebla")) %>% 
            mutate(label1 = str_c("[", entidad_abr_m, "]")) %>% 
            mutate(label2 = str_c("",prettyNum(format(round(value, 1), nsmall = 1), big.mark = ",")))
        
        minimapa_box = st_as_sfc(st_bbox(minimapa))
        coord_caja <- minimapa_box %>% 
            st_coordinates() %>% 
            as_data_frame() %>% 
            summarise(minX = min(X), 
                      maxX = max(X), 
                      minY = min(Y), 
                      maxY = max(Y))
        
        mp <- minimapa %>% 
            ggplot(aes(fill = value)) + 
            geom_sf(color = "gray50", 
                    linewidth = 0.2) + 
            geom_text(aes(x = ctrd_x, 
                          y = ctrd_y, 
                          label = label2), 
                      size = 5, 
                      color = "black",
                      family = "Ubuntu", 
                      fontface = "bold") + 
            geom_text(aes(x = ctrd_x, 
                          y = ctrd_y, 
                          label = label1), 
                      vjust = -1.1,
                      size = 5, 
                      color = "black",
                      family = "Ubuntu", 
                      fontface = "bold") +
            scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 62) +
            theme_void() + 
            theme(panel.background = element_rect(fill = "transparent",colour = NA), 
                  panel.border = element_rect(fill = "transparent",
                                              color = mcv_semaforo[1], 
                                              linewidth = 1), 
                  legend.position = "none"
            )
        mp
        
        mg <- mapa %>%
            mutate(label1 = ifelse(ENTIDAD %in% c("Ciudad de México", 
                                                  "México",
                                                  "Morelos", 
                                                  "Hidalgo", 
                                                  "Querétaro de Arteaga", 
                                                  "Tlaxcala", 
                                                  "Puebla"),
                                   no = str_c("[", entidad_abr_m, "]"), 
                                   yes = "")) %>% 
            mutate(label2 = ifelse(ENTIDAD %in% c("Ciudad de México", 
                                                  "México",
                                                  "Morelos", 
                                                  "Hidalgo", 
                                                  "Querétaro de Arteaga", 
                                                  "Tlaxcala", 
                                                  "Puebla"),
                                   no = str_c("",prettyNum(format(round(value, 1), nsmall = 1), big.mark = ",")), 
                                   yes = "")) %>% 
            ggplot(aes(fill = value)) + 
            scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 62) +
            geom_sf(color = "gray50", 
                    linewidth = 0.2) + 
            geom_sf(data = minimapa_box, 
                    color = mcv_semaforo[1], 
                    linewidth = 1,
                    fill = NA) +
            geom_text(aes(x = ctrd_x, 
                          y = ctrd_y, 
                          label = label2), 
                      size = 5, 
                      color = "black",
                      family = "Ubuntu", 
                      fontface = "bold") + 
            geom_text(aes(x = ctrd_x, 
                          y = ctrd_y, 
                          label = label1), 
                      vjust = -1.1,
                      size = 5, 
                      color = "black",
                      family = "Ubuntu", 
                      fontface = "bold") + 
            labs(x = NULL,y = NULL,fill = "Ingreso ") + 
            theme_minimal() +
            theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
                  plot.subtitle = element_text(size = 20, colour = "#777777", margin=margin(0,0,60,0)),
                  plot.margin= margin(1.5, 0.5, 1.5, 0.5, "cm"), # margin(top,right, bottom,left)
                  plot.caption = element_text(size = 20),
                  strip.text.x = element_text(size = 25),
                  panel.grid.minor  = element_blank(),
                  panel.background = element_rect(fill = "transparent",colour = NA),
                  panel.spacing = unit(2, "lines"),
                  text = element_text(family = "Ubuntu"),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank(),
                  # axis.text.x = element_text(size = 20, vjust = 0.5),
                  # axis.text.y = element_text(size = 17),
                  legend.text = element_text(size = 25),
                  axis.text = element_blank(), 
                  panel.grid = element_blank(),
                  legend.position = "none")
        
        
        g_mapa = mg + annotation_custom(ggplotGrob(mp),
                                        xmin = -99.5,
                                        xmax = -84.5, 
                                        ymin = 21.5,
                                        ymax = 29.5)
        
        g_mapa
        
        g_barras <- mapa %>% 
            ggplot(aes(x = reorder(entidad_abr_m, value), 
                       y = value)) + 
            geom_col(fill = paleta(mapa$value)) + 
            geom_col(fill = paleta(mapa$value)) + 
            geom_text(aes(label = str_c(format(round(value, 1), nsmall = 1), "")), 
                      family = "Ubuntu", 
                      hjust = -0.2, 
                      size = 7) + 
            coord_flip() + 
            scale_y_continuous(expand = expansion(c(0, 0.3))) +
            theme_minimal() + 
            theme(panel.grid = element_blank(), 
                  axis.text.x = element_blank(), 
                  axis.title = element_blank(), 
                  axis.text.y = element_text(size = 18),
                  text = element_text(family = "Ubuntu"), 
                  plot.margin= margin(3.7, 0.5, 2, 0.5, "cm") # margin(top,right, bottom,left)
            )
        
        g_mapa = mg + annotation_custom(ggplotGrob(mp),
                                        xmin = -99.5,
                                        xmax = -84.5, 
                                        ymin = 21.5,
                                        ymax = 29.5)
        
        g_plot <- 
            ggarrange(g_mapa, g_barras, widths = c(2.2, 1))
        
        ggsave(filename = str_c("05_infobites/10_mapas/mapas_juve/", d, ".svg"), 
               width = 23, height = 12, dpi = 200)
        
        ggsave(filename = str_c("05_infobites/10_mapas/mapas_juve/", d, ".png"), 
            device = "png",
            type = "cairo",
            width = 23, height = 12, dpi = 200)

})

d = "01"
lapply(c("01", "02", "03", "00"), function(d){
    
    mapa <- shp %>% 
        left_join(df_ips) %>% 
        left_join(dimensiones_t) %>% 
        filter(id == d)
    
    # Correccion EDOMEX: 
    # Correccion EDOMEX: 
    mapa$ctrd_x[mapa$ENTIDAD == "México"] <- -99.9
    mapa$ctrd_y[mapa$ENTIDAD == "México"] <- 19.0
    mapa$ctrd_y[mapa$ENTIDAD == "Morelos"] <- 18.6
    mapa$ctrd_y[mapa$ENTIDAD == "Tlaxcala"] <- 19.35
    # Corrección GTO
    mapa$ctrd_y[mapa$ENTIDAD == "Guanajuato"] <- 21.2
    # Corrección PUE
    mapa$ctrd_y[mapa$ENTIDAD == "Puebla"] <- 18.27
    
    paleta <- colorNumeric(domain = mapa$value, 
                           palette = mcv_semaforo %>% rev())
    
    minimapa <- mapa %>% 
        filter(ENTIDAD %in% c("Ciudad de México", 
                              "México",
                              "Morelos", 
                              "Hidalgo", 
                              "Querétaro de Arteaga", 
                              "Tlaxcala", 
                              "Puebla")) %>% 
        mutate(label1 = str_c("[", entidad_abr_m, "]")) %>% 
        mutate(label2 = str_c("",prettyNum(format(round(value, 1), nsmall = 1), big.mark = ",")))
    
    minimapa_box = st_as_sfc(st_bbox(minimapa))
    coord_caja <- minimapa_box %>% 
        st_coordinates() %>% 
        as_data_frame() %>% 
        summarise(minX = min(X), 
                  maxX = max(X), 
                  minY = min(Y), 
                  maxY = max(Y))
    
    mp <- minimapa %>% 
        ggplot(aes(fill = value)) + 
        geom_sf(color = "gray50", 
                linewidth = 0.2) + 
        geom_text(aes(x = ctrd_x, 
                      y = ctrd_y, 
                      label = label2), 
                  size = 5, 
                  color = "black",
                  family = "Ubuntu", 
                  fontface = "bold") + 
        geom_text(aes(x = ctrd_x, 
                      y = ctrd_y, 
                      label = label1), 
                  vjust = -1.1,
                  size = 5, 
                  color = "black",
                  family = "Ubuntu", 
                  fontface = "bold") +
        scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 62) +
        theme_void() + 
        theme(panel.background = element_rect(fill = "transparent",colour = NA), 
              panel.border = element_rect(fill = "transparent",
                                          color = mcv_semaforo[1], 
                                          linewidth = 1), 
              legend.position = "none"
        )
    mp
    
    mg <- mapa %>%
        mutate(label1 = ifelse(ENTIDAD %in% c("Ciudad de México", 
                                              "México",
                                              "Morelos", 
                                              "Hidalgo", 
                                              "Querétaro de Arteaga", 
                                              "Tlaxcala", 
                                              "Puebla"),
                               no = str_c("[", entidad_abr_m, "]"), 
                               yes = "")) %>% 
        mutate(label2 = ifelse(ENTIDAD %in% c("Ciudad de México", 
                                              "México",
                                              "Morelos", 
                                              "Hidalgo", 
                                              "Querétaro de Arteaga", 
                                              "Tlaxcala", 
                                              "Puebla"),
                               no = str_c("",prettyNum(format(round(value, 1), nsmall = 1), big.mark = ",")), 
                               yes = "")) %>% 
        ggplot(aes(fill = value)) + 
        scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 62) +
        geom_sf(color = "gray50", 
                linewidth = 0.2) + 
        geom_sf(data = minimapa_box, 
                color = mcv_semaforo[1], 
                linewidth = 1,
                fill = NA) +
        geom_text(aes(x = ctrd_x, 
                      y = ctrd_y, 
                      label = label2), 
                  size = 5, 
                  color = "black",
                  family = "Ubuntu", 
                  fontface = "bold") + 
        geom_text(aes(x = ctrd_x, 
                      y = ctrd_y, 
                      label = label1), 
                  vjust = -1.1,
                  size = 5, 
                  color = "black",
                  family = "Ubuntu", 
                  fontface = "bold") + 
        labs(x = NULL,y = NULL,fill = "Ingreso "
             # , 
             # title = mapa$nombre_dimensión[1]
             ) + 
        theme_minimal() +
        theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
              plot.subtitle = element_text(size = 20, colour = "#777777", margin=margin(0,0,60,0)),
              plot.margin= margin(0, 0.5, 1.5, 0.5, "cm"), # margin(top,right, bottom,left)
              plot.caption = element_text(size = 20),
              strip.text.x = element_text(size = 25),
              panel.grid.minor  = element_blank(),
              panel.background = element_rect(fill = "transparent",colour = NA),
              panel.spacing = unit(2, "lines"),
              text = element_text(family = "Ubuntu"),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              # axis.text.x = element_text(size = 20, vjust = 0.5),
              # axis.text.y = element_text(size = 17),
              legend.text = element_text(size = 25),
              axis.text = element_blank(), 
              panel.grid = element_blank(),
              legend.position = "none")
    
    
    g_mapa = mg + annotation_custom(ggplotGrob(mp),
                                    xmin = -99.5,
                                    xmax = -84.5, 
                                    ymin = 21.5,
                                    ymax = 29.5)
    
    g_mapa
    
    g_barras <- mapa %>% 
        ggplot(aes(x = reorder(entidad_abr_m, value), 
                   y = value)) + 
        geom_col(fill = paleta(mapa$value)) + 
        geom_col(fill = paleta(mapa$value)) + 
        geom_text(aes(label = str_c(format(round(value, 1), nsmall = 1), "")), 
                  family = "Ubuntu", 
                  hjust = -0.2, 
                  size = 7) + 
        coord_flip() + 
        scale_y_continuous(expand = expansion(c(0, 0.3))) +
        theme_minimal() + 
        theme(panel.grid = element_blank(), 
              axis.text.x = element_blank(), 
              axis.title = element_blank(), 
              axis.text.y = element_text(size = 18),
              text = element_text(family = "Ubuntu"), 
              plot.margin= margin(0.8, 0.5, 2, 0.5, "cm") # margin(top,right, bottom,left)
        )
    
    
    g_mapa = mg + annotation_custom(ggplotGrob(mp),
                                    xmin = -99.5,
                                    xmax = -84.5, 
                                    ymin = 21.5,
                                    ymax = 29.5)
    
    g_plot <- 
        ggarrange(g_mapa, g_barras, widths = c(2.2, 1))
    
    g_plot <- 
        annotate_figure(
            g_plot, 
            top = text_grob(
                str_c("\n", mapa$nombre_dimensión[1], ", 2022"),
                # paste0(titulo, "\n", subtitulo), 
                size = 45, face = "bold", 
                family = "Ubuntu", color = "#6950D8", 
                vjust = 0.6
            )
        )
    
    g_plot <- g_plot %>% 
        ggimage::ggbackground("05_infobites/00_plantillas/00_IPS.pdf")
    
    
    ggsave(filename = str_c("05_infobites/10_mapas/mapas_juve/", d, "_plantilla.svg"), 
           width = 23, height = 12, dpi = 200)
    
    ggsave(filename = str_c("05_infobites/10_mapas/mapas_juve/", d, "_plantilla.png"), 
           device = "png",
           type = "cairo",
           width = 23, height = 12, dpi = 200)
    
})


# Otros mapas ----
## 1. IED ----
cat_edos <- df_ips %>% 
    select(cve_ent,entidad_abr_m) %>% 
    unique()
datos <- tibble::tribble(
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
    left_join(cat_edos)

mapa <- shp %>% 
    left_join(datos)

# Correccion EDOMEX: 
mapa$ctrd_x[mapa$ENTIDAD == "México"] <- -99.9
mapa$ctrd_y[mapa$ENTIDAD == "México"] <- 19.0
mapa$ctrd_y[mapa$ENTIDAD == "Morelos"] <- 18.6
mapa$ctrd_y[mapa$ENTIDAD == "Tlaxcala"] <- 19.35

# Corrección GTO
mapa$ctrd_y[mapa$ENTIDAD == "Guanajuato"] <- 21.2
# Corrección PUE
mapa$ctrd_y[mapa$ENTIDAD == "Puebla"] <- 18.27

############### COLORES @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# RColorBrewer::brewer.pal(9, "BuPu")
 # "#F7FCFD" "#E0ECF4" "#BFD3E6" "#9EBCDA" "#8C96C6" "#8C6BB1" "#88419D" "#810F7C" "#4D004B"

colores_seleccionados <- c( "#9EBCDA", "#8C96C6","#88419D","#810F7C")
paleta <- colorNumeric(domain = mapa$value, 
                       palette = colores_seleccionados, 
                       reverse = F)
color_linea <- "white"
############### COLORES @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# colores_gradiente <- RColorBrewer::brewer.pal(n = 9, name = colores_seleccionados)

minimapa <- mapa %>% 
    filter(ENTIDAD %in% c("Ciudad de México", 
                          "México",
                          "Morelos", 
                          "Hidalgo", 
                          "Querétaro de Arteaga", 
                          "Tlaxcala", 
                          "Puebla")) %>% 
    mutate(label1 = str_c("[", entidad_abr_m, "]")) %>% 
    mutate(label2 = str_c("",prettyNum(format(round(value, 1), nsmall = 1), big.mark = ",")))

minimapa_box = st_as_sfc(st_bbox(minimapa))
coord_caja <- minimapa_box %>% 
    st_coordinates() %>% 
    as_data_frame() %>% 
    summarise(minX = min(X), 
              maxX = max(X), 
              minY = min(Y), 
              maxY = max(Y))

mp <- minimapa %>% 
    ggplot(aes(fill = value)) + 
    geom_sf(color = color_linea, 
            linewidth = 0.3, 
            fill = paleta(minimapa$value)) + 
    geom_text(aes(x = ctrd_x, 
                  y = ctrd_y, 
                  label = str_c(label2, "%")), 
              size = 5, 
              color = ifelse(sapply(paleta(minimapa$value), isDark), 
                                     "white", "black"),
              family = "Ubuntu", 
              fontface = "bold") + 
    geom_text(aes(x = ctrd_x, 
                  y = ctrd_y, 
                  label = label1), 
              vjust = -1.1,
              size = 4, 
              color = ifelse(sapply(paleta(minimapa$value), isDark), 
                             "white", "black"),
              family = "Ubuntu", 
              fontface = "bold") +
    # scale_fill_gradientn(colors = paleta(minimapa$value)) +
    theme_void() + 
    theme(panel.background = element_rect(fill = "transparent",colour = NA), 
          panel.border = element_rect(fill = "transparent",
                                      color = mcv_semaforo[1], 
                                      linewidth = 1), 
          legend.position = "none")
mp

mg <- mapa %>%
    mutate(label1 = ifelse(ENTIDAD %in% c("Ciudad de México", 
                                          "México",
                                          "Morelos", 
                                          "Hidalgo", 
                                          "Querétaro de Arteaga", 
                                          "Tlaxcala", 
                                          "Puebla"),
                           no = str_c("[", entidad_abr_m, "]"), 
                           yes = "")) %>% 
    mutate(label2 = ifelse(ENTIDAD %in% c("Ciudad de México", 
                                          "México",
                                          "Morelos", 
                                          "Hidalgo", 
                                          "Querétaro de Arteaga", 
                                          "Tlaxcala", 
                                          "Puebla"),
                           no = str_c("",prettyNum(format(round(value, 1), nsmall = 1), big.mark = ",")), 
                           yes = "")) %>% 
    ggplot(aes(fill = value)) + 
    # scale_fill_gradientn(colors = paleta(sort(mapa$value))) +
    geom_sf(color = color_linea, 
            fill = paleta(mapa$value),
            linewidth = 0.3) +
    geom_sf(data = minimapa_box, 
            color = mcv_semaforo[1], 
            linewidth = 1,
            fill = NA) +
    geom_text(aes(x = ctrd_x, 
                  y = ctrd_y, 
                  label = str_c(label2, ifelse(label2 == "", 
                                               "", "%"))), 
              size = 5, 
              color = ifelse(sapply(paleta(mapa$value), isDark), 
                             "white", "black"),
              family = "Ubuntu", 
              fontface = "bold") + 
    geom_text(aes(x = ctrd_x, 
                  y = ctrd_y, 
                  label = label1), 
              vjust = -1.1,
              size = 5.5, 
              color = ifelse(sapply(paleta(mapa$value), isDark), 
                             "white", "black"),
              family = "Ubuntu", 
              fontface = "bold") + 
    labs(x = NULL,y = NULL,fill = "Ingreso ", 
         title = "Porcentaje de la Inversión Extranjera Directa\npor Entidad Federativa, 2022") + 
    theme_minimal() +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 20, colour = "#777777", margin=margin(0,0,60,0)),
          plot.margin= margin(1.5, 0.5, 1.5, 0.5, "cm"), # margin(top,right, bottom,left)
          plot.caption = element_text(size = 20),
          strip.text.x = element_text(size = 25),
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          panel.spacing = unit(2, "lines"),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title.position = "plot",
          # axis.text.x = element_text(size = 20, vjust = 0.5),
          # axis.text.y = element_text(size = 17),
          legend.text = element_text(size = 25),
          axis.text = element_blank(), 
          panel.grid = element_blank(),
          legend.position = "none")

# barras <- 

g_barras <- mapa %>% 
    ggplot(aes(x = reorder(entidad_abr_m, value), 
               y = value)) + 
    geom_col(fill = paleta(mapa$value)) + 
    geom_col(fill = paleta(mapa$value)) + 
    geom_text(aes(label = str_c(format(round(value, 1), nsmall = 1), "%")), 
              family = "Ubuntu", 
              hjust = -0.2, 
              size = 7) + 
    coord_flip() + 
    scale_y_continuous(expand = expansion(c(0, 0.3))) +
    theme_minimal() + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_blank(), 
          axis.title = element_blank(), 
          axis.text.y = element_text(size = 18),
          text = element_text(family = "Ubuntu"), 
          plot.margin= margin(3.7, 0.5, 2, 0.5, "cm") # margin(top,right, bottom,left)
          )

g_mapa = mg + annotation_custom(ggplotGrob(mp),
                                xmin = -99.5,
                                xmax = -84.5, 
                                ymin = 21.5,
                                ymax = 29.5)

g_plot <- 
    ggarrange(g_mapa, g_barras, widths = c(2.2, 1))


g_plot <- g_plot %>% 
    ggimage::ggbackground("05_infobites/00_plantillas/02_BASE.pdf")

ggsave(filename = str_c("05_infobites/10_mapas/mapas_juve/", "ied", ".svg"), 
       width = 23, height = 12, dpi = 200)

ggsave(filename = str_c("05_infobites/10_mapas/mapas_juve/", "ied", ".png"), 
       device = "png",
       type = "cairo",
       width = 23, height = 12, dpi = 200)



## 2. Anuncios ----
cat_edos <- df_ips %>% 
    select(cve_ent,entidad_abr_m) %>% 
    unique()

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

mapa <- shp %>% 
    left_join(lugares_anuncios_ie_abs) %>% 
    left_join(cat_edos)

# Correccion EDOMEX: 
mapa$ctrd_x[mapa$ENTIDAD == "México"] <- -99.9
mapa$ctrd_y[mapa$ENTIDAD == "México"] <- 19.0
mapa$ctrd_y[mapa$ENTIDAD == "Morelos"] <- 18.6
mapa$ctrd_y[mapa$ENTIDAD == "Tlaxcala"] <- 19.35

# Corrección GTO
mapa$ctrd_y[mapa$ENTIDAD == "Guanajuato"] <- 21.2
# Corrección PUE
mapa$ctrd_y[mapa$ENTIDAD == "Puebla"] <- 18.5

############### COLORES @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# RColorBrewer::brewer.pal(9, "BuPu")
# "#F7FCFD" "#E0ECF4" "#BFD3E6" "#9EBCDA" "#8C96C6" "#8C6BB1" "#88419D" "#810F7C" "#4D004B"

RColorBrewer::brewer.pal(n = 9, "YlOrRd")
# "white", "#FFFFCC" "#FFEDA0" "#FED976" "#FEB24C" "#FD8D3C" "#FC4E2A" "#E31A1C" "#BD0026" "#800026"

colores_seleccionados <- c("white", "#FFFFCC",
                           "#FFEDA0", "#FED976", 
                           "#FEB24C" ,"#FD8D3C",
                           "#FC4E2A" ,"#E31A1C" ,
                           "#BD0026", "#800026")
paleta <- colorNumeric(domain = mapa$value, 
                       palette = colores_seleccionados, 
                       reverse = F)
color_linea <- "gray60"
############### COLORES @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# colores_gradiente <- RColorBrewer::brewer.pal(n = 9, name = colores_seleccionados)

minimapa <- mapa %>% 
    filter(ENTIDAD %in% c("Ciudad de México", 
                          "México",
                          "Morelos", 
                          "Hidalgo", 
                          "Querétaro de Arteaga", 
                          "Tlaxcala", 
                          "Puebla")) %>% 
    mutate(label1 = str_c("[", entidad_abr_m, "]")) %>% 
    mutate(label2 = str_c("",prettyNum(format(round(value, 0), nsmall = 0), big.mark = ",")))

minimapa_box = st_as_sfc(st_bbox(minimapa))
coord_caja <- minimapa_box %>% 
    st_coordinates() %>% 
    as_data_frame() %>% 
    summarise(minX = min(X), 
              maxX = max(X), 
              minY = min(Y), 
              maxY = max(Y))

mp <- minimapa %>% 
    ggplot(aes(fill = value)) + 
    geom_sf(color = color_linea, 
            linewidth = 0.3, 
            fill = paleta(minimapa$value)) + 
    geom_text(aes(x = ctrd_x, 
                  y = ctrd_y, 
                  label = str_c(label2, "")), 
              size = 5, 
              color = ifelse(sapply(paleta(minimapa$value), isDark), 
                             "white", "black"),
              family = "Ubuntu", 
              fontface = "bold") + 
    geom_text(aes(x = ctrd_x, 
                  y = ctrd_y, 
                  label = label1), 
              vjust = -1.1,
              size = 4, 
              color = ifelse(sapply(paleta(minimapa$value), isDark), 
                             "white", "black"),
              family = "Ubuntu", 
              fontface = "bold") +
    # scale_fill_gradientn(colors = paleta(minimapa$value)) +
    theme_void() + 
    theme(panel.background = element_rect(fill = "transparent",colour = NA), 
          panel.border = element_rect(fill = "transparent",
                                      color = mcv_semaforo[1], 
                                      linewidth = 1), 
          legend.position = "none")
mp

mg <- mapa %>%
    mutate(label1 = ifelse(ENTIDAD %in% c("Ciudad de México", 
                                          "México",
                                          "Morelos", 
                                          "Hidalgo", 
                                          "Querétaro de Arteaga", 
                                          "Tlaxcala", 
                                          "Puebla"),
                           no = str_c("[", entidad_abr_m, "]"), 
                           yes = "")) %>% 
    mutate(label2 = ifelse(ENTIDAD %in% c("Ciudad de México", 
                                          "México",
                                          "Morelos", 
                                          "Hidalgo", 
                                          "Querétaro de Arteaga", 
                                          "Tlaxcala", 
                                          "Puebla"),
                           no = str_c("",prettyNum(format(round(value, 0), nsmall = 0), big.mark = ",")), 
                           yes = "")) %>% 
    ggplot(aes(fill = value)) + 
    # scale_fill_gradientn(colors = paleta(sort(mapa$value))) +
    geom_sf(color = color_linea, 
            fill = paleta(mapa$value),
            linewidth = 0.3) +
    geom_sf(data = minimapa_box, 
            color = mcv_semaforo[1], 
            linewidth = 1,
            fill = NA) +
    geom_text(aes(x = ctrd_x, 
                  y = ctrd_y, 
                  label = str_c(label2, ifelse(label2 == "", 
                                               "", ""))), 
              size = 5, 
              color = ifelse(sapply(paleta(mapa$value), isDark), 
                             "white", "black"),
              family = "Ubuntu", 
              fontface = "bold") + 
    geom_text(aes(x = ctrd_x, 
                  y = ctrd_y, 
                  label = label1), 
              vjust = -1.1,
              size = 5.5, 
              color = ifelse(sapply(paleta(mapa$value), isDark), 
                             "white", "black"),
              family = "Ubuntu", 
              fontface = "bold") + 
    labs(x = NULL,y = NULL,fill = "Ingreso ", 
         title = "Anuncios de proyectos de inversión por entidad federativa", 
         subtitle = "Mayo 2022 a Octubre 2023") + 
    theme_minimal() +
    theme(plot.title = element_text(size = 40, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 35, colour = "#777777", margin=margin(0,0,60,0)),
          plot.margin= margin(0.8, 0.5, 1.5, 0.5, "cm"), # margin(top,right, bottom,left)
          plot.caption = element_text(size = 20),
          strip.text.x = element_text(size = 25),
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          panel.spacing = unit(2, "lines"),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title.position = "plot",
          # axis.text.x = element_text(size = 20, vjust = 0.5),
          # axis.text.y = element_text(size = 17),
          legend.text = element_text(size = 25),
          axis.text = element_blank(), 
          panel.grid = element_blank(),
          legend.position = "none")

mg
# barras <- 

g_barras <- mapa %>% 
    ggplot(aes(x = reorder(entidad_abr_m, value), 
               y = value)) + 
    geom_col(fill = paleta(mapa$value)) + 
    geom_col(fill = paleta(mapa$value)) + 
    geom_text(aes(label = str_c(format(round(value, 0), nsmall = 0), "")), 
              family = "Ubuntu", 
              hjust = -0.2, 
              size = 7) + 
    coord_flip() + 
    scale_y_continuous(expand = expansion(c(0, 0.3))) +
    theme_minimal() + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_blank(), 
          axis.title = element_blank(), 
          axis.text.y = element_text(size = 18),
          text = element_text(family = "Ubuntu"), 
          plot.margin= margin(3.7, 0.5, 2, 0.5, "cm") # margin(top,right, bottom,left)
    )

g_mapa = mg + annotation_custom(ggplotGrob(mp),
                                xmin = -99.5,
                                xmax = -84.5, 
                                ymin = 21.5,
                                ymax = 29.5)

g_plot <- 
    ggarrange(g_mapa, g_barras, widths = c(2.2, 1))


g_plot <- g_plot %>% 
    ggimage::ggbackground("05_infobites/00_plantillas/03_integralia.pdf")

ggsave(filename = str_c("05_infobites/10_mapas/mapas_juve/", "anuncios_inv", ".svg"), 
       width = 23, height = 12, dpi = 200)

ggsave(filename = str_c("05_infobites/10_mapas/mapas_juve/", "anuncios_inv", ".png"), 
       device = "png",
       type = "cairo",
       width = 23, height = 12, dpi = 200)

# Función mapas con variables ----
# datos_ips_long <- readxl::read_xlsx("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx")
# gen_mapa_con_variables <- function(var, anio){
#         
# }

## 03 Mapas por region ----
# Mapas por grupo de PIB per cápita: 
# categorias_pib_pc <- mapx$grupo_pib %>% unique()
# # mapx
# lapply(categorias_pib_pc,
#        function(cat){
#            
#            bd_mapa <- mapx %>% 
#                filter(id == "00") 
#            
#            bd_mapa %>%
#                select(ENTIDAD, value)
#            
#            min(bd_mapa$value)
#            max(bd_mapa$value)
#            
#            bd_mapa$entidad_abr_m
#            
#            bd_mapa %>% 
#                filter(grupo_pib == cat) %>% 
#                ggplot(aes(fill = value)) + 
#                geom_sf(data = shp, fill = "gray60") + 
#                geom_sf(color = "white") + 
#                ggrepel::geom_text_repel(aes(x = x, y = y, 
#                               label = str_c(entidad_abr_m, "\n", 
#                                             "[", format(round(value, 1),
#                                                                   nsmall = 1), "]")),
#                               nudge_x = c(-5, 20),
#                               # nudge_y = c(93, 110),
#                               direction = "x",
#                          family = "Ubuntu") + 
#                scale_fill_gradientn(colors = mcv_semaforo %>% rev(), 
#                                     limits = c(min(bd_mapa$value),
#                                                max(bd_mapa$value)))
#            
# })
