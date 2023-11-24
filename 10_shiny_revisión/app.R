library(tidyverse)
library(googlesheets4)
library(gargle)
library(shinycssloaders)
library(sf)
library(shinyWidgets)
library(leaflet)
library(ggpubr)
library(pdftools)
library(DT)
library(ggtext)

# Funciones propias 
isDark <- function(colr) { (sum( col2rgb(colr) * c(299, 587,114))/1000 < 123) }
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")

# Carga de los datos 
# googlesheets4::gs4_auth(cache = ".secrets",
#                         email = "juvenal@mexicocomovamos.mx")
# options(gargle_oauth_cache = ".secrets")
# gargle::gargle_oauth_cache()
# list.files(".secrets/")

# Iniciar sesión con tu cuenta de google o descargar archivo y cargarlo con readxl::read_xlsx
id_excel = "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1148109682"
hojas <- googlesheets4::sheet_names(id_excel)
metadatos <- googlesheets4::read_sheet(id_excel, sheet = "00_directorio")

# Verificando las claves: 
# datos2 = datos %>% 
#   do.call(rbind, .) 
cat_edos <- readxl::read_xlsx("catalogo_estatal.xlsx")

opciones_edos <- cat_edos %>% 
  slice(1:33) %>% 
  pull(entidad_abr_m)

names(opciones_edos) = cat_edos$entidad[1:33]

opciones_indicador <- metadatos$id_indicador
names(opciones_indicador) <- str_c(metadatos$id_indicador, "-", metadatos$indicador)

# edo_sel = "Todos los estados"
# ind_sel = "01"

get_datos <- function(ind_sel){
  h <- hojas[str_detect(hojas, pattern = str_c("\\_", ind_sel, "\\_"))]
  datos_seleccionados <- googlesheets4::read_sheet(id_excel,
                            sheet = h) %>%
    select(anio,	cve_ent,	entidad_abr_m,
           id_dimension,	id_indicador,	indicador_value) %>%
    mutate(anio = as.numeric(anio),
           cve_ent = as.character(cve_ent),
           id_dimension = as.numeric(id_dimension),
           id_indicador = as.numeric(id_indicador),
           indicador_value = as.numeric(indicador_value)
    )
  return(datos_seleccionados)
}

# edo_sel = "MOR"
# datos_sel = get_datos("01")
gen_grafica <- function(edo_sel, datos_sel){
  
  if(sum(str_detect(edo_sel, "Todos los estados")) > 0){
    edo_sel = opciones_edos
  }
  
  titulo = metadatos %>% 
    filter(id_indicador %in% str_pad(unique(datos_sel$id_indicador), width = 2, side = "left", pad = "0")) %>% 
    pull(indicador)
  
  bd_plt = datos_sel %>% 
    mutate(color = ifelse(entidad_abr_m == "Nacional", 
                          yes = "Nacional", 
                          no = "Otros estados")) %>% 
    filter(entidad_abr_m %in% edo_sel)
  
  plt = bd_plt %>% 
    ggplot(aes(x = anio, 
               y = indicador_value, 
               group = entidad_abr_m, 
               color = color, 
               linewidth = color)) +
    scale_x_continuous(breaks = bd_plt$anio %>% unique() %>% sort()) + 
    scale_color_manual(values = c("Nacional" = "gold",
                                  "Otros estados" = "gray")) +
    scale_discrete_manual(aesthetics = "linewidth", 
                          values = c("Nacional" = 2,
                                     "Otros estados" = 0.5)) + 
    geom_line() + 
    geom_point() + 
    labs(color = "Entidad: ", 
         title = titulo, 
         y = NULL, 
         x = NULL) +
    theme_minimal() + 
    theme(text = element_text(family ="Ubuntu"), 
          legend.position = "bottom", 
          plot.title.position = "plot", 
          axis.text.x = element_text(angle = 90),
          plot.title = element_text(hjust = 0.5, 
                                    color = "purple", 
                                    face = "bold"))
  plotly::ggplotly(plt)
}

# edo_sel = "GRO"
# datos_sel = get_datos("01")
gen_grafica_ggplot <- function(edo_sel, datos_sel, grises = TRUE){
  
  if(sum(str_detect(edo_sel, "Todos los estados")) > 0){
    edo_sel = opciones_edos
  }
  
  meta_sel <- metadatos %>% 
    filter(id_indicador %in% str_pad(unique(datos_sel$id_indicador), width = 2, side = "left", pad = "0"))
  
  titulo = metadatos %>% 
    filter(id_indicador %in% str_pad(unique(datos_sel$id_indicador), width = 2, side = "left", pad = "0")) %>% 
    pull(indicador)
  
  unidad = ifelse((!is.na(meta_sel$unidad) & meta_sel$unidad == "Porcentaje"), 
                           yes = "%", no = "")
  
  bd_plt = datos_sel %>% 
    mutate(edo_seleccionado = ifelse(entidad_abr_m %in% c(edo_sel, "Nacional"), 
                                     yes = entidad_abr_m, 
                                     no = "Otros estados"))
  bd_plt = bd_plt %>% 
    mutate(entidad_abr_m = factor(entidad_abr_m, 
                                  levels = c("Nacional", edo_sel,
                                             unique(bd_plt$entidad_abr_m)[!(unique(bd_plt$entidad_abr_m) %in% c("Nacional", edo_sel))]
                                             ) %>% rev())) %>% 
    mutate(ancho_linea = ifelse(edo_seleccionado == "Otros estados", 
                                yes = "linea_delgada", 
                                no = "linea_gruesa")) %>% 
    mutate(is.nacional = ifelse(entidad_abr_m == "Nacional", yes = "nac", no = "otros"))
  
  if(length(edo_sel) <= 5){
    colores_edo_sel = mcv_discrete[1:length(edo_sel)]
    names(colores_edo_sel) <- edo_sel
  } else {
    colores_edo_sel = rep(mcv_discrete[4] , length(edo_sel))
    names(colores_edo_sel) <- edo_sel
  }
  
  colores_escala = c("Nacional" = "orange", 
                     "Otros estados" = "gray90", 
                     colores_edo_sel)
  
  suma_angulo = ifelse(unique(bd_plt$anio) %>% length() > 13, 
                       yes = 90, 
                       no = 0)
  
  plt = bd_plt %>% 
    # {if (grises != TRUE){filter(bd_plt, ancho_linea == "linea_gruesa" )}} %>% 
      ggplot(aes(x = anio, 
                 y = indicador_value, 
                 group = entidad_abr_m, 
                 color = edo_seleccionado, 
                 linewidth = ancho_linea, 
                 linetype = is.nacional)) +
    geom_line() + 
      geom_point(size = 5, 
                 pch = 21, 
                 stroke = 2,
                 fill = "white") + 
      # geom_text(data = bd_plt %>%
      #             filter(anio %in% c(min(anio), max(anio))) %>% 
      #             filter(ancho_linea == "linea_delgada"),
      #           aes(label = entidad_abr_m, 
      #               hjust = factor(anio)),
      #           family = "Ubuntu", 
      #           show.legend = F
      #           ) +
      ggrepel::geom_text_repel(data = bd_plt %>%
                  filter(anio %in% c(min(anio), max(anio))) %>% 
                  filter(ancho_linea == "linea_gruesa"),
                aes(label = entidad_abr_m, 
                    hjust = factor(anio)),
                family = "Ubuntu", 
                fontface = "bold",
                # label.padding = unit(0.3, "lines"),
                direction = "y", 
                show.legend = F, 
                size = 10
      ) +
      scale_discrete_manual(aesthetics = "hjust", values = c(1.4, -0.4)) + 
      scale_discrete_manual(aesthetics = "linetype", values = c("otros" = 1,"nac" = 2)) + 
      scale_x_continuous(breaks = bd_plt$anio %>% unique() %>% sort(), 
                         expand = expansion(0.1, 0.1)) + 
      scale_y_continuous(labels = scales::comma_format(suffix = unidad)) +
      scale_color_manual(values = colores_escala) +
      scale_discrete_manual(aesthetics = "linewidth",
                            values = c("linea_gruesa" = 3,
                                       "linea_delgada" = 1.5), 
                            guide = NULL) +
      labs(color = "Entidad: ", 
           title = titulo, 
           y = NULL, 
           x = NULL) +
      theme_minimal() + 
      theme(text = element_text(family ="Ubuntu"), 
            legend.position = "none", 
            plot.title.position = "plot", 
            panel.grid.minor = element_blank(), 
            panel.grid.major = element_line(linetype = 2, 
                                            linewidth = 0.9),
            plot.margin= margin(1.6, 1, 2.5, 1, "cm"), # margin(top,right, bottom,left)
            axis.text.x = element_text(angle = suma_angulo, vjust = 0.5, size = 30),
            axis.text.y = element_text(angle = 0, vjust = 0.5, size = 30),
            legend.text = element_text(size = 30), 
            legend.title = element_text(size = 30), 
            plot.title = element_text(hjust = 0, 
                                      size = 50,
                                      color = mcv_discrete[1], 
                                      face = "bold"))
  plt
  return(plt)
}

# Gen mapa ----
# gen_anios_disponibles <- function(datos, ind_sel){
#   datos$anio %>% unique() %>% sort()
# }

# ind_sel = "02"
# datos <- get_datos("02")
# anio_sel = 2015
# edos_marcar = c("AGS", "Nacional")

gen_mapa_vars_ips <- function(ind_sel, anio_sel, datos, 
                              edos_marcar = NULL){
  meta_sel <- metadatos %>% 
    filter(id_indicador == ind_sel)
  
  unidad = ifelse((!is.na(meta_sel$unidad) & meta_sel$unidad == "Porcentaje"), 
                  yes = "%", no = "")
  sentido_barras <- meta_sel$direccion
  
  # metadatos$unidad
  
  shp <- read_sf("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/geojsons/Division%20Politica/DivisionEstatal.geojson") %>% 
    rename(cve_ent = CVE_EDO) %>% 
    mutate(ctrd_x = (st_centroid(.) %>% st_coordinates())[,1],
           ctrd_y = (st_centroid(.) %>% st_coordinates())[,2])
  
  mapa <- shp %>% 
    left_join(datos %>% filter(anio == anio_sel)) %>% 
    rename(value = indicador_value)
  
  # Correccion EDOMEX: 
  mapa$ctrd_x[mapa$ENTIDAD == "México"] <- -99.9
  mapa$ctrd_y[mapa$ENTIDAD == "México"] <- 19.1
  mapa$ctrd_y[mapa$ENTIDAD == "Morelos"] <- 18.6
  mapa$ctrd_y[mapa$ENTIDAD == "Tlaxcala"] <- 19.35
  
  # Corrección GTO
  mapa$ctrd_y[mapa$ENTIDAD == "Guanajuato"] <- 21
  # Corrección PUE
  mapa$ctrd_y[mapa$ENTIDAD == "Puebla"] <- 18.27
  
  ############### COLORES @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  # RColorBrewer::brewer.pal(9, "BuPu")
  # "#F7FCFD" "#E0ECF4" "#BFD3E6" "#9EBCDA" "#8C96C6" "#8C6BB1" "#88419D" "#810F7C" "#4D004B"
  
  colores_seleccionados <- mcv_semaforo
  # c( "#9EBCDA", "#8C96C6","#88419D","#810F7C")
  
  reversa <- ifelse(meta_sel$direccion == 1, yes = T, no = F)
  
  paleta <- colorNumeric(domain = mapa$value, 
                         palette = colores_seleccionados, 
                         reverse = reversa)
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
                  label = str_c(label2, unidad)), 
              size = 5, 
              color = ifelse(sapply(paleta(minimapa$value), isDark), 
                             "black", "black"),
              family = "Ubuntu", 
              fontface = "bold") + 
    geom_text(aes(x = ctrd_x, 
                  y = ctrd_y, 
                  label = label1), 
              vjust = -1.1,
              size = 4, 
              color = ifelse(sapply(paleta(minimapa$value), isDark), 
                             "black", "black"),
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
                                               "", unidad))), 
              size = 4, 
              color = ifelse(sapply(paleta(mapa$value), isDark), 
                             "black", "black"),
              family = "Ubuntu", 
              fontface = "bold") + 
    geom_text(aes(x = ctrd_x, 
                  y = ctrd_y, 
                  label = label1), 
              vjust = -1.1,
              size = 4.5, 
              color = ifelse(sapply(paleta(mapa$value), isDark), 
                             "black", "black"),
              family = "Ubuntu", 
              fontface = "bold") + 
    labs(x = NULL,y = NULL,fill = "Ingreso ", 
         title = str_c(meta_sel$indicador, ", ", anio_sel) %>% 
           str_wrap(60)) + 
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
  
  datos <- datos %>% 
    rename(value = indicador_value) %>% 
    filter(anio == anio_sel) %>% 
    mutate(fill_barra = ifelse(entidad_abr_m %in% edos_marcar, 
                               yes = paleta(value),
                               no = "gray50")) %>% 
    mutate(entidad_abr_m = ifelse(entidad_abr_m == "Nacional", 
                                  yes = "<b style = 'color:#ff6260'>Nacional</b>", 
                                  no = entidad_abr_m))
  
  g_barras <- 
    datos %>% 
    ggplot(aes(x = reorder(entidad_abr_m,
                           sentido_barras*value),y = value)) + 
    # geom_col(fill =
    #            paleta(datosx$value)) 
    geom_col(fill = datos$fill_barra) +
    geom_text(aes(label = str_c(format(round(value, 1), nsmall = 1), unidad)), 
              family = "Ubuntu", 
              hjust = -0.2, 
              size = 7) + 
    coord_flip() + 
    scale_y_continuous(expand = expansion(c(0.1, 0.35))) +
    theme_minimal() + 
    theme(panel.grid = element_blank(), 
          axis.text.x = element_blank(), 
          axis.title = element_blank(), 
          axis.text.y = element_markdown(size = 18, hjust = 1),
          text = element_text(family = "Ubuntu"), 
          plot.margin= margin(4.7, 0.5, 2, 0.5, "cm") # margin(top,right, bottom,left)
    )
  
  g_mapa = mg + annotation_custom(ggplotGrob(mp),
                                  xmin = -99.5,
                                  xmax = -84.5, 
                                  ymin = 21.5,
                                  ymax = 29.5)
  
  g_plot <- 
    ggarrange(g_mapa, g_barras, widths = c(2.2, 1))
  return(g_plot)
}

datos_long <- readxl::read_xlsx("01_Datos/08_02_ips_ranking_series.xlsx")
abb_edos_control <- datos_long %>% select(cve_ent, entidad_abr_m) %>% unique()
abb_control <- abb_edos_control$cve_ent
names(abb_control) <- abb_edos_control$entidad_abr_m


gen_tabla_peores <- function(edo_sel){
  datos_filt <- datos_long %>% 
    group_by(id) %>% 
    filter(anio == max(anio)) %>% 
    filter(cve_ent == edo_sel) %>% 
    arrange(-ips_rank_nacional) %>% 
    select(`Clave entidad` = cve_ent,
           `Abr. Entidad` = entidad_abr_m,
           `Nivel` = nivel, 
           `ID` = id,
           `Nombre del indicador` = name, 
           `Valor` = value, 
           `Lugar nacional` = ips_rank_nacional)
}


# SHINY ----

library(shiny)

ui <- fluidPage(
  tags$head(
    tags$style(
      "
      @import url('https://fonts.googleapis.com/css2?family=Ubuntu:ital,wght@0,400;0,500;0,700;1,300;1,400;1,500;1,700&display=swap');
      "
    )
  ),
  h1("Análisis del IPS"), 
  
  tabsetPanel(
    
    tabPanel("Mapa y barra", 
      wellPanel(
        fluidRow(
          column(4, selectInput("selIndicadorMapa", "Seleccione indicador", choices = opciones_indicador)), 
          column(4, uiOutput("selAnioMapa")), 
          column(4, 
                 uiOutput("selEdosMarcar"),
                 downloadButton("descargarGrafica", 
                                   label = "Descargar gráfica con formato MCV"))
        )
      ), 
      br(), 
      plotOutput("mapa_barra", width = "100%", height = "110vh") %>% withSpinner()
    ),
    tabPanel("Desempeño individual estados", 
     p("Esta tabla permite ver, ordenados del peor al mejor, el lugar que ocupa cada entidad en cada uno de los indicadores que conforman el IPS. "),
     wellPanel(
        fluidRow(
          column(6, 
                 selectInput("selEstado2", 
                             "Seleccione estado", 
                             choices = c(abb_control), 
                             selected = abb_control[1],
                             multiple = F)
          )
        )       
      ), br(), 
     DT::DTOutput("tabla_peores")
    ),
    tabPanel("Series de tiempo", 
             wellPanel(
               fluidRow(
                 column(4, 
                        selectInput("selIndicador", 
                                    "Seleccione indicador", 
                                    choices = opciones_indicador)
                 ), 
                 column(4, 
                        selectInput("selEstado", 
                                    "Seleccione estado", 
                                    choices = c("Todos los estados", 
                                                opciones_edos), 
                                    multiple = T, 
                                    selected = "Todos los estados")
                 ), 
                 column(4, 
                        downloadButton("descargarGrafica2", 
                                       label = "Descargar gráfica con formato MCV"))
               )), 
             br(), 
             fluidRow(
               column(12, 
                      plotly::plotlyOutput("grafica_lineas", height = "90vh") %>% withSpinner(), 
                      br(), 
                      plotOutput("grafica_lineas2", height = "90vh") %>% withSpinner()
               )
             )
        )
  )
)

server <- function(input, output, session) {
  
  traer_datos <- reactive({get_datos(ind_sel = input$selIndicador)})
  traer_datos_mapa <- reactive({get_datos(ind_sel = input$selIndicadorMapa)})
  
  edos_marcar <- reactive({unique(traer_datos_mapa()$entidad_abr_m) %>% sort()})
  
  output$selEdosMarcar <- renderUI({
    selectInput("selEdosMarcar", 
                "Estados a marcar: ", 
                choices = c("Todos los estados", edos_marcar()), 
                selected = "Todos los estados", 
                multiple = T)
  })
  
  output$descargarGrafica <- downloadHandler(
    filename = function(){
      # "grafica.png"
      str_c(input$selIndicadorMapa, "_",input$selAnioMapa,  ".png")
    },
    content = function(file){
      
      plot_print = gen_mapa_vars_ips(ind_sel = input$selIndicadorMapa,
                        anio_sel = input$selAnioMapa, 
                        datos = traer_datos_mapa()) %>%
        ggimage::ggbackground("00_plantillas/00_IPS.pdf")
      ggsave(file, plot = plot_print, device = "png", 
             width = 23, height = 12, dpi = 200)
    }
  )
  
  output$mapa_barra <- renderPlot({
    if(sum("Todos los estados" %in% input$selEdosMarcar) > 0){
      edos_a_marcar = edos_marcar()
    } else {
      edos_a_marcar = input$selEdosMarcar
    }
      
    gen_mapa_vars_ips(ind_sel = input$selIndicadorMapa,
                      anio_sel = input$selAnioMapa, 
                      datos = traer_datos_mapa(), 
                      edos_marcar = edos_a_marcar)
  })
  
  output$selAnioMapa <- renderUI({
    sliderTextInput(inputId = "selAnioMapa", 
                label = "Seleccione año", 
                choices = unique(traer_datos_mapa() %>% pull(anio) %>% sort()), 
                selected = max(unique(traer_datos_mapa() %>% pull(anio) %>% sort())),
                grid = T
                )
  })
  
  output$grafica_lineas <- plotly::renderPlotly({
    print(input$selEstado)
    gen_grafica(edo_sel = input$selEstado,
                datos_sel = traer_datos())
  })
  
  output$grafica_lineas2 <- renderPlot({
    print(input$selEstado)
    gen_grafica_ggplot(edo_sel = input$selEstado,
                datos_sel = traer_datos())
  })
  
  output$descargarGrafica2 <- downloadHandler(
    filename = function(){
      # "grafica.png"
      str_c(input$selEstado, "_",input$selIndicador,".png")
    },
    content = function(file){
      plot_print = gen_grafica_ggplot(edo_sel = input$selEstado,
                                      datos_sel = traer_datos()) %>%
        ggimage::ggbackground("00_plantillas/00_IPS.pdf")
      ggsave(file, plot = plot_print, device = "png", 
             width = 23, height = 12, dpi = 200)
    }
  )
  
  output$tabla_peores <- DT::renderDT({
    gen_tabla_peores(input$selEstado2) %>% 
      DT::datatable(extensions = 'FixedColumns',escape = FALSE,rownames= FALSE,
                    options = list(
                      pageLength = 10,language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
                      autoWidth = FALSE,scrollX = TRUE
                    ))
  })
}

shinyApp(ui, server)
