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
google_token <- "AQUÍ VA TU TOKEN"

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1859408845")[1, 2])
1
# Función para importar de manera más corta desde drive
imp_dv <- function(x, y){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x), sheet = y)}

# 1. Consolidación de bd ----
anio_vec <- 2015:2020
ips_wide <- data.frame()
ips_long <- data.frame()
ips_direccion <- data.frame()
for(x in 5:6){
    
    print(
        paste0(
            "Proceso para ",
            anio_vec[x]
        )
    )
    
    ips_complete <- data.frame()
    for(i in 3:57){
        
        ips_tempo <- imp_dv(v_id, i) %>% 
            mutate_at(
                vars(c(contains("value"),contains("anio"))),
                ~as.numeric(str_remove_all(as.character(.),"[[:blank:]]"))
            ) %>% 
            mutate_at(
                vars(c(starts_with("id"), starts_with("cve"))),
                ~as.character(str_pad(.,2,"l","0"))
            ) %>% 
            mutate(
                anio_dist = anio-anio_vec[x]
            ) %>% 
            filter(anio_dist<=0) %>% 
            mutate(anio_dist = abs(anio_dist)) %>% 
            filter(anio_dist == min(anio_dist)) %>% 
            select(-contains("drop"))
        
        
        ips_complete <- bind_rows(ips_complete, ips_tempo)
        
        #Sys.sleep(5) 
        
    }
    
    ips_complete <- ips_complete %>% 
        select(-contains("drop")) %>% 
        select(-contains("dist"))
    
    ips_long <- bind_rows(
        ips_long, 
        ips_complete %>% 
            mutate(anio = anio_vec[x])
    ) 
    
    # 2. Direcciones ----
    ips_uto_disto_discrecionales <- imp_dv(v_id, 2) %>% 
        select(id_dimension:direccion,utopia,distopia)
    
    ips_direccion_tempo <- ips_complete %>% 
        left_join(ips_uto_disto_discrecionales) %>% 
        mutate(
            indicador_value_abs = indicador_value*direccion
        )
    
    ips_direccion <- bind_rows(ips_direccion, ips_direccion_tempo)
    
    ips_wide_tempo <- ips_direccion_tempo %>% 
        arrange(desc(anio)) %>% 
        distinct(cve_ent, id_dimension, indicador_value_abs, .keep_all = T) %>% 
        select(cve_ent:indicador_value_abs) %>% 
        drop_na(indicador_value_abs) %>% 
        mutate(indicador_value_abs = round(indicador_value_abs,4)) %>% 
        mutate(id_unica = paste0("ind_",id_dimension, id_indicador)) %>% 
        select(cve_ent, entidad_abr_m, id_unica, indicador_value_abs) %>% 
        arrange(id_unica) %>% 
        pivot_wider(names_from = id_unica, values_from = indicador_value_abs) %>% 
        mutate(anio = anio_vec[x]) %>% 
        select(cve_ent, anio, everything()) %>% 
        glimpse 
    
    ips_wide <- bind_rows(
        ips_wide, ips_wide_tempo
    )
    
    
}

ips_direccion <- ips_direccion %>% 
    select(-contains("topia")) %>% 
    left_join(
        imp_dv(v_id, 2) %>% 
            select(id_dimension:id_indicador,utopia,distopia)
    )

openxlsx::write.xlsx(ips_long, "03_ips_clean/01_ips_long.xlsx")
openxlsx::write.xlsx(ips_wide, "03_ips_clean/02_ips_wide.xlsx")
openxlsx::write.xlsx(ips_direccion, "03_ips_clean/03_ips_direccion.xlsx")

ips_long <- readxl::read_excel("03_ips_clean/01_ips_long.xlsx")
ips_wide <- readxl::read_excel("03_ips_clean/02_ips_wide.xlsx")
ips_direccion <- readxl::read_excel("03_ips_clean/03_ips_direccion.xlsx")


# 3. Estadística descriptiva ----
## 3.1. Estadísticos ----
ips_stats <- ips_wide %>% 
    psych::describe(quant=c(.25,.5,.75,1)) %>%
    as_tibble(rownames="indicador")  %>%
    print()

openxlsx::write.xlsx(ips_stats, "03_ips_clean/04_ips_stats.xlsx")

ips_stats <- readxl::read_excel("03_ips_clean/04_ips_stats.xlsx")

## 3.2. Histogramas ----
plot(hist(ips_wide$ind_0114[ips_wide$cve_ent!="00"]))


# 4. Utopías y distopías ----
utop_distop_long <- ips_direccion %>% 
    select(starts_with("id"), ends_with("topia"), direccion) %>% 
    mutate(indicador = paste0("ind_",id_dimension, id_indicador)) %>%
    distinct(indicador, utopia, distopia, direccion, .keep_all = T) %>% 
    left_join(
        ips_stats %>% 
            filter(str_starts(indicador, "ind")) %>% 
            select(indicador, min, max, sd)
    ) %>% 
    mutate(
        utopia_final = case_when(
            is.na(utopia) & direccion == -1 ~ max+sd,
            is.na(utopia) & direccion == 1 ~ max+sd,
            !(is.na(utopia)) & direccion == -1 & utopia>0 ~ utopia*direccion,
            !(is.na(utopia)) & direccion == 1 & utopia>0 ~ utopia*direccion,
            T ~ utopia
        ),
        distopia_final = case_when(
            is.na(distopia) & direccion == -1 ~ min-sd,
            is.na(distopia) & direccion == 1 ~ min-sd,
            !(is.na(distopia)) & direccion == -1 & distopia>0 ~ distopia*direccion,
            !(is.na(distopia)) & direccion == 1 & distopia>0 ~ distopia*direccion,
            T ~ distopia
        )
        
    ) %>% 
    mutate(id_unica = paste0("ind_",id_dimension, id_indicador)) %>% 
    select(id_unica, utopia_final, distopia_final) %>% 
    distinct(id_unica, .keep_all = T) %>% 
    arrange(id_unica)

openxlsx::write.xlsx(utop_distop_long, "03_ips_clean/05_ips_utop_distop_long.xlsx")


utop_distop_wide <- bind_cols(
    tribble(~cve_ent, ~entidad_abr_m,
            "98", "Utopía",
            "99", "Distopía"),
    t(utop_distop_long$utopia_final) %>% 
        as_data_frame() %>% 
        bind_rows(
            t(utop_distop_long$distopia_final) %>% 
                as_data_frame() 
        )
)

names(utop_distop_wide)[3:57] <- unique(utop_distop_long$id_unica)

openxlsx::write.xlsx(utop_distop_wide, "03_ips_clean/06_ips_utop_distop_wide.xlsx")

ips_wide <- ips_wide %>% 
    bind_rows(
        utop_distop_wide
    )


# 5. Normalización ----
ips_wide_norm <- bind_cols(
    ips_wide[1:3],
    as.data.frame(scale(ips_wide[4:58])) 
)


openxlsx::write.xlsx(ips_wide_norm, paste0("03_ips_clean/07_ips_wide_norm_complete.xlsx"))


# 6. Alfas de Cronbach y KMO ----
ids_comp <- imp_dv(v_id, 2) %>% 
    select(id_dimension:direccion,utopia,distopia) %>% 
    mutate(id_comp = paste0(id_dimension, id_indicador, "_", id_componente, "comp")) %>% 
    select(id_comp) %>% 
    dplyr::distinct(id_comp) %>% 
    filter(!id_comp=="0231_08comp") %>% 
    glimpse

ips_wide_norm <- ips_wide_norm %>% 
    select(-ind_0231)

names(ips_wide_norm)[4:57] <- ids_comp$id_comp

ips_comp <- ips_wide_norm %>% 
    select(ends_with("comp")) 

ids_comp_vec <- str_sub(ids_comp$id_comp, -6) %>% 
    as_tibble() %>%
    distinct() 

ids_comp_vec <- as.character(ids_comp_vec$value)

alphas <- list()
kmos <- list()

for(i in 1:length(ids_comp_vec)){
    
    tempo <- ips_comp %>% 
        select(ends_with(ids_comp_vec[i]))
    
    alpha_tempo <- psych::alpha(tempo, check.keys=TRUE)
    kmo_tempo <- psych::KMO(tempo)
    
    
    alphas[i] <- list(alpha_tempo)
    kmos[i] <- list(kmo_tempo)
    
    
    
    
}


# Output alphas y KMO
for(i in 1:12){
    print(paste0("COMPONENTE ", i))
    print(alphas[[i]])
    print(kmos[[i]])
    
}

# 7. Estimación de factores ----
drop_names <- unique(paste0(str_sub(ids_comp$id_comp,1,2), str_sub(ids_comp$id_comp,-7)))
coefs <- data.frame()
valores_esperados <- ips_wide[1:3]
valores_esperados_norm <- ips_wide[1:3]
for(i in 1:length(ids_comp_vec)){
    
    tempo <- ips_comp %>% 
        select(ends_with(ids_comp_vec[i]))
    
    
    coefs_tempo <- 
    fa(tempo, nfactors = 1, scores="regression", rotate = "varimax")$weights %>% 
        as_data_frame() %>% 
        rename(coefs = MR1) %>% 
        mutate(id_comp = str_pad(i, 2, "l", "0"))
    
    tempo_fac <- fa(tempo, nfactors = 1, scores="regression", rotate = "varimax")$scores %>% 
        as_data_frame()
    
    valores_esperados_tempo <- tempo_fac %>% 
        rename_all(~paste0(drop_names[i]))
    
    valores_esperados_norm_tempo <- 
        scales::rescale(as.numeric(tempo_fac$MR1), to = c(0,100)) %>% 
            as_data_frame() %>% 
            rename_all(~paste0(drop_names[i]))
    
    coefs <- bind_rows(coefs, coefs_tempo)
    valores_esperados <- bind_cols(valores_esperados, valores_esperados_tempo)
    valores_esperados_norm <- bind_cols(valores_esperados_norm, valores_esperados_norm_tempo)
    
    
}



rm(drop_names)

ips_final <- valores_esperados_norm %>% 
    select(cve_ent, anio, entidad_abr_m, starts_with("0")) %>% 
    pivot_longer(
        starts_with("0"),
        names_to = "id_comp"
    )  %>% 
    mutate(id_dim = str_sub(id_comp,1,2)) %>% 
    group_by(cve_ent, anio, entidad_abr_m, id_dim) %>% 
    summarise(dim_value = mean(value)) %>% 
    bind_rows(
        
        valores_esperados_norm %>% 
            select(cve_ent, anio, entidad_abr_m, starts_with("0")) %>% 
            pivot_longer(
                starts_with("0"),
                names_to = "id_comp"
            )  %>% 
            mutate(id_dim = str_sub(id_comp,1,2)) %>% 
            group_by(cve_ent, anio, entidad_abr_m, id_dim) %>% 
            summarise(dim_value = mean(value)) %>% 
            group_by(cve_ent, anio, entidad_abr_m) %>% 
            summarise(dim_value = mean(dim_value)) %>% 
            mutate(id_dim = "00")
        
    ) %>% 
    arrange(cve_ent, anio)




# Guardar base larga
wb = createWorkbook()
addWorksheet(wb, "01_IPS_DIM")
addWorksheet(wb, "02_IPS_COMP")
writeData(
    wb    = wb, 
    sheet = 1, 
    x = ips_final, 
    colNames = T, 
    rowNames = F
)

writeData(
    wb    = wb, 
    sheet = 2, 
    x = valores_esperados_norm %>% 
        pivot_longer(
            ends_with("comp"),
            names_to = "id_comp",
            values_to = "comp_value"
        ), 
    colNames = T, 
    rowNames = F
)

saveWorkbook(wb, "03_ips_clean/00_IPS_COMPLETE_LONG.xlsx", overwrite = T)

# Guardar base ancha
wb = createWorkbook()
addWorksheet(wb, "01_IPS_DIM")
addWorksheet(wb, "02_IPS_COMP")
writeData(
    wb    = wb, 
    sheet = 1, 
    x = ips_final %>% 
        pivot_wider(
            names_from = id_dim,
            values_from = dim_value
        ) %>% 
        arrange(anio, cve_ent), 
    colNames = T, 
    rowNames = F
)

writeData(
    wb    = wb, 
    sheet = 2, 
    x = valores_esperados_norm, 
    colNames = T, 
    rowNames = F
)

saveWorkbook(wb, "03_ips_clean/00_IPS_COMPLETE_WIDE.xlsx", overwrite = T)




# 8. Infobites ----
ips_final <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx")
# Colores MCV
mcv_discrete <- c("#6950d8", "#00b783", "#ff6260", "#ffaf84", "#ffbd41", "#3CEAFA")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados

## 1. Heatmaps ----
### 1.0. IPS ----
titulo <- "Índice de progreso social"
subtitulo <- "2015 - 2020"
eje_x <- ""
eje_y <- "Años"

g <- 
ggplot(data = 
           ips_final %>% 
           filter(!as.numeric(cve_ent) > 33) %>% 
           filter(id_dim == "00") %>% 
           mutate(dim = case_when(
               id_dim == "00" ~ "0. Índice de Progreso Social",
               id_dim == "01" ~ "1. Necesidades Humanas Básicas",
               id_dim == "02" ~ "2. Fundamentos del Bienestar",
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

ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/00_IPS_heatmap.png", width = 23, height = 12, dpi = 100)

### 1.1. Necesidades Humanas Básicas  ----
titulo <- "Dim 1. Necesidades Humanas Básicas"
subtitulo <- "2015 - 2020"
eje_x <- ""
eje_y <- "Años"

g <- 
    ggplot(data = 
               ips_final %>% 
               filter(!as.numeric(cve_ent) > 33) %>% 
               filter(id_dim == "01") %>% 
               mutate(dim = case_when(
                   id_dim == "00" ~ "0. Índice de Progreso Social",
                   id_dim == "01" ~ "1. Necesidades Humanas Básicas",
                   id_dim == "02" ~ "2. Fundamentos del Bienestar",
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

ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/01_NHB_heatmap.png", width = 23, height = 12, dpi = 100)

### 1.2. Fundamentos del Bienestar ----
titulo <- "Dim 2. Fundamentos del Bienestar"
subtitulo <- "2015 - 2020"
eje_x <- ""
eje_y <- "Años"

g <- 
    ggplot(data = 
               ips_final %>% 
               filter(!as.numeric(cve_ent) > 33) %>% 
               filter(id_dim == "02") %>% 
               mutate(dim = case_when(
                   id_dim == "00" ~ "0. Índice de Progreso Social",
                   id_dim == "01" ~ "1. Necesidades Humanas Básicas",
                   id_dim == "02" ~ "2. Fundamentos del Bienestar",
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

ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/02_SB_heatmap.png", width = 23, height = 12, dpi = 100)

### 1.3. Oportunidades ----
titulo <- "Dim 3. Oportunidades"
subtitulo <- "2015 - 2020"
eje_x <- ""
eje_y <- "Años"

g <- 
    ggplot(data = 
               ips_final %>% 
               filter(!as.numeric(cve_ent) > 33) %>% 
               filter(id_dim == "03") %>% 
               mutate(dim = case_when(
                   id_dim == "00" ~ "0. Índice de Progreso Social",
                   id_dim == "01" ~ "1. Necesidades Humanas Básicas",
                   id_dim == "02" ~ "2. Fundamentos del Bienestar",
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

ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/03_OP_heatmap.png", width = 23, height = 12, dpi = 100)

## 2. Sankeys (rankings) ----
d <- ips_final %>% 
    filter(!as.numeric(cve_ent) > 33) %>% 
    filter(!cve_ent == "00") %>% 
    group_by(anio, id_dim) %>% 
    arrange(desc(dim_value), .by_group = T) %>% 
    mutate(ranking = 1:32,
           ranking = str_pad(ranking, 2, "l", "0")) %>% 
    ungroup()

### 2.0. IPS ----
titulo <- "Ranking del Índice de Progreso Social"
subtitulo <- "2015 - 2020"
eje_x <- "Años"
eje_y <- "Ranking"

g <- 
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
ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/04_IPS_sankey.png", width = 23, height = 12, dpi = 100)

### 2.1. NHB ----
titulo <- "Ranking de la Dim 1. Necesidades Humanas Básicas"
subtitulo <- "2015 - 2020"
eje_x <- "Años"
eje_y <- "Ranking"

g <- 
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
ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/05_NHB_sankey.png", width = 23, height = 12, dpi = 100)

### 2.2. SB ----
titulo <- "Ranking de la Dim 2. Fundamentos del Bienestar"
subtitulo <- "2015 - 2020"
eje_x <- "Años"
eje_y <- "Ranking"

g <- 
    ggplot(
        d %>% 
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
ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/06_SB_sankey.png", width = 23, height = 12, dpi = 100)


### 2.3. OP ----
titulo <- "Ranking de la Dim 3. Oportunidades"
subtitulo <- "2015 - 2020"
eje_x <- "Años"
eje_y <- "Ranking"

g <- 
    ggplot(
        d %>% 
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
ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/07_OP_sankey.png", width = 23, height = 12, dpi = 100)

## 3. Scores por entidades ----
cves <- unique(ips_final$cve_ent)[1:33]

for(i in 1:33){
    
    a <- ips_final  %>% 
        mutate(dim = case_when(
            id_dim == "00" ~ "0. Índice de Progreso Social",
            id_dim == "01" ~ "1. Necesidades Humanas Básicas",
            id_dim == "02" ~ "2. Fundamentos del Bienestar",
            T ~ "3. Oportunidades"
        )) %>% 
        filter(cve_ent == cves[i]) %>% 
        glimpse
    
    titulo <- "Puntaje en el IPS y dimensiones"
    subtitulo <- paste0(unique(a$entidad_abr_m), " | 2015 - 2020")
    eje_x <- "Años"
    eje_y <- "Puntaje"
    
    g <- 
        ggplot(
            a,
            aes(
                x = anio,
                y = dim_value,
                group = 1,
                col = id_dim
            )
        ) +
        geom_line(size = 3.7, alpha = 0.7, show.legend = F) +
        geom_point(size = 6.7)  +
        geom_text(
            aes(label = round(dim_value,1)), 
            vjust = -1.5, size = 4, fontface = "bold", family = "Ubuntu", show.legend = F
        ) +
        facet_wrap(~dim)  +
        scale_color_manual(values = mcv_discrete)+
        ylim(30,90)+
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
            #panel.grid.major.x = element_blank(),
            panel.background   = element_rect(fill = "transparent", colour = NA),
            text               = element_text(family = "Ubuntu"),
            axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
            axis.text.x        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
            axis.text.y        = element_text(family = "Ubuntu", size = 15, colour = "#777777"),
            legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
            legend.position    = "none") 
    ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
    ggsave(filename = paste0("05_infobites/01_entidades/", unique(a$cve_ent), "_", unique(a$entidad_abr_m), ".png"), width = 23, height = 12, dpi = 100)
    
    
}


## 4. Componentes ----
ips_comp <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx", sheet = 2) %>% 
    left_join(
        readxl::read_excel("02_datos_crudos/00_diccionario_componentes.xlsx")
    ) %>%
    select(ends_with("comp"), everything()) %>% 
    glimpse

id_comp_vec <- unique(ips_comp$id_comp)


### 4.1. Heatmaps ----
for(i in 1:length(id_comp_vec)){
    
    a <- ips_comp %>% 
        filter(id_comp == id_comp_vec[i])
    
    titulo <- paste0(
        str_replace_all(
            str_replace_all(
                str_remove_all(unique(a$id_comp), "0"), "_", "."
            ), "comp", ". "
        ),
        unique(a$name_comp)
    )
    
    subtitulo <- "2015 - 2020"
    eje_x <- ""
    eje_y <- "Años"
    
    g <- 
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
    
    ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
    ggsave(filename = paste0("05_infobites/02_componentes/01_heatmaps/", str_remove_all(unique(a$id_comp), "comp"), "_", str_replace_all(tolower(unique(a$name_comp)), " ", "_"), ".png"), 
           width = 23, height = 12, dpi = 100)
    
}

### 4.2. Sankeys (rankings) ----
for(i in 1:length(id_comp_vec)){
    
    a <- ips_comp %>% 
        filter(id_comp == id_comp_vec[i]) %>% 
        filter(!as.numeric(cve_ent) > 33) %>% 
        filter(!cve_ent == "00") %>% 
        group_by(anio) %>% 
        arrange(desc(comp_value), .by_group = T) %>% 
        mutate(ranking = 1:32,
               ranking = str_pad(ranking, 2, "l", "0")) %>% 
        ungroup()
    
    titulo <- paste0(
        "Ranking del componente ",
        str_replace_all(
            str_replace_all(
                str_remove_all(unique(a$id_comp), "0"), "_", "."
            ), "comp", ". "
        ),
        unique(a$name_comp)
    )
    subtitulo <- "2015 - 2020"
    eje_x <- "Años"
    eje_y <- "Ranking"
    
    g <- 
        ggplot(
            a, 
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
    ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
    ggsave(filename = paste0("05_infobites/02_componentes/02_sankeys/", str_remove_all(unique(a$id_comp), "comp"), "_", str_replace_all(tolower(unique(a$name_comp)), " ", "_"), ".png"), 
           width = 23, height = 12, dpi = 100)
    
}

#####
ips_cambios_2019_2020 <- ips_final %>% 
    filter(id_dim == "01", anio >2018)%>% 
    filter(!as.numeric(cve_ent) > 33) %>% 
    pivot_wider(
        names_from = "anio",
        names_prefix = "anio_",
        values_from = "dim_value"
    ) %>% 
    mutate(
        dif = anio_2020-anio_2019
    )

# 5. Transformación para Omar ----
ips_final <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx") %>% 
    glimpse




openxlsx::write.xlsx(
    ips_final %>% 
        filter(id_dim=="00", as.numeric(cve_ent) < 33) %>% 
        select(anio, entidad_abr_m, value = dim_value) %>% 
        pivot_wider(
            names_from = entidad_abr_m, values_from = value
        ),
    "03_ips_clean/09_data_web/00_ips.xlsx"
)

openxlsx::write.xlsx(
    ips_final %>% 
        filter(id_dim=="01", as.numeric(cve_ent) < 33) %>% 
        select(anio, entidad_abr_m, value = dim_value) %>% 
        pivot_wider(
            names_from = entidad_abr_m, values_from = value
        ),
    "03_ips_clean/09_data_web/01_nhb.xlsx"
)

openxlsx::write.xlsx(
    ips_final %>% 
        filter(id_dim=="02", as.numeric(cve_ent) < 33) %>% 
        select(anio, entidad_abr_m, value = dim_value) %>% 
        pivot_wider(
            names_from = entidad_abr_m, values_from = value
        ),
    "03_ips_clean/09_data_web/02_fb.xlsx"
)

openxlsx::write.xlsx(
    ips_final %>% 
        filter(id_dim=="03", as.numeric(cve_ent) < 33) %>% 
        select(anio, entidad_abr_m, value = dim_value) %>% 
        pivot_wider(
            names_from = entidad_abr_m, values_from = value
        ),
    "03_ips_clean/09_data_web/03_op.xlsx"
)
