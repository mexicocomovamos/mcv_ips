# 0. Configuración inicial -----------------------------------------------------

options(scipen=999)
Sys.setlocale("LC_TIME", "es_ES")

## 0.1. Paquetes ---------------------------------------------------------------

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
if(!require("mxmaps")) install.packages("mxmaps") & require("mxmaps")
if(!require("ggrepel")) install.packages("ggrepel") & require("ggrepel")

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

##############################
EDICION <- 2024
# De todos modos verificar que todo cambie 
##############################


# 1. Consolidación de bd -------------------------------------------------------
anio_vec <- 2015:2024
ips_wide <- data.frame()
ips_long <- data.frame()
ips_direccion <- data.frame()

# prueba <- imp_dv(x = v_id, y = 3)
# lapply(prueba$cve_ent, class)
# sum(lapply(prueba$cve_ent, length) %>% as.numeric() != 1)
# prueba %>% 
#     group_by(entidad_abr_m) %>% 
#     count() %>% 
#     print(n = Inf)
# unique(prueba$id_indicador)
# prueba$indicador_value

# x = 3
for(x in 1:length(anio_vec)){
    print(
        paste0(
            "Proceso para ",
            anio_vec[x]
        )
    )
    ips_complete <- data.frame()
    # i = 3
    # TENER CUIDADO EN LA LONGITUD DEL BUCLE
    # LE SUMAMOS DOS PARA VERIFICAR QUE APAREZCAN TODAS LAS VARIABLES EN LA BASE!!!
    
    # i = 3
    for(i in hojas_i[-c(1,2)]){
    # i = hojas[5]
    # for(i in hojas){
        print(i)
        ips_tempo <-
            # read_sheet(id_excel, sheet = i) %>% 
            readxl::read_excel("02_datos_crudos/00_IPS_bd.xlsx", sheet = i) %>%
            mutate(cve_ent = as.character(cve_ent)) %>%
            mutate(cve_ent = str_remove_all(cve_ent, "\\.\\d+")) %>%
            # imp_dv(v_id, i) %>%
            mutate_at(
                vars(c(contains("value"),contains("anio"))),
                ~as.numeric(str_remove_all(as.character(.),"[[:blank:]]"))
            ) %>%
            mutate_at(
                vars(c(starts_with("id"), starts_with("cve"))),
                ~as.character(str_pad(.,2,"left","0"))
            ) %>%
            mutate(
                anio_dist = anio-anio_vec[x]
            ) %>%
            filter(anio_dist<=0) %>%
            mutate(anio_dist = abs(anio_dist)) %>%
            filter(anio_dist == min(anio_dist)) %>%
            select(-contains("drop"))
        ips_complete <- bind_rows(ips_complete, ips_tempo) %>%
            as_tibble()
        # is.na(ips_complete$indicador_value) %>% table()
        # %>%
        #     as_tibble() %>%
        #     mutate(cve_ent = str_remove_all(cve_ent, pattern = "\\.0"))
        # Sys.sleep(2.5)
    }
    ips_complete <- ips_complete %>%
        select(-contains("drop")) %>%
        select(-contains("dist"))
    ips_long <- bind_rows(
        ips_long,
        ips_complete %>%
            mutate(anio = anio_vec[x], 
                   id_dim_ind = paste0(id_dimension, id_indicador) %>% as.character()) %>%
            select(cve_ent, anio, entidad_abr_m, id_dim_ind, indicador_value) %>%
            drop_na(id_dim_ind)
    )
    
    ips_wide_tempo <- ips_complete %>%
        left_join(
            readxl::read_excel("02_datos_crudos/00_IPS_bd.xlsx", sheet = 2) %>%
                # imp_dv(v_id, 2) %>%
                select(id_dimension:direccion, utopia, distopia)
        ) %>%
        mutate(cve_ent = as.character(cve_ent)) %>%
        mutate(
            indicador_value_abs = indicador_value*direccion
        ) %>%
        arrange(desc(anio)) %>%
        # distinct(cve_ent, anio, id_dimension, indicador_value_abs, .keep_all = T) %>%
        select(cve_ent:indicador_value_abs) %>%
        drop_na(indicador_value_abs) %>%
        mutate(indicador_value_abs = round(indicador_value_abs,4)) %>%
        mutate(id_unica = paste0("ind_",id_dimension, id_indicador)) %>%
        select(cve_ent, entidad_abr_m, id_unica, indicador_value_abs) %>%
        arrange(id_unica) %>%
        pivot_wider(names_from = id_unica, values_from = indicador_value_abs) %>%
        mutate(anio = anio_vec[x]) %>%
        select(cve_ent, anio, everything())
    ips_wide <- bind_rows(
        ips_wide, ips_wide_tempo
    )
}

# Revisión de abreviaturas correctas
# unique(ips_long$entidad_abr_m) %>% sort()
# ips_long$id_dim_ind[ips_long$entidad_abr_m == "TAMP"]

ips_wide %>% as_tibble()
ips_long %>% as_tibble()
names(ips_wide)
ips_long %>% pull(id_dim_ind) %>% unique()

# ips_long[(as.numeric(ips_long$cve_ent) > 32),]

table(ips_long$id_dim_ind)
table(is.na(ips_long$indicador_value))
table(is.na(ips_wide$indicador_value))

# 2. Direcciones ---------------------------------------------------------------

ips_uto_disto_discrecionales <- imp_dv(v_id, 2) %>% 
    select(id_dimension:direccion,utopia,distopia)

# ips_long %>%
#     lapply(function(x){is.na(x) %>% sum})

ips_direccion %>% 
    lapply(function(x){is.na(x) %>% sum})

# rr <-ips_uto_disto_discrecionales %>% 
#     mutate(id_dim_ind = paste0(id_dimension, id_indicador))
# 
# unique(ips_long$id_dim_ind)[!(unique(ips_long$id_dim_ind) %in% rr$id_dim_ind)]

ips_direccion <- ips_long %>% 
    # filter(id_dim_ind == "0103") %>% 
    left_join(
        ips_uto_disto_discrecionales %>% 
            mutate(id_dim_ind = paste0(id_dimension, id_indicador))
    ) %>% 
    as_tibble() %>% 
    mutate(
        indicador_value_abs = indicador_value*direccion
    ) %>% 
    select(-contains("topia")) %>% 
    left_join(
        imp_dv(v_id, 2) %>% 
            select(id_dimension:id_indicador,utopia,distopia)
    ) %>% 
    select(
        cve_ent, entidad_abr_m, anio, id_dimension, id_indicador, indicador_value,
        id_componente, direccion, indicador_value_abs, utopia, distopia
    ) %>% 
    as_tibble()

table(is.na(ips_direccion$indicador_value_abs)) # Acá NO debe haber NAs
openxlsx::write.xlsx(ips_long, "03_ips_clean/01_ips_long.xlsx")
openxlsx::write.xlsx(ips_wide, "03_ips_clean/02_ips_wide.xlsx")
openxlsx::write.xlsx(ips_direccion, "03_ips_clean/03_ips_direccion.xlsx")

ips_long <- readxl::read_excel("03_ips_clean/01_ips_long.xlsx")
ips_wide <- readxl::read_excel("03_ips_clean/02_ips_wide.xlsx") 
ips_direccion <- readxl::read_excel("03_ips_clean/03_ips_direccion.xlsx")


# 3. Estadística descriptiva ---------------------------------------------------
## 3.1. Estadísticos -----------------------------------------------------------

ips_stats <- ips_wide %>% 
    psych::describe(quant=c(.25,.5,.75,1)) %>%
    as_tibble(rownames="indicador")  %>%
    print()

openxlsx::write.xlsx(ips_stats, "03_ips_clean/04_ips_stats.xlsx")
ips_stats <- readxl::read_excel("03_ips_clean/04_ips_stats.xlsx")

## 3.2. Histogramas ------------------------------------------------------------
ggplot(
    ips_long %>% 
        filter(!cve_ent == "00"),
    aes(x = indicador_value)
) + labs(title = "Distribución de los valores de la variable") + 
    facet_wrap(~id_dim_ind,scales = "free")+
    geom_density()

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
        as_tibble() %>% 
        bind_rows(
            t(utop_distop_long$distopia_final) %>% 
                as_data_frame() 
        )
)

length(names(utop_distop_wide)[3:length(names(utop_distop_wide))])==length(unique(utop_distop_long$id_unica))
names(utop_distop_wide)[3:length(names(utop_distop_wide))] <- unique(utop_distop_long$id_unica)

openxlsx::write.xlsx(utop_distop_wide, "03_ips_clean/06_ips_utop_distop_wide.xlsx")

ips_wide <- ips_wide %>% 
    bind_rows(
        utop_distop_wide
    )

# 5. Normalización -------------------------------------------------------------

ips_wide_norm <- bind_cols(
    ips_wide[1:3],
    as_tibble(scale(ips_wide[4:ncol(ips_wide)])) 
)

openxlsx::write.xlsx(ips_wide_norm, paste0("03_ips_clean/07_ips_wide_norm_complete.xlsx"))

# 6. Alfas de Cronbach y KMO ---------------------------------------------------
# unique(ips_long$id_dim_ind) %>% length()
# 55 indicadores 
ids_comp <- imp_dv(v_id, 2) %>% 
    select(id_dimension:direccion,utopia,distopia) %>% 
    mutate(id_comp = paste0(id_dimension, id_indicador, "_", id_componente, "comp")) %>% 
    select(id_comp) %>% 
    dplyr::distinct(id_comp) %>% 
    filter(!(id_comp %in% c("0256_08comp",
                            "0257_08comp", 
                            "0230_07comp"))) %>% 
    glimpse

# ESTO TIENE QUE DAR TRUE PARA SABER QUE TENEMOS EL MISMO NUMERO EN LOS IDS QUE EN LAS CLAVES DE ANTES!! 
length(names(ips_wide_norm)[4:length(names(ips_wide_norm))]) == length(ids_comp$id_comp)
# VERIFICAR LOS NOMBRES NUEVOS QUE LE VAS A PONER A LA TABLA
# cbind(names(ips_wide_norm)[4:length(names(ips_wide_norm))],
#       ids_comp$id_comp) %>%
#     View()
names(ips_wide_norm)[4:length(names(ips_wide_norm))] <- ids_comp$id_comp

ips_comp <- ips_wide_norm %>%
    select(ends_with("comp")) %>%
# Variables eliminadas en la edición 2024.
    select(-c("0104_01comp",
              # "0113_04comp",
              "0221_05comp",
              "0227_07comp",
              "0231_08comp",
              "0339_09comp",
              "0344_10comp",
              "0353_12comp"))

names(ips_comp)

# ips_comp %>%
#     select(1:4) %>%
#     cor()

ids_comp_vec <- str_sub(ids_comp$id_comp, -6) %>% 
    as_tibble() %>%
    distinct()

ids_comp_vec <- as.character(ids_comp_vec$value)

source("01_códigos/14_codigo_matriz_correlacion.R")

alphas  <- list()
kmos    <- list()

# i = 1
for(i in 1:length(ids_comp_vec)){
    
    tempo       <- ips_comp %>% 
        select(ends_with(ids_comp_vec[i]))
    
    alpha_tempo <- psych::alpha(tempo, check.keys=TRUE)
    kmo_tempo   <- psych::KMO(tempo)
    
    
    alphas[i]   <- list(alpha_tempo)
    # El análisis de KMO debería ser después del PCA
    kmos[i]     <- list(kmo_tempo)
    
}

tabla_alfas <- lapply(1:length(alphas), function(i){
    vcj = alphas[[i]]$feldt %>% as.vector()
    attributes(vcj) <- NULL
    vcj = (vcj %>% unlist())[1:3]
    names(vcj) <- NULL
    tibble(componente = i, 
           lim_inf = vcj[1], 
           alpha = vcj[2],
           lim_sup = vcj[3]) %>% return()
}) %>% 
    do.call(rbind, .) %>% 
    mutate(cumple_limite = ifelse(lim_sup > 0.65, yes = T, no = F))

tabla_alfas
# i = 1
tabla_kmos <- lapply(1:length(kmos), function(i){
    vcj = kmos[[i]]$MSA  %>% as.vector()
    vcj_i <- kmos[[i]]$MSAi
    attributes(vcj_i) <- NULL
    
    tibble(componente = i, 
           names = c(names(kmos[[i]]$MSAi), str_c("componente ", i)), 
           values = c(vcj_i, vcj))
    
           }) %>% 
    do.call(rbind, .)

tabla_kmos %>% 
    filter(str_detect(names, "componente")) %>% 
    mutate(cumple = ifelse(values >= 0.5, yes = T, no = F))
tabla_kmos %>% 
    arrange(-values)

openxlsx::write.xlsx(tabla_alfas, "04_análisis_factorial/01_solo_alphas.xlsx")
openxlsx::write.xlsx(tabla_kmos, "04_análisis_factorial/02_tablas_kmos.xlsx")

# Output alphas y KMO
for(i in 1:12){
    print(paste0("COMPONENTE ", i))
    print(alphas[[i]])
    print(kmos[[i]])
}

# 7. Estimación de factores ----------------------------------------------------
drop_names <- unique(paste0(str_sub(ids_comp$id_comp,1,2), str_sub(ids_comp$id_comp,-7)))
coefs <- data.frame()
ips_wide_fac <- ips_wide 

valores_esperados       <- ips_wide_fac[1:3]
valores_esperados_norm  <- ips_wide_fac[1:3]

## 1. Modelo PCA
## 2. Estimar KMOs
## 3. Pondear y reescalar

# i = 12
for(i in 1:length(ids_comp_vec)){
    
    # Con los normalizados: 
    tempo <- ips_comp %>% 
        select(ends_with(ids_comp_vec[i]))
    
    # cor(tempo)
    modelo <- principal(tempo, nfactors = 1, rotate = "varimax")
    
    coefs_tempo <- principal(tempo, nfactors = 1, rotate = "varimax")$weights %>% 
        as_data_frame() %>%
        rename(coefs = PC1) %>%
        mutate(id_comp = str_pad(i, 2, "left", "0"))
    
    # En esta parte hicimos prueba numérica, y la extracción y uso de los scores es homonimoa
    # a hacer una suma ponderada manual con factores que suman 1. 
    
    tempo_fac <- principal(tempo, nfactors = 1, rotate = "varimax")$scores %>% 
        as_tibble()
        
    valores_esperados_tempo <- tempo_fac %>% 
        rename_all(~paste0(drop_names[i]))
    
    # Reescalar a valores entre 0 y 100 
    valores_esperados_norm_tempo <- 
        scales::rescale(as.numeric(tempo_fac$PC1), to = c(0,100)) %>% 
            as_data_frame() %>% 
            rename_all(~paste0(drop_names[i]))
    
    coefs <- bind_rows(coefs, coefs_tempo)
    valores_esperados <- bind_cols(valores_esperados, valores_esperados_tempo)
    valores_esperados_norm <- bind_cols(valores_esperados_norm, valores_esperados_norm_tempo)
    
}

# warnings()
valores_esperados_norm
table(is.na(valores_esperados_norm$`01_01comp`))
rm(drop_names)

# h = 1

# Checar componente 8
valores_esperados_norm$anio
h <- 1

# En la columna de año (la 2) salen valores faltantes, lo cual es correcto. No se espanten. 
# Es porque utopia/distopia no tienen años asociados
lapply(1:ncol(valores_esperados_norm), function(h){
    is.na(valores_esperados_norm[h]) %>% 
        as.vector() %>% 
        table()
})

# Sacar promedios para estimar el puntaje de las dimensiones y del IPS
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
        # IPS completo 
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
    arrange(cve_ent, anio) %>% 
    ungroup()

ips_final[is.na(ips_final$dim_value),]
table(is.na(ips_final$dim_value)) # NO HAY NAs


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

# 8. Infobites -----------------------------------------------------------------

ips_final <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx")
# Colores MCV
mcv_discrete <- c("#6950d8", "#00b783", "#ff6260", "#ffaf84", "#ffbd41", "#3CEAFA")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados

## 0. Mapa ----
mxmap <- mxstate.map %>% 
    rename(cve_ent = region) %>% 
    left_join(
        ips_final %>% 
            filter(anio == EDICION, id_dim == "00", !cve_ent == "00") %>% 
            select(cve_ent, id_dim, entidad_abr_m, dim_value) 
    ) 


g1 <- 
    ggplot(
        mxmap, 
        aes(long, lat, group=group, fill = dim_value)
    ) +
    geom_polygon(color = "black", linewidth = .2) +
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 62) +
    theme_void() +
    coord_map() +
    labs(
        title = "",
        subtitle = "",
        color="", shape="", y = ""
    ) +
    theme(plot.margin= margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none")


d <- 
    ips_final %>% 
    filter(anio == EDICION, id_dim == "00", !cve_ent == "00") %>% 
    select(cve_ent, id_dim, entidad_abr_m, dim_value) %>% 
    arrange(desc(dim_value)) %>% 
    glimpse

g2 <- 
    ggplot(
        d,
        aes(
            x = dim_value,
            y = reorder(entidad_abr_m, dim_value),
            fill = dim_value,
            label = round(dim_value, 2)
        )
    ) +
    geom_col(show.legend = F) +
    # Etiquetas para números positivos (excepto el máximo)
    geom_text(hjust = 1, col = "white", size = 5, fontface = "bold", family = "Ubuntu"
    ) +
    scale_x_continuous("", labels = scales::number, breaks = seq(20,80,20), limits = c(0,85))+
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 62) +
    theme_minimal() +
    theme(plot.title = element_text(size = 35, face = "bold", colour = "#6950D8"),
          plot.subtitle = element_text(size = 30, colour = "#777777", margin=margin(0,0,60,0)),
          plot.margin= margin(0.3, 0.5, 2, 0.3, "cm"), # margin(top,right, bottom,left)
          plot.caption = element_text(size = 20),
          strip.text.x = element_text(size = 25),
          panel.grid.minor  = element_blank(),
          panel.background = element_rect(fill = "transparent",colour = NA),
          panel.spacing = unit(2, "lines"),
          text = element_text(family = "Ubuntu"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15),
          legend.text = element_text(size = 25),
          legend.position = "none")

g_plot <- 
    ggpubr::ggarrange(g1, g2, widths = c(2,0.7))

titulo <- "Índice de Progreso Social"
subtitulo <- EDICION

g_plot <- 
    ggpubr::annotate_figure(
        g_plot, 
        top = ggpubr::text_grob(paste0(titulo, "\n", subtitulo, "\n"), size = 40, 
                                face = "bold", family = "Ubuntu", color = "#6950D8", vjust = 0.6)
    )
g_plot <- ggimage::ggbackground(g_plot, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/00_IPS_mapa.png", width = 23, height = 12, dpi = 100)
ggsave(filename = "05_infobites/00_IPS_mapa.svg", width = 23, height = 12, dpi = 100)

## 1. Heatmaps -----------------------------------------------------------------
### 1.0. IPS -------------------------------------------------------------------

titulo <- "Índice de Progreso Social"
subtitulo <- str_c("2015 - ", EDICION)
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
    geom_text(aes(label = round(dim_value,1)), family = "Ubuntu", fontface = "bold", size = 4) +
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1],midpoint = 62)  +
    guides(label = "none") +
    scale_x_discrete(position = "bottom")+
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

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/00_IPS_heatmap.png", width = 23, height = 12, dpi = 100)
ggsave(filename = "05_infobites/00_IPS_heatmap.svg", width = 23, height = 12, dpi = 100)

# Vertical
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
       aes(x = anio, 
           y = reorder(entidad_abr_m, -as.numeric(cve_ent)),
           fill = dim_value)) +
    geom_tile(col = "white", show.legend = F) +
    geom_text(aes(label = round(dim_value,1)), family = "Ubuntu",fontface = "bold", size = 4) +
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1],midpoint = 62)  +
    guides(label = "none") +
    scale_x_continuous(position = "bottom", 
                       breaks = min(ips_final$anio, na.rm = T):max(ips_final$anio, na.rm = T), 
                       expand = expansion(c(0,0))) +
    # Etiquetas
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        y = eje_x, 
        x = NULL, 
        color = ""
    )   + 
    coord_fixed() +
    theme(legend.position="bottom") +
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8", hjust = 0.5),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0), hjust = 0.5),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 25, angle = 90, vjust = 0.5, hjust = 1, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "top")  

ggsave(filename = "05_infobites/00_en_cifras_ips/00_IPS.png", 
       width = 12, height = 23, dpi = 200)

ggsave(filename = "05_infobites/00_en_cifras_ips/00_IPS.svg", 
       width = 12, height = 23, dpi = 200)

### 1.1. Necesidades Humanas Básicas  ----
titulo <- "Dim 1. Necesidades Humanas Básicas"
subtitulo <- str_c("2015 - ", EDICION)
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
    geom_text(aes(label = round(dim_value,1)), family = "Ubuntu",fontface = "bold", size = 4) +
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

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/01_NHB_heatmap.png", width = 23, height = 12, dpi = 100)
ggsave(filename = "05_infobites/01_NHB_heatmap.svg", width = 23, height = 12, dpi = 100)

# Vertical
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
       aes(x = anio, 
           y = reorder(entidad_abr_m, -as.numeric(cve_ent)),
           fill = dim_value)) +
    geom_tile(col = "white", show.legend = F) +
    geom_text(aes(label = round(dim_value,1)), family = "Ubuntu",fontface = "bold", size = 4) +
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1],midpoint = 62)  +
    guides(label = "none") +
    scale_x_continuous(position = "bottom", 
                       breaks = min(ips_final$anio, na.rm = T):max(ips_final$anio, na.rm = T), 
                       expand = expansion(c(0,0))) +
    # Etiquetas
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        y = eje_x, 
        x = NULL, 
        color = ""
    )   + 
    coord_fixed() +
    theme(legend.position="bottom") +
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8", hjust = 0.5),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0), hjust = 0.5),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 25, 
                                          hjust = 1, vjust = 0.5, angle = 90, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "top")  

ggsave(filename = "05_infobites/00_en_cifras_ips/01_00_NHB.png", 
       width = 12, height = 23, dpi = 200)

ggsave(filename = "05_infobites/00_en_cifras_ips/01_00_NHB.svg", 
       width = 12, height = 23, dpi = 200)

### 1.2. Fundamentos del Bienestar ---------------------------------------------

titulo <- "Dim 2. Fundamentos del Bienestar"
subtitulo <- str_c("2015 - ", EDICION)
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
    geom_text(aes(label = round(dim_value,1)), family = "Ubuntu",fontface = "bold", size = 4) +
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

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/02_SB_heatmap.png", width = 23, height = 12, dpi = 100)
ggsave(filename = "05_infobites/02_SB_heatmap.svg", width = 23, height = 12, dpi = 100)

# Vertical
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
       aes(x = anio, 
           y = reorder(entidad_abr_m, -as.numeric(cve_ent)),
           fill = dim_value)) +
    geom_tile(col = "white", show.legend = F) +
    geom_text(aes(label = round(dim_value,1)), family = "Ubuntu",fontface = "bold", size = 4) +
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1],midpoint = 62)  +
    guides(label = "none") +
    scale_x_continuous(position = "bottom", 
                       breaks = min(ips_final$anio, na.rm = T):max(ips_final$anio, na.rm = T), 
                       expand = expansion(c(0,0))) +
    # Etiquetas
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        y = eje_x, 
        x = eje_y, 
        color = ""
    )   + 
    coord_fixed() +
    theme(legend.position="bottom") +
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8", hjust = 0.5),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0), hjust = 0.5),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "top")  

ggsave(filename = "05_infobites/00_en_cifras_ips/02_00_FB.png", 
       width = 12, height = 23, dpi = 200)

ggsave(filename = "05_infobites/00_en_cifras_ips/02_00_FB.svg", 
       width = 12, height = 23, dpi = 200)

### 1.3. Oportunidades ----
titulo <- "Dim 3. Oportunidades"
subtitulo <- str_c("2015 - ", EDICION)
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
    geom_text(aes(label = round(dim_value,1)), family = "Ubuntu",fontface = "bold", size = 4) +
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

g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/03_OP_heatmap.png", width = 23, height = 12, dpi = 100)
ggsave(filename = "05_infobites/03_OP_heatmap.svg", width = 23, height = 12, dpi = 100)

# Vertical
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
       aes(x = anio, 
           y = reorder(entidad_abr_m, -as.numeric(cve_ent)),
           fill = dim_value)) +
    geom_tile(col = "white", show.legend = F) +
    geom_text(aes(label = round(dim_value,1)), family = "Ubuntu",fontface = "bold", size = 4) +
    scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1],midpoint = 62)  +
    guides(label = "none") +
    scale_x_continuous(position = "bottom", 
                       breaks = min(ips_final$anio, na.rm = T):max(ips_final$anio, na.rm = T), 
                       expand = expansion(c(0,0))) +
    # Etiquetas
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        y = eje_x, 
        x = eje_y, 
        color = ""
    )   + 
    coord_fixed() +
    theme(legend.position="bottom") +
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8", hjust = 0.5),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0), hjust = 0.5),
        plot.caption       = element_text(size = 20),
        plot.margin        = margin(0.3, 0.3, 2, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 25, angle = 90, hjust = 1, vjust = 0.5, colour = "#777777"),
        axis.text.y        = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "top")  

ggsave(filename = "05_infobites/00_en_cifras_ips/03_00_OP.png", 
       width = 12, height = 23, dpi = 200)

ggsave(filename = "05_infobites/00_en_cifras_ips/03_00_OP.svg", 
       width = 12, height = 23, dpi = 200)


## 2. Sankeys (rankings) -------------------------------------------------------

d <- ips_final %>% 
    filter(!as.numeric(cve_ent) > 33) %>% 
    filter(!cve_ent == "00") %>% 
    group_by(anio, id_dim) %>% 
    arrange(desc(dim_value), .by_group = T) %>% 
    mutate(ranking = 1:32,
           ranking = str_pad(ranking, 2, "left", "0")) %>% 
    ungroup()

### 2.0. IPS -------------------------------------------------------------------

titulo <- "Ranking del Índice de Progreso Social"
subtitulo <- str_c("2015 - ", EDICION)
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
g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/04_IPS_sankey.png", width = 23, height = 12, dpi = 100)
ggsave(filename = "05_infobites/04_IPS_sankey.svg", width = 23, height = 12, dpi = 100)

# En inglés -----

titulo <- "Social Progress Index Ranking"
subtitulo <- str_c("2015 - ", EDICION)
eje_x <- ""
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
    geom_text(stat = "stratum", size = 4, family = "Ubuntu", fontface = "bold") +
    scale_fill_gradient2("", high = mcv_semaforo[4], mid = mcv_semaforo[2], low = mcv_semaforo[1],midpoint = 16)  +
    scale_x_discrete(expand = expansion(c(0.05, 0.05))) + 
    scale_y_continuous(expand = expansion(c(0.01, 0.05))) + 
    # Etiquetas
    labs(
        title = titulo, 
        subtitle = subtitulo, 
        x = eje_x, 
        y = eje_y, 
        color = "", 
        caption = "\nMADE BY MEXICO, ¿CÓMO VAMOS? WITH PUBLIC DATA"
    )   + 
    theme_minimal() +
    theme(
        plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
        plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,20,0)),
        plot.caption       = element_text(size = 25, face = "bold", color = "white", hjust = 0),
        plot.caption.position = "plot",
        plot.margin        = margin(0.3, 0.3, 0.3, 0.3, "cm"), # margin(top,right, bottom,left)
        strip.text.x       = element_text(size = 25, colour = "#777777"),
        panel.grid.minor   = element_blank(),
        panel.grid.major = element_blank(),
        panel.background   = element_rect(fill = "transparent", colour = NA),
        text               = element_text(family = "Ubuntu"),
        axis.title.x       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.title.y       = element_text(family = "Ubuntu", size = 25, colour = "#777777"),
        axis.text.x        = element_text(family = "Ubuntu", size = 20, colour = "#777777", face = "bold"),
        axis.text.y        = element_blank(),
        legend.text        = element_text(family = "Ubuntu", size = 35, colour = "#777777"),
        legend.position    = "none")  
g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/01_00_00_plantilla_blanco.pdf")
ggsave(filename = "05_infobites/04_IPS_sankey_ENG.png", width = 23, height = 12, dpi = 100)
ggsave(filename = "05_infobites/04_IPS_sankey_ENG.svg", width = 23, height = 12, dpi = 100)


### 2.1. NHB -------------------------------------------------------------------

titulo <- "Ranking de la Dim 1. Necesidades Humanas Básicas"
subtitulo <- str_c("2015 - ", EDICION)
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
g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/05_NHB_sankey.png", width = 23, height = 12, dpi = 100)
ggsave(filename = "05_infobites/05_NHB_sankey.svg", width = 23, height = 12, dpi = 100)

### 2.2. SB --------------------------------------------------------------------

titulo <- "Ranking de la Dim 2. Fundamentos del Bienestar"
subtitulo <- str_c("2015 - ", EDICION)
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
g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/06_SB_sankey.png", width = 23, height = 12, dpi = 100)
ggsave(filename = "05_infobites/06_SB_sankey.svg", width = 23, height = 12, dpi = 100)


### 2.3. OP --------------------------------------------------------------------

titulo <- "Ranking de la Dim 3. Oportunidades"
subtitulo <- str_c("2015 - ", EDICION)
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
g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
ggsave(filename = "05_infobites/07_OP_sankey.png", width = 23, height = 12, dpi = 100)
ggsave(filename = "05_infobites/07_OP_sankey.svg", width = 23, height = 12, dpi = 100)

## 3. Scores por entidades -----------------------------------------------------

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
    subtitulo <- paste0(unique(a$entidad_abr_m), " | 2015 - ", EDICION)
    eje_x <- "Años"
    eje_y <- "Puntaje"
    
    g <- 
        ggplot(
            a,
            aes(
                x = as.factor(anio),
                y = dim_value,
                group = 1,
                col = id_dim
            )
        ) +
        geom_line(size = 3.7, alpha = 0.7, show.legend = F) +
        geom_point(size = 6.7)  +
        geom_text(
            aes(label = round(dim_value,1)), 
            vjust = -1.5, size = 5.5, fontface = "bold", family = "Ubuntu", show.legend = F
        ) +
        facet_wrap(~dim)  +
        scale_color_manual(values = mcv_discrete)+
        ylim(30,95)+
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
    g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
    ggsave(filename = paste0("05_infobites/08_entidades/", unique(a$cve_ent), "_", unique(a$entidad_abr_m), ".png"), width = 23, height = 12, dpi = 100)
    ggsave(filename = paste0("05_infobites/08_entidades/", unique(a$cve_ent), "_", unique(a$entidad_abr_m), ".svg"), width = 23, height = 12, dpi = 100)
    
    
}

# En Inglés 
# i = 1
for(i in 1:33){
    
    a <- ips_final  %>% 
        mutate(dim = case_when(
            id_dim == "00" ~ "0. Índice de Progreso Social",
            id_dim == "01" ~ "1. Necesidades Humanas Básicas",
            id_dim == "02" ~ "2. Fundamentos del Bienestar",
            T ~ "3. Oportunidades"
        )) %>% 
        filter(cve_ent == cves[i]) %>% 
        mutate(dim = str_replace_all(dim, c("0. Índice de Progreso Social" = "0. Social Progress Index",
                                            "1. Necesidades Humanas Básicas" = "1. Basic Human Needs",
                                            "2. Fundamentos del Bienestar" = "2. Foundations of wellbeing",
                                            "3. Oportunidades" = "3. Opportunities"))) %>% 
        mutate(entidad_abr_m = ifelse(entidad_abr_m == "Nacional", yes = "National", no = entidad_abr_m)) %>% 
        glimpse
    
    a$dim
    
    titulo <- "Score obtained on the Social Progress Index and its dimensions"
    subtitulo <- paste0(unique(a$entidad_abr_m), " | 2015 - ", EDICION)
    eje_x <- "Year"
    eje_y <- "Score"
    caption = "\nMade by México, ¿Cómo vamos? with public data" %>% str_to_upper()
    
    g <- 
        ggplot(
            a,
            aes(
                x = as.factor(anio),
                y = dim_value,
                group = 1,
                col = id_dim
            )
        ) +
        geom_line(size = 3.7, alpha = 0.7, show.legend = F) +
        geom_point(size = 6.7)  +
        geom_text(
            aes(label = round(dim_value,1)), 
            vjust = -1.5, size = 5.5, fontface = "bold", family = "Ubuntu", show.legend = F
        ) +
        facet_wrap(~dim)  +
        scale_color_manual(values = mcv_discrete)+
        ylim(30,95)+
        # Etiquetas
        labs(
            title = titulo, 
            subtitle = subtitulo, 
            x = eje_x, 
            y = eje_y, 
            color = "", 
            caption = caption
        )   + 
        theme_minimal() +
        theme(
            plot.title         = element_text(size = 40, family = "Ubuntu", face = "bold", colour = "#6950D8"),
            plot.subtitle      = element_text(size = 35, family = "Ubuntu", colour = "#777777", margin=margin(0,0,30,0)),
            plot.caption       = element_text(size = 25, color = "white", hjust = 0, face = "bold", family = "Ubuntu"),
            plot.caption.position = "plot",
            plot.margin        = margin(0.3, 0.3, 0.3, 0.3, "cm"), # margin(top,right, bottom,left)
            strip.text.x       = element_text(size = 25, colour = "#777777", face = "bold"),
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
    g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/01_00_00_plantilla_blanco.pdf")
    ggsave(filename = paste0("05_infobites/08_entidades/ingles/", unique(a$cve_ent), "_", unique(a$entidad_abr_m), ".png"), width = 23, height = 12, dpi = 100)
    ggsave(filename = paste0("05_infobites/08_entidades/ingles/", unique(a$cve_ent), "_", unique(a$entidad_abr_m), ".svg"), width = 23, height = 12, dpi = 100)
    
    
}

## 3.1 Scores por entidades comparativa JALISCO-----------------------------------------------------

#INFOBITE de comparación de las entidades Jalisco, Nacional y CDMX los cuadrantes de las dimensiones
    
    a <- ips_final  %>% 
        mutate(dim = case_when(
            id_dim == "00" ~ "0. Índice de Progreso Social",
            id_dim == "01" ~ "1. Necesidades Humanas Básicas",
            id_dim == "02" ~ "2. Fundamentos del Bienestar",
            T ~ "3. Oportunidades"
        )) %>% 
        filter(cve_ent %in% c("14", "09", "00")) %>% 
        glimpse
    
    titulo <- "Puntaje en el IPS y dimensiones"
    subtitulo <- "Evolución en el tiempo"
    eje_x <- "Años"
    eje_y <- "Puntaje"
    
    g <- 
        ggplot(
            a,
            aes(
                x = as.factor(anio),
                y = dim_value,
                group = cve_ent,
                col = cve_ent
            )
        ) +
        geom_line(size = 1.7, alpha = 0.7, show.legend = T) +
        geom_point(size = 2.7)  +
        geom_text_repel(
            aes(label = round(dim_value,1)), 
            vjust = -1.5, size = 5.5, fontface = "bold", family = "Ubuntu", show.legend = F, hjust = 1
        ) +
        facet_wrap(~dim)  +
        scale_color_manual(
            values = c("14" = "#1f78b4", "09" = "#33a02c", "00" = "#e31a1c"),
            labels = c("14" = "Jalisco", "09" = "CDMX", "00" = "Nacional")
        ) +
        ylim(45,85)+
        # Etiquetas
        labs(
            title = titulo, 
            subtitle = subtitulo, 
            x = eje_x, 
            y = eje_y, 
            color = "Entidad"
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
            legend.text        = element_text(family = "Ubuntu", size = 23, colour = "#777777"),
            legend.title        = element_text(family = "Ubuntu", size = 23, colour = "#777777"),
            legend.position    = "top") 
    g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
    ggsave(filename = paste0("05_infobites/08_entidades/", "Jal_comparación.png"), width = 23, height = 12, dpi = 100)
    ggsave(filename = paste0("05_infobites/08_entidades/", "Jal_comparación.svg"), width = 23, height = 12, dpi = 100)
    

## 4. Componentes --------------------------------------------------------------
ips_comp <- readxl::read_excel("03_ips_clean/00_IPS_COMPLETE_LONG.xlsx", sheet = 2) %>% 
    left_join(
        readxl::read_excel("02_datos_crudos/00_diccionario_componentes.xlsx")
    ) %>%
    select(ends_with("comp"), everything()) %>% 
    glimpse

id_comp_vec <- unique(ips_comp$id_comp)


### 4.1. Heatmaps --------------------------------------------------------------
# i = 1
for(i in 1:length(id_comp_vec)){
    
    a <- ips_comp %>% 
        filter(id_comp == id_comp_vec[i])
    
    titulo <- paste0(
        str_replace_all(
            str_replace_all(
                str_sub(unique(a$id_comp), 2, str_length(unique(a$id_comp))), "_", "."
            ), "comp", ". "
        ),
        unique(a$name_comp)
    )
    
    subtitulo <- str_c("2015 - ", EDICION)
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
    
    g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
    ggsave(filename = paste0("05_infobites/09_componentes/01_heatmaps/", str_remove_all(unique(a$id_comp), "comp"), "_00_", str_replace_all(tolower(unique(a$name_comp)), " ", "_"), ".png"),
           width = 23, height = 12, dpi = 100)
    ggsave(filename = paste0("05_infobites/09_componentes/01_heatmaps/", str_remove_all(unique(a$id_comp), "comp"), "_00_", str_replace_all(tolower(unique(a$name_comp)), " ", "_"), ".svg"),
           width = 23, height = 12, dpi = 100)
    
}

### 4.2. Sankeys (rankings) ----------------------------------------------------
# i = 1
for(i in 1:length(id_comp_vec)){
    
    a <- ips_comp %>% 
        filter(id_comp == id_comp_vec[i]) %>% 
        filter(!as.numeric(cve_ent) > 33) %>% 
        filter(!cve_ent == "00") %>% 
        group_by(anio) %>% 
        arrange(desc(comp_value), .by_group = T) %>% 
        mutate(ranking = 1:32,
               ranking = str_pad(ranking, 2, "left", "0")) %>% 
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
    subtitulo <- str_c("2015 - ", EDICION)
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
    g <- ggimage::ggbackground(g, "05_infobites/00_plantillas/00_IPS.pdf")
    ggsave(filename = paste0("05_infobites/09_componentes/02_sankeys/", str_remove_all(unique(a$id_comp), "comp"), "_", str_replace_all(tolower(unique(a$name_comp)), " ", "_"), ".png"), 
           width = 23, height = 12, dpi = 100)
    
}

### 4.3. Heatmaps verticales ---------------------------------------------------

for(i in 1:length(id_comp_vec)){
    
    a <- ips_comp %>% 
        filter(id_comp == id_comp_vec[i]) %>% 
        glimpse
    
    titulo <- paste0(
        str_replace_all(
            str_replace_all(
                str_remove_all(unique(a$id_comp), "0"), "_", "."
            ), "comp", ". "
        ),
        unique(a$name_comp)
    )
    
    subtitulo <- str_c("2015 - ", EDICION)
    eje_y <- ""
    eje_x <- "Años"
    
    #g <- 
        ggplot(data = 
                   a %>% 
                   filter(!as.numeric(cve_ent) > 33),
               aes(x = as.factor(anio), 
                   y = reorder(entidad_abr_m, -as.numeric(cve_ent)),
                   fill = comp_value)) +
        geom_tile(col = "white", show.legend = F) +
        geom_text(aes(label = round(comp_value,1)), family = "Ubuntu", size = 4) +
        scale_fill_gradientn(colours = rev(mcv_semaforo))+
        guides(label = "none") +
        #scale_y_discrete(position = "top")+
        # Etiquetas
        labs(
            title = str_wrap(titulo, 25), 
            subtitle = subtitulo, 
            x = eje_x, 
            y = eje_y, 
            color = ""
        )   + 
        coord_fixed() +
        theme(legend.position="bottom") +
        theme_minimal() +
        theme(
            plot.title        = element_text(size = 40, face = "bold", colour = "#6950D8", hjust = 0.5),
            plot.subtitle     = element_text(size = 35, colour = "#777777", margin=margin(0,0,10,0), hjust = 0.5),
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
    
    ggsave(filename = paste0("05_infobites/09_componentes/03_heatmaps_verticales/", str_replace_all(unique(a$id_comp), "comp", "_00"), "_", str_replace_all(tolower(unique(a$name_comp)), " ", "_"), ".png"), 
           width = 12, height = 23, dpi = 200)
    ggsave(filename = paste0("05_infobites/09_componentes/03_heatmaps_verticales/", str_replace_all(unique(a$id_comp), "comp", "_00"), "_", str_replace_all(tolower(unique(a$name_comp)), " ", "_"), ".svg"), 
           width = 12, height = 23, dpi = 200)
    
}


# FIN. -------------------------------------------------------------------------

