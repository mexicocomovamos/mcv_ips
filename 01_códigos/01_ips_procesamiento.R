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

# 1. Consolidación de bd ----
anio_vec <- 2015:2020
ips_wide <- data.frame()
ips_long <- data.frame()
ips_direccion <- data.frame()
for(x in 1:6){
    
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
            filter(anio_dist == min(anio_dist))
        
        
        ips_complete <- bind_rows(ips_complete, ips_tempo)
        
        
    }
    
    ips_complete <- ips_complete %>% 
        select(-contains("drop")) %>% 
        select(-contains("dist"))
    
    ips_long <- bind_rows(
        ips_long, ips_complete 
    ) %>% 
        mutate(anio = anio_vec[x])
    
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
        filter(!id_unica == "ind_0346") %>% 
        bind_rows(
            # Proceso para limpiar indicador 03_46
            ips_direccion_tempo %>% 
                arrange(desc(anio)) %>% 
                distinct(cve_ent, id_dimension, indicador_value_abs, .keep_all = T) %>% 
                select(cve_ent:indicador_value_abs) %>% 
                drop_na(indicador_value_abs) %>% 
                mutate(indicador_value_abs = round(indicador_value_abs,4)) %>% 
                mutate(id_unica = paste0("ind_",id_dimension, id_indicador)) %>% 
                select(cve_ent, entidad_abr_m, id_unica, indicador_value_abs) %>% 
                arrange(id_unica) %>% 
                filter(id_unica == "ind_0346") %>% 
                distinct(cve_ent, entidad_abr_m, .keep_all = T)
            
        ) %>% 
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
            !(is.na(utopia)) & direccion == -1 ~ utopia*direccion,
            !(is.na(utopia)) & direccion == 1 ~ utopia*direccion,
            T ~ utopia
        ),
        distopia_final = case_when(
            is.na(distopia) & direccion == -1 ~ min-sd,
            is.na(distopia) & direccion == 1 ~ min-sd,
            !(is.na(distopia)) & direccion == -1 ~ distopia*direccion,
            !(is.na(distopia)) & direccion == 1 ~ distopia*direccion,
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


openxlsx::write.xlsx(ips_wide_norm, paste0("03_ips_clean/00_ips_wide_norm_complete.xlsx"))




openxlsx::write.xlsx(ips_wide_norm %>% 
                         mutate_at(
                             vars(starts_with("ind")),
                             ~abs(.)
                         ),
                     paste0("03_ips_clean/00_ips_wide_norm_complete_abs.xlsx"))


# 6. Alfas de Cronbach y KMO ----
ids_comp <- imp_dv(v_id, 2) %>% 
    select(id_dimension:direccion,utopia,distopia) %>% 
    mutate(id_comp = paste0(id_dimension, id_indicador, "_", id_componente, "comp")) %>% 
    select(id_comp) %>% 
    dplyr::distinct(id_comp) 

names(ips_wide_norm)[4:58] <- ids_comp$id_comp

ips_comp <- ips_wide_norm %>% 
    # mutate_at(
    #     vars(starts_with("ind")),
    #     ~abs(.)
    # ) %>% 
    select(ends_with("comp")) 

ids_comp_vec <- str_sub(ids_comp$id_comp, -6) %>% as_tibble() %>% distinct() 
ids_comp_vec <- as.character(ids_comp_vec$value)

alphas <- list()
kmos <- list()

for(i in 1:length(ids_comp_vec)){
    
    tempo <- ips_comp %>% 
        select(ends_with(ids_comp_vec[i]))
    
    alpha_tempo <- psych::alpha(tempo)
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
for(i in 1:12){
    
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






openxlsx::write.xlsx(ips_final, "03_ips_clean/10_IPS_COMPLETE.xlsx")
openxlsx::write.xlsx(ips_final %>% 
                         pivot_wider(
                             names_from = id_dim,
                             values_from = dim_value
                         ) %>% 
                         arrange(anio, cve_ent), 
                     "03_ips_clean/11_IPS_COMPLETE_WIDE.xlsx")

openxlsx::write.xlsx(coefs, "03_ips_clean/99_coefs.xlsx")


# 8. Heatmap ----


ggplot(data = ips_final %>% 
           # filter(id_dim == "02") %>% 
           filter(!as.numeric(cve_ent) > 33),
       aes(x = anio, 
           #y = reorder(entidad_abr_m, dim_value),
           y = fct_rev(entidad_abr_m),
           fill = dim_value)) +
    geom_tile(col = "white") +
    geom_text(aes(label = round(dim_value,1)))+
    facet_wrap(~id_dim)+
    scale_fill_gradient2(low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1],midpoint = 62)
    theme(legend.position="bottom") +
    theme_hc() +
    labs(x = "", y = "",
         title = str_wrap(fiuf, width = 100),
         caption = fiuffi) +
    scale_y_discrete(position = "right") +
    scale_x_discrete(position = "bottom",
                     breaks = as.character(seq(2005,2019,14)),
                     labels = as.character(seq(2005,2019,14))) +
    coord_fixed()
    
    
ips_cambios_2019_2020 <- ips_final %>% 
    filter(id_dim == "03", anio >2018)%>% 
    filter(!as.numeric(cve_ent) > 33) %>% 
    pivot_wider(
        names_from = "anio",
        names_prefix = "anio_",
        values_from = "dim_value"
    ) %>% 
    mutate(
        dif = anio_2020-anio_2019
    )

