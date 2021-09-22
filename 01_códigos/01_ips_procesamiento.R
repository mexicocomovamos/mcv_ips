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


# Tidyverse <3
require(tidyverse)

## 0.2. Tokens ----
google_token <- "AIzaSyDF4E80nih1fBMPg785wYO-ruAKgJiEdW0"

# Obtener identificador de la base de del IPS 
v_id <- as.character(
    googledrive::drive_get(
        "https://docs.google.com/spreadsheets/d/1hi5qzhpZz1S7_TFe68lqMQCYUFOEQjRejMOlvSTjw0w/edit#gid=1859408845")[1, 2])

# Función para importar de manera más corta desde drive
imp_dv <- function(x, y){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x), sheet = y)}

# 1. Consolidación de bd ----
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
            anio_dist = anio-2015
        ) %>% 
        filter(anio_dist<=0) %>% 
        mutate(anio_dist = abs(anio_dist)) %>% 
        filter(anio_dist == min(anio_dist))
    
    
    ips_complete <- bind_rows(ips_complete, ips_tempo)
    
    
}

ips_complete <- ips_complete %>% 
    select(-contains("drop")) %>% 
    select(-contains("dist"))



# 2. Direcciones ----
ips_uto_disto_discrecionales <- imp_dv(v_id, 2) %>% 
    select(id_dimension:distopia)

ips_direccion <- ips_complete %>% 
    left_join(ips_uto_disto_discrecionales) %>% 
    mutate(
        indicador_value_abs = indicador_value*direccion
    )

ips_wide <- ips_direccion %>% 
    arrange(desc(anio)) %>% 
    distinct(cve_ent, id_dimension, indicador_value_abs, .keep_all = T) %>% 
    select(cve_ent:indicador_value_abs) %>% 
    drop_na(indicador_value_abs) %>% 
    mutate(indicador_value_abs = round(indicador_value_abs,4)) %>% 
    mutate(id_unica = paste0("ind_",id_dimension, id_indicador)) %>% 
    select(cve_ent, entidad_abr_m, id_unica, indicador_value_abs) %>% 
    arrange(id_unica) %>% 
    pivot_wider(names_from = id_unica, values_from = indicador_value_abs)

# 3. Estadística descriptiva ----
## 3.1. Estadísticos ----
ips_stats <- ips_wide %>% 
    psych::describe(quant=c(.25,.5,.75,1)) %>%
    as_tibble(rownames="indicador")  %>%
    print()

## 3.2. Histogramas ----
plot(hist(ips_wide$ind_0114[ips_wide$cve_ent!="00"]))

# 4. Utopías y distopías ----
test <- ips_direccion %>% 
    mutate(indicador = paste0("ind_",id_dimension, id_indicador)) %>% 
    left_join(
        ips_stats %>% 
            filter(str_starts(indicador, "ind")) %>% 
            select(indicador, min, max, sd)
    ) %>% 
    mutate(
        utopia_final = case_when(
            is.na(utopia) & direccion == -1 ~ max+sd,
            is.na(utopia) & direccion == 1 ~ max+sd,
            T ~ utopia
        ),
        distopia_final = case_when(
            is.na(distopia) & direccion == -1 ~ min-sd,
            is.na(distopia) & direccion == 1 ~ min-sd,
            T ~ distopia
        )
        
    )

haven::write_dta(test, "02_bases_procesadas/01_ips_complete_long.dta")
openxlsx::write.xlsx(test, "02_bases_procesadas/01_ips_complete_long.xlsx")
