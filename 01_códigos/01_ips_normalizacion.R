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

#Leemos la base procesada en formato larga
ipsl <- read.xlsx("./02_bases_procesadas/01_ips_complete_long.xlsx", sheet = 1)


#Transformamos a formato largo
ipsw <- ipsl %>% 
    select(c(cve_ent, entidad_abr_m, indicador, indicador_value))%>% 
    pivot_wider(names_from = indicador, values_from = indicador_value)


##Obtenemos vectores de utopias y distopias
utopia <- ipsl %>% 
        group_by(indicador)%>% 
        summarise(
        utopia = mean(utopia_final, na.rm = T),
        distopia = mean(distopia_final, na.rm = T),
            ) %>% 
    select(-ends_with("se"))%>% 
    mutate(cve_ent = 33,
           entidad_abr_m = "Utopia")%>% 
    select(-c(distopia))%>% 
    pivot_wider(names_from = indicador, values_from = utopia )
    
distopia <- ipsl %>% 
    group_by(indicador)%>% 
    summarise(
        utopia = mean(utopia_final, na.rm = T),
        distopia = mean(distopia_final, na.rm = T),
    ) %>% 
    select(-ends_with("se"))%>% 
    mutate(cve_ent = 34,
           entidad_abr_m = "Distopia")%>% 
    select(-c(utopia))%>% 
    pivot_wider(names_from = indicador, values_from = distopia ) 

uyd <- rbind(utopia,distopia)

final <- rbind(ipsw, uyd)    

#standardize una variable
prov <- (final$ind_0101 - mean(final$ind_0101 )) / sd(final$ind_0101 )

#standardize un rango de variables
final_standarized <- as.data.frame(scale(final[3:52]))

prov <- final %>% 
            select(cve_ent, entidad_abr_m)

final <- cbind(prov, final_standarized)

haven::write_dta(final, "02_bases_procesadas/01_ips_complete_wide.dta")
openxlsx::write.xlsx(final, "02_bases_procesadas/01_ips_complete_wide.xlsx")
