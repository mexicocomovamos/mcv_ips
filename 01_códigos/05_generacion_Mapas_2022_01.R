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
if(!require("sf")) install.packages("sf") & require("sf")
if(!require("mxmaps")) install.packages("mxmaps") & require("mxmaps")

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

t <- readxl::read_excel("03_ips_clean/00_ips_complete_long.xlsx") %>% 
    filter(anio == 2022, id_dim == "00", !cve_ent == "00") %>% 
    mutate(region = case_when(
        
        cve_ent == "01" ~ "1. El Bajío",
        cve_ent == "11" ~ "1. El Bajío",
        cve_ent == "22" ~ "1. El Bajío",
        cve_ent == "24" ~ "1. El Bajío",
        cve_ent == "32" ~ "1. El Bajío",
        
        cve_ent == "05" ~ "2. La Laguna",
        cve_ent == "10" ~ "2. La Laguna",
        
        cve_ent == "07" ~ "3. El Sureste",
        cve_ent == "12" ~ "3. El Sureste",
        cve_ent == "20" ~ "3. El Sureste",
        
        cve_ent == "09" ~ "4. Valle de México",
        cve_ent == "15" ~ "4. Valle de México",
        cve_ent == "13" ~ "4. Valle de México",
        cve_ent == "17" ~ "4. Valle de México",
        cve_ent == "21" ~ "4. Valle de México",
        cve_ent == "29" ~ "4. Valle de México",
        
        cve_ent == "04" ~ "5. Golfo de México",
        cve_ent == "27" ~ "5. Golfo de México",
        cve_ent == "30" ~ "5. Golfo de México",
        
        cve_ent == "02" ~ "6. Mar de Cortés",
        cve_ent == "03" ~ "6. Mar de Cortés",
        cve_ent == "25" ~ "6. Mar de Cortés",
        cve_ent == "26" ~ "6. Mar de Cortés",
        
        cve_ent == "06" ~ "7. Occidente",
        cve_ent == "14" ~ "7. Occidente",
        cve_ent == "18" ~ "7. Occidente",
        cve_ent == "16" ~ "7. Occidente",
        
        cve_ent == "08" ~ "8. El Norte",
        cve_ent == "19" ~ "8. El Norte",
        cve_ent == "28" ~ "8. El Norte",
        
        T ~ "9. La Península de Yucatán"
        
    )) %>% 
    select(region, cve_ent, entidad_abr_m, value = dim_value) 

i = 1

for(i in 1:9){
    
    mxmap <- mxstate.map %>% 
        rename(cve_ent = region) %>% 
        left_join(
            t 
        ) %>% 
        filter(region == unique(t$region)[i])
    
    tempo <- mxmap %>%
        st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
        group_by(cve_ent) %>%
        summarise(geometry = st_combine(geometry)) %>%
        st_cast("POLYGON") 
    
    
    mxmap_labs <- mxmap %>% 
        distinct(cve_ent, entidad_abr_m, value) %>% 
        bind_cols(
            as.data.frame(rgeos::gCentroid(as(tempo, "Spatial"), byid = TRUE))
        )
    
    ggplot(
        mxmap, 
        aes(long, lat, group=group, fill = value)
    ) +
        geom_polygon(color = "black", size = .2, show.legend = F, alpha = 0.8) +
        geom_label(data = mxmap_labs,
                   aes(x, y, label = paste0(entidad_abr_m, "\n[", round(value, 2), "]"), group = cve_ent), alpha = .4,
                   family = "Ubuntu", show.legend = F) +
        scale_fill_gradient2("", low = mcv_semaforo[4], mid = mcv_semaforo[2], high = mcv_semaforo[1], midpoint = 62) +
        theme_void() +
        labs(title = unique(mxmap$region)) +
        theme(plot.title = element_text(size = 40, family = "Ubuntu", face = "bold", colour = mcv_blacks[3], hjust = 0.5)) +
        coord_map()
    
    ggsave(
        filename = 
            paste0(
                "05_infobites/10_mapas/0", 
                str_replace_all(trimws(str_remove_all(tolower(unique(mxmap$region)), "\\."), "both"), 
                                " ", "_"), ".svg"
            ), width = 23, height = 12, dpi = 200
    )
    
    ggsave(
        filename = 
            paste0(
                "05_infobites/10_mapas/0", 
                str_replace_all(trimws(str_remove_all(tolower(unique(mxmap$region)), "\\."), "both"), 
                                " ", "_"), ".png"
            ), width = 23, height = 12, dpi = 200
    )
    
    
    
    
}
