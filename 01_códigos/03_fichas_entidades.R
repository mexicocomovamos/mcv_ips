#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Fichas por entidades 
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          29 de octubre de 2021
# Última actualización:       20 de octubre de 2022
#------------------------------------------------------------------------------#

# Nota: Modifiqué el código en 2022 para generar una copia del ranking que 
# tuviera datos para todos los años y así poder hacer los heatmapas de los 
# componentes del IPS. Para esto comenté el filtro del año del procesamiento
# inicial y lo añadí hasta el final en el que se guardan las bases. 

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
require(pacman)
p_load(readxl, tidyverse, googledrive, 
    googlesheets4, lubridate, janitor, beepr,
    gt, data.table, formattable, tidyr)

# Desactiva notación científica
options(scipen=999)

# Vaciar espacio de trabajo 
rm(list=ls())

# Colores MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados
ips_discrete <- c(
    '#00B7CE', '#F7943F', '#ACCC4A', '#5A494E', '#C64736'
)

# Vectores para directorio 
inp_pib <- "02_datos_crudos/"
inp_ips <- "03_ips_clean/"


# 1. Importar datos ------------------------------------------------------------

# PIB per cápita 
# df_pibr <- read_excel(paste0(inp_pib, "03_pib_per_capita.xlsx")) %>%
#     glimpse

df_pibr <- read_excel("02_datos_crudos/03_pib_per_capita.xlsx") %>%
    rename(value_pib_pc = PIB_pc)

# Puntajes de las dimensiones del IPS 
df_ipsr <- read_excel(paste0(inp_ips, "00_IPS_COMPLETE_LONG.xlsx"))

# Puntajes de los componentes del IPS 
df_ips_componentesr <- read_excel(paste0(inp_ips, "00_IPS_COMPLETE_LONG.xlsx"), 
    sheet = "02_IPS_COMP")

# Puntajes de los indicadores específicos del IPS
df_ips_indicadoresr <- read_excel(paste0(inp_ips, "01_ips_long.xlsx"))
# Correccion para multiplicaciones x 100
# df_ips_indicadoresr <- rbind(df_ips_indicadoresr %>% filter(id_dim_ind == "0346") %>% mutate(indicador_value = indicador_value*100),
#       df_ips_indicadoresr %>% filter(id_dim_ind != "0346")) %>% 
#     arrange(id_dim_ind)

qua <- df_pibr %>% 
    filter(cve_ent != "00") %>% 
    filter(anio == 2022) %>% 
    pull(value_pib_pc) %>% 
    quantile(c(0.333, 0.6666, 1))

umbral_bajo <- qua[1]
umbral_medio <- qua[2]

# Clasificar grupos de entidades según el PIB (tres grupos)
df_pib  <- df_pibr                                          %>% 
            filter(cve_ent != "00",
                   # tipo != "Serie desestacionalizada",
                   anio == 2022)   %>% 
            mutate(
                anio = 2023,
                grupo_pib = case_when(
                    value_pib_pc >=  umbral_medio ~ "PIB per cápita alto" , 
                    value_pib_pc >= umbral_bajo & value_pib_pc <   umbral_medio ~ "PIB per cápita medio", 
                    value_pib_pc <  umbral_bajo                        ~ "PIB per cápita bajo")) %>% 
            select(cve_ent, grupo_pib, value_pib_pc) %>% 
    mutate(ranking_pib   = rank(desc(value_pib_pc)))
    
table(df_pib$grupo_pib)

# Unir IPS con PIB y hacer rankings (agregado a nivel dimensión)
df_ips <- df_ipsr                                           %>% 
    filter(anio == 2023) %>% 
    filter(cve_ent != "00")                   %>% 
    left_join(df_pib, by  = c("cve_ent"))           %>% 
    group_by(id_dim)                                  %>% 
    mutate(
        dim_mean_nacional = mean(dim_value), 
        dim_rank_nacional = rank(desc(dim_value)))          %>% 
    # Variables grupales (según grupo del pib de la entidad)            
    group_by(anio, grupo_pib, id_dim)                       %>% 
    mutate(
        dim_mean_grupal   = mean(dim_value), 
        dim_rank_grupal   = rank(desc(dim_value)), 
        dim_fort_deb_gr   = case_when(
            dim_value > (quantile(dim_value, 0.5) + sd(dim_value)) ~ "Desempeño superior", 
            dim_value < (quantile(dim_value, 0.5) - sd(dim_value)) ~ "Desempeño inferior", 
            T ~ "Desempeño esperado"), 
        dim_name = case_when(
            id_dim == "01" ~ "Necesidades Humanas Básicas", 
            id_dim == "02" ~ "Fundamentos del Bienestar", 
            id_dim == "03" ~ "Oportunidades", 
            id_dim == "00" ~ "Índice de Progreso Social"), 
        nivel = "Dimensión")                                %>% 
    ungroup() %>% 
    select(
        anio, cve_ent, entidad_abr_m, nivel, id = id_dim, name = dim_name, 
        value = dim_value, ips_mean_nacional = dim_mean_nacional, 
        ips_rank_nacional = dim_rank_nacional, grupo_pib, value_pib_pc, ranking_pib, 
        ips_mean_grupal = dim_mean_grupal, ips_rank_grupal = dim_rank_grupal, 
        ips_fort_deb_gr = dim_fort_deb_gr) %>% 
    glimpse


# Unir IPS con PIB y hacer rankings (agregado a nivel indicador)
df_ips_componentes <- df_ips_componentesr                   %>% 
    filter(anio == 2023) %>% 
    filter(cve_ent != "00")                                 %>% 
    left_join(df_pib, by  = c("cve_ent"))           %>% 
    # Dejar solo datos del año más reciente                 
    group_by(anio, id_comp)                                 %>% 
    mutate(
        comp_mean_nacional = mean(comp_value), 
        comp_rank_nacional = rank(
            desc(comp_value),ties.method = "min"))          %>% 
    # Variables grupales (según grupo del pib)
    group_by(anio, id_comp, grupo_pib)                      %>% 
    mutate(
        comp_mean_grupal   = mean(comp_value), 
        comp_rank_grupal   = rank(desc(comp_value),ties.method = "min"), 
        comp_fort_deb_gr   = case_when(
            comp_value > (quantile(comp_value, 0.5) + sd(comp_value)) ~ "Desempeño superior", 
            comp_value < (quantile(comp_value, 0.5) - sd(comp_value)) ~ "Desempeño inferior", 
            T ~ "Desempeño esperado"))                      %>% 
    ungroup()                                               %>% 
    # Agregar nombre de los componentes 
    mutate(
        id_comp = str_sub(id_comp, 4, 5),
        comp_name = case_when(
            id_comp == "01" ~ "Nutrición y cuidados médicos básicos", 
            id_comp == "02" ~ "Agua y saneamiento", 
            id_comp == "03" ~ "Vivienda", 
            id_comp == "04" ~ "Seguridad personal", 
            id_comp == "05" ~ "Acceso a conocimientos básicos", 
            id_comp == "06" ~ "Acceso a información y comunicaciones", 
            id_comp == "07" ~ "Salud y bienestar", 
            id_comp == "08" ~ "Calidad medioambiental", 
            id_comp == "09" ~ "Derechos personales", 
            id_comp == "10" ~ "Libertad personal y de elección", 
            id_comp == "11" ~ "Inclusión", 
            id_comp == "12" ~ "Acceso a educación superior"), 
        id_comp = case_when(
            id_comp == "01" ~ "0101", 
            id_comp == "02" ~ "0102", 
            id_comp == "03" ~ "0103", 
            id_comp == "04" ~ "0104", 
            id_comp == "05" ~ "0205", 
            id_comp == "06" ~ "0206", 
            id_comp == "07" ~ "0207", 
            id_comp == "08" ~ "0208", 
            id_comp == "09" ~ "0309", 
            id_comp == "10" ~ "0310", 
            id_comp == "11" ~ "0311", 
            id_comp == "12" ~ "0312"), 
        nivel = "Componente")                               %>% 
    select(
        anio, cve_ent, entidad_abr_m, nivel, id = id_comp, name = comp_name, 
        value = comp_value, ips_mean_nacional = comp_mean_nacional, 
        ips_rank_nacional = comp_rank_nacional, grupo_pib, value_pib_pc, ranking_pib, 
        ips_mean_grupal = comp_mean_grupal, ips_rank_grupal = comp_rank_grupal, 
        ips_fort_deb_gr = comp_fort_deb_gr) %>% 
    glimpse


# 1
df_ind_names <- metadatos %>% 
    # imp_dv(v_id, 2) %>% 
    select(id_dimension:id_indicador, indicador_name, direccion) %>% 
    mutate(id_dim_ind = paste0(id_dimension, id_indicador)) %>% 
    select(id_dim_ind, indicador_name, direccion) %>% 
    glimpse
# 1
df_ips_indicadores <- df_ips_indicadoresr                   %>% 
    filter(anio == 2023) %>% 
    filter(cve_ent != "00")                                 %>% 
    left_join(df_pib, by  = c("cve_ent"))           %>% 
    rename(ind_value     = indicador_value)                 %>% 
    left_join(df_ind_names) %>% 
    # Dejar solo datos del año más reciente                 
    filter(cve_ent != "00")                   %>% 
    mutate(ind_value_direccion = ind_value*direccion) %>% 
    # Varibales nacionales              
    group_by(anio, id_dim_ind)                              %>% 
    mutate(
        ind_mean_nacional = mean(ind_value), 
        ind_rank_nacional = rank(
            desc(ind_value_direccion),ties.method = "min"))           %>% 
    # Variables grupales (según grupo del pib)
    group_by(anio, id_dim_ind, grupo_pib)                   %>% 
    mutate(
        ind_mean_grupal   = mean(ind_value), 
        ind_rank_grupal   = rank(desc(ind_value_direccion),ties.method = "min"), 
        ind_fort_deb_gr   = case_when(
            direccion == 1 & ind_value_direccion > (quantile(ind_value_direccion, 0.5) + sd(ind_value_direccion)) ~ "Desempeño superior", 
            direccion == 1 & ind_value_direccion < (quantile(ind_value_direccion, 0.5) - sd(ind_value_direccion)) ~ "Desempeño inferior",
            direccion == -1 & ind_value_direccion > (quantile(ind_value_direccion, 0.5) + sd(ind_value_direccion)) ~ "Desempeño superior", 
            direccion == -1 & ind_value_direccion < (quantile(ind_value_direccion, 0.5) - sd(ind_value_direccion)) ~ "Desempeño inferior", 
            T ~ "Desempeño esperado"))                      %>% 
    ungroup() %>% 
    # Agregar nombre de los indicadores
    mutate(
        id_dim_ind = case_when(
            id_dim_ind == "0101" ~ "010101",
            id_dim_ind == "0102" ~ "010102",
            id_dim_ind == "0103" ~ "010103",
            id_dim_ind == "0104" ~ "010104",
            id_dim_ind == "0105" ~ "010205",
            id_dim_ind == "0106" ~ "010206",
            id_dim_ind == "0107" ~ "010207",
            id_dim_ind == "0108" ~ "010308",
            id_dim_ind == "0109" ~ "010309",
            id_dim_ind == "0110" ~ "010310",
            id_dim_ind == "0111" ~ "010311",
            id_dim_ind == "0112" ~ "010412",
            id_dim_ind == "0113" ~ "010413",
            id_dim_ind == "0114" ~ "010414",
            id_dim_ind == "0115" ~ "010415",
            id_dim_ind == "0116" ~ "010416",
            id_dim_ind == "0217" ~ "020517",
            id_dim_ind == "0218" ~ "020518",
            id_dim_ind == "0219" ~ "020519",
            id_dim_ind == "0220" ~ "020520",
            id_dim_ind == "0221" ~ "020521",
            id_dim_ind == "0222" ~ "020622",
            id_dim_ind == "0223" ~ "020623",
            id_dim_ind == "0224" ~ "020624",
            id_dim_ind == "0225" ~ "020625",
            id_dim_ind == "0226" ~ "020726",
            id_dim_ind == "0227" ~ "020727",
            id_dim_ind == "0228" ~ "020728",
            id_dim_ind == "0229" ~ "020729",
            id_dim_ind == "0230" ~ "020730",
            id_dim_ind == "0231" ~ "020831",
            id_dim_ind == "0232" ~ "020832",
            id_dim_ind == "0233" ~ "020833",
            id_dim_ind == "0234" ~ "020834",
            id_dim_ind == "0256" ~ "020856",
            id_dim_ind == "0335" ~ "030935",
            id_dim_ind == "0336" ~ "030936",
            id_dim_ind == "0337" ~ "030937",
            id_dim_ind == "0338" ~ "030938",
            id_dim_ind == "0339" ~ "030939",
            id_dim_ind == "0340" ~ "031040",
            id_dim_ind == "0341" ~ "031041",
            id_dim_ind == "0342" ~ "031042",
            id_dim_ind == "0343" ~ "031043",
            id_dim_ind == "0344" ~ "031044",
            id_dim_ind == "0345" ~ "031145",
            id_dim_ind == "0346" ~ "031146",
            id_dim_ind == "0347" ~ "031147",
            id_dim_ind == "0348" ~ "031148",
            id_dim_ind == "0349" ~ "031149",
            id_dim_ind == "0350" ~ "031250",
            id_dim_ind == "0351" ~ "031251",
            id_dim_ind == "0352" ~ "031252",
            id_dim_ind == "0353" ~ "031253",
            id_dim_ind == "0354" ~ "031254",
            id_dim_ind == "0355" ~ "031255",
        ),
        nivel = "Indicador", 
        id_ind = str_sub(id_dim_ind, 3, 4)) %>% 
    select(
        anio, cve_ent, entidad_abr_m, nivel, id = id_dim_ind, name = indicador_name, 
        value = ind_value, ips_mean_nacional = ind_mean_nacional, 
        ips_rank_nacional = ind_rank_nacional, grupo_pib, value_pib_pc, ranking_pib, 
        ips_mean_grupal = ind_mean_grupal, ips_rank_grupal = ind_rank_grupal, 
        ips_fort_deb_gr = ind_fort_deb_gr) %>% 
    glimpse

# Unir base de dimensiones e indicadores 
df_ranking_ips_all <- df_ips        %>%
    bind_rows(df_ips_componentes)   %>% 
    bind_rows(df_ips_indicadores)   %>%
    arrange(cve_ent) %>% 
    mutate(value = abs(value)) %>% 
    glimpse

# 3. Guardar -------------------------------------------------------------------

# Filtrar solo datos del último año 
df_ranking_ips <-  df_ranking_ips_all %>% 
    filter(anio == 2023) %>% 
    glimpse

openxlsx::write.xlsx(df_ranking_ips, "03_ips_clean/08_ips_ranking.xlsx", overwrite = T)

# Guardar copia con todos los años
openxlsx::write.xlsx(df_ranking_ips_all, "03_ips_clean/08_02_ips_ranking_series.xlsx", overwrite = T)

# 4. Hacer tablas --------------------------------------------------------------

df_ranking_ips <- df_ranking_ips %>%
    select(-entidad_abr_m) %>% 
    left_join(
        readxl::read_excel("02_datos_crudos/00_cve_ent.xlsx") %>% 
            select(cve_ent, entidad)
    ) %>% 
    select(anio, cve_ent, entidad, everything()) %>% 
    # BORRAMOS INDICADORES NO UTILIZADOS EN ESTA EDICIÓN
    filter(!(id %in% c("010104", "020727", "020831", "030939", "031253")))

grupos <- unique(df_ranking_ips$grupo_pib)

# Revisar! 
# df_ranking_ips$value[df_ranking_ips$id == "031251"] <- df_ranking_ips$value[df_ranking_ips$id == "031251"]*100

# x = 1
for(x in 1:length(grupos)){
    
    print(grupos[x])
    
    tempo_grupo <- df_ranking_ips %>%
        filter(grupo_pib==grupos[x]) %>% 
        glimpse
    cves <- unique(tempo_grupo$cve_ent)
    
    # i = 1
    for(i in 1:length(cves)){
        tempo_ent <- tempo_grupo %>% 
            filter(cve_ent==cves[i]) %>% 
            drop_na(value) %>% 
            glimpse
        
        name_ent <- unique(tempo_ent$entidad)
        
        print(name_ent)
        
        ips_ent <- round(tempo_ent$value[tempo_ent$id=="00"],2)
        ips_ent_rank <- tempo_ent$ips_rank_nacional[tempo_ent$id=="00"]
        ips_ent_fort_deb <- ifelse(
            unique(tempo_ent$value[tempo_ent$id=="00"])[1] > as.numeric(quantile(unique(tempo_grupo$value[tempo_ent$id=="00"])[1], 0.5)+sd(unique(tempo_grupo$value[tempo_ent$id=="00"])[1])),
            "Desempeño superior", ifelse(
                unique(tempo_ent$value[tempo_ent$id=="00"])[1] < as.numeric(quantile(unique(tempo_grupo$value[tempo_ent$id=="00"])[1], 0.5)-sd(unique(tempo_grupo$value[tempo_ent$id=="00"])[1])),
                "Desempeño inferior", "Desempeño esperado"
            )
        )
        pib_p_cap <- paste0("$", prettyNum(round(unique(tempo_ent$value_pib_pc)), big.mark = ","))
        pib_p_cap_rank <- unique(tempo_ent$ranking_pib)
        
        tempo_table <- tempo_ent %>% 
            select(nivel, id, name, value, ips_rank_nacional, ips_fort_deb_gr) %>% 
            #filter(!id=="00") %>% 
            arrange(id) %>% 
            glimpse
        
        tempo_table_id <- tempo_table %>% 
            #filter(str_starts(id, "01")) %>% 
            mutate(
                value = ifelse(value>=1, prettyNum(round(value, 1), big.mark = ","), prettyNum(round(value, 3), big.mark = ",")),
                ips_fort_deb_gr = ifelse(ips_fort_deb_gr == "Desempeño superior", 1,
                                         ifelse(ips_fort_deb_gr == "Desempeño esperado", 2, 3))
            ) %>% 
            rename(
                ` ` = name,
                Puntaje = value, 
                Posición = ips_rank_nacional,
                `Fortaleza/debilidad` = ips_fort_deb_gr
            ) %>% 
            glimpse
        
        
        tempo_table_id %>% 
            gt %>% 
            data_color(
                columns = `Fortaleza/debilidad`,
                colors = c(mcv_semaforo[1], mcv_semaforo[3], mcv_semaforo[4])
            ) %>% 
            tab_style(
                style = list(
                    cell_text(weight = "bold", size = "x-large", align = "center")
                ),
                locations = cells_body(
                    columns = c(` `, "Puntaje", "Posición"),
                    rows = nchar(id) == 2
                )
            ) %>% 
            tab_style(
                style = list(
                    cell_text(weight = "bold", size = "large", align = "center")
                ),
                locations = cells_body(
                    columns = c(` `, "Puntaje", "Posición"),
                    rows = nchar(id) == 4
                )
            ) %>% 
            tab_style(
                style = list(
                    cell_text(align = "right")
                ),
                locations = cells_body(
                    columns = ` `,
                    rows = nchar(id) == 6
                )
            ) %>% 
            tab_style(
                style = list(
                    cell_fill(color = ips_discrete[1], alpha = 0.5)
                ),
                locations = cells_body(
                    columns = c(` `, "Puntaje", "Posición"),
                    rows = id == "01"
                ) 
            ) %>%
            tab_style(
                style = list(
                    cell_fill(color = ips_discrete[1], alpha = 0.3)
                ),
                locations = cells_body(
                    columns = c(` `, "Puntaje", "Posición"),
                    rows = (nivel == "Componente" & str_sub(id, 1, 2) == "01")
                ) 
            )  %>% 
            tab_style(
                style = list(
                    cell_fill(color = ips_discrete[2], alpha = 0.5)
                ),
                locations = cells_body(
                    columns = c(` `, "Puntaje", "Posición"),
                    rows = id == "02"
                ) 
            ) %>%
            tab_style(
                style = list(
                    cell_fill(color = ips_discrete[2], alpha = 0.3)
                ),
                locations = cells_body(
                    columns = c(` `, "Puntaje", "Posición"),
                    rows = (nivel == "Componente" & str_sub(id, 1, 2) == "02")
                ) 
            ) %>%
            tab_style(
                style = list(
                    cell_fill(color = ips_discrete[3], alpha = 0.7)
                ),
                locations = cells_body(
                    columns = c(` `, "Puntaje", "Posición"),
                    rows = id == "03"
                ) 
            ) %>%
            tab_style(
                style = list(
                    cell_fill(color = ips_discrete[3], alpha = 0.3)
                ),
                locations = cells_body(
                    columns = c(` `, "Puntaje", "Posición"),
                    rows = (nivel == "Componente" & str_sub(id, 1, 2) == "03")
                ) 
            ) %>%
            tab_style(
                style = list(
                    cell_text(align = "center")
                ),
                locations = cells_body(
                    columns = c("Puntaje", "Posición", "Fortaleza/debilidad")
                )
            ) %>% 
            tab_style(
                style = list(
                    cell_text(color = mcv_semaforo[1])
                ),
                locations = cells_body(
                    columns = c("Fortaleza/debilidad"),
                    rows = `Fortaleza/debilidad` == 1
                )
            ) %>%
            tab_style(
                style = list(
                    cell_text(color = mcv_semaforo[3])
                ),
                locations = cells_body(
                    columns = c("Fortaleza/debilidad"),
                    rows = `Fortaleza/debilidad` == 2
                )
            ) %>%
            tab_style(
                style = list(
                    cell_text(color = mcv_semaforo[4])
                ),
                locations = cells_body(
                    columns = c("Fortaleza/debilidad"),
                    rows = `Fortaleza/debilidad` == 3
                )
            ) %>%
            tab_header(
                title = md(name_ent),
                subtitle = md(paste0("PIB per cápita ", pib_p_cap, " [", pib_p_cap_rank,"/32]"))
            ) %>%
            tab_options(
                heading.title.font.size = "xx-large",
                heading.title.font.weight = "bold",
                heading.subtitle.font.size = "x-large",
                heading.subtitle.font.weight = "italic",
            ) %>% 
            cols_hide(c(nivel, id)) %>% 
            gtsave(paste0("06_fichas_entidades/0", x, "_", unique(tempo_ent$cve_ent), "_", name_ent, ".html"))
    }
}


# FIN. -------------------------------------------------------------------------
