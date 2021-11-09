#------------------------------------------------------------------------------#
# Proyecto:                   ÍNDICE DE PROGRESO SOCIAL
# Objetivo:                   Fichas por entidades 
#
# Encargada:                  Regina Isabel Medina Rosales     
# Correos:                    regimedina19@gmail.com
# 
# Fecha de creación:          29 de octubre   de 2021
# Última actualización:       04 de noviembre de 2021
#------------------------------------------------------------------------------#

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

# Activar las credenciales de google
# googledrive::drive_auth("regimedina19@gmail.com")
# googlesheets4::gs4_auth("regimedina19@gmail.com")

googledrive::drive_auth("katia@mexicocomovamos.mx")
googlesheets4::gs4_auth("katia@mexicocomovamos.mx")

# Verificar credenciales 
googledrive::drive_user()
googlesheets4::gs4_user()

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x))}


# 1. Importar datos ------------------------------------------------------------

# PIB per cápita 
df_pibr <- read_excel(paste0(inp_pib, "03_pib_per_capita.xlsx"))

# Puntajes de las dimensiones del IPS 
df_ipsr <- read_excel(paste0(inp_ips, "00_IPS_COMPLETE_LONG.xlsx"))

# Puntajes de los componentes del IPS 
df_ips_componentesr <- read_excel(paste0(inp_ips, "00_IPS_COMPLETE_LONG.xlsx"), 
    sheet = "02_IPS_COMP")

# Puntajes de los indicadores específicos del IPS
df_ips_indicadoresr <- read_excel(paste0(inp_ips, "01_ips_long.xlsx"))

# Clasificar grupos de entidades según el PIB (tres grupos)
df_pib  <- df_pibr                                          %>% 
    filter(cve_ent != "00", tipo_pib != "PIB per cápita")   %>% 
    arrange(desc(value_pib))                                %>% 
    mutate(
        grupo_pib = case_when(
                                 value_pib >=  150000 ~ "PIB per cápita alto" , 
            value_pib >= 95000 & value_pib < 150000 ~ "PIB per cápita medio", 
            value_pib <  95000                       ~ "PIB per cápita bajo"), 
        ranking_pib = rank(desc(value_pib)))

table(df_pib$grupo_pib)

# Unir IPS con PIB y hacer rankings (agregado a nivel dimensión)
df_ips <- df_ipsr                                           %>% 
    left_join(df_pib, by  = c("cve_ent"))                   %>% 
    # Dejar solo datos del año más reciente           
    filter(cve_ent != "00", anio == 2020)                   %>% 
    # Variables nacionales 
    group_by(anio, id_dim)                                  %>% 
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
        ips_rank_nacional = dim_rank_nacional, grupo_pib, value_pib, ranking_pib, 
        ips_mean_grupal = dim_mean_grupal, ips_rank_grupal = dim_rank_grupal, 
        ips_fort_deb_gr = dim_fort_deb_gr)


# Unir IPS con PIB y hacer rankings (agregado a nivel indicador)
df_ips_componentes <- df_ips_componentesr                   %>% 
    left_join(df_pib, by = c("cve_ent"))                    %>% 
    # Dejar solo datos del año más reciente                 
    filter(cve_ent != "00", anio == 2020)                   %>% 
    # Varibales nacionales              
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
        ips_rank_nacional = comp_rank_nacional, grupo_pib, value_pib, ranking_pib, 
        ips_mean_grupal = comp_mean_grupal, ips_rank_grupal = comp_rank_grupal, 
        ips_fort_deb_gr = comp_fort_deb_gr)

# Unir IPS con PIB y hacer rankings (agregado a nivel indicador)
df_ips_indicadores <- df_ips_indicadoresr                   %>% 
    left_join(df_pib, by = c("cve_ent"))                    %>% 
    rename(ind_value     = indicador_value)                 %>% 
    # Dejar solo datos del año más reciente                 
    filter(cve_ent != "00", anio == 2020)                   %>% 
    # Varibales nacionales              
    group_by(anio, id_dim_ind)                              %>% 
    mutate(
        ind_mean_nacional = mean(ind_value), 
        ind_rank_nacional = rank(
            desc(ind_value),ties.method = "min"))           %>% 
    # Variables grupales (según grupo del pib)
    group_by(anio, id_dim_ind, grupo_pib)                   %>% 
    mutate(
        ind_mean_grupal   = mean(ind_value), 
        ind_rank_grupal   = rank(desc(ind_value),ties.method = "min"), 
        ind_fort_deb_gr   = case_when(
            ind_value > (quantile(ind_value, 0.5) + sd(ind_value)) ~ "Desempeño superior", 
            ind_value < (quantile(ind_value, 0.5) - sd(ind_value)) ~ "Desempeño inferior", 
            T ~ "Desempeño esperado"))                      %>% 
    ungroup() %>% 
    # Agregar nombre de los indicadores
    mutate(
        ind_name = case_when(
            id_dim_ind == "0101" ~ "Carencia por acceso a la alimentación (% de personas)", 
            id_dim_ind == "0102" ~ "Mortalidad materna (muertes / 100,000 nacidxs vivxs)", 
            id_dim_ind == "0103" ~ "Mortalidad infantil (muertes / 100,000 nacidxs vivxs)",
            id_dim_ind == "0104" ~ "Mortalidad por enfermedades infecciosas (muertes / 100,000 habitantes)", 
            id_dim_ind == "0105" ~ "Hogares con disponibilidad de agua dentro de la vivienda (%)", 
            id_dim_ind == "0106" ~ "Hogares con dotación diaria de agua (%)",
            id_dim_ind == "0107" ~ "Hogares con servicio sanitario exclusivo para la vivienda (%)",
            id_dim_ind == "0108" ~ "Hogares con paredes material frágil (%)",
            id_dim_ind == "0109" ~ "Hogares con piso de tierra (%)",
            id_dim_ind == "0110" ~ "Hogares que cocinan con leña o carbón (%)",
            id_dim_ind == "0111" ~ "Hogares en hacinamiento (%)",
            id_dim_ind == "0112" ~ "Homicidios (muertes / 100,000 habitantes)",
            id_dim_ind == "0113" ~ "Muertes por accidentes dde tráfico (accidente con muertes / 100,000 habitantes)", 
            id_dim_ind == "0114" ~ "Nivel de crimen violento (1 = bajo, 5 = alto)", 
            id_dim_ind == "0115" ~ "Presencia de crimen organizado (1 = bajo, 5 = alto)",
            id_dim_ind == "0116" ~ "Inseguridad percibida (% de población adulta)", 
            id_dim_ind == "0217" ~ "Matriculación educación preescolar (%)", 
            id_dim_ind == "0218" ~ "Analfabetismo (% población)",
            id_dim_ind == "0219" ~ "Matriculación educación primaria (%)", 
            id_dim_ind == "0220" ~ "Matriculación educación secundaria (%)", 
            id_dim_ind == "0221" ~ "Paridad de género en educación secundaria (diferencia absoluta de 1)",
            id_dim_ind == "0222" ~ "Usuarios de telefonía móvil (% población)",
            id_dim_ind == "0223" ~ "Hogares con computadoras (%)",
            id_dim_ind == "0224" ~ "Hogares con conexión a internet (%)",
            id_dim_ind == "0225" ~ "Tasa de agresión a periodistas (agresión / 100,000)", 
            id_dim_ind == "0226" ~ "Esperanza de vida (años)", 
            id_dim_ind == "0227" ~ "Tasa de suicidios (suicidios / 100,000)",
            id_dim_ind == "0228" ~ "Mortalidad por enfermedades circulatorias (muertes / 100,000)", 
            id_dim_ind == "0229" ~ "Mortalidad por diabetes (muertes / 100,000)", 
            id_dim_ind == "0230" ~ "Tasa de obesidad (% de la población)",
            id_dim_ind == "0231" ~ "Grado de presión del agua", 
            id_dim_ind == "0232" ~ "Enterrar o quemar basura (% hogares)", 
            id_dim_ind == "0233" ~ "Satisfacción con áreas verdes (% adultos)",
            id_dim_ind == "0234" ~ "Uso de focos ahorradores (% hogares)", 
            id_dim_ind == "0335" ~ "Hogares con titulo de propiedad (%)", 
            id_dim_ind == "0336" ~ "Participación electoral (% de lista nominal)",
            id_dim_ind == "0337" ~ "Interacción con gobierno electrónico (%)", 
            id_dim_ind == "0338" ~ "Participación ciudadana en el gobierno (# espacios)", 
            id_dim_ind == "0339" ~ "Percepción de corrupción en instituciones que imparten justicia (%)",
            id_dim_ind == "0340" ~ "Jóvenes que no estudian ni trabajan (% población de 15 a 24 años)", 
            id_dim_ind == "0341" ~ "Embarazo adolescente (tasa por cada 100,000 nacidxs vivxs)", 
            id_dim_ind == "0342" ~ "Incidencia de corrupción (incidencia / 100,000)",
            id_dim_ind == "0343" ~ "Informalidad laboral (% de la población ocupada)", 
            id_dim_ind == "0344" ~ "Tiempo de traslado (% de la población ocupada que toma > 2 hrs)", 
            id_dim_ind == "0345" ~ "Confianza en los vecinos (%)",
            id_dim_ind == "0346" ~ "Paridad de género en congresos locales (diferencia absoluta de 1)", 
            id_dim_ind == "0347" ~ "Inclusión personas LGBT+ (% que aprueba el matrimonio igualitario)", 
            id_dim_ind == "0348" ~ "Tasa de analfabetización en personas indígenas",
            id_dim_ind == "0349" ~ "Tasa de analfabetización en personas con discapacidad", 
            id_dim_ind == "0350" ~ "Absorción en eduación superior (% sigue estudios)", 
            id_dim_ind == "0351" ~ "Cobertura educación superior (% pob 18-23 años)",
            id_dim_ind == "0352" ~ "Escolaridad promedio mujeres (# años)", 
            id_dim_ind == "0353" ~ "Paridad de género en posgrado (diferencia absoluta de 1)", 
            id_dim_ind == "0354" ~ "Paridad de género en licenciatura (diferencia absoluta de 1)",
            id_dim_ind == "0355" ~ "Posgrados nacionales de calidad (# / 100,000 habitantes)"), 
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
        anio, cve_ent, entidad_abr_m, nivel, id = id_dim_ind, name = ind_name, 
        value = ind_value, ips_mean_nacional = ind_mean_nacional, 
        ips_rank_nacional = ind_rank_nacional, grupo_pib, value_pib, ranking_pib, 
        ips_mean_grupal = ind_mean_grupal, ips_rank_grupal = ind_rank_grupal, 
        ips_fort_deb_gr = ind_fort_deb_gr)

# Unir base de dimensiones e indicadores 
df_ranking_ips <- df_ips            %>%
    bind_rows(df_ips_componentes)   %>% 
    bind_rows(df_ips_indicadores)   %>% 
    arrange(cve_ent) %>% 
    mutate(value = abs(value))

# 3. Guardar -------------------------------------------------------------------

openxlsx::write.xlsx(df_ranking_ips, "03_ips_clean/08_ips_ranking.xlsx", overwrite = T)

# 4. Hacer tablas --------------------------------------------------------------


grupos <- unique(df_ranking_ips$grupo_pib)



for(x in 1:length(grupos)){
    
    print(grupos[x])
    
    tempo_grupo <- df_ranking_ips %>%
        filter(grupo_pib==grupos[x]) %>% 
        filter(!id == "020831") %>% 
        glimpse
    cves <- unique(tempo_grupo$cve_ent)
    
    for(i in 1:length(cves)){
        
        
        
        tempo_ent <- tempo_grupo %>% 
            filter(cve_ent==cves[i]) %>% 
            glimpse
        
        name_ent <- unique(tempo_ent$entidad_abr_m)
        
        print(name_ent)
        
        ips_ent <- round(tempo_ent$value[tempo_ent$id=="00"],2)
        ips_ent_rank <- tempo_ent$ips_rank_nacional[tempo_ent$id=="00"]
        ips_ent_fort_deb <- ifelse(
            unique(tempo_ent$value[tempo_ent$id=="00"]) > as.numeric(quantile(unique(tempo_grupo$value[tempo_ent$id=="00"]), 0.5)+sd(unique(tempo_grupo$value[tempo_ent$id=="00"]))),
            "Desempeño superior", ifelse(
                unique(tempo_ent$value[tempo_ent$id=="00"]) < as.numeric(quantile(unique(tempo_grupo$value[tempo_ent$id=="00"]), 0.5)-sd(unique(tempo_grupo$value[tempo_ent$id=="00"]))),
                "Desempeño inferior", "Desempeño esperado"
            )
        )
        pib_p_cap <- paste0("$", prettyNum(round(unique(tempo_ent$value_pib)), big.mark = ","))
        pib_p_cap_rank <- unique(tempo_ent$ranking_pib)
        
        tempo_table <- tempo_ent %>% 
            select(-c(anio, cve_ent, entidad_abr_m, ips_mean_nacional:ips_mean_grupal)) %>% 
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
                Posición = ips_rank_grupal,
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