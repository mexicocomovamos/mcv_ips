#------------------------------------------------------------------------------#
# Proyecto:               ÍNDICE DE PROGRESO SOCIAL
# Objetivo:               Análisis por grupos del PIB (fichas, cambios y mapas)
#
# Encargada:              Regina Isabel Medina Rosales     
# Correos:                regimedina19@gmail.com
# 
# Fecha de creación:      2 de noviembre de 2022
# Última actualización:   3 de noviembre de 2022
#------------------------------------------------------------------------------#

# Fuente API del INEGI: https://www.inegi.org.mx/servicios/api_indicadores.html 

# 0. Configuración inicial -----------------------------------------------------

Sys.setlocale("LC_TIME", "es_ES")

# Cargar paquetería 
# devtools::install_github("munichrocker/DatawRappr")

require(pacman)
p_load(readxl, inegiR, tidyverse, dplyr, DatawRappr,
       googledrive, googlesheets4, lubridate, janitor, beepr)

# Desactiva notación científica
options(scipen=999)

# Vaciar espacio de trabajo 
rm(list=ls())

# Colores MCV
mcv_discrete <- c("#6950d8", "#3CEAFA", "#00b783", "#ff6260", "#ffaf84", "#ffbd41")
mcv_semaforo <- c("#00b783", "#E8D92E", "#ffbd41", "#ff6260") # Verde, amarillo, naranja y rojo
mcv_blacks   <- c("black"  , "#D2D0CD", "#777777")            # Negros
mcv_morados  <- c("#6950D8", "#A99BE9")                       # Morados

# Vectores para directorio 
inp <- "02_datos_crudos/"

# # Activar las credenciales de google
v_usuaria <- "regina"
# v_usuaria <- "katia"

googledrive::drive_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))
googlesheets4::gs4_auth(paste0(v_usuaria, "@mexicocomovamos.mx"))


# Verificar credenciales
googledrive::drive_user()
googlesheets4::gs4_user()

# Función para importar de manera más corta desde drive
imp_dv <- function(x){
    googlesheets4::read_sheet(
        paste0("https://docs.google.com/spreadsheets/d/", x))}

# ----- Funciones de importación y exportación
paste_inp <- function(x){paste0("03_ips_clean/"       , x)}
paste_out <- function(x){paste0("02_bases_procesadas/", x)}


# 1. Importar datos ------------------------------------------------------------

# ---- IPS 2021
df_ips2021 <- read_excel(paste_inp("08_ips_ranking.xlsx"))

# ---- Bases históricas
# Dimensiones
df_ips <- read_excel(paste_inp("00_IPS_COMPLETE_LONG.xlsx"))

# Variables
# df_ips <- read_excel(paste_inp("01_ips_long.xlsx"))

# Dimensiones, componentes e indicadores (variables)
# df_ips <- read_excel(paste_inp("08_02_ips_ranking_series.xlsx"))

# 2. Procesar datos ------------------------------------------------------------

# ---- Identificadores de entidad, PIB e IPS
df_ips1 <- df_ips2021 %>% 
    # IPS global
    filter(id == "00") %>% 
    # Estimar ranking del IPS
    arrange(desc(value)) %>% 
    mutate(r_ips = 1:32) %>% 
    select(anio, cve_ent, entidad_abr_m, nivel, id, ips_2021 = value,
           r_ips, pib_pc = value_pib_pc, r_pib_pc = ranking_pib) 
        

# ---- Ingreso laboral 
# ing_lab	
# r_ing_lab
df_ingresos <- readRDS("02_datos_crudos/2021T4_ingresos_imputados.rds") %>% 
    mutate(cve_ent = str_pad(ent, 2, "l", "0")) %>% 
    select(cve_ent, ingreso_sm, factor) %>% 
    srvyr::as_survey_design(weights = factor) %>% 
    group_by(cve_ent) %>% 
    summarise(ing_lab = srvyr::survey_mean(ingreso_sm, na.rm = T)) %>% 
    ungroup() %>% 
    select(-ing_lab_se) %>% 
    arrange(desc(ing_lab)) %>% 
    mutate(r_ing_lab = 1:32) %>% 
    glimpse

# ---- Pobreza laboral 
df_pob_lab <- readxl::read_excel("02_datos_crudos/02_04_pobreza_laboral_complete_long.xlsx") %>% 
    filter(periodo == "2021T4", sexo == "General") %>% 
    select(anio, cve_ent, pob_lab = tlp) %>% 
    arrange(pob_lab) %>% 
    mutate(r_pob_lab = 1:32) %>% 
    glimpse 

# ---- Acceso a salud 
# acc_salud	
# r_acc_salud
df_censo_at_med <- tribble(
    ~cve_ent, ~tot_pob, ~tot_no_afiliada, ~tot_no_esp,
    "01",   1425607,   262088, 2380,
    "02",   3769020,   836317, 27438,
    "03",   798447,   129270, 5055,
    "04",   928363,   203304, 5382,
    "05",   3146771,   597373, 8690,
    "06",   731391,   123074, 2370,
    "07",   5543828,  1814782, 30383,
    "08",   3741869,   574108, 11467,
    "09",   9209944,  2502789, 18143,
    "10",   1832650,   461394, 4591,
    "11",   6166934,  1275190, 17083,
    "12",   3540685,   891961, 16720,
    "13",   3082841,   928550, 4918,
    "14",   8348151,  2452519, 59922,
    "15",   16992418,  5672574, 52167,
    "16",   4748846,  1784629, 9661,
    "17",   1971520,   552170, 2466,
    "18",   1235456,   273039, 1854,
    "19",   5784442,  1072664, 30313,
    "20",   4132148,  1215990, 11455,
    "21",   6583278,  1921945, 10351,
    "22",   2368467,   486467, 8840,
    "23",   1857985,   468580, 24077,
    "24",   2822255,   490284, 4519,
    "25",   3026943,   575115, 4313,
    "26",   2944840,   547727, 5634,
    "27",   2402598,   755484, 2025,
    "28",   3527735,   713859, 10469,
    "29",   1342977,   356641, 21737,
    "30",   8062579,  2228480, 8566,
    "31",   2320898,   505108, 5669,
    "32",   1622138,   326238, 2841
) %>% 
    mutate(
        tot_afiliada = tot_pob-tot_no_afiliada-tot_no_esp,
        prop_afiliada = tot_afiliada/tot_pob
    ) %>% 
    arrange(desc(prop_afiliada)) %>% 
    mutate(r_acc_salud = 1:32) %>% 
    select(cve_ent, acc_salud = prop_afiliada, r_acc_salud) %>% 
    glimpse()

# ---- Escolaridad 
# escolaridad	
# r_escolaridad	

load("02_datos_crudos/df_enoe_reciente.RData")

df_escolaridad <- df_enoe_reciente %>% 
    select(cve_ent = ent, anios_esc, fac) %>% 
    mutate(cve_ent = str_pad(cve_ent, 2, "l", "0")) %>% 
    srvyr::as_survey_design(weights = fac) %>% 
    group_by(cve_ent) %>% 
    summarise(escolaridad = srvyr::survey_mean(anios_esc, na.rm = T, vartype = "ci")) %>% 
    arrange(desc(escolaridad)) %>% 
    mutate(r_escolaridad = 1:32) %>% 
    select(cve_ent, escolaridad, r_escolaridad) %>% 
    glimpse


# ---- Carreteras 
df_carreteras <- 
    tribble(
        ~cve_ent,	~entidad_abr_m,	~carreteras_km,	~area,
        "00",	"Nacional",	15613,	1960189,
        "01",	"AGS",	    145,	5615.70,
        "02",	"BC",	    413,	71450,
        "03",	"BCS",	    185,	73909.40,
        "04",	"CAMP",	    169,	57484.90,
        "05",	"COAH",	    865,	151594.80,
        "06",	"COL",	    146,	5626.90,
        "07",	"CHPS",	    312,	73311,
        "08",	"CHIH",	    893,	247412.60,
        "09",	"CDMX",	    61,	    1494.30,
        "10",	"DGO",	    489,	123364,
        "11",	"GTO",	    639,	30606.70,
        "12",	"GRO",	    315,	63595.90,
        "13",	"HGO",	    592,	20821.40,
        "14",	"JAL",	    1442,	78595.90,
        "15",	"MEX",	    1049,	22351.80,
        "16",	"MICH",	    458,	58598.70,
        "17",	"MOR",	    353,	4878.90,
        "18",	"NAY",	    232,	27856.50,
        "19",	"NL",	    817,	64156.20,
        "20",	"OAX",	    94,	    93757.60,
        "21",	"PUE",	    278,	34309.60,
        "22",	"QRO",	    363,	11690.60,
        "23",	"QROO",	    208,	44705.20,
        "24",	"SLP",	    530,	61138,
        "25",	"SIN",	    820,	57365.40,
        "26",	"SON",	    940,	179354.70,
        "27",	"TAB",	    274,	24730.90,
        "28",	"TAM",	    323,	80249.30,
        "29",	"TLAX",	    269,	3996.60,
        "30",	"VER",	    785,	71823.50,
        "31",	"YUC",	    483,	39524.40,
        "32",	"ZAC",	    667,	75275.30
    ) %>% 
    mutate(
        carreteras_km_area = carreteras_km/area*100
    ) %>% 
    arrange(desc(carreteras_km_area)) %>% 
    filter(cve_ent != "00") %>% 
    mutate(r_carreteras = 1:32) %>% 
    select(cve_ent, carreteras = carreteras_km, r_carreteras) %>% 
    glimpse

# ---- Pegar base 
# Indicadores 
df_indicadores <- df_ips1 %>% 
    left_join(df_ingresos) %>% 
    left_join(df_pob_lab %>% select(-anio)) %>%
    left_join(df_censo_at_med) %>% 
    left_join(df_escolaridad) %>% 
    left_join(df_carreteras) %>% 
    select(cve_ent, entidad_abr_m, ips_2021, r_ips, pib_pc, r_pib_pc, 
           ing_lab, r_ing_lab, pob_lab, r_pob_lab, acc_salud, r_acc_salud, 
           escolaridad, r_escolaridad, carreteras, r_carreteras) %>% 
    glimpse()

# ---- Guardar base
openxlsx::write.xlsx(df_indicadores, 
                     file = paste_out("03_ips_indicadores.xlsx"))
    
