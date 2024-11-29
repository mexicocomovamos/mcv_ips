library(tidyverse)

# Cargamos proyeccion de CONAPO: 
load("02_datos_crudos/df_pop_state.Rdata")
proy_conapo <- df_pop_state

catalogo_abreviaturas <- read_csv("https://raw.githubusercontent.com/JuveCampos/Shapes_Resiliencia_CDMX_CIDE/master/Datos/cat_edos.csv")
# Cargamos las enfermedades. 
# Para esto, vamos a desdoblar el tabulado de INEGI de mortalidad, seleccionando: 
#     * Año de ocurrencia
#     * Entidad y municipio de ocurrencia
#     * Lista de enfermedades de la lista mexicana de enfermedades
# Y vamos a pasar el año de ocurrencia y la Entidad y municipio de ocurrencia a los renglones, y vamos a desdoblar todas las categorías de enfermedades
# Guardamos como csv, verificamos la estructura y seguimos corriendo el código: 

TODAS_ENFERMEDADES <- readxl::read_xlsx("02_datos_crudos/01_01_mortalidad/TODAS_ENFERMEDADES_RESIDENCIA.xlsx", skip = 4)
# ¡¡¡¡El renombrado se hace tomando en cuenta los arreglos de las columnas en la consulta de INEGI!!
names(TODAS_ENFERMEDADES)[c(1,2,3,4)] <- c("ocurrencia", "cve_ent", "nom_ent", "registro")

# Procesamiento: 

# TODAS_ENFERMEDADES <- 
    # TODAS_ENFERMEDADES %>% 
    # mutate(cve_ent = ifelse(nom_ent == "Total", yes = "00", no = cve_ent)) %>% 
    # pivot_longer(6:ncol(.))
    # TODAS_ENFERMEDADES[,c(1:5, c(62:476))] %>% 
    # filter(ocurrencia %in% c(1998:2023))

dx <- TODAS_ENFERMEDADES %>% 
    mutate(cve_ent = ifelse(nom_ent == "Total", yes = "00", no = cve_ent)) %>% 
    mutate(nom_ent = ifelse(nom_ent == "Total", yes = "Nacional", no = cve_ent)) %>% 
    pivot_longer(6:ncol(.)) %>% 
    mutate(value = str_remove_all(value, pattern = ",") %>% as.numeric()) %>% 
    filter(name %in% c(
        "(01B) Fiebre tifoidea",
        "(01C) Fiebre paratifoidea",
        "(01D) Shigelosis",
        "(01E) Intoxicación alimentaria",
        "(01F) Amebiasis",
        "(01G) Infecciones intestinales debidas a otros organismos especificados",
        "(01H) Diarrea y gastroenteritis de presunto origen infeccioso",
        
        "(02A) Tuberculosis pulmonar",
        "(02B) Otras tuberculosis del aparato respiratorio",
        "(02C) Tuberculosis del sistema nervioso",
        "(02D) Tuberculosis de los intestinos, el peritoneo y los ganglios mesentéricos",
        "(02E) Tuberculosis de huesos y articulaciones",
        "(02F) Tuberculosis del  aparato genitourinario",
        "(02Z) Las demás causas",
        
        "(03B) Brucelosis",
        "(03C) Lepra",
        "(03E) Tos ferina",
        "(03F) Escarlatina y erisipela",
        "(03G) Infección meningocócica",
        "(03H) Otros tétanos",
        "(03I) Septicemia",
        "(03Z) Las demás causas",
        
        "(04A) Sífilis congénita",
        "(04B) Sífilis precoz",
        "(04C) Otras sífilis",
        "(04D) Infección gonocócica",
        "(04E) Enfermedades de transmisión sexual debidas a clamidias",
        "(04F) Otras infecciones con un modo de transmisión predominantemente sexual",
        
        "(05A) Otras enfermedades debidas a espiroquetas",
        "(05B) Fiebres recurrentes",
        "(05C) Micosis",
        "(05D) Esquistosomiasis",
        "(05E) Otras infecciones debidas a trematodos",
        "(05F) Equinococosis",
        "(05H) Oncocercosis",
        "(05I) Filariasis",
        "(05J) Anquilostomiasis y necatoriasis",
        "(05K) Otras helmintiasis",
        "(05L) Secuelas de tuberculosis",
        "(05M) Secuelas de poliomielitis",
        "(05N) Secuelas de lepra",
        "(05O) Secuelas de otras enfermedades infecciosas y parasitarias y de las no especificadas",
        "(05Z) Las demás causas",
        
        "(06B) Viruela",
        "(06D) Rubéola",
        "(06F) Dengue sin signos de alarma y no especificado",
        "(06G) Dengue con signos de alarma y severo",
        "(06H) Enfermedad por virus de la inmunodeficiencia humana",
        "(06I) Encefalitis viral transmitida por artrópodos",
        "(06J) Hepatitis aguda tipo B",
        "(06K) Otras hepatitis virales",
        "(06L) Rabia",
        "(06N) Otras fiebres virales transmitidas por artrópodos y fiebres hemorrágicas virales",
        "(06O) Infecciones herpéticas",
        "(06P) Varicela y herpes zoster",
        "(06Q) Parotiditis infecciosa",
        "(06R) Otras enfermedades causadas por clamidias",
        "(06S) Enfermedad por el virus del Zika, sin especificación",
        "(06T) COVID-19",
        "(06Z) Las demás causas",
        
        "(07A) Tifus epidémico transmitido por piojos",
        "(07B) Otros tifus",
        "(07C) Otras rickettsiosis",
        "(07D) Paludismo",
        "(07E) Leishmaniasis",
        "(07F) Tripanosomiasis",
        
        "(23A) Meningitis",
        "(23B) Otras enfermedades inflamatorias del sistema nervioso",
        
        "(33A) Bronquitis y bronquiolitis agudas",
        "(33B) Neumonía",
        "(33C) Influenza", 
        "(33K) Infección aguda no especificada de las vías respiratorias inferiores",
        
        "(46G) Enfermedades infecciosas y parasitarias congénitas",
        "(46H) Otras infecciones específicas del período perinatal")) %>% 
    group_by(cve_ent, nom_ent, ocurrencia) %>% 
    summarise(value = sum(value, na.rm = T)) %>% 
    ungroup() %>% 
    rename(year = ocurrencia) %>% 
    mutate(year = as.numeric(year)) %>% 
    filter(!is.na(year))

datos <- left_join(dx,
          proy_conapo %>% 
              mutate(cve_ent = str_pad(CVE_GEO, width = 2, side = "left", pad = "0")) %>% 
              select(-CVE_GEO)) %>% 
    filter(year >= 1998) %>% 
    mutate(razon = value/(pop_tot/100000)) %>% 
    select(-nom_ent, -state) %>% 
    left_join(catalogo_abreviaturas) %>% 
    mutate(id_dimension = "01", 
           id_indicador = "04") %>% 
    ungroup() %>% 
    select(cve_ent,entidad_abr_m,anio=year,id_dimension,id_indicador,indicador_value=razon) %>% 
    filter(as.numeric(cve_ent) <= 32) %>% 
    arrange(anio, cve_ent)


openxlsx::write.xlsx(datos, "02_datos_crudos/01_01_04_datos_mortalidad_infecciosas.xlsx")
