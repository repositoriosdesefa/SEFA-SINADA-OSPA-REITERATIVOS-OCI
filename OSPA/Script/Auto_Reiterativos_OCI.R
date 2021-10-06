##########################################################################-
########  Generación automatizada  de oficios reiterativos  OSPA ########-
############################# By LE ######################################-

##################### 0. Parámetros generales #####################-

# Parámetros Locales

DIRECTORIO <- ""
PROYECTO <- file.path(DIRECTORIO, "")
CARPETA_GUARDADO <- ""

OFICIO_RMD <- file.path(PROYECTO, "Oficio.Rmd")
OFICIO_NACIONAL <- file.path(PROYECTO, "Oficio_Nacional.Rmd")
MEMO_OD_RMD <- file.path(PROYECTO, "Memorando_OD.Rmd")
MEMO_LIN_RMD <- file.path(PROYECTO, "Memorando_Lineas.Rmd")
OFICIO_OCI <- file.path(PROYECTO, "OCI.Rmd")

# Parámetros No Locales (En la web)
M_INSUMOS <- ""
TABLA_INSUMOS <- ""


################ I. Librerias, drivers y directorio #################-

# I.1 Librerias

library(dplyr)
library(readxl)
library(rmarkdown)
library(purrr)
library(lubridate)
library(stringi)

# I.2 Parametros

# i) Conexion de la base y descarga de matriz de insumos

tp1 <- tempfile() # Creacion de un archivo temporal
download.file(M_INSUMOS, tp1, mode ="wb")

########################### II. Funciones ###########################-

# II.1 Funcion de renderizado de documento

auto_lec_rep <- function(tipo, estado,
                         ht, num, lugar, nombre, 
                         efa, defa, prefa, direfa,
                         oci, doci, proci, diroci, 
                         firma, 
                         aux_1, aux_2, aux_3, 
                         aux_4, aux_5){
  
  # Definición de tipo de documento
  T_DOC <- "OSPA - REIT - "

  
  # Se eliminan carácteres especiales
  efa_n = gsub("Ñ", "N", efa)
  
  # Eliminación de tildes
  con_tilde_may <- c("Á", "É", "Í", "Ó", "Ú")
  sin_tilde_may <- c("A", "E", "I", "O", "U")
  con_tilde_min <- c("á", "é", "í", "ó", "ú")
  sin_tilde_min <- c("a", "e", "i", "o", "u")
  
  efa_f = stri_replace_all_regex(efa_n, con_tilde_may, sin_tilde_may, vectorize = F)
  efa_f = stri_replace_all_regex(efa_n, con_tilde_min, sin_tilde_min, vectorize = F)
  
  # Se selecciona el tipo de plantilla
  
  if(tipo == "ODE/OE"){
    MODELO_RMD = MEMO_OD_RMD
  } else if(estado == "Enviar oficio a OCI") {
    MODELO_RMD = OFICIO_OCI
    T_DOC <- "OSPA - OCI - "
  } else if (tipo == "EFA Nacional") {
    MODELO_RMD = OFICIO_NACIONAL
  } else if (tipo == "Directa") {
    MODELO_RMD = MEMO_LIN_RMD
  } else {
    MODELO_RMD = OFICIO_RMD
  }
  
  rmarkdown::render(input = MODELO_RMD,
                    # Heredamos los par?metros desde la matriz de insumos
                    params = list(HT_1 = ht,
                                  N_OFICIO_2 = num,
                                  LUGAR_3 = lugar,
                                  DESTINATARIO_4 = nombre,
                                  EFA_5 = efa,
                                  DPTO_EFA_6 = defa,
                                  PROV_EFA_7 = prefa,
                                  DIR_EFA_8 = direfa,
                                  OCI_9 = oci,
                                  DPTO_OCI_10 = doci,
                                  PROV_OCI_11 = proci,
                                  DIR_OCI_12 = diroci,
                                  FIRMANTE_13 = firma,
                                  ADICIONAL_14 = aux_1,
                                  ADICIONAL_15 = aux_2,
                                  ADICIONAL_16 = aux_3,
                                  ADICIONAL_17 = aux_4,
                                  ADICIONAL_18 = aux_5),
                    output_file = paste0(CARPETA_GUARDADO,
                                         T_DOC,
                                         aux_1,
                                         " - ",
                                         ht))
}

# II.2 Funcion robustecida
R_auto_lec_rep <- function(tipo, estado,
                           ht, num, lugar, nombre, 
                           efa, defa, prefa, direfa,
                           oci, doci, proci, diroci, 
                           firma, 
                           aux_1, aux_2, aux_3,
                           aux_4, aux_5){
  out = tryCatch(auto_lec_rep(tipo, estado,
                              ht, num, lugar, nombre, 
                              efa, defa, prefa, direfa,
                              oci, doci, proci, diroci, 
                              firma, 
                              aux_1, aux_2, aux_3,
                              aux_4, aux_5),
                 error = function(e){
                   auto_lec_rep(tipo, estado,
                                ht, num, lugar, nombre, 
                                efa, defa, prefa, direfa,
                                oci, doci, proci, diroci, 
                                firma, 
                                aux_1, aux_2, aux_3,
                                aux_4, aux_5) 
                 })
  return(out)
}

###################### III. Procesamiento de datos ######################-

# III.1 Carga de datos
INSUMOS <- as.data.frame(read_xlsx(tp1, sheet = TABLA_INSUMOS))

# III.2 Filtrado
INSUMOS <- INSUMOS %>%
  filter(!is.na(HT_1))

############################ IV. Renderizado ###########################-

# IV.1 Creación del documento

pwalk(list(INSUMOS$T_EFA,
           INSUMOS$ESTADO,
           INSUMOS$HT_1, 
           INSUMOS$N_OFICIO_2,
           INSUMOS$LUGAR_3,
           INSUMOS$DESTINATARIO_4,
           INSUMOS$EFA_5, 
           INSUMOS$DPTO_EFA_6,
           INSUMOS$PROV_EFA_7,
           INSUMOS$DIR_EFA_8,
           INSUMOS$OCI_9, 
           INSUMOS$DPTO_OCI_10,
           INSUMOS$PROV_OCI_11,
           INSUMOS$DIR_OCI_12,
           INSUMOS$FIRMANTE_13,
           INSUMOS$ADICIONAL_14,
           INSUMOS$ADICIONAL_15,
           INSUMOS$ADICIONAL_16,
           INSUMOS$ADICIONAL_17,
           INSUMOS$ADICIONAL_18),
      slowly(R_auto_lec_rep, 
             rate_backoff(10, max_times = Inf)))
