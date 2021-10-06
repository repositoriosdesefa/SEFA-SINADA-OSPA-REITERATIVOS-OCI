####################################################################-
############  Generaci?n masiva de oficios reiterativos  ###########-
####################################################################-

################ I. Librerias, drivers y directorio ################-

# I.1 Librerias

#install.packages("dplyr")
library(dplyr)
#install.packages("readxl")
library(readxl)
#install.packages("rmarkdown")
library(rmarkdown)
#install.packages("purrr")
library(purrr)
#install.packages("lubridate")
library(lubridate)
#intall.packages("stringi")
library(stringi)

# I.2 Parametros

# i) Conexion de la base----

SINADA_INSUMOS <- ""
tp1 <- tempfile() # Creacion de un archivo temporal
NOMBRE_PEDIDO <- "SINADA REIT - "

download.file(SINADA_INSUMOS, tp1, mode ="wb")
SINADA_INSUMOS <- as.data.frame(read_xlsx(tp1, sheet = "INSUMOS"))

SINADA_INSUMOS_F <- filter(SINADA_INSUMOS, SINADA_INSUMOS$ADICIONAL_18 == "BIEN")

# ii) Directorio donde se guaradarn los documentos generados----

dir <- ""
reiterativos_dir <- file.path(dir, "SINADA/Reiterativos")
CARPETA_GUARDADO <- "SINADA - Reiterativos/"

# iii) Reemplazo de carácteres especiales----

con_tilde <- c("á", "é", "í", "ó", "ú")
sin_tilde <- c("a", "e", "i", "o", "u")

########################### II. Funciones ###########################-

# II.1 Funcion de renderizado de documento----

auto_lec_rep <- function(ht, num, cod,
                         lugar, cargo,
                         efa, pefa, defa,
                         oci, doci, deoci,
                         ref, plazo, firma, sigla, depaux, proaux){
  
  # Reemplazo de carácteres especiales para nombre de archivo
  efa_n = gsub("Ñ", "N", efa)
  efa_f = stri_replace_all_regex(efa_n, con_tilde, sin_tilde, vectorize = F)
  
  rmarkdown::render(input = file.path(reiterativos_dir, "Oficio_sinada.Rmd"),
                    # Heredamos los par?metros desde la matriz de insumos
                    params = list(HT_1 = ht,
                                  N_OFICIO_2 = num,
                                  ADICIONAL_14 = cod,
                                  LUGAR_3 = lugar,
                                  DESTINATARIO_4 = cargo,
                                  PROV_EFA_7 = pefa,
                                  DPTO_EFA_6 = defa,
                                  OCI_9 = oci,
                                  DIR_OCI_12 = doci,
                                  DPTO_OCI_10 = deoci,
                                  ADICIONAL_15 = ref,
                                  ADICIONAL_16 = plazo,
                                  FIRMANTE_13 = firma,
                                  ADICIONAL_17 = sigla,
                                  ADICIONAL_19 = efa,
                                  ADICIONAL_20 = depaux,
                                  ADICIONAL_21 = proaux),
                    output_file = paste0(CARPETA_GUARDADO,
                                         cod,
                                         " - ",
                                         ht))
}

# II.2 Funcion robustecida

R_auto_lec_rep <- function(ht, num, cod,
                           lugar, cargo,
                           efa, pefa, defa,
                           oci, doci, deoci,
                           ref, plazo, firma, sigla, depaux, proaux){
  out = tryCatch(auto_lec_rep(ht, num, cod,
                              lugar, cargo,
                              efa, pefa, defa,
                              oci, doci, deoci,
                              ref, plazo, firma, sigla, depaux, proaux),
                 error = function(e){
                   auto_lec_rep(ht, num, cod,
                                lugar, cargo,
                                efa, pefa, defa,
                                oci, doci, deoci,
                                ref, plazo, firma, sigla, depaux, proaux) 
                 })
  return(out)
}


########################### III. Renderizado ###########################-

pwalk(list(SINADA_INSUMOS_F$HT_1, 
           SINADA_INSUMOS_F$N_OFICIO_2,
           SINADA_INSUMOS_F$ADICIONAL_14, 
           SINADA_INSUMOS_F$LUGAR_3, 
           SINADA_INSUMOS_F$DESTINATARIO_4,
           SINADA_INSUMOS_F$PROV_EFA_7,
           SINADA_INSUMOS_F$DPTO_EFA_6, 
           SINADA_INSUMOS_F$OCI_9,
           SINADA_INSUMOS_F$DIR_OCI_12,
           SINADA_INSUMOS_F$DPTO_OCI_10, 
           SINADA_INSUMOS_F$ADICIONAL_15,
           SINADA_INSUMOS_F$ADICIONAL_16,
           SINADA_INSUMOS_F$FIRMANTE_13,
           SINADA_INSUMOS_F$ADICIONAL_17,
           SINADA_INSUMOS_F$ADICIONAL_19,
           SINADA_INSUMOS_F$ADICIONAL_20,
           SINADA_INSUMOS_F$ADICIONAL_21),
      slowly(R_auto_lec_rep, 
             rate_backoff(10, max_times = Inf)))


