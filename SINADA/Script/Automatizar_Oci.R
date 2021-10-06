####################################################################-
############  Generaci?n masiva de oficios reiterativos  ###########-
####################################################################-

################ I. Librerias, drivers y directorio ################

# I.1 Librerias----

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
#install.packages("stringi")
library(stringi)

# I.2 Parametros----

# i) Conexion de la base----

SINADA_INSUMOS <- ""
tp1 <- tempfile() # Creacion de un archivo temporal
NOMBRE_PEDIDO = "SINADA -"

download.file(SINADA_INSUMOS, tp1, mode ="wb")
SINADA_INSUMOS <- as.data.frame(read_xlsx(tp1, sheet = "INSUMOS"))

# ii) Directorio donde se guaradarn los documentos generados----

dir <- ""
reiterativos_dir <- file.path(dir, "Documentos")

OFICIO_OCI <- file.path(reiterativos_dir, "OCIsinada.Rmd")
MEMORANDO <- file.path(reiterativos_dir, "Memorando_Lineas.Rmd")

########################### II. Funciones ###########################-

# II.1 Funcion de renderizado de documento----

auto_lec_rep <- function(ht, num, lugar,
                         cargo, oci, deoci,
                         dioci, firma, cod,
                         ref1, ref2, inicial,
                         tipodoc, efa, pefa,
                         defa, doci, ref, plazo,
                         sigla, depaux, proaux){
  # Reemplazo de carácteres especiales para nombre de archivo
  doc_n = gsub(" N°", "-", num)
  doc_f = gsub("/", "-", doc_n)
  
  if(tipodoc == "Oficio"){
    MODELO_RMD = OFICIO_RMD
  } else {
    MODELO_RMD = MEMORANDO
  }
  
  rmarkdown::render(input = MODELO_RMD,
                    # Heredamos los par?metros desde la matriz de insumos
                    params = list(HT_1 = ht,
                                  N_OFICIO_2 = num,
                                  LUGAR_3 = lugar,
                                  DESTINATARIO_4 = cargo,
                                  OCI_9 = oci,
                                  DPTO_OCI_10 = deoci,
                                  DIR_OCI_12 = dioci,
                                  FIRMANTE_13 = firma,
                                  ADICIONAL_14 = cod,
                                  ADICIONAL_15 = ref1,
                                  ADICIONAL_22 = ref2,
                                  ADICIONAL_17 = inicial,
                                  ADICIONAL_18 = tipodoc,
                                  ADICIONAL_19 = efa),
                    output_file = paste0(NOMBRE_PEDIDO,
                                         tipodoc, " - ",
                                         cod," - ",ht))
}

# II.2 Funcion robustecida----

R_auto_lec_rep <- function(ht, num, lugar,
                           cargo, oci, deoci,
                           dioci, firma, cod,
                           ref1, ref2, inicial,
                           tipodoc, efa){
  out = tryCatch(auto_lec_rep(ht, num, lugar,
                              cargo, oci, deoci,
                              dioci, firma, cod,
                              ref1, ref2, inicial,
                              tipodoc, efa),
                 error = function(e){
                   auto_lec_rep(ht, num, lugar,
                                cargo, oci, deoci,
                                dioci, firma, cod,
                                ref1, ref2, inicial,
                                tipodoc, efa) 
                 })
  return(out)
}


########################### III. Renderizado ###########################-

pwalk(list(SINADA_INSUMOS$HT_1, SINADA_INSUMOS$N_OFICIO_2,
           SINADA_INSUMOS$LUGAR_3, 
           SINADA_INSUMOS$DESTINATARIO_4, SINADA_INSUMOS$OCI_9,
           SINADA_INSUMOS$DPTO_OCI_10,
           SINADA_INSUMOS$DIR_OCI_12, SINADA_INSUMOS$FIRMANTE_13,
           SINADA_INSUMOS$ADICIONAL_14,
           SINADA_INSUMOS$ADICIONAL_15, 
           SINADA_INSUMOS$ADICIONAL_22,
           SINADA_INSUMOS$ADICIONAL_17,
           SINADA_INSUMOS$ADICIONAL_18,
           SINADA_INSUMOS$ADICIONAL_19),
      slowly(R_auto_lec_rep, 
             rate_backoff(10, max_times = Inf)))


