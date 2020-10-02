install.packages("readr")
library(readr)

ruta_csv <- file.choose()
ruta_csv

datos_csv <- read_csv(ruta_csv)
head(datos_csv)

#sin titulos > read_csv(ruta, col_names=FALSE)
#para agregar nombre > read_scv(ruta, col_names = c('elMismoNumeroDeColumnasEnLosNombres", ...))
#csv separado por ; > read_csv2(ruta)