install.packages("readxl")
library(readxl)

ruta_excel <- file.choose()
ruta_excel

excel_sheets(ruta_excel)

#Caso ideal, primera hoja e inicia en A0
caso_ideal <- read_excel(ruta_excel)

#Caso medio, cualquier hoja e inicia en A0
caso_medio <- read_excel(ruta_excel,
                         sheet = 'Hoja2')

#Caso dificl, cualquier hoja en cualquier celda
caso_dificl <- read_excel(ruta_excel,
                          sheet = 'Hoja3',
                          range = 'E4:I4')
