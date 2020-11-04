# La serie IPC,
# Variaci´on porcentual mensual del Indice de precios
# al consumidor, 2000-2012.
library(readxl)
file_IPCporcentual = file.choose()
res <- read_excel(file_IPCporcentual, 1) # lee el primer libro
attach(res)
y = ts(IPC, frequency=12,start=c(2000,01))
ts.plot(y)
