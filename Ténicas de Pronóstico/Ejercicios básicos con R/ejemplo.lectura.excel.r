#----------Ejemplo A.6.5 lectura archivo excel
library(readxl)
file = file.choose()
res <- read_excel(file)#res <- read_excel("international-petroleum-world-cr.xlsx")
attach(res)
str(res)
plot.ts(res[,c(2,3,4,5)])

require(insuranceData)

data(package="insuranceData") 

data(AutoClaims)

y=AutoClaims  
