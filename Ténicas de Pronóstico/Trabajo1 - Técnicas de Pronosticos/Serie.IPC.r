# La serie IPC,
# Variaci´on porcentual mensual del Indice de precios
# al consumidor, 2000-2012.
library(readxl)
file_IPCporcentual = file.choose()
res <- read_excel(file_IPCporcentual, 1) # lee el primer libro
attach(res)
y = ts(IPC, frequency=12,start=c(2002,01)) #periodo 12, IPC=y, IPC como objeto ts (serie de tiempo), inicia en enero del 2000
ts.plot(y)
frequency(y)
#-- validacion cruzada yi->datos de entrenamiento, yf->periodo de contraste
yi = window(y,start=c(2002,01),end=c(2016,07))
yf = window(y,start=c(2016,08),end=c(2017,07))
m=12 #numero de periodos con el que se va a contrastar
#----estimacion de modelo con tendencia cúbica más estacionalidad con indicadoras

require(forecast)

ti = seq(1,length(yi))
ti2 = ti*ti
ti3 = ti2*ti

It = seasonaldummy(yi) #matriz de indicadoras 

T = length(yi)
Xt = cbind(rep(1,T),ti,ti2,ti3,It)

mod1 = lm(yi ~ ti + ti2 + ti3 + It)
summary(mod1)

#Valores ajustados
yhat1 = mod1$fitted.values


plot(ti,yi,type='o',col='darkblue')
lines(ti,yhat1,col='red')

#---- Y vs Y ajustado 


plot(yi,yhat1)

#------------ MSE - R2-Ajustado - AIC - BIC
file_medidas = file.choose()
source(file_medidas)
M = cbind(medidas(mod1,yi,4))
colnames(M) = c("m1")

(M)

# -------pronosticos 


Itf = seasonaldummy(yi,m)
tf = seq(T+1,T+m,1)
tf2 = tf*tf
tf3 = tf2 * tf
Xtf = cbind(rep(1,m),tf,tf2,tf3,Itf)

pron1 = predict(mod1,data.frame(Xt=I(Xtf)))

plot(tf,yf, type = 'o',ylim=c(1.3,2.0))
lines(tf,pron1, type = 'b', pch = 3,col='red' )

pron1 = ts(pron1,frequency=12,start=c(2016,08),end=c(2017,07))

A=rbind(accuracy(pron1,yf))
rownames(A) = c("Cúbica-Indicadoras")

library(xtable)
print(xtable(A))
