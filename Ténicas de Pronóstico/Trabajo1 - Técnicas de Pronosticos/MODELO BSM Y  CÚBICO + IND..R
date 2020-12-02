library(readxl)
require(forecast)
medidas = function(m,y,k){
  # m = objeto producido con lm()
  # y = variable dependiente
  # k = número de coeficientes beta
  T = length(y)
  yest = fitted(m)
  sse = sum((yest-y)^2)
  ssr = sum((y-mean(y))^2) 
  mse = sse/(T-k)
  R2 = 1 - sse/ssr
  Ra2 = 1 - (T-1)*(1-R2)/(T-k)
  aic = log((T-k)*exp(2*k/T)*mse/T)
  bic = log(T^(k/T)*(T-k)*mse/T)
  M = c(mse,Ra2,  aic, bic)
  names(M) = c("mse","R2-ad","log.aic","log.bic")
  return(M)
}

medidas.struc = function(y,yest,k){
  # y = serie, m = modelo, k = numero parametros
  T = length(y)
  sse = sum((yest-y)^2)
  ssr = sum((y-mean(y))^2) 
  mse = sse/(T-k)
  R2 = 1 - sse/ssr
  Ra2 = 1 - (T-1)*(1-R2)/(T-k)
  aic = log((T-k)*exp(2*k/T)*mse/T)
  bic = log(T^(k/T)*(T-k)*mse/T)
  
  M = c(Ra2, mse, aic, bic)
  names(M) = c("R2-ajus","MSE","logAIC","logBIC")
  return(M)
}


#leer datos
G = read_excel("C:/Users/santiagogc/Desktop/Archivos Analista de Riesgos-Santiago/CODIGOS DE R/PRONÓSTICOS/Trabajo PRONÓSTICOS/IPCporcentual.xlsx" , sheet="Hoja1")
#transformar datos en serie de tiempo
y=ts(G$IPC,frequency=12,start=c(2002,1),end=c(2017,7))
ts.plot(yi)
#variable m significa la periodicidad de la estacionalidad
m = 12
n = length(y)
#grafica datos muestrales
np = length(yi)

fecha = seq(as.Date("2016/08/01"), length.out=(m), by="months")

ejex.mes = seq(fecha[1],fecha[np], "months")
ejex.año = seq(fecha[1],fecha[np],"years")

plot(fecha,y, xaxt="n", panel.first = grid(),type='l',
     ylab='IPC Mensual', lwd = 1)
abline(v=175, lty=3)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)


#segmento de entrenamiento
yi = ts(y[1:(n-m)],frequency=12,start=c(2002,01))
#segmento de prueba
yf = ts(y[(n-m+1):n],frequency=12,start=c(2016,08))
lyi = log(yi)
to =seq(1,length(y))
t = seq(1,length(yi))
tf = seq(1,length(yf))

##########MODELO BSM

#estimación del modelo
model1 = StructTS(yi)
print(model1)


#ajuste del modelo
yhat = apply(fitted(model1),1,sum)
medidas.e = medidas.struc(yi,yhat,3)
#pronóstico del modelo
fcast1=forecast(model1,h=m)
pron1 = fcast1$mean
medidas.p = accuracy(pron1,yf)
#graficas ajuste
plot(fecha,yi, xaxt="n", panel.first = grid(),type='o',
     ylab='IPC Mensual', lwd = 1)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
lines(fecha,yhat,col='red',pch=22, lty=2)
#graficas pronóstico
plot(fecha,yf, xaxt="n", panel.first = grid(),type='o',
     ylab='IPC Mensual', lwd = 1)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
lines(fecha,pron1,col='red',pch=22, lty=2)
#graficas estilizada
plot(fecha,yi, xaxt="n", panel.first = grid(),type='l',
     ylab='licencias.mes', lwd = 2,col='darkgray')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

lines(fecha[-seq(1,12)],yest.hw, xaxt="n", panel.first = grid(),
      type='l',col='red')

lines(fecha,yest.bsm, xaxt="n", panel.first = grid(),
      type='l',col='blue',lty=3,lwd=2)

lines(fecha[-seq(1,12)],na.omit(yest.nnar), xaxt="n", panel.first = grid(),
      type='l',col='orange',lty=1,lwd=2)



##########CUBICO + INDICADORAS

#ajuste del modelo

It = seasonaldummy(yi)
t2 =t^2
t3 =t^3
model2 = lm(yi ~ t + t2 + t3 + It)
summary(model2)

yhat1 = model2$fitted.values
medidas.e = medidas.struc(yi,yhat1,3)
#pronóstico del modelo
T = length(yi)
Itf = seasonaldummy(yi,m)
tf = seq(T+1,T+m,1)
tf2= tf^2
tf3= tf^3
pron2 = predict(model2,data.frame(t= tf,t2=tf2,t3=tf3, It=I(Itf)))
medidas.p = accuracy(pron2,yf)
#graficas ajuste
plot(fecha,yi, xaxt="n", panel.first = grid(),type='o',
     ylab='IPC Mensual', lwd = 1)
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
lines(fecha,yhat1,col='red',pch=22, lty=2)
#graficas pronóstico
plot(fecha,yf, xaxt="n", panel.first = grid(),type='o',
     ylab='IPC Mensual', lwd = 1,ylim=c(-1,1.5))
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)
lines(fecha,pron2,col='red',pch=22, lty=2)
lines(fecha,pron1,col='blue',pch=22, lty=2)

plot(tf,yf,type='o',col='darkgray')
lines(tf,pron2,col='red')
