# programa en R

# la ayuda (help) de R esta es una direccion URL interna
# que se abre con un explorador, por ejemplo con Chrome
# hay que colocar
 options(browser="C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

#-----------ejemplo instalacion libreria
install.packages("fRegression")

require(fRegression) # se carga la librería

# descripcion: A collection of functions for linear and non-linear regression modelling. It implements a wrapper for several regression models available in the base and contributed packages of R.


#------Primera parte: estimación y pruebas de significancia


#----------leer datos en una url
archivo = "http://www.stat.umn.edu/geyer/5102/data/ex5-3.txt"
# archivo = "ex5-3.txt"

D = read.table(archivo,header=T)
str(D)
attach(D)

#-------------estimar el modelo y=x1+x2+e, por MCO
m2 =  lm(y ~ x1 + x2) # se corre el modelo lineal
summary(m2)

#--------ejercicios con formular matriciales
#---- conformar la matriz de diseño
X = model.matrix(m2)

#-------- calcular la matriz "hat"
H = X%*%solve(t(X)%*%X)%*%t(X)

#-----------pruebas de significacion
require(lmtest)
coeftest(m2)

#-----------calcular residuos
ehat = residuals(m2)
hist(ehat,15)

#-----------estimar mse y sigma

sigma=summary(m2)$sigma
MSE = sigma^2

#-----------calcular residuos estandarizados r
require(MASS)
r = stdres(m2)


#-----------valores ajustados

yhat = fitted(m2) 


#-----------graficas

par(mfrow=c(2,2))
plot(x1,y,type='p',main="A")
plot(r,main="B")
abline(h=0)
plot(y,yhat,type='p',main="C")
hist(r,20,main="D")

#----------intervalos de confianza para parametros beta

# Error tipo I: rechazar Ho siendo cierta (falso positivo)
# Error tipo II: no rechazar Ho siendo falsa (falso negativo)

coefficients(m2) 

m80=confint(m2, level=0.80)  
m90=confint(m2, level=0.90)
m95=confint(m2, level=0.95)
m99=confint(m2, level=0.99)

M.int = cbind(m80[1,],m90[1,],m95[1,],m99[1,])
rownames(M.int)=c("lim.iz","lim.dr")
colnames(M.int)=c("0.8","0.9","0.95","0.99")
(M.int)

M.x1 = cbind(m80[2,],m90[2,],m95[2,],m99[2,])
rownames(M.x1)=rownames(M.int)
colnames(M.x1)=colnames(M.int)
(M.x1)
M.x1[2,]-M.x1[1,]

M.x2 = cbind(m80[3,],m90[3,],m95[3,],m99[3,])
rownames(M.x2)=rownames(M.int)
colnames(M.x2)=colnames(M.int)
M.x2[2,]-M.x2[1,]


#------Segunda parte: estadísticos de selección de modelos



m1 =  lm(y ~ x1)
summary(m1)

m2 =  lm(y ~ x1 + x2)
summary(m2)

m3 =  lm(y ~ -1 + x1 + x2)
summary(m3)

# ver la estructura de arbol del objeto m1

str(m1)

# calcular el MSE, R2, Ra2, AIC y BIC de cada modelo

source("medidas.r")

N1 = medidas(m1,y,k1) 
N2 = medidas(m2,y,k2) 
N3 = medidas(m3,y,k3) 

N = cbind(N1,N2,N3)
rownames(N) = rownames(M)

(N)

require(wle)

result <- mle.aic(m1)
summary(result,num.max=10)

result <- mle.aic(m2)
summary(result,num.max=10)

result <- mle.aic(m2)
summary(result,num.max=10)

require(MASS)

#------------------

m4 = nls(y ~ a0+a1/(1+exp(b0+ b1*x1+b2*x2)),
start=list(a0=1.0,a1=0.10,b0=-0.4, b1=0.4, b2=0.06))
summary(m4)

N1 = medidas(m1,y,k1) 
N2 = medidas(m2,y,k2) 
N3 = medidas(m3,y,k3) 
N4 = medidas(m4,y,k3)

N = cbind(N1,N2,N3,N4)
rownames(N) = rownames(M)

(N)

#--------------------investigar...
require(car)
confidenceEllipse(lm(y ~ x1 + x2, data=D), Scheffe=TRUE)


