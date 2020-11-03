
#------------ejercicios con formulas matriciales


#----------leer datos en una url
archivo = "http://www.stat.umn.edu/geyer/5102/data/ex5-3.txt"
# archivo = "ex5-3.txt"

D = read.table(archivo,header=T)
str(D)
attach(D)



#-------------estimar el modelo y=x1+x2+e, por MCO
m2 =  lm(y ~ x1 + x2)
summary(m2)

# matriz de covariazas para los parametros estimados
V=vcov(m2) 

# matriz de correlaciones entre los parametros estimados
cov2cor(vcov(m2))

# estadisticos t-Student
beta = coefficients(m2)
(tStudent = beta/sqrt(diag(V)))

# valores p. Note la función pt()
nu=nrow(H)-3 # es T-k
(valor.p = pt(abs(tStudent),nu,lower.tail=FALSE)+
(pt(-abs(tStudent),nu,lower.tail=TRUE)))

# tabla resumen
err.std = sqrt(diag(V))
cbind(beta,err.std,tStudent,valor.p)

# alternativa: summary
summary(m2)

#-----------conformar la matriz de diseño
X = model.matrix(m2)

#-----------calcular la matriz "hat"
H = X%*%solve(t(X)%*%X)%*%t(X)

#-----------calcular residuos
ehat = (diag(nrow(H))-H)%*%y
hist(ehat,15)

#-----------estimar mse y sigma
T = nrow(X)
sigma.hat = sqrt(sum(ehat*ehat)/(T-ncol(X)))

#-----------calcular residuos r
r = ehat/(sigma.hat*(rep(1,ncol(H))-diag(H)))

#-----------valores ajustados

yhat = fitted(m2) # predicted values


par(mfrow=c(2,2))
plot(x1,y,type='p',main="A")

plot(r,main="B")
abline(h=0)

plot(y,yhat,type='p',main="C")

plot(x2,diag(H),type='p',main="D")



