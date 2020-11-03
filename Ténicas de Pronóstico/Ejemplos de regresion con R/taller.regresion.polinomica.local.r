# Ejemplo de regresion polinomica local

file_salarioMpioMedDat = file.choose()

D = read.table(file_salarioMpioMedDat,header=TRUE,stringsAsFactors=FALSE)
attach(D)


#--------------regresion loess salarios vs edad

x08 = order(edad08,S08)

x = edad08[x08]
s = S08[x08]


#------------------ variables explicativas adicionales
#------------------ es un modelo polinomico
x = as.numeric(x)
x2 = x*x

plot(x,s)

# transformacion logaritmica para buscar 
# que la variable y sea aproximadamente normal


#------------------------ regresion lineal (primer per�odo)

m.08.2 = lm(s ~ x + x2 )
summary(m.08.2)
shat = fitted(m.08.2)
#------------------------ regresion noparam nadaraya-watson

library(KernSmooth) #suavisamiento por kernel


h8 <- dpill(edad08,S08)
h8 = 1.2
mh08 = locpoly(x,s,bandwidth = h8) #regresion local polinomica

plot(edad08,S08,type="p",
main=paste("h =", round(h8,2),"con n�cleo", 
kernel),
xlab="Edad",
ylab="Salario",col="skyblue",
pch=19,cex=1.2)

lines(mh08$x,mh08$y,lty=1,col=4,lwd=2) # se compara el yhat de la regresion polinomica y el de la regresi�n
lines(x,shat,lty=1,col='red',lwd=2)


