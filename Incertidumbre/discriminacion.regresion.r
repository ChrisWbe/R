datos=read.csv(file.choose(),sep=",",header = T)
y=datos$Salario.anual
x1=datos$genero
x2=datos$Experiencia.previa
x3=datos$experiencia.en.la.empresa
x4=datos$Nivel.educativo


#Regresion del salario vs genero
r1=lm(y~x1)
summary(r1)

#Regresion de salario vs genero, experiencia previa y actual
r2=lm(y~x1+x2+x3)
summary(r2)

#Se expresa x4 como una variable categórica
x4=as.factor(x4)

#Regresion de salario vs genero, experiencia previa, experiencia actual y nivel educativo
r3=lm(y~x1+x2+x3+x4)
summary(r3)

