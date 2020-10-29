#Ejemplo de tipo de vectores
e = c(34,35,56,78)
g = c("h","m","m","m")
s = c(3.4,4.5,2.4,12.3)
L = c(TRUE,FALSE,FALSE,NA)
r = complex(3)
r[1] = -0.8 - 1.3i
r[2] = Conj(r[1])
r[3] =3
#------
str(e) # define el tipo de dato
str(r)
#------
e = as.integer(e)
str(e)
#------
a = as.numeric(L)
a
str(a)

#Objetos data.frame
e = c(34,35,56,78)
g = c("h","m","m","m")
s = c(3.4,4.5,2.4,12.3)
D = data.frame(edad=e,genero=g,salario=s)
str(D)
es = D$edad/D$salario
str(es)


#Objetos listas
# ejemplo list
a = matrix(c(2,3,4,5),2,2)
b = c("web","real")
d = rnorm(120,2,3)
L = list(a=a,b=b,d=d)


#Lectura de datos en archivos
G = read.table("nombre") # se utiliza para leer datos en R, de tal forma que retorna un data.frame
#----------------------------------------
archivo = "http://www.medellin.unal.edu.co/~ndgirald/Datos/Datos%20curso%20Series%20II/fechaydatos.prn"
G = read.table(archivo, header = TRUE,stringsAsFactors=FALSE)
attach(G)
#-----------------------grafica