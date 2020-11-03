#---Ejemplo A.6.4 lectura de una URL
archivo = "http://www.medellin.unal.edu.co/~ndgirald/
Datos/Datos%20curso%20Series%20II/fechaydatos.prn"
G = read.table(archivo, header = TRUE, 
stringsAsFactors=FALSE)
attach(G)
#-----------------------grafica
np = length(x)
#-----------------------convierte fecha a formato de R
fechas = as.Date(fecha,format="%d/%m/%y")
ejex.mes = seq(fechas[1],fechas[np], "months")
ejex.año = seq(fechas[1],fechas[np],"years")

plot(fechas,x, xaxt="n", panel.first = grid(),
type='l', lwd=2,ylab='produccion.diaria')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

par(mfrow=c(2,1))

plot(fechas,x, xaxt="n", panel.first = grid(),
type='b',ylab='produccion.diaria')
axis.Date(1, at=ejex.mes, format="%m/%y")
axis.Date(1, at=ejex.año, labels = FALSE, tcl = -0.2)

hist(x,15)

