####################
#                  #
# Copiar todo esto #
#                  #
####################
# Hecho con gusto por Rafa @GonzalezGouveia

# video 07 - factores

# Objetivo: estudiar qu� son factores en R.
# ---------------
# En este ejercicio vamos a:
# 1. Crear factores en R
# 2. Recodificar niveles de un factor
# 3. Ordenar niveles de un factor


#####################################
# correr esto antes de empezar.   #
#####################################

# crear vector de ventas
tallas <- c('m', 'g', 'S', 'S','m', 'M')

# intentar graficar
plot(tallas)

###################################
# pr�ctica 1: crear factores en R #
###################################

# crear factor de un vector
tallas_factor <- factor(tallas)

# graficar factor
plot(tallas_factor)

# mirar niveles de factor
levels(tallas_factor)

######################################
# pr�ctica 2: recodificando factores #
######################################

# creando factor recodificado
tallas_recodificadas <- factor(tallas, 
                               levels = c("g", "m", "M", "S"),
                               labels = c("G", "M", "M", "S"))



# graficando ventas_recodificado
plot(tallas_recodificadas)

#############################################
# pr�ctica 3: ordenando niveles de factores #
#############################################

# ordenando niveles (copiar factor anterior)
tallas_ordenado <- factor(tallas,
                          ordered = TRUE,
                          levels = c("S", "m", "M", "g"),
                          labels = c("S", "M", "M", "G"))




# viendo el orden en los niveles
tallas_ordenado

# graficando el factor ordenado
plot(tallas_ordenado)

# Hecho con gusto por Rafa @GonzalezGouveia
# Suscribete para m�s c�digo en R https://bit.ly/2WNDhNR