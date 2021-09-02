# Cargar datos en R
# data frames
# tablas, cargar datos de entrada, las columadas / filas pueden tener diferetes
# tipos de valores


## Data sets ##
# Saber como estás armado el data set, ás facil de graficar
# Procesar facilmente los datos, + eficienia
# Entendible para los terceros
# Algoritmos estandar

# Ordenar los datos toma mucho tiempo


# Ejemplo cargando un archivo del cel #

# En 00.tsv están los datos del celerómetro del cel
# Para cargarlo en R hay que indicar cual es el directorio de trabajo

print(getwd())

# Campiar donde están mi working directory
setwd("C:/Users/Atahulpq/Desktop/R/programas/Fisica-Experimental-1/2da clase")

## B Carga de datos ##
# ver la lista de archivos
list.files()

# En el tsv los archivos están así, Nro de medición / tiempo de medición / Ax /Ay / Az

#lee linea por linea, strings con los char
archivo = gsub(readLines(con = "00.tsv", n = -1),pattern = "\"", replacement="")

#lee por variables, es un data frame. Los mete en variabes
data = read.csv(file = "00.tsv", sep = "\t", header = F)

print(data)

# Primers 3 datos, dimensiones.
head(data, 3); dim(data);
tail(data, 3)

# no saco filas por eso el espacio
data = data[ , c(-1, -6)]

# asignar nombres a las varibales
colnames(data)  = c("t", "ax", "ay", "az")
head(data, 3)

# tomo la variable t de data y le resto el t_0
data$t = data$t - data$t[1]

# paso la dat aa segundos
data$t = data$t / 1000; head(data, 10); range(data$t)



library(ggplot2); source(getwd())
ga <- ggplot(data = data) +
  geom_point(aes(x = t, y = ax, colour = "ax")) + 
  geom_point(aes(x = t, y = ay, colour = "ay")) +
  geom_point(aes(x = t, y = az, colour = "az")) +
  ylab(TeX("$a \\; (m\\, / \\,s^2)$")) + xlab("t (s)") + 
  scale_colour_manual(values = c("ax" = "green4", "ay" = "red4", "az" = "Deepskyblue4" ),name = NULL) +
  temajuan3  + theme(legend.position = c(0.2,0.4), legend.direction = "horizontal")
fig(width = 24,heigth = 16); show(ga)