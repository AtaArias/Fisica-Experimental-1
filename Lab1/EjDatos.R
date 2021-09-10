# librerías
library(ggplot2)
library(fitdistrplus)
library(repr)
library(xtable)
library(latex2exp)

# Cargamos los datos del periodo en p
pers <- read.csv( file = "per_Atahualpa", header = F, sep = ",", skip = 2)
pers$V4 -> p
plot(p)
# limpiamos
p.limpio <- p[p < 2.4 & p >1.6]
par( mar = c(6.1, 5.1, 5.1, 3.1))
plot(p.limpio,  xlab = "Número de medida", ylab = "Periodo", main = "Distribución de las mediciones de los periodos",
     pch = 16, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
mean = mean(p.limpio)

# Valor medio
abline(h = mean, col = "blue", lwd = 2)
text(x =10, y = 2.1, label = "mean", col = "blue", cex = 1.3)

#####
# Histograma
hist(p.limpio, breaks = nclass.FD(p.limpio), col = "magenta",
     main = "Histograma de los periodos del péndulo",
     xlab = "Periodo", ylab = "Frecuencia",
     cex.axis = 1.3, cex.lab = 1.5, cex.main = 2)
