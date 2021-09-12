#librerías
library(ggplot2)
library(repr)
library(fitdistrplus)
library(latex2exp)

#####
# Dispersión
pers <- read.csv( file = "per_Atahualpa", header = F, sep = ",", skip = 1)
pers$V4 -> p
plot(p)
#limpiamos
p.limpio <- p [p < median(p) + 0.4 & p > median(p) -0.4]
# pdf("DispersiónPer2.pdf", 10, 10)
# default margings (5, 4, 4, 2) - bottom, left, top, right
par(mar = c(6.1,5.1,5.1,3.1))
plot(p.limpio, pch = 16, 
     xlab = "Número de medida", ylab = "Periodo en segundos",
     main = "Periodos de osilación",
     cex.lab = 1.7, cex.main = 2)
mean <- mean(p.limpio)
text(x = 5, y = 2.08, label = "Mean", col = "blue", cex = 1.5)
abline(h = mean, col = "blue", lwd = 2)
# dev.off()

#####
# Histograma
# pdf("HistogramaPer.pdf", 10, 10)

par(mar = c(6.1, 5.1, 5.1, 3.1))
hist(p.limpio, breaks = nclass.FD(p.limpio), col = "magenta",
     main = "Periodos del pendulo",
     xlab = "segundos", ylab = "Frecuencia",
     cex.axis = 1.35, cex.lab = 1.55, cex.main = 2)
# dev.off()
#####
# Media muestral, desviación estandar, error estandar
# Comenzamos dibujando el histograma

par(mar = c(6.1, 5.1, 5.1, 3.1))
hist(p.limpio, breaks = nclass.FD(p.limpio), col = "magenta",
     main = "Periodos del pendulo",
     xlab = "segundos", ylab = "Frecuencia",
     cex.axis = 1.35, cex.lab = 1.55, cex.main = 2)
abline(v = mean, col = "red", lwd = 2)