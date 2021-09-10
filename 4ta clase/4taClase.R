# librerias
library(ggplot2)
library(fitdistrplus)
library(repr)
library(xtable)
library(latex2exp)

# Tau teoórico + error
# error medio absoluto. Por la herramienta sería de 0.001 m, esta es la mínima escala
L.cm <- 1.038; d.L <- 0.001; L.max <- 1.11; L.min <- 0.97; g.RAGA  <- 9.79316985
tau.modelo <- 2*pi*(sqrt(L.cm/g.RAGA)) 
d.tau.modelo <- abs(pi/sqrt(L.cm * g.RAGA) * d.L)
print( paste("tau es ", toString(round(tau.modelo, 2)), " +- ", toString(round(d.tau.modelo, 2)) ) )                    

medidas <- read.csv(file = "per_Atahualpa", sep = ",", head = F, skip = 1)
# tomamos solo los segundos
medidas <- medidas$V4
plot(medidas)

# limpiamos los datos
mediana = median(medidas)
m.limpios <- medidas[medidas < mediana + 0.4 & medidas > mediana - 0.4]

# ploteamos los datos
plot(m.limpios, xlab = "Número de medida", ylab = "Periodo", main = "Distribución de las mediciones de los periodos",
     pch = 16, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)
mn = mean(m.limpios)
abline(h = mn, col = "magenta", lwd = 3)
text(label = "mean", x = 5, y = 2.1 , col = "magenta", cex = 1.3)

#####
# Histograma
hist(p.limpio, breaks = nclass.FD(m.limpios), col = "magenta",
     main = "Histograma de los periodos del péndulo",
     xlab = "Periodo", ylab = "Frecuencia",
     cex.axis = 1.3, cex.lab = 1.5, cex.main = 2)

#####
# Calcule y grafique: la media muestral τ , la desviación estándar de la muestra de periodos s τ i , el error
# estándar de la media, SEM .
aju <- fitdistrplus::fitdist(data = m.limpios, distr = "norm")
hist(p.limpio, breaks = nclass.FD(m.limpios), col = "magenta",
     main = "Histograma de los periodos del péndulo",
     xlim = c(1.75, 2.4),
     xlab = "Periodo", ylab = "Frecuencia",
     cex.axis = 1.3, cex.lab = 1.5, cex.main = 2)
# Media muestral
abline( v = mn, col = "red", lwd = 3)

# standar deviation
s.m = sd(m.limpios)

#arrows para la sd
arrows(x0 = (mn-seq(1,3,1)*s.m), x1 = (mn+seq(1,3,1)*s.m),
       y0 = c(34,13,2), y1 = c(34,13,2),
       lwd = 3, col = "yellow3",
       angle = 90,length = 0.1,code = 3,
)

#texto
text(x = mn + 0.006, y = 30, labels = TeX("$\\bar{T}$"), col = "Red3", cex = 2)
text(x = mn + 0.006, y = c(34,13,2) + 2, 
    labels = c(TeX("$\\bar{T} \\pm  s$"), TeX("$\\bar{T} \\pm  2s$"), TeX("$\\bar{T} \\pm 3s$")),
    cex = 2, col = "yellow")


