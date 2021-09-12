# librerias
suppressMessages(library("crayon")); suppressMessages(library("ggplot2"));
suppressMessages(library("gridExtra"));suppressMessages(library("latex2exp"))
library(fitdistrplus)
library(repr)
library(xtable)
library(paletteer)

#colores a usar
colores = paletteer_d("awtools::ppalette")
col.hist = colores[2]; col.tau = colores[8]; col.sd = colores[1]
col.kernel = colores[4]; col.normal = colores[7]; col.SEM = colores[5]


# Tau teoórico + error
# error medio absoluto. Por la herramienta sería de 0.001 m, esta es la mínima escala
L.cm <- 1.038; d.L <- 0.001; L.max <- 1.11; L.min <- 0.97; g.RAGA  <- 9.79316985
tau.modelo <- 2*pi*(sqrt(L.cm/g.RAGA)) 
d.tau.modelo <- abs(pi/sqrt(L.cm * g.RAGA) * d.L)
print( paste("tau es ", toString(round(tau.modelo, 2)), " +- ", toString(round(d.tau.modelo, 2)) ) )                    

# Toma de datos y filtrado de columnas
medidas <- read.csv(file = "per_Atahualpa", sep = ",", head = F, skip = 1)
medidas <- medidas$V4

# limpiamos los datos
mediana = median(medidas)
m.limpios <- medidas[medidas < mediana + 0.4 & medidas > mediana - 0.4]

# Calculamos las variables
mn = mean(m.limpios)
aju <- fitdistrplus::fitdist(data = m.limpios, distr = "norm") # ajuste normal
s.m = sd(m.limpios) # standar deviation
SEM = s.m / sqrt(length(m.limpios))

# Dispersión de los datos
plot(m.limpios, xlab = "Número de medida", ylab = "Periodo", main = "Distribución de las mediciones de los periodos",
     pch = 16, cex.lab = 1.5, cex.axis = 1.5, cex.main = 2)

abline(h = mn, col = col.tau, lwd = 3) # valor medio

arrows(x0 = 100, y0 = mn - s.m, x1 = 100, y1 = mn + s.m, # standar deviation
       lwd = 3, col = col.sd, angle = 90, length = 0.1, code = 3)
abline(h = mn + c(-s.m, s.m), col = col.sd, lwd = 3)

arrows(x0 = 25, y0 = mn - SEM, x1 = 25, y1 = mn + SEM, # SEM
       lwd = 3, col = col.SEM, angle = 90, length = 0.1, code = 3)
abline(h = mn + c(-SEM, SEM), col = col.SEM, lwd = 3)

# Texto
text(label = TeX(r'($\bar{ \tau }$)'), x = 5, y = mn +0.025, col = col.tau, cex = 2) #valor medio
text(label = "Standar \n deviation", x = 100, y = 1.9, col = col.sd, cex = 1.3)
text(label = "SEM", x = 25, y = mn - SEM - 0.025, col = col.SEM, cex = 1.3)

#####

# Calcule y grafique: la media muestral τ , la desviación estándar de la muestra de periodos s τ i , el error
# estándar de la media, SEM .

par(mar = c(6.1, 5.1, 5.1, 3.1))
hist(m.limpios, breaks = nclass.FD(m.limpios), probability = T, col = col.hist,
     main = "Histograma de los periodos del péndulo",
     xlab = "Periodo",
     xlim = c(1.75,2.35),
     cex.axis = 1.3, cex.lab = 2, cex.main = 3)
# Media muestral
abline( v = mn, col = col.tau, lwd = 3)

#arrows para la sd
arrows(x0 = (mn-seq(1,3,1)*s.m), x1 = (mn+seq(1,3,1)*s.m),
       y0 = c(2.7,1.2,0.2), y1 = c(2.7,1.2,0.2),
       lwd = 3, col = col.sd,
       angle = 90,length = 0.1,code = 3,
)

# arrowas para el SEM
arrows(x0 = mn + SEM, x1 = mn - SEM,
       y0 = 4, y1 = 4,
       lwd = 3, col = col.SEM,
       angle = 90, length = 0.1, code = 3)

# Distribución normal
x <- seq(min(m.limpios), max(m.limpios), length.out = 1000)
lines( x = x, y = dnorm(x = x, mean = aju$estimate[1], sd = aju$estimate[2]), col = col.normal, lwd = 3, type = "l", lty = 5)

# Estimación de la pdf, Kernel Density Estimate
lines(density(m.limpios), col = col.kernel, lwd = 3)

# leyenda
legend(x = 1.75, y = 4, legend = c("Distribución normal", "Kernel Density Estimate"), bg = NA, box.lwd = 0, box.lty = 0,
                                   col = c(col.normal, col.kernel), lty = c(5,1), lwd = 4, cex = 1.5, seg.len = 2)


#texto
text(x = mn + 0.006, y = 2, labels = TeX(r'($\bar{ \tau }$)'), col = col.tau, cex = 3)
text(2.025, y = c(3,1.5,0.4), 
     labels = c(TeX(r'($\bar{\tau} \\pm  s$)'), TeX(r'($\bar{\tau} \\pm  2s$)'), TeX(r'($\bar{\tau} \\pm  3s$)')),
     cex = 1.5, col = col.sd)
text(x = mn - 0.025, y = 4, labels = "SEM", col = col.SEM, cex = 1.5)
