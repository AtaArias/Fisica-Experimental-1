# Primera tarea usar los datos$gx
# Informar la media muestral y la desviación estandar de la muestra

#Borra consola, variables y funciones definidas en el entorno Global
rm(list = setdiff(ls(), lsf.str()));
rm(list=lsf.str());
cat("\014")

suppressMessages(library("crayon")); suppressMessages(library("ggplot2"));
suppressMessages(library("gridExtra"));suppressMessages(library("latex2exp"))
library(xtable)
library(IRdisplay)
library(repr)

library(ggplot2)+geom_density(aes(y=0.045*..count..), colour="black", adjust=4)

temajuan3 <- theme(
  legend.title = element_text(size = 14),
  panel.background = element_rect(fill = "gray93"),
  legend.box = "vertical",
  legend.direction = "vertical",
  legend.key.height = unit(0.1,"snpc"),
  legend.key.width  = unit(0.1,"snpc"),
  axis.line = element_line(size = 0.5, lineend = "square"),
  axis.ticks = element_line(size = 1, colour = "black"),
  panel.border = element_rect(size = 1, fill = NA ),
  axis.title = element_text(size = 24),
  axis.text.x  = element_text(size = 20),
  axis.text.y = element_text(size = 20),
  legend.background = element_blank(),
  legend.position = c(0.3,0.8),
  legend.key = element_blank(),
  legend.text = element_text(size = 20)
)
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}

fig(width = 24, heigth = 12)
# setwd("ubicación del archivo")


datos <- read.csv(file = "00.tsv", sep = "\t", header = F); datos <- datos[, c(-1,-2,-6)];
colnames(datos) <- c("gx","gy", "gz"); head(datos, 3)
# limpiamos los valores:

datos <- datos[20: (nrow(datos)- 10), ]

## tendencia central
mm.g <- mean(datos$gx); mm.g

#standar deviation
s.g <- sd(datos$gx);


par(mar = c(10,10,5,0), mgp = c(6,2,0))

hist(probability = T, datos$gx, breaks = nclass.FD(datos$gx), col = "aquamarine2",
     main = "Medidas de g en x con acelerómetro",
     xlab = expression(g(m/s^2)),
     cex.axis = 1.35, cex.lab = 1.55, cex.main = 2)

# ajuste normal
library("fitdistrplus")
aju <- fitdistrplus::fitdist(data = datos$gx, distr = "norm")

# gráficos de líneas
x <- seq(min(datos$gx), max(datos$gx), length.out = 1000) # distribución normal
lines( x = x, y = dnorm(x = x, mean = aju$estimate[1], sd = aju$estimate[2]), col = "red3", lwd = 3, type = "l", lty = 3)


lines(density(datos$gx), col = "Deepskyblue1", lwd = 3) #gráfica del KDE

# media y desviación estandar
#abline hace líneas rectas (buscar en la ayuda si es necesario)
abline(v = mm.g, col = "Red3", lwd = 3)  #lwd = grosor de la línea

#arrows para la sd
arrows(x0 = (mm.g-seq(1,3,1)*s.g), x1 = (mm.g+seq(1,3,1)*s.g),
       y0 = c(40,13,2), y1 = c(40,13,2),
       lwd = 3, col = "yellow3",
       angle = 90,length = 0.1,code = 3,
)

#texto
text(x = -0.09, y = 30, labels = TeX("$\\bar{g}$"), col = "Red3", cex = 2)
text(x = (mm.g+0.0005), y = c(25,-4,-14) + 20, 
     labels = c(TeX("$\\bar{g} \\pm  s$"), TeX("$\\bar{g} \\pm  2s$"), TeX("$\\bar{g} \\pm 3s$")),
     cex = 2.5, col = "yellow3"            
)


legend(x = -0.13, y = 90, legend=c("Ajuste Distribución Normal \n (Máx. Likelihood)", "Kernel Density Estimate"),bg = NA,box.lwd = 0,
       col=c("red3", "Deepskyblue1"), lty=c(1,1), lwd = 4,cex= 2, seg.len = 0.75)


