#Inicialización: borra todo y carga librerías (se pueden instalar fácilmente si no las tienen):
# install.packages("libreríaquequiero")

#Borra consola, variables y funciones definidas en el entorno Global
rm(list = setdiff(ls(), lsf.str()));
rm(list=lsf.str());
cat("\014")

#librerías, temas
suppressMessages(library("crayon")); suppressMessages(library("ggplot2"));
suppressMessages(library("gridExtra"));suppressMessages(library("latex2exp"))
library(xtable)
library(IRdisplay)
library(repr)

#library con TeX()
install.packages('latex2exp')
library("latex2exp")

#library para el ajuste de PDF y 
install.packages("fitdistrplus")
library("fitdistrplus")
# Loading required package: MASS
# Loading required package: survival
install.packages("MASS")
install.packages("survival")
library(MASS)
library(survival)

#opciones de ploteo ggplot2
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

# histogramas
# en las abcisas variable partida
# en el vertical la frencuencia abosluta o aparición
datos <- read.csv(file = "00.tsv", sep = "\t", header = F)
datos <- datos[ , c(-1, -2, -6)]
colnames(datos) <- c("gx","gy","gz")

# calculamos g
datos$g <- sqrt(datos$gx^2 + datos$gy^2 + datos$gz^2); head(datos, 3)

fig(24, 16)
hist(datos$g)
plot(datos$g)

# se ve que los ultimos datos son ruido, los vamos a borrar
datos <- datos[(1:(nrow(datos) - 10)), ]
plot (datos$g)

hist(datos$g)

par(mar = c(5,5,5,5))
hist(datos$g, breaks = 6, col = "Deepskyblue1", xlab = "g(m / s^2)", main = "Pocas Clases", cex.axis = 2,cex.lab = 3, cex.main = 3 );
hist(datos$g, breaks = 60, col = "Deepskyblue2", xlab = "g(m / s^2)", main = "Muchas Clases", cex.axis = 2,cex.lab = 3, cex.main = 3 );
#la mejor cantidad de clases, con la función nclass.FD() ó nclass.Sturges() ó nclass.scott()
hist(datos$g, breaks = nclass.FD(datos$g), col = "Deepskyblue4", xlab = "g(m / s^2)" ,main = "Clases con nclass.FD", cex.axis = 2,cex.lab = 3, cex.main = 3 )

## medida de tendencia, el promedio de los valores
mm.g <- mean(datos$g); mm.g

hist(datos$g, breaks = nclass.FD(datos$g), col = "Deepskyblue4",
     main = "Medidas de g con acelerómetro",
     xlab = expression(g(m/s^2)), ylab = "Frecuencia",
     cex.axis = 1.35, cex.lab = 1.55, cex.main = 2)
#abline hace líneas rectas (buscar en la ayuda si es necesario)
abline(v = mm.g, col = "Red3", lwd = 3)  #lwd = grosor de la línea

#texto
text(x = 9.89, y = 500, labels = TeX("$\\bar{g}$"), col = "Red3", cex = 2)


# como calcular la desviación estandar
# Si se trata de una distribución normal,  s  estima cuántas medidas caen entre  g¯¯¯±s . Si se utilizan enteros de s, tenemos que:
#   
#   ~68.3% de las medidas están en el intervalo  g¯¯¯±s 
# ~95.4% de las medidas están en el intervalo  g¯¯¯±2s 
# ~99.7% de las medidas están en el intervalo  g¯¯¯±3s
s.g <- sd(datos$g); s.g

# graficación de la  desviación estandar, creo que es la distancia a la media
par(mar = c(10,10,5,0), mgp = c(6,2,0))
hist(datos$g, breaks = nclass.FD(datos$g), col = "Deepskyblue4",
     main = "Medidas de g con acelerómetro",
     xlab = expression(g(m/s^2)), ylab = "Frecuencia",
     cex.axis = 2, cex.lab = 3, cex.main = 3)
#abline hace líneas rectas (buscar en la ayuda si es necesario)
abline(v = mm.g, col = "Red3", lwd = 3)  #lwd = grosor de la línea

#arrows para la sd
arrows(x0 = (mm.g-seq(1,3,1)*s.g), x1 = (mm.g+seq(1,3,1)*s.g),
       y0 = c(250,70,20), y1 = c(250,70,20),
       lwd = 3, col = "yellow3",
       angle = 90,length = 0.1,code = 3,
)

#texto
text(x = 9.89, y = 500, labels = TeX("$\\bar{g}$"), col = "Red3", cex = 2)
text(x = (mm.g+0.005), y = c(260,80,20) + 20, 
     labels = c(TeX("$\\bar{g} \\pm  s$"), TeX("$\\bar{g} \\pm  2s$"), TeX("$\\bar{g} \\pm 3s$")),
     cex = 2.5, col = "yellow3"            
)



## Histograma con probability density function
hist(probability =  T,datos$g, breaks = nclass.FD(datos$g), col = "Deepskyblue4",
     main = "Medidas de g con acelerómetro",
     xlab = expression(g(m/s^2)), 
     ylab = "Densidad", # cambiamos density por densidad
     cex.axis = 1.35, cex.lab = 1.55, cex.main = 2)
#abline hace líneas rectas (buscar en la ayuda si es necesario)
abline(v = mm.g, col = "Red3", lwd = 3)  #lwd = grosor de la línea

#arrows para la sd
arrows(x0 = (mm.g-seq(1,3,1)*s.g), x1 = (mm.g+seq(1,3,1)*s.g),
       y0 = c(25,8,2), y1 = c(25,8,2),
       lwd = 3, col = "yellow3",
       angle = 90,length = 0.1,code = 3,
)

#texto
text(x = 9.89, y = 500, labels = TeX("$\\bar{g}$"), col = "Red3", cex = 2)
text(x = (mm.g+0.005), y = c(25,8,2) + 2, 
     labels = c(TeX("$\\bar{g} \\pm  s$"), TeX("$\\bar{g} \\pm  2s$"), TeX("$\\bar{g} \\pm 3s$")),
     cex = 2.5, col = "yellow3"            
)

# PDF y distribución normal
par(mar = c(10,10,5,0), mgp = c(6,2,0))

hist(probability =  T,datos$g, breaks = nclass.FD(datos$g), col = "Deepskyblue4",
     main = "Medidas de g con acelerómetro",
     xlab = expression(g(m/s^2)), 
     cex.axis = 1.35, cex.lab = 1.55, cex.main = 2)

#ajuste
library("fitdistrplus")
aju <- fitdistrplus::fitdist(data = datos$g, distr = "norm")

#gráficos de líneas
x <- seq(min(datos$g), max(datos$g),length.out = 1000) #distribución normal
lines( x = x, y = dnorm(x = x, mean = aju$estimate[1], sd = aju$estimate[2]), col = "red3", lwd = 3, type = "l", lty = 3)

lines(density(datos$g), col = "Deepskyblue1", lwd = 3) #gráfica del KDE

#se le agrega una leyenda a la gráfica para que sea más facil de leer
par(mar = c(10,10,5,0), mgp = c(6,2,0))
hist(probability =  T,datos$g, breaks = nclass.FD(datos$g), col = "Deepskyblue4",
     main = "Medidas de g con acelerómetro",
     xlab = expression(g(m/s^2)), 
     cex.axis = 2, cex.lab = 3, cex.main = 3)
#abline hace líneas rectas (buscar en la ayuda si es necesario)
abline(v = mm.g, col = "Red3", lwd = 3)  #lwd = grosor de la línea

#arrows para la sd
arrows(x0 = (mm.g-seq(1,3,1)*s.g), x1 = (mm.g+seq(1,3,1)*s.g),
       y0 = c(25,5,1), y1 = c(25,5,1),
       lwd = 3, col = "yellow3",
       angle = 90,length = 0.1,code = 3,
)

#texto media muestral
text(x = 9.89, y = 45, labels = TeX("$\\bar{g}$"), col = "Red3", cex = 3)


#gráficos líneas
x <- seq(min(datos$g), max(datos$g),length.out = 1000) #distribución normal
lines( x = x, y = dnorm(x = x, mean = aju$estimate[1], sd = aju$estimate[2]), col = "red3", lwd = 4, type = "l", lty = 3)

lines(density(datos$g), col = "Deepskyblue1" ,lwd = 4) #gráfica del KDE

#leyenda
legend(x = 9.853, y = 52.5, legend=c("Ajuste Distribución Normal \n (Máx. Likelihood)", "Kernel Density Estimate"),bg = NA,box.lwd = 0,
       col=c("red3", "Deepskyblue1"), lty=c(1,1), lwd = 4,cex= 2, seg.len = 0.75)
