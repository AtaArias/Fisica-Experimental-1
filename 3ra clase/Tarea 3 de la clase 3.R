# gráficos x, y con barras de error

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
library(ggplot2)
library(latex2exp)
library(fitdistrplus)

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
setwd(paste(getwd(), "documentos", sep="/"))




a <- read.csv("calibra.csv", sep = ",", header = T); head(a, 3) #carga y visualiza datos para corroborar

#pasamos todo de radianes a grados sexagesimales, para que podamos visualizar esos ángulos naturalmente
a$Atrig <- a$Atrig*180/pi; a$Aacel <- a$Aacel*180/pi; a$sAacel <- a$sAacel*180/pi
head(a)

#barras de error, eje x ángulo trig, eje  y error y angulo del acelerómetro
#dejamos dos símbolos, para soja y maíz con un truquito, agregamos detalles
plot(x = a$Atrig, y =  a$Aacel, pch = 16,  cex = 1.5, main = "Calibración Ángulos",xlab = "Ángulo Trigonometría (°)", ylab = "Ángulo Acel. (°)",
     cex.lab = 1.4,cex.main = 1.5, cex.axis = 1.2)

arrows(x0 = a$Atrig, x1 = a$Atrig, 
       y0 = a$Aacel - a$sAacel, #la media - s
       y1 = a$Aacel + a$sAacel, #la media + s
       length =0, lwd = 1.5
)


# nos queda un barra por afuera, usamos xlim() para acomodar
plot(x = a$Atrig, y =  a$Aacel, pch = 16,  cex = 1.5, main = "Calibración Ángulos",xlab = "Ángulo Trigonometría (°)", ylab = "Ángulo Acel. (°)",
     cex.lab = 1.4,cex.main = 1.5, cex.axis = 1.2, ylim = c(-1,78))

arrows(x0 = a$Atrig, x1 = a$Atrig, 
       y0 = a$Aacel - a$sAacel, #la media - 3*s
       y1 = a$Aacel + a$sAacel, #la media + 3*s
       length =0, lwd = 1.5
)

# ajuste linear


#tarea 4
#pdf(nombre, width, height) ...  dev.off()
# 
pdf("BarrasError169.pdf", width = 16, height = 9)

ajuste <- lm(a$Aacel ~ a$Atrig)

plot(x = a$Atrig, y =  a$Aacel, pch = 16,  cex = 1.5,main = "Calibración Ángulos",xlab = "Ángulo Trigonometría (°)", ylab = "Ángulo Acel. (°)",
     cex.lab = 1.4,cex.main = 1.5, cex.axis = 1.2, xlim = c(0,70), ylim = c(-1,78), type = "n")
abline(a = 0,b = 1, lty = 1, lwd = 10, col = "grey85")
abline(ajuste, lwd = 2, col = "red3", )
points(x = a$Atrig, y =  a$Aacel, pch = 16,  cex = 1.5) #ploteo encima para que se vean bien los puntos

arrows(x0 = a$Atrig, x1 = a$Atrig, 
       y0 = a$Aacel - a$sAacel, #la media - 3*s
       y1 = a$Aacel + a$sAacel, #la media + 3*s
       length =0, lwd = 1.5
)
#ajuste y línea x = y

#LEYENDA
legend(x = 0, y = 80, #dónde va la leyenda en unidades del plot
       legend=c("Medidas","x = y","Ajuste Lineal"),
       pch = c(16,NA,NA), #no hay puntos en los ajustes, sólo líneas
       lty = c(0,1,1), #sin línea en los lugares de los puntos
       lwd = c(1,10,1),
       col = c("Black", "Grey85", "red3"),
       cex = 1.5,
       bty = "n",
       bg = NA,box.lwd = 0 #sin color de fondo y sin "caja"
)
 
dev.off()


summary.lm(ajuste)


# cambio de escala usando un log
# definimos x e y
x <- seq(0,10,0.1)
y <- exp(0.5 * x) # e^x
plot(x,y, ylab = "e^{1/2 x)")

# Indicando a plot que use logs
plot(x,y,log = "y"); #recordamos siempre que log es logaritmo natural 

a <- lm(log(y) ~ x); #ajuste de una recta en escala semi-log

abline(a)#nos grafica una recta, que tiene pendiente a, y ordenada al origen b,
#pero en la escala transformada...y sale cualquier cosa


ord.origen <- a$coefficients[1]; pendiente <- a$coefficients[2]; #sacamos los parámetros del ajuste
ord.origen; pendiente #imprime parámetros de ajuste
# Las rectas no coinciden porque a está en otra escala no log
# pasamos de la recta a la exponencial de la cual se tomó la recta y los puntos

pdf("ExpEscalaLog169.pdf", width = 16, height = 9)

# graficamos un exp
plot(x,y,log = "y")
lines(x = x, y = exp(ord.origen+pendiente*x), col = "red")
#ahora sí funciona, porque graficamos una exponencial

dev.off()
# 2da forma

plot(x, log(y))
abline(a, col = "Deepskyblue4", lwd = 2)

# queremos ver el valor de y no de su log
plot(x, log(y), ylab = "y (unidades) [log]", yaxt = "n") #yaxt le dice que no grafique intervalos el eje y

labels <- c(1,10,50,100,150) #etiquetas que vamos a querer
axis(2,at=log(labels),labels=labels) #vean que at= log(labels) indica que ponga cada etiqueta
# en las posiciones log(etiquta) del eje

abline(a, col = "Deepskyblue4", lwd = 2) #podemos, claro, dibujar la recta, porque la escala
#del gráfico no está transformada.


### escalas log - log
# tenemos la ecuación y = [x / C]^(1/c) + 5

y <- (x/2)^{1/2} + 5 # acá pusimos C = 2, pero podría ser cualquier valor
plot(x,y)

# Si conocemos el valor de c y C podemos testear si los valores corresponden a la función graficada igualando
# plot (ecuación de arriba, y)
set.seed(as.integer(runif(1,10,1000)))
C.azar <- runif(min = 0, max = 10, n = 1); #número aleatorio para la constante C
e.azar <- runif(min = 0, max = 10, n = 1); #número aleatorio para el denominador del exponente 1/c
y <- (x/ C.azar )^{1/e.azar} + 5
plot(x,y)


y.prima <- y - 5;
plot(log(x), log(y.prima))
ajuste <- lm(log(y.prima) ~ log(x)); pendiente <- ajuste$coefficients[2];
# lo anterior tira error porque dividmos por 0
# ahora lo hacemos de nuevo tomando solo valores mayores a 0
ajuste <- lm(log(y.prima[x>0]) ~ log(x[x>0]));ord.origen <- ajuste$coefficients[1] ;pendiente <- ajuste$coefficients[2];

pdf("logs19.pdf", width = 16, height = 9)

plot(log(x), log(y.prima))
abline(ajuste, col = "red")

dev.off()

# y = (x/C)^1/c +5  | y' = y - 5 = (x/C)^1/c   log(y') = log(y - 5) = 1/c log(x/C) = 1/c (log(x) - log(C))
# Tenemos las ecuación de y' sacamos log a ambos lados para llegar a la siguiente expresión
# log(y') = log(x) / c - log(C) / c
# esta expresión coincide con la ecuación de una recta y = ax + b, donde y = log(y')| x = log(x) | a = 1/c | b = - log(C) / c
# la ordenada al origen o b = - log(C) / c, por lo tanto, C = exp(b * (-c)
# la pendiente es 1 / c = a entonces c = 1 / a
c = 1 / pendiente
C = exp(ord.origen * (-c))

print(paste("El c calculado es: ", toString(c)," el c conseguido al azar es: ", toString(e.azar)," su diferencia es: ", toString(c - e.azar)))
print(paste("El C calculado es: ", toString(C)," el C conseguido al azar es: ", toString(C.azar)," su diferencia es: ", toString(C - C.azar)))


# Tarea 4 importar como pdf
# Guardar gráficos en el disco
#1ro indicar directorio de trabajo

# Ejemplo en pdf
pdf("Ejemplo.pdf",width=7,height=5) #acá va el tamaño, que puede ser útil

#plot
x=rnorm(1000)
y=rnorm(1000)
plot(x,y, type = "l", lty= 1,lwd=1,col="Deepskyblue4")
lines(y,x,lty=2,lwd = 0.25,col="green4")
#endplot

dev.off()

#Ejemplo en png
png("Ejemplo.png",units = "cm", width = 10, height = 10, res = 200) #acá va el tamaño, que puede ser útil

#plot
x=rnorm(1000)
y=rnorm(1000)
plot(x,y, type = "l", lty= 1,lwd=1,col="Deepskyblue4")
lines(y,x,lty=2,lwd = 0.25,col="green4")
#endplot

dev.off()

# Guarde los gráficos que realizó en este archivo. archivos que tengan igual ancho que alto, y en relación 16:9