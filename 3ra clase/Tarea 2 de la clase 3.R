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
# setwd("C:/Users/atahu/OneDrive/Escritorio/Universidad/1er año/2do cuatrimestre/Física experimental I/Programación/githum/física experimental 1/Fisica-Experimental-1/3ra clase")
# setwd(paste(getwd(), "documentos", sep = "/"))

cultivos <- read.csv(file = "Crop_FAO.csv"); head(cultivos, 2); tail(cultivos, 2)

plot(x = cultivos$Ano, y = cultivos$Superficie)

plot(x = cultivos$Ano, y = cultivos$Superficie, pch = seq(1,25,1), cex = 1)

#dejamos dos símbolos, para soja y maíz con un truquito, agregamos detalles
fig(24,14);plot(x = cultivos$Ano, y = cultivos$Superficie,
                pch = c(17,16)[as.integer(cultivos$Cultivo == "Soja") + 1], #símbolos triángulo y círculo PLOT CHARACTER
                cex = 2, #tamaño puntos 
                main = "Superficie Cultiva de Soja y Maíz en la Argentina 1960-2014 (FAO)",
                xlab = "Año", ylab = "Superficie (Millones de ha)",
                cex.lab = 1.3, #tamaño letra ejes
                cex.main = 1.5, #tamaño título
                cex.axis = 1.2 #tamaño de los números en los ejes
)

#LEYENDA
legend(x = 1960, y = 20, #dónde va la leyenda en unidades del plot
       legend=c("Soja", "Maíz"),
       pch = c(16,17),
       cex= 2,
       bty = "n",
       bg = NA, box.lwd = 0 #sin color de fondo y sin "caja"
)

head(cultivos, 3)
print(cultivos)


#seleccionamos los datos que queremos
maiz <- cultivos[cultivos$Cultivo == "MaÃ­z" & cultivos$Ano >= 2002, ];head(maiz, 2)
soja <- cultivos[cultivos$Cultivo == "Soja" & cultivos$Ano >= 2002, ]; head(soja, 2)

#hacemos el modelito (ajuste) lineal
aju.maiz <- lm(maiz$Superficie ~ maiz$Ano) #es lm(y ~ x) 
aju.soja <- lm(soja$Superficie ~ soja$Ano) 

print("Pendientes")
summary.lm(aju.maiz)$coefficients[2,1:2]
summary.lm(aju.soja)$coefficients[2,1:2]

print("R²")
summary.lm(aju.maiz)$r.squared
summary.lm(aju.soja)$r.squared

print("Crece la soja, decrece el maíz")
#estos objetos aju son complejos y tienen mucha info


#graficamos el ajuste encima de los datos

#sólo ploteo entre el 2002 y el 2014
plot(x = cultivos$Ano, y = cultivos$Superficie, pch = c(17,16)[as.integer(cultivos$Cultivo == "Soja") + 1],
     cex = 1, main = "Superficie Cultiva de Soja y Maíz en la Argentina 1960-2014 (FAO)",xlab = "Año", ylab = "Superficie (Millones de ha)",
     cex.lab = 1.4,cex.main = 1.5, cex.axis = 1.2, xlim = c(2002,2014))

#gráfico rectas ajustes
abline(aju.soja, lty = 1)
abline(aju.maiz, lty = 2)

#LEYENDA
legend(x = 2001.5, y = 20.5, #dónde va la leyenda en unidades del plot
       legend=c("Soja","Ajuste Soja","Maíz", "Ajuste Maíz"),
       pch = c(16,NA,17,NA), #no hay puntos en los ajustes, sólo líneas
       lty = c(0,1,0,2), #sin línea en los lugares de los puntos
       cex = 1,
       bty = "n", bg = NA,box.lwd = 0 #sin color de fondo y sin "caja"
)


# Tarea 2¶
# Cambie los años de graficación usando, en plot(), el parámetro xlim = c(1996,2015)
# Cambie el rango de la Superficie, usando ylim(0,54). 54 Mha es la nueva superficie de la zona cultivable, dados desmontes y quemazones.
# Cambie las líneas de los ajustes por líneas enteras, con dos colores diferentes. Ajuste los parámetros de la leyenda.

plot(x = cultivos$Ano, y = cultivos$Superficie, pch = c(16,17)[as.integer(cultivos$Cultivo == "Soja") + 1],
     cex = 1, main = "Superficie de cultivo. Soja vs Maiz. Arg 1996-2015", xlab = "Año", ylab = "Superficio (Millones de ha",
     cex.lab = 1.4, cex.main = 1.5, cex.axis = 1.2, xlim = c(1996, 2015), ylim = c(0, 54))

#gráfico rectas ajustes
abline(aju.soja, lty = 1, col= "blue")
abline(aju.maiz, lty = 1, col = "red")

#LEYENDA
legend(x = 1997, y = 55, #dónde va la leyenda en unidades del plot
       legend=c("Soja","Ajuste Soja","Maíz", "Ajuste Maíz"),
       pch = c(16,NA,17,NA), #no hay puntos en los ajustes, sólo líneas
       lty = c(0,1,0,1), #sin línea en los lugares de los puntos
       col = c("black" , "blue", "black", "red"),
       cex = 1,
       bty = "n", bg = NA,box.lwd = 0 #sin color de fondo y sin "caja"
)
