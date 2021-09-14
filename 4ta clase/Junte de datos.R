# librerías
library(ggplot2)
library(latex2exp)
library(xtable)
library(IRdisplay)
library(repr)
library(paletteer)

## Constantes
csvDirs = "C:/Users/atahu/OneDrive/Escritorio/Universidad/1er año/2do cuatrimestre/Física experimental I/PrimerLab/Medidas de tau"
g.Raga = 9.79316985
# Colores
colores <- paletteer_d("beyonce::X89")
col.l.manual <- colores[3]; col.l.origen <- colores[6]; col.l.libre <- colores[7]


##### 
# Lectura de datos
# Vamos a la carpeta con las medidas y los datos
setwd("C:/Users/atahu/OneDrive/Escritorio/Universidad/1er año/2do cuatrimestre/Física experimental I/PrimerLab/Medidas de tau")

# Leemos los archvos tipo csv
files <- list.files(path = csvDirs,pattern = ".csv")


# creamos la table
tabla <-  data.frame(
  l.min = factor(),
  l.cm = factor(),
  l.mac = factor(),
  d.l = factor(),
  tau = factor(),
  SEM = factor(),
  nombre = factor()
)

# incluimos los datos
for (file in files){
  data = read.csv(file, header = F, sep = ",")
  if (!is.na(data$V2)){
    tabla <- rbind(tabla, data)
  }
}
Taus <- tabla$V5
Largos <- tabla$V2

#####
# Gráfica de L vs Tau
par(mar = c(6.1, 5.1, 5.1, 3.1))
plot(x = Largos, y = Taus, 
     xlab = "Largo del péndulo", ylab = TeX(r'($\bar{\tau}$)'),
     main = "Periodo vs Largo del péndulo",
     pch = 1, cex.axis = 1.5, cex.main = 2.5, cex.lab = 1.5,
     xlim = c(0,3), ylim = c(0,3))

arrows(x0 = tabla$V2, x1 = tabla$V2,
       y0 = tabla$V5 - tabla$V6, y1 = tabla$V5 + tabla$V6,
       length = 0, lwd = 3, col = "red", cex = 1.3)
#####
# graficación del ajuste lineal
# Tau = sqrt(l) 2pi / sqrt g, x = sqrt(l) => y(x) = x 2pi / sqrt g
plot(x = sqrt(Largos), y = Taus, pch = 19,
     xlab = TeX(r'($\sqrt{l_{cm}$)'), ylab = TeX(r'($\bar{\tau}$)'),
     main = "Ajuste lineal",
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 2,
     xlim = c(0,1.5), ylim = c(0,3))

## Ajuste lineales
# Despeje a mano
abline(a = 0, b = 2 * pi / sqrt(g.Raga), col = col.l.manual, lwd = 2, lty = 1)

# modelo lineal con corte en origen
ajuste.pendiente <- lm(Taus ~0+sqrt(Largos))
abline(a = 0, b = 2.025, col = col.l.origen, lwd = 2, lty = 1)

# modelo lineal sin restricción
ajuste.pendiente.ord <- lm(Taus ~sqrt(Largos))
Ord.origen = ajuste.pendiente.ord$coefficients[1]
Pendiente = ajuste.pendiente.ord$coefficients[2]
abline(a = Ord.origen, b = Pendiente, col = col.l.libre, lwd = 2, lty = 1)

legend(x = 0, y =3, 
       legend = c("Despeje a mano", "lm con corte en origen", "lm sin restricción"), 
       box.lty = 0, bty = "n", col = c(col.l.manual, col.l.origen, col.l.libre),
       lwd = 4, lty = c(1,1,1), seg.len = 3)

