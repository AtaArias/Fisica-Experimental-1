# librerías
library(ggplot2)
library(latex2exp)
library(xtable)
library(IRdisplay)
library(repr)

# Constantes
csvDirs = "C:/Users/atahu/OneDrive/Escritorio/Universidad/1er año/2do cuatrimestre/Física experimental I/PrimerLab/Medidas de tau"
g.Raga = 9.79316985



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

par(mar = c(6.1, 5.1, 5.1, 3.1))
plot(x = Largos, y = Taus, 
     xlab = "Largo del péndulo", ylab = TeX(r'($\bar{\tau}$)'),
     main = "Periodo vs Largo del péndulo",
     pch = 20, cex.axis = 1.5, cex.main = 2.5, cex.lab = 1.5)

arrows(x0 = tabla$V2, x1 = tabla$V2,
       y0 = tabla$V5 - tabla$V6, y1 = tabla$V5 + tabla$V6,
       length = 0, lwd = 3, col = "red", cex = 1.3)

# graficación del ajuste lineal
plot(x = sqrt(Largos), y = Taus,
     pch = 20)

# Tau Sqrt l = 2pi / sqrt g
plot(x = Taus, y = sqrt(Largos) * 2 * pi / sqrt(g.Raga),
     xlab = "Tiempo medido", ylab = "Tiempo modelo",
     main = "Tiempo medido vs tiempo modelo")

# Tau = sqrt(l) 2pi / sqrt g, x = sqrt(l) => y(x) = x 2pi / sqrt g
plot(x = sqrt(Largos), y = Taus, pch = 19, 
     xlim = c(0,1.5), ylim = c(0,3))
abline(a = 0, b = 2 * pi / sqrt(g.Raga), col = "Yellow", lwd = 2, lty = 1)

ajuste.pendiente <- lm(Taus ~0+sqrt(Largos))
abline(a = 0, b = 2.025, col = "red", lwd = 2, lty = 1)

ajuste.pendiente.ord <- lm(Taus ~sqrt(Largos))
Ord.origen = ajuste.pendiente.ord$coefficients[1]
Pendiente = ajuste.pendiente.ord$coefficients[2]
abline(a = Ord.origen, b = Pendiente, col = "blue", lwd = 2, lty = 1)

legend(x = 0, y =3, 
       legend = c("Ajuste algebraico", "Ajuste lineal con ordenada 0", "Ajuste lineal"), 
       box.lty = 0, bty = "n", col = c("red", "blue", "Yellow"),
       lwd = 4, lty = c(1,1,1), seg.len = 3)

