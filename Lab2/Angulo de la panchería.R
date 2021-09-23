# Librerías
library(ggplot2)

# Lectura de datos
# vamos a la carpeta con los archivos
setwd(paste(getwd(), "Lab2", "Datos", sep = "/"))
files <- list.files(pattern = ".tsv")

# tidy-data
acel <- data.frame(NULL)
for(file in files){
  acel <- rbind(acel, read.csv(file = file, header = F, sep = "\t")[1:2000,])
}
acel <- acel[,c(-1,-2)]
colnames(acel) <- c("ax", "ay", "az", "name")

panchos <- acel[acel$name == "panchos",]
horizontal <- acel[acel$name == "horizontal prueba",]

produc <- (panchos$ax * horizontal$ax) + (panchos$ay * horizontal$ay) + (panchos$az * horizontal$az)
mod1 <- sqrt(panchos$ax**2 + panchos$ay**2 + panchos$az**2)
mod2 <- sqrt(horizontal$ax**2 + horizontal$ay**2 + horizontal$az**2)

coseno <- produc / (mod1 * mod2)
  
angle <- acos(coseno) * (180 / pi)
plot(angle, xlab = "número de medida", ylab = "ángulo", main = "Inclinación mesada de la panchería",
     pch = 20, cex.axis = 1.5, cex.main = 2)
abline(h = mean(angle), col = "yellow", lwd = 3)

text(x = 0, y = mean(angle), label = round(mean(angle),5), col = "red")

StandardDev <- sd(angle)
SEM <- StandardDev / sqrt(length(angle))

arrows(y0 = mean(angle) - StandardDev, y1 = mean(angle) + StandardDev,
       x0 = 500, x1 = 500, col = "blue", angle = 90, lwd = 3,code = 3)

arrows(y0 = mean(angle) - SEM, y1 = mean(angle) + SEM,
       x0 = 500, x1 = 500, col = "green", angle = 90, lwd = 3,code = 3)

hist(angle, breaks = nclass.FD(angle), probability = TRUE)

#ajuste
library("fitdistrplus")
aju <- fitdistrplus::fitdist(data = angle, distr = "norm")

#gráficos de líneas
x <- seq(min(angle), max(angle),length.out = 1000) #distribución normal
lines( x = x, y = dnorm(x = x, mean = aju$estimate[1], sd = aju$estimate[2]), col = "red3", lwd = 3, type = "l", lty = 3)

lines(density(angle), col = "Deepskyblue1", lwd = 3) #gráfica del KDE

