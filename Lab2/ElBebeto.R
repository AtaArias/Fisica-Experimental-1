# Librerías
library(ggplot2)

# colores
library(paletteer)
colores <- paletteer::paletteer_d("ggsci::default_aaas")

# Lectura de datos  
# vamos a la carpeta con los archivos
setwd(paste(getwd(), "Lab2", "DatosReal", sep = "/"))
files <- list.files(pattern = ".tsv")

# tidy-data
acel <- data.frame(NULL)
# se leen todos los datos, se limpian, genera una columna con el g, otra con la media de g y se inserta
# en el data frame con todos
for(file in files){
  # leemos el archivo
  leyendo <- read.csv(file = file, header = F, sep = "\t",
                      colClasses = c("integer", "numeric", "numeric", "numeric","numeric", "integer" ))
  
  # limpiamos las columnas
  leyendo <- leyendo[,c(-1,-2)]
  
  # nombramos las columnas
  colnames(leyendo) <- c("ax", "ay", "az", "name")
  
  # agregamos una columna con la gravedad
  leyendo$g <- sqrt(leyendo$ax**2 +  leyendo$ay**2 + leyendo$az**2)
  
  mediana <- median(leyendo$g)

  # filtramos los datos
  leyendo <- leyendo[leyendo$g < mediana + 0.04 & leyendo$g > mediana - 0.04,]
  
  #los cortamos en 1973
  leyendo <- leyendo[1:2000,]

  # insertamos la media
  leyendo$media <- mean(leyendo$g)
  
  acel <- rbind(acel, leyendo)
}

# Distribución de los datos de gravedad
par(mar = c(6.1,5.1,5.1,3.1))
plot(acel$g - acel$media,
     main = "Distribución de las medidas de gravedad", xlab = "Número de medida", ylab = "Desviación de g (m/^2)",
     cex.axis = 1.5, cex.lab = 1.5, cex.main = 3,
     pch = 20, col = colores[acel$name + 1],
     ylim = c(-0.06, 0.06))

for (i in 0:8){
  med = i * 10
  legend(x = (i * 2000)-500, y = 0.06, 
         legend = expr(paste(theta, " = ", !!med)), 
         pch = 20, col = colores[i + 1], bty = "n")
}

# seleccionamos la primera medida, la horizontal
angulos <- data.frame(NULL)
horz <- acel[acel$name == 0,]
for (i in 1:8){
  # seleccionamos los datos tita i
  current <- acel[acel$name == i,]
  
  # graficos el g para cada punto
  plot(current$g, xlab = "Número de medida", ylab = "g medido (m/s^2)", 
       main = expr( paste( "g de ", theta, " = ", !!(i * 10) )),
       cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
       pch = 20, col = colores[current$name[1] + 1])
  # media de la gravedad
  abline(h = current$media[1], col = "red", lwd = 2)
  text(x = 200, y = current$media[1] + 0.0025, 
       label = paste("g = ", round(current$media[1],4), "m/^2"), 
       cex = 1.5, col = "red")
  
  # producto punto
  produc <- (horz$ax * current$ax)+(horz$ay * current$ay)+(horz$az * current$az)
  
  # angulo a través del arcoseno
  coseno <- produc / (current$g * horz$g)
  ang <- acos(coseno) * 180 / pi
  SEM <- sd(ang) / sqrt(length(ang))
  plot(ang, main = paste(i * 10, " ángulo"), ylab = "ángulo (°)", xlab = "número de medida",
        pch = 20, col = colores[current$name + 1],
        cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
  
  # información sobre los angulos
  datos <- c((i * 10), mean(ang), SEM)
  angulos <- rbind(angulos, datos)
}
 
colnames(angulos) <- c("medida", "acelAng", "delta Acel")

Trigs <- read.csv("trigDatos.csv")
cosen <- Trigs$adyacente / Trigs$Hip
angulos$trigAng <- acos(cosen) * (180 / pi)
angulos$"delta Trig" <- (( 1 / (sqrt( 1 - cosen ^ 2) * Trigs$Hip)) * Trigs$d.med) * (1 + cosen)
angulos$"delta Trig" <- angulos$`delta Trig` * 180 / pi

plot(x = angulos$acelAng, y = angulos$trigAng, pch = 20,
     col = colores[angulos$medida / 10])
abline(a = 0, b = 1, col = "red")
arrows(x0 = angulos$acelAng, x1 = angulos$acelAng, 
       y0 = angulos$trigAng + angulos$`delta Trig`, 
       y1 = angulos$trigAng - angulos$`delta Trig`,
       col = "yellow", length = 0, lwd = 1)
ajuste <- lm(angulos$trigAng ~ angulos$acelAng)
abline(ajuste, lwd = 2, col = "yellow3")

# diferencias en la ordenada al origen es un error sitemático
sis_error <- ajuste$coefficients[1]
# diferencias en la pendiente es otra cosa
pendiente <- ajuste$coefficients[2]

