#####
# Librerías
library(ggplot2)

# colores
library(paletteer)
colores <- paletteer::paletteer_d("ggsci::default_aaas")

# Lectura de datos  
# vamos a la carpeta con los archivos
setwd(paste(getwd(), "Lab2", "DatosReal", sep = "/"))
files <- list.files(pattern = ".tsv")

#####
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
#####
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

#####
# Cálculo de los ángulos
# seleccionamos la primera medida, la horizontal
angulos <- data.frame(NULL)
horz <- c(mean(acel[acel$name == 0,]$ax), mean(acel[acel$name == 0,]$ay), mean(acel[acel$name == 0,]$az), acel[acel$name == 0,]$media[1])
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
  produc <- (horz[1] * current$ax)+(horz[2] * current$ay)+(horz[3] * current$az)
  
  # angulo a través del arcoseno
  coseno <- produc / (current$g * horz[4])
  ang <- acos(coseno) * 180 / pi
  s.dev <- sd(ang)
  SEM <- s.dev / sqrt(length(ang))
  a.media <- mean(ang)
  
  # información sobre los angulos
  datos <- c((i * 10), a.media, SEM)
  angulos <- rbind(angulos, datos)
}
colnames(angulos) <- c("medida", "acelAng", "delta Acel")

#####
## graficamos aquellas mediciones con el menor y la mayor sd
min.sd <- angulos[angulos$`delta Acel` == min(angulos$`delta Acel`),]$medida / 10
max.sd <- angulos[angulos$`delta Acel` == max(angulos$`delta Acel`),]$medida / 10

for(name in c(min.sd, max.sd)){
  # seleccionamos los datos tita name
  current <- acel[acel$name == name,]
  
  # producto punto
  produc <- (horz[1] * current$ax)+(horz[2] * current$ay)+(horz[3] * current$az)
  
  # angulo a través del arcoseno
  coseno <- produc / (current$g * horz[4])
  ang <- acos(coseno) * 180 / pi
  s.dev <- sd(ang)
  SEM <- s.dev / sqrt(length(ang))
  a.media <- mean(ang)
  
  if (name == 8) {
    tipo <- "maximo"
  }
  else
    tipo <- "mínimo"
  
  # histograma de cada ángulo
  hist(ang, breaks = nclass.scott(ang),
       main = paste(name * 10, " ángulo, con sd", tipo), xlab = "ángulo (°)",
       pch = 20, col = colores[5],
       cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
  
  # valor medio del ángulo
  abline(v = a.media, col = "red", lty = 1, lwd = 2)
  text(x = a.media + 0.04, y = -4, label = "Media", pch = 1.5)
  
  # standard deviation
  arrows(x0 = a.media + ((1:3) * s.dev), x1 = a.media - ((1:3) * s.dev),
         y0 = c(150, 50, 10), y1 = c(150, 50, 10),
         col = "yellow", lwd = 2,
         angle = 90, length = 0.1, code = 3)
  text(x = a.media + 0.04, y = c(150, 50, 10) + 5,
       label = paste("Ang ±", (1:3), "sd", sep = ""), col = "yellow")
  
  # SEM
  arrows(x0 = a.media - SEM, x1 = a.media + SEM, 
         y0 = 100, y1 = 100, col = colores[2], lwd = 2, 
         angle = 90, code = 3, length = 0.1)
  text(x =a.media +0.04, y = 100, col = colores[2], label = "SEM", pch = 1.5)
}

#####
# Curva de calibración y errores
Trigs <- read.csv("trigDatos.csv")
cosen <- Trigs$adyacente / Trigs$Hip
angulos$trigAng <- acos(cosen) * (180 / pi)
angulos$"delta Trig" <- (( 1 / (sqrt( 1 - cosen ^ 2) * Trigs$Hip)) * Trigs$d.med) * (1 + cosen)
angulos$"delta Trig" <- angulos$`delta Trig` * 180 / pi

## plot de calibración
plot(y = angulos$acelAng, x = angulos$trigAng, pch = 20,
     main = "Curva de calibración", ylab = "Ángulos con acelerómetro (°)", xlab = "Ángulos con trigonometría (°)",
     cex.axis = 1.3, cex.lab = 1.3, cex.main = 2,
     col = colores[angulos$medida / 10])

# curva perfecta
abline(a = 0, b = 1, col = "red3")

# errores
arrows(x0 = angulos$trigAng, x1 = angulos$trigAng, 
       y0 = angulos$acelAng + angulos$`delta Acel`, 
       y1 = angulos$acelAng - angulos$`delta Acel`,
       col = "yellow", length = 0, lwd = 1)

# ajuste lineal
ajuste <- lm(angulos$acelAng ~ angulos$trigAng)
abline(ajuste, lwd = 2, col = "yellow3")

#leyenda
legend(x = 20, y = 80, legend = c("y = x", "ajuste lineal"),
       col = c("red3", "yellow"), lty = 1, lwd = 3, bty = "n",)

# diferencias en la ordenada al origen es un error sitemático
sis_error <- ajuste$coefficients[1]
# diferencias en la pendiente es otra cosa
pendiente <- ajuste$coefficients[2]
# error de la pendiente

# r squared
r.sq <- summary(ajuste)$r.squared

legend(x =35, y = 30, legend = c(
                                  paste("ordenada al origen del ajuste: ", round(sis_error, 4)), 
                                  paste("pendiente del ajuste: ", round(pendiente, 4)),
                                  paste("R-squared: ", round(r.sq, 4))
                                  ),
       bty = "n", cex = 0.9
       )

print(paste(" R-cuadrado: ", round(r.sq, 4), sep = ""))
print(paste("Error sistemático (ordenada al origen del ajuste) de: ", round(sis_error, 4), "°", sep = ""))
print(paste("La pendiente del ajuste lineal es: ", round(pendiente, 4), sep = ""))


