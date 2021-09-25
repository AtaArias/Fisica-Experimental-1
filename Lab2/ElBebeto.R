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
  # 
  # # filtramos los datos
  # leyendo <- leyendo[leyendo$g < mediana + 0.1 & leyendo$g > mediana - 0.1,]
  
  #  
  # #los cortamos en 1973
  # leyendo <- leyendo[1:2000,]
  # 
  
  # insertamos la media
  leyendo$media <- mean(leyendo$g)
  
  print(length(leyendo[,1]))
  
  acel <- rbind(acel, leyendo)
}


# seleccionamos la primera medida, la horizontal
horz <- acel[acel$name == 0,]
for (i in 1:8)
{
  # seleccionamos los datos tita i
  current <- acel[acel$name == i,]
  
  plot(current$g, main = i, ylab = "gravedad (m/s^2)", xlab = "número de medida",
       pch = 20, col = colores[current$name + 1],
       cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  abline(h = current$media, col = "yellow", lwd = 3)
  
  # producto punto
  produc <- (horz$ax * current$ax)+(horz$ay * current$ay)+(horz$az * current$az)
  
  # angulo a través del arcoseno
  coseno <- produc / (current$g * horz$g)
  ang <- acos(coseno) * 180 / pi
  plot(ang, main = paste(i * 10, " ángulo"), ylab = "ángulo (°)", xlab = "número de medida",
        pch = 20, col = colores[current$name + 1],
        cex.main = 2, cex.lab = 1.5, cex.axis = 1.5)
}

# este los coloca todos uno al lado del otro, para seguir trabajandolo
plot(acel$g - acel$media, col = colores[acel$name + 1],
     pch = 20)
