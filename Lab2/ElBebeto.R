# Librerías
library(ggplot2)

# Lectura de datos
# vamos a la carpeta con los archivos
setwd(paste(getwd(), "Lab2", "DatosReal", sep = "/"))
files <- list.files(pattern = ".tsv")

# tidy-data
acel <- data.frame(NULL)
for(file in files){
  acel <- rbind(acel, read.csv(file = file, header = F, sep = "\t",
                colClasses = c("integer", "numeric", "numeric", "numeric","numeric", "integer" ))[1:2000,])
}
acel <- acel[,c(-1,-2)]
colnames(acel) <- c("ax", "ay", "az", "name")

# calculamos el g de cada medida
acel$g <- sqrt(acel$ax**2+acel$ay**2+acel$az**2)

# creamos un lugar para la media
acel$med <- NA

# seleccionamos la primera medida, la horizontal
horz <- acel[acel$name == 0,]
for (i in 0:8)
{
  # seleccionamos los datos tita i
  current <- acel[acel$name == i,]
  
  media <- mean(current$g)
  
  acel[acel$name == i,]$med <- media
  plot(current$g, main = i, ylab = "gravedad", xlab = "medida")
  abline(h = media, col = "yellow", lwd = 3)
  
  # producto punto
  produc <- (horz$ax * current$ax)+(horz$ay * current$ay)+(horz$az * current$az)
  
  # angulo a través del arcoseno
  coseno <- produc / (current$g * horz$g)
  ang <- acos(coseno) * 180 / pi
  plot(ang, main = paste(i, " ángulo"))
}

# este los coloco todos uno al lado del otro, para seguir trabajandolo
plot(acel$g - acel$med)
