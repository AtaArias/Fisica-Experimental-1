# Librerías
library(ggplot2)

# Lectura de datos
# vamos a la carpeta con los archivos
setwd(paste(getwd(), "Lab2", "DatosReal", sep = "/"))
files <- list.files(pattern = ".tsv")

# tidy-data
acel <- data.frame(NULL)
for(file in files){
  acel <- rbind(acel, read.csv(file = file, header = F, sep = "\t")[1:2000,])
}
acel <- acel[,c(-1,-2)]
colnames(acel) <- c("ax", "ay", "az", "name")
horz <- acel[acel$name == 0,]
mod0 <- sqrt(horz$ax**2+horz$ay**2+horz$az**2)

for (i in 1:8)
{
  current <- acel[acel$name == i,]
  plot(sqrt(current$ax**2 + current$ay**2+ current$az**2), main = i)
  
  modi <- sqrt(current$ax**2+ current$ay**2+ current$az**2)
  produc <- (horz$ax * current$ax)+(horz$ay * current$ay)+(horz$az * current$az)
  coseno <- produc / (mod0 * modi)
  ang <- acos(coseno) * 180 / pi
  plot(ang, main = paste(i, " ángulo"))
}

