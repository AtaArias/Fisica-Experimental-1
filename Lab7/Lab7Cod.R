setwd(paste(getwd(), "Lab7", sep = "/"))

##### 
#funciones
limpieza <- function(dataSet){
  malos <- dataSet[dataSet$Interrupt == 13,]$Tirada
  dataSet <- dataSet[!(dataSet$Tirada %in% malos),]
  buenos <- dataSet[dataSet$Interrupt == 12,]$Tirada
  dataSet <- dataSet[dataSet$Tirada %in% buenos,]
  
  dataSet$x <- posc$dist[dataSet$Interrupt]
  
  dataSet
}

#####
# constantes y archivos
lCasa <- read.csv("cebraSolaCasa.csv", header = T, sep = ";")
liv <- read.csv("cebraSola.csv", header = T, sep = ";")
liv2 <- read.csv("cebraSola2.csv", header = T, sep = ";")
pes <- read.csv("cebraPesada.csv", header = T, sep = ";")

posc <- read.csv("x'sCebra.csv", header = T, sep = ";")
##### 
# limpieza de archivos
pes <- limpieza(pes)
liv <- limpieza(liv)
liv2 <- limpieza(liv2)
lCasa <- limpieza(lCasa)

for (i in pes$Tirada){
  pes[pes$Tirada == i,]$t <- pes[pes$Tirada == i,]$t - pes[pes$Tirada == i & pes$Interrupt == 1,]$t
}
for (i in lCasa$Tirada){
  lCasa[lCasa$Tirada == i,]$t <- lCasa[lCasa$Tirada == i,]$t - lCasa[lCasa$Tirada == i & lCasa$Interrupt == 1,]$t
}
#####
#
for (i in 1:tail(pes$Tirada, n = 1)){
  if (i %in% pes$Tirada){
    datos <- pes[pes$Tirada == i,]
    plot(datos$t / 1000000, datos$x, main = paste("tirada",i,"\n x vs t"),
         xlab = "t(s)", ylab = "x",pch = 20)
  }
}


