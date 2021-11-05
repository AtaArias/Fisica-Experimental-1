setwd(paste(getwd(), "Lab7", sep = "/"))

limpieza <- function(dataSet){
  malos <- dataSet[dataSet$Interrupt == 13,]$Tirada
  dataSet <- dataSet[!(dataSet$Tirada %in% malos),]
  buenos <- dataSet[dataSet$Interrupt == 12,]$Tirada
  dataSet <- dataSet[dataSet$Tirada %in% buenos,]
  
  dataSet
}

liv <- read.csv("cebraSola.csv", header = T, sep = ";")
liv2 <- read.csv("cebraSola2.csv", header = T, sep = ";")
pes <- read.csv("cebraPesada.csv", header = T, sep = ";")

pes <- limpieza(pes)
liv <- limpieza(liv)
liv2 <- limpieza(liv2)
mean(pes[pes$Interrupt == 12,]$t)
for (i in pes$Tirada){
  pes[pes$Tirada == i,]$t <- pes[pes$Tirada == i,]$t - pes[pes$Tirada == i & pes$Interrupt == 1,]$t
}
mean(pes[pes$Interrupt == 12,]$t)
mean(liv[liv$Interrupt == 12,]$t)
mean(liv2[liv2$Interrupt == 12,]$t)
