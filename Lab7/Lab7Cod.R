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
g_aju_pes <- c()
for (i in 1:tail(pes$Tirada, n = 1)){
  if (i %in% pes$Tirada){
    datos <- pes[pes$Tirada == i,]
    datos$t <- datos$t / 1000000
    datos$t2 <- datos$t^2
    aju <- lm(x ~ t + t2, data = datos)
    plot(datos$t, datos$x, main = paste("tirada",i,"\n x vs t"),
         xlab = "t(s)", ylab = "x",pch = 20)
    lines(datos$t, fitted(aju))
    g <- aju$coefficients[3] * 2
    aju$coefficients[2]
    #lines(x = step())
    #curve()
    g_aju_pes <- c(g_aju_pes, g)
    }
  }
}

g_aju_lCasa<- c()
for (i in 1:tail(lCasa$Tirada, n = 1)){
  if (i %in% lCasa$Tirada){
    datos <- lCasa[lCasa$Tirada == i,]
    datos$t <- datos$t / 1000000
    datos$t2 <- datos$t^2
    aju <- lm(x ~ t + t2, data = datos)
    plot(datos$t, datos$x, main = paste("tirada",i,"\n x vs t"),
         xlab = "t(s)", ylab = "x",pch = 20)
    lines(datos$t, fitted(aju))
    g <- aju$coefficients[3] * 2
    aju$coefficients[2]
    #lines(x = step())
    #curve()
    g_aju_lCasa <- c(g_aju_lCasa, g)
  }
}
mean(g_aju_lCasa)
median(g_aju_lCasa)
max(g_aju_lCasa)
min(g_aju_lCasa)

SEM_lCasa <- sd(g_aju_lCasa) / sqrt(length(g_aju_lCasa))


SEM_pes <- sd(g_aju_pes) / sqrt(length(g_aju_pes))

mean(g_aju_pes) + SEM_pes
mean(g_aju_lCasa) - SEM_lCasa
