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
g_aju_pes <- data.frame(NULL)
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
    d.g <- summary(aju)$coefficients[3,2] * 2
    
    g_aju_pes <- rbind(g_aju_pes, c(g, d.g))
    }
  }
}

g_aju_lCasa<- data.frame(NULL)
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
    d.g <- summary(aju)$coefficients[3,2] * 2
    
    g_aju_lCasa <- rbind(g_aju_lCasa, c(g, d.g))
  }
}

head(g_aju_lCasa)
colnames(g_aju_pes) <- colnames(g_aju_lCasa) <- c("g","d.g")

sd_lCasa = (g_aju_lCasa$g / g_aju_lCasa$d.g)**2
SEM_lCasa <- sum(g_aju_lCasa$g / sd_lCasa**2) / sum(1 / sd_lCasa**2)
SEM_lCasa
### SEM = mean / sq(sd) => sq(sd) = mean / SEM => sd = (mean/SEM)^2
mean(sd_lCasa)
mean(g_aju_lCasa$d.g)

SEM_pes <- sum(g_aju_pes$d.g) / sum(g_aju_pes$d.g / g_aju_pes$g)
SEM_pes

mean(g_aju_lCasa$g)
