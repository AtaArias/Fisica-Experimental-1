setwd(paste(getwd(), "Lab7", sep = "/")) # directory with our stuff

##### 
#funciones

# Saca todos los datos con menos o mas de 12
limpieza <- function(dataSet){
  malos <- dataSet[dataSet$Interrupt == 13,]$Tirada
  dataSet <- dataSet[!(dataSet$Tirada %in% malos),]
  buenos <- dataSet[dataSet$Interrupt == 12,]$Tirada
  dataSet <- dataSet[dataSet$Tirada %in% buenos,]
  
  # lee el archivo de las posiciones y coloca su distancia
  dataSet$x <- posc$dist[dataSet$Interrupt]
  
  dataSet
}

g.ajuste <- function(table, dataFrame){
  for (i in 1:tail(table$Tirada, n = 1)){
    if (i %in% table$Tirada){
      datos <- table[table$Tirada == i,]
      datos$t <- datos$t / 1000000
      datos$t2 <- datos$t^2
      aju <- lm(x ~ t + t2, data = datos)
      # plot(datos$t, datos$x, main = paste("tirada",i,"\n x vs t"),
      #      xlab = "t(s)", ylab = "x",pch = 20)
      # lines(datos$t, fitted(aju))
      g <- aju$coefficients[3] * 2
      SEM <- summary(aju)$coefficients[3,2] * 2
      sd <- SEM * sqrt(12)
      
      dataFrame <- rbind(dataFrame, c(g, SEM, sd))
    }
  }
  colnames(dataFrame) <- c("g", "SEM", "sd")
  
  dataFrame
}

linear.g.ajuste <- function(table){
  g <- data.frame(NULL)
  for (i in 1:tail(table$Tirada, n = 1)){
    if (i %in% table$Tirada){
      datos <- table[table$Tirada == i,]
      datos$t <- datos$t / 1000000
      aju <- lm(datos$x/datos$t ~ datos$t)
      
      grv <- 2 * aju$coefficients[2]
      v <- aju$coefficients[1]
      
      sm <- summary(aju)
      
      d.g <- sm$coefficients[2,2]
      d.v <- sm$coefficients[1,2]
      
      g <- rbind(g, c(grv, d.g, v, d.v))
    }
  }
  colnames(g) <- c("grv", "dg", "v", "dv")
  
  g$sd.g <- g$dg * sqrt(12)
  
  g
}

# calcula la media de las medias, el sd y el sem
fullData <- function(table){
  # tabla con los ajuste de g
  g_data <- data.frame(NULL)
  g_data <- g.ajuste(table, g_data)
  
  # calculo de la media, el SEM y el sd
  median <- sum(g_data$g / g_data$sd**2) / sum(1 / g_data$sd**2)
  sd.median <- sqrt(1 / sum(1/g_data$sd**2))
  SEM <- sd.median/sqrt(nrow(g_data))
  c(median, sd.median, SEM)
}

linearFullData <- function(table){
  linear_g_data <- linear.g.ajuste(table)
  
  meanMean <-  sum(linear_g_data$grv / linear_g_data$sd.g^2) / sum (1 / linear_g_data$sd.g^2)
  
  sd.meanMean <- 1 / sqrt (sum(1 / linear_g_data$sd.g^2))
  
  sem.meanMean <- sd.meanMean / sqrt(nrow(table))
  
  c(meanMean, sd.meanMean, sem.meanMean)
}


#####
# constantes y archivos
lCasa <- read.csv("cebraSolaCasa.csv", header = T, sep = ";") # datos sin peso hechos en casa
liv <- read.csv("cebraSola.csv", header = T, sep = ";") # datos sin peso lab
liv2 <- read.csv("cebraSola2.csv", header = T, sep = ";") # datos sin peso lab2
pes <- read.csv("cebraPesada.csv", header = T, sep = ";") # datos con peso hechos en casa

posc <- read.csv("x'sCebra.csv", header = T, sep = ";") # csv con las posiciones
##### 
# limpieza de archivos
pes <- limpieza(pes)
liv <- limpieza(liv)
liv2 <- limpieza(liv2)
lCasa <- limpieza(lCasa)

# t1 = 0
for (i in 1:tail(pes$Tirada, n = 1)){
  if (i %in% pes$Tirada)
    pes[pes$Tirada == i,]$t <- pes[pes$Tirada == i,]$t - pes[pes$Tirada == i & pes$Interrupt == 1,]$t
}
for (i in 1:tail(lCasa$Tirada, n = 1)){
  if (i %in% lCasa$Tirada)
    lCasa[lCasa$Tirada == i,]$t <- lCasa[lCasa$Tirada == i,]$t - lCasa[lCasa$Tirada == i & lCasa$Interrupt == 1,]$t
}
#####

AllData <- data.frame(NULL)
l.AllData <- data.frame(NULL)
# AllData <- rbind(AllData, c("liv", fullData(liv)))
# AllData <- rbind(AllData, c("liv2", fullData(liv2)))
AllData <- rbind(AllData, c("pes", fullData(pes)))
AllData <- rbind(AllData, c("lCasa", fullData(lCasa)))

l.AllData <- rbind(l.AllData, c("pes", linearFullData(pes)))
l.AllData <- rbind(l.AllData, c("lCasa", linearFullData(lCasa)))

colnames(AllData) <- c("data","meanMean", "sd", "SEM")

for (i in 1:4){
  print(as.numeric(AllData$`mean of means`[i]) + as.numeric(AllData$SEM[i]) * c(1,-1))
}

# x = 1/2 a t^2
# x / t = vi + 1/2 a * t
# pendinte = 1/2 a => a = 2 pendiente

data <- linear.g.ajuste(lCasa)

head(data)

head(linear.g.ajuste(pes))
