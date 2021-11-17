setwd(paste(getwd(), "Lab7", sep = "/")) # directory with our stuff

library(ggplot2)
library(latex2exp)

par(mar = c(5.1,5.1,5.1,2.1))
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

g.ajuste <- function(table){
  dataFrame <- data.frame(NULL)
  for (i in unique(table$Tirada)){
      datos <- table[table$Tirada == i,]
      datos$t <- datos$t / 1000000
      datos$t2 <- datos$t^2
      aju <- lm(x ~ t + t2, data = datos)
      g <- aju$coefficients[3] * 2
      SEM <- summary(aju)$coefficients[3,2] * 2
      sd <- SEM * sqrt(12)
      
      dataFrame <- rbind(dataFrame, c(g, SEM, sd))
  }
  colnames(dataFrame) <- c("g", "SEM", "sd")
  
  dataFrame <- dataFrame[dataFrame$g < median(dataFrame$g)+1 & dataFrame$g > median(dataFrame$g)- 1,]
  
  dataFrame
}

linear.g.ajuste <- function(table){
  g <- data.frame(NULL)
  for (i in unique(table$Tirada)){
      datos <- table[table$Tirada == i,]
      datos$t <- datos$t / 1000000
      aju <- lm(datos$x/datos$t ~ datos$t)
      
      grv <- 2 * aju$coefficients[2]
      v <- aju$coefficients[1]
      
      sm <- summary(aju)
      
      d.g <- sm$coefficients[2,2]  * 2
      d.v <- sm$coefficients[1,2]
      
      g <- rbind(g, c(grv, d.g, v, d.v))
  }
  colnames(g) <- c("grv", "dg", "v", "dv")
  
  g$sd.g <- g$dg * sqrt(12)
  g$sd.v <- g$dv * sqrt(12)
  
  g <- g[g$grv < median(g$grv)+1 & g$grv > median(g$grv)- 1,]
  
  g
}

# calcula la media de las medias, el sd y el sem
fullData <- function(table){
  # tabla con los ajuste de g
  g_data <- g.ajuste(table)
  
  # calculo de la media, el SEM y el sd
  median <- sum(g_data$g / g_data$sd**2) / sum(1 / g_data$sd**2)
  sd.median <- sqrt(1 / sum(1/g_data$sd**2))
  SEM <- sd.median/sqrt(nrow(g_data))
  c(median, sd.median, SEM)
}

linearFullData <- function(table){
  linear_g_data <- linear.g.ajuste(table)

  meanMean <- sum(linear_g_data$grv / linear_g_data$sd.g**2) / sum(1 / linear_g_data$sd.g**2)
  
  sd.meanMean <- 1 / sqrt (sum(1 / linear_g_data$sd.g^2))
  
  sem.meanMean <- sd.meanMean / sqrt(nrow(linear_g_data))
  
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
for (i in (pes$Tirada)){
    pes[pes$Tirada == i,]$t <- pes[pes$Tirada == i,]$t - pes[pes$Tirada == i & pes$Interrupt == 1,]$t
}
for (i in 1:tail(lCasa$Tirada, n = 1)){
  if (i %in% lCasa$Tirada)
    lCasa[lCasa$Tirada == i,]$t <- lCasa[lCasa$Tirada == i,]$t - lCasa[lCasa$Tirada == i & lCasa$Interrupt == 1,]$t
}
#####
# histogramas de la gravedad aju cuadrático
pes.g <- g.ajuste(pes)
lCasa.g <- g.ajuste(lCasa)


hist_pes <- hist(pes.g$g, nclass.FD(pes.g$g))
hist_lCasa <- hist(lCasa.g$g, nclass.FD(lCasa.g$g))

# histogramas de la gravedad aju cuadrático


c1 <- rgb(0, 0, 255, alpha =  125, names = "blue50",maxColorValue =  255)
plot(hist_lCasa, col = "green3", main ="Ajuste cuadrático",
     xlab = TeX("$ g \\left[\\frac{m}{s^2}\\right]  $"),
     cex.lab = 1.5, cex.main = 2)
plot(hist_pes, add= T, col = c1)

### mean casa
datalCasa <-fullData(lCasa)
abline(v = datalCasa[1], lwd = 3, col = "green")

### mean pes
dataPes <- fullData(pes)
abline(v = dataPes[1], lwd = 3, col = "blue")

### error sd
arrows(x0 = datalCasa[1] + datalCasa[2], x1 = datalCasa[1]-datalCasa[2],
       y0 = 5, y1 =5, code = 3, angle = 90, lwd = 3, col = "green")
arrows(x0 = dataPes[1] + dataPes[2], x1 = dataPes[1]-dataPes[2],
       y0 = 5, y1 =5, code = 3, angle = 90, col = "blue", lwd  = 3)

### error sem
arrows(x0 = datalCasa[1] + datalCasa[3], x1 = datalCasa[1]-datalCasa[3],
       y0 = 10, y1 =10, code = 3, angle = 90, lwd = 3, col = "green")
arrows(x0 = dataPes[1] + dataPes[3], x1 = dataPes[1]-dataPes[3],
       y0 = 10, y1 =10, code = 3, angle = 90, col = "blue", lwd  = 3)

###
text(x = 9.55, y = 4, label = " ± sd", cex = 1.5)
text(x = 9.55, y = 10, label = " ± SEM", cex = 1.5)

###
legend(x = 8.6, y = 30, legend = c("Cebra sin peso", "Cebra con plomada"),
       bty = "n", lwd = 5, lty = 1, col = c("green", "blue"))


















#####################
###
# ajuste lineal
# histogramas de la gravedad aju lineal
linear.pes.g <- linear.g.ajuste(pes)
linear.lCasa.g <- linear.g.ajuste(lCasa)

linear.hist_pes <- hist(linear.pes.g$g, nclass.FD(pes.g$g))
linear.hist_lCasa <- hist(linear.lCasa.g$g, nclass.FD(lCasa.g$g))

# histogramas de la gravedad aju lineal
c1 <- rgb(0, 0, 255, alpha =  125, names = "blue50",maxColorValue =  255)
plot(linear.hist_lCasa, col = "green3", main = "Ajustes lineales de g",
     xlab = TeX("$ g\\left[\\frac{m}{s^2}\\right] $"), cex.main = 2, cex.lab = 1.5)
plot(linear.hist_pes, add= T, col = c1)

### mean casa
linear.datalCasa <-linearFullData(lCasa)
abline(v = linear.datalCasa[1], lwd = 3, col = "green")

### mean pes
linear.dataPes <- linearFullData(pes)
abline(v = linear.dataPes[1], lwd = 3, col = "blue")

### error sd
arrows(x0 = linear.datalCasa[1] + linear.datalCasa[2], x1 = linear.datalCasa[1]-linear.datalCasa[2],
       y0 = 5, y1 =5, code = 3, angle = 90, lwd = 3, col = "green")
arrows(x0 = linear.dataPes[1] + linear.dataPes[2], x1 = linear.dataPes[1]-linear.dataPes[2],
       y0 = 5, y1 =5, code = 3, angle = 90, col = "blue", lwd  = 3)

### error sem
arrows(x0 = linear.datalCasa[1] + linear.datalCasa[3], x1 = linear.datalCasa[1]-linear.datalCasa[3],
       y0 = 10, y1 =10, code = 3, angle = 90, lwd = 3, col = "green")
arrows(x0 = linear.dataPes[1] + linear.dataPes[3], x1 = linear.dataPes[1]-linear.dataPes[3],
       y0 = 10, y1 =10, code = 3, angle = 90, col = "blue", lwd  = 3)

###
text(x = 9.25, y = 4, label = " ± sd", cex = 1.5)
text(x = 9.25, y = 10, label = " ± SEM", cex = 1.5)

###
legend(x = 8.4, y = 30, legend = c("Cebra sin peso", "Cebra con plomada"),
       bty = "n", lwd = 5, lty = 1, col = c("green", "blue"))












###############################
# ejemplo de ajuste cuadrático
tirada <- sample(unique(pes$Tirada),1)
data <- pes[pes$Tirada == tirada,]
data$t <- data$t / 1000000
plot(y = data$x, x = data$t , main = "Ejemplo de ajuste cuadrático",
     xlab = "t(s)", ylab = "x(m)", cex.lab = 1.5, cex.main = 2, pch = 16, col = "blue")
aju <- lm(x ~ t + I(t^2), data = data)
curve(aju$coefficients[1] + aju$coefficients[2] * x + aju$coefficients[3]*x^2, add = T, col = "red", lwd = 2)
legend(x = 0, y = 0.2, legend = TeX("$V_0 \\cdot t + 1/2 g \\cdot t^2  $"),
       bty = "n", lty = 1, lwd = 3, col = "red", cex = 1.5)


# ejemplo de ajuste lineal
plot(y = data$x / data$t, x = data$t , main = "Ejemplo de ajuste lineal",
     xlab = "t(s)", ylab = "x/t(m/s)", cex.lab = 1.5, cex.main = 2, pch = 16,
     col = "blue")
aju <- lm(x / t ~ t, data = data)
abline(aju, col = "red", lwd = 2)
legend(x = 0, y = 1.3, legend = TeX("$x / t = V_0 + 1/2 g \\cdot t  $"),
       bty = "n", lty = 1, lwd = 3, col = "red", cex = 1.5)

print(data)










