#####
library(latex2exp)

setwd(paste(getwd(), "Lab5", sep = "/"))
p.des <- read.csv("medidasPavaDesnuda.csv", header = T, sep = ";")
p.ves <- read.csv("medidasPavaVestida.csv", header = T, sep = ";")

# 
orden <- function(medidas){
  colnames(medidas) <- c("h","m","s","T")
  medidas$t <- medidas$h * 3600 + medidas$m * 60  + medidas$s
  t0 <- medidas[medidas$T == max(medidas$T),]$t[1]
  medidas <- medidas[medidas$t >= t0,]
  medidas$t <- medidas$t - t0
  
  medidas
}
error <- function(medidas){
  medidas$dT <- NA
  for (i in 1:nrow(medidas)){
    if (medidas$T[i] < 85)
      medidas$dT[i]<-0.5
    else 
      medidas$dT[i] <- 2
  }
  medidas
}

des <- orden(p.des)
ves <- orden(p.ves)
ves <- ves[ves$t >200,]
plot(ves$t, ves$T, col = "blue", pch = 20)
des <- des[des$T <= max(ves$T),]
des <- orden(des)
ves <- orden(ves)
des <- error(des); ves <- error(ves)
ves <- ves[1:min(nrow(ves), nrow(des)),]
des <- des[1:min(nrow(ves), nrow(des)),]
desAm <- 19.2
vesAm <- 18.7
dAm <- 0.5
dt <- 1


##### 
# T vs t
par(mar = c(5.1, 5.1,4.1,2.1))
plot(des$t, des$T, main = "T vs t",
     xlab = "t(s)", ylab = "T(°C)",
     pch = 20, cex.lab = 1.5, cex.main = 2)
points(x = ves$t, y = ves$T, col = "blue", pch = 20)
for (i in -1:1){
  lines(x = ves$t, y = ves$T + i * ves$dT, col = "blue", pch = 20, lwd = 1)
  lines(x = des$t, y = des$T + i * des$dT, col = "black", pch = 20, lwd = 1)
}

legend(x = 0, y = 70, legend = c("Pava vestida", "Pava desnuda"),
       col = c("black", "blue"), lwd = 3, bty = "n", cex = 1.5)

#####
#Semi Log
par(mar = c(5.1, 7.1, 5.1, 2.1))
plot(ves$t, log((ves$T-vesAm)/(ves$T[1]-vesAm)), col = "blue",
     main = TeX("$ ln\\left(\\frac{T -T_a}{T_0 -T_a}\\right) $ vs $ t $, Pava vestida", italic = T),
     xlab = "t(s)", ylab = TeX("$ ln\\left(\\frac{T -T_a}{T_0 -T_a}\\right) $"),
     cex.main = 2, cex.lab = 1.5)
#ajuste lineal
aju.rv <- lm(log((ves$T-vesAm)/(ves$T[1]-vesAm)) ~ ves$t) 
abline(aju.rv, col = "red", lwd = 2)
sm.rv <- summary(aju.rv)
rv <- -1 * sm.rv$coefficients[2,1]
d.rv <- sm.rv$coefficients[2,2]
legend(legend = c("ajuste lineal", paste("r = ",round(rv,7), "±", round(d.rv,7))),
       x = 0, y = -0.3, lwd = 3, col = "red", bty = "n", lty = c(1,0), cex = 1.5)


plot(des$t, log((des$T-vesAm)/(des$T[1]-desAm)),
     main = TeX("$ ln\\left(\\frac{T -T_a}{T_0 -T_a}\\right) $ vs $ t $, Pava desnuda", italic = T),
     xlab = "t(s)", ylab = TeX("$ ln\\left(\\frac{T -T_a}{T_0 -T_a}\\right) $"),
     cex.main = 2, cex.lab = 1.5)
#ajuste lineal
aju.rd <- lm(log((des$T-desAm)/(des$T[1]-desAm)) ~ des$t) 
abline(aju.rd, col = "red", lwd = 2)
sm.rd <- summary(aju.rd)
rd <- -1 * sm.rd$coefficients[2,1]
d.rd <- sm.rd$coefficients[2,2]
legend(legend = c("ajuste lineal", paste("r = ",round(rd,7), "±", round(d.rd,7))),
       x = 0, y = -0.3, lwd = 3, col = "red", bty = "n", lty = c(1,0), cex = 1.5)
##### 
# Modelos
par(mar = c(5.1, 5.1,4.1,2.1))
# Pava vestida
plot(y = ves$T, x =ves$t, main = "T vs t, Pava vestida",
     xlab = "t(s)", ylab = "T[°C]", col = "blue", pch = 20,
     cex.main = 2, cex.lab = 1.5)

dy.v <- abs(1 - exp(-rv* ves$t)) * 0.5 + exp(-rv* ves$t) * 0.5 + 
  abs((ves$T[1] - vesAm) * exp(-rv* ves$t) * rv) * dt + abs((ves$T[1] - vesAm) * exp(-rv* ves$t) * ves$t) * d.rv


for (i in -1:1){
  lines(x = ves$t, y = vesAm + (ves$T[1] - vesAm)* exp(-rv * ves$t) + dy.v * i, col = "red",
         pch = 20, lwd = 3-2 * abs(i))
  lines(x = ves$t, y = ves$T + i * ves$dT, col = "blue", pch = 20, lwd = 1)
}
legend(x = 0, y = 70, legend = c("Medidas","Modelo con r del ajuste"), col = c("blue", "red"),
       bty = "n", lwd = 3, cex = 1.5)

# Pava desnuda
plot(y = des$T, x =des$t, main = "T vs t, Pava desnuda",
     xlab = "t(s)", ylab = "T[°C]", col = "black", pch = 20,
     cex.main = 2, cex.lab = 1.5)

dy.d <- abs(1 - exp(-rd* des$t)) * 0.5 + exp(-rd* des$t) * 0.5 + 
  abs((des$T[1] - desAm) * exp(-rd* des$t) * rd) * dt + abs((des$T[1] - desAm) * exp(-rv* ves$t) * ves$t) * d.rv


for (i in -1:1){
  lines(x = des$t, y = desAm + (des$T[1] - desAm)* exp(-rd * des$t) + dy.d * i, col = "red",
        pch = 20, lwd = 3-2 * abs(i))
  lines(x = des$t, y = des$T + i * des$dT, col = "black", pch = 20, lwd = 1)
}
legend(x = 0, y = 70, legend = c("Medidas","Modelo con r del ajuste"), col = c("black", "red"),
       bty = "n", lwd = 3, cex = 1.5)

# y = Ta + (T0 - Ta) * exp(-r t)
# dy = | 1 - e^(-rt) | d.Ta + e^(-rt) d.To + | (To - Ta) e^(-rt)r | d.t + | ( To- Ta) e^(-rt) t| d.r
# elabore un modelo que permita conocer el intervalo de tiempo en el que el agua se puede utilizar para cebar
# mate en función de la temperatura ambiente
# t = -ln()
plot(y = - log((72 - seq(1,40,0.1))/(92 - seq(1,40,0.1))) * 1 / rv, x = seq(1,40,0.1), 
     xlab = TeX("$T_a$(°C)"), ylab = "t(s)",
     main = TeX("Tiempo cuando el mate es aceptable vs $T_a$"),
     pch = 20, col = "blue", ylim = c(1500, 3500), cex.lab = 1.5,
     cex.main = 2)
points(y = - log((72 - seq(1,40,0.1))/(92 - seq(1,40,0.1))) * 1 / rd, x = seq(1,40,0.1), pch = 20)
legend(x = 0, y = 3000, legend = c("Pava vestida", "Pava desnuda"), pch = 20, col = c("blue", "black"),
       bty = "n", cex = 1.5)

