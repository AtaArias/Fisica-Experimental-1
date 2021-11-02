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
des <- orden(p.des)
ves <- orden(p.ves)
ves <- ves[ves$t >500 & ves$t < 3500,]
plot(ves$t, ves$T, col = "blue")
des <- des[des$t > 500 & des$t < 3500,]
des <- orden(des)
ves <- orden(ves)
desAm <- 19.2
vesAm <- 18.7

plot(des$t, des$T, main = "T vs t",
     xlab = "t(s)", ylab = "T(°C)")
points(x = ves$t, y = ves$T, col = "blue")
lines(x = ves$t, y = vesAm + (ves$T[1] - vesAm)* exp(r.m * ves$t), col = "red",
       pch = 20, lwd = 3)


plot(ves$t, log((ves$T-vesAm)/(ves$T[1]-vesAm)) / ves$t)
r <- log((ves$T-vesAm)/(ves$T[1]-vesAm)) / ves$t
r <- r[-1]
r.m <- mean(r)
