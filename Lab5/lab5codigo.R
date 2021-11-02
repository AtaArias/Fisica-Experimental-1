setwd(paste(getwd(), "Lab5", sep = "/"))
p.des <- read.csv("medidasPavaDesnuda.csv", header = T, sep = ";")
p.ves <- read.csv("medidasPavaVestida.csv", header = T, sep = ";")

# 
orden <- function(medidas){
  colnames(medidas) <- c("h","m","s","T")
  print(1)
  medidas$t <- medidas$h * 3600 + medidas$m * 60  + medidas$s
  print(2)
  t0 <- medidas[medidas$T == max(medidas$T),]$t[1]
  print(3)
  medidas <- medidas[medidas$t >= t0,]
  print(4)
  medidas$t <- medidas$t - t0
  print(5)
  
  medidas
}

des <- orden(p.des)
ves <- orden(p.ves)

plot(des$t, des$T)
points(x = ves$t, y = ves$T, col = "blue")
