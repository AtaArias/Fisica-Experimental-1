setwd(paste(getwd(), "TpFinal", sep = "/")) # directory with our stuff

library(latex2exp)
#g raga
g.Raga <- 9.79316985
# m vs h
crudo <- read.csv("MvsH.csv", header =T, sep =";")
colnames(crudo)<- c("b", "m", "d.m", "h1", "h2", "d.h")
crudo$h1 <- crudo$h1 / 100; crudo$h2 <- crudo$h2 / 100; crudo$d.h <- crudo$d.h / 100
crudo$m <- crudo$m / 1000; crudo$d.m <- crudo$d.m / 1000

medidas <- crudo
medidas$x1 <- (medidas$h1 - medidas[medidas$m == 0,]$h1) * -1
medidas$x2 <- (medidas$h2 - medidas[medidas$m == 0,]$h2) * -1

medidas$d.x <- crudo$d.h*2

# k1 con x vs masa
# x = g/k * m -> pendiente = g/k k = g / pendiente
plot(x = medidas$m, y = medidas$x2, xlab = "m(kg)", ylab = "x(m)", pch = 20, col = "blue",
     main = "x vs m", cex.main =2, cex.lab = 1.5)

m <- medidas$m; x <- medidas$x2; d.x <- medidas$d.x; d.m <- medidas$d.m

arrows(x0 = m, x1 = m, y1 = x + d.x, y0 = x - d.x,
       code = 3, angle = 0, col = "blue", length = 0)

aju1 <- lm(x ~ 0 + m)
abline(aju1, col = "red", lwd = 2)
pen1 <- aju1$coefficients[1]
ke = g.Raga / pen1

legend(x = 0, y = 0.08, 
       legend = c("Datos experimentales",TeX("$ x = \\frac{g}{k} \\cdot m $")),
       bty = "n", lwd = 2, lty = c(1,1), pch = c(16, 30), col = c("blue", "red"))


sm <- summary(aju1)
d.pen <-  sm$coefficients[2]
d.ke = (g.Raga/pen1**2)*d.pen
d.ke2 <- g.Raga * summary(lm(m ~ 0 + x))$coefficients[2]
ke2 <- g.Raga * lm(m ~ 0 + x)$coefficients[1]
ke2; ke
d.ke
d.ke2

Sprint(paste("k estático es",round(ke,1), "mas menoSs", round(d.ke,1)))

#####################
# 2 pi / tau = sqrt(k / m)
mDyn <- 0.1368; d.mDyn <-0.0001

tiempos <- read.csv("periodos.txt", header = T, sep = ";")
tiempos <- tiempos[,-1]

######
timpIm <- tiempos[seq(1, length(tiempos), 2)]/10^6
diff(timpIm) -> delta1

timpPr <- tiempos[seq(2, length(tiempos), 2)] /10^6
delta2 <- diff(timpPr)

deltas <- c(delta1, delta2)
deltas <- deltas[deltas > 0.907 & deltas < 0.920]

sdT <- sd(deltas)
mn.dl <- mean(deltas)
SEMT <- sdT / sqrt(length(deltas))
#####

hist(deltas, main = TeX("Histograma de $\\tau$"),
     xlab = TeX("$\\tau$"), cex.lab = 1.5, cex.main = 2)
abline(v = mn.dl,lwd = 3, col = "blue") 
arrows(x0 = mn.dl - sdT, x1 = mn.dl + sdT , y = 50, y1 = 50,
       code = 3, length = 0.1, col = "blue", angle = 90, lwd = 2)
arrows(x0 = mn.dl - SEMT, x1 = mn.dl + SEMT , y = 100, y1 = 100,
       code = 3, length = 0.1, col = "red", angle = 90, lwd = 3)

kd = 4 * pi^2 * mDyn / mn.dl^2
d.kd = 8 * pi^2 * mDyn * SEMT / mn.dl^3 + 4 * pi^2 / mn.dl^2 * d.mDyn  

text(x = mn.dl - 0.0005,cex = 1.2, y = 200, label = TeX("$\\bar{\\tau}$"))
text(x = mn.dl - 0.0005, cex = 1.2, y = 110, label = TeX("$\\bar{\\tau}\\pm SEM$"))
text(x = mn.dl - 0.0005, cex = 1.2, y = 60, label = TeX("$\\bar{\\tau}\\pm sd$"))

print(paste("k estático es",round(ke,1), "mas menos", round(d.ke,1)))
print(paste("k dinámico es",round(kd,3), "mas menos", round(d.kd,3)))
