library(latex2exp)

setwd(paste(getwd(), "Lab6", sep = "/"))

#1.a) Realice el montaje del péndulo de montaje, y haga las medidas correspondientes para calcular L_t +/- d.Lt
desviación <- read.csv("ResultsPendulo.csv", header = T, sep = ",")
head(desviación)
desviación <- desviación[,c(-1,-2)]

h1 <- 0.645; h3 <- 0.841; d.h <- 0.001

lt <- 2 * sqrt(h1 * h3); d.lT <- d.h*(sqrt(h1 / h3) + sqrt(h3 / h1))
media <- mean(lt - desviación)
stanDev <- sd(lt- desviación)
sem <- stanDev / sqrt(length(desviación))


hist(lt - desviación, breaks = nclass.FD(lt - desviación),
     main = "Distancia de caida", xlab = TeX("$L_E(s)$"), 
     cex.main = 2, cex.lab = 1.5)
abline(v = lt, col = "red", lwd = 3)
arrows(x0 = lt + d.lT, x1 = lt - d.lT,
       y0 = 1.2, y1 = 1.2, col = "red2", lwd = 3,
       angle = 90, length = 0.1, code = 3)
text(x = 1.474, y = 1.5, label = TeX("$L_T \\pm \\delta L_t $"), cex = 1.5, col = "red2")

abline(v = media, col = "blue", lwd = 3)
arrows(x0 = media + sem, x1 = media -sem,
       y0 = 1, y1 = 1, col = "blue2", lwd = 3,
       angle = 90, length = 0.1, code = 3)
text(x = 1.47, y = 1.2, label = TeX("$ \\bar{L_E} \\pm SEM $"), cex = 1.5, col = "blue2")

arrows(x0 = media - stanDev, x1 = media + stanDev,
       y0 = 3, y1 = 3, 
       angle = 90, length = 0.1, code = 3,
        col = "orange", lwd = 3)

text(x = 1.468, y = 3.3, label = TeX("$ \\bar{L_E} \\pm sd $"), cex = 1.5, col = "orange")

legend(x = 1.464, y = 6, bty = "n", 
       legend = c(TeX("$L_T$"), TeX("$ \\bar{L_E}$")),
       lty = c(1,1), col = c("red","blue"), lwd = 4, cex = 1.5)




#####################################
g.Raga <-  9.79316985

# Modelo del cañon
tiempos <- read.csv("tiempos.csv", header = F)
tiempos <- tiempos$V1

# Velocidad de salida en tiro vectical
h_0 <- 0.195
d.h_0 <-0.005

t <- mean(tiempos)
d.t <- sd(tiempos) / sqrt(length(tiempos))

# Tiro vertical de la bola
Vi <- (-h_0 / t) + (g.Raga * t / 2)

# D.Vi = d.h0 / t + |h0 / t^2 + g.Raga/2| d.t

d.Vi = d.h_0 / t + abs(h_0 / t^2 + g.Raga / 2) * d.t

d.hang <- 0.005
h_ang <- 0.215
alpha <- 40;
d.alpha <- (pi / 180 )/ 2
alpha <- alpha * pi / 180

Vy <- sin(alpha) * Vi
Vx <- cos(alpha) * Vi

#D.vy = cos(a)* Vy * d.a + sin(alpha) * d.Vi
d.Vy <- Vx * d.alpha + sin(alpha) * d.Vi
d.Vx <- Vy * d.alpha + cos(alpha) * d.Vi

t_alcance <- (-Vy - sqrt( Vy^2 + 2 *h_ang * g.Raga) ) / -g.Raga

#d.t_a = abs((-1 - Vy/sqrt(Vy^2 + 2 h g)) / -g)*D.Vy + abs( 1/sqrt(Vy^2 + 2 h g)) * D.h_ang
d.ta <- abs(1/g.Raga *( -1-(Vy/sqrt(Vy^2+2 * h_ang * g.Raga)))) * d.Vy + (1 /sqrt(Vy^2 + 2 * h_ang * g.Raga)) * d.hang

alcance <- Vx * t_alcance

#d.alcance = d.Vx * t_alcance + Vx  * d.t_alcance
d.alcance <- d.Vx * t_alcance + Vx * d.ta