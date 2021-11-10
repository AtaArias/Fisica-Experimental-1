library(latex2exp)

setwd(paste(getwd(), "Lab6", sep = "/"))

#1.a) Realice el montaje del péndulo de montaje, y haga las medidas correspondientes para calcular L_t +/- d.Lt
desviación <- read.csv("ResultsPendulo.csv", header = T, sep = ",")
head(desviación)
desviación <- desviación[,c(-1,-2)]

h1 <- 0.645; h3 <- 0.841; d.h <- 0.001

lt <- 2 * sqrt(h1 * h3); d.lT <- d.h*(sqrt(h1 / h3) + sqrt(h3 / h1))

hist(lt - desviación, breaks = nclass.FD(lt - desviación),
     main = "Distancia de caida", xlab = TeX("$L_E(s)$"))
abline(v = lt, col = "red", lwd = 3)
abline(v = lt + c(d.h, -d.h), col = "red2", lwd = 2)

media <- mean(lt - desviación)
stanDev <- sd(lt- desviación)
sem <- stanDev / sqrt(length(desviación))

abline(v = media, col = "blue", lwd = 3)
abline(v = media + c(sem, -sem), col = "blue2", lwd = 2)

arrows(x0 = media - stanDev, x1 = media + stanDev,
       y0 = 3, y1 = 3, 
       angle = 90, length = 0.1, code = 3,
        col = "orange", lwd = 3)

text(x = 1.468, y = 3.3, label = TeX("$ \\bar{L_E} \\pm sd $"), cex = 1.5)

legend(x = 1.464, y = 6, bty = "n", 
       legend = c(TeX("$L_T$"), TeX("$ \\bar{L_E}$")),
       lty = c(1,1), col = c("red","blue"), lwd = 4, cex = 1.5)

#1.c) Calcule el alcance experimental y su incerteza 

