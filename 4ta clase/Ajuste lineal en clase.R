library(ggplot2)

g.RAGA  <- 9.79316985

setwd(paste(getwd(), "4ta clase", sep = "/"))
d <- read.csv(file = "tutti.tsv", header = T, sep = '\t')
plot(d$l_cm, d$tau_media, xlab = "L centro de masa", ylab = "Periodo", pch = 20)
plot(x = sqrt(d$l_cm), y = d$tau_media, xlab = "Raiz de L", ylab = "Periodo en (s)", pch = 20, xlim = c(0,1.5), ylim = c(0, 3),
     main = " Ajuste lineal")
abline(a = 0, b = (2 * pi /sqrt(g.RAGA)), col = "red")
lm()