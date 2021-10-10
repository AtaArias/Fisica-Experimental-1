#librerías
suppressMessages(library("crayon")); suppressMessages(library("ggplot2"));
suppressMessages(library("gridExtra"));suppressMessages(library("latex2exp"))
library(xtable)
library("readr")
library(ggplot2)

#####
# carga de datos del drive
# id <- "1rQkkdXpTXr70-SCYl4nI1WRBcKssTW1_dcg6g5VQJRU" #id del archivo (estable, copipastear de la dirección)
# url del archivo publicado
bolas_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTsyvdo2lU0sTbAhqoRNA8fIJaazMu0gCksWKBsCgQ-1snwm3A9SnpsbF4qQT04hdZBHQieKVbwP73-/pub?gid=0&single=true&output=csv"
bolas <- read.csv(bolas_url, header = T, sep = ",")
bolas <- bolas[bolas$r_cm < 3.5,]
bolas <- bolas[bolas$Nombre == "",]
b_papel <- bolas[bolas$Papel == "papel",]
b_alum <- bolas[bolas$Papel == "aluminio",]

#####
# radio vs masa
plot(y = b_papel$m_mg, x = b_papel$r_cm,
     xlab = "Radio(cm)", ylab = "Masa(mg)",
     main = "Radio vs Masa de papel",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)

aju_p <- lm(b_papel$m_mg ~ b_papel$r_cm)
abline(aju_p, lwd = 2, col = "yellow")
aju_p$coefficients


# aluminio
plot(y = b_alum$m_mg, x = b_alum$r_cm,
     xlab = "Radio(cm)", ylab = "Masa(mg)",
     main = "Radio vs Masa de aluminio",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)

aju_al <- lm(b_alum$m_mg ~ b_alum$r_cm)
abline(aju_al, lwd = 2, col = "blue")

#####
# log(radio) vs log(masa)
plot(y = log(b_papel$m_mg), x = log(b_papel$r_cm),
     xlab = "log(radio(cm))", ylab = "log(masa(mg))",
     main = "log-log Radio vs Masa de papel",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)
aju_plog <- lm(log(b_papel$m_mg) ~ log(b_papel$r_cm))
abline(aju_plog, lwd = 2, col = "yellow")

corte <- aju_plog$coefficients[1]
pendiente <- aju_plog$coefficients[2]

densidad <- exp(corte) /  (4/3 * pi) # grafica log 
densidades <- b_papel$m_mg / b_papel$r_cm^3 * 4/3 * pi # masa / vol
mean(densidades)
densidad

# aluminio
plot(y = log(b_alum$m_mg), x = log(b_alum$r_cm),
     xlab = "log(radio(cm))", ylab = "log(masa(mg))",
     main = "log-log Radio vs Masa de aluminio",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)

aju_alog <- lm(log(b_alum$m_mg) ~ log(b_alum$r_cm))
abline(aju_alog, lwd = 2, col = "blue")

al_corte <- aju_alog$coefficients[1]
al_pend <- aju_alog$coefficients[2]

aju_alog
al_den <- exp(al_corte) /  (4/3 * pi)
al_dens <- b_alum$m_mg / b_alum$r_cm^3 * 4/3 * pi
median(al_dens)
al_den * 10

#####
# densidad vs radio
plot(x = densidades, y = b_papel$r_cm,
     xlab = "Densidad(mg/cm^3)", ylab = "Radio(cm)",
     main = "Densidad vs Radio de papel",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)

# aluminio
plot(x = al_dens, y = b_alum$r_cm,
     xlab = "Densidad(mg/cm^3)", ylab = "Radio(cm)",
     main = "Densidad vs Radio de aluminio",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)
#####
# densidad vs masa
plot(x = densidades, y = b_papel$m_mg,
     xlab = "Densidad(mg/cm^3)", ylab = "Masa(mg)",
     main = "Densidad vs Masa de papel",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)

#aluminio
plot(x = al_dens, y = b_alum$m_mg,
     xlab = "Densidad(mg/cm^3)", ylab = "Masa(mg)",
     main = "Densidad vs Masa de aluminio",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)
