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

# agrego la densidad
bolas$densidad <- bolas$m_mg / ((bolas$r_cm^3) * 4/3 * pi)

b_papel <- bolas[bolas$Papel == "papel",]
b_papel <- b_papel[b_papel$densidad < median(b_papel$densidad) + 100 & b_papel$densidad > median(b_papel$densidad) - 100,]

b_alum <- bolas[bolas$Papel == "aluminio",]

#####
# radio vs masa
plot(x = b_papel$m_mg, y = b_papel$r_cm,
     ylab = "Radio(cm)", xlab = "Masa(mg)",
     main = "Radio vs Masa de papel",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)

aju_p <- lm(b_papel$r_cm ~ b_papel$m_mg)
abline(aju_p, lwd = 2, col = "yellow")



# aluminio
plot(x = b_alum$m_mg, y = b_alum$r_cm,
     ylab = "Radio(cm)", xlab = "Masa(mg)",
     main = "Radio vs Masa de aluminio",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)

aju_al <- lm(b_alum$r_cm ~ b_alum$m_mg)
abline(aju_al, lwd = 2, col = "blue")

#####
# log(radio) vs log(masa)
plot(x = log(b_papel$m_mg), y = log(b_papel$r_cm),
     ylab = "log(radio(cm))", xlab = "log(masa(mg))",
     main = "log-log Radio vs Masa de papel",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)
aju_plog <- lm(log(b_papel$r_cm) ~ log(b_papel$m_mg) )
abline(aju_plog, lwd = 2, col = "yellow")

corte <- aju_plog$coefficients[1]
pendiente <- aju_plog$coefficients[2]
r.sq <- summary(aju_plog)$r.squared

text(x = 0.5, y = 7, label = paste("corte al origen: ", round(corte, 4)) )
text(x = 0.5, y = 6.8, label = paste("pendiente: ", round(pendiente, 4)))
text(x = 0.5, y = 6.6, label = paste("rsquared: ", round(r.sq, 4)))

# p_lden <- 1 / pendiente  # grafica log 

# aluminio
plot(x = log(b_alum$m_mg), y = log(b_alum$r_cm),
     ylab = "log(radio(cm))", xlab = "log(masa(mg))",
     main = "log-log Radio vs Masa de aluminio",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)

aju_alog <- lm(log(b_alum$r_cm) ~ log(b_alum$m_mg))
abline(aju_alog, lwd = 2, col = "blue")

al_corte <- aju_alog$coefficients[1]
al_pend <- aju_alog$coefficients[2]
al_rsq <- summary(aju_alog)$r.squared

text(x = 0.3, y = 7.25, 
     label = paste("corte: ", round(al_corte, 4), "\npendiente: ", round(al_pend, 4), "\nrsquared: ", round(al_rsq, 4)))

# al_lden <- exp(-3 * al_corte) *3 / (4 * pi)
# al_lden  
#####
# densidad vs radio
plot(y = b_papel$densidad, x = b_papel$r_cm,
     ylab = "Densidad(mg/cm^3)", xlab = "Radio(cm)",
     main = "Densidad vs Radio de papel",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)

# aluminio
plot(y = b_alum$densidad, x = b_alum$r_cm,
     ylab = "Densidad(mg/cm^3)", xlab = "Radio(cm)",
     main = "Densidad vs Radio de aluminio",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)
#####
# densidad vs masa
plot(y = b_papel$densidad, x = b_papel$m_mg,
     ylab = "Densidad(mg/cm^3)", xlab = "Masa(mg)",
     main = "Densidad vs Masa de papel",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)

#aluminio
plot(y = b_alum$densidad, x = b_alum$m_mg,
     ylab = "Densidad(mg/cm^3)", xlab = "Masa(mg)",
     main = "Densidad vs Masa de aluminio",
     cex.main = 2, cex.lab = 1.5, cex.axis = 1.5,
     pch = 20)

#####
# chequear todas las nombres y el sem de su densidad
nombres <- NULL
for (nombre in b_papel$Nombre){
  if (!(nombre %in% nombres))
    nombres <- c(nombres, nombre)
}
for (nombre in nombres){
  print(nombre)
  current <- b_papel[b_papel$Nombre == nombre,]
  hist(current$densidad, 
       main = paste("Histograma de densidades: ",nombre), 
       breaks = nclass.FD(current$densidad))
}


print(paste("La densidad según el ajuste log-log de papel es:", p_lden,
            "La densidad a calculada es: ", mean(b_papel$densidad),
            "La dimensión efectiva es: ", 1 / pendiente))
