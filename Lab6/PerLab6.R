##### Primer experimento

medicione <- read.csv("medicionesLab6.1.csv", sep = ",", header = T)
print("Inserte  h1 h3 d.h1 d.h3: ")
leido <- scan()
while(length(leido) != 4){
  print(" Número incorrecto de inputs, intente de nuevo: ");
  leido <- scan();
}
h1 <- leido[1]; h3 <-leido[2] ; d.h1 <- leido[3]; d.h3 <- leido[4]

lT <- 2 * sqrt(h1 * h3); d.lT <- sqrt(h1 / h3) * d.h3 + sqrt(h3 / h1) * d.h1 
lE <- mean(mediciones); dlE <- sd(mediciones) / sqrt(length(mediciones))

hist(mediciones)
abline(v = lE, col = "red", lwd = 3)
abline(v = lE + c(d.lE, -d.lE), col = "red", lwd = 2)

abline(v = lT, col = "blue", lwd = 3)
abline(v = lT + c(d.lT, -d.lT), col = "blue", lwd = 2)

##### Segundo experimento
# Velocidad de salida en tiro vectical



