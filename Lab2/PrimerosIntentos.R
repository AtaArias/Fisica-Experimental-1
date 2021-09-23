# leemos los datos

# setwd(paste(getwd(), "Lab2", sep = "/"))

horizontal = read.csv("horizontal prueba.tsv", header = F, sep = "\t")
horizontal <- horizontal[, c(-1,-2,-6)]
horizontal <- horizontal[1:2000,]
colnames(horizontal) <- c("ax","ay","az")
head(horizontal)
plot(horizontal$az)
g <- sqrt((horizontal$ax * horizontal$ax) + (horizontal$ay * horizontal$ay) + (horizontal$az * horizontal$az))
Realg <- mean(g)
Realg


inclinado <- read.csv("prueba.tsv", header = F, sep = "\t")
inclinado <- inclinado[, c(-1,-2,-6)]
inclinado <- inclinado[1:2000,]
colnames(inclinado) <- c("ax", "ay", "az")
tail(inclinado)
g2 <- sqrt((inclinado$ax^2) + (inclinado$ay^2) + (inclinado$az^2))
Realg2 <- mean(g2)

angulo <- acos( (inclinado$ax * horizontal$ax + inclinado$ay * horizontal$ay + inclinado$az * horizontal$az) / (g * g2) )
angulo
anguloMedidas <- acos(38.3 / 53.5)
anguloSenMed <- asin(36.4 / 53.5)
anguloTan <- atan(36.4 / 38.3)
anguloMedidas
anguloSenMed
anguloTan
plot(angulo)
hist(angulo, xlim = c(0.770, 0.790))
abline(v = mean(angulo))
std <- sd(angulo)
arrows(x0 = mean(angulo) + std , x1 = mean(angulo) - std,
        y0 = 400 , y1 = 400,
        angle = 90, col = "yellow") 
SEM <- std / 2000
SEM <- as.numeric(SEM)
arrows(x0 = mean(angulo) + SEM, x1 = mean(angulo) - SEM, y0 = 200, y1 = 200, 
       angle = 90, col = "red", lwd = 3)

abline(v = anguloMedidas)
