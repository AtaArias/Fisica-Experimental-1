##### Primer experimento
g.Raga <- 9.79316985
setwd(paste(getwd(), "Lab6", sep = "/"))
# 
# medicione <- read.csv("medicionesLab6.1.csv", sep = ",", header = T)

h1 <- 0.645; h3 <-0.841; d.h1 <- 0.001; d.h3 <- 0.001
# 
lT <- 2 * sqrt(h1 * h3); d.lT <- sqrt(h1 / h3) * d.h3 + sqrt(h3 / h1) * d.h1 
lT; d.lT



##### Segundo experimento
tiempos <- read.csv("tiempos.csv", header = F)
tiempos <- tiempos$V1
# Velocidad de salida en tiro vectical
h_0 <- 0.195
d.h_0 <-0.005

t <- mean(tiempos)
d.t <- sd(tiempos) / sqrt(length(tiempos))

Vi <- (-h_0 / t) + (g.Raga * t / 2)
Vi

#### alcance
#vel bauti
# Vi <- 5.925
d.hang <- 0.005
h_ang <- 0.215
alpha <- 40;
alpha <- alpha * pi / 180
t_alcance <- (-sin(alpha) * Vi - sqrt( (sin(alpha) * Vi)**2 + 2 *h_ang * g.Raga) ) / -g.Raga
d_alcance <- cos(alpha) * Vi * t_alcance
d_alcance
