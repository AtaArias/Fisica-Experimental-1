# setwd(paste(getwd(), "Lab3", "datos", sep = "/"))
#####
# funciones
gravedad <- function(set) {
  set$g <- sqrt(set$ax**2 + set$ay**2 + set$az**2)
}

limpieza <- function(set) {
  set[set$g < median(set$g) + 0.1 & set$g > median(set$g) - 0.1,]
}

angulos <- function(set1, set2){
  rows <- min(c(nrow(set1),  nrow(set2)))
  set1 <- set1[1:rows,]; set2 <- set2[1:rows,]
  
  ppunto <- set1$ax * set2$ax + set1$ay * set2$ay + set1$az* set2$az
  coseno <- ppunto / (set1$g * set2$g)
  
  angulo <- acos(coseno) * 180 / pi
}

#####
# constantes
Raga <- 9.79316985
n_hor <- "pisoLab3.tsv"
n_inc <- "ang1lab3.tsv"
n_plano1 <- "MPlanoAta.04.csv"
#####
# lectura de datos
hor <- read.csv(n_hor, sep = "\t", header = F)
inc <- read.csv(n_inc, sep = "\t", header = F)
p1 <- read.csv(n_plano1, sep = ";", header = T)

# limpieza
hor <- hor[,c(-1,-2,-6)]; inc <- inc[,c(-1,-2,-6)]
colnames(hor) <- colnames(inc) <- c("ax", "ay", "az")
colnames(p1) <- c("t", "x", "v", "a")

# se agrega el módulo
hor$g <- gravedad(hor)
inc$g <- gravedad(inc)

# limpieza 
hor <- limpieza(hor) 
inc <- limpieza(inc)
#####

angs <- angulos(hor, inc)
hist(angs)
# x(t) = 1/2 sen(alpha) * t²

plot(p1$t, p1$x)

m <- p1[p1$t < 2.5 & p1$t > 1.5,]
plot(m$t, m$x)

p2 <- p1[p1$t >1.8,]
plot(p2$t, p2$x)

hist(asin((2 * p1$x )/ (Raga * p1$t**2)))
print(asin((2 * p1$x )/ (Raga * p1$t**2)))
head(p1, 11)
