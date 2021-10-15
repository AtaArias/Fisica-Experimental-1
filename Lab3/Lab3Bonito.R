setwd(paste(getwd(), "Lab3", "datos", sep = "/"))
#####
# funciones
gravedad <- function(set) {
  set$g <- sqrt(set$ax**2 + set$ay**2 + set$az**2)
}

limpieza <- function(set) {
  set$g <- gravedad(set)
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
n_hor <- "hor.tsv"
n_inc <- "AAng.tsv"
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

# se agrega el módulo y se limpia
hor <- limpieza(hor) 
inc <- limpieza(inc)
#####
angs <- angulos(hor, inc)
# hist(angs)
# plot(p1$t, p1$x)
# m <- p1[p1$t < 2.5 & p1$t > 1.5,]
# plot(m$t, m$x)
comienzo <- 2

p2 <- p1[p1$t >comienzo,]
t_0 <- p2$t[1]
x_0 <- p2$x[1]
p2$t <- p2$t - t_0
p2$x <- p2$x - x_0

plot(p2$t, p2$x)
lines(p2$t, x <- 1/2*Raga*sin((mean(angs)) * pi / 180) * p2$t^2, col = "Blue")
lines(p2$t, x <- 1/2*Raga*sin((mean(ang_per))) * p2$t^2, col = "Yellow")
lines(p2$t, x <- 1/2*Raga*sin((an_aju) * pi / 180) * p2$t^2, col = "Red")



# ajuste linear

plot(x = p2$t**2,y = p2$x, main = "Ajuste linear")
x <- p2$t**2
y <- p2$x
aju <- lm(y ~ x)
ord.origen <- aju$coefficients[1]
ord.origen
pendiente <- aju$coefficients[2]
abline(aju, col= "blue")
# 
ang_per <- asin((2 * p2[p2$t != 0,]$x)/ (Raga * p2[p2$t != 0,]$t**2))
# hist(ang_per)
# # y = posición x = t²
# x = i/2 sen(alpha) g t²
# alpha = asen(2 * pendiente / g)
# pendiente = 1/2 sen(a) g
# pendiente * 2 / g = sen(a)
# arcoseno(pendiente * 2 / g) = a
an_aju <- asin((2 * pendiente )/ Raga ) * 180 / pi
print(paste("Alpha seguń el ajuste lineal es: ", an_aju, "°", sep = ""))

print(paste("El angulo calculado es: ", mean(angs)))

print(paste("El angulo tomado con la función es:",mean(ang_per) * 180 / pi))
