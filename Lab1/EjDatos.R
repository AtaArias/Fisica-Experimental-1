pers <- read.csv( file = "per_Iara.txt", header = F, sep = ",")
pers$V4 -> p
plot(p)
#limpiamos
p.limpio <- p.limpio[p.limpio < 2]
plot(p.limpio)
mean(p.limpio)
# > plot(p)
# > p.limpio <- p[p < 3]
# > plot(p.limpio)
# > p.limpio <- p.limpio[p.limpio < 2.4 & p.limpio > 1.6]
# > mean(p.limpio)
# [1] 2.058028
# > plot(p.limpio)
# > iara *200 / 60
# Error: object 'iara' not found
# > l <- seq(0.3, 2.25, 0.15)
# > tao <- 2*pi*sqrt(l/9.8)
# > tiempos <- tao * 200 / 60
# > tiempo_total = sum(tiempos)
# > tiempo_total
# [1] 102.2703
# > nuestro_tiempo = sum(tiempos[seq(2, length(l), 2)])
# > nuestro_tiempo
# [1] 52.88269
# > iara = 2*pi*sqrt(2.55/9.8)
# > iara *200 / 60
# [1] 10.68355
# > 
#   > tao[6]
# [1] 2.056655
# > pers$V4 -> p
# > 