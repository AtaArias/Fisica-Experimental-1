l <- seq(0.3, 2.25, 0.15)
tao <- 2*pi*sqrt(l/9.8)
tiempos <- tao * 200 / 60
tiempo_total = sum(tiempos)
tiempo_total
nuestro_tiempo = sum(tiempos[seq(2, length(l), 2)])
nuestro_tiempo

iara = 2*pi*sqrt(1.05/9.8)
iara
