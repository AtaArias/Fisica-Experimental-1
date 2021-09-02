# Negación de la negación, carga de datos más complicados,
# Itento no alterar los datos si no es necesario


# merged ice core. 
#ahora vemos el merged-ice-core-yearly.csv/ parámetro n = -1 : lee todas las líneas del archivo
archivo <- gsub(readLines(con =  "merged-ice-core-yearly.csv", n = -1), , pattern = "\"",replacement = "")

# cargamos los datos y saltaos 26 renglones
diox <- read.csv(file = "merged-ice-core-yearly.csv", header = T, sep = ";", skip = 26)

# verificaos que es´te todo como queremos
head(diox, 3); tail(diox,3);nrow(diox)


at <- read.csv("TA.csv", sep = ",", header = T); head(at,5) #cada une mira cómo hacerlo

# esto no nos permite hacerlo porque tienen diferente tamaño
plot(at$TA, diox$CO2)

# juntaos los datos donde coinciden los años
Y.comunes <- intersect(at$Year, diox$Y ); Y.comunes

# hacemos un bucle para crear nuevos vectores

co2.comun <- vector(mode = "numeric", length = length(Y.comunes)) #valores co2 para años comunes
at.comun <- vector(mode = "numeric", length = length(Y.comunes)) #valores anomalía térmica 
for(i in 1:length(Y.comunes)){
  co2.comun[i] <- diox$CO2[diox$Y == Y.comunes[i]]
  at.comun[i] <- at$TA[at$Year == Y.comunes[i]]
}
co2.comun; at.comun; length(co2.comun) == length(at.comun)

correlatio <- data.frame(Y = Y.comunes, CO2 = co2.comun, AT = at.comun);
rm(Y.comunes, co2.comun, at.comun) #borra variables de la memoria

head(correlatio)

