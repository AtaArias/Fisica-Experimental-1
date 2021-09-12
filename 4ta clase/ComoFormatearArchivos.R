## Análisis de datos ##
# Una vez tenemos el archivo de texto limpiado con los ":" remplazaods por ","
# El archivo de txt se debería ver así
#
# Total time,0,06,01.34
# <Lap>, <Lap Time>, <Total Time>
# 1, 0,00,10.23, 0,00,10.23
# 2, 0,00,algo,  0,00,algo
# 3, 0,00,algo,  0,00,algo
# y así por varias lineas


#1ro Seleccionar la carpeta donde está el archivo con los periodos usando setwd()
setwd("ubicación de la carpeta donde está el .txt con los periodos") 

periodos <- read.csv("Nommbre del archivo.txt", header = F, sep = ",", skip = 2) # leemos los archivos 
periodos <- periodos$V4 # nos quedamos con los segundos

# 3ro insertar los valores de :largos del péndulo, el error en la medición del largo en
# y el nombre del que tomó los datos. (todas las mediciones en metros)
l.min <- DISTANCIA AL TOPE; l.cM <- DISTANCIA AL CENTRO DE MASA; l.max <- DISTANCIA A LA BASE
d.l <- ERROR EN LA MEDICIÓN DEL LARGO; nombre <- "Atahualpa"

p.limpio <- periodos[periodos < median(periodos) + 0.4 & periodos > median(periodos) - 0.4] # Se limpian los valores usando la mediana

tau <- mean(p.limpio) # definimos tau como el valor medio de las mediciones
SEM <- sd(p.limpio) / sqrt(length(p.limpio)) # SEM es la desviación estandar sobre la raiz de la cantidad de mediciones

tabla <- data.frame(l.min, l.cm, l.max, d.l, tau, SEM, nombre) # se crea una tabla para guardar los valores
write.table(tabla, paste("per_", nombre,".csv", sep = ""), row.names = F, col.names = F, sep = ",") # se crea el csv para subir al drive

# 4to: compartir con el drive de lab 1, si ya hay un archivo compartido reempmlazarlo por este
# el archivo resultante debería estar en la carpeta con el txt original y el nombre 
# tendría que ser per_NOMBRE.csv
