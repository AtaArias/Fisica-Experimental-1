#num es para números con coma
# si vamos a usar int's hay que especificar el tipo de valor

vector1 =c(1,2,3)

class(vector1)

# cambiamos de float a int
vector1 <- as.integer(vector1)

# output de la consola
# > vector1 =c(1,2,3)
# > vec1= 1
# > class(vector1)
# [1] "numeric"
# > vector1 <- as.integer(vector1)
# > class(vector1)
# [1] "integer"

logic <- c(TRUE, TRUE, FALSE, TRUE)

class(logic)

# cambiamos el tipo de valor de logic a int
logic = as.integer(logic)

# nos devuelve el largo
largo = length(vetor1)

# uso de legth para calcular el promedio
promedio = sum(vector1) / largo
promedio

# concatenación de strings
MiNombre = "Ata"
Completo = c(MiNombre, "hualpa")
Completo

# concatenación of chars
chars = c("c","h","a","r")
chars
character = c(chars,"a","c","t","e","r")
character

# index
vector1[2]
character[c(2,4,6)]

#uso de la coma
coma = c(chars[2], character[c(3,1)]);
coma

# media
mean(vector1)

# "idexar" por operacioes lógicas
# el ; sirve para separar, pero no es obligatorio
vector1 == 3; vector1 != 3
vector1 <= 2

# idexar solo donde es cumple
vector1[vector1 > 1]

vector1[c(FALSE, TRUE, FALSE)]

# or, and
vector1[(vector1 == 2) | (vector1 == 3)] # or operator
vector1[(vector1 < 3) & (vector1 > 1)] # and operator

# así funciona el lenguaje no tiene nada que ver con álgebra
print(vector1)
print(vector1 + 1)
print(vector1 + c(2,1,4))

# seq, : es seq con paso 1

vector2 = seq(1,5,2) # comienzo, final, paso
vector2

# los multiplica elemento a elemeto
print(vector1 *vector2)

# producto punto
sum(vector1 * vector2)



#Matirces
#Byrow dice como mete los valores, por fila o por columna
M <- matrix(c(1:9), nrow = 3, ncol = 3, byrow = F)
ncol(M); nrow(M);
M


#Data frames, como lib pandas en python.
#Se crean diferetes tipos de datos
