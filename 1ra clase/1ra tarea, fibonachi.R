# Secuencia de fibonachi

#Borra consola, variables y funciones definidas en el entorno Global
rm(list = setdiff(ls(), lsf.str())); rm(list=lsf.str()); cat("\014")
#vector que contendra la sucesión
fib <- vector(mode = "integer")
#definimos los primeros dos términos
fib[1] <- 0 ; fib[2] <- 1

#bucle que calcula n términos de la sucesion a partir de los primeros dos números
#aumento n hasta 300
n <- 300; #ojo que quedan n+1 = 301 términos
for (i in 2:n) {fib[i+1] <- fib[i] + fib[i-1]}

# que número es el 100avo de termino de la sucesión de fibonachi
fib[100]

# Obtenga los primeros 10 terminos, obtengo los primeros 300. Pruebe con length(fib)
fib[1:20]
fib[1:300]

# Cual es el número 20
fib[20]

#Obtenga todos los números de la sucesión menores a 1000
fib[fib<1000]

# Que lugares ocupan en la sucesión de fibonachi, los números mayores que 100
# y menores que 2000
fib[fib > 100 & fib < 2000]

# Veo cuantos elementos tiene el vector de valores menores a 100
# cuento desde el siguiente, hasta la cantidad de elementos en el vector con
# números menores a 2000
length((fib[fib<100]) + 1) : length(fib[fib<2000])


#defina aproxphi
aproxphi = vector(mode = "numeric")

# Cargue en cada indice de aproxphi el coeficiente f_n+1 / f _ n
# calcule phi como (1+sqrt(5))/2
phi = (1 + sqrt(5))/2
for (i in 1:n) {aproxphi[i - 1]= fib[i + 1]/fib[i]}

#el primer elemento de la sucesión de fibonachi es 0 por lo que no se puede 
#dividir por este, comenzamos por el 2

#defina diferencia como la diferencia entre aprox phi y phi
diferencia <- abs(aproxphi - phi)

# Cual es el valor de n para que la diferencia de aprox phi y phi sea menor a 10^-8
# Tomamos el largo del vector con todos los elementos mayores a 10^-8, ese el index
# del ultimo elemento de la diferencia mayor a 10^-8. el siguiente index es el primer
# n cuya diferencia con phi es menor.
length(diferencia[diferencia > 10^(-8)])] + 1


## Ejercicios parte C
# Calcule los primeros 100 numeros de la serie de fibonachi
# Grafique la función
plot(1:101 , fib[1:101])

# Grafique el logritmo natural de los valores
plot(2:101 , log(fib[2:101]))
