  setwd(paste(getwd(), "Lab3", "datos", sep = "/"))
  #####
  # funciones
  gravedad <- function(set) {
    set$g <- sqrt(set$ax**2 + set$ay**2 + set$az**2)
  }
  
  limpieza <- function(set) {
    set <- set[,c(-1,-2,-6)]
    colnames(set) <- c("ax", "ay", "az")
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
  
  acel_plano <- function(angle) {
    sin(angle * pi / 180) * Raga
  }
  
  #####
  # constantes
  Raga <- 9.79316985
  n_hor <- "hor.tsv"
  n_inc <- "AAng.tsv"
  n_plano1 <- "MPlanoAta.04.csv"
  n_fric <- "FricConstante.tsv"
  n_pfric <- "MFric.csv"
  
  
  #####
  # lectura de datos
  hor <- read.csv(n_hor, sep = "\t", header = F)
  inc <- read.csv(n_inc, sep = "\t", header = F)
  inc_fric <- read.csv(n_fric, sep = "\t", header = F)
  p1 <- read.csv(n_plano1, sep = ";", header = T)
  m_f <- read.csv(n_pfric, sep = ";", header = T)
  
  
  # limpieza y modulo
  hor <- limpieza(hor) 
  inc <- limpieza(inc)
  inc_fric <- limpieza(inc_fric)
  
  colnames(m_f) <- c("t", "x", "v", "a") -> colnames(p1)
  
  #####
  angs <- angulos(hor, inc)
  # hist(angs)
  # plot(p1$t, p1$x, main = " datos completos, pos vs tiempo")
  # plot(p1$t, p1$v, main = " datos compleots, vel vs tiempo")
  # m <- p1[p1$t < 2.5 & p1$t > 1.5,]
  # plot(m$t, m$x)
  comienzo <- 1.75
  final <- 6
  
  p2 <- p1[p1$t >comienzo & p1$t < final,]
  t_0 <- p2$t[1]
  x_0 <- p2$x[1]
  p2$t <- p2$t - t_0
  p2$x <- p2$x - x_0
    
  plot(p2$t, p2$x, main = "pos vs tiempo limpios")
  lines(p2$t, x <- 1/2*  acel_plano(mean(angs)) * p2$t^2, col = "Blue")
  lines(p2$t, x <- 1/2* (acel_plano(mean(angs)) - Frsubida) * p2$t^2, col = "Blue")
  lines(p2$t, x <- 1/2* acel_bj * p2$t**2, col = "green")
  lines(p2$t, x <- 1/2* acel_plano(mean(ang_per) * 180 / pi) * p2$t^2, col = "Yellow")
  lines(p2$t, x <- 1/2* acel_plano(an_aju) * p2$t^2, col = "Red")
  
  plot(p2$t, p2$v, "vel vs tiempo limpios")
  
  # ajuste lineal
  
  plot(x = p2$t**2,y = p2$x, main = "Ajuste lineal")
  x <- p2$t**2
  y <- p2$x
  aju <- lm(y ~ x)
  ord.origen <- aju$coefficients[1]
  ord.origen
  pendiente <- aju$coefficients[2]
  abline(aju, col= "blue")
  
  abline(a= 0, b = 1/2 * ((((sin(mean(angs))* pi / 180))* Raga) - cteFr))
  ang_per <- asin((2 * p2[p2$t != 0,]$x)/ (Raga * p2[p2$t != 0,]$t**2))
  # hist(ang_per)
  # # y = posición x = t²
  # x = i/2 sen(alpha) g t²
  # alpha = asen(2 * pendiente / g)
  # pendiente = 1/2 sen(a) g
  # pendiente * 2 / g = sen(a)
  # arcoseno(pendiente * 2 / g) = a
  an_aju <- asin((2 * pendiente )/ Raga ) * 180 / pi
  
  ##########################################
  # Fricción constante
  # 
  f_ang <- angulos(hor, inc_fric)
  # hist(f_ang, breaks = nclass.FD(f_ang))
  
  # a la ida tiene la aceleración de la gravedad más la producida por la fricción
  plot(m_f$t, m_f$x, main = "fricción pos vs t")
  
  plot(m_f$t, m_f$v, main = "friccion vel vs t")
  
  plot(m_f[m_f$a < 1 &  m_f$a > -1,]$t, m_f[m_f$a < 1 & m_f$a > -1,]$a, main = "friccion a vs t recortada")
  
  min <- min(m_f$x)
  t_min <- m_f[m_f$x == min,]$t
  t_min <- mean(t_min)
  
  subida <- m_f[m_f$t < t_min,]
  plot(subida$t, subida$x, main = "friccion pos vs t subida")
  plot(subida$t, subida$v, main = "friccion vel vs t subida")
  
  # limpieza subida
  subida <- subida[subida$t > 1,]
  plot(subida$t, subida$v, main = "friccion vel vs t subida limpio")
  aju_sb <- lm(subida$v ~ subida$t)
  abline(aju_sb)
  acel_sb <- aju_sb$coefficients[2]
  text(x = 3, y = -0.5, label = paste("acel subida:", acel_sb))
  
  
  bajada <- m_f[m_f$t > t_min,]
  plot(bajada$t, bajada$x, main = "fricción pos vs t bajada")
  plot(bajada$t, bajada$v, main = "friccion vel vs t bajada")
  
  
  # limpieza bajada
  bajada <- bajada[bajada$t < 10 & bajada$t > 6,]
  plot(bajada$t, bajada$v, main = "friccion vel vs t bajada limpio")
  aju_bj <- lm(bajada$v ~ bajada$t)
  abline(aju_bj)
  acel_bj <- aju_bj$coefficients[2]
  text(x = 8, y = 0.15, label = paste("acel bajada:", acel_bj))
  
  # acel subida = acelG + fr
  # acel bajada = acelG - fr
  
  # acel subida - fr = acelG
  # acel bajada + fr = acelG
  
  # acel subida - fr = acel bajada + fr
  # acel subida - acel bajada = 2 fr
  # (acel subida - acel bajada) / 2 = fr
  
  cteFr <- (acel_sb - acel_bj) / 2
  print(paste("fricción:", cteFr))
  
  Frsubida <- acel_sb - acel_plano(mean(angs))
  Frsubida
  Frbajada <- acel_plano(mean(angs)) - acel_bj
  Frbajada
  cteFr
  (Frbajada + Frsubida) / 2
  
  ######################## Mensajes
  print(paste("Alpha seguń el ajuste lineal es: ", an_aju, "°", sep = ""))
  
  print(paste("El angulo calculado es: ", mean(angs)))
  
  print(paste("El angulo tomado con la función es:",mean(ang_per) * 180 / pi))
  
  print(paste("coordenada al origen:", ord.origen))