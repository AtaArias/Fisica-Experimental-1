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
   
    set 
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
  n_hor <- "acelPiso01.tsv"
  n_inc <- "planoInclinado01.tsv"
  n_plano1 <- "MedidasAta01.csv"
  n_fric <- "acelPiso01.tsv"
  n_pfric <- "SubiBaja01.csv"
  
  
  #####
  # lectura de datos
  hor <- read.csv(n_hor, sep = "\t", header = F)
  inc <- read.csv(n_inc, sep = "\t", header = F)
  inc_fric <- read.csv(n_fric, sep = "\t", header = F)
  pCom <- read.csv(n_plano1, sep = ";", header = T)
  m_f <- read.csv(n_pfric, sep = ";", header = T)
  
  
  # limpieza y modulo
  hor <- limpieza(hor) 
  inc <- limpieza(inc)
  inc_fric <- limpieza(inc_fric)
  
  colnames(m_f) <- c("t", "x", "v", "a") -> colnames(pCom)
  
  #####
  # Medidas en el plano yz
  hor_yz <- hor;  inc_yz <- inc; inc_yz$ax <- 0
  
  # angulos
  angs <- angulos(hor_yz, inc_yz)
  # el angulo trig no toma en cuenta la inclinación de la mesa
  op <- 4; hip <- 200; seno <- op / hip; d.l <- 0.1 # cm
  ang_trig <- asin(seno) * 180 / pi
  d.ang_trig <- (d.l /( hip * sqrt(1 - seno^2)))*(1 + seno)
  d.ang_trig <- d.ang_trig * 180 / pi
  stdDev <- sd(angs)
  d.angs <- stdDev / length(angs)
  d.angs
  
  #####
  plot(pCom$t, pCom$x, main = " datos completos, pos vs tiempo")
  comienzo <- 1.85;  final <- 5
  
  pRec <- pCom[pCom$t >comienzo & pCom$t < final,]
  # t0 = 0, x0 = 0
  pRec$t <- pRec$t - pRec$t[1]; pRec$x <- pRec$x - pRec$x[1]
  plot(pRec$t, pRec$x, main = "pos vs tiempo limpios")
  
  # angulo perfecto
  ang_per <- asin((2 * pRec[pRec$t != 0,]$x)/ (Raga * pRec[pRec$t != 0,]$t**2)) * 180 / pi
    
  # angulo con el ajuste lineal
  x <- pRec$t**2; y <- pRec$x
  aju <- lm(y ~ x)
  ord.origen <- aju$coefficients[1]; pendiente <- aju$coefficients[2]
  an_aju <- asin((2 * pendiente )/ Raga ) * 180 / pi
  
  # modelos:
  
  for (i in c(-1,0,1)){
  lines(pRec$t, x <- 1/2*  (acel_plano(ang_trig+i*d.ang_trig)) * pRec$t^2, col = "red")
  }
  
  lines(pRec$t, x <- 1/2* acel_plano(mean(ang_per)) * pRec$t^2, col = "Yellow")
  lines(pRec$t, x <- 1/2* acel_plano(an_aju) * pRec$t^2, col = "Red")
  
  # vel vs tiempo
  plot(pRec$t, pRec$v, main = "vel vs tiempo limpios")
  for (i in -1:1){
    abline(a = 0, b = acel_plano(ang_trig + i*d.ang_trig), col = "blue")
  }
  abline(a = 0, b = acel_plano((mean(ang_per))), col = "yellow")
  abline(a = ord.origen, b = acel_plano(an_aju), col = "red")
  
  # acel vs tiempo
  plot(pRec$t, pRec$a, main = "acel vs tiempo")
  for (i in -1:1){
    abline(h = acel_plano(ang_trig + i *d.ang_trig), col = "blue")
  }
  abline(h = acel_plano(mean(ang_per)), col = "yellow")
  abline(h = acel_plano(an_aju), col = "red")
  abline(h = mean(pRec$a))
  
  #####
  # ajuste lineal
  
  plot(x = pRec$t**2,y = pRec$x, main = "Ajuste lineal", 
       xlab = "t^2(s^2)", ylab = "x(m)")
  abline(a= 0, b = 1/2 * acel_plano(mean(angs)), col = "blue")
  abline(a = 0, b = 1/2 * acel_plano(mean(ang_per)), col = "yellow")
  abline(aju, col= "red")
  
  #####
  ##########################################
  # Fricción constante
  f_ang <- angs
  
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
  # bajada <- bajada[bajada$t < 10 & bajada$t > 6,]
  plot(bajada$t, bajada$v, main = "friccion vel vs t bajada limpio")
  aju_bj <- lm(bajada$v ~ bajada$t)
  abline(aju_bj)
  acel_bj <- aju_bj$coefficients[2]
  text(x = 8, y = 0.15, label = paste("acel bajada:", acel_bj))

  cteFr <- (acel_sb - acel_bj) / 2
  