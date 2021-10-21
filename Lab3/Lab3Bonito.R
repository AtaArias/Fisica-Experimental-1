  setwd(paste(getwd(), "Lab3", "datos", sep = "/"))
  library(ggplot2)
  library(paletteer)
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
  
  colores <- paletteer::paletteer_d("awtools::ppalette")
  col_modelo <- colores[3]
  col_ajuste <- colores[1]
  
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
  # Ángulos con acelerómetro
  hor_yz <- hor; hor_yz$ax <- 0  ;inc_yz <- inc; inc_yz$ax <- 0
  angs <- angulos(hor_yz, inc_yz)
  stdDev <- sd(angs)
  d.angs <- stdDev / sqrt(length(angs))
  
  # Angulos con los catetos
  # el angulo trig no toma en cuenta la inclinación de la mesa
  op <- 4; hip <- 200; seno <- op / hip; d.l <- 0.1 # cm
  ang_trig <- asin(seno) * 180 / pi
  d.ang_trig <- ((d.l /( hip * sqrt(1 - seno^2)))*(1 + seno)) * 180 / pi
  
  
  #####
  # Recorte y grafíca de los datos
  comienzo <- 1.85;  final <- 5
  pRec <- pCom[pCom$t >comienzo & pCom$t < final,]
  pRec$t <- pRec$t - pRec$t[1]; pRec$x <- pRec$x - pRec$x[1] # t0 = 0, x0 = 0
  plot(x = pRec$t, y= pRec$x, main = "Posición vs tiempo",
       xlab = "Tiempo (s)", ylab = " Posición (m)",
       pch = 16, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  
  # Cálculo de los ángulos ajustados
  # Ángulo perfecto
  ang_per <- asin((2 * pRec[pRec$t != 0,]$x)/ (Raga * pRec[pRec$t != 0,]$t**2)) * 180 / pi
  
  # modelos:
  # modelo con el ángulo trigonométrico 
  for (i in c(-1,0,1)){
  lines(pRec$t, x <- 1/2*  (acel_plano(ang_trig+i*d.ang_trig)) * pRec$t^2, col = col_modelo, lwd = 2-abs(i))
  }
  
  #ajuste cuadrático
  pRec$t2 <- pRec$t^2
  aju_cu <- lm(formula = x ~ t + t2 - t - 1, data = pRec)
  sa <- summary(aju_cu)
  acel_aju <- 2 * sa$coefficients[1,1]
  d.acel_aju <- 2 * sa$coefficients[1,2]
  ang_aju_c <- asin(acel_aju/Raga) * 180 / pi 
    
  lines(pRec$t, x <- (acel_aju / 2) * pRec$t^2 , col = col_ajuste, lwd = 2)
  
  # leyenda
  legend(x = min(pRec$t), y = max(pRec$x), 
         legend = c("Mediciones",
                    paste("Modelo  ", "Ángulo: ", round(ang_trig, 4), "°", sep = ""),
                    paste("Ajuste  ", "Ángulo: ",round(ang_aju_c,4),"°", sep = "")),
         col = c("black", col_modelo, col_ajuste), lty =c(0,1,1), lwd = c(0,3,3), bty = "n", pch = c(16,26,26))
  
  
  
  
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
  # Ajuste lineal
  x <- pRec$t**2; y <- pRec$x
  aju <- lm(y ~ x)
  ord.origen <- aju$coefficients[1]; pendiente <- aju$coefficients[2]
  an_aju <- asin((2 * pendiente )/ Raga ) * 180 / pi
  
  plot(x = pRec$t**2,y = pRec$x, main = "Ajuste lineal", 
       xlab = "t^2(s^2)", ylab = "x(m)")
  abline(a= 0, b = 1/2 * acel_plano(ang_trig), col = col_modelo, lwd = 2)
  abline(aju, col= col_ajuste, lwd = 2)
  
  ############################################
  # acel vs sen(a)
  # leo el archivo de internet
  acels_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vShqcluBL3-Gxk9bj1ceO6cy-inHN5THAlMP75_fdK05gfAF3aDFDNLJXb2MqZA9bxPXj4Xir0Baqc2/pub?gid=0&single=true&output=csv"
  acels <- read.csv(acels_url, header = T, sep = ",")
  colnames(acels) <- c("acel", "ang", "d.ang", "d.acel", "nombre")
  # calculo el seno de los angulos
  acels$sin <- sin(acels$ang * pi / 180)
  # ploteo acel vs seno y el ajuste lineal
  plot(y = acels$acel, x = acels$sin)
  abline(a = 0, b = Raga)
  aju_todos <- lm(acels$acel ~ acels$sin)
  abline(aju_todos)
  arrows(x0 = acels$sin, x1 = acels$sin,
         y0 = acels$acel - acels$d.acel, y1 = acels$acel + acels$d.acel,
         length = 0, col ="blue")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #####
  ##########################################
  # Fricción constante
  f_ang <- angs
  
  # a la ida tiene la aceleración de la gravedad más la producida por la fricción
  plot(m_f$t, m_f$x, main = "Subida-Bajada", ylab = "x(m)", xlab = "t(s)")
  
  min <- min(m_f$x)
  t_min <- m_f[m_f$x == min,]$t; t_min <- mean(t_min)
  
  subida <- m_f[m_f$t < t_min,]
  plot(subida$t, subida$x, main = "friccion pos vs t subida")
  # limpieza subida
  subida <- subida[subida$t > 1.2 & subida$t < 4.5,]
  plot(subida$t, subida$v, main = "friccion vel vs t subida limpio")
  aju_sb <- lm(subida$v ~ subida$t)
  abline(aju_sb)
  acel_sb <- aju_sb$coefficients[2]
  text(x = 3, y = -0.5, label = paste("acel subida:", acel_sb))
  
  
  bajada <- m_f[m_f$t > t_min,]
  plot(bajada$t, bajada$x, main = "fricción pos vs t bajada")
  # limpieza bajada
  plot(bajada$t, bajada$v, main = "friccion vel vs t bajada limpio")
  aju_bj <- lm(bajada$v ~ bajada$t)
  abline(aju_bj)
  acel_bj <- aju_bj$coefficients[2]
  text(x = 8, y = 0.15, label = paste("acel bajada:", acel_bj))

  cteFr <- (acel_sb - acel_bj) / 2
  
  
  