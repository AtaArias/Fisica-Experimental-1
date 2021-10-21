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
    
    angulo <- acos(coseno)
  }
  
  acel_plano <- function(angle) {
    sin(angle) * Raga
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
  col_error <- colores[5]
  col_fric <- colores[8]
  
  
  d.t <- mean(pCom[2:nrow(pCom),]$t - pCom[1:nrow(pCom)-1,]$t)
  
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
  ang_cel <- mean(angs)
  acel_cel <- acel_plano(ang_cel)

  
  # Angulos con los catetos
  # el angulo trig no toma en cuenta la inclinación de la mesa
  op <- 4; hip <- 200; seno <- op / hip; d.l <- 0.1 # cm
  ang_trig <- asin(seno)
  d.ang_trig <- ((d.l /( hip * sqrt(1 - seno^2)))*(1 + seno))
  acel_trig <- acel_plano(ang_trig)

  #####
  # Recorte y grafíca de los datos
  comienzo <- 1.85;  final <- 5
  pRec <- pCom[pCom$t >comienzo & pCom$t < final,]
  pRec$t <- pRec$t - pRec$t[1]; pRec$x <- pRec$x - pRec$x[1] # t0 = 0, x0 = 0
  plot(x = pRec$t, y= pRec$x, main = "x vs t",
       xlab = "t(s)", ylab = " x (m)",
       pch = 16, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  
  # Cálculo de los ángulos ajustados
  # Ángulo perfecto
  # ang_per <- asin((2 * pRec[pRec$t != 0,]$x)/ (Raga * pRec[pRec$t != 0,]$t**2)) * 180 / pi
  
  # modelos:
  # modelo con el ángulo trigonométrico 
  # Dx = 1/2 g cos a t^2 d.a + g sen a t d.t
  for (i in c(-1,0,1)){
    lines(pRec$t, x <- 1/2*  (acel_cel - cteFr) * pRec$t^2 + i * (1/2 * Raga * cos(ang_cel) * pRec$t^2 *d.angs + Raga * sin(ang_cel)* pRec$t * d.t), col = col_modelo, lwd = 2-abs(i))
    lines(pRec$t, x <- 1/2*  (acel_trig - cteFr)  * pRec$t^2 + i * (1/2 * Raga * cos(ang_trig) * pRec$t^2 *d.ang_trig + Raga * sin(ang_trig)* pRec$t * d.t), col = col_error, lwd = 2-abs(i))
  }
  
  #ajuste cuadrático
  pRec$t2 <- pRec$t^2
  aju_cu <- lm(formula = x ~ t + t2, data = pRec)
  sa <- summary(aju_cu)
  x0_aju <- sa$coefficients[1,1]; d.x0_aju <- sa$coefficients[1,2]
  v0_aju <- sa$coefficients[2,1]; d.v0_aju <- sa$coefficients[2,2]
  acel_aju <- 2 * sa$coefficients[3,1];d.acel_aju <- 2 * sa$coefficients[3,2]
  ang_aju_c <- asin(acel_aju/Raga) * 180 / pi 
  
  # D aju
  # [a t+v0]d.t+t^2 / 2 d.a + t d.v + dx
  for (i in -1:1){
    lines(pRec$t, x <- (acel_aju / 2) * pRec$t^2 +v0_aju * pRec$t + x0_aju + i * ( (acel_aju * pRec$t + v0_aju)*d.t + pRec$t^2 / 2 * d.acel_aju + pRec$t * d.v0_aju + d.x0_aju), col = col_ajuste, lwd = 2)
  }
  
  # leyenda
  legend(x = min(pRec$t), y = max(pRec$x), 
         legend = c("Mediciones", "Modelo acelerometro", "Modelo trigonometría", "Ajuste cuadrático"),
         col = c("black", col_modelo, col_error, col_ajuste), lty =c(0,1,1,1), lwd = c(0,3,3,3), bty = "n", pch = c(16,26,26,26))
  
  
  
  
  # vel vs tiempo
  plot(pRec$t, pRec$v, main = "Velocidad vs tiempo",
       xlab = "Tiempo(s)", ylab = "Velocidad(m/s)",
       pch = 16, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  for (i in -1:1){
    abline(a = 0, b = (acel_cel + i * d.acel_cel), col = col_modelo, lwd = 2-abs(i))
  }
  
  abline(a = 0, b = acel_aju, col = col_ajuste, lwd = 2)
  
  
  # leyenda
  legend(x = min(pRec$t), y = max(pRec$v), 
         legend = c("Mediciones",
                    paste("Modelo, pendiente:",round(acel_cel,3)),
                    paste("Ajuste, pendiente:",round((acel_aju),3))),
         col = c("black", col_modelo, col_ajuste), lty =c(0,1,1), lwd = c(0,3,3), bty = "n", pch = c(16,26,26))
  
  
  # acel vs tiempo
  plot(pRec$t, pRec$a, main = "Aceleración vs tiempo",
       xlab = "Tiempo(s)", ylab = "Aceleración(m/s2)",
       pch = 16, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  for (i in -1:1){
    abline(h = acel_cel + i * d.acel_cel, col = col_modelo, lwd = 2-abs(i))
  }
  abline(h = acel_aju, col = col_ajuste, lwd = 2)
  
  # leyenda
  legend(x = 0, y = 0.05,
         legend = c("Mediciones", 
                    paste("Acel modelo:", round(acel_cel,3), "m/s^2"),
                    paste("Acel ajuste:", round(acel_aju,3), "m/s^2")),
         col = c("black", col_modelo, col_ajuste), lty =c(0,1,1), lwd = c(0,3,3), bty = "n", pch = c(16,26,26))
  
  #####
  # Ajuste lineal
  x <- pRec$t**2; y <- pRec$x
  aju_li <- lm(y ~ x)
  ord.origen <- aju_li$coefficients[1]; pendiente <- aju_li$coefficients[2]
  ang_aju_li <- asin((2 * pendiente )/ Raga ) * 180 / pi
  
  plot(x = pRec$t**2,y = pRec$x, main = "Ajuste lineal", 
       xlab = "t^2(s^2)", ylab = "Posición(m)",
       pch = 16, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  for(i in -1:1){
    abline(a= 0, b = 1/2 * (acel_cel + i*d.acel_cel), col = col_modelo, lwd = 2-abs(i))
  }
  
  abline(aju_li, col= col_ajuste, lwd = 2)
  
  # leyenda
  legend(x = min(pRec$t), y = max(pRec$x), 
         legend = c("Mediciones",
                    paste("Modelo, pendiente: ", round(acel_cel / 2, 3)),
                    paste("Ajuste lineal, pendiente: ",round(pendiente, 3))),
         col = c("black", col_modelo, col_ajuste), lty =c(0,1,1), lwd = c(0,3,3), bty = "n", pch = c(16,26,26))
  
  
  ############################################
  # acel vs sen(a)
  # leo el archivo de internet
  acels_url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vShqcluBL3-Gxk9bj1ceO6cy-inHN5THAlMP75_fdK05gfAF3aDFDNLJXb2MqZA9bxPXj4Xir0Baqc2/pub?gid=0&single=true&output=csv"
  acels <- read.csv(acels_url, header = T, sep = ",")
  colnames(acels) <- c("acel", "ang", "d.ang", "d.acel", "nombre")
  acels$sin <- sin(acels$ang * pi / 180)
  
  # ploteo acel vs seno y el ajuste lineal
  plot(y = acels$acel, x = acels$sin, main = "Datos de todos",
       xlab = "Seno(ángulo)", ylab = "Aceleración(m/s^2)",
       pch = 16, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  abline(a = 0, b = Raga, col = col_modelo, lwd = 2)
  aju_todos <- lm(acels$acel ~ acels$sin)
  abline(aju_todos, lwd = 2, col = col_ajuste)
  
  # error
  arrows(x0 = acels$sin, x1 = acels$sin,
         y0 = acels$acel - acels$d.acel, y1 = acels$acel + acels$d.acel,
         length = 0, col =col_error, lwd = 2)
  
  # leyenda
  legend(x = min(acels$sin), y = max(acels$acel),
         legend = c("Mediciones", "y = Raga * x",
                    paste("Ajuste lineal, pendiente: ", round(aju_todos$coefficients[2],3))),
         col = c("black", col_modelo, col_ajuste), lty =c(0,1,1), lwd = c(0,3,3), bty = "n", pch = c(16,26,26))
  
  ##########################################
  # Fricción constante
  # subida a + fr
  plot(m_f$t, m_f$x, main = "Subida-Bajada", ylab = "x(m)", xlab = "t(s)",
       pch = 20, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  
  min <- min(m_f$x)
  t_min <- m_f[m_f$x == min,]$t; t_min <- mean(t_min)
  
  subida <- m_f[m_f$t < t_min,]
  plot(subida$t, subida$x, main = "Posición vs tiempo, Subida",
       xlab = "Posición(m)", ylab = "Tiempo(s)",
       pch = 16, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  # limpieza subida
  subida <- subida[subida$t > 1.2 & subida$t < 4.5,]
  plot(subida$t, subida$v, main = "Velocidad vs tiempo, Subida",
       ylab = "Velocidad(m/s)", xlab = "Tiempo(s)",
       pch = 16, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  aju_sb <- lm(subida$v ~ subida$t)
  abline(aju_sb, lwd = 2, col = col_ajuste)
  acel_sb <- aju_sb$coefficients[2]
  legend(x = min(subida$t), y = max(subida$v),
         legend = c("Mediciones", paste("Ajuste lineal, pendiente: ", round(acel_sb,3))),
         col = c("black", col_ajuste), lty =c(0,1), lwd = c(0,3), bty = "n", pch = c(16,26))
  sm_sb <- summary(aju_sb)
  err_sb <- sm_sb$coefficients[2,2]
  
  
  
  bajada <- m_f[m_f$t > t_min,]
  plot(bajada$t, bajada$x, main = "Posición vs tiempo, Bajada",
       xlab = "Posición(m)", ylab = "Tiempo(s)",
       pch = 16, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  # limpieza bajada
  bajada <- bajada[bajada$t > 4.8 & bajada$t < 9,]
  plot(bajada$t, bajada$v, main = "Velocidad vs tiempo, Bajada",
       ylab = "Velocidad(m/s)", xlab = "Tiempo(s)",
       pch = 16, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  aju_bj <- lm(bajada$v ~ bajada$t)
  abline(aju_bj, lwd = 2, col = col_ajuste)
  acel_bj <- aju_bj$coefficients[2]
  legend(x = min(bajada$t), y = max(bajada$v),
         legend = c("Mediciones", paste("Ajuste lineal, pendiente: ", round(acel_bj,3))),
         col = c("black", col_ajuste), lty =c(0,1), lwd = c(0,3), bty = "n", pch = c(16,26))
  sm_bj <- summary(aju_bj)
  err_bj <- sm_bj$coefficients[2,2]
  
  cteFr <- (acel_sb - acel_bj) / 2
  d.cteFr <- (err_bj + err_sb) / 2
  text(x = 7, y = 0.1, label = paste("Aceleración producida por la fricción:\n", round(cteFr,3),"m/s^2"))
  
  #################
  # Datos de la bajada todos + fricción
  plot(x = pRec$t, y= pRec$x, main = "Posición vs tiempo",
       xlab = "Tiempo (s)", ylab = " Posición (m)",
       pch = 16, cex.main = 2, cex.axis = 1.5, cex.lab = 1.5)
  for (i in c(-1,0,1)){
    lines(pRec$t, x <- 1/2* (acel_cel + i* d.acel_cel) * pRec$t^2, col = col_modelo, lwd = 2-abs(i))
  }
  
  for (i in c(-1,0,1)){
    lines(pRec$t, x <- 1/2* (acel_cel + i* d.acel_cel - cteFr) * pRec$t^2, col = col_fric, lwd = 2-abs(i))
  }
  
  # leyenda
  legend(x = min(pRec$t), y = max(pRec$x), 
         legend = c("Mediciones", "Modelo sin fricción", "Modelo con fricción"),
         col = c("black", col_modelo, col_fric), lty =c(0,1,1), lwd = c(0,3,3), bty = "n", pch = c(16,26,26))
  
  