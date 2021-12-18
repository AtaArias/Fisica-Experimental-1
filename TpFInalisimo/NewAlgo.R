# funciones 
secs <- function(name){
  ending <- unlist(strsplit(name, "_"))[3]
  times <- gsub(".jpg", "", ending)
  times <- as.integer(times)
  seconds <- times %% 100
  minutes <- (times %% 10000 - seconds) / 100
  hours <- (times - minutes * 100 - seconds) / 10000
  t <- seconds + minutes * 60 + hours * 3600
  
  t
}

#librerías
library(jpeg)

#Análisis de quilmes
root <- "/home/ata_arias/Desktop/Experimental1/TpFinal/Quilmes"
setwd(root)

temps <- list.files()

sens <- c(0.5, 0.55, 0.6, 0.6, 0.5, 0.6, 0.6, 0.53, 0.53)
t.list <- list()
total.list <- list()
for (i in 1:length(temps)){
  temp <- temps[i]
  
  setwd(paste(root, temp, sep = "/"))
  
  # Leo la imagen
  # La paso a blanco y negro
  # Saco las columnas que suman menos de 10
  h <- c()
  t <- c()
  
  for (file in list.files()) {
    img <-file
    img <- readJPEG(img)
    img <- as.matrix(img[,,1])
    
    # plot(1:2, type= "n")
    # rasterImage(img, 1, 1, 2, 2)
    
    img <- (img > sens[i])
    storage.mode(img) <- "numeric"
    
    # plot(1:2, type= "n")
    # rasterImage(img, 1, 1, 2, 2)
    
    img <- img[,colSums(img) > 10]
    if (temp == temps[9])
      img <- img[, (0.1 * ncol(img)):(0.3*ncol(img))]
    else
      img <- img[, (0.4 * ncol(img)):(0.75*ncol(img))]
    
    # plot(1:2, type= "n")
    # rasterImage(img, 1, 1, 2, 2)
    
    imgEdg <- diff(img)
    h_top <- c()
    h_bot <- c()
    
    grosor <- c()
    
    for (j in 1:ncol(imgEdg)){
      colum <- imgEdg[,j]
      
      debajo <- which(colum == -1)
      arriba <- which(colum == 1)
      
      arriba <- arriba[1:min(length(arriba), length(debajo))]
      debajo <- debajo[1:min(length(arriba), length(debajo))]
      
      height <- debajo - arriba
      
      index <- match(max(height), height)
      
      grosor <- c(grosor, max(height))
      
      h_top <- c(h_top, arriba[index])
      h_bot <- c(h_bot, debajo[index])
    }
    
    # plot(1:2, type= "n", main = file)
    # rasterImage(img, 1, 1, 2, 2)
    # 
    # abline(h = 2-(median(h_top) / nrow(img)), col = "red", lwd = 5, lty = 3)
    # abline(h = 2-(median(h_bot, na.rm = T) / nrow(img)), col = "blue", lwd = 5, lty = 3)

    h <- c(h, median(grosor, na.rm = T))
    
    t <- c(t, secs(file))
    
  }
  
  t <- t - t[1]
  t.list[[i]] <- t
  total.list[[i]] <- h
  
  plot(t, h/h[1] * 100, main = temp)
}


plot(t.list[[1]], total.list[[1]], pch = 8, col = colores[1], ylim = c(0, 1))
for (n in 2:length(temps)){
  # points(x = t.list[[n]], total.list[[n]], pch =  7 + n, col = colores[n])
  lines(x = t.list[[n]], total.list[[n]], pch =  7 + n, col = colores[n])
}

