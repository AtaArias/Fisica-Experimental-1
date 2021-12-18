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

setwd(paste(root, temps[10], sep = "/"))

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
  
  img <- (img > 0.55)
  storage.mode(img) <- "numeric"
  
  # plot(1:2, type= "n")
  # rasterImage(img, 1, 1, 2, 2)
  
  img <- img[,colSums(img) > 10]
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
  # 
  # plot(1:2, type= "n")
  # rasterImage(img, 1, 1, 2, 2)
  # 
  # 
  # abline(h = 2-(median(h_top) / nrow(img)), col = "red", lwd = 5, lty = 3)
  # abline(h = 2-(median(h_bot) / nrow(img)), col = "blue", lwd = 5, lty = 3)
  
  h <- c(h, median(grosor))
  
  t <- c(t, secs(file))
}

t <- t - t[1]

plot(t, h)
