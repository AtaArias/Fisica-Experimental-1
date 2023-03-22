#Detectar los bordes en una imagen 
library(jpeg)

#leo la imagen
for (file in list.files()) {
  img <-file
  img <- readJPEG(img)
  img <- as.matrix(img[,,1])
  
  plot(1:2, type= "n")
  rasterImage(img, 1, 1, 2, 2)
  hist(img)
  abline(v = mean(img) + sd(img), lwd = 2)
  img <- (img > 0.55)
  storage.mode(img) <- "numeric"
  
  plot(1:2, type= "n")
  rasterImage(img, 1, 1, 2, 2)
  
  dens <- c()
  for (i in 1:ncol(img)){
    dens <- c(dens, mean(img[,i]))
  }
  plot(dens)
  img <- img[,(mean(which(dens > mean(dens)))-200):(mean(which(dens > mean(dens)))+200)]
  
  plot(1:2, type= "n")
  rasterImage(img, 1, 1, 2, 2)
}

dens2 <- c()
for (i in 1:nrow(img)){
  dens2 <- c(dens2, mean(img[i,]))
}
plot(dens2)
per = max(which(dens2 == max(dens2)))
abline(v = per + 100)
