#Detectar los bordes en una imagen 
library(jpeg))

#leo la imagen
img <-list.files()[2]
img <- readJPEG(img)

img <- as.matrix(img[,,1])


plot(1:2, type= "n")
rasterImage(img, 1, 1, 2, 2)

hist(img)

img <- (img > 0.6)
storage.mode(img) <- "numeric"

plot(1:2, type= "n")
rasterImage(img, 1, 1, 2, 2)

dens <- c()
for (i in 1:ncol(img)){
  dens <- c(dens, mean(img[,i]))
}
dens
img <- img[,which(dens > max(dens)-0.02)]

plot(1:2, type= "n")
rasterImage(img, 1, 1, 2, 2)
