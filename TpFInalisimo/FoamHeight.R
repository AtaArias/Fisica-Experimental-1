library(png)

files <- list.files()

img <- readPNG(files[9])
img.filtered <- as.matrix(img[,,1])

plot(1:2, type = "n")
rasterImage(img.filtered, 1.2, 1.27, 1.8, 1.73)
# 
# hist(img.filtered)  
# NCOL(img.filtered)

img.sat <- (img.filtered > 0.85)
storage.mode(img.sat) <-"numeric"

# hist(img.sat)

plot(1:2, type = "n")
# img.sat[1050:1060,] <- 1
rasterImage(img.sat, 1.2, 1.27, 1.8, 1.73)

#ver cual es la media de la altura del pixel blanco mas alto y cual es el mas bajo
h_top <- c()
h_bot <- c()
for (i in 1:NCOL(img.sat)){
  columna <- img.sat[,i]
  indices <- which(columna == 1)
  if (length(indices) > 2){
    h_top <- c(h_top, min(indices))
    h_bot <- c(h_top, max(indices))
  }
}


tops <- c()
bots <- c()
for (i in 1:length(files)) {
  pic <- readPNG(files[i])
  pic <- as.matrix(pic[,,1])
  org <- pic
  
  #original 0.7
  pic <- (pic > 0.7)
  storage.mode(pic) <- "numeric"
  
  
  h_top <- c()
  h_bot <- c()
  for (j in 1:NCOL(pic)){
    columna <- pic[,j]
    columna <- columna == 1
    list <- c(FALSE)
    for (k in 2:length(columna)-1){
        list <- c(list, (columna[k-1] != columna[k+1]) & columna[k])
    }
    indices <- which(list)
    if (length(indices) > 1){
      h_top <- c(h_top, tail(indices,2)[1])
      h_bot <- c(h_bot, tail(indices,1))
    }
  }
  
  # Que uso mean o median??
  tops <- c(tops, median(tail(h_top,100)))
  bots <- c(bots, median(h_bot))
  
  pic[tops[i]:(tops[i]+10),] <- 0.5
  pic[bots[i]:(bots[i]+10),] <- 0.5
  
  org[tops[i]:(tops[i]+10),] <- 0.5
  org[bots[i]:(bots[i]+10),] <- 0.5

  plot(1:2, type = "n")
  rasterImage(org, 1.2, 1.27, 1.8, 1.73)
  
  plot(1:2, type= "n")
  rasterImage(pic, 1.2, 1.27, 1.8, 1.73)
  
}
# cual es el error de cada punto?
plot(y = (bots - tops)/(bots[1] - tops[1]) * 100, x = 1:length(files) * 30)

#exponential fit
t <- 30 * 1:length(files)
plot(t, log(bots - tops))
aju <- lm(log(bots - tops) ~ t)
abline(aju)
abline(coefficients(aju) + summary(aju)$coefficients[,2], lty = 3, col = "blue")
abline(coefficients(aju) - summary(aju)$coefficients[,2], lty = 3, col = "blue")

