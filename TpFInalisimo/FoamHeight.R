library(png)

files <- list.files()
img <- readJPEG(files[9])


img <- readPNG(files[9])
img.filtered <- as.matrix(img[,,1])

plot(1:2, type = "n")
rasterImage(img.filtered, 1, 1, 2, 2)
# 
# hist(img.filtered)  
# NCOL(img.filtered)

img.sat <- (img.filtered < 0.95 & img.filtered > 0.7)
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
  pic <- (pic > 0.65)
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
  h_top <- h_top[h_top < median(h_bot)]
  tops <- c(tops, median(h_top))
  bots <- c(bots, median(h_bot))
  
  pic[tops[i]:(tops[i]+10),] <- 0.75
  pic[bots[i]:(bots[i]+10),] <- 0.5
  
  org[tops[i]:(tops[i]+10),] <- 0.75
  org[bots[i]:(bots[i]+10),] <- 0.5

  plot(1:2, type = "n")
  rasterImage(org, 1, 1, 2, 2)
  
  plot(1:2, type= "n")
  rasterImage(pic, 1, 1, 2, 2)
  
}
# cual es el error de cada punto?
plot(y = (bots - tops)/(bots[1] - tops[1]) * 100, x = 1:length(files) * 30,
     xlab = "t(s)", ylab = "% de espuma restante")

#exponential fit
t <- 30 * 1:length(files)
plot(t, log((bots - tops)/(bots[1] - tops[1])))
aju <- lm(log((bots - tops)/(bots[1] - tops[1])) ~ t - 1)
abline(aju)
abline(a = 0, b = coefficients(aju) + summary(aju)$coefficients[2], lty = 3, col = "blue")
abline(a = 0, b = coefficients(aju) - summary(aju)$coefficients[2], lty = 3, col = "blue")


# h = h_0 e^(-rt)
# h / h_0 = e^(-rt)
# ln(h) - ln(h_0) = -rt
# ln(h) = ln(h_0) - rt -> corte = ln(h_o) y -pendiente = r
restante = exp(coefficients(aju)[1])
r = - coefficients(aju)
r
