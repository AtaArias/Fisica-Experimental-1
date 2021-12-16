library(png)

files <- list.files()

img <- readPNG(files[9])
img.filtered <- as.matrix(img[,,1])

# plot(1:2, type = "n")
# rasterImage(img.filtered, 1.2, 1.27, 1.8, 1.73)
# 
# hist(img.filtered)  
# NCOL(img.filtered)

img.sat <- (img.filtered > 0.7)
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
for (i in 1:8) {
  pic <- readPNG(files[i])
  pic <- as.matrix(pic[,,1])
  org <- pic
  
  pic <- (pic > 0.7)
  storage.mode(pic) <- "numeric"
  
  
  h_top <- c()
  h_bot <- c()
  for (i in 1:NCOL(pic)){
    columna <- pic[,i]
    columna <- columna == 1
    list <- c(FALSE)
    for (i in 2:length(columna)-1){
        list <- c(list, (columna[i-1] != columna[i+1]) & columna[i])
    }
    indices <- which(list)
    if (length(indices) > 1){
      h_top <- c(h_top, tail(indices,2)[1])
      h_bot <- c(h_bot, tail(indices,1))
    }
  }
  tops <- c(tops, mean(h_top))
  bots <- c(bots, median(h_bot))
  
  # pic[median(h_top):(median(h_top)+10),] <- 0.5
  # pic[median(h_bot):(median(h_bot)+10),] <- 0.5
 
  org[median(h_top):(median(h_top)+10),] <- 0.5
  org[median(h_bot):(median(h_bot)+10),] <- 0.5

  plot(1:2, type = "n")
  rasterImage(org, 1.2, 1.27, 1.8, 1.73)
  
  # plot(1:2, type= "n")
  # rasterImage(pic, 1.2, 1.27, 1.8, 1.73)
  
}
bots
plot(tops)
plot(log10(seq(1, 240, 30)),bots-tops)
plot(tops)
