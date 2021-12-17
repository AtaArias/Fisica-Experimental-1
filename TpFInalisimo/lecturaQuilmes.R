#Análisis de quilmes

#librerías
# install.packages("jpeg")
library(jpeg)

files <- list.files()

tops <- c()
bots <- c()

for (i in 1:length(files)) {
  pic <- readJPEG(files[i])
  pic <- as.matrix(pic[,,1])
  org <- pic
  
  pic <- pic[0:(0.9*nrow(pic)),]
  pic <- pic[, (0.42 * ncol(pic)):(0.47 * ncol(pic))]
  # pic <- t(pic)
  # pic <- pic[1: (nrow(pic)/2 + 220),]
  # pic <- pic[,(0.35 * ncol(pic)):(  0.40 * ncol(pic))]
  
  # 
  #original 0.7
  pic <- (pic < 0.5)
  storage.mode(pic) <- "numeric"
  
  
  h_top <- c()
  h_bot <- c()
  ## iterar por todas las columnas
  for (j in 1:ncol(pic)){
    columna <- pic[,j]
    columna <- columna == 1 # veo en que valores hay blanco
    list <- c(FALSE)
    for (k in 2:length(columna)-1){
      list <- c(list, (columna[k-1] != columna[k+1]) & columna[k]) #true donde es blanco y son diferentes a los costados(edge)
    }
    indices <- which(list) # indices de los bordes
    if (length(indices) > 1){
      h_top <- c(h_top, tail(indices,2)[1])
      h_bot <- c(h_bot, tail(indices,1))
    }
    mean(columna)
  }
  
  # Que uso mean o median??
  tops <- c(tops, median(h_top))
  bots <- c(bots, median(h_bot))
  
  pic[tops[i]:(tops[i]+10),] <- "blue"
  pic[bots[i]:(bots[i]+10),] <- "red"
  
  # org[tops[i]:(tops[i]+10),] <- "blue"
  # org[bots[i]:(bots[i]+10),] <- "red"
  # 
  
  # plot(1:2, type = "n")
  # rasterImage(org, 1, 1, 2, 2)
  # 
  plot(1:2, type= "n")
  rasterImage(pic, 1, 1, 2, 2)
  
}
files[1]
x <- strsplit(files[1], "_")
a <- unlist(x)
a[3]
plot(y = bots - tops, x  = 30 * 1:length(files))

