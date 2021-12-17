#Análisis de quilmes
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
# install.packages("jpeg")
library(jpeg)

files <- list.files()
files <- head(files, -1 )
tops <- c()
bots <- c()

for (i in 1:length(files)) {
  pic <- readJPEG(files[i])
  pic <- as.matrix(pic[,,1])
  
  # plot(1:2, type= "n", main = i)
  # rasterImage(pic, 1, 1, 2, 2)
  # 
  pic <- pic[0:(0.9*nrow(pic)),]
  pic <- pic[, (0.45 * ncol(pic)):(0.47 * ncol(pic))]
 
  
  # pic <- t(pic) #rotar si es necesario
  # pic <- pic[,(0.35 * ncol(pic)):(  0.40 * ncol(pic))]
  
  #original 0.7
  pic <- (pic > 0.6)
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
      bajo <- tail(indices,1)
      alto <- tail(indices,2)[1]
      while((bajo - alto) < (length(files)-i) * 10){
        indices <- indices[-length(indices)]
        alto <- tail(indices,2)[1]
      }
      h_top <- c(h_top, alto)
      h_bot <- c(h_bot, bajo)
    }
    mean(columna)
  }
  
  # Que uso mean o median??
  tops <- c(tops, median(h_top))
  bots <- c(bots, median(h_bot))
  
  plot(1:2, type= "n", main = i)
  rasterImage(pic, 1, 1, 2, 2)
  abline(h = 2-(median(h_top) / nrow(pic)), col = "red", lwd = 5, lty = 3)
  abline(h = 2-(median(h_bot) / nrow(pic)), col = "blue", lwd = 5, lty = 3)
  
  print(paste(round(i/length(files),3) * 100, "% completado"))
}

t <- c()
for (title in files){
  t <- c(t, secs(title))
}
t <- t - t[1]

plot(x = t, y = log((bots - tops)/(bots[1]-tops[1]) * 100))
