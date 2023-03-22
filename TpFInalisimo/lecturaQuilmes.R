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

t.list <- list() 

per.list <- list()

for (temp in temps)
{
  setwd(paste(root, temp, sep ="/"))
  files <- list.files()
  
  tops <- c()
  bots <- c()
  
  for (i in 1:length(files)) {
    pic <- readJPEG(files[i])
    pic <- as.matrix(pic[,,1])
    
    # plot(1:2, type= "n", main = i)
    # rasterImage(pic, 1, 1, 2, 2)
    # 
    if (temp == temps[1] || temp == temps[3])
      pic <- pic[0:(0.9*nrow(pic)),]
    else
      pic <- pic[0:(0.85*nrow(pic)),]
    pic <- pic[, (0.45 * ncol(pic)):(0.47 * ncol(pic))]
   
    
    # pic <- t(pic) #rotar si es necesario
    # pic <- pic[,(0.35 * ncol(pic)):(  0.40 * ncol(pic))]
    
    #original 0.7
    pic <- (pic > 0.5)
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
    
    # plot(1:2, type= "n", main = paste(i, temp))
    # rasterImage(pic, 1, 1, 2, 2)
    # abline(h = 2-(median(h_top) / nrow(pic)), col = "red", lwd = 5, lty = 3)
    # abline(h = 2-(median(h_bot) / nrow(pic)), col = "blue", lwd = 5, lty = 3)
    # 
    # print(paste(round(i/length(files),3) * 100, "% completado"))
  }
  
  t <- c()
  for (title in files){
    t <- c(t, secs(title))
  }
  t <- t - t[1]
  percent <- (bots - tops)/(bots[1]-tops[1])
  percent <- percent * 100
  
  t.list[[match(temp, temps)]] <- t
  per.list[[match(temp, temps)]] <- percent
  
  # plot(x = t, y =percent)
}

plot(t.list[[1]], per.list[[1]], pch = 20, type = "l")
for (n in 2:4){
  lines(x = t.list[[n]], per.list[[n]], col = c("red", "green", "blue")[n-1], pch = 20)
}
