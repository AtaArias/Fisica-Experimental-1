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

lateral <- c(0.45, 0.5, 0.47, 0.53)
frontal <- c(0.15, 0.9, 0.2, 0.85)

sens <- c(0.5, 0.6)

for (temp in temps)
{
  Tnum <- match(temp, temps)
  
  setwd(paste(root, temp, sep ="/"))
  files <- list.files()
  
  tops <- c()
  bots <- c()
  
  for (i in 1:length(files)) {
    pic <- readJPEG(files[i])
    pic <- as.matrix(pic[,,1])

    plot(1:2, type= "n", main = i)
    rasterImage(pic, 1, 1, 2, 2)

    # pic <- pic[,(lateral[2*Tnum - 1]*ncol(pic)):(lateral[2*Tnum]*ncol(pic))]
    # pic <- pic[(frontal[2*Tnum - 1]*nrow(pic)):(frontal[2*Tnum]*nrow(pic)),]
    # 
    # pic <- t(pic) #rotar si es necesario
    # pic <- pic[,(0.35 * ncol(pic)):(  0.40 * ncol(pic))]
    
    #original 0.7
    pic <- (pic > sens[Tnum])
    storage.mode(pic) <- "numeric"
    
    
    h_top <- c()
    h_bot <- c()
    ## iterar por todas las columnas
    # for (j in 1:ncol(pic)){
    #   columna <- pic[,j]
    #   columna <- columna == 1 # veo en que valores hay blanco
    #   list <- c(FALSE)
    #   for (k in 2:length(columna)-1){
    #     list <- c(list, (columna[k-1] != columna[k+1]) & columna[k]) #true donde es blanco y son diferentes a los costados(edge)
    #   }
    #   indices <- which(list) # indices de los bordes
    #   if (length(indices) > 1){
    #     bajo <- tail(indices,1)
    #     alto <- tail(indices,2)[1]
    #     while((bajo - alto) < (length(files)-i) * 10){
    #       indices <- indices[-length(indices)]
    #       alto <- tail(indices,2)[1]
    #     }
    #     h_top <- c(h_top, alto)
    #     h_bot <- c(h_bot, bajo)
    #   }
    # }
    # 
    # Mediana de los bordes
    tops <- c(tops, median(h_top))
    bots <- c(bots, median(h_bot))
    
    plot(1:2, type= "n", main = paste(i, temp))
    rasterImage(pic, 1, 1, 2, 2)
    abline(h = 2-(median(h_top) / nrow(pic)), col = "red", lwd = 5, lty = 3)
    abline(h = 2-(median(h_bot) / nrow(pic)), col = "blue", lwd = 5, lty = 3)
    
    dens <- c()
    for (j in 1:(nrow(pic)-500)){
      dens <- c(dens, (mean(pic[j:j+500,])))
    }
    plot(dens)
    dens <- dens[dens > mean(dens)]    
    
    
    print(paste(round(i/length(files),3) * 100, "% completado"))
  }
  
  t <- c()
  for (title in files){
    t <- c(t, secs(title))
  }
  t <- t - t[1]
  percent <- (bots - tops)/(bots[1]-tops[1])
  percent <- percent * 100
  
  t.list[[Tnum]] <- t
  per.list[[Tnum]] <- percent
  
  # plot(x = t, y =percent)
}

plot(t.list[[1]], per.list[[1]], pch = 20, type = "l")
for (n in 2:4){
  lines(x = t.list[[n]], per.list[[n]], col = c("red", "green", "blue")[n-1], pch = 20)
}

