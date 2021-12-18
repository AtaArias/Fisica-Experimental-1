## By density
library(paletteer) 
colores <- paletteer_d("jcolors::pal11")



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
install.li

#Análisis de quilmes
root <- "/home/ata_arias/Desktop/Experimental1/TpFinal/Quilmes"
setwd(root)

temps <- list.files()

t.list <- list() 
total.list <- list()

for (temp in temps)
{
  Tnum <- match(temp, temps)
  
  setwd(paste(root, temp, sep ="/"))
  
  files <- list.files()
  
  foams <-c()
  for (i in 1:length(files)) {
    pic <- readJPEG(files[i])
    pic <- as.matrix(pic[,,1])
    
    pic <- pic[,(0.3*ncol(pic)):(0.6*ncol(pic))] # cols
    pic <- pic[(0.1*nrow(pic)):(0.9*nrow(pic)),] # rows
    
    #original 0.7
    pic <- (pic > 0.55)
    storage.mode(pic) <- "numeric"
    
    # plot(1:2, type= "n", main = i)
    # rasterImage(pic, 1, 1, 2, 2)
    
    dens <- c()
    for (j in 1:(nrow(pic))){
      dens <- c(dens, mean(pic[j,]))
    }
    dens <- dens[dens > mean(dens)]    
    dens <- dens[abs(diff(dens)) < 0.005]
    dens <- dens[abs(diff(dens)) < 0.005]

    foam <- length( abs(diff(dens)) < 0.0025)
      
    foams <- c(foams, foam)

    abline(h = max(dens) - 0.1, col = "blue", lwd = 2, lty = 3)
    abline(h = max(dens), col = "blue", lwd = 4, lty = 3)
    
    plot(diff(dens))
    
    print(paste(round(i/length(files),3) * 100, "% completado"))
  }
  
  t <- c()
  for (title in files){
    t <- c(t, secs(title))
  }
  t <- t - t[1]
  
  total <- foams / foams[1]
  
  t.list[[Tnum]] <- t
  total.list[[Tnum]] <- total
  
  plot(x = t, y =total, pch = 20)
}

plot(t.list[[1]], total.list[[1]], pch = 8, col = colores[1], ylim = c(0, 2))
for (n in 2:length(temps)){
  # points(x = t.list[[n]], total.list[[n]], pch =  7 + n, col = colores[n])
  lines(x = t.list[[n]], total.list[[n]], pch =  7 + n, col = colores[n])
}
