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
library(paletteer)

colores <- paletteer_d("jcolors::pal12")


#Análisis de quilmes
root <- "/home/ata_arias/Desktop/Experimental1/TpFinal/Quilmes"
setwd(root)

temps <- list.files()

sens <- c(0.5, 0.55, 0.6, 0.6, 0.5, 0.58, 0.6, 0.53, 0.5)
t.list <- list()
bot.list <- list()
top.list <- list()
total.list <- list()

for (i in 1:length(temps)){
  temp <- temps[i]
  
  setwd(paste(root, temp, sep = "/"))
  
  # Leo la imagen
  # La paso a blanco y negro
  # Saco las columnas que suman menos de 10
  h <- c()
  t <- c()
  tops <- c()
  bots <- c()
  for (k in 1:length(list.files())) {
    file <- list.files()[k] 

    img <-file
    img <- readJPEG(img)
    img <- as.matrix(img[,,1])
    
    if ((k == 9 || k == 10) && i  == 9)
      img <- (img > 0.7)
    else
      img <- (img > sens[i])
    storage.mode(img) <- "numeric"
    
    img <- img[,colSums(img) > 50]
    
    if (i == 9)
      img <- img[, (0.4 * ncol(img)):(0.6*ncol(img))]
    else if (i == 3)
      img <- img[, (0.1 * ncol(img)):(0.3 *ncol(img))]
    else
      img <- img[, (0.4 * ncol(img)):(0.75*ncol(img))]
    
    imgEdg <- diff(img)
    h_top <- c()
    h_bot <- c()
    
    grosor <- c()
    
    for (j in 1:ncol(imgEdg)){
      colum <- imgEdg[,j]
      
      debajo <- which(colum == -1)
      arriba <- which(colum == 1)
      
      arriba <- arriba[1:min(length(arriba), length(debajo))]
      debajo <- debajo[1:min(length(arriba), length(debajo))]
      
      height <- debajo - arriba
      
      index <- match(max(height), height)
      
      grosor <- c(grosor, max(height))
      
      h_top <- c(h_top, arriba[index])
      h_bot <- c(h_bot, debajo[index])
    }
    
    # plot(1:2, type= "n", main = paste(temp, "°C", sep =""))
    # rasterImage(img, 1, 1, 2, 2)
    # 
    # abline(h = 2-(median(h_top) / nrow(img)), col = "red", lwd = 5, lty = 3)
    # abline(h = 2-(median(h_bot, na.rm = T) / nrow(img)), col = "blue", lwd = 5, lty = 3)

    h <- c(h, median(grosor, na.rm = T))
    tops <- c(tops, median(h_top, na.rm = T))
    bots <- c(bots, median(h_bot, na.rm = T))
    
    
    t <- c(t, secs(file))
    
  }
  
  t <- t - t[1]
  t.list[[i]] <- t
  total.list[[i]] <- h/h[1]
  bot.list[[i]] <- bots
  top.list[[i]] <- tops
  
  # plot(t, tops, main = paste(temp, "top"))
  # plot(t, bots, main = paste(temp, "bot"))
  
  # plot(t, h/h[1] * 100, main = temp)
}

h.list <- list()

for (i in 1:length(t.list)){
  top.list[[i]] <- abs(top.list[[i]] - bot.list[[i]][1])
  bot.list[[i]] <- abs(bot.list[[i]] - bot.list[[i]][1])
  
  h.list[[i]] <- top.list[[i]] - bot.list[[i]]  
}


plot(t.list[[1]], total.list[[1]])

largo <- length(total.list[[1]])
total <- total.list[[1]]
times <- t.list[[1]]
lim <- 1
for (i in 1:lim){
  y <- total[((i-1)/lim * largo):(i / lim * largo)]
  x <- times[((i-1)/lim * largo):(i / lim * largo)]
  aju <- lm(log(y) ~ x - 1)
  d.r <- summary(aju)$coefficients[2]
  r <- -aju$coefficients[1]
  # print(aju$coefficients[1])
  # # plot(x, log(y))
  # abline(aju)
  # plot(x, y)
  
  d.y = exp(-r * x)*(x * d.r + r)
  lines(x = x, y = exp(-r * x), col = "blue", lwd = 2)
  lines(x = x, y = exp(-r * x) + d.y, col = "blue", lty = 3)
  lines(x = x, y = exp(-r * x) - d.y, col = "blue", lty = 3)
}

erres <- c()
rsq <- c()
for (i in 1:length(total.list)){
  aju <- lm(log(total.list[[i]]) ~ t.list[[i]] - 1)
  erres <- c(erres, - aju$coefficients[1])
  summ <- summary(aju)
  rsq <- c(rsq, summ$r.squared)
}

plot(rsq, temps)
plot(temps[rsq > 0.985], erres[rsq > 0.985])