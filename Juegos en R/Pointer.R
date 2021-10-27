par(mar = c(0,0,0,0))
plot.new()
puntos <- data.frame(NULL)
alive <- T;
a <- locator(n = 1)
puntos <- rbind(puntos, a)
points(x = a$x, y = a$y, pch = 20)
while (alive){
  a <- locator(n = 1)
  for(i in 1:length(puntos[,1])){
    punto <- puntos[i,]
    dist <- sqrt((punto$y-a$y)^2 + (punto$x - a$x)^2)
    print(dist)
    if (dist < 1){
      alive <- F
      text(x = 0.5, y = 0.5, cex = 3, label = "You lost" , col = "red")
    }
  }
  puntos <- rbind(puntos, a)
  points(x = a$x, y = a$y, pch = 20)
}