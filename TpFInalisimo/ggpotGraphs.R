#Learning ggplot2
# install.packages("ggplot2")
# install.packages("latex2exp")
library(latex2exp)
library(ggplot2)

tidy$Temperatura <- as.numeric(tidy$T)

gtutti <- ggplot(data = tidy) +
geom_point(mapping = aes(x = t, y = h_h0, colour = Temperatura)) + 
#geom_smooth(aes(x = t, y = h_h0, colour = Temperatura), method = "lm", formula = "y ~ x", show.legend = F) + 
scale_colour_gradient(low = "blue3", high = "red") + 
#scale_colour_gradientn(low = "DeepskyBlue4", high = "Red3", ) + 
# scale_y_continuous(trans = "log", breaks = round(seq(1,min((tidy$h_h0)), length.out = 4), 2), labels = round(seq(1,min((tidy$h_h0)), length.out = 4), 2)      )  + 

  
xlab(TeX("$ t (s) $")) + 
ylab(TeX(r"($ \\frac{h}{h_0}$)")) 

show(gtutti)

setwd("~/Desktop/Experimental1/TpFinal/ggplot graphs")
for (i in 1:max(tidy$n)){
  datos <- tidy[tidy$n == i,]
  
  aju <- lm(log(h_h0) ~ t -1, data = datos)
  erre <- - aju$coefficients[1]
  d.erre <- summary(aju)$coefficients[2]
  
  ecuación = paste(" y = -", round(erre, 4)," x", sep = "")
  ecuación2 = TeX(paste("$$ y = e^{", round(-1 * erre, 4),"x} $$"))
  
  name <- paste("AjulinealLog", datos$Temperatura[1], ".png", sep ="_")
  name2 <- paste("AjulinealExp", datos$Temperatura[1], ".png", sep ="_")
  
  
  plot <- ggplot(data = datos) +
    geom_point(mapping = aes(x = t, y = h_h0)) +
    geom_smooth(aes(x = t, y = h_h0, colour = Temperatura), method = "lm", formula = "y ~ x - 1", show.legend = F) +
    scale_y_continuous(trans = "log", breaks = round(seq(1,min((tidy$h_h0)), length.out = 4), 2), labels = round(seq(1,min((tidy$h_h0)), length.out = 4), 2))  + 
    
    geom_text(mapping = aes(x = 1/2 * max(t), y = max(h_h0), colour = Temperatura), label = ecuación) +
    geom_text(mapping = aes(x = 1/2 * max(t), y = max(h_h0) -0.1, colour = Temperatura), label = paste("r =", round(erre,4), "±", round(d.erre, 6), "[/s]")) +
    
    xlab(TeX("$ t (s) $")) + 
    ylab(TeX(r"($ \\frac{h}{h_0} \[ log \] $)")) 

  show(plot)
  
  plot <- ggplot(data = datos) +
    geom_point(mapping = aes(x = t, y = h_h0, colour = Temperatura)) +
    geom_function(fun = function(x) exp(-erre * x)) +
    geom_ribbon(mapping = aes(x = t, y =  exp(-erre * t), ymin = exp(-erre * t)*(1- t * d.erre - erre), ymax = exp(-erre * t)*(1 + t * d.erre + erre), col = Temperatura), alpha = 0.5) +
    geom_text(mapping = aes(x = 1/2 * max(t), y = max(h_h0)), label = ecuación2) +
    xlab(TeX("$ t (s) $")) +
    ylab(TeX(r"($ \\frac{h}{h_0}$)"))

  show(plot)
}
