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
scale_y_continuous(trans = "log", breaks = round(seq(1,min((tidy$h_h0)), length.out = 4), 2), labels = round(seq(1,min((tidy$h_h0)), length.out = 4), 2)      )  + 

  
xlab(TeX("$ t (s) $")) + 
ylab(TeX(r"($ \\frac{h}{h_0}(adimensional) \[ log \] $)")) 

show(gtutti)

for (i in 1:max(tidy$n)){
  plot()
}

