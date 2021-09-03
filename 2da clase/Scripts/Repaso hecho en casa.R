rm(list = setdiff(ls(), lsf.str()));
rm(list=lsf.str());
cat("\014")
# Ya están instalados
# install.packages("xtable")
# install.packages("IRdisplay")
# install.packages("repr")
# install.packages("ggplot2")

#librerías
suppressMessages(library("crayon")); suppressMessages(library("ggplot2"));
suppressMessages(library("gridExtra"));suppressMessages(library("latex2exp"))
library(xtable)
library(IRdisplay)
library(repr)
library(latex2exp)
library(ggplot2)

#opciones de ploteo ggplot2
temajuan3 <- theme(
  legend.title = element_text(size = 20),
  panel.background = element_rect(fill = "gray93"),
  legend.box = "vertical",
  legend.direction = "vertical",
  legend.key.height = unit(0.1,"snpc"),
  legend.key.width  = unit(0.1,"snpc"),
  axis.line = element_line(size = 0.5, lineend = "square"),
  axis.ticks = element_line(size = 1, colour = "black"),
  panel.border = element_rect(size = 1, fill = NA ),
  axis.title = element_text(size = 28),
  axis.text.x  = element_text(size = 24),
  axis.text.y = element_text(size = 24),
  legend.background = element_blank(),
  legend.position = c(0.3,0.8),
  legend.key = element_blank(),
  legend.text = element_text(size = 28)
)
fig <- function(width, heigth){
  options(repr.plot.width = width, repr.plot.height = heigth)
}

archivo <- gsub(readLines(con = "00.tsv", n = -1), , pattern = "\"", replacement = "")
print(archivo[1:6])

## los mete en variables
datos <- read.csv(file = "00.tsv", sep = "\t", header = F)

# head to see the top, dim for dimensions
head(datos, 3); dim(datos)

# limpiamos los datos. c isn't for colum, is for concatenar
datos <- datos[ , c(-1,-6)]; head(datos, 3)

colnames(datos) <- c("t", "ax", "ay", "az")

datos$g = sqrt(datos$ax^2 + datos$ay^2 + datos$az^2)
head(datos, 3)

# Tx - T_0, easier to use
datos$t <- datos$t - datos$t[1]; head(datos, 2); range(datos$t)

# no se que hace esto che, lets decifer it
library(ggplot2); source(getwd())
ga <- ggplot(data = datos) +
  geom_point(aes(x = t, y = ax, colour = "ax")) +
  geom_point(aes(x = t, y = ay, colour = "ay")) +
  geom_point(aes(x = t, y = az, colour = "az")) +
  ylab(TeX("$a \\; (m\\, / \\,s^2)$")) + xlab("t (s)") +
  scale_colour_manual(values = c("ax" = "green4", "ay" = "red4", "az" = "Deepskyblue4" ),name = NULL) +
  temajuan3  + theme(legend.position = c(0.2,0.4), legend.direction = "horizontal")
fig(width = 24,heigth = 16); show(ga)

# No me funciona lo de juan, probemos con plot
#    Creo que lo resolví, habia que instalar la librería de latex2exp
plot(x = datos$t, y = datos$g)


