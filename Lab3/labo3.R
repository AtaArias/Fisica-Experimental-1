Raga <- 9.79316985

# Instalar plot_ly
horizontal <- read.csv("Jhorz.tsv", header = F, sep = "\t")
horizontal <- horizontal[,c(-1,-2)]

plano <- read.csv("aang.02.tsv", header = F, sep = "\t")
plano <- plano[,c(-1,-2)]

horz <- c(mean(horizontal$V3), mean(horizontal$V4), mean(horizontal$V5))
horz_mod <- sqrt(sum(horz**2))

inclinado <- c(mean(plano$V3), mean(plano$V4), mean(plano$V5))
inclinado_mod <- sqrt(sum(inclinado**2))

ppunto <- horz[1] * inclinado[1] + horz[2] * inclinado[2] + horz[3] * inclinado[3]
angulo <- acos(ppunto / (horz_mod * inclinado_mod))
angulo <- angulo * 180 / pi
angulo

NameMedidas <- "MplanoAta.04.csv"

m <- read.csv(NameMedidas, header = F, sep = ";", skip = 1)
colnames(m) <- c("t", "x", "v", "a")
#View(Meds)

plot(m$t, m$x)
plot(m$t[m$t > 1.5 & m$t < 2.2], m$x[m$t > 1.5 & m$t < 2.2])
points(m$t[m$t > 0.5 & m$t < 1.5], m$x[m$t > 0.5 & m$t < 1.5])

t_0 = m[78,1]
x_0 = m[78,2]

m <- m[78: nrow(m),]
m$t <- m$t - t_0
m$x <- m$x - x_0

################################################
plot(m$t, m$x)
#angulo real = 0.63 
lines(m$t, x <- 1/2*Raga*sin((angulo - 0.8) * pi / 180) * m$t^2, col = "Red")
# points(m$t, x <- 1/2*Raga*sin((angulo - 0.8) * pi / 180) * m$t^2, col = "Red")

m_reduced <- m[m$t <3,]
plot(m_reduced$t, m_reduced$x)
lines(m$t, x <- 1/2*Raga*sin((angulo - 0.8) * pi / 180) * m$t^2, col = "Red")
################################################
# plot(m$t, m$v)
# plot(m$t, m$a)
# acel <- mean(m$a)
# abline(h = acel)
# acel
# aju <- lm(m$a ~ m$t)
# abline(aju, col = "red4")
# 
# plot(m$v, m$a)
