library(tidyverse)
library(ggthemes)

x <- c(173, 169, 176, 166, 161, 164, 160, 158, 180, 187)
y <- c(80, 68, 72, 75, 70, 65, 62, 60, 85, 92) 
dat <- as.data.frame(cbind(x,y),stringsAsFactors = F)


ggplot(dat, aes(x,y)) +
  labs(
    x = "Cost", 
    y = "Quality",
    title = "Cost versus quality") +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 14)) + 
  geom_segment(aes(x=150, xend = 200 , y=55, yend = 55), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_segment(aes(x=150, xend = 150 , y=55, yend = 100), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_point(size=4)

cor(x,y)
cor(x*100,y)
cor(y,x)


library(modelr)
mod <- lm(y ~ x)
mod

grid <-  dat %>%
  add_predictions(mod,"pred") %>%
  add_residuals(mod,"res")

ggplot(grid, aes(x,y)) +
  labs(
    x = "Koszt wyprodukowania", 
    y = "Jakoœæ",
    title = "Jakoœæ modelu regresji liniowej mierzona sum¹ kwadratów b³êdów") +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 14)) + 
  geom_segment(aes(x=150, xend = 200 , y=55, yend = 55), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_segment(aes(x=150, xend = 150 , y=55, yend = 100), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_point(size=4) +
  geom_line(aes(y=.fitted), colour="darkgray", data=mod, size = 2) +
  geom_segment(aes(xend = x, yend = pred), colour="red", size = 1) +
  geom_label(aes(label = round(res,1)),nudge_x = 1.5)


par(mfrow=c(1, 2))
mod.stdres = rstandard(mod)
library(MASS)
hist(mod.stdres, freq=FALSE, main="Rozk³ad standaryzowanych reszt modelu \n versus rozk³ad normaly", col="gray35", ylab = "Gêstoœæ", xlab = "Standaryzowane reszty")
xfit<-seq(min(mod.stdres),max(mod.stdres),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit,lwd=4) 
qqnorm(mod.stdres, main="Wykres Q-Q ", ylab="Standaryzowane b³êdy", xlab="Teoretyczne b³êdy",cex = 1.5, bg='black', pch=21) 
qqline(mod.stdres, lwd=3)

dev.off()
library(car)
?spreadLevelPlot
spreadLevelPlot(mod, main="Rozproszenie b³êdów modelu", lwd=4, xlab="Faktyczne wartoœci", ylab="Standaryzowane reszty", pch=16)

library(plot3D)
x <- seq(-2, 2, length.out = 15)
y <- seq(-1, 3, length.out = 15)

# Rosenbrock Function
f1 <- function (x, y) {
  out <- (1 - x) ** 2 + 100 * (y - x**2)**2
  return (out)
}
z <- outer(x, y, f1)

# Initial Guess
w_old <- matrix(c(0, 0)); w_new <- matrix(c(-1.8,-.8))
gamma <- .0002 # set the learning rate
precision <- .00001 # set the precision

f1_primew1 <- function (w1, w2) {
  out <- -2 * (1 - w1) - 400 * (w2 - w1**2) * w1
  return (out)
}
f1_primew2 <- function (w1, w2) {
  out <- 200 * (w2 - w1**2)
  return (out)
}

# Gradient Vector
g_vec <- function (w1, w2) {
  matrix(c(f1_primew1(w1, w2), f1_primew2(w1, w2)))
}

# Contour Plot
contour2D(z = z, x = x, y = y, col = 'black', colkey = FALSE)

i <- 2; cx <- cy <- sx <- sy <- sz <- numeric()
cx[1] <- w_new[1, ]; cy[1] <- w_new[2, ]

# Perform Gradient Descent
while(norm(w_new - w_old, 'F') > precision) {
  w_old <- w_new
  w_new <- w_old - gamma * g_vec(w_old[1,], w_old[2,])
  cx[i] <- sx[i - 1] <- w_new[1, ]; cy[i] <- sy[i - 1] <- w_new[2, ]
  sz[i - 1] <- f1(sx[i - 1], sy[i - 1])
  arrows2D(cx[i - 1], cy[i - 1], cx[i], cy[i], add = TRUE)
  i <- i + 1
}
i - 2; w_new

persp3D(x, y, z, col = ramp.col(n = 50, col = c("#FF033E", "#FFBF00", "#FF7E00", "#08E8DE", "#00FFFF", "#03C03C"), alpha = .1), border = "#808080", theta = 20, phi = 20, colkey = FALSE, xlab = "Ox", ylab = "Oy", zlab = "Oz")
contour3D(x, y, z = 0, colvar = z, col = c("darkgray"), alpha = .3, add = TRUE, colkey = FALSE)
points3D(sx, sy, sz,  col = 'black', size = 0.5, add = TRUE, pch=25)

concrete <- read.csv2("concreteData.csv")
colnames(concrete) <- c("Cement","Blast","Fly","Water","Superplasticizer","Coarse","Concrete","Age","Strength")

normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

concreteNorm <- as.data.frame(lapply(concrete, normalize))
concreteTrain <- concreteNorm[1:773, ]
concreteTest <- concreteNorm[774:1030, ]

#install.packages("neuralnet")
library(neuralnet)
names(concreteTest <- concreteNorm[774:1030, ])

modelConcrete <- neuralnet(Strength ~ Cement + Blast +
                              Fly + Water + Superplasticizer +
                              Coarse + Concrete + Age,
                            data = concreteTrain, hidden = 3)

plot(modelConcrete)

dev.off()
