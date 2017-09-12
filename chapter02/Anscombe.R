require(stats)
require(graphics)

rm(list=ls())

data(anscombe)



anscombe.1 <- data.frame(x = anscombe[["x1"]], y = anscombe[["y1"]], Set = "Anscombe Set 1")
anscombe.2 <- data.frame(x = anscombe[["x2"]], y = anscombe[["y2"]], Set = "Anscombe Set 2")
anscombe.3 <- data.frame(x = anscombe[["x3"]], y = anscombe[["y3"]], Set = "Anscombe Set 3")
anscombe.4 <- data.frame(x = anscombe[["x4"]], y = anscombe[["y4"]], Set = "Anscombe Set 4")

anscombe.data <- rbind(anscombe.1, anscombe.2, anscombe.3, anscombe.4)
aggregate(cbind(x, y) ~ Set, anscombe.data, mean)

aggregate(cbind(x, y) ~ Set, anscombe.data, sd)

library(plyr)

correlation <- function(data) {
  x <- data.frame(r = cor(data$x, data$y))
  return(x)
}

ddply(.data = anscombe.data, .variables = "Set", .fun = correlation)

model1 <- lm(y ~ x, subset(anscombe.data, Set == "Anscombe Set 1"))
model2 <- lm(y ~ x, subset(anscombe.data, Set == "Anscombe Set 2"))
model3 <- lm(y ~ x, subset(anscombe.data, Set == "Anscombe Set 3"))
model4 <- lm(y ~ x, subset(anscombe.data, Set == "Anscombe Set 4"))

summary(anscombe)

library(ggplot2)

gg <- ggplot(anscombe.data, aes(x = x, y = y))
gg <- gg + geom_point(color = "black")
gg <- gg + facet_wrap(~Set, ncol = 2)
gg <- gg + geom_smooth(formula = y ~ x, method = "lm", se = FALSE, data = anscombe.data)
gg