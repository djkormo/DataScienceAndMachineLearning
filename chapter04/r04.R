library(tidyverse)
library(ggthemes)

source("multiplot.R")


fit <- lm(mpg ~ hp, data = mtcars)
prd <- data.frame(hp = seq(from = range(mtcars$hp)[1], to = range(mtcars$hp)[2], length.out = 100))
err <- predict(fit, newdata = prd, se.fit = TRUE)

prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit

p1 <- ggplot(prd, aes(x = hp, y = fit)) +
  labs(
    x = "Moc silnika", 
    y = "Spalanie",
    title = "Model liniowy") +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 14)) + 
  geom_segment(aes(x=50, xend = 350 , y=0, yend = 0), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_segment(aes(x=50, xend = 50 , y=0, yend = 40), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity", size=2) +
  geom_point(data = mtcars, aes(x = hp, y = mpg))

fit <- lm(mpg ~ hp + I(hp^2), data = mtcars)
prd <- data.frame(hp = seq(from = range(mtcars$hp)[1], to = range(mtcars$hp)[2], length.out = 100))
err <- predict(fit, newdata = prd, se.fit = TRUE)

prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit

prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit

p2 <- ggplot(prd, aes(x = hp, y = fit)) +
  labs(
    x = "Moc silnika", 
    y = "Spalanie",
    title = "Model wielomianowy drugiego stopnia") +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 14)) + 
  geom_segment(aes(x=50, xend = 350 , y=0, yend = 0), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_segment(aes(x=50, xend = 50 , y=0, yend = 40), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity", size=2) +
  geom_point(data = mtcars, aes(x = hp, y = mpg))

fit <- lm(mpg ~ hp + I(hp^2) + I(hp^3), data = mtcars)
prd <- data.frame(hp = seq(from = range(mtcars$hp)[1], to = range(mtcars$hp)[2], length.out = 100))
err <- predict(fit, newdata = prd, se.fit = TRUE)

prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit

prd$lci <- err$fit - 1.96 * err$se.fit
prd$fit <- err$fit
prd$uci <- err$fit + 1.96 * err$se.fit

p3 <- ggplot(prd, aes(x = hp, y = fit)) +
  labs(
    x = "Moc silnika", 
    y = "Spalanie",
    title = "Model wielomianowy trzeciego stopnia ") +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 14)) + 
  geom_segment(aes(x=50, xend = 350 , y=0, yend = 0), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_segment(aes(x=50, xend = 50 , y=0, yend = 40), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_smooth(aes(ymin = lci, ymax = uci), stat = "identity", size=2) +
  geom_point(data = mtcars, aes(x = hp, y = mpg))

multiplot(p1, p2, p3, cols=3)

