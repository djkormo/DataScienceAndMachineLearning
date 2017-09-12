clusters <- hclust(dist(iris[,1:4]))
plot(clusters, main = "Dendrogram klastrów", ylab="Poziom", xlab="Klastry irysów")

clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)

library(tidyverse)
library(ggthemes)

ggplot(iris, aes(Petal.Length, Petal.Width)) + 
  labs(
    x = "D³ugoœæ p³atka", 
    y = "Szerokoœæ p³atka",
    title = "Zale¿noœæ pomiêdzy d³ugoœci¹ i szerokoœci¹ p³atków a gatunkiem irysów") +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))+ 
  geom_segment(aes(x=0, xend = 7 , y=0, yend = 0), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_segment(aes(x=0, xend = 0 , y=0, yend = 3), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_jitter(aes(color = iris$Species, shape = iris$Species), size = 4) +
  scale_color_manual(values = c('black', 'red', 'green')) +
  scale_shape_manual(values = c(15,16,18))
  

clusters <- hclust(dist(iris[,1:4]), method = 'average')
plot(clusters, main = "Dendrogram klastrów", ylab="Poziom", xlab="Klastry irysów")
clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)

clusters <- hclust(dist(iris[,3:4]), method = 'average')
clusterCut <- cutree(clusters, 3)
table(clusterCut, iris$Species)


ggplot(iris, aes(Petal.Length, Petal.Width)) + 
  labs(
    x = "D³ugoœæ p³atka", 
    y = "Szerokoœæ p³atka",
    title = "Zale¿noœæ pomiêdzy d³ugoœci¹ i szerokoœci¹ p³atków a gatunkiem irysów") +
  theme_tufte() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.title = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))+ 
  geom_segment(aes(x=0, xend = 7 , y=0, yend = 0), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_segment(aes(x=0, xend = 0 , y=0, yend = 3), size=1.5,
               arrow = arrow(length = unit(0.4,"cm"))) +
  geom_point(aes(color = iris$Species, shape = iris$Species), size = 6, alpha = 0.6) +
  geom_point(col = clusterCut, size = 2) +
  scale_color_manual(values = c('black', 'red', 'green')) +
  scale_shape_manual(values = c(15,16,18))


# Set the RNG seed to ensure reproducibility
set.seed(12345)
?rnorm
# Let's create 3 visually distinct clusters
n <- c(1000, 500, 850)
x <- c(rnorm(n[1], 10, 2), 
       rnorm(n[2], 15, 2),
       rnorm(n[3], 11, 2))
y <- c(rnorm(n[1], 5, 2),
       rnorm(n[2], 6, 2),
       rnorm(n[3], 3, 2))

par(mfrow=c(3,1))
col = c("blue", "darkgreen", "darkred")
# Run k-means with 3 clusters and random initial centroids 
# to check the clusters are correctly recognized
km <- kmeans(cbind(x, y), 3, iter.max =1)
# Plot the data, colored by cluster
plot(x, y, pch=20, col=col[km$cluster], main = "Pierwsza iteracja")

# Mark the final centroids
points(km$centers, pch=8, cex=5, col="black", lwd=3)

km <- kmeans(cbind(x, y), 3, iter.max =2)
plot(x, y, pch=20, col=col[km$cluster], main = "Druga iteracja")
points(km$centers, pch=8, cex=5, col="black", lwd=3)

km <- kmeans(cbind(x, y), 3, iter.max =3)
plot(x, y, pch=20, col=col[km$cluster], main = "Trzecia iteracja")
points(km$centers, pch=8, cex=5, col="black", lwd=3)
