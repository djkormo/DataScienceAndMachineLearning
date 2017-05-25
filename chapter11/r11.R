
library(psych)
?pairs.panels
pairs.panels(iris,hist.col="gray30")



# Execute R Script: mean accuracy -----------------------------------------
dataset1 <- maml.mapInputPort(1) # class: data.frame
virginica <- dataset1[dataset1[,9] == "Iris-virginica",8]
versicolor <- dataset1[dataset1[,9] == "Iris-versicolor",7]
setosa <- dataset1[dataset1[,9] == "Iris-setosa",6]
data.set <- append(virginica,versicolor)
data.set <- append(data.set,setosa)
data.set <- mean(data.set)
data.set <- as.data.frame(data.set)
maml.mapOutputPort("data.set");

