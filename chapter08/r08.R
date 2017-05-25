library(arules)
library(datasets)
dev.off()
data(Groceries)
itemFrequencyPlot(Groceries,topN=20,type="absolute", xlab = "Czêsto kupowane produkty", ylab = "Czêstoœæ", col = "Black", main = "Lista 20 najpopularniejszych produktów ")
?itemFrequencyPlot

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))
inspect(rules[1:5])
rules<-sort(rules, by="confidence", decreasing=TRUE)
inspect(rules[1:3])
rules<-sort(rules, by="lift", decreasing=TRUE)
inspect(rules[1:3])

library(arulesViz)
plot(rules[1:50],method="graph",interactive=TRUE,shading=NA)
plot(rules[1:50],method="graph", main = "")
