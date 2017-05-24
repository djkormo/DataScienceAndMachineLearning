dataset <- maml.mapInputPort(1)

# read in two different classification results
aresults <- dataset[,1]
bresults <- dataset[,2]

# compare the classification results using McNemar's test'
mtest <- mcnemar.test(aresults, bresults)

# output the p-value of McNemar's test'
data.set <- data.frame(mtest["p.value"])

maml.mapOutputPort("data.set");