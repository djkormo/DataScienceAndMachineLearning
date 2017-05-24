# for first run
#install.packages("entropy")
library(entropy)

flips<-rep("Head",10)
freqs<-table(flips)/length(flips)
freqs
entropy(freqs,"unit=log2")