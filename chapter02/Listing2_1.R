# for first run
#install.packages("entropy")
library(entropy)
flips<-c("Head","Head","Tail","Tail","Head","Tail","Head","Head","Head","Tail")
freqs<-table(flips)/length(flips)
freqs
entropy(freqs,"unit=log2")