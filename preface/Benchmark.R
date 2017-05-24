rm(list=ls())
set.seed(1)
m<-10000
n<-5000
A<-matrix(runif(m*n),m,n)
system.time(B<-crossprod(A))
