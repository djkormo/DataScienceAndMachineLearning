#install.packages("tidyverse")
#install.packages("ggthemes")

rm(list=ls())

library(tidyverse)
library(ggthemes)

x<-rnorm(100,mean=10,sd=4)
y<-rnorm(100,mean=50,sd=10)

bimodal<-c(x,y)
dat<-as.data.frame(bimodal)

summary(bimodal)

ggplot(dat,aes(bimodal))+
  labs(
    x=" Wartoœæ zmiennej X",
    y="J¹drowy estymator gêstoœci",
    title="Rozk³ad dwumianowy")+
  theme_tufte()+
  theme(plot.title=element_text(face="bold",size=14),
        axis.title=element_text(size=14))+
  geom_segment(aes(x=0,xend=100,y=0,yend=0),size=1.5,
               arrow=arrow(length=unit(0.4,"cm")))+
  geom_segment(aes(x=0,xend=100,y=0,yend=0.03),size=1.5,
               arrow=arrow(length=unit(0.4,"cm")))+
  geom_density(size=1.5)
  