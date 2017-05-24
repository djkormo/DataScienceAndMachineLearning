library(readr)
library(ggplot2)
titanic <- read_csv("D:/installs/data science/MarcinSzeligaDataScience/chapter02/train.csv")



ggplot(titanic,aes(x=Survived,y=Age))+
  labs(
    x=" 0 osoba zginela , 1 - przezyla",
    y="Wiek",
    title="Zwiazek wieku z przezyciem katastrofy")+
  theme_tufte()+
  theme(plot.title=element_text(face="bold",size=14),
        axis.title=element_text(size=14))+
 # geom_segment(aes(x-1,xend=2,y=0,yend=0),size=1.5,
#               arrow=arrow(length=unit(0.4,"cm")))+
  geom_segment(aes(x=-1,xend=-1,y=0,yend=80),size=1.5,
               arrow=arrow(length=unit(0.4,"cm")))+
geom_boxplot(aes(group=Survived))