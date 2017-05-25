install.packages("party")
library(party)
iris <- read.csv2(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header = FALSE, sep = ",")
names(iris) <- c("sepal length","sepal width","petal length","petal width","species")

iris_ctree <- ctree(species ~ ., data=iris)
plot(iris_ctree)

#source("https://bioconductor.org/biocLite.R")
#biocLite("gRain")
library(gRain)
library(Rgraphviz)

d <- cptable(~ kierowca, values=c(0.75, 0.25),
              levels=c("dobry", "z³y"))

szkody <- c(0, 1, 2)
c1 <- cptable(~ rok_1|kierowca, 
               values=c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2),
               levels=as.character(szkody))
c2 <- cptable(~ rok_2|kierowca, 
               values=c(0.7, 0.2, 0.1, 0.5, 0.3, 0.2),
               levels=as.character(szkody))
plist <- compileCPT(list(d, c1, c2))

(srednioSzkody <- colSums(sweep(plist[["rok_1"]], 1, szkody, "*")))
(bezwarunkowoSzkody <- sum(srednioSzkody * plist[["kierowca"]]))

pn <- grain(plist)
par(mfrow=c(2,1))
?mosaicplot
mosaicplot(pn[["cptlist"]][["rok_1"]], color=TRUE, cex.axis=1.2,
            main="Prawdopodobieñstwo warunkowe",
            xlab="Liczba szkód w roku",
            ylab="Kategoria kierowcy")

plot(pn[["dag"]], main="Sieæ Bayesa", 
      attrs = list(node = list(fillcolor = "ghostwhite"),
                   edge = list(color = "black"), 
                   graph = list(rankdir = "LR")))




# Execute R Script: combine labels ----------------------------------------

data <- maml.mapInputPort(1) # class: data.frame

data$workclass = gsub("^Federal-gov","Federal-Govt",data$workclass)
data$workclass = gsub("^Local-gov","Other-Govt",data$workclass)
data$workclass = gsub("^State-gov","Other-Govt",data$workclass)
data$workclass = gsub("^Private","Private",data$workclass)
data$workclass = gsub("^Self-emp-inc","Self-Employed",data$workclass)
data$workclass = gsub("^Self-emp-not-inc","Self-Employed",data$workclass)
data$workclass = gsub("^Without-pay","Not-Working",data$workclass)
data$workclass = gsub("^Never-worked","Not-Working",data$workclass)

data$occupation = gsub("^Adm-clerical","Admin",data$occupation)
data$occupation = gsub("^Armed-Forces","Military",data$occupation)
data$occupation = gsub("^Craft-repair","Blue-Collar",data$occupation)
data$occupation = gsub("^Exec-managerial","White-Collar",data$occupation)
data$occupation = gsub("^Farming-fishing","Blue-Collar",data$occupation)
data$occupation = gsub("^Handlers-cleaners","Blue-Collar",data$occupation)
data$occupation = gsub("^Machine-op-inspct","Blue-Collar",data$occupation)
data$occupation = gsub("^Other-service","Service",data$occupation)
data$occupation = gsub("^Priv-house-serv","Service",data$occupation)
data$occupation = gsub("^Prof-specialty","Professional",data$occupation)
data$occupation = gsub("^Protective-serv","Other-Occupations",data$occupation)
data$occupation = gsub("^Sales","Sales",data$occupation)
data$occupation = gsub("^Tech-support","Other-Occupations",data$occupation)
data$occupation = gsub("^Transport-moving","Blue-Collar",data$occupation)

data$country[data$country=="Cambodia"] = "SE-Asia"
data$country[data$country=="Canada"] = "British-Commonwealth" 
data$country[data$country=="China"] = "China" 
data$country[data$country=="Columbia"] = "South-America" 
data$country[data$country=="Cuba"] = "Other" 
data$country[data$country=="Dominican-Republic"] = "Latin-America"
data$country[data$country=="Ecuador"] = "South-America" 
data$country[data$country=="El-Salvador"] = "South-America" 
data$country[data$country=="England"] = "British-Commonwealth"
data$country[data$country=="France"] = "Europe"
data$country[data$country=="Germany"] = "Europe"
data$country[data$country=="Greece"] = "Europe"
data$country[data$country=="Guatemala"] = "Latin-America"
data$country[data$country=="Haiti"] = "Latin-America"
data$country[data$country=="Holand-Netherlands"] = "Europe"
data$country[data$country=="Honduras"] = "Latin-America"
data$country[data$country=="Hong"] = "China"
data$country[data$country=="Hungary"] = "Europe"
data$country[data$country=="India"] = "British-Commonwealth"
data$country[data$country=="Iran"] = "Other"
data$country[data$country=="Ireland"] = "British-Commonwealth"
data$country[data$country=="Italy"] = "Europe"
data$country[data$country=="Jamaica"] = "Latin-America"
data$country[data$country=="Japan"] = "Other"
data$country[data$country=="Laos"] = "SE-Asia"
data$country[data$country=="Mexico"] = "Latin-America"
data$country[data$country=="Nicaragua"] = "Latin-America"
data$country[data$country=="Outlying-US(Guam-USVI-etc)"] = "Latin-America"
data$country[data$country=="Peru"] = "South-America"
data$country[data$country=="Philippines"] = "SE-Asia"
data$country[data$country=="Poland"] = "Europe"
data$country[data$country=="Portugal"] = "Europe"
data$country[data$country=="Puerto-Rico"] = "Latin-America"
data$country[data$country=="Scotland"] = "British-Commonwealth"
data$country[data$country=="South"] = "Europe"
data$country[data$country=="Taiwan"] = "China"
data$country[data$country=="Thailand"] = "SE-Asia"
data$country[data$country=="Trinadad&Tobago"] = "Latin-America"
data$country[data$country=="United-States"] = "United-States"
data$country[data$country=="Vietnam"] = "SE-Asia"
data$country[data$country=="Yugoslavia"] = "Europe"

data$education = gsub("^10th","Dropout",data$education)
data$education = gsub("^11th","Dropout",data$education)
data$education = gsub("^12th","Dropout",data$education)
data$education = gsub("^1st-4th","Dropout",data$education)
data$education = gsub("^5th-6th","Dropout",data$education)
data$education = gsub("^7th-8th","Dropout",data$education)
data$education = gsub("^9th","Dropout",data$education)
data$education = gsub("^Assoc-acdm","Associates",data$education)
data$education = gsub("^Assoc-voc","Associates",data$education)
data$education = gsub("^Bachelors","Bachelors",data$education)
data$education = gsub("^Doctorate","Doctorate",data$education)
data$education = gsub("^HS-Grad","HS-Graduate",data$education)
data$education = gsub("^Masters","Masters",data$education)
data$education = gsub("^Preschool","Dropout",data$education)
data$education = gsub("^Prof-school","Prof-School",data$education)
data$education = gsub("^Some-college","HS-Graduate",data$education)

data$marital[data$marital=="Never-married"] = "Never-Married"
data$marital[data$marital=="Married-AF-spouse"] = "Married"
data$marital[data$marital=="Married-civ-spouse"] = "Married"
data$marital[data$marital=="Married-spouse-absent"] = "Not-Married"
data$marital[data$marital=="Separated"] = "Not-Married"
data$marital[data$marital=="Divorced"] = "Not-Married"
data$marital[data$marital=="Widowed"] = "Widowed"

maml.mapOutputPort("data");

