install.packages("tree")
library(car)
library(tree)
library("readxl")
library(FNN)
library(MASS)

Snails <- read_excel("F:\\Fall2021\\ISEN 613\\HW\\HW 5\\Snails.xlsx")
names(Snails)
set.seed (13)
#train <- sample (1: nrow(Snails), nrow(Snails)/2)
#tree.Snails<- tree(Rings~.,data=Snails,subset=train)

#K-Fold validation with Regression Tree
k<-10
n<- nrow(Snails)
set.seed(13)
folds<-sample(rep(1:k,length=n))
Test.Rsquared<- rep(NA,k)
for (i in 1:k){
  train<-Snails[folds!=i,]
  test<-Snails[folds==i,]
  tree.Snails<- tree(Rings~.,data=train)
  predsnails<- predict(tree.Snails ,newdata =test)
  RSS<-sum((predsnails-test$Rings)^2)
  TSS<-sum((test$Rings-mean(test$Rings))^2)
  Test.Rsquared[i]<- 1-(RSS/TSS)
}
Test.Rsquared
mean(Test.Rsquared)
plot(tree.Snails)
text(tree.Snails, pretty=0)
summary(tree.Snails)
