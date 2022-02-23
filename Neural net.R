#Building a model with a neural net
install.packages("neuralnet")
library(leaps)
library(neuralnet)
library(MASS)
library("readxl")
mydata <- read_excel("F:\\Fall2021\\ISEN 613\\HW\\HW 5\\Snails.xlsx")
#preprocessing using PCA
head(mydata)
mydata1 <- mydata
mydata1[which(mydata1$Type=="M"), 'Sex']= 1
mydata1[which(mydata1$Type=="F"), 'Sex']= 2
mydata1[which(mydata1$Type=="I"), 'Sex']= 3
mydata1=mydata1[,-1]
head(mydata1)
pcainput <- mydata1[,-8]
head(pcainput)
snailrings <- mydata1$Rings
apply(pcainput,2,mean) #finding mean of each column
#preprocessing with PCA 
pr.out1 = prcomp(pcainput, scale = TRUE)
summary(pr.out1)
names(pr.out1)
pca1<- summary(pr.out1)$x
newdata <- cbind(pca1, snailrings)
newdata

# With 10 fold CV
newdata1 <- newdata
maxs <- apply(newdata1,2,max)
mins <- apply(newdata1,2,min)
k=10
set.seed(13)
folds<-sample(rep(1:k,length=nrow(newdata1)))
Test1.Rsquared<- rep(NA,k)
scaled1 <- as.data.frame(scale(newdata1, center = mins, scale = maxs - mins))
for (i in 1:10){
  train1<-scaled1[folds!=i,]
  test1<-scaled1[folds==i,]
  nn2 <- neuralnet(snailrings~PC1+PC2+PC3+PC4+PC5, data=train1)
  pr.nn1 <- compute(nn2, test1[,1:5])
  pr.nn1_ <- pr.nn1$net.result*(max(newdata1[,9])-min(newdata1[,9]))+min(newdata1[,9])
  test.r1 <- (test1[,9])*(max(newdata1[,9]-min(newdata1[,9])))+min(newdata1[,9])
  predsnailrings1 <- test.r1
  #truesnailrings1 <- unscaledtest[,9]
  prround1 <- round(pr.nn1_)
  #RSS1<- sum((prround1 - test.r1)^2)
  RSS2<- sum((pr.nn1_ - test.r1)^2)
  TSS2<- sum((mean(test.r1)-test.r1)^2)
  Test1.Rsquared[i] <- 1-RSS2/TSS2
  #R2<- 1- RSS1/TSS1
  #R2
}
mean(Test1.Rsquared)
summary(nn2)
plot(nn2)

