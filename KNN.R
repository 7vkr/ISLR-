#Building a model Using KNN
#Sathvik 2
library(FNN)
library(ISLR)
library("readxl")
mydata <- read_excel("F:\\Fall2021\\ISEN 613\\HW\\HW 5\\Snails.xlsx")
head(mydata)
mydata1 <- mydata
#Step1: Decide and finalize the parameters
#Using all the parameters to avoid hassle 
head(mydata1)
k <- 10
n<- nrow(mydata1)
set.seed(13)
folds<- sample(rep(1:k, length = n))
Testing.Rsquared <- matrix(NA, k, nrow(mydata1[folds==10,]), dimnames =list(NULL, paste(1:350)))
###
for (j in 1:k){
  for (i in 1:nrow(mydata1[folds==j,])) {
    traindata<- mydata1[folds!=j,]
    testdata<- mydata1[folds==j,]
    KnnModel <- knn.reg(train= traindata[,2:8], y = traindata$Rings, test= testdata[,2:8], k=i)
    RSS <- sum((KnnModel$pred - testdata$Rings)^2)
    TSS <- sum((mean(testdata$Rings)-testdata$Rings)^2)
    Testing.Rsquared[j,i] <- 1-RSS/TSS
  }
}
mean.Rsquared<-apply(Testing.Rsquared,2,mean)
which.max(mean.Rsquared) #Highest R2 is obtained at k = 25
max(mean.Rsquared) # R2 = 55.41%





