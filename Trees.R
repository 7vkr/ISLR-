#Trees
library(tree)
library(ISLR)
library(FNN)
library(leaps)
library(MASS)
library("readxl")
Snails <- read_excel("F:\\Fall2021\\ISEN 613\\HW\\HW 5\\Snails.xlsx")
Snails
mydata <- Snails
mydata[which(mydata$Type=="M"), 'Gender']= 1
mydata[which(mydata$Type=="F"), 'Gender']= 2
mydata[which(mydata$Type=="I"), 'Gender']= 3
head(mydata)
mydatarings <- mydata$Rings
mydata <- mydata[,-1]
set.seed(13)

#Validation set
trainindex = sample(1:nrow(mydata), nrow(mydata)*0.8)
train = mydata[trainindex, ]
test = mydata[-trainindex,]

#tree model with all predictors
tree1 <- tree(Rings~., data= train)
summary(tree1)
names(summary(tree1))
summary(tree1)$size
par(mfrow = c(1,1))
plot(tree1)
text(tree1, pretty=0)

#pruning to improve performance and finding the test R2 using validation set
#we donot know for which best = value we get a better model so lets do between 1 - 15
#Oops can't do for i=1 as for one node there is no use of predict function
RMD=c() #root mean deviation
RSquare=c() 
set.seed(13)
for(i in 2:15){
prune.tree1 = prune.tree(tree1,best=i)
treepred = predict(prune.tree1, newdata=test)
RMD[i]=mean((treepred-test$Rings)^2)
RSS=sum((test$Rings-treepred)^2)
RSS
TSS=sum((mean(test$Rings)-test$Rings)^2)
TSS
RSquare[i]=1-RSS/TSS
}
RMD
which.min(RMD)
min(na.omit(RMD))
RSquare
which.max(RSquare)
max(na.omit(RSquare))

#Cross validate using k - fold 
folds<- sample(rep(1:k, length = n))
Testing.Rsquared <- matrix(NA, k, nrow(mydata1[folds==10,]), dimnames =list(NULL, paste(1:350)))
RMDcv=c()
RSquarecv=c()
set.seed(13)
k=10 #10 fold cross validation
size=c()
Rsq2=c()
Rsq2new = c()
for(j in 1:k){
  index=sample(1:nrow(mydata),0.7*nrow(mydata),replace=F)
  train2=mydata1[folds!=j,]
  test2=mydata1[folds==j,]
  tree.cv= tree(Rings~.,train2)
  size[j]=summary(tree.cv)$size
  for(i in 2:size[j]){
    prune.tree.cv=prune.tree(tree.cv,best=i)
    treecvpred=predict(prune.tree.cv,newdata=test)
    RMD[i]=mean((treecvpred-test$Rings)^2)
    RSS=sum((test$Rings-treecvpred)^2)
    TSS=sum((mean(test$Rings)-test$Rings)^2)
    Rsq2[i]=1-RSS/TSS
  }
  
  Rsq2new[j]=max(na.omit(Rsq2[i]))
}

mean(na.omit(Rsq2new))
