library(ISLR)
library(FNN)
library(leaps)
library(car)
library("readxl")
Snails <- read_excel("F:\\Fall2021\\ISEN 613\\HW\\HW 5\\Snails.xlsx")
head(Snails)
Snails1 = Snails

#predict function for regsubsets
predict.regsubsets =function (object ,newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix (form ,newdata )
  coefi =coef(object ,id=id)
  xvars =names (coefi )
  mat[,xvars ]%*% coefi
}

#best subset without interaction
k<- 10
n<- nrow(Snails1)
set.seed(13)
folds<- sample(rep(1:k,length=n))
cv.errors1<- matrix(NA,k,8,dimnames = list(NULL, paste(1:8)))
for (j in 1:k){
  best.fit1<- regsubsets(Rings~.,data=Snails1[folds!=j,], method="forward")
  for (i in 1:8){
    pred<- predict(best.fit1,Snails1[folds==j,],id=i) #calculating best subset with adj r2
    cv.errors1[j,i]<- mean((Snails1$Rings[folds==j]-pred)^2)
  }
}
mean.cverror1<- apply(cv.errors1, 2, mean)
which.min(mean.cverror1) #at id = 33 we get the least bic and therefore the parameters should be considered
coef(best.fit1, id= 7)

Snails1[which(Snails1$Type=="M"), 'Sex']= 1
Snails1[which(Snails1$Type=="F"), 'Sex']= 2
Snails1[which(Snails1$Type=="I"), 'Sex']= 3
Snails1 = Snails1[,-1]
#Model without interactions and k fold cross validation
#Validation using k fold = 10 
k=10
set.seed(13)
folds<-sample(rep(1:k,length=nrow(Snails1)))
Test1.Rsquared<- rep(NA,k)
for (i in 1:k){
  train1<-Snails1[folds!=i,]
  test1<-Snails1[folds==i,]
  #Should we use train data or whole data here
  lmfit1 <- lm(Rings~.-LongestShell, data=train1)
  pred<- predict(lmfit1,test1)
  RSS<-sum((test1$Rings-pred)^2)
  TSS<-sum((mean(test1$Rings)-test1$Rings)^2)
  Test1.Rsquared[i]<- 1-(RSS/TSS)
}
mean(Test1.Rsquared)  #52.81% is the test R2


#Doing K-fold for best subsets and building a model with interactions
k<- 10
n<- nrow(Snails1)
set.seed(13)
folds2<- sample(rep(1:k,length=n))
cv.errors2<- matrix(NA,k,36,dimnames = list(NULL, paste(1:36)))
for (j in 1:k){
  best.fit2<- regsubsets(Rings~.^2,data=Snails1[folds2!=j,], nvmax=36, method="forward")
  for (i in 1:36){
    pred2<- predict(best.fit2,Snails1[folds2==j,],id=i) #calculating best subset with adj r2
    cv.errors2[j,i]<- mean((Snails1$Rings[folds2==j]-pred2)^2)
  }
}
mean.cverror2<- apply(cv.errors2, 2, mean)
which.min(mean.cverror2) #19 predictors
reg.summary <- summary(best.fit2)

#after kfold validation the value best subsets are the one with 19 predictors
coef(best.fit2, id=19)

#Linear fit using k=10 fold cross validation 
k=10
set.seed(13)
folds2<-sample(rep(1:k,length=nrow(Snails1)))
Test.Rsquared2<- rep(NA,k)
for (i in 1:k){
  train2<-Snails1[folds!=i,]
  test2<-Snails1[folds==i,]
  lmfitk1 <-lm(Rings~Sex+Diameter+Height+WholeWeight+ShuckedWeight+
                 ShellWeight+Sex:Diameter+Sex:Height+Sex:WholeWeight+Sex:ShuckedWeight+
                 +LongestShell:VisceraWeight+Diameter:ShellWeight+Height:WholeWeight+
                 Height:ShuckedWeight+WholeWeight:ShuckedWeight+WholeWeight:VisceraWeight+
                 ShuckedWeight:VisceraWeight+ShuckedWeight:ShellWeight+VisceraWeight:ShellWeight,
               data = train2)
  pred2<- predict(lmfitk1,test2)
  RSS2<-sum((test2$Rings-pred2)^2)
  TSS2<-sum((mean(test2$Rings)-test2$Rings)^2)
  Test.Rsquared2[i]<- 1-(RSS2/TSS2)
}
mean(Test.Rsquared2) #56.18% test R2 value with a k fold of 10 at set seed 13


# plots to see if data follows the assumptions of linear model
#model diagnostics
par(mfrow=c(2,2))
plot(lmfitk1)
#diagnostic plots reveal that there is a non linearity between the residuals vs fitted 
ncvTest(lmfitk1) #p value highly significant therefore we do not have a non constant variance term

# we can apply a log transformation to response

# K fold cross validation with logarithm applied to response
k=10
set.seed(13)
folds3<-sample(rep(1:k,length=nrow(Snails1)))
Test2.Rsquared3<- rep(NA,k)
for (i in 1:k){
  train3<-Snails1[folds3!=i,]
  test3<-Snails1[folds3==i,]
  lmfitk2 <-lm(log(Rings)~Type+Diameter+Height+WholeWeight+ShuckedWeight+
                 ShellWeight+Type:Diameter+Type:Height+Type:WholeWeight+Type:ShuckedWeight+
                 +LongestShell:VisceraWeight+Diameter:ShellWeight+Height:WholeWeight+
                 Height:ShuckedWeight+WholeWeight:ShuckedWeight+WholeWeight:VisceraWeight+
                 ShuckedWeight:VisceraWeight+ShuckedWeight:ShellWeight+VisceraWeight:ShellWeight,
               data = train3)
  
  pred3<- predict(lmfitk2,test3)
  RSS3<-sum((test3$Rings-exp(pred3))^2)
  TSS3<-sum((mean(test3$Rings)-test3$Rings)^2)
  Test2.Rsquared3[i]<- 1-(RSS3/TSS3)
}
mean(Test2.Rsquared3) #56.011 % is the testing R2 with 19 predictors 

summary(lmfitk1)

#Final suggested model
lmfitk2 <-lm(Rings~Type+Diameter+Height+WholeWeight+ShuckedWeight+
               ShellWeight+Type:Diameter+Type:Height+Type:WholeWeight+Type:ShuckedWeight+
               +LongestShell:VisceraWeight+Diameter:ShellWeight+Height:WholeWeight+
               Height:ShuckedWeight+WholeWeight:ShuckedWeight+WholeWeight:VisceraWeight+
               ShuckedWeight:VisceraWeight+ShuckedWeight:ShellWeight+VisceraWeight:ShellWeight,
             data = Snails1)
