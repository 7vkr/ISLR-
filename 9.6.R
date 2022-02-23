setwd("C:/Users/james/Dropbox/Courses Taught/Analytics or similar/Data Analtyics ISEN 613 -- Tamu 2021 Fall/9.6")


########################### Checking Assumptions


my.frame = read.csv("Advertising.csv", header=T)

head(my.frame)
attach(my.frame)

par(mfrow=c(1,3))

plot(TV,sales, col="red", main="sales vs TV Budget")
fitTV<- lm(sales ~ TV)
abline(fitTV,col="blue",lwd=4)

plot(radio,sales, col="red", main="sales vs radio Budget")
fitradio<- lm(sales ~ radio)
abline(fitradio,col="blue",lwd=4,lty=2)

plot(newspaper,sales, col="red", main="sales vs newspaper Budget")
fitnewspaper<- lm(sales ~ TV)
abline(fitnewspaper,col="blue",lwd=4,lty=3)


#######################Effect of Noise


n<-14           #Number of Data points
sd<-5         #Standard Deviation of Noise
sd<-0

NoiseImpact<- function(n,sd){
  lb<-0
  ub<-40
  
  set.seed(1)
  x<- runif(n, lb,ub)  #Input Data
  noise<- rnorm(n,mean=0,sd)
  y<- 1/200*(x-25)^3+1/10*(x-25)^2+noise #Randomly Generated Output Data
  
  par(mfrow=c(1,1))
  
  w<-seq(lb,ub,length=1000)
  z<-1/200*(w-25)^3+1/10*(w-25)^2  #True Function
  
  plot(w,z, col="blue",type="l",lwd=2, xlim=c(0,40), ylim=c(min(y)-15, max(y)+15), xlab="x/input", ylab="y/output",main="Estimates of y=1/200*(x-25)^3+1/10*(x-25)^2")
  legend("topleft",inset=0.05, legend=c("True"), col=c("blue"), lty=1,cex=0.8)
  
  points(x,y,col="black",cex=1)
  
  
  legend("topleft",inset=0.05, legend=c("True"), col=c("blue"), lty=1,cex=0.8)
}

#Checking Assumptions

NoiseImpact(1000,5)

NoiseImpact(14,0) #zero noise
NoiseImpact(14,1)
NoiseImpact(14,5)
NoiseImpact(1400,5)
#For this model, The relationship doesn't do a good job of predicting points.  Why?

#######################Flexibility and Overfitting


Fits<- function(n,sd,high){
  lb<-0
  ub<-40

  set.seed(1)
  x<- runif(n, lb,ub)  #Input Data
  noise<- rnorm(n,mean=0,sd)
  y<- 1/200*(x-25)^3+1/10*(x-25)^2+noise #Randomly Generated Output Data

  par(mfrow=c(1,1))
  plot(x,y,col="black",cex=.2, xlim=c(0,40), ylim=c(min(y)-15, max(y)+15), xlab="x/input", ylab="y/output",main="Estimates of y=1/200*(x-25)^3+1/10*(x-25)^2")
  
  
  w<-seq(lb,ub,length=1000)
  z<-1/200*(w-25)^3+1/10*(w-25)^2  #True Function
  
  points(w,z, col="blue",type="l",lwd=2,lty=3)
  legend("topleft",inset=0.05, legend=c("True"), col=c("blue"), lty=3,cex=0.8)
  

  fit<- lm(y~x)           #linear model of y vs x
  new<- data.frame(x=w)
  z <- predict(fit, new)
  points(w,z,col="green",type="l",lwd=2)
  legend("topleft",inset=0.05, legend=c("True","Linear"), col=c("blue","green"), lty=c(3,1),cex=0.8)
  
  
  fitQuadratic<- lm(y ~ poly(x,2))  #linear model of y vs x+x^2
  new<- data.frame(x=w)
  z <- predict(fitQuadratic, new)
  points(w,z,col="orange",type="l",lwd=2)
  legend("topleft",inset=0.05, legend=c("True","Linear", "Quadratic"), col=c("blue","green","orange"), lty=c(3,1,1),cex=0.8)
  

  
  fitCubic<- lm(y ~ poly(x,3)) #linear model of y vs x+x^2+x^3
  new<- data.frame(x=w)
  z <- predict(fitCubic, new)
  points(w,z,col="purple",type="l",lwd=2)
  legend("topleft",inset=0.05, legend=c("True","Linear", "Quadratic", "Cubic"), col=c("blue","green","orange", "purple"), lty=c(3,1,1,1),cex=0.8)

  
  
  if(high==1){
    fithigh<- lm(y ~ poly(x,10))
    new<- data.frame(x=w)
    z <- predict(fithigh, new)
    points(w,z,col="red",type="l",lwd=1.5)
    legend("topleft",inset=0.05, legend=c("True","Linear", "Quadratic", "Cubic","10th degree"), col=c("blue","green","orange", "purple","red"), lty=c(3,1,1,1,1),cex=0.8)
  }  
}

Fits(14,5,0)
Fits(100,5,0)
Fits(100,10,0)
Fits(100,100,0)
Fits(100,5,0)
Fits(100,5,0)
Fits(14,5,0)
Fits(14,100,0)

#in the last case, even the true model looks bad.  Why?

#Higher Degree:

Fits(14,5,1)






####### Evaluating Quality of the Model

Fits(14,0,1)
##which model is best?
Fits(14,1,1)
##which model is best?
Fits(14,5,1)
##which model is best?
Fits(14,100,1)
##which model is best?


NewPoints<- function(n,sd,high,numbernew){
  set.seed(1)
  lb<-0
  ub<-40
  
  x<- runif(n, lb,ub)
  noise<- rnorm(n)
  y<- 1/200*(x-25)^3+1/10*(x-25)^2+sd*noise
  par(mfrow=c(1,1))
  
  w<-seq(lb,ub,length=1000)
  z<-1/200*(w-25)^3+1/10*(w-25)^2
  plot(w,z,col="blue",type="l",lwd=2, xlim=c(0,40), ylim=c(min(y)-15, max(y)+15), xlab="x/input", ylab="y/output",main="Estimates of y=1/200*(x-25)^3+1/10*(x-25)^2",lty=1)
  
  points(x,y, col="black", cex=1.5)
  
  
  
  fit<- lm(y~x)           #linear model of y vs x
  abline(fit,col="green")
  
  
  fitQuadratic<- lm(y ~ poly(x,2))  #linear model of y vs x+x^2
  summary(fitQuadratic)
  fitQuadratic$coefficients
  
  new<- data.frame(x=w)  #places w (our old vector we used to draw blue line) into same data format as x -- our input data)
  z <- predict(fitQuadratic, new)  #predicts the output for the vector w
  points(w,z,col="orange",type="l",lwd=2)

  fitCubic<- lm(y ~ poly(x,3)) #linear model of y vs x+x^2+x^3
  new<- data.frame(x=w)
  z <- predict(fitCubic, new)
  points(w,z,col="purple",type="l",lwd=2)
  
  
  
  if(high==1){
    fithigh<- lm(y ~ poly(x,10))
    new<- data.frame(x=w)
    z <- predict(fithigh, new)
    points(w,z,col="red",type="l",lwd=1.5)
  }  
  
  x<- sample(lb:ub, numbernew, replace=TRUE)
  noise<- rnorm(numbernew)
  y<- 1/200*(x-25)^3+1/10*(x-25)^2+sd*noise
  points(x,y, cex=1,pch=16)
  
  if(high==0){
    legend("topleft",inset=0.05, legend=c("True","Linear", "Quadratic", "Cubic"), col=c("blue","green","orange", "purple"), lty=c(3,1,1,1),cex=0.8)
  }
  if(high==1){
    legend("topleft",inset=0.05, legend=c("True","Linear", "Quadratic", "Cubic","10th degree"), col=c("blue","green","orange", "purple","red"), lty=c(3,1,1,1,1),cex=0.8)
  }  
}




###What is more important?  Accuracy of data that we 
###trained the model on or the accuracy of future data?

NewPoints(14,0,1,50)
NewPoints(14,1,1,50)
NewPoints(14,5,1,50)
NewPoints(14,100,1,50)




#######################Variance of Models


##our sampling process is random implying our model itself is random

VarianceFits<- function(n,sd,high,pointsize){
  lb<-0
  ub<-40
  
  #set.seed(1)
  
  x<-  runif(n, lb,ub) #Input Data
  noise<- rnorm(n,mean=0,sd)
  y<- 1/200*(x-25)^3+1/10*(x-25)^2+noise #Randomly Generated Output Data
  
  par(mfrow=c(1,1))
  plot(x,y,col="black",cex=pointsize, xlim=c(0,40),ylim=c(-20,50), xlab="x/input", ylab="y/output",main="Estimates of y=1/200*(x-25)^3+1/10*(x-25)^2")
  
  
  w<-seq(lb,ub,length=1000)
  z<-1/200*(w-25)^3+1/10*(w-25)^2  #True Function
  
  points(w,z, col="blue",type="l",lwd=2,lty=3)
  legend("topleft",inset=0.05, legend=c("True"), col=c("blue"), lty=3,cex=0.8)
  
  
  fit<- lm(y~x)           #linear model of y vs x
  abline(fit,col="green",lwd=3)
  legend("topleft",inset=0.05, legend=c("True","Linear"), col=c("blue","green"), lty=c(3,1),cex=0.8)
  
  
  fitQuadratic<- lm(y ~ poly(x,2))  #linear model of y vs x+x^2
  new<- data.frame(x=w)
  z <- predict(fitQuadratic, new)
  points(w,z,col="orange",type="l",lwd=2)
  legend("topleft",inset=0.05, legend=c("True","Linear", "Quadratic"), col=c("blue","green","orange"), lty=c(3,1,1),cex=0.8)
  
  
  
  fitCubic<- lm(y ~ poly(x,3)) #linear model of y vs x+x^2+x^3
  new<- data.frame(x=w)
  z <- predict(fitCubic, new)
  points(w,z,col="purple",type="l",lwd=2)
  legend("topleft",inset=0.05, legend=c("True","Linear", "Quadratic", "Cubic"), col=c("blue","green","orange", "purple"), lty=c(3,1,1,1),cex=0.8)
  
  
  
  if(high==1){
    fithigh<- lm(y ~ poly(x,10))
    new<- data.frame(x=w)
    z <- predict(fithigh, new)
    points(w,z,col="red",type="l",lwd=1.5)
    legend("topleft",inset=0.05, legend=c("True","Linear", "Quadratic", "Cubic","10th degree"), col=c("blue","green","orange", "purple","red"), lty=c(3,1,1,1,1),cex=0.8)
  }  
}




VarianceFits(14,10,0,1)
VarianceFits(14,10,0,1)
VarianceFits(14,10,0,1)
VarianceFits(14,10,0,1)
VarianceFits(14,10,0,1)

#Notice we fixed the scale of the plot so the true function doesn't move

#Remarks for later on sample size:

VarianceFits(100,10,0,0.5)
VarianceFits(100,10,0,0.5)
VarianceFits(100,10,0,0.5)
VarianceFits(100,10,0,0.5)
VarianceFits(100,10,0,0.5)
#Remarks for later on sample size:

VarianceFits(1000,10,0,0.3)
VarianceFits(1000,10,0,0.3)
VarianceFits(1000,10,0,0.3)
VarianceFits(1000,10,0,0.3)
VarianceFits(1000,10,0,0.3)


#Observation on Flexibility and Sample size:
VarianceFits(14,10,1,0.7)
VarianceFits(14,10,1,0.7)
VarianceFits(1000,10,1,0.3)
VarianceFits(1000,10,1,0.3)



###########################


