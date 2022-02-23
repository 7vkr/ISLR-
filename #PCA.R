#PCA

library(leaps) #for best subset selection
library(ISLR)
library(FNN)
library("readxl")
mydata <- read_excel("F:\\Fall2021\\ISEN 613\\HW\\HW 5\\Snails.xlsx")
head(mydata)
names(mydata)
dim(mydata)


names(pr.out1)
pr.out1$scale
pr.out1$center
pr.out1$rotation
biplot (pr.out1 , scale =0)
pr.out1$rotation=-pr.out1$rotation
pr.out1$x=-pr.out1$x
biplot (pr.out1 , scale =0)
pr.out1$sdev
pr.var1 = pr.out1$sdev^2
pve1=pr.var1/sum(pr.var1)
plot(pve1 , xlab=" Principal Component ", ylab="Proportion of
Variance Explained ", ylim=c(0,1),type='b')
plot(cumsum(pve1), xlab="Principal Component ", ylab="
Cumulative Proportion of Variance Explained ", ylim=c(0,1),
     type='b')


