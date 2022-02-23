library(ISLR)
library(FNN)
library(leaps)
library(car)
library("readxl")
Snails <- as.data.frame(read_excel("Snails.xlsx"))
TestSnails <-  as.data.frame(read_excel("TestSnails.xlsx"))

fit<- lm(Rings~Type+Diameter+Height+WholeWeight+ShuckedWeight+
           ShellWeight+Type:Diameter+Type:Height+Type:WholeWeight+Type:ShuckedWeight+
           +LongestShell:VisceraWeight+Diameter:ShellWeight+Height:WholeWeight+
           Height:ShuckedWeight+WholeWeight:ShuckedWeight+WholeWeight:VisceraWeight+
           ShuckedWeight:VisceraWeight+ShuckedWeight:ShellWeight+VisceraWeight:ShellWeight,
         data = Snails)

#computes testing R squared for the data set
#TestSnails
test.pred <- predict(fit, newdata = TestSnails)
test.RSS <- sum((test.pred - TestSnails[,"Rings"])^2)
test.TSS <- sum((mean(TestSnails[,"Rings"]) - TestSnails[,"Rings"])^2)
test.R.Squared<- 1-test.RSS/test.TSS
test.R.Squared



             
             