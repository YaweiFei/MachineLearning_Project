##function to install packages
rm(list=ls())
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    library(thispackage, character.only = T)
  }
}
#load required packages
needed  <-  c("ISLR","curl", "MASS", "car","imputeTS","corrplot","class","pROC","verification",
              "leaps", "ISLR", "glmnet", "glmulti")      
installIfAbsentAndLoad(needed)

#load data
data<-read.csv(file = "UsingThis.csv",header = TRUE)

#replace missing data with median of that state
statecodes<-c(unique(data[4]))
for (j in statecodes) {
  for (i in 8:ncol(data)) {
    data[data[4]==j & is.na(data[i]),i]<-median(data[data[4]==j & !is.na(data[i]),i])
  }
}
#replace remaining missing data with median of entire column
for (k in 8:ncol(data)) {
  data[is.na(data[,k]),k] <- median(data[,k],na.rm = TRUE)
}
#change urban area to factor and county pop to numeric
data$Urban.Area<-as.factor(data$Urban.Area)
data$County.Population.in.2000<-as.numeric(data$County.Population.in.2000)

#store old data
origdata<-data
str(origdata)
regressdata<-data[,-(1:6)]
#use lm to test for VIF bc VIF only looks at relationships between the predictors
lm<-lm(origdata$Total.Crime.Rate ~ ., data = regressdata)
VIF<-vif(lm)
VIF[VIF>5]
#pov rate, mean household income, fraction of children w single mother have vifs greater than 5, mean household is the highest so is removed
regressdata2<-regressdata[,-(19)]
lm<-lm(origdata$Total.Crime.Rate ~ ., data = regressdata2)
VIF<-vif(lm)
VIF[VIF>5]
#fraction of children with single mother has vif greater than 5
str(regressdata2)
regressdata3<-regressdata2[,(-25)]
lm<-lm(origdata$Total.Crime.Rate ~ ., data = regressdata3)
VIF<-vif(lm)
VIF[VIF>5]
#no more predictors have vifs over 5, create xs and y for log regression 
x<-regressdata3
y<-origdata$Total.Crime.Rate
##build a linear regression model with all the remaining predictors
lm<-lm(y~.,data=regressdata3)
par(mfrow=c(2,2))
plot(lm)
#get rid of high leverage point 
regressdata4<-regressdata3[-c(81),]
y<-origdata[-c(81),6]
lm1<-lm(y~.,data=regressdata4)
plot(lm1)
##after plotting model, we see that the relationship between the predictors and total crime rate is not linear so we take the log of total crime rate and all the variables
origmod<-lm(log(y)~.,data=regressdata4)
plot(origmod)
#######choosing the model/variables
###################Full selection########################################
regfit.full <- regsubsets(log(y)~.,data=regressdata4, nvmax=26)
(reg.summary <- summary(regfit.full))
(min <- which.min(reg.summary$bic))
par(mfrow=c(1, 1))
plot(reg.summary$bic, 
     xlab="Number of Variables", 
     ylab="BIC", 
     type='l')
points(min, reg.summary$bic[min], 
       col="red", 
       cex=2, 
       pch=20)
coef(regfit.full, min)
#minimum bic value for the reg full selection
reg.summary$bic[min]
#######################forward subset selection###########################
regfit.fwd <- regsubsets(log(y)~.,data=regressdata4, nvmax=26, method="forward")
(fwdsumm<-summary(regfit.fwd))
(min <- which.min(fwdsumm$bic))
plot(fwdsumm$bic, 
     xlab="Number of Variables", 
     ylab="BIC", 
     type='l')
points(min, fwdsumm$bic[min], 
       col="red", 
       cex=2, 
       pch=20)
#minimum bic value for the reg forward selection
fwdsumm$bic[min]
coef(regfit.fwd, min)
#########################backword selection###############################
regfit.bwd <- regsubsets(log(y)~.,data=regressdata4, nvmax=26,method="backward")
(bcksumm<-summary(regfit.bwd))
(min <- which.min(bcksumm$bic))
plot(bcksumm$bic, 
     xlab="Number of Variables", 
     ylab="BIC", 
     type='l')
points(min, bcksumm$bic[min], 
       col="red", 
       cex=2, 
       pch=20)
bcksumm$bic[min]
coef(regfit.bwd, min)
### Our Best model#####
bestregressdata<-regressdata4[,c('Income.Segregation','Poverty.Rate','Percent.Black','Unemployment..Rate','Percent.Foreign.Born'
,'High.School.Dropout.Rate..Income.Adjusted.','Percent.Uninsured','Gini.Index.Within.Bottom.99.','Top.1..Income.Share',
'Percent.Hispanic','Labor.Force.Participation','Student.Teacher.Ratio','Local.Government.Expenditures')]
bestmodel<-lm(log(y)~.,data=bestregressdata)
par(mfrow=c(2,2))
plot(bestmodel)
################Predict total crime rate for Florida and Wisconsin ##################################
################where No total crime rate report in our given dataset#################################
flor<-read.csv(file = "Florida.csv",header = TRUE)
wisc<-read.csv(file = "wisconsin.csv",header = TRUE)
for (i in 3:ncol(flor)) {
  flor[is.na(flor[i]),i]<-median(flor[!is.na(flor[i]),i])
}
for (i in 3:ncol(wisc)) {
  wisc[is.na(wisc[i]),i]<-median(wisc[!is.na(wisc[i]),i])
}

flor$Urban.Area<-as.factor(flor$Urban.Area)
flor$County.Population.in.2000<-as.numeric(flor$County.Population.in.2000)
wisc$Urban.Area<-as.factor(wisc$Urban.Area)
wisc$County.Population.in.2000<-as.numeric(wisc$County.Population.in.2000)

#drop mean household and single mother variables
flor<-flor[,-c(20,27)]
wisc<-wisc[,-c(20,27)]

#use 13 variables found from subset selection
flor$crimerate <- exp(-8.578346e+00 +
                     flor$Percent.Uninsured * 9.040430e-01 + 
                     flor$Income.Segregation * 3.141514e+00 +
                     flor$Gini.Index.Within.Bottom.99. * 3.047219e+00 +
                     flor$Poverty.Rate * -1.503533e+00 +
                     flor$Top.1..Income.Share * -2.637139e+00 +
                     flor$Percent.Black * 7.425082e-01 +
                     flor$Percent.Hispanic * 7.426740e-01 +
                     flor$Unemployment..Rate * 6.588352e+00 +
                     flor$Labor.Force.Participation * 1.417780e+00 +
                     flor$Percent.Foreign.Born * -1.272379e+00 +
                     flor$Student.Teacher.Ratio * 5.719114e-02 +
                     flor$High.School.Dropout.Rate..Income.Adjusted. * 2.505816e+00 +
                     flor$Local.Government.Expenditures * 4.633905e-05)
flor$crimerate
wisc$crimerate <- exp(-8.578346e+00 +
                        wisc$Percent.Uninsured * 9.040430e-01 + 
                        wisc$Income.Segregation * 3.141514e+00 +
                        wisc$Gini.Index.Within.Bottom.99. * 3.047219e+00 +
                        wisc$Poverty.Rate * -1.503533e+00 +
                        wisc$Top.1..Income.Share * -2.637139e+00 +
                        wisc$Percent.Black * 7.425082e-01 +
                        wisc$Percent.Hispanic * 7.426740e-01 +
                        wisc$Unemployment..Rate * 6.588352e+00 +
                        wisc$Labor.Force.Participation * 1.417780e+00 +
                        wisc$Percent.Foreign.Born * -1.272379e+00 +
                        wisc$Student.Teacher.Ratio * 5.719114e-02 +
                        wisc$High.School.Dropout.Rate..Income.Adjusted. * 2.505816e+00 +
                        wisc$Local.Government.Expenditures * 4.633905e-05)
wisc$crimerate
####Confidence Interval and prediction interval#############
dfflor<-data.frame(flor[,c('Percent.Uninsured','Income.Segregation','Gini.Index.Within.Bottom.99.','Poverty.Rate',
                           'Top.1..Income.Share','Percent.Black', 'Percent.Hispanic','Unemployment..Rate','Labor.Force.Participation',
                           'Percent.Foreign.Born', 'Student.Teacher.Ratio','High.School.Dropout.Rate..Income.Adjusted.',
                           'Local.Government.Expenditures')])
b<-predict(bestmodel, dfflor, interval="prediction")
c<-predict(bestmodel, dfflor, interval="confidence") 
(PI<-exp(b)) #prediction interval
(CI<-exp(c))  #confidence interval

## test R square
reg.summary <- summary(regfit.full)
reg.summary$adjr2[13]
