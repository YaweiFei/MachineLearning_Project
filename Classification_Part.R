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

#set directory and load data
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
#to classify counties as either safe or unsafe, 1 is unsafe, 0 is safe
hist(data$Total.Crime.Rate,breaks = 20) #take a look how the total crime rate is distributed
mean(data$Total.Crime.Rate)
quantile(data$Total.Crime.Rate)
data$crimecat<-as.factor(with(
  ifelse(Total.Crime.Rate>0.003369659,'1','0'),
  data=data))

#store old data
origdata<-data
str(origdata)
regressdata<-data[,-(1:6)]
regressdata<-regressdata[,-(29)]
#use lm to test for VIF bc VIF only looks at relationships between the predictors
lm<-lm(origdata$Total.Crime.Rate ~ ., data = regressdata)
VIF<-vif(lm)
VIF[VIF>5]
#pov rate, mean household income, fractino of children w single mother have vifs greater than 5, mean household is the highest so is removed
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
y<-origdata$crimecat

#run logistical regression with all the variables
glm<-glm(y ~ ., family = binomial, data = x)

#plot to see high leverage points
par(mfrow=c(2,2))
plot(glm)
#cooks distance is not greater than 1 for any observation

#find number of predictors
ncol(x)
num.predictors <- 26
#combine xs and ys
mydf <- data.frame(x, "crimecat"= y) 
#how many models we want to keep
num.to.keep <- 5

#genetic model selection
#run subset selection using aic to select
glmulti.glm.out <- glmulti(mydf$crimecat ~ .,
                           data=mydf,
                           method="g",
                           crit = "aic",
                           confsetsize = num.to.keep,
                           fitfunction = "glm",
                           family = binomial,
                           level = 1)                           
glmulti.summary.aic <- summary(glmulti.glm.out)
glmulti.summary.aic$bestmodel
glmulti.summary.aic$icvalues
plot(glmulti.summary.aic$icvalues, 
       type='b', 
       xlab='Item',
       ylab='AIC',
       main='AIC for Candidate Set')

#run subset selection using bic to select
glmulti.glm.out.bic <- glmulti(mydf$crimecat ~ .,
                           data=mydf,
                           method="g",
                           crit = "bic",
                           confsetsize = num.to.keep,
                           fitfunction = "glm",
                           family = binomial,
                           level = 1)                           
glmulti.summary.bic <- summary(glmulti.glm.out.bic)
glmulti.summary.bic$bestmodel
glmulti.summary.bic$icvalues
plot(glmulti.summary.bic$icvalues, 
       type='b', 
       xlab='Item',
       ylab='BIC',
       main='BIC for Candidate Set')
#make train and test sets
set.seed(5072)
trainprop<-.8
n<-nrow(mydf)
train<-sample(n,trainprop*n)
test<-setdiff(1:n,train)
trainset<-(mydf[train,])
testset<-(mydf[test,])
#run logistical regression using subset selection models
#aic model
glmulti.summary.aic$bestmodel
glm.aic<-glm(crimecat ~ 1 + County.Population.in.2000 + Percent.Uninsured +              
Income.Segregation + Gini.Index.Within.Bottom.99. + Poverty.Rate +           
Top.1..Income.Share + Percent.Religious + Percent.Black +                    
Percent.Hispanic + Unemployment..Rate + Labor.Force.Participation +          
Share.Working.in.Manufacturing + Percent.Foreign.Born + Population.Density + 
School.Expenditure.per.Student + Student.Teacher.Ratio +                     
High.School.Dropout.Rate..Income.Adjusted. + Local.Government.Expenditures + 
Local.Tax.Rate, family = binomial, data = trainset)

glm.aic.prob<-predict(glm.aic,testset,type = "response")
predicted<-rep('0',nrow(testset))
predicted[glm.aic.prob>.5]<-'1'
actuals<-testset$crimecat
glmaictable<-table(actuals,predicted)
glmaictable

glm.aic.prob<-predict(glm.aic,testset,type = "response")
predicted<-rep('0',nrow(testset))
predicted[glm.aic.prob>.4]<-'1'
actuals<-testset$crimecat
glmaictable<-table(actuals,predicted)
glmaictable

errorrateglmaic<-(glmaictable[1,2]+glmaictable[2,1])/sum(glmaictable)
correctglmaic<-1-errorrateglmaic
type1errglmaic <- glmaictable[1,2]/sum(glmaictable[1,])
type2errglmaic <- glmaictable[2,1]/sum(glmaictable[2,])
powerglmaic <- glmaictable[2,2]/sum(glmaictable[2,])
precisionglmaic <- glmaictable[2,2]/sum(glmaictable[,2])
auccglmaic <- roc.area(as.numeric(actuals)-1,glm.aic.prob)$A
print(paste('The AOC is ',auccglmaic))
roc.plot(as.integer(as.numeric(actuals))-1,glm.aic.prob, 
         main="ROC Curve")
#bic model
glmulti.summary.bic$bestmodel
glm.bic<-glm(crimecat ~ 1 + County.Population.in.2000 + Gini.Index.Within.Bottom.99. + 
            Top.1..Income.Share + Percent.Black + Percent.Hispanic +                   
            Unemployment..Rate + Labor.Force.Participation + Student.Teacher.Ratio +   
            Local.Government.Expenditures + Local.Tax.Rate , family =
             binomial, data = trainset)
glm.bic.prob<-predict(glm.bic,testset,type = "response")
predicted<-rep('0',nrow(testset))
predicted[glm.bic.prob>.5]<-'1'
actuals<-testset$crimecat
glmbictable<-table(actuals,predicted)
glmbictable

glm.bic.prob<-predict(glm.bic,testset,type = "response")
predicted<-rep('0',nrow(testset))
predicted[glm.bic.prob>.4]<-'1'
actuals<-testset$crimecat
glmbictable<-table(actuals,predicted)
glmbictable

glmbicerrorrate<-(glmbictable[1,2]+glmbictable[2,1])/sum(glmbictable)
glmbiccorrect<-1-glmbicerrorrate
glmbictype1err <- glmbictable[1,2]/sum(glmbictable[1,])
glmbictype2err <- glmbictable[2,1]/sum(glmbictable[2,])
glmbicpower <- glmbictable[2,2]/sum(glmbictable[2,])
glmbicprecision <- glmbictable[2,2]/sum(glmbictable[,2])
glmbicaucc <- roc.area(as.numeric(actuals)-1,glm.bic.prob)$A
print(paste('The AOC is ',glmbicaucc))
roc.plot(as.integer(as.numeric(actuals))-1,glm.bic.prob, 
         main="ROC Curve")

#lda
#aic model
lda.aic<-lda(crimecat ~ 1 + County.Population.in.2000 + Percent.Uninsured +              
               Income.Segregation + Gini.Index.Within.Bottom.99. + Poverty.Rate +           
               Top.1..Income.Share + Percent.Religious + Percent.Black +                    
               Percent.Hispanic + Unemployment..Rate + Labor.Force.Participation +          
               Share.Working.in.Manufacturing + Percent.Foreign.Born + Population.Density + 
               School.Expenditure.per.Student + Student.Teacher.Ratio +                     
               High.School.Dropout.Rate..Income.Adjusted. + Local.Government.Expenditures + 
               Local.Tax.Rate, data = trainset)

lda.aic.prob<-predict(lda.aic,testset,type = "response")
predicted<-rep('0',nrow(testset))
predicted[lda.aic.prob$posterior[,2]>.5]<-'1'
actuals<-testset$crimecat
ldaaictable<-table(actuals,predicted)
ldaaictable

lda.aic.prob<-predict(lda.aic,testset,type = "response")
predicted<-rep('0',nrow(testset))
predicted[lda.aic.prob$posterior[,2]>.4]<-'1'
actuals<-testset$crimecat
ldaaictable<-table(actuals,predicted)
ldaaictable

ldaaicerrorrate<-(ldaaictable[1,2]+ldaaictable[2,1])/sum(ldaaictable)
ldaaiccorrect<-1-ldaaicerrorrate
ldaaictype1err <- ldaaictable[1,2]/sum(ldaaictable[1,])
ldaaictype2err <- ldaaictable[2,1]/sum(ldaaictable[2,])
ldaaicpower <- ldaaictable[2,2]/sum(ldaaictable[2,])
ldaaicprecision <- ldaaictable[2,2]/sum(ldaaictable[,2])
ldaaicaucc <- roc.area(as.numeric(actuals)-1,lda.aic.prob$posterior[,2])$A
print(paste('The AOC is ',ldaaicaucc))
roc.plot(as.integer(as.numeric(actuals))-1,lda.aic.prob$posterior[,2], 
         main="ROC Curve")
#bic model
lda.bic<-lda(crimecat ~ 1 + County.Population.in.2000 + Gini.Index.Within.Bottom.99. + 
               Top.1..Income.Share + Percent.Black + Percent.Hispanic +                   
               Unemployment..Rate + Labor.Force.Participation + Student.Teacher.Ratio +   
               Local.Government.Expenditures + Local.Tax.Rate , data = trainset)
lda.bic.prob<-predict(lda.bic,testset,type = "response")
predicted<-rep('0',nrow(testset))
predicted[lda.bic.prob$posterior[,2]>.5]<-'1'
actuals<-testset$crimecat
ldabictable<-table(actuals,predicted)
ldabictable

lda.bic.prob<-predict(lda.bic,testset,type = "response")
predicted<-rep('0',nrow(testset))
predicted[lda.bic.prob$posterior[,2]>.4]<-'1'
actuals<-testset$crimecat
ldabictable<-table(actuals,predicted)
ldabictable

ldabicerrorrate<-(ldabictable[1,2]+ldabictable[2,1])/sum(ldabictable)
ldabiccorrect<-1-ldabicerrorrate
ldabictype1err <- ldabictable[1,2]/sum(ldabictable[1,])
ldabictype2err <- ldabictable[2,1]/sum(ldabictable[2,])
ldabicpower <- ldabictable[2,2]/sum(ldabictable[2,])
ldabicprecision <- ldabictable[2,2]/sum(ldabictable[,2])
ldabicaucc <- roc.area(as.numeric(actuals)-1,lda.bic.prob$posterior[,2])$A
print(paste('The AOC is ',ldabicaucc))
roc.plot(as.integer(as.numeric(actuals))-1,lda.bic.prob$posterior[,2], 
         main="ROC Curve")
#qda
#aic model
qda.aic<-qda(crimecat ~ 1 + County.Population.in.2000 + Percent.Uninsured +              
               Income.Segregation + Gini.Index.Within.Bottom.99. + Poverty.Rate +           
               Top.1..Income.Share + Percent.Religious + Percent.Black +                    
               Percent.Hispanic + Unemployment..Rate + Labor.Force.Participation +          
               Share.Working.in.Manufacturing + Percent.Foreign.Born + Population.Density + 
               School.Expenditure.per.Student + Student.Teacher.Ratio +                     
               High.School.Dropout.Rate..Income.Adjusted. + Local.Government.Expenditures + 
               Local.Tax.Rate, data = trainset)

qda.aic.prob<-predict(qda.aic,testset,type = "response")
predicted<-rep('0',nrow(testset))
predicted[qda.aic.prob$posterior[,2]>.5]<-'1'
actuals<-testset$crimecat
qdaaictable<-table(actuals,predicted)
qdaaictable

qda.aic.prob<-predict(qda.aic,testset,type = "response")
predicted<-rep('0',nrow(testset))
predicted[qda.aic.prob$posterior[,2]>.4]<-'1'
actuals<-testset$crimecat
qdaaictable<-table(actuals,predicted)
qdaaictable

qdaaicerrorrate<-(qdaaictable[1,2]+qdaaictable[2,1])/sum(qdaaictable)
qdaaiccorrect<-1-qdaaicerrorrate
qdaaictype1err <- qdaaictable[1,2]/sum(qdaaictable[1,])
qdaaictype2err <- qdaaictable[2,1]/(qdaaictable[2,1]+qdaaictable[2,2])
qdaaicpower <- qdaaictable[2,2]/sum(qdaaictable[2,])
qdaaicprecision <- qdaaictable[2,2]/sum(qdaaictable[,2])
qdaaicaucc <- roc.area(as.numeric(actuals)-1,qda.aic.prob$posterior[,2])$A
print(paste('The AOC is ',qdaaicaucc))
roc.plot(as.integer(as.numeric(actuals))-1,qda.aic.prob$posterior[,2], 
         main="ROC Curve")
#bic model
qda.bic<-qda(crimecat ~ 1 + County.Population.in.2000 + Gini.Index.Within.Bottom.99. + 
               Top.1..Income.Share + Percent.Black + Percent.Hispanic +                   
               Unemployment..Rate + Labor.Force.Participation + Student.Teacher.Ratio +   
               Local.Government.Expenditures + Local.Tax.Rate , data = trainset)
qda.bic.prob<-predict(qda.bic,testset,type = "response")
predicted<-rep('0',nrow(testset))
predicted[qda.bic.prob$posterior[,2]>.5]<-'1'
actuals<-testset$crimecat
qdabictable<-table(actuals,predicted)
qdabictable

qda.bic.prob<-predict(qda.bic,testset,type = "response")
predicted<-rep('0',nrow(testset))
predicted[qda.bic.prob$posterior[,2]>.4]<-'1'
actuals<-testset$crimecat
qdabictable<-table(actuals,predicted)
qdabictable

qdabicerrorrate<-(qdabictable[1,2]+qdabictable[2,1])/sum(qdabictable)
qdabiccorrect<-1-qdabicerrorrate
qdabictype1err <- qdabictable[1,2]/sum(qdabictable[1,])
qdabictype2err <- qdabictable[2,1]/sum(qdabictable[2,])
qdabicpower <- qdabictable[2,2]/sum(qdabictable[2,])
qdabicprecision <- qdabictable[2,2]/sum(qdabictable[,2])
qdabicaucc <- roc.area(as.numeric(actuals)-1,qda.bic.prob$posterior[,2])$A
print(paste('The AOC is ',qdabicaucc))
roc.plot(as.integer(as.numeric(actuals))-1,qda.bic.prob$posterior[,2], 
         main="ROC Curve")
#knn
scaled.mydf<-scale(mydf[,c('County.Population.in.2000','Percent.Uninsured','Income.Segregation',
                           'Racial.Segregation','Gini.Index.Within.Bottom.99.','Poverty.Rate',
                           'Top.1..Income.Share','Percent.Religious','Percent.Black','Percent.Hispanic',
                           'Unemployment..Rate','Labor.Force.Participation','Share.Working.in.Manufacturing',
                           'Percent.Foreign.Born','Migration.Inflow..Rate','Migration.Outflow..Rate',
                           'Population.Density','Median..House.Value','School.Expenditure.per.Student',
                           'Student.Teacher.Ratio','Test.Score.Percentile..Income.Adjusted.',
                           'High.School.Dropout.Rate..Income.Adjusted.','Percent..College.Grads',
                           'Local.Government.Expenditures','Local.Tax.Rate')])
y<-mydf$crimecat
train.x <- scaled.mydf[train,]
train.y<-y[train]
test.x<-scaled.mydf[-train,]
errorrate<-c()
for (k in seq(1,99,1)){
  knn.pred<-knn(train.x,test.x,train.y,k=k)
  knncm<-table(testset$crimecat,knn.pred)
  errorrate[k]<-(knncm[1,2]+knncm[2,1])/sum(knncm)
}
bestk<-which(errorrate==min(errorrate))
bestk

knn.pred<-knn(train.x,test.x,train.y,k=bestk)
knntable<-table(testset$crimecat,knn.pred)
knnerrorrate<-(knntable[1,2]+knntable[2,1])/sum(knntable)
knncorrect<-1-knnerrorrate
knntype1err <- knntable[1,2]/sum(knntable[1,])
knntype2err <- knntable[2,1]/sum(knntable[2,])
knnpower <- knntable[2,2]/sum(knntable[2,])
knnprecision <- knntable[2,2]/sum(knntable[,2])

auccknn <- roc.area(as.numeric(testset$crimecat)-1,as.numeric(knn.pred))$A
print(paste('The AUC is ',auccknn))

#alltables
glmaictable
glmbictable
ldaaictable
ldabictable
qdaaictable
qdabictable
knntable

#consolidated into single table

findings<- matrix(c(errorrateglmaic, correctglmaic, type1errglmaic, type2errglmaic, powerglmaic, precisionglmaic, auccglmaic,
                    glmbicerrorrate, glmbiccorrect, glmbictype1err, glmbictype2err, glmbicpower, glmbicprecision, glmbicaucc,
                    ldaaicerrorrate, ldaaiccorrect, ldaaictype1err, ldaaictype2err, ldaaicpower, ldaaicprecision, ldaaicaucc,
                    ldabicerrorrate, ldabiccorrect, ldabictype1err, ldabictype2err, ldabicpower, ldabicprecision, ldabicaucc,
                    qdaaicerrorrate, qdaaiccorrect, qdaaictype1err, qdaaictype2err, qdaaicpower, qdaaicprecision, qdaaicaucc,
                    qdabicerrorrate, qdabiccorrect, qdabictype1err, qdabictype2err, qdabicpower, qdabicprecision, qdabicaucc,
                    knnerrorrate, knncorrect, knntype1err, knntype2err, knnpower, knnprecision, auccknn),nrow = 7,ncol = 7,byrow = FALSE)
colnames(findings) <- c("logreg aic", "logreg bic", "lda aic", "lda bic", "qda aic", "qda bic", "knn")
rownames(findings) <- c("error rate", "correct rate", "type 1", "type 2", "power", "precision", "aucc")
findings <- as.table(findings)
findings

#states with no crimerate reported, same cleaning steps  with filling in missing data and transforming necessary column types
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

#make prediction for wisconsin data
crimcatglmaic <- predict(glm.aic,newdata = wisc,type = "response")
wisc$crimecatglmaic<-rep('0',nrow(wisc))
wisc$crimecatglmaic[crimcatglmaic>.5]<-'1'

crimcatglmbic <- predict(glm.bic,newdata = wisc,type = "response")
wisc$crimecatglmbic<-rep('0',nrow(wisc))
wisc$crimecatglmbic[crimcatglmbic>.5]<-'1'

crimcatldaaic <-predict(lda.aic,newdata = wisc,type = "response")
wisc$crimecatldaaic<-rep('0',nrow(wisc))
wisc$crimecatldaaic[crimcatldaaic$posterior[,2]>.5]<-'1'

crimcatqdaaic <-predict(qda.aic,newdata = wisc,type = "response")
wisc$crimecatqdaaic<-rep('0',nrow(wisc))
wisc$crimecatqdaaic[crimcatqdaaic$posterior[,2]>.5]<-'1'

crimcatldabic <-predict(lda.bic,newdata = wisc,type = "response")
wisc$crimecatldabic<-rep('0',nrow(wisc))
wisc$crimecatldabic[crimcatldabic$posterior[,2]>.5]<-'1'

crimcatqdabic <-predict(qda.bic,newdata = wisc,type = "response")
wisc$crimecatqdabic<-rep('0',nrow(wisc))
wisc$crimecatqdabic[crimcatqdabic$posterior[,2]>.5]<-'1'

crimcatglmaic <- predict(glm.aic,newdata = wisc,type = "response")
wisc$crimecatglmaic<-rep('0',nrow(wisc))
wisc$crimecatglmaic[crimcatglmaic>.4]<-'1'

crimcatglmbic <- predict(glm.bic,newdata = wisc,type = "response")
wisc$crimecatglmbic<-rep('0',nrow(wisc))
wisc$crimecatglmbic[crimcatglmbic>.4]<-'1'

crimcatldaaic <-predict(lda.aic,newdata = wisc,type = "response")
wisc$crimecatldaaic<-rep('0',nrow(wisc))
wisc$crimecatldaaic[crimcatldaaic$posterior[,2]>.4]<-'1'

crimcatqdaaic <-predict(qda.aic,newdata = wisc,type = "response")
wisc$crimecatqdaaic<-rep('0',nrow(wisc))
wisc$crimecatqdaaic[crimcatqdaaic$posterior[,2]>.4]<-'1'

crimcatldabic <-predict(lda.bic,newdata = wisc,type = "response")
wisc$crimecatldabic<-rep('0',nrow(wisc))
wisc$crimecatldabic[crimcatldabic$posterior[,2]>.4]<-'1'

crimcatqdabic <-predict(qda.bic,newdata = wisc,type = "response")
wisc$crimecatqdabic<-rep('0',nrow(wisc))
wisc$crimecatqdabic[crimcatqdabic$posterior[,2]>.4]<-'1'

wiscknn <- wisc[,- c(1,2,28,29,30,31,32,33,34)]
knn.predwisc<-knn(train.x,wiscknn,train.y,k=bestk)
wisc$crimecatknn <- knn.predwisc

#make predictions for florida data
crimcatglmaic <- predict(glm.aic,newdata = flor,type = "response")
flor$crimecatglmaic<-rep('0',nrow(flor))
flor$crimecatglmaic[crimcatglmaic>.5]<-'1'

crimcatglmbic <- predict(glm.bic,newdata = flor,type = "response")
flor$crimecatglmbic<-rep('0',nrow(flor))
flor$crimecatglmbic[crimcatglmbic>.5]<-'1'

crimcatldaaic <-predict(lda.aic,newdata = flor,type = "response")
flor$crimecatldaaic<-rep('0',nrow(flor))
flor$crimecatldaaic[crimcatldaaic$posterior[,2]>.5]<-'1'

crimcatqdaaic <-predict(qda.aic,newdata = flor,type = "response")
flor$crimecatqdaaic<-rep('0',nrow(flor))
flor$crimecatqdaaic[crimcatqdaaic$posterior[,2]>.5]<-'1'

crimcatldabic <-predict(lda.bic,newdata = flor,type = "response")
flor$crimecatldabic<-rep('0',nrow(flor))
flor$crimecatldabic[crimcatldabic$posterior[,2]>.5]<-'1'

crimcatqdabic <-predict(qda.bic,newdata = flor,type = "response")
flor$crimecatqdabic<-rep('0',nrow(flor))
flor$crimecatqdabic[crimcatqdabic$posterior[,2]>.5]<-'1'

crimcatglmaic <- predict(glm.aic,newdata = flor,type = "response")
flor$crimecatglmaic<-rep('0',nrow(flor))
flor$crimecatglmaic[crimcatglmaic>.4]<-'1'

crimcatglmbic <- predict(glm.bic,newdata = flor,type = "response")
flor$crimecatglmbic<-rep('0',nrow(flor))
flor$crimecatglmbic[crimcatglmbic>.4]<-'1'

crimcatldaaic <-predict(lda.aic,newdata = flor,type = "response")
flor$crimecatldaaic<-rep('0',nrow(flor))
flor$crimecatldaaic[crimcatldaaic$posterior[,2]>.4]<-'1'

crimcatqdaaic <-predict(qda.aic,newdata = flor,type = "response")
flor$crimecatqdaaic<-rep('0',nrow(flor))
flor$crimecatqdaaic[crimcatqdaaic$posterior[,2]>.4]<-'1'

crimcatldabic <-predict(lda.bic,newdata = flor,type = "response")
flor$crimecatldabic<-rep('0',nrow(flor))
flor$crimecatldabic[crimcatldabic$posterior[,2]>.4]<-'1'

crimcatqdabic <-predict(qda.bic,newdata = flor,type = "response")
flor$crimecatqdabic<-rep('0',nrow(flor))
flor$crimecatqdabic[crimcatqdabic$posterior[,2]>.4]<-'1'


#remove unnecssary columns to conduct knn
florknn <- flor[, - c(1,2,28,29,30,31,32,33,34)]
knn.predflor<-knn(train.x,florknn,train.y,k=bestk)
flor$crimecatknn <- knn.predflor

#create data structures that only hold countyname and classification results
finalflor <- flor[,c(1,28,29,30,31,32,33,34)]
finalwisc <- wisc[,c(1,28,29,30,31,32,33,34)]
finalflor
finalwisc
