rm(list=ls())
#todo:
#1. Clean master matrix i.e. remove x44 (Done)
#2. Using best model from hypo 5, derive matrix55 from master matrix (done)
#3. Redo calibration and testing. And also cross (done)

mastercortable<-data.frame(cor(matA[,2:86],use="pairwise"))
mastercor<-data.frame(c=cor(matA[,2:86],use="pairwise")[2:85,1])
library(treemap)
mastercor<-cbind(mastercor,n=row.names(mastercor),a=abs(mastercor$c))
treemap(mastercor,title="Correlation of Output & Predictor in Reactor A",vSize ="a",vColor = "c",index= "n",type="manual", palette="RdYlBu")

plot(matA$Date,matA$x60,pch=18,cex=0.1,main="x60 Time Series",xlab="Date",ylab = "x60" )
lines(matA$Date,matA$x60)
axis(side=2, at=seq(1, 2, by=0.1))
### this R script is used to determine the prediction errors of the different linear models
### so that we can create the model comparison table

read1<-paste0("/Volumes/JORDAN 1/SUTD/SembCorp/Matrices/Master/MatrixA.csv")
matA<-read.csv(read1) ###big matrix
for (i in 1:ncol(matA.10)){
  sum<-sum(is.na(matA.10[,i]))
  print (c(colnames(matA.10[,i]),sum))
}
#hypothesis 6 (correlation >0.3)
matA.6<-matA[,c("Date","x94","x3","x5","x6","x12","x18","x20","x27","x29","x30","x40","x47","x51","x55","x56","x60","x73","x82")]
matA.6<-matA.6[complete.cases(matA.6),]
#hypothesis 7 (from 6 with lesser variables)
matA.7<-matA[,c("Date","x94","x3","x5","x6","x12","x19","x20","x27","x40","x47","x51","x55","x56","x73","x82")]
matA.7<-matA.7[complete.cases(matA.7),]
#hypothesis 8 (includes variables from hypo 5)
matA.8<-matA[,c("Date","x94","x3","x4","x5","x6","x12","x18","x20","x21","x23","x24","x27","x28","x29","x30","x40","x47","x51","x53","x54","x55","x56","x60","x73","x82")]
matA.8<-matA.8[complete.cases(matA.8),]

test<-matA[c("Date","x94","x1",'x2','x3','x4','x5','x6','x7','x8','x9','x10','x12','x17','x19','x20','x21','x22','x23','x24','x25','x26','x27','x28','x29','x30','x32','x35','x36','x37','x38','x39','x40','x41','x42','x45','x46','x47','x48','x49','x51','x52','x53','x54','x55','x56','x57','x58','x60','x61','x62','x64','x67','x70','x76','x85')]
test<-test[complete.cases(test),]

matA.55<-matA[,c("Date","x94","x3","x4","x12","x20","x23","x24","x28","x30","x32","x47","x53","x54","x56","x57","x58")]
matA.55<-matA.55[complete.cases(matA.55),]
matA.54<-matA[,c("Date","x94","x1","x3","x4","x6","x10","x12","x19","x20","x23","x24","x28","x30","x32","x41","x47","x49","x51","x53","x54","x56","x57","x58","x64","x76")]
matA.54<-matA.54[complete.cases(matA.54),]

matA.62<-matA[,c("Date","x94","x3","x5","x6","x12","x18","x19","x20","x27","x29","x30","x40","x47","x51","x55","x56","x60","x73","x82")]
matA.62<-matA.62[complete.cases(matA.62),]
#HIGH correlation as seen from pairwise correlation from treemap
matA.9<-matA[,c("Date","x94","x3","x5","x6","x12","x17","x18","x19","x20","x27","x29","x30","x40","x41",'x47',"x51","x55","x56","x60","x68","x71","x73","x82")]
matA.9<-matA.9[complete.cases(matA.9),]
#common vairables
matA.10<-matA[,c("Date","x94","x1","x4","x12","x19","x20","x23","x24","x28","x30","x32","x41","x47","x51","x53","x54","x56","x57","x58")]
matA.10<-matA.10[complete.cases(matA.10),]

#reformatting the date & extracting testing scheme
matA.10$Date<-as.POSIXct(strptime(matA.10$Date,"%d/%m/%Y"))
month<-list("01","11","01","11","01","11")
year<-list("2012","2012","2013","2013","2014","2014")
test<-mapply(function (month,year){
  data<- data.frame()
  data<-rbind(data,subset(matA.10, format.Date(Date, "%m")==month& format.Date(Date, "%Y")==year ))
  return (data)
},month,year)

#calibration
library(dplyr)
testA.10<-rbind(data.frame(test[,1]),data.frame(test[,2]),data.frame(test[,3]),data.frame(test[,4]),data.frame(test[,5]),data.frame(test[,6]))
trainA.10<-setdiff(matA.10, testA.10)
modelA.10<-lm(x94~0+x1+x4+x12+x19+x20+x23
              +x24+x28+x30+x32+x41+x47+x51+x53
              +x54+x56+x57+x58, data =trainA.10)

summary(modelA.10)
AIC(modelA.10)
#crossvalidation using DAAG
library(DAAG)
crossA.10<-cv.lm(trainA.10, form.lm = formula(x94~0+x1+x4+x12+x19+x20+x23
                                              +x24+x28+x30+x32+x41+x47+x51+x53
                                              +x54+x56+x57+x58),
             m=10, dots = FALSE, seed=29, plotit=FALSE, printit=TRUE)
cvResults <- suppressWarnings(crossA.10)
attr(cvResults, 'ms')
crossvalARE(crossA.10)

######testing cross val model 
crossvalARE<-function(cross){
  cross$diff<-c((cross$x94-cross$cvpred)/cross$x94)
  are<- sum(abs(cross$diff))/nrow(cross)
  return(are)
}

#modelA.10<-lm(x94 ~ 0+x3+x5+x6+x12+x18+x20+x27+x29+x30+x40+x44+x47+x51+x55+x56+x60+x73+x82, data = trainA.10)
#summary(modelA.10)

#plot time series of cross val (ownself check ownself)
plotcvtimeseries<- function(c){
  plot(c$Date,c$x94,pch=18, cex=0.5 ,ylim=(c(min(c$x94,c$cvpred),c(max(c$x94,c$cvpred)+100))))
  lines(c$Date,c$x94 )
  lines(c$Date,c$cvpred,col="red",pch=18, cex=0.5)
  title(paste("Time Series of Reactor A with",deparse(substitute(c))),cex=0.4)
}
plotcvtimeseries(crossA.10)

#crossvalidation using CVTOOLS
library(cvTools)
cvA.10<-cvFit(lm,data=matA.55,y=trainA.10$x94,formula=x94 ~ 0+x3+x4
             +x12+x20+x23
             +x24+x28+x30
             +x32+x47+x53
             +x54+x56+x57
             +x58,K = 5, R = 10)
summary(cvA.10)
aggregate(cvA.10)
mspe(matA.10.1$x94, predict(lm(data=matA.10,x94 ~ x3+x5+x6+x12+x18+x20+x27+x29+x30+x40+x44+x47+x51+x55+x56+x60+x73+x82)))


#######testing for calibration
plottimeseries <-function(model,test){
  pred<-data.frame(x94pred=predict(model, test))
  plot(test$Date,test$x94,pch=18,cex=0.5,ylim=(c(min(pred$x94pred,test$x94),c(max(pred$x94pred,test$x94)))),xlab="Date",ylab = "x94")
  points(test$Date,pred$x94pred,pch=18,cex=0.6,col="red")
  lines(test$Date,test$x94)
  lines(test$Date,pred$x94pred,col="red")
  title(paste("Time Series of Reactor A with",deparse(substitute(model))),cex=0.25)
}
plottimeseries(modelA.10,testA.10)

modelvalidation<-function(model,test){
  mse<- mean((model$residuals)^2)
  rmse<-sqrt(mse)
  rs<-summary(model)$r.squared
  ars<-summary(model)$adj.r.squared
  aic<-AIC(model)
  are<-sum(abs(model$residuals)/model$model[,c("x94")])/length(model$fitted.values)
  prediction<-data.frame(x94pred=predict(model, test))
  testmse<-mean((prediction$x94pred-test$x94)^2)
  testrmse<-sqrt(testmse)
  diff<-c((test$x94-prediction$x94pred)/test$x94)
  SSE <- sum((test$x94 - prediction$x94pred) ^ 2)
  SST <- sum((test$x94 - mean(test$x94)) ^ 2)
  testrs<- 1 - (SSE/SST)
  testars<- testrs- ((model$rank)/(length(model$fitted.values)-model$rank-1))*(1-testrs)
  testare<-sum(abs(diff))/nrow(test)
  c1<-c("MSE:","RMSE:","R^2:","Adj R^2:","AIC:","ARE","Test MSE:","Test RMSE:","Test R^2","Test Adj R^2","Test ARE:")
  c2<-c(mse,rmse,rs,ars,aic,are,testmse,testrmse,testrs,testars,testare)
  table<-cbind(c1,c2)
  return (table)
}
modelvalidation(modelA.10,testA.10)
#######testing for calibration


library(cvTools)
cross1<-cvFit(lm,formula=x94~0+x1+x4
              + x12
              +x19+x20+x23
              +x24+x28+x30
              +x32+x41+x47
              +x49+x51+x53
              +x54+x56+x57
              +x58+x82,data=matA.10,y=matA.10$x94,K = 10,R=10)
cross2<-cvFit(lm,formula=x94 ~ 0+x4
              +x12+x20+x23
              +x24+x28+x30
              +x32+x47+x53
              +x54+x56+x57
              +x58,data=matA.10,y=matA.10$x94,K = 10, R = 10)

cross3<-cvFit(lm,formula=x94~0+x1+x4+x12
              +x19+x20+x23
              +x24+x28+x30
              +x32+x41+x47
              +x51+x53
              +x54+x56+x57
              +x58,data=matA.10,y=matA.10$x94,K = 10, R = 10)
summary(cross1)
hey<-cvSelect(cross1,cross2,cross3)
hey$selectBest
hey$seFactor
hey$reps
mspe(matA.10$x94, predict(model))



###### Variable Selection
library(MASS)
fit <- lm( x94~0+x1+x3+x4
           +x6+x10+ x12
           +x19+x20+ x23
           +x24+x28+ x30
           +x32+x41+ x47
           +x49+x51+ x53
           +x54+x56+ x57
           +x58+x64+ x76,data=matA.54)
step <- stepAIC(fit, direction="both")
step$anova # display results



