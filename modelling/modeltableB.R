rm(list=ls())

readB<-paste0("/Volumes/JORDAN 1/SUTD/SembCorp/Matrices/Master/MatrixB.csv")
matB<-read.csv(readB) ###big matrix
#correlation and visualization
mastercorB<-data.frame(cor(matB[,2:86],use="pairwise")) #[2:85,1]
library(treemap)
mastercorB<-cbind(mastercorB,n=row.names(mastercorB),a=abs(mastercorB$c))
treemap(mastercorB,title="Correlation of Output & Predictor in Reactor B",vSize ="a",vColor = "c",index= "n",type="manual", palette="RdYlBu")
library(corrplot)
corrplot(as.matrix(mastercortable),method="square",na.label = "o",sig.level=0.3,tl.cex=0.5,number.cex = 0.3,title = "Correlation Plot for Reactor A",mar=c(0,0,1,0))
Bhypo6<- subset(mastercorB,a>=0.3)
namesB<-rownames(Bhypo6)


#creating matrices with different set of predictors
#correlation >0.3
matB.6<-matB[,c("Date","x95","x3","x12","x18","x19","x20","x22","x27","x30","x40","x51","x55","x56","x66","x72","x87","x88","x90","x91","x93")]
matB.6<-matB.6[complete.cases(matB.6),]
#count number of null values in each column
for (i in 1:ncol(matB.6)){
  sum<-sum(is.na(matB.6[,i]))
  print (c(colnames(matB.6[,i]),sum))
}
#remove x91,x72,x44
matB.61<-matB[,c("Date","x95","x3","x12","x18","x19","x20","x22","x27","x30","x40","x51","x55","x56","x66","x87","x88","x90","x93")]
matB.61<-matB.61[complete.cases(matB.61),]
#correlation>0.4 less x91,x72,x44 but has x27 lel
matB.62<-matB[,c("Date","x95","x12","x18","x20","x27","x40","x56","x87")]
matB.62<-matB.62[complete.cases(matB.62),]
#correlation>0.35 less x91,x72,x44
matB.63<-matB[,c("Date","x95","x12","x18","x20","x27","x30","x40","x51","x56","x66","x87","x88","x93")]
matB.63<-matB.63[complete.cases(matB.63),]
#remove x20,x18,x30,x88
matB.64<-matB[,c("Date","x95","x12","x27","x40","x51","x56","x66","x87","x93")]
matB.64<-matB.64[complete.cases(matB.64),]

matB.65<-matB[,c("Date","x95","x12","x40","x56","x87")]
matB.65<-matB.65[complete.cases(matB.65),]

matB.66<-matB[,c("Date","x95","x12","x56","x87")]
matB.66<-matB.66[complete.cases(matB.66),]
#using variable selection
matB.7<-matB[,c("Date","x95","x12","x18","x30","x56","x66","x87","x88","x93")]
matB.7<-matB.7[complete.cases(matB.7),]

#reformatting the date & extracting testing scheme
matB.7$Date<-as.POSIXct(strptime(matB.7$Date,"%d/%m/%Y"))
month<-list("09","10","01","07","01","08")
year<-list("2012","2012","2013","2013","2014","2014")
test<-mapply(function (month,year){
  data<- data.frame()
  data<-rbind(data,subset(matB.7, format.Date(Date, "%m")==month& format.Date(Date, "%Y")==year ))
  return (data)
},month,year)

#calibration with linear model
library(dplyr)
testB.7<-rbind(data.frame(test[,1]),data.frame(test[,2]),data.frame(test[,3]),data.frame(test[,4]),data.frame(test[,5]),data.frame(test[,6]))
trainB.7<-setdiff(matB.7, testB.7)
modelB.7<-lm(x95 ~ 0+x12 + x18
              + x30 + x56
              + x66 + x87
              + x88 + x93, data=trainB.7)

modelvalidation(modelB.7,testB.7)
#crossval training
#uses cv.lm() to train the model and predict against the same 30 months
#because we cant extract a model out of this function, we cant test it on the 6 months
library(DAAG)
crossB.7<-cv.lm(trainB.7, form.lm = formula(x95 ~ 0+x12 + x18
                                              + x30 + x56
                                              + x66 + x87
                                              + x88 + x93),
                 m=10, dots = FALSE, seed=29, plotit=FALSE)
cvResults <- suppressWarnings(crossB.7)
attr(cvResults, 'ms')
crossvalARE(crossB.7)
#crossvalidation
crossvalARE<-function(cross){
  cross$diff<-c((cross$x95-cross$cvpred)/cross$x95)
  are<- sum(abs(cross$diff))/nrow(cross)
  return(are)
}


#validation linear model
modelvalidation<-function(model,test){
  mse<- mean((model$residuals)^2)
  rmse<-sqrt(mse)
  rs<-summary(model)$r.squared
  ars<-summary(model)$adj.r.squared
  aic<-AIC(model)
  are<-sum(abs(model$residuals)/model$model[,c("x95")])/length(model$fitted.values)
  prediction<-data.frame(x95pred=predict(model, test))
  testmse<-mean((prediction$x95pred-test$x95)^2)
  testrmse<-sqrt(testmse)
  diff<-c((test$x95-prediction$x95pred)/test$x95)
  SSE <- sum((test$x95 - prediction$x95pred) ^ 2)
  SST <- sum((test$x95 - mean(test$x95)) ^ 2)
  testrs<- 1 - (SSE/SST)
  testars<- testrs- ((model$rank)/(length(model$fitted.values)-model$rank-1))*(1-testrs)
  testare<-sum(abs(diff))/nrow(test)
  c1<-c("MSE:","RMSE:","R^2:","Adj R^2:","AIC:","ARE","Test MSE:","Test RMSE:","Test R^2","Test Adj R^2","Test ARE:")
  c2<-c(mse,rmse,rs,ars,aic,are,testmse,testrmse,testrs,testars,testare)
  table<-cbind(c1,c2)
  return (table)
}

#######plotting the accuracy of prediction by calibrating using linear model
plottimeseries <-function(model,test){
  pred<-data.frame(x95pred=predict(model, test))
  plot(test$Date,test$x95,pch=18,cex=0.5,ylim=(c(min(pred$x95pred,test$x95),c(max(pred$x95pred,test$x95)))),xlab="Date",ylab = "x95")
  points(test$Date,pred$x95pred,pch=18,cex=0.6,col="red")
  lines(test$Date,test$x95)
  lines(test$Date,pred$x95pred,col="red")
  title(paste("Time Series of Reactor A with",deparse(substitute(model))),cex=0.25)
}
plottimeseries(modelB.7,testB.7)
#plotting time series of crossvalidation predictions vs actual
plotcvtimeseries<- function(c){
  plot(c$Date,c$x95,pch=18, cex=0.5 ,ylim=(c(min(c$x95,c$cvpred),c(max(c$x95,c$cvpred)+100))))
  lines(c$Date,c$x95 )
  lines(c$Date,c$cvpred,col="red",pch=18, cex=0.5)
  title(paste("Time Series of Reactor B with",deparse(substitute(c))),cex=0.4)
}
plotcvtimeseries(crossB.7)

library(MASS)
fit <- lm(x95~0+x3+x12
          +x18+x19+x20
          +x22+x27+x30
          +x40+x51
          +x55+x56+x66
          +x87+x88+x93,data=matB.6)
step <- stepAIC(fit, direction="both")
step$anova # display results

#x95~0+x3+x12+x18+x19+x20+x22+x27+x30+x40+x44+x51+x55+x56+x66+x87+x88+x90+x93
