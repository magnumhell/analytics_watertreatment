### this R script is for linear models mainly for hypothesis 5


read<-paste0("/Volumes/JORDAN 1/SUTD/SembCorp/Matrices/Hypo 5/2013_MatrixA_NoNULLs.csv")
read1<-paste0("/Volumes/JORDAN 1/SUTD/SembCorp/Matrices/Hypo 5/2012_MatrixA.csv")
matA.5.12<- read.csv(read1)
matA.5.13<-read.csv(read)

library(dplyr)
matA.5.1213<- unique(rbind(matA.5.12,matA.5.13))
names(matA.5.1213)<- c("Date","x94","x1",'x2','x3','x4','x5','x6','x7','x8','x9','x10','x12','x17','x19','x20','x21','x22','x23','x24','x25','x26','x27','x28','x29','x30','x32','x35','x36','x37','x38','x39','x40','x41','x42','x44','x45','x46','x47','x48','x49','x51','x52','x53','x54','x55','x56','x57','x58','x60','x61','x62','x64','x67','x70','x76','x85')
matA.5.1213<-matA.5.1213[complete.cases(matA.5.1213),]
matA.5<-rbind(matA.5.1213,matA.5.14)
matA.5<-read.csv("/Volumes/JORDAN 1/SUTD/SembCorp/Matrices/Hypo 5/matA5.csv")
save(matA.5,file="/Volumes/JORDAN 1/SUTD/SembCorp/Matrices/Hypo 5/matA5.csv")

model<-lm(x94 ~0+x48+x12+x56,matA.5.1213)
model<-lm(x94 ~ 0 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + 
            x9 + x10 + x12 + x17 + x19 + x20 + x21 + x22 + x23 + x24 + 
            x25 + x26 + x27 + x28 + x29 + x30 + x32 + x35 + x36 + x37 + 
            x38 + x39 + x40 + x41 + x42 + x44 + x45 + x46 + x47 + x48 + 
            x49 + x51 + x52 + x53 + x54 + x55 + x56 + x57 + x58 + x60 + 
            x61 + x62 + x64 + x67 + x70 + x76 + x85, data = matA.5.121314)


###hypo5model
model<-lm(x94 ~ 0+x3+x4+x12+x20+x23+x24+x28+x30+x32+x47+x53+x54+x56+x57+x58, matA.5)
summary(model)
AIC(model)

plotactualvsfitted <- function(dataset){
  #dataset<-matA.5.1213[complete.cases(matA.5.1213),]
  fit<-model$fitted.values
  x94<-dataset$x94
  r1<-c(0,max(x94))
  plot(fit,x94,xlim=r1,ylim=r1,pch=18)
  lines(r1,r1,col="red")
  title('Actual vs Fitted Values (Year 2012,13 with Hypo 5)',cex.main=0.75)
  text(x=c(max(x94)/2),y=c(max(x94)/4),adj=c(0,0),labels=paste("Adjusted R-squared:",round(summary(model)$adj.r.squared,4)))
}
plotactualvsfitted(matA.5.1213)

points(matA.5.1213$Date,matA.5.1213$x94,col='red')
plot(matA.5.1213$Date,fit)
lines(fit)
abline(model)

plotresidualvsactual <- function(dataset){
  fit<-model$fitted.values
  x94<-dataset$x94
  residual<-x94-fit
  plot(x94,residual,pch=18)
  r1<-c(0,max(x94))
  lines(r1,c(0,0))
  lines(lowess(x94,residual,f=0.8),col="red")
  title('Residual vs Actual Values (Year 2012,13 with Hypo 5)',cex.main=0.75)
  text(x=c(1000/2),y=c(-100),adj=c(0,0),labels=paste("Adjusted R-squared:", round(summary(model)$adj.r.squared,4)))
}
plotresidualvsactual(matA.5.1213)

####AIC
AIC(model,k=2.2671717284)

##### prediction
matA.5.14<- read.csv("/Volumes/JORDAN 1/SUTD/SembCorp/Matrices/Hypo 5/2014_MatrixA_NoNULLsTest.csv")
names(matA.5.14)<- c("Date","x94","x1",'x2','x3','x4','x5','x6','x7','x8','x9','x10','x12','x17','x19','x20','x21','x22','x23','x24','x25','x26','x27','x28','x29','x30','x32','x35','x36','x37','x38','x39','x40','x41','x42','x44','x45','x46','x47','x48','x49','x51','x52','x53','x54','x55','x56','x57','x58','x60','x61','x62','x64','x67','x70','x76','x85')
prediction <- predict(model, newdata = matA.5.14)
test<-matA.5.14
test[,1]<-prediction
head(prediction)
SSE <- sum((test$x94 - prediction) ^ 2)
SST <- sum((test$x94 - mean(test$x94)) ^ 2)
1 - SSE/SST
#### plot predicted vs actual
plot(test$x94,prediction,xlim=c(0,max(test$x94)),ylim=c(0,max(test$x94)),pch=18)
lines(c(0,max(test$x94)),c(0,max(test$x94)),col="red")
#### time series
plot(matA.5.14$Date,test$x94)
points(matA.5.14$Date,prediction,col='red',pch=18,cex=0.7)
title("Time series of prediction(red) with actual(black)\n Reactor A,year 2014")

### cross validation
matA.5.121314<-unique(rbind(matA.5.1213,matA.5.14))
library(DAAG)
cross<-cv.lm(matA.5.121314, form.lm = formula(x94 ~ 0+x3+x4+x12+x20+x21+x23+x24+x28+x47+x53+x54+x56+x58),
      m=6, dots = FALSE, seed=29, plotit=TRUE, printit=TRUE)
lines(cross$Date,cross$x94 )
lines(cross$Date,cross$cvpred,col="red",pch=18, cex=0.5)
title("Time series of Cvpred vs x94 for Reactor, Hypo 5")
### finding Average Relative Error
cross$diff<-c((cross$x94-cross$cvpred)/cross$x94)
are<- sum(abs(cross$diff))/280
are
##### CVTOOLS
library(cvTools)
cross1<-cvFit(lm,formula=x94 ~ 0+x3+x4+x12+x20+x21+x23+x24+x28+x30+x32+x47+x53+x54+x56+x57+x58,data=matA.5,y=matA.5$x94,K = 10,R=10)
cross2<-cvFit(lm,formula=x94 ~ 0+x3+x4+x12+x20+x21+x23+x24+x28+x30+x32+x44+x47+x48+x53+x54+x56+x57+x58,data=matA.5,y=matA.5$x94,K = 10, R = 10)
cross3<-cvFit(lm,formula=x94 ~ 0+x12+x44+x56,data=matA.5,y=matA.5$x94,K = 10, R = 10)
summary(cross1)
hey<-cvSelect(cross1,cross2,cross3)
hey$selectBest
hey$seFactor
hey$reps
mspe(matA.5$x94, predict(model))



###### Variable Selection
library(MASS)
fit <- lm(x94~ 0+x3+x4+x6+x12+x20+x21+x23+x24+x28+x29+x30+x32+x40+x44+x47+x48+x51+x53+x54+x56+x57+x58,data=matA.5)
step <- stepAIC(fit, direction="both")
step$anova # display results
x94 ~ 0+ x3 + x4 + x12 + x20 + x23 + x24 + x28 + x32 + x44 + x47 + 
  x51 + x53 + x54 + x56 + x57 + x58 