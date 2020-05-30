
####final model
## this R script uses linear models to test against other matrices



x94 ~ 0+x3+x5+x6+x12+x20+x27+x40+x44+x47+x51+x55+x56+x73+x82 #belongs to hypo7
#### try on matrix 5 for reactor A  ######################################################
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(matA.5), 0.8*nrow(matA.5))  # row indices for training data
trainingData <- matA.5[trainingRowIndex, ]  # model training data
testData  <- matA.5[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(x94 ~ 0+x3+x5+x6+x12+x20+x27+x40+x44+x47+x51+x55+x56,data=matA.5)  # build the model
Pred <- predict(lmMod, testData)  # prediction
summary(lmMod)
AIC (lmMod) #3267

actuals_preds <- data.frame(cbind(actuals=testData$x94, predicteds=Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 88.3%
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 78.0%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 26.9%, mean absolute percentage deviation
cross<-cv.lm(matA.5, form.lm = formula(x94 ~ 0+x3+x5+x6+x12+x20+x27+x40+x44+x47+x51+x55+x56),
             m=10, dots = FALSE, seed=29, plotit=FALSE, printit=TRUE)
plotcvtimeseries<- function(c){
  plot(c$Date,c$x94,pch=18, cex=0.5 ,ylim=(c(min(c$x94,c$cvpred),c(max(c$x94,c$cvpred)+100))))
  lines(c$Date,c$x94 )
  lines(c$Date,c$cvpred,col="red",pch=18, cex=0.5)
  title(paste("Time Series of Reactor A with",deparse(substitute(c))),cex=0.4)
}
plotcvtimeseries(cross)
attr(cross, 'ms') #7471

#### try on matrix 6 for reactor A ######################################################

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(matA.6), 0.8*nrow(matA.6))  # row indices for training data
trainingData <- matA.6[trainingRowIndex, ]  # model training data
testData  <- matA.6[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(x94 ~ 0+x3+x5+x6+x12+x20+x27+x40+x44+x47+x51+x55+x56+x73+x82,data=matA.6)  # build the model
Pred <- predict(lmMod, testData)  # prediction
summary(lmMod)
AIC (lmMod)

actuals_preds <- data.frame(cbind(actuals=testData$x94, predicteds=Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 92.0%
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 77.7%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 28.5%, mean absolute percentage deviation
cross<-cv.lm(matA.6, form.lm = formula(x94 ~ 0+x3+x5+x6+x12+x20+x27+x40+x44+x47+x51+x55+x56+x73+x82),
             m=10, dots = FALSE, seed=29, plotit=FALSE, printit=TRUE)
plotcvtimeseries<- function(c){
  plot(c$Date,c$x94,pch=18, cex=0.5 ,ylim=(c(min(c$x94,c$cvpred),c(max(c$x94,c$cvpred)+100))))
  lines(c$Date,c$x94 )
  lines(c$Date,c$cvpred,col="red",pch=18, cex=0.5)
  title(paste("Time Series of Reactor A with",deparse(substitute(c))),cex=0.4)
}
plotcvtimeseries(cross)
attr(cross, 'ms') #7853

#### try on matrix 8 for reactor A ######################################################
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(matA.8), 0.8*nrow(matA.8))  # row indices for training data
trainingData <- matA.8[trainingRowIndex, ]  # model training data
testData  <- matA.8[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(x94 ~ 0+x3+x5+x6+x12+x20+x27+x40+x44+x47+x51+x55+x56+x73+x82,data=matA.8)  # build the model
Pred <- predict(lmMod, testData)  # prediction
summary(lmMod)
AIC (lmMod) #4296

actuals_preds <- data.frame(cbind(actuals=testData$x94, predicteds=Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 92.4%
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 80.5%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 24.2%, mean absolute percentage deviation
cross<-cv.lm(matA.8, form.lm = formula(x94 ~ 0+x3+x5+x6+x12+x20+x27+x40+x44+x47+x51+x55+x56+x73+x82),
             m=10, dots = FALSE, seed=29, plotit=FALSE, printit=TRUE)
plotcvtimeseries<- function(c){
  plot(c$Date,c$x94,pch=18, cex=0.5 ,ylim=(c(min(c$x94,c$cvpred),c(max(c$x94,c$cvpred)+100))))
  lines(c$Date,c$x94 )
  lines(c$Date,c$cvpred,col="red",pch=18, cex=0.5)
  title(paste("Time Series of Reactor A with",deparse(substitute(c))),cex=0.4)
}
plotcvtimeseries(cross)
attr(cross, 'ms') #7725

#### try on matrix 7 for reactor A  ######################################################
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(matA.7), 0.8*nrow(matA.7))  # row indices for training data
trainingData <- matA.7[trainingRowIndex, ]  # model training data
testData  <- matA.7[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(x94 ~ 0+x3+x5+x6+x12+x20+x27+x44+x47+x51+x55+x56+x73+x82,data=matA.7)  # build the model
Pred <- predict(lmMod, testData)  # prediction
summary(lmMod)
AIC (lmMod) #5700

actuals_preds <- data.frame(cbind(actuals=testData$x94, predicteds=Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 94.5%
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 79.9%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 25.9%, mean absolute percentage deviation
cross<-cv.lm(matA.7, form.lm = formula(x94 ~ 0+x3+x5+x6+x12+x20+x27+x44+x47+x51+x55+x56+x73+x82),
             m=10, dots = FALSE, seed=29, plotit=FALSE, printit=TRUE)
plotcvtimeseries<- function(c){
  plot(c$Date,c$x94,pch=18, cex=0.5 ,ylim=(c(min(c$x94,c$cvpred),c(max(c$x94,c$cvpred)+100))))
  lines(c$Date,c$x94 )
  lines(c$Date,c$cvpred,col="red",pch=18, cex=0.5)
  title(paste("Time Series of Reactor A with",deparse(substitute(c))),cex=0.4)
}
plotcvtimeseries(cross)
attr(cross, 'ms') #7038

model<- x94 ~ 0+x3+x4+x12+x20+x21+x23+x24+x28+x30+x32+x47+x53+x54+x56+x57+x58 #belongs to hypo5
#### try on matrix 5 for reactor A  ######################################################
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(matA.5), 0.8*nrow(matA.5))  # row indices for training data
trainingData <- matA.5[trainingRowIndex, ]  # model training data
testData  <- matA.5[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(model,data=matA.5)  # build the model
Pred <- predict(lmMod, testData)  # prediction
summary(lmMod)
AIC (lmMod) #3267

actuals_preds <- data.frame(cbind(actuals=testData$x94, predicteds=Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 88.3%
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 78.0%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 26.9%, mean absolute percentage deviation
library(DAAG)
cross<-cv.lm(matA.5, form.lm = formula(model),
             m=10, dots = FALSE, seed=29, plotit=FALSE, printit=TRUE)
plotcvtimeseries<- function(c){
  plot(c$Date,c$x94,pch=18, cex=0.5 ,ylim=(c(min(c$x94,c$cvpred),c(max(c$x94,c$cvpred)+100))))
  lines(c$Date,c$x94 )
  lines(c$Date,c$cvpred,col="red",pch=18, cex=0.5)
  title(paste("Time Series of Reactor A with",deparse(substitute(c))),cex=0.4)
}
plotcvtimeseries(cross)
attr(cross, 'ms') #7471

#### try on matrix 6 for reactor A ######################################################

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(matA.6), 0.8*nrow(matA.6))  # row indices for training data
trainingData <- matA.6[trainingRowIndex, ]  # model training data
testData  <- matA.6[-trainingRowIndex, ]   # test data

# Build the model on training data - (missing x4+x23+x24+x28+x32+x53+x54+x57+x58)
lmMod <- lm(model<- x94 ~ 0+x3+x12+x20+x30+x47+x56,data=matA.6)  # build the model
Pred <- predict(lmMod, testData)  # prediction
summary(lmMod)
AIC (lmMod)

actuals_preds <- data.frame(cbind(actuals=testData$x94, predicteds=Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 92.0%
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 77.7%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 28.5%, mean absolute percentage deviation
cross<-cv.lm(matA.6, form.lm = formula(model<- x94 ~ 0+x3+x12+x20+x30+x47+x56),
             m=10, dots = FALSE, seed=29, plotit=FALSE, printit=TRUE)
plotcvtimeseries<- function(c){
  plot(c$Date,c$x94,pch=18, cex=0.5 ,ylim=(c(min(c$x94,c$cvpred),c(max(c$x94,c$cvpred)+100))))
  lines(c$Date,c$x94 )
  lines(c$Date,c$cvpred,col="red",pch=18, cex=0.5)
  title(paste("Time Series of Reactor A with",deparse(substitute(c))),cex=0.4)
}
plotcvtimeseries(cross)
attr(cross, 'ms') #7853

#### try on matrix 8 for reactor A ######################################################
set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(matA.8), 0.8*nrow(matA.8))  # row indices for training data
trainingData <- matA.8[trainingRowIndex, ]  # model training data
testData  <- matA.8[-trainingRowIndex, ]   # test data

# Build the model on training data -
lmMod <- lm(x94 ~ 0+x3+x4+x12+x20+x23+x24+x28+x30+x47+x53+x54+x56,data=matA.8)  # build the model
Pred <- predict(lmMod, testData)  # prediction
summary(lmMod)
AIC (lmMod) #4296

actuals_preds <- data.frame(cbind(actuals=testData$x94, predicteds=Pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  # 92.4%
head(actuals_preds)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  
# => 80.5%, min_max accuracy
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)  
# => 24.2%, mean absolute percentage deviation
cross<-cv.lm(matA.8, form.lm = formula(x94 ~ 0+x3+x5+x6+x12+x20+x27+x40+x44+x47+x51+x55+x56+x73+x82),
             m=10, dots = FALSE, seed=29, plotit=FALSE, printit=TRUE)
plotcvtimeseries<- function(c){
  plot(c$Date,c$x94,pch=18, cex=0.5 ,ylim=(c(min(c$x94,c$cvpred),c(max(c$x94,c$cvpred)+100))))
  lines(c$Date,c$x94 )
  lines(c$Date,c$cvpred,col="red",pch=18, cex=0.5)
  title(paste("Time Series of Reactor A with",deparse(substitute(c))),cex=0.4)
}
plotcvtimeseries(cross)
attr(cross, 'ms') #7725