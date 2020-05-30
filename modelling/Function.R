rm(list = ls())
read1<-paste0("/Volumes/JORDAN 1/SUTD/SembCorp/Matrices/Master/MatrixA.csv")
matA<-read.csv(read1) ###big matrix
readB<-paste0("/Volumes/JORDAN 1/SUTD/SembCorp/Matrices/Master/MatrixB.csv")
matB<-read.csv(readB) ###big matrix

matA.51<-matA[,c("Date","x94","x1","x4","x12","x19","x20","x23","x24","x28","x30","x32","x41","x47","x51","x53","x54","x56","x57","x58")]
matA.51<-matA.51[complete.cases(matA.51),]
matA.51$Date<-as.POSIXct(strptime(matA.51$Date,"%d/%m/%Y"))
matA.52<-matA[,c("Date","x94","x1","x4","x12","x19","x20","x23","x24","x28","x30","x32","x41","x47","x49","x51","x53","x54","x56","x57","x58","x82")]
matA.52<-matA.52[complete.cases(matA.52),]
matA.52$Date<-as.POSIXct(strptime(matA.52$Date,"%d/%m/%Y"))
matA.55<-matA[,c("Date","x94","x3","x4","x12","x20","x23","x24","x28","x30","x32","x47","x53","x54","x56","x57","x58")]
matA.55<-matA.55[complete.cases(matA.55),]
matA.55$Date<-as.POSIXct(strptime(matA.55$Date,"%d/%m/%Y"))

matB.65<-matB[,c("Date","x95","x12","x40","x56","x87")]
matB.65<-matB.65[complete.cases(matB.65),]
matB.65$Date<-as.POSIXct(strptime(matB.65$Date,"%d/%m/%Y"))

ModelCreate <- function(x,form){
  #data <- model.frame(form, x)
  data<-x
  data <- data[complete.cases(data),]
  rownames(data) <- NULL
  Results <- data
  Results$PredVal <- NA
  Results$StdErr <- NA
  for (i in 2:nrow(data)){
    if (i <= 50){
      Train <- data[1:(i-1),1:ncol(data)]
      LimMod <- lm(form, Train)
      FitVal <- predict.lm(LimMod, data[i,2:ncol(data)], se.fit = TRUE)
      Results$PredVal[i] <- mean(FitVal$fit)
      Results$StdErr[i] <- FitVal$se.fit
      #Results$StdErr[i] <-abs(Results$PredVal[i] -Results[i,2])
    } else if (i >= 51){
      Train <- data[(i-50):(i-1),1:ncol(data)]
      LimMod <- lm(form, Train)
      FitVal <- predict.lm(LimMod, data[i,2:ncol(data)], se.fit = TRUE)
      Results$PredVal[i] <- mean(FitVal$fit)
      Results$StdErr[i] <- FitVal$se.fit
      #Results$StdErr[i] <-abs(Results$PredVal[i] -Results[i,2])
    }
  }
  return(Results)
} 
resultA.55<-ModelCreate(matA.55,x94 ~ 0+x3+x4+x12+x20+x23+x24+x28+x30+x32+x47+x53+x54+x56+x57+x58)
resultA.52<-ModelCreate(matA.52,x94~0+x1+x4+x12+x19+x20+x23+x24+x28+x30+x32+x41+x47+x49+x51+x53+x54+x56+x57+x58+x82)
resultA.51<-ModelCreate(matA.51,x94~0+x1+x4 +x12+x19+x20+ x23+x24+x28+ x30+x32+x41+ x47 +x51+ x53+x54+x56+ x57 +x58)
resultB.65<-ModelCreate(matB.65,x95~0+x12+x40+x56+x87)

plotresult<- function(result,matrix){
  result<-result[50:nrow(result),]
  rownames(result) <- NULL
  relerror<-result$StdErr/result[,2]
  nm<-max(matrix[,2])/max(relerror)
  
  # par(mfrow=c(2,1))
  # plot(matrix[,1],(matrix[,2])/nm,pch=16,col="blue",cex=0.4,ylab ="Relative Error",xlab="Date",ylim=c(0,max(relerror)))
  # lines(matrix[,1],(matrix[,2])/nm,col="blue")
  # points(result$Date,relerror,pch=16,cex=0.5)
  
  # lines(result$Date,relerror)
  # segments(as.numeric(result$Date[1]),relerror[1],as.numeric(result$Date[nrow(result)]),relerror[nrow(result)],col="red")
  
  plot(result$Date,result[,2] ,pch=16, cex=0.4,col="blue",ylab = colnames(result)[2],xlab="Date",ylim=c(0,max(result[,2])))
  lines(result$Date,result[,2] ,col="red")
  sub<-subset(result,result$PredVal>=0)
  points(sub$Date,sub$PredVal,pch=16,cex=0.5)
  lines(sub$Date,sub$PredVal)
  legend("topright",c("Actual","Predicted"),
         lty=c(1,1), # gives the legend appropriate symbols (lines)
         lwd=c(2.5,2.5),col=c("red","black"))
  title("Accuracy of Time Series Algorithm, Reactor B Model 65")
  summary(relerror)
}
plotresult(resultB.65,matB.65)


modelvalidation<-function(model){
  mse<- mean((model$StdErr[50:length(rownames(model))])^2)
  rmse<-sqrt(mse)
  
  are<-sum(abs(model$StdErr[50:length(rownames(model))])/model[,2][50:length(rownames(model))])/(length(rownames(model))-50)
  acc<-(1-are)*100
  c1<-c("MSE:","RMSE:","ARE","Accuracy (%):")
  c2<-c(mse,rmse,are,acc)
  table<-cbind(c1,c2)
  return (table)
}
modelvalidation(resultA.55)


sapply(resultA.51$StdErr, function(x) sum(length(which(is.na(x))))) 

######################################################## BASIL
MatrixA <- read.csv("MatrixA.csv")
MatrixB <- read.csv("MatrixB.csv")
ModA <- x94 ~ 0+x3+x4+x12+x20+x21+x23+x24+x28+x30+x32+x47+x53+x54+x56+x57+x58
Pred <- ModelCreate(x=MatrixA, form=ModA)


mat <- data.frame("x1"=1:3,"x2"=4:6,"x3"=7:9,"x4"=10:12)
mod <- x1 ~ x3 + x4
ans <- model.frame(mod,mat)
rm(c(mat,mod,ans))



mat <- data.frame("x1"=1:4,"x2"=5:8,"x3"=9:12)
mod <- x1 ~ x2 + x3
lim <- lm(mod, mat[1:3,])
ans <- predict.lm(lim, mat[4,2:3])
