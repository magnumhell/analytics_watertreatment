corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  directory <- "/Volumes/JORDAN 1/SUTD/SembCorp/Matrices/"
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  fname<-"MatrixA1213.csv"
  threshold=0
  ## Return a numeric vector of correlations
  tcorr <- function(fname) {
    data <- read.csv(paste0(directory, fname))
    nobs <- sum(complete.cases(data))
    if (nobs > threshold) {
      return (cor(data$x94, data$x1, use="complete.obs"))
    }
  }
  tcorrs <- lapply(names(data), tcorr) #get all correlations + NULLs
  tcorrs <- unlist(tcorrs[!sapply(tcorrs, is.null)]) #remove NULLs
  return (tcorrs)
}


#getting correlation
list1 <-c()
for (i in 3:(length(names(data)))){
  print (i)
  a<-cor(data$x94, data[i], use="complete.obs")
  list1<-c(list1,a)
}
cor(matrixA$x94, matrixA$x1, use="complete.obs",method="kendall")

Kendall<-data.frame(list1)
Kendall<-abs(Kendall)
Kendall[,1]<-Kendall[,1]*1000
write.table(correff2, "/Volumes/JORDAN 1/SUTD/SembCorp/Correlation/corr121314A.txt", sep="\t",row.names=FALSE)

correff<-read.table("/Volumes/JORDAN 1/SUTD/SembCorp/Correlation/corr121314A.txt",header = TRUE,sep="\t")

#### corrplot
library(corrplot)
corrplot(corrtable1,method="square",na.label = "o",sig.level=0.3,tl.cex=0.5,number.cex = 0.3)


ktable2<-cor(matAcom[,2:74],method = "kendall")

absolute<-function(){
  for (i in 1:nrow(correff)){
    if (correff[i,1] < 0){
      correff[i,2]<-"negative"
      print (i)
    }else{
      correff[i,2]<-"positive"
    }
  }
}
correff<-data.frame(corrtable[,1])
correff[56, ]<-0
correff[,4]<-abs(correff[,4])*1000
correff[2:72,4]<-as.numeric(correff[2:72,4])

#treemapping
correff<-data.frame(correff[-c(1),])
names(correff)<-c("x94")
correff[,2]<- rownames(corrtable[2:73,])
l<-abs(correff$x94)
correff[,3]<-l
library(treemap)
treemap(correff,title="Correlation(Year 2012,13,14) of Ouput & Predictor in Reactor A",index="V2",vSize="V3",vColor="x94", type="manual", palette="RdYlBu")


##### matrix B for hypo 1########
library(dplyr)
read<-paste0("/Volumes/JORDAN 1/SUTD/SembCorp/Matrices/Hypo 1/2014_MatrixB_NoNULLs.csv")
matB.1.14 <- read.csv(read)
matB.1<-unique(rbind(matB.1,matB.1.14))
corrtableB<-cor(matB.1[,2:74])
correffB <-data.frame(corrtableB[,1])
correffB <-data.frame(correffB[-c(1,56), ])
rownames(correffB)<- rownames(corrtableB[-c(1,56),])
names(correffB)<-c("x95")
### treemapping
correffB[,2]<- rownames(correffB)
l<-abs(correffB$x95)
correffB[,3]<-l
treemap(correffB,title="Correlation(Year 2012,13,14) of Output & Predictor in Reactor B",index="V2",vSize="V3",vColor="x95", type="manual", palette="RdYlBu")
####circos
correffB[,3]<-abs(correffB[,3])*1000
write.table(correffB, "/Volumes/JORDAN 1/SUTD/SembCorp/Correlation/matB121314Correff.txt", sep="\t")
#####crazycircos
abscorrtableB<-abs(corrtableB)*1000
write.table(abscorrtableB, "/Volumes/JORDAN 1/SUTD/SembCorp/Correlation/matB.1.CorrTable.txt", sep="\t")
#### corrplot
library(corrplot)
corrplot(corrtable,method="square",na.label = "o",sig.level=0.3,tl.cex=0.5,number.cex = 0.3,title = "Correlation Plot for Matrix A (Hypo 1)",mar=c(0,0,1,0))

#### correlation for hypo 5
corMatAhypo5<-cor(matA.5.121314[,2:57])
corMatAhypo5col<-as.data.frame(corMatAhypo5[2:56,1])
corMatAhypo5col[,2]<-rownames(corMatAhypo5[2:56,])
corMatAhypo5col[,3]<-abs(corMatAhypo5col[,1])
names(corMatAhypo5col)<-c('x94',"V2","V3")
treemap(corMatAhypo5col,title="Correlation of Output & Predictor in Reactor A, Hypothesis 5",index="V2",vSize="V3",vColor="x94", type="manual", palette="RdYlBu")
