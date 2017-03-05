bc<-read.csv("breast.csv")
str(bc)
summary(bc)

##splitting
size<-sample(1:nrow(bc),round(.7*nrow(bc)))
train<-bc[size,]
test<-bc[-size,]
train<-train[,-1] 
test<-test[,-1]
diagtrain<-train[,1]
diagtest<-test[,1]

##exploration
table(bc$diagnosis)
round(prop.table(table(bc$diagnosis))*100,digits = 1)
summary(bc[,c(3,6,7)])

##scaling
normalize <- function(x) {       return ((x - min(x)) / (max(x) - min(x))) } 
apply(train,2,normalize)
apply(test,2,normalize)

##prediction
library(class)
near<-knn(train = train,test = test,cl=diagtrain,k= 21)
round(prop.table(table(near))*100,digits = 2)
acc<-(length(which(near==diagtest,T))/length(diagtest))
acc
##plots and other inference
library(gmodels)
CrossTable(diagtest,near,prop.chisq = F)
library(ggplot2)
##with z_transformation scaling
zbc<-as.data.frame(bc[,-1])
head(zbc)
zbc<-zbc[,-1]
zbc<-scale(zbc)
ztrain<-zbc[size,]
ztest<-zbc[-size,]
znear<-knn(train=ztrain,test=ztest,cl=diagtrain,k=21)
round(prop.table(table(znear))*100,digits = 2)
acc1<-length(which(znear==diagtest,T))/length(diagtest)
acc1
library(ggplot2)
