setwd("C:/Users/Ashwin/Desktop/COURSE/R & ML/Assignment data")

wine<-read.csv("wine.csv")

set.seed(1234)
str(wine)
library(class)

##splitting

t<-floor(0.7*nrow(wine))
t1<-sample(seq_len(nrow(wine)),size = t)
train<-wine[t1,]
test<-wine[-t1,]

##min max and knn
minmax<-function(x){
  xnew<-(x-min(x))/(max(x)-min(x))
}
train[,1:11]<-apply(train[,-12],2,minmax)
test[,1:11]<-apply(test[,-12],2,minmax)

data<-knn1(train[,-12], test[,-12], as.factor(train[,12]))
ac<-length(which(data==as.factor(test[,12]),T))/length((test[,12]))
##vector loop for optimal k
cv1<-c()
for(i in 1 :20){
  cvr<-knn.cv(train[,-12], as.factor(train[,12]),k=i,l=0,prob=F,use.all = T)
  cv1<-c(cv1,length(which(cvr==as.factor(train[,12]),T))/length(train[,12]))
}
cv1
plot(cv1)

##keep and condense
data1<-knn(train[,-12], test[,-12], as.factor(train[,12]),k=9,l=0,prob=F,use.all = T)
ac1<-length(which(data1==as.factor(test[,12]),T))/length((test[,12]))

keep<-condense(train[,-12],as.factor(train[,12]))
keep1<-reduce.nn(train[,-12], keep,as.factor(train[,12]))

data2<-knn(train[keep1,-12], test[,-12], as.factor(train[keep1,12]),k=1,l=0,prob=F,use.all = T)
ac2<-length(which(data2==as.factor(test[,12]),T))/length((test[,12]))

data3<-knn(train[keep,-12], test[,-12], as.factor(train[keep,12]),k=1,l=0,prob=F,use.all = T)
ac3<-length(which(data3==as.factor(test[,12]),T))/length((test[,12]))

##quality wise splitting
q5<-subset(wine,wine$quality==5)
q6<-subset(wine,wine$quality==6)
q7<-subset(wine,wine$quality==7)
q4<-subset(wine,wine$quality==4)
q8<-subset(wine,wine$quality==8)
q3<-subset(wine,wine$quality==3)

##splitting quality wise data for train and test
train5<-floor(seq_len(.8*nrow(q5)))
test5<-q5[-train5,]
train6<-floor(seq_len(.8*nrow(q6)))
test6<-q6[-train6,]
train7<-floor(seq_len(.8*nrow(q7)))
test7<-q7[-train7,]
train8<-floor(seq_len(.8*nrow(q8)))
test8<-q8[-train8,]
train3<-floor(seq_len(.8*nrow(q3)))
test3<-q3[-train3,]
train4<-floor(seq_len(.8*nrow(q4)))
test4<-q4[-train4,]


traini5<-q5[train5,]
traini6<-q6[train6,]
traini7<-q7[train7,]
traini8<-q8[train8,]
traini3<-q3[train3,]
traini4<-q4[train4,]
##combine quality wise data
library(class)
trainbig<-rbind(traini5,traini6,traini7,traini8,traini3,traini4)
testbig<-rbind(test5,test6,test7,test8,test3,test4)
knn4<-knn(train=trainbig[,-12],test=testbig[,-12],as.factor(trainbig[,12]),k=199,l=2)
ac2<-length(which(knn4==as.factor(testbig[,12]),T))/length((testbig[,12]))

qv<-matrix(0,10,10)
for(i in 1:10){
  for(j in 1:10){
    knn9<-knn.cv(trainbig[,-12],as.factor(trainbig[,12]),k=i,l=j-1)
    qv[i,j]<-length(which(knn9==as.factor(trainbig[,12]),T))/length(trainbig[,12])
  }
}
qv
keep2<-condense(trainbig[,-12],as.factor(trainbig[,12]))
keep3<-reduce.nn(trainbig[,-12], keep2,as.factor(trainbig[,12]))

data4<-knn.cv(trainbig[keep2,-12],as.factor(trainbig[keep2,12]),k=1,l=0,prob=F,use.all = T)
accu3<-length(which(data4==as.factor(trainbig[,12]),T))/length(trainbig[,12])
accu3
plot(qv)

