##4
setwd("c:/users/ashwin/desktop/course/r & ml/assignment data")
data<-read.csv("tel.csv",stringsAsFactors = T)
str(data)
library(rpart)
set.seed(123)
##remove first column fields of customer ID

data<-data[,-1]

##data splitting

yes<-subset(data,Churn=="Yes")
no<-subset(data,Churn=="No")
t1<-sample(1:nrow(yes),round(.8*nrow(yes)))
t2<-sample(1:nrow(no),round(.8*nrow(no)))
train<-rbind(yes[t1,],no[t2,])
test<-rbind(yes[-t1,],no[-t2,])

##random forest
fit<-randomForest(train[,1:18],train[,19],mtry = 3,strata = T,do.trace = 100)
pred<-predict(fit,test[,1:18],type = "class")
confusionMatrix(pred,test[,19],positive = "Yes")
