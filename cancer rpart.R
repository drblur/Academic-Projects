setwd("c:/users/ashwin/desktop/course/r & ml/assignment data")
data<-read.csv("chin.csv")
set.seed(123)
head(data)
head1<-subset(data,grade==1)
head2<-subset(data,grade==0)
t1<-sample(1:nrow(head1),round(.9*nrow(head1)))
t2<-sample(1:nrow(head2),round(.9*nrow(head2)))
train<-rbind(head1[t1,],head2[t2,])
test<-rbind(head1[-t1,],head2[-t2,])

train[,"age"]<-train[,"age"]/10
test[,"age"]<-test[,"age"]/10

##fitting
library(rpart)


fit<-rpart(factor(grade)~ age + factor(ethnicity) + factor(ER) + factor(PR) + factor(RT) +
             factor(CT) + factor(HT) + factor(N) + tumorStage + tumorSize,data = train,control =
             rpart.control(minsplit = 1,xval = 20),parms = list(split="gini"))



pre<-predict(fit,test,type="class")
fit$cptable


fit1=prune(fit,cp = .045)
pre1<-predict(fit1,test,type="class")

library(caret)
confusionMatrix(pre1,test$grade)

acc1<-length(which(pre1==test$grade,T))/length(test)
acc1


