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

##creating a fit

library(rpart)

fit<-rpart(factor(Churn)~ factor(gender) + factor(SeniorCitizen) + factor(Partner) + factor(Dependents) + factor(PhoneService) +
            factor(MultipleLines) +  factor(InternetService) + factor(InternetService) + factor(OnlineSecurity) + factor(OnlineBackup)
           + factor(DeviceProtection) + factor(TechSupport) + factor(StreamingTV) + factor(StreamingMovies) + factor(Contract) + 
             factor(PaperlessBilling) + factor(PaymentMethod) + MonthlyCharges + TotalCharges,data = train,control = rpart.control
           (minsplit = 3,xval = 15,maxsurrogate = 10),parms = list(split="gini"))

pred<-predict(fit,test,type = "class")

table(pred)
table(test$Churn)
prediction<-ifelse(pred=="Yes",1,0)
test1<-ifelse(test$Churn=="Yes",1,0)
library(caret)




##pruning
fit1<-prune(fit,cp=.05)
pred1<-predict(fit1,test,type = "class")
prediction1<-ifelse(pred1=="Yes",1,0)
confusionMatrix(prediction1,test1)
