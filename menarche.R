library("MASS")
attach(menarche)
str(menarche)
summary(menarche)
plot(Menarche/Total~Age,data = menarche)
lines(menarche$Age,fit$fitted,type = "l",col="red")
predict(fit,)
fit<-glm(cbind(Menarche,Total-Menarche)~ Age,data = menarche, family = binomial("logit"))

predict(fit,newdata )




######q2#####
ib<-read.csv(file = "in.csv",header = T)

