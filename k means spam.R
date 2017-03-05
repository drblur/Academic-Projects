setwd("C:/Users/Ashwin/Desktop/COURSE/R & ML/Assignment data")
data=read.csv("spam.csv",stringsAsFactors = T)
cat<-data[,58]
spam<-data[,-58]
str(spam)
###pre-processing data
scale_spam<-scale(spam,center = T,scale = T)
###k -means clustering
kmean<-kmeans(scale_spam,centers = 2,iter.max = 10,nstart =20)
wtss<-c()
for(i in 2:10){
  kmean1<-kmeans(scale_spam,centers=i,iter.max = 10,nstart=20)
  wtss<-c(wtss,kmean1$tot.withinss)  
}
plot(2:10,wtss,type = "b")
##min-max
minmax<-function(x){
  xnew<- (x-min(x))/(max(x)-min(x))
}
spam1<-apply(spam,2,minmax)
head(spam1)

##taking minmax data

wtss1<-c()
for(i in 2:10){
  kmean2<-kmeans(spam1,centers=i,iter.max = 10,nstart=20)
  wtss1<-c(wtss1,kmean2$tot.withinss)  
}
wtss1
plot(2:10,wtss1,type = "b")

kmean3<-kmeans(spam1,centers=2,iter.max = 10,nstart=20)
library(cluster)
clus<-pam(spam1,2,metric="euclidean")
clusplot(clus)
library(NbClust)
res<-NbClust(spam1, diss=NULL, distance = "euclidean", min.nc=2, max.nc=6, 
             method = "kmeans", index = "kl") 
res


