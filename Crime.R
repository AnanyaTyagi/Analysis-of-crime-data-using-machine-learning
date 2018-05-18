setwd("C:/Users/LENOVO/Desktop/A20")

#Linear Regression
#Data preprocessing

df<-read.csv("CIv2006.csv")
df<-df[,-c(8:13)]
df$state<-ifelse(is.na(df$state),ave(df$state,FUN=function(x) mean(x,na.rm=(TRUE))),df$state)
df$year<-ifelse(is.na(df$year),ave(df$year,FUN=function(x) mean(x,na.rm=(TRUE))),df$year)
df$murder<-ifelse(is.na(df$murder),ave(df$murder,FUN=function(x) mean(x,na.rm=(TRUE))),df$murder)
df$dacoity<-ifelse(is.na(df$dacoity),ave(df$dacoity,FUN=function(x) mean(x,na.rm=(TRUE))),df$dacoity)
df$riots<-ifelse(is.na(df$riots),ave(df$riots,FUN=function(x) mean(x,na.rm=(TRUE))),df$riots)

#Categorical Data

df$year<-factor(df$year,levels=c(1953:2006),labels=c(1:54))


library(caTools)
set.seed(2)
df<-df[5:6]
split<-sample.split(df$murder,SplitRatio = 0.75)
training_set<-subset(df,split==TRUE)
test_set<-subset(df,split==FALSE)
lin_reg<-lm(formula = murder~dacoity,data = training_set)
y_predLinear<-predict(lin_reg,test_set)
n<-length(y_predLinear)
data<-unname(y_predLinear)
c<-0
for(i in 1:n){
  if(floor(data[i])==test_set[i,1]){
    c=c+1
  }
}
accuracy<-(c*100)/n
#Visualization of linear regression
#install.packages("ggplot2")
library(ggplot2)
ggplot()+
  geom_point(aes(x=df$dacoity,y=df$murder),color='red')+
  geom_line(aes(x=df$dacoity,y=predict(lin_reg,newdata=df)),color="blue")+
  ggtitle("Linear Regression")+xlab('dacoity')+ylab('murder')

#Multiple Linear Regression
#Data preprocessing

df<-read.csv("CIv2006.csv")
df<-df[,-c(8:13)]
df$state<-ifelse(is.na(df$state),ave(df$state,FUN=function(x) mean(x,na.rm=(TRUE))),df$state)
df$year<-ifelse(is.na(df$year),ave(df$year,FUN=function(x) mean(x,na.rm=(TRUE))),df$year)
df$murder<-ifelse(is.na(df$murder),ave(df$murder,FUN=function(x) mean(x,na.rm=(TRUE))),df$murder)
df$dacoity<-ifelse(is.na(df$dacoity),ave(df$dacoity,FUN=function(x) mean(x,na.rm=(TRUE))),df$dacoity)
df$riots<-ifelse(is.na(df$riots),ave(df$riots,FUN=function(x) mean(x,na.rm=(TRUE))),df$riots)

#Categorical Data

df$year<-factor(df$year,levels=c(1953:2006),labels=c(1:54))

#spliting data in training and test set

library(caTools)
set.seed(12)
split<-sample.split(df$year,SplitRatio = 0.75)
training_set<-subset(df,split==TRUE)
test_set<-subset(df,split==FALSE)

regressor<-lm(formula=murder~state+dacoity+riots,data=training_set)
y_pred<-predict(regressor,newdata = test_set)
n<-length(y_pred)
data<-unname(y_pred)
c<-0
for(i in 1:n){
    if(floor(data[i])==test_set[i,5]){
       c=c+1
    }
}
accuracy<-(c*100)/n
#Multiple Regression Visualization
library(ggplot2)
ggplot()+
  geom_point(aes(x=df$dacoity,y=df$murder),color='red')+
  geom_line(aes(x=df$dacoity,y=predict(regressor,newdata=df)),color="blue")+
  ggtitle("Multiple Linear Regression")+xlab('dacoity+state+riots')+ylab('murder')


#Polynomial Regression
 
#Data preprocessing

df<-read.csv("CIv2006.csv")
df<-df[,-c(8:13)]
df$state<-ifelse(is.na(df$state),ave(df$state,FUN=function(x) mean(x,na.rm=(TRUE))),df$state)
df$year<-ifelse(is.na(df$year),ave(df$year,FUN=function(x) mean(x,na.rm=(TRUE))),df$year)
df$murder<-ifelse(is.na(df$murder),ave(df$murder,FUN=function(x) mean(x,na.rm=(TRUE))),df$murder)
df$dacoity<-ifelse(is.na(df$dacoity),ave(df$dacoity,FUN=function(x) mean(x,na.rm=(TRUE))),df$dacoity)
df$riots<-ifelse(is.na(df$riots),ave(df$riots,FUN=function(x) mean(x,na.rm=(TRUE))),df$riots)


#Categorical Data

df$year<-factor(df$year,levels=c(1953:2006),labels=c(1:54))
library(caTools)
set.seed(2)
df<-df[5:6]
df$level2<-df$dacoity^2
df$level3<-df$dacoity^3
df$level4<-df$dacoity^4
df$level5<-df$dacoity^5
df$level6<-df$dacoity^6
df$level7<-df$dacoity^7
df$level8<-df$dacoity^8
df$level9<-df$dacoity^9
df$level10<-df$dacoity^10
df$level11<-df$dacoity^11
df$level12<-df$dacoity^12
df$level13<-df$dacoity^13
df$level14<-df$dacoity^14
df$level15<-df$dacoity^15
poly_reg<-lm(formula = murder~.,data = df)
y_predPoly<-predict(poly_reg,df)
n<-length(y_predPoly)
data<-unname(y_predPoly)
c<-0
for(i in 1:n){
  if(floor(data[i])==df[i,1]){
    c=c+1
  }
}
accuracy<-(c*100)/n

#Polynomial Regression Visualization
library(ggplot2)
ggplot()+
  geom_point(aes(x=df$dacoity,y=df$murder),color='red')+
  geom_line(aes(x=df$dacoity,y=predict(poly_reg,newdata=df)),color="blue")+
  ggtitle("Polynomial Regression")+xlab('dacoity')+ylab('murder')

##SVR 
#Data preprocessing

df<-read.csv("CIv2006.csv")
df<-df[,-c(8:13)]
df$state<-ifelse(is.na(df$state),ave(df$state,FUN=function(x) mean(x,na.rm=(TRUE))),df$state)
df$year<-ifelse(is.na(df$year),ave(df$year,FUN=function(x) mean(x,na.rm=(TRUE))),df$year)
df$murder<-ifelse(is.na(df$murder),ave(df$murder,FUN=function(x) mean(x,na.rm=(TRUE))),df$murder)
df$dacoity<-ifelse(is.na(df$dacoity),ave(df$dacoity,FUN=function(x) mean(x,na.rm=(TRUE))),df$dacoity)
df$riots<-ifelse(is.na(df$riots),ave(df$riots,FUN=function(x) mean(x,na.rm=(TRUE))),df$riots)


#Categorical Data

df$year<-factor(df$year,levels=c(1953:2006),labels=c(1:54))
library(e1071)
regressor=svm(formula= murder~.,
              data= df,
              type= 'eps-regression')
y_pred=predict(regressor,df)
n<-length(y_pred)
data<-unname(y_pred)
c<-0
for(i in 1:n){
  if(floor(data[i])==df[i,5]){
    c=c+1
  }
}
accuracy<-(c*100)/n

#Support Vector Regression Visualization
library(ggplot2)
ggplot()+
  geom_point(aes(x=df$dacoity,y=df$murder),color='red')+
  geom_line(aes(x=df$dacoity,y=predict(regressor,newdata=df)),color="blue")+
  ggtitle("Support Vector Regression")+xlab('dacoity+state+status+year+riots')+ylab('murder')


#KNN classification
#Data preprocessing

df<-read.csv("CIv2006.csv")
df<-df[,-c(8:13)]
df$state<-ifelse(is.na(df$state),ave(df$state,FUN=function(x) mean(x,na.rm=(TRUE))),df$state)
df$year<-ifelse(is.na(df$year),ave(df$year,FUN=function(x) mean(x,na.rm=(TRUE))),df$year)
df$murder<-ifelse(is.na(df$murder),ave(df$murder,FUN=function(x) mean(x,na.rm=(TRUE))),df$murder)
df$dacoity<-ifelse(is.na(df$dacoity),ave(df$dacoity,FUN=function(x) mean(x,na.rm=(TRUE))),df$dacoity)
df$riots<-ifelse(is.na(df$riots),ave(df$riots,FUN=function(x) mean(x,na.rm=(TRUE))),df$riots)


#Categorical Data

df$year<-factor(df$year,levels=c(1953:2006),labels=c(1:54))
library(class)
df<-df[5:7]
View(df)
y_pred<-knn(train = df[,-1],test = df[,-1],cl=df[,1],k=5)
View(y_pred)
n<-length(y_pred)
data<-unname(y_pred)
c<-0
for(i in 1:n){ 
  if((data[i])==df[i,1]){
    c=c+1
  }
}
accuracy<-(c*100)/n

##Decision Tree

#Data preprocessing

df<-read.csv("CIv2006.csv")
df<-df[,-c(8:13)]
df$state<-ifelse(is.na(df$state),ave(df$state,FUN=function(x) mean(x,na.rm=(TRUE))),df$state)
df$year<-ifelse(is.na(df$year),ave(df$year,FUN=function(x) mean(x,na.rm=(TRUE))),df$year)
df$murder<-ifelse(is.na(df$murder),ave(df$murder,FUN=function(x) mean(x,na.rm=(TRUE))),df$murder)
df$dacoity<-ifelse(is.na(df$dacoity),ave(df$dacoity,FUN=function(x) mean(x,na.rm=(TRUE))),df$dacoity)
df$riots<-ifelse(is.na(df$riots),ave(df$riots,FUN=function(x) mean(x,na.rm=(TRUE))),df$riots)


#Categorical Data

df$year<-factor(df$year,levels=c(1953:2006),labels=c(1:54))
#spliting data in training and test set

library(caTools)
set.seed(12)
split<-sample.split(df$year,SplitRatio = 0.75)
training_set<-subset(df,split==TRUE)
test_set<-subset(df,split==FALSE)
library(rpart)
classifier=rpart(formula = dacoity ~ murder,data = training_set)
y_pred3<-predict(classifier,newdata = test_set)
n<-length(y_pred3)
data<-unname(y_pred3)
c<-0
for(i in 1:n){
  if(floor(data[i])==df[i,1]){
    c=c+1
  }
}
accuracy<-(c*100)/n


#finding optimal K 
df<-read.csv("CIv2006.csv")
df<-df[,-c(8:13)]
df$state<-ifelse(is.na(df$state),ave(df$state,FUN=function(x) mean(x,na.rm=(TRUE))),df$state)
df$year<-ifelse(is.na(df$year),ave(df$year,FUN=function(x) mean(x,na.rm=(TRUE))),df$year)
df$murder<-ifelse(is.na(df$murder),ave(df$murder,FUN=function(x) mean(x,na.rm=(TRUE))),df$murder)
df$dacoity<-ifelse(is.na(df$dacoity),ave(df$dacoity,FUN=function(x) mean(x,na.rm=(TRUE))),df$dacoity)
df$riots<-ifelse(is.na(df$riots),ave(df$riots,FUN=function(x) mean(x,na.rm=(TRUE))),df$riots)


#Categorical Data

df$year<-factor(df$year,levels=c(1953:2006),labels=c(1:54))
set.seed(6)
X<-df[c(5,6)]
wcss<-vector()
for(i in 1:10)wcss[i]<-sum(kmeans(X,i)$withinss)
plot(1:10,wcss,type="b",main=paste("Cluster of crime"),xlab="No. of clusters",ylab="WCSS")
#k means
X<-df[c(5,6)]
set.seed(365)
kmeans<-kmeans(X,5,iter.max = 300,nstart=10)

#Visualizing cluster
library(cluster) 
clusplot(X,kmeans$cluster,lines = 0,shade=TRUE,color=TRUE,labels=2,plotchar=FALSE,span=TRUE,main=paste('Clusters of crimes'),xlab="X",ylab="Y")




