setwd("C:/Users/LENOVO/Desktop/A20")
library(gWidgets)
library(gWidgetstcltk)
win <- gwindow("Results",width = 1000,height = 1000)
grp1<-ggroup(container = win)
grp2<-ggroup(container = win)
grp3<-ggroup(container = win)
grp4<-ggroup(container = win)
grp5<-ggroup(container = win)
grp6<-ggroup(container = win)
grp7<-ggroup(container = win)
grp8<-ggroup(container = win)
grp9<-ggroup(container = win)
grp10<-ggroup(container = win)
grp11<-ggroup(container = win)
grp12<-ggroup(container = win)
grp13<-ggroup(container = win)
btn_PolyReg <- gbutton(
  text      = "Polynomial Regression",
  container = grp1,
  handler   = function(h, ...)
  {
    df<-read.csv("CIv2006.csv")
    df<-df[,-c(8:13)]
    df$state<-ifelse(is.na(df$state),ave(df$state,FUN=function(x) mean(x,na.rm=(TRUE))),df$state)
    df$year<-ifelse(is.na(df$year),ave(df$year,FUN=function(x) mean(x,na.rm=(TRUE))),df$year)
    df$murder<-ifelse(is.na(df$murder),ave(df$murder,FUN=function(x) mean(x,na.rm=(TRUE))),df$murder)
    df$dacoity<-ifelse(is.na(df$dacoity),ave(df$dacoity,FUN=function(x) mean(x,na.rm=(TRUE))),df$dacoity)
    df$riots<-ifelse(is.na(df$riots),ave(df$riots,FUN=function(x) mean(x,na.rm=(TRUE))),df$riots)
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
    svalue(obj1)<-accuracy
  
  }
)

lab1<-glabel("Accuracy: ", container=grp2)
obj1<-glabel("", container = grp2)

btn_MultiReg <- gbutton(
  text      = "Multiple Linear Regression",
  container = grp3,
  handler   = function(h, ...)
  {
    
    df<-read.csv("CIv2006.csv")
    df<-df[,-c(8:13)]
    df$state<-ifelse(is.na(df$state),ave(df$state,FUN=function(x) mean(x,na.rm=(TRUE))),df$state)
    df$year<-ifelse(is.na(df$year),ave(df$year,FUN=function(x) mean(x,na.rm=(TRUE))),df$year)
    df$murder<-ifelse(is.na(df$murder),ave(df$murder,FUN=function(x) mean(x,na.rm=(TRUE))),df$murder)
    df$dacoity<-ifelse(is.na(df$dacoity),ave(df$dacoity,FUN=function(x) mean(x,na.rm=(TRUE))),df$dacoity)
    df$riots<-ifelse(is.na(df$riots),ave(df$riots,FUN=function(x) mean(x,na.rm=(TRUE))),df$riots)
    df$year<-factor(df$year,levels=c(1953:2006),labels=c(1:54))
    
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
    svalue(obj2)<-accuracy
  }
)


lab2<-glabel("Accuracy: ", container=grp4)
obj2<-glabel("", container = grp4)

btn_LinearReg <- gbutton(
  text      = "Linear Regression",
  container = grp5,
  handler   = function(h, ...)
  {
    df<-read.csv("CIv2006.csv")
    df<-df[,-c(8:13)]
    df$state<-ifelse(is.na(df$state),ave(df$state,FUN=function(x) mean(x,na.rm=(TRUE))),df$state)
    df$year<-ifelse(is.na(df$year),ave(df$year,FUN=function(x) mean(x,na.rm=(TRUE))),df$year)
    df$murder<-ifelse(is.na(df$murder),ave(df$murder,FUN=function(x) mean(x,na.rm=(TRUE))),df$murder)
    df$dacoity<-ifelse(is.na(df$dacoity),ave(df$dacoity,FUN=function(x) mean(x,na.rm=(TRUE))),df$dacoity)
    df$riots<-ifelse(is.na(df$riots),ave(df$riots,FUN=function(x) mean(x,na.rm=(TRUE))),df$riots)
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
    svalue(obj3)<-accuracy
  }
)


lab3<-glabel("Accuracy: ", container=grp6)
obj3<-glabel("", container = grp6)

btn_Decision<- gbutton(
  text      = "Decision Tree",
  container = grp8,
  handler   = function(h, ...)
  {
    df<-read.csv("CIv2006.csv")
    df<-df[,-c(8:13)]
    df$state<-ifelse(is.na(df$state),ave(df$state,FUN=function(x) mean(x,na.rm=(TRUE))),df$state)
    df$year<-ifelse(is.na(df$year),ave(df$year,FUN=function(x) mean(x,na.rm=(TRUE))),df$year)
    df$murder<-ifelse(is.na(df$murder),ave(df$murder,FUN=function(x) mean(x,na.rm=(TRUE))),df$murder)
    df$dacoity<-ifelse(is.na(df$dacoity),ave(df$dacoity,FUN=function(x) mean(x,na.rm=(TRUE))),df$dacoity)
    df$riots<-ifelse(is.na(df$riots),ave(df$riots,FUN=function(x) mean(x,na.rm=(TRUE))),df$riots)
    df$year<-factor(df$year,levels=c(1953:2006),labels=c(1:54))
    
    
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
    svalue(obj4)<-accuracy
  }
)

lab4<-glabel("Accuracy: ", container=grp9)
obj4<-glabel("", container = grp9)

btn_knn<- gbutton(
  text      = "K-NN",
  container = grp10,
  handler   = function(h, ...)
  {
    
    df<-read.csv("CIv2006.csv")
    df<-df[,-c(8:13)]
    df$state<-ifelse(is.na(df$state),ave(df$state,FUN=function(x) mean(x,na.rm=(TRUE))),df$state)
    df$year<-ifelse(is.na(df$year),ave(df$year,FUN=function(x) mean(x,na.rm=(TRUE))),df$year)
    df$murder<-ifelse(is.na(df$murder),ave(df$murder,FUN=function(x) mean(x,na.rm=(TRUE))),df$murder)
    df$dacoity<-ifelse(is.na(df$dacoity),ave(df$dacoity,FUN=function(x) mean(x,na.rm=(TRUE))),df$dacoity)
    df$riots<-ifelse(is.na(df$riots),ave(df$riots,FUN=function(x) mean(x,na.rm=(TRUE))),df$riots)
    df$year<-factor(df$year,levels=c(1953:2006),labels=c(1:54))
    
    library(class)
    df<-df[5:7]
    y_pred<-knn(train = df[,-1],test = df[,-1],cl=df[,1],k=5)
    n<-length(y_pred)
    data<-unname(y_pred)
    c<-0
    for(i in 1:n){ 
      if((data[i])==df[i,1]){
        c=c+1
      }
    }
    accuracy<-(c*100)/n
    svalue(obj5)<-accuracy
  }
)

lab5<-glabel("Accuracy: ", container=grp11)
obj5<-glabel("", container = grp11)

btn_PolyReg <- gbutton(
  text      = "Support Vector Regression",
  container = grp12,
  handler   = function(h, ...)
  {
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
    accuracy<-(c*100)/n
    svalue(obj6)<-accuracy
  }
)

lab6<-glabel("Accuracy: ", container=grp13)
obj6<-glabel("", container = grp13)
