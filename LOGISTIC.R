#LOGISTI REGRESSION
#AIM:TO DETERMINE IF A PATIENT IS DIABETIC OR NOT


#READ THE DATASET ON DIABETES & VIEW
dd<-read.csv("D:/dataset/archive/diabetes.csv",header = TRUE)
View(dd)

#SPLIT THE DATASET
install.packages("caTools")
library("caTools")
splt<-sample.split(dd,SplitRatio = 0.8)
splt
train<-subset(dd,splt=="TRUE")
test<-subset(dd,splt=="FALSE")

#CREATE LOGISTIC REGRESSION MODEL
modl<-glm(outcome~.,train,family = "binomial")
summary(modl)

#SKIN IS INSIGNIFICANT INDEPENDENT VARIABLE
modl<-glm(outcome~.-skin,train,family = "binomial")
summary(modl)

#TESTING MODEL
resp<-predict(modl,test,type = "response")
resp
test

#CONFUSION MATRIX
table(Actualvalue=test$outcome,Predictedvalue=resp>0.5)

#LOGISTIC PLOT
install.packages("ggplot2")
library("ggplot2")
ggplot(dd,aes(x=glu,y=outcome))+geom_point(alpha=0.5)+stat_smooth(method="glm",se=FALSE,method.args = list(family=binomial),col="green")

#HOW TO FIND THRESHOLD
resp<-predict(modl,train,type = "response")
library("ROCR")
ROPred=prediction(resp,train$outcome)
ROPref<-performance(ROPred,"tpr","fpr")
plot(ROPref,colorize=TRUE,print.cutoffs.at=seq(0.1,by=0.1))

#TEST AGAIN
resp<-predict(modl,test,type = "response")
table(Actualvalue=test$outcome,Predictedvalue=resp>0.5)
table(Actualvalue=test$outcome,Predictedvalue=resp>0.4)
