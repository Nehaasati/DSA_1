#install.packages("dplyr")
#install.packages("psy")
#install.packages("GPArotation")
#install.packages("dplyr")
#install.packages("ggplot2")

library(dplyr)
library(VIM)
library(mice)
library(missForest)
library(psych)
library(psy)
library(GPArotation)
library(zipcode)
library(ggplot2)
library(caret)
library(car)
setwd("C://Users//gasati//Documents//neha peoj")
mydata11=read.csv("Dataset-1.csv",stringsAsFactors = F)

set.seed(121)
train = sample(1:nrow(mydata),nrow(mydata)/2)
test = -train
training_data = mydata11[train,]
testing_data = mydata11
testing_salary = mydata11$salary[test]



glimpse(mydata11)
View(mydata11)
install.packages("VIM")


boxplot(mydata11$price, horizontal= TRUE)
boxplot(mydata11$bedrooms,horizontal = TRUE)
boxplot(mydata11$bathrooms,horizontal = TRUE)
boxplot(mydata11$floors,horizontal = TRUE)
boxplot(mydata11$grade,horizontal = TRUE)
md.pattern(mydata11)
#plot missing value pattern
mice_plot <- aggr(mydata11, col=c('grey','red'),numbers=TRUE, sortVars=TRUE,labels=names(mydata11), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#Start MICE imputing
imputed_Data <- mice(mydata11, m=5, maxit = 2, method = 'cart')
summary(imputed_Data)

#Check complete data using one impute set
mydata11 <- complete(imputed_Data,3)
View(mydata11)
glimpse(mydata11)
#Check hitogram

summary(mydata11)
histinf <-hist(x=mydata11$sqft_lot, breaks=4 , plot =TRUE, col= "lightblue", freq= TRUE,  main = paste("Histogram of Subjects", lables= TRUE))
mice_plot <- aggr(mydata11, col=c('grey','red'),numbers=TRUE, sortVars=TRUE,labels=names(mydata11), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))



corMat <- cor(mydata11)
corMat
n.factors <-9
# number of fac
summary(mydata11)
mydata1<-mydata11[,2:15]


#FA with orthogonal rotation- Varimax
fit <- factanal(mydata1, 
                n.factors,scores="regression",lower = 0.0001)     # 'varimax' is an ortho rotation
fit
load <- fit$loadings[,1:9] 
fit$scores
data1<-fit$scores
plot(load,type="n") # set up plot 
text(load,labels=names(mydata11),cex=.7) # add variable names
load
scree.plot(fit$correlation)




model<-lm(price~ +bedrooms+bathrooms+sqft_living+sqft_lot+floors+view+grade+yr_built+zipcode+sqft_basement+waterfront,data=mydata11)
summary(model)

model1<-lm(price~ +bedrooms+bathrooms+sqft_living+floors+view+grade+zipcode+yr_built+waterfront,data=mydata11)
summary(model1)

mydata11$zipcode<-as.character( mydata11$zipcode)

  dmy <- dummyVars(" ~ zipcode", data = mydata11, fullRank=T)
  trsf <- data.frame(predict(dmy, newdata = mydata11))
  zip1<-data.frame(trsf)
  mydata12<-cbind(mydata11,zip1)
  model12<-lm(price~.,data=mydata12)
  summary(model12)
  set.seed(121)
  train = sample(1:nrow(mydata12),nrow(mydata12)/2)
  test = -train
  training_data = mydata12[train,]
  testing_data = mydata12
  testing_salary = mydata12$salary[test]
  model12<-lm(price~.,data=training_data)
  summary(model12)
  View(mydata12)
  
  
 mydata12$yr =2019- mydata11$yr_renovated 

mydata11=CreateDummies(mydata11,"grade",5)

df<-data.frame(mydata11$grade_4,mydata11$grade_12,mydata11$grade_5,mydata11$grade_11,mydata11$grade_10,mydata11$grade_9,mydata11$grade_7)
mydata13<-cbind(mydata12,df)

new_predict = predict.lm(model,testing_data,type = 'response')

#Error estimation
rmse(testing_data$price,new_predict)
mape(testing_data$price,new_predict)


