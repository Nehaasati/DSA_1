
install.packages("rpart")
install.packages("rpart.plot")
install.packages("RColorBrewer")
install.packages("rattle")
install.packages("RGtk2")
install.packages("ISLR")

library(rpart.plot)
library(RColorBrewer)
library(rpart)
library(RGtk2)
library(rattle)
library(ISLR)
rattle()

bank=read.csv("C:\\Users\\ACER\\Desktop\\cust_Bank .csv")
#Convert numerical input to Binary

Bank = data.frame(bank)
Bank$y
#Create Training and testing data set

set.seed(2)
train = sample(1:nrow(Bank),nrow(Bank)/2)
test = -train
training_data = Bank[train,]
testing_data = Bank[test,]
testing_y = Bank$y[test]


#Create a Decision tree with defaults

Bank1 <- rpart( y~age+job+marital+education+housing+loan ,data=training_data,method="class",minsplit = 1)

#Plot decision tree with inbulit functions
plot(Bank1)
text(Bank1, pretty = 1)

#Plot decision tree with Rattle functions
fancyRpartPlot(Bank1)


#Predict test data using decision tree computed
Bank1_pred1 = predict(Bank1, testing_data, type="class")
er1 <-mean(Bank1_pred1 != testing_y) # misclassification error on comparing actual and Predicted values

#Print Accuracy
Accu1<- 1-er1; Accu1


#Print Complexity parameter
#The complexity measure is a combination of the size of a tree and the ability of the 
#tree to separate the classes of the target variable
printcp(Bank1)

#Visualize CP graph
plotcp(Bank1)

#Build a pruned tree based on cp
pBank1<- prune(Bank1,cp= Bank1$cptable[which.min(Bank1$cptable[,"xerror"]),"CP"])


#Plot decision tree with Rattle functions
fancyRpartPlot(Bank1)

#Predict test data using pruned decision tree computed
Bank_predp1 = predict(pBank1, testing_data, type="class")
erp1<-mean(Bank_predp1 != testing_y) # misclassification error
Accup1<- 1-erp1; Accup1


#Create a Decision tree with minsplit and Minbucket parameters
#minsplit is the minimum number of observations that must exist in a node in order for a split to be attempted
#minbucket is the minimum number of observations in any terminal node
Bank2 <- rpart(y~age+job+marital+education+housing+loan,data=training_data,method="class", minsplit = 1, minbucket = 1)
fancyRpartPlot(Bank2)
Bank_pred2 = predict(Bank2, testing_data, type="class")
er2 <-mean(Bank_pred2 != testing_y) # misclassification error
Accu2<- 1-er2; Accu2


#rpart by default uses ginin score to create leaf nodes. Create a decision tree using information gain
Bank3 <- rpart(y~age+job+marital+education+housing+loan,data=training_data,method="class", parms = list(split = 'information'))
fancyRpartPlot(Bank3)

Bank_pred3 = predict(Bank3, testing_data, type="class")
er3 <-mean(Bank_pred3 != testing_y) # misclassification error
Accu3<- 1-er3; Accu3



printcp(Bank3)
plotcp(Bank3)
pBank3<- prune(Bank3,cp= Bank3$cptable[which.min(Bank3$cptable[,"xerror"]),"CP"])
fancyRpartPlot(pBank3)


Bank_predp3 = predict(pBank3, testing_data, type="class")
erp3 <-mean(Bank_predp3 != testing_y) # misclassification error
Accup3<- 1-erp3; Accup3







###############################################Random Forests##########################################################
#install.packages("randomForest")
library(randomForest)
#remove.packages(c("ggplot2", "data.table"))
#install.packages('Rcpp', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)
#install.packages('data.table', dependencies = TRUE)
#install.packages("caret")
library(caret)

Bankrf <- randomForest(y~age+job+marital+education+housing+loan,data=training_data)

Bankrf

plot(Bankrf)
legend("topright", colnames(Bankrf$err.rate),col=1:4,cex=0.8,fill=1:4)

Bank_predrf = predict(Bankrf, testing_data, type="class")
errf <-mean(Bank_predrf != testing_y) # misclassification error
Accurf<- 1-errf; Accurf



#Random forest with crossvalidation

control <- trainControl(method = "cv", number = 4)

#Use mtry to give  number of columns that needs to be randoly selected, default is Sqrt of total number of columns
Bankrfcv <- randomForest(y~age+job+marital+education+housing+loan,data=training_data,trControl = control, method = "RF")


print(Bankrfcv)

#The mean decrease in Gini coefficient is a measure of how each variable contributes to the homogeneity of the nodes 
#and leaves in the resulting random forest. Each time a particular variable is used to split a node, 
#the Gini coefficient for the child nodes are calculated and compared to that of the original node
varImpPlot(Bankrfcv,sort=TRUE)


Bank_predrfcv = predict(Bankrfcv, testing_data, type="class")
errfcv <-mean(Bank_predrfcv != testing_y) # misclassification error
Accurfcv<- 1-errfcv; Accurfcv


##########################################Bayes Model####################################################################


#install.packages("e1071")
library(e1071)

modelby <- naiveBayes(y~age+job+marital+education+housing+loan, data = training_data)


predby  = predict(modelby,testing_data)
table(predby)

erby <-mean(predby != testing_y) # misclassification error
Accuby<- 1-erby; Accuby




##########################################Logistic####################################################################


library(pscl)


logit=glm(y~age+job+marital+education+housing+loan,family= "binomial",data=training_data)
summary(logit)


# Confusion Matrix Table

prob=predict(logit,testing_data)
confusion<-table(prob>0.5,testing_data$y)
confusion<-table(prob>0.4,testing_data$y)
confusion<-table(prob>0.35,testing_data$y)
confusion<-table(prob>0.3,testing_data$y)
rownames(confusion) <- c("0", "1")
confusion


# Model Accuracy

AccuracyLg<-sum(diag(confusion))/sum(confusion)
AccuracyLg

#################################################### Compare the models################################################################


#Compare all the accuracies and determine which is the best fit
choice <- as.data.frame(rbind(Accu1,Accup1,Accu2,Accu3,Accup3,Accurf,Accurfcv,Accuby,AccuracyLg))
maxm <- max(choice[,1]);maxm
choice








