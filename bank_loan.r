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
setwd("C://Users//gasati//Documents//neha peoj")
bank=read.csv("cust_bank.csv",stringsAsFactors = F)



#Create Training and testing data set

set.seed(121)
train = sample(1:nrow(bank),nrow(bank)/2)
test = -train
training_data = bank[train,]
testing_data = bank[test,]
testing_High = bank$y[test]


#Create a Decision tree with defaults
tree1 <- rpart(y~age+job+marital+education+default+housing+loan,data=training_data,method="class")

#Plot decision tree with inbulit functions
plot(tree1)
text(tree1, pretty = 0)

#Plot decision tree with Rattle functions
fancyRpartPlot(tree1)


#Predict test data using decision tree computed
tree_pred1 = predict(tree1, testing_data, type="class")
er1 <-mean(tree_pred1 !=testing_High) # misclassification error on comparing actual and Predicted values

#Print Accuracy
Accu1<- 1-er1; Accu1


#Print Complexity parameter
#The complexity measure is a combination of the size of a tree and the ability of the 
#tree to separate the classes of the target variable
printcp(tree1)

#Visualize CP graph
plotcp(tree1)

#Build a pruned tree based on cp
ptree1<- prune(tree1,cp= tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"])


#Plot decision tree with Rattle functions
fancyRpartPlot(ptree1)

#Predict test data using pruned decision tree computed
tree_predp1 = predict(ptree1, testing_data, type="class")
erp1<-mean(tree_predp1 != testing_High) # misclassification error
Accup1<- 1-erp1; Accup1


#Create a Decision tree with minsplit and Minbucket parameters
#minsplit is the minimum number of observations that must exist in a node in order for a split to be attempted
#minbucket is the minimum number of observations in any terminal node
tree2 <- rpart(y ~ age+job+marital+education+default+housing+loan,data=training_data,method="class", minsplit = 8, minbucket = 1)
fancyRpartPlot(tree2)
tree_pred2 = predict(tree2, testing_data, type="class")
er2 <-mean(tree_pred2 != testing_High) # misclassification error
Accu2<- 1-er2; Accu2


#rpart by default uses ginin score to create leaf nodes. Create a decision tree using information gain
tree3 <- rpart(y ~ age+job+marital+education+default+housing+loan,data=training_data,method="class", parms = list(split = 'information'))
fancyRpartPlot(tree3)

tree_pred3 = predict(tree3, testing_data, type="class")
er3 <-mean(tree_pred3 != testing_High) # misclassification error
Accu3<- 1-er3; Accu3



printcp(tree3)
plotcp(tree3)
ptree3<- prune(tree3,cp= tree3$cptable[which.min(tree3$cptable[,"xerror"]),"CP"])
fancyRpartPlot(ptree3)


tree_predp3 = predict(ptree3, testing_data, type="class")
erp3 <-mean(tree_predp3 != testing_High) # misclassification error
Accup3<- 1-erp3; Accup3







###############################################Random Forests##########################################################
install.packages("randomForest")
library(randomForest)
remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
install.packages("caret")
library(caret)
summary(bank)
dmy <- dummyVars(" ~y", data = bank, fullRank=T)
trsf <- data.frame(predict(dmy, newdata = bank))
y1<-data.frame(trsf)
bank1<-cbind(bank,y1)
set.seed(121)
train1 = sample(1:nrow(bank),nrow(bank1)/2)
test1 = -train
training_data1 = bank1[train,]
testing_data1 = bank1[test,]
testing_High1 = bank1$y[test]


treerf <- randomForest(yyes~age+job+marital+education+housing+loan,data=training_data1)
bank1$y1
View(bank1)
treerf

plot(treerf)
legend("topright", colnames(treerf$err.rate),col=1:4,cex=0.8,fill=1:4)

tree_predrf = predict(treerf, testing_data, type="class")
errf <-mean(tree_predrf != testing_High) # misclassification error
Accurf<- 1-errf; Accurf



#Random forest with crossvalidation

control <- trainControl(method = "cv", number = 5)


treerfcv <- randomForest(y ~age+job+marital+education+default+housing+loan ,data=training_data,trControl = control, method = "RF")


print(treerfcv)

#The mean decrease in Gini coefficient is a measure of how each variable contributes to the homogeneity of the nodes 
#and leaves in the resulting random forest. Each time a particular variable is used to split a node, 
#the Gini coefficient for the child nodes are calculated and compared to that of the original node
varImpPlot(treerfcv,sort=TRUE)


tree_predrfcv = predict(treerfcv, testing_data, type="class")
errfcv <-mean(tree_predrfcv != testing_High) # misclassification error
Accurfcv<- 1-errfcv; Accurfcv


##########################################Bayes Model####################################################################


install.packages("e1071")
library(e1071)

modelby <- naiveBayes(y ~age+job+marital+education+default+housing+loan,  data = training_data)
 

predby  = predict(modelby,testing_data)
table(predby)

erby <-mean(predby != testing_High) # misclassification error
Accuby<- 1-erby; Accuby




##########################################Logistic####################################################################


library(pscl)


logit=glm(High ~ Income + Advertising + Population + Price + CompPrice + Age + Education + Urban + US + ShelveLoc,family= "binomial",data=training_data)
summary(logit)


# Confusion Matrix Table

prob=predict(logit,testing_data)
confusion<-table(prob>0.5,testing_data$High)
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



####################################################Association Rule Mining################################################################
#install.packages("arules")
library(arules)

summary(Carseats$CompPrice)
b <- factor(cut(Carseats$CompPrice, c(70,80,90,100,110,120,130,140,150,160,170,180)))

summary(Carseats$Income)
c <- factor(cut(Carseats$Income, c(20,40,60,80,100,120)))

summary(Carseats$Advertising)
d <- factor(cut(Carseats$Advertising, c(5,10,15,20,25,30)))

summary(Carseats$Population)
e <- factor(cut(Carseats$Population, c(100,200,300,400,500)))

summary(Carseats$Price)
f <- factor(cut(Carseats$Price, c(25,100,125,150,175,200)))

summary(Carseats$ShelveLoc)
g <- factor(Carseats$ShelveLoc)


summary(Carseats$Age)
h <- factor(cut(Carseats$Price, c(25,30,35,40,45,50,55,60,65,70,75,80)))

summary(Carseats$Education)

i <- factor(cut(Carseats$Education, c(10,12,14,16,18,20)))

summary(Carseats$Urban)
j <- factor(Carseats$Urban)

summary(Carseats$US)
k <- factor(Carseats$US)

summary(Carseats$High)
l <- factor(Carseats$High)

input <- as.data.frame(cbind(b,c,d,e,f,g,h,i,j,k,l))
input2 <- data.frame(apply(input, 2, as.factor))

rules <- apriori(input2)

inspect(rules)

rules2 <- apriori(input2,parameter = list(minlen=4, supp=0.005, conf=0.8),appearance = list(rhs=c("l=2"), default="lhs"))
rules.sorted <- sort(rules2, by="confidence")
inspect(rules2)


