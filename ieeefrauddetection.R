
#RUN THE DATA AS PARTICULAR PARTITIONS WE WILL GET THE OUTPUT.

#INSTALLING THE REQUIRED PACKAGES AND LIBRARIES.

install.packages("e1071")
install.packages("randomForest")
library(ggplot2)
library(randomForest)
library(caret)
library(dplyr)
library(tibble)

# LOADING THE DATA
train_id=read.csv(file = "train_identity.csv",na.strings = "")
train_transc=read.csv(file = "train_transaction.csv",na.strings = "")
test_id=read.csv(file = "test_identity.csv",na.strings = "")
test_transc=read.csv("test_transaction.csv",na.strings = "")

#ANALYSING THE DATA

dim(train_id)
dim(train_transc)
dim(test_id)
dim(test_transc)
head(train_id)
head(train_transc)
names(train_transc)

#CONVERTING ALL THE DIFFERENT DATA TYPES TO FACTOR TO APPLY ML ALGORITHMS
train_transc[,"isFraud"]=factor(train_transc[,"isFraud"])
fraud<-train_transc$isFraud
test_transc$fraud<-NA
?select
#RE ARRANGING THE COLUMNS TO MERGE DIFFERENT DATA SETS
train_transc=select(train_transc, ,-2)
train_transc<-add_column(train_transc, fraud, .after = "V339")
train_transc$label="train"
test_transc$label="test"
train=merge(x = train_id,y = train_transc, by = "TransactionID")
dim(train)
test=merge(x = test_id,y = test_transc, by = "TransactionID")
dim(test)
dim(train)


# will merge the train and test using label "train" and "test"


names(test)
names(train)
full<- rbind(test,train)
str(full)

#FINDING MISSING VALUES FOR DATA CLEANING 

missingPercent=(colSums(is.na(full))/nrow(full))*100
missPercentDF=data.frame(colnames(full),missingPercent)
colnames(missPercentDF)<-c("Variable","MissPercentage")
missPercentDF=missPercentDF[order(missPercentDF$MissPercentage,decreasing = TRUE), ]
missPercentDF
plot(missPercentDF$MissPercentage, ylab = "% Missing", main = "Percentage of Missing data")

# remove columns which having more than 50% missing value as to reduce deviation w.r.t class label 

full<-full[missingPercent<50]
str(full)
partialNAcol<-(full)[!colSums(is.na(full))==0]
partialNAcol<-colnames(partialNAcol)

for(f in partialNAcol){
  if(any(is.na(full[[f]]))){
    if(is.factor(full[,f])){
      
      full[,f] <- as.character(full[,f])
      full[,f][is.na(full[,f])]<-"Unknown"
      full[,f] <- factor(full[,f])
      
      
    }
    else{
      
      full[is.na(full[,f]),f]<- mean(full[,f], na.rm=TRUE)
    }
  }}

#We can convert Fraud,ProductCD,card1 - card6,addr1, addr2,Pemaildomain, Remaildomain,M1 - M9 into factors.
# AS LARGE DATASET WE HAD ISSUE IN USING FOR LOOP SO :

full[,"card1"]=factor(full[,"card1"])
full[,"card2"]=factor(full[,"card2"])
full[,"card3"]=factor(full[,"card3"])
full[,"card4"]=factor(full[,"card4"])
full[,"card5"]=factor(full[,"card5"])
full[,"card6"]=factor(full[,"card6"])
full[,"addr1"]=factor(full[,"addr1"])
full[,"addr2"]=factor(full[,"addr2"])
full[,"P_emaildomain"]=factor(full[,"P_emaildomain"])
full[,"R_emaildomain"]=factor(full[,"R_emaildomain"])
full[,"DeviceType"]=factor(full[,"DeviceType"])
full[,"DeviceInfo"]=factor(full[,"DeviceInfo"])
full[,"id.12"]=factor(full[,"id.12"]) 
full[,"id.13"]=factor(full[,"id.13"]) 
full[,"id.14"]=factor(full[,"id.14"]) 
full[,"id.15"]=factor(full[,"id.15"])
full[,"id.16"]=factor(full[,"id.16"]) 
full[,"id.17"]=factor(full[,"id.17"])
full[,"id.19"]=factor(full[,"id.19"])
full[,"id.20"]=factor(full[,"id.20"]) 
full[,"id.28"]=factor(full[,"id.28"]) 
full[,"id.29"]=factor(full[,"id.29"]) 
full[,"id.30"]=factor(full[,"id.30"]) 
full[,"id.31"]=factor(full[,"id.31"]) 
full[,"id.32"]=factor(full[,"id.32"]) 
full[,"id.33"]=factor(full[,"id.33"])
full[,"id.34"]=factor(full[,"id.34"])
full[,"id.35"]=factor(full[,"id.35"]) 
full[,"id.36"]=factor(full[,"id.36"]) 
full[,"id.37"]=factor(full[,"id.37"]) 
full[,"id.38"]=factor(full[,"id.38"]) 
  
  

 #PREPROCESSING 
  
 
names <- full$DeviceInfo
full$DeviceInfo <-  factor(gsub("([A-Za-z]+).*",  "\\1", names,ignore.case = FALSE))
table(full$DeviceInfo)

names <- full$id.30
full$id.30 <-  factor(gsub("([A-Za-z]+).*",  "\\1", names,ignore.case = FALSE))

table(full$id.30)

full$card4= recode_factor(full$card4, 'american express' = "OTHER",  'discover' = "OTHER", 
                          'visa' = "visa",   'mastercard' = "mastercard",   .default = "OTHER")
full$card6 = recode_factor(full$card6,'credit' = "credit", 
                           'debit' = "debit",
                           .default = "OTHER")
full$fraud=factor(x = full$fraud,exclude = "Unknown" )
str(full)
colnames(full)

train=subset(full,label=="train")
str(train)
test=subset(full,label=="test")

#BINARY CLASSIFICATION

ggplot(train,aes(x = factor(ProductCD),fill=fraud))+geom_bar(position = "fill")

ggplot(train,aes(factor(card1),fill=factor(fraud)))+geom_bar(position = "fill")+coord_flip()

ggplot(train,aes(factor(card2),fill=factor(fraud)))+geom_bar(position = "fill")+coord_flip()

ggplot(train,aes(factor(card3),fill=factor(fraud)))+geom_bar(position = "fill")+coord_flip()

ggplot(train,aes(factor(card4),fill=factor(fraud)))+geom_bar()

ggplot(train,aes(factor(card5),fill=factor(fraud)))+geom_bar()+coord_flip()

ggplot(train,aes(factor(card6),fill=factor(fraud)))+geom_bar()

ggplot(train,aes(x = factor(P_emaildomain),fill=fraud))+geom_bar()+coord_flip()

ggplot(train,aes(x = factor(R_emaildomain),fill=fraud))+geom_bar()+coord_flip()

ggplot(train,aes(x = factor(ProductCD),fill=fraud))+geom_bar(position = "fill")

ggplot(train,aes(x = factor(id.30),fill=fraud))+geom_bar(position = "fill")+coord_flip()

#BOX PLOTING TO FIND OUTLIERS

ggplot(train,aes(factor(fraud),TransactionAmt))+geom_boxplot()


numcols=colnames(full[,sapply(full, is.numeric)])
numdata=full[,numcols]
table(train$fraud)

# Prep Training and Test data.
set.seed(100)
trainDataIndex <- createDataPartition(train$fraud, p=.7, list = F)  # 70% training data
trainData <- train[trainDataIndex, ]
testData <- train[-trainDataIndex, ]


#SAMPLE WITHOUT PRE-PROCESSING
set.seed(100)
down_train<-sample(train_transc,replace=FALSE,prob=NULL)
logit1<-glm(formula = fraud~ProductCD+card4+card6,data = down_train,family = "binomial")
summary(logit1)

#  SAMPLE WITH PRE-PROCESSING

set.seed(100)

trainDataset1=select(trainData, ,-394)
colnames(trainDataset1)
down_train1<-sample(trainData,replace=FALSE,prob=NULL)


#LINEAR REGRESSION

logit<-glm(formula = fraud~ProductCD+card4+card6+DeviceType,data = down_train1,family = "binomial")
summary(logit)
pred <- predict(logit, newdata = testData, type = "response")
y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
testData$fraud[testData$fraud==""] <- 0
y_act <- testData$fraud
y_act <- factor(y_act, levels=c(0, 1))

confusionMatrix(y_act,y_pred)

#PREDICTING THE DATA AND WRITE IT INTO CSV FILE

library(data.table)
testpredict= predict(logit, newdata = test, type = "response")
solution <- data.frame( TransactionID = test$TransactionID , fraud = testpredict)
write.csv( solution , file = 'FraudDetection.csv' , row.names = FALSE )
