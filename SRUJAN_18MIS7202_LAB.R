#TERALA SRUJAN 18MIS7202-LAB EXPERIMENT

install.packages("Hmisc")
library(caTools)  
library(Hmisc)
library(e1071)

data<-read.csv("loans_data_Apply_Linear_ Regression.csv")
dim(data)
data <- na.omit(data)
#pre-processing
#data$Amount.Requested 
data$Amount.Requested<-as.numeric(as.character(data$Amount.Requested,na.rm = TRUE),na.rm = TRUE)
NOT<-which(is.na(data$Amount.Requested))
NOT
describe(data$Amount.Requested)
skewness(data$Amount.Requested,na.rm = T)   
#Skewness=0.911062
# positive skweness, Therefore filling missing values with median
data[NOT,]<-median(data$Amount.Requested,na.rm = T)  
table(data$Amount.Requested)

#data$Amount.Funded.By.Investors    

data$Amount.Funded.By.Investors<-as.numeric(as.character(data$Amount.Funded.By.Investors,na.rm = TRUE),na.rm = TRUE)
NOT1<-which(is.na(data$Amount.Funded.By.Investors))
NOT1
describe(data$Amount.Funded.By.Investors)
skewness(data$Amount.Funded.By.Investors,na.rm = T)   
#Skewness=0.9319742
# positive skweness, Therefore filling missing values with median
data[NOT1,]<-median(data$Amount.Funded.By.Investors,na.rm = T)  
table(data$Amount.Funded.By.Investors)
typeof(data$Amount.Funded.By.Investors)

#data$Loan.Length   
#converting to factor
data$Loan.Length <- as.factor(data$Loan.Length)
# replacing dot values with NA
data$Loan.Length <- gsub(".", NA, data$Loan.Length, fixed = TRUE) 
data$Loan.Length

#data$open.CREDIT.Lines
# converting to numeric
skewness(data$Open.CREDIT.Lines)
#skewness=-0.09320306
data$Open.CREDIT.Lines <- as.numeric((data$Open.CREDIT.Lines))                                        
data$Open.CREDIT.Lines <- ifelse(is.na(data$Open.CREDIT.Lines),
                                 ave(data$Open.CREDIT.Lines, FUN = function(x) median(x,
                                                                                    na.rm = 'TRUE')),
                                 data$Open.CREDIT.Lines)
data$Open.CREDIT.Lines

#data$interest rate
skewness(data$Interest.Rate)
#skewness= 0.1099921
# converting to factor
data$Interest.Rate <- as.factor(data$Interest.Rate)            
data$Interest.Rate <- ifelse(is.na(data$Interest.Rate), ave(data$Interest.Rate, FUN = function(x) mean(x, na.rm = 'TRUE')),data$Interest.Rate)          # filling missing values with mean


#data$Home.Ownership  
#Converting data to numeric
data$Home.Ownership = factor(data$Home.Ownership, levels = c('MORTGAGE','RENT','OWN'), labels = c(1,2,3)) 
data$Home.Ownership <- na.omit(data$Home.Ownership)       


#data$Monthly.Income    
data$Monthly.Income  
data$Monthly.Income<-as.character(data$Monthly.Income,na.rm = TRUE)
data$Monthly.Income<-as.numeric(data$Monthly.Income,na.rm = TRUE)
NOT3<-which(is.na(data$Monthly.Income))
NOT3
describe(data$Monthly.Income)
skewness(data$Monthly.Income,na.rm = T)   
#Skewness=8.388158
# positive skweness, filling missing values with median
data[NOT3,]<-median(data$Monthly.Income,na.rm = T)  
table(data$Monthly.Income)
typeof(data$Monthly.Income)

#data$Inquiries.in.the.Last.6.Months
skewness(data$Inquiries.in.the.Last.6.Months,na.rm = T) 
#skewness= 2.044432
data$Inquiries.in.the.Last.6.Months <- ifelse(is.na(data$Inquiries.in.the.Last.6.Months), 
                                        ave(data$Inquiries.in.the.Last.6.Months,
                                        FUN = function(x) median(x, na.rm = 'TRUE')),data$Inquiries.in.the.Last.6.Months)    

#data$Revolving.CREDIT.Balance     

data$Revolving.CREDIT.Balance<-as.numeric(as.character(data$Revolving.CREDIT.Balance,na.rm = TRUE),na.rm = TRUE)
Q<-which(is.na(data$Revolving.CREDIT.Balance))
Q<-c(70,367,571,1595,2499)
describe(data$Revolving.CREDIT.Balance)
skewness(data$Revolving.CREDIT.Balance,na.rm = T)   
which(data$Revolving.CREDIT.Balance==10979)
#Skewness=5.365805
# positive skweness,  filling missing values with median
data[Q,]<-median(data$Revolving.CREDIT.Balance,na.rm = T) 
table(data$Revolving.CREDIT.Balance)
typeof(data$Revolving.CREDIT.Balance)
colnames(data)
data$Debt.To.Income.Ratio <- as.factor(data$Debt.To.Income.Ratio)
data <- na.omit(data)        
data 


#spliting into train_data and test_data
set.seed(100)        
sample  = sample.split(data,SplitRatio = 0.6)
train_data = subset(data, sample == TRUE)        
test_data = subset(data, sample == FALSE)         

#applying multilinear
multi1<-lm(Interest.Rate~Amount.Requested + Loan.Length + Monthly.Income+Amount.Funded.By.Investors + Debt.To.Income.Ratio + Open.CREDIT.Lines + Home.Ownership,test_data)
summary(multi1)  
#obtaining r^2 value
summary(multi1)$r.squared  
