#-----------------------------------------#
# Project Bank Job #
#-----------------------------------------#

# Nueral Nets with 1) Categorical predictors as dummies 2) Binary output varible as dummy

#Imports
library(neuralnet)
library(scales)
library(dummies)
library(caret)
library(forecast)


defaulter.df <- read.csv(file="Default.csv",header=TRUE) #Load data
summary(defaulter.df) 

defaulter.df<-defaulter.df[,-1] #Removed ID

defaulter.df$SEX<-as.factor(defaulter.df$SEX)
defaulter.df$EDUCATION<-as.factor(defaulter.df$EDUCATION)
defaulter.df$MARRIAGE<-as.factor(defaulter.df$MARRIAGE)
defaulter.df$PAY_Sep<-as.factor(defaulter.df$PAY_Sep)
defaulter.df$PAY_Aug<-as.factor(defaulter.df$PAY_Aug)
defaulter.df$PAY_Jul<-as.factor(defaulter.df$PAY_Jul)
defaulter.df$PAY_Jun<-as.factor(defaulter.df$PAY_Jun)
defaulter.df$PAY_May<-as.factor(defaulter.df$PAY_May)
defaulter.df$PAY_Apr<-as.factor(defaulter.df$PAY_Apr)
defaulter.df$default.payment.next.month<-as.factor(defaulter.df$default.payment.next.month)

summary(defaulter.df)

colnames(defaulter.df)
#Scale Numeric variables 
vars.to.scale <- c(1,5,12,13,14,15,16,17,18,19,20,21,22,23)
defaulter.df[,vars.to.scale] <-lapply(defaulter.df[,vars.to.scale], rescale)
summary(defaulter.df) 
# rescaling the Pay_# fields 
vars.pay.to.scale <- c(6:11)
defaulter.df[,vars.pay.to.scale] <- lapply(defaulter.df[,vars.pay.to.scale], as.numeric)    
defaulter.df[,vars.pay.to.scale] <- lapply(defaulter.df[,vars.pay.to.scale], rescale) 

# Create dummies for Categorical Variabes
default_dummy <- model.matrix(~ 0+SEX
                                 +MARRIAGE, data = defaulter.df)
default_dummy.df <- as.data.frame(default_dummy) # Converting to a df
head(default_dummy.df)
defaulter.df <- cbind(defaulter.df,default_dummy.df) # adding to the main df

dim(defaulter.df)
head(defaulter.df)

#partition Train 60% , Valid 40%
set.seed(1) 
train.rows <- sample(rownames(defaulter.df), dim(defaulter.df)[1]*0.6)
valid.rows <- setdiff(rownames(defaulter.df), train.rows)
train.data <- defaulter.df[train.rows, ]
valid.data <- defaulter.df[valid.rows, ]
dim(train.data)
dim(valid.data)



#Model 1: No Pay_#
train.data
n <- names(train.data[,-c(2,3,4,6:11,24)])
f <- as.formula(paste("default.payment.next.month ~", 
                      paste(n[!n %in% c("default.payment.next.month")], 
                            collapse = " + ")))
banknn <- neuralnet(f,data = train.data[1:1000,], linear.output = F, hidden = 3)

#options(scipen=999)
#prediction(banknn1)
plot(banknn, rep="best")

banknn.predict <- compute(banknn, valid.data[,-c(2,3,4,6:11,24)])

default.predict <- as.factor(ifelse(banknn.predict$net.result[,1]>0.5,1,0))
confusionMatrix(data = as.factor(default.predict),
                reference = as.factor(valid.data$default.payment.next.month),
                positive = '1', 
                dnn = c('Prediction','Actual'))

#Model 2: Apr

n <- names(train.data[,-c(2,3,4,6:10,12:16,18:22,24)])
f <- as.formula(paste("default.payment.next.month ~", 
                      paste(n[!n %in% c("default.payment.next.month")], 
                            collapse = " + ")))
banknn2 <- neuralnet(f,data = train.data[1:1000,], linear.output = F, hidden = 3)

#options(scipen=999)
#prediction(banknn1)
plot(banknn2, rep="best")

banknn2.predict <- compute(banknn2, valid.data[,-c(2,3,4,6:10,12:16,18:22,24)])

default2.predict <- as.factor(ifelse(banknn2.predict$net.result[,1]>0.5,1,0))
confusionMatrix(data = as.factor(default2.predict),
                reference = as.factor(valid.data$default.payment.next.month),
                positive = '1', 
                dnn = c('Prediction','Actual'))

#Model 3: Apr-May
n <- names(train.data[,-c(2,3,4,6:9,12:15,18:21,24)])
f <- as.formula(paste("default.payment.next.month ~", 
                      paste(n[!n %in% c("default.payment.next.month")], 
                            collapse = " + ")))
banknn3 <- neuralnet(f,data = train.data[1:1000,], linear.output = F, hidden = 3)

#options(scipen=999)
#prediction(banknn1)
plot(banknn3, rep="best")

banknn3.predict <- compute(banknn3, valid.data)

default3.predict <- as.factor(ifelse(banknn3.predict$net.result[,1]>0.5,1,0))
confusionMatrix(data = as.factor(default3.predict),
                reference = as.factor(valid.data$default.payment.next.month),
                positive = '1', 
                dnn = c('Prediction','Actual'))

#Model 4: Apr-June
n <- names(train.data[,-c(2,3,4,6:8,12:14,18:20,24)])
f <- as.formula(paste("default.payment.next.month ~", 
                      paste(n[!n %in% c("default.payment.next.month")], 
                            collapse = " + ")))
banknn4 <- neuralnet(f,data = train.data[1:1000,], linear.output = F, hidden = 3)

#options(scipen=999)
#prediction(banknn1)
plot(banknn4, rep="best")

banknn4.predict <- compute(banknn4, valid.data)

default4.predict <- as.factor(ifelse(banknn4.predict$net.result[,1]>0.5,1,0))
confusionMatrix(data = as.factor(default4.predict),
                reference = as.factor(valid.data$default.payment.next.month),
                positive = '1', 
                dnn = c('Prediction','Actual'))

#Model 5: Apr-Jul
n <- names(train.data[,-c(2,3,4,6:7,12:13,18:19,24)])
f <- as.formula(paste("default.payment.next.month ~", 
                      paste(n[!n %in% c("default.payment.next.month")], 
                            collapse = " + ")))
banknn5 <- neuralnet(f,data = train.data[1:1000,], linear.output = F, hidden = 3)

#options(scipen=999)
#prediction(banknn1)
plot(banknn5, rep="best")

banknn5.predict <- compute(banknn5, valid.data)

default5.predict <- as.factor(ifelse(banknn5.predict$net.result[,1]>0.5,1,0))
confusionMatrix(data = as.factor(default5.predict),
                reference = as.factor(valid.data$default.payment.next.month),
                positive = '1', 
                dnn = c('Prediction','Actual'))

#Model 6: Apr-Aug
n <- names(train.data[,-c(2,3,4,6,12,18,24)])
f <- as.formula(paste("default.payment.next.month ~", 
                      paste(n[!n %in% c("default.payment.next.month")], 
                            collapse = " + ")))
banknn6 <- neuralnet(f,data = train.data[1:1000,], linear.output = F, hidden = 3)

#options(scipen=999)
#prediction(banknn1)
plot(banknn6, rep="best")

banknn6.predict <- compute(banknn6, valid.data)

default6.predict <- as.factor(ifelse(banknn6.predict$net.result[,1]>0.5,1,0))
confusionMatrix(data = as.factor(default6.predict),
                reference = as.factor(valid.data$default.payment.next.month),
                positive = '1', 
                dnn = c('Prediction','Actual'))

#Model 7: Apr-Sep ALL
n <- names(train.data[,-c(2,3,4,24)])
f <- as.formula(paste("default.payment.next.month ~", 
                      paste(n[!n %in% c("default.payment.next.month")], 
                            collapse = " + ")))
banknn7 <- neuralnet(f,data = train.data[1:1000,], linear.output = F, hidden = 3)
head(train.data)
#options(scipen=999)
#prediction(banknn1)
plot(banknn7, rep="best")

banknn7.predict <- compute(banknn7, valid.data)

default7.predict <- as.factor(ifelse(banknn7.predict$net.result[,1]>0.5,1,0))
confusionMatrix(data = as.factor(default7.predict),
                reference = as.factor(valid.data$default.payment.next.month),
                positive = '1', 
                dnn = c('Prediction','Actual'))

#Model 8: Apr-Jul All rows [NOTE: This take a while to run]
n <- names(train.data[,-c(2,3,4,6:7,12:13,18:19,24)])
f <- as.formula(paste("default.payment.next.month ~", 
                      paste(n[!n %in% c("default.payment.next.month")], 
                            collapse = " + ")))
banknn8 <- neuralnet(f,data = train.data, linear.output = F, hidden = 3)

#options(scipen=999)
#prediction(banknn1)
plot(banknn8, rep="best")

banknn8.predict <- compute(banknn8, valid.data)

default8.predict <- as.factor(ifelse(banknn8.predict$net.result[,1]>0.5,1,0))
confusionMatrix(data = as.factor(default8.predict),
                reference = as.factor(valid.data$default.payment.next.month),
                positive = '1', 
                dnn = c('Prediction','Actual'))
# Output for banknn8
'
          Actual
Prediction    0    1
         0  801  909
         1 8572 1718
                                          
               Accuracy : 0.2099          
                 95% CI : (0.2027, 0.2173)
    No Information Rate : 0.7811          
    P-Value [Acc > NIR] : 1               
                                          
                  Kappa : -0.1271         
                                          
 Mcnemars Test P-Value : <2e-16          

Sensitivity : 0.65398         
Specificity : 0.08546 
'