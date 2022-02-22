defaulter.df <- read.csv(file="Default.csv",header=TRUE)
summary(defaulter.df)
defaulter.df$SEX<-as.factor(defaulter.df$SEX)
defaulter.df$EDUCATION<-as.factor(defaulter.df$EDUCATION)
defaulter.df$MARRIAGE<-as.factor(defaulter.df$MARRIAGE)
defaulter.df$PAY_Sep<-as.factor(defaulter.df$PAY_Sep)
defaulter.df$PAY_Aug<-as.factor(defaulter.df$PAY_Aug)
defaulter.df$PAY_Jul<-as.factor(defaulter.df$PAY_Jul)
defaulter.df$PAY_Jun<-as.factor(defaulter.df$PAY_Jun)
defaulter.df$PAY_May<-as.factor(defaulter.df$PAY_May)
defaulter.df$PAY_Apr<-as.factor(defaulter.df$PAY_Apr)

class(defaulter.df$BILL_AMT_Sep)
class(defaulter.df$BILL_AMT_Aug)
class(defaulter.df$LIMIT_BAL)
class(defaulter.df$PAY_Apr)
defaulter.df$LIMIT_BAL<-as.numeric(defaulter.df$LIMIT_BAL)
defaulter.df$AGE<-as.numeric(defaulter.df$AGE)
defaulter.df$BILL_AMT_Sep<-as.numeric(defaulter.df$BILL_AMT_Sep)
defaulter.df$BILL_AMT_Aug<-as.numeric(defaulter.df$BILL_AMT_Aug)
defaulter.df$BILL_AMT_Jul<-as.numeric(defaulter.df$BILL_AMT_Jul)
defaulter.df$BILL_AMT_Jun<-as.numeric(defaulter.df$BILL_AMT_Jun)
defaulter.df$BILL_AMT_May<-as.numeric(defaulter.df$BILL_AMT_May)
defaulter.df$BILL_AMT_Apr<-as.numeric(defaulter.df$BILL_AMT_Apr)
defaulter.df$PAY_AMT_Sep<-as.numeric(defaulter.df$PAY_AMT_Sep)
defaulter.df$PAY_AMT_Aug<-as.numeric(defaulter.df$PAY_AMT_Aug)
defaulter.df$PAY_AMT_Jul<-as.numeric(defaulter.df$PAY_AMT_Jul)
defaulter.df$BILL_AMT_Jun<-as.numeric(defaulter.df$BILL_AMT_Jun)
defaulter.df$PAY_May<-as.numeric(defaulter.df$PAY_May)
defaulter.df$PAY_Apr<-as.numeric(defaulter.df$PAY_Apr)
map.df<-defaulter.df[,c(6,13,14,15,16,17,18,19,20,21,22,23,24)]
summary(map.df)
heatmap(map.df)

defaulter.df<-defaulter.df[,-1]
summary(defaulter.df)
#visualization
par(mfcol = c(1,1))
plot(defaulter.df$PAY_Sep,xlab = "repayment status", ylab = "Frequency", main="Status in september 2005")
plot(defaulter.df$PAY_Aug,xlab = "repayment status", ylab = "Frequency",main="Status in August 2005")
plot(defaulter.df$PAY_Jul,xlab = "repayment status", ylab = "Frequency", main="Status in July 2005")
plot(defaulter.df$PAY_Jun,xlab = "repayment status", ylab = "Frequency", main="Status in June 2005")
plot(defaulter.df$PAY_May,xlab = "repayment status", ylab = "Frequency",main="Status in May 2005")
plot(defaulter.df$PAY_Apr,xlab = "repayment status", ylab = "Frequency",main="Status in April 2005")

# -2:(decreased) category: No of INACTIVE cards have reduced over 6 months period
# -1: (increased) More customers are paying full amount over 6 months period
# 0: (decreased) Less number customers do partial payment(revolving credit)
#1:(increased): one month payment delay has increased
#2:(increase-dec-increase-decr)-fluctuating
class(defaulter.df$EDUCATION)
summary(defaulter.df$EDUCATION)
missing_edu <- defaulter.df$EDUCATION == 0
defaulter.df[missing_edu,]$EDUCATION<-1
summary(defaulter.df$EDUCATION)
summary(defaulter.df$PAY_Sep)
#partition
set.seed(1) 

defaulter1.df <- defaulter.df
summary(defaulter1.df)
train.rows <- sample(rownames(defaulter1.df), dim(defaulter1.df)[1]*0.6)
# collect all the columns with training row ID into training set:
train.data <- defaulter1.df[train.rows, ]
# assign row IDs that are not already in the training set, into validation 
valid.rows <- setdiff(rownames(defaulter1.df), train.rows) 
valid.data <- defaulter1.df[valid.rows, ]
dim(train.data)
dim(valid.data)
#valid.data<-valid.data[-c(2691,4098),]
#level1.data<-defaulter.df[c(2691,4098),]
#train.data<-rbind(train.data,level1.data)
#model1
summary(train.data$PAY_Jun)
summary(valid.data$PAY_Jun)
summary(train.data$PAY_May)
summary(valid.data$PAY_May)

Jun_level1_row<-valid.data[valid.data$PAY_Jun==1,]
Jun_level1_row
valid.data<-valid.data[-c(6783,11498),]
train.data<-rbind(train.data,Jun_level1_row)
dim(valid.data)
dim(train.data)

May_level8_row<-valid.data[valid.data$PAY_May==8,]
May_level8_row
valid.data<-valid.data[-c(8655),]
train.data<-rbind(train.data,May_level8_row)
dim(valid.data)
dim(train.data)


banklogit.reg <- glm(default.payment.next.month ~ ., data = train.data, family = "binomial") 

#model2 (without pay_0 to 6)
banklogit.reg <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE+ 
                       BILL_AMT_Sep + BILL_AMT_Aug + BILL_AMT_Jul + BILL_AMT_Jun + BILL_AMT_May + BILL_AMT_Apr 
                     + PAY_AMT_Sep + PAY_AMT_Aug + PAY_AMT_Jul + PAY_AMT_Jun + PAY_AMT_May + PAY_AMT_Apr, 
                     data = train.data, family = "binomial") 


#model3 (upto August)
summary(train.data$PAY_Jul)

banklogit.reg <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE+
                     +PAY_Aug+  PAY_Jul+PAY_Jun+PAY_May+PAY_Apr
                    +BILL_AMT_Aug +BILL_AMT_Jul+ BILL_AMT_Jun + BILL_AMT_May+BILL_AMT_Apr
                    +PAY_AMT_Aug +PAY_AMT_Jul+PAY_AMT_Jun + PAY_AMT_May + PAY_AMT_Apr, 
                     data = train.data, family = "binomial") 
#try
banklogit.reg <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE+
                       +PAY_Aug+  PAY_Jul+PAY_Jun+PAY_May+PAY_Apr, 
                     data = train.data, family = "binomial") 
banklogit.reg <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE+
                       +  PAY_Jul+PAY_Jun+PAY_May+PAY_Apr, 
                     data = train.data, family = "binomial") 
banklogit.reg <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE+
                       +PAY_Jun+PAY_May+PAY_Apr, 
                     data = train.data, family = "binomial") 
banklogit.reg <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE+
                      +PAY_May+PAY_Apr, 
                     data = train.data, family = "binomial") 
banklogit.reg <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE+
                       +PAY_Apr, 
                     data = train.data, family = "binomial")
#model4 (upto July)
summary(train.data$PAY_Jul)
banklogit.reg <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE+
                       PAY_Jul+PAY_Jun+PAY_May+PAY_Apr
                     +BILL_AMT_Jul+ BILL_AMT_Jun + BILL_AMT_May+BILL_AMT_Apr
                     +PAY_AMT_Jul+PAY_AMT_Jun + PAY_AMT_May + PAY_AMT_Apr, 
                     data = train.data, family = "binomial") 


#model5 (Upto June)
banklogit.reg <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE+
                       +PAY_Jun+PAY_May+PAY_Apr
                      + BILL_AMT_Jun + BILL_AMT_May+BILL_AMT_Apr
                       +PAY_AMT_Jun + PAY_AMT_May + PAY_AMT_Apr, 
                     data = train.data, family = "binomial") 
#model6 (Upto May)
banklogit.reg <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE+
                       +PAY_May+PAY_Apr
                      + BILL_AMT_May+BILL_AMT_Apr
                      + PAY_AMT_May + PAY_AMT_Apr, 
                     data = train.data, family = "binomial") 
#model7 (Just Apr)
banklogit.reg <- glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE+
                       +PAY_Apr
                     +BILL_AMT_Apr
                      + PAY_AMT_Apr, 
                     data = train.data, family = "binomial")

options(scipen=999)
summary(banklogit.reg)
hist(banklogit.reg$residuals)
# sex2(beta=-0.138) female(***)---> Odds of belonging to class=1(defaulter) will decrease
# limit_bal(***), education4(*),5(***)---> Odds of belonging to class=1(defaulter) will decrease
#
banklogit.reg.pred <- predict(banklogit.reg, valid.data[, -24], type = "response") 
df<-data.frame(actual.defaulter = valid.data$default.payment.next.month, predicted.defaulter = banklogit.reg.pred)
View(df)
library(gains)
bankgain <- gains(valid.data$default.payment.next.month, banklogit.reg.pred, groups=10)
bankgain


#Lift Index is Mean Resp / Cum MEan Resp for the last row*100

# plot lift chart
plot(c(0,bankgain$cume.pct.of.total*sum(valid.data$default.payment.next.month))~c(0,bankgain$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(valid.data$default.payment.next.month))~c(0, dim(valid.data)[1]), lty=2)

summary(as.factor(valid.data$default.payment.next.month))

# compute deciles and plot decile-wise chart
heights <- bankgain$mean.resp/mean(valid.data$default.payment.next.month)
heights
decileplot <- barplot(heights, names.arg = bankgain$depth, ylim = c(0,9), 
                      xlab = "Percentile", ylab = "Mean Response/Overall Mean", main = "Decile-wise lift chart")

# add labels to columns
text(decileplot, heights+0.5, labels=round(heights, 1), cex = 0.8)

library(forecast)
library(caret)
#generate confusion matrix
banklogit.reg.pred
confusionMatrix(as.factor(ifelse(banklogit.reg.pred>0.5,1,0)), as.factor(valid.data[, 24]),positive = '1' )





























