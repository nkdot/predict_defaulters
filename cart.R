defaulter.df <- read.csv(file="Default.csv",header=TRUE)
summary(defaulter.df)
defaulter.df$default.payment.next.month<-as.factor(defaulter.df$default.payment.next.month)
defaulter.df$SEX<-as.factor(defaulter.df$SEX)
defaulter.df<-defaulter.df[,-1]
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


library(rpart)
library(rpart.plot)
#model1
defaultcv.ct <- rpart(default.payment.next.month ~ ., data = train.data, method = "class",cp = 0.001, minsplit = 2,xval=5)
#model2(till august)
defaultcv.ct <- rpart(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION +
                        MARRIAGE + AGE+
                      +PAY_Aug+  PAY_Jul+PAY_Jun+PAY_May+PAY_Apr
                      +BILL_AMT_Aug +BILL_AMT_Jul+ BILL_AMT_Jun +BILL_AMT_May+BILL_AMT_Apr
                      +PAY_AMT_Aug +PAY_AMT_Jul+PAY_AMT_Jun + PAY_AMT_May + PAY_AMT_Apr, data = train.data, method = "class",cp = 0.001, minsplit = 2,xval=5)
#model3(till July)
defaultcv.ct <- rpart(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION +
                        MARRIAGE + AGE+
                        +  PAY_Jul+PAY_Jun+PAY_May+PAY_Apr
                      +BILL_AMT_Jul+ BILL_AMT_Jun +BILL_AMT_May+BILL_AMT_Apr
                      +PAY_AMT_Jul+PAY_AMT_Jun + PAY_AMT_May + PAY_AMT_Apr, data = train.data, method = "class",cp = 0.001, minsplit = 2,xval=5)
#model4(till June)
defaultcv.ct <- rpart(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION +
                        MARRIAGE + AGE+
                       +PAY_Jun+PAY_May+PAY_Apr
                      + BILL_AMT_Jun +BILL_AMT_May+BILL_AMT_Apr
                      +PAY_AMT_Jun + PAY_AMT_May + PAY_AMT_Apr, data = train.data, method = "class",cp = 0.001, minsplit = 2,xval=5)
#model5(till May)
defaultcv.ct <- rpart(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION +
                        MARRIAGE + AGE+
                       +PAY_May+PAY_Apr
                       +BILL_AMT_May+BILL_AMT_Apr
                       + PAY_AMT_May + PAY_AMT_Apr, data = train.data, method = "class",cp = 0.001, minsplit = 2,xval=5)
#model6(till Apr)
defaultcv.ct <- rpart(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION +
                        MARRIAGE + AGE+
                      +PAY_Apr
                      +BILL_AMT_Apr
                       + PAY_AMT_Apr, data = train.data, method = "class",cp = 0.001, minsplit = 2,xval=5)




# count number of leaves
length(defaultcv.ct$frame$var[defaultcv.ct$frame$var == "<leaf>"])
# plot tree
prp(defaultcv.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, cex=0.5,
    box.col=ifelse(defaultcv.ct$frame$var == "<leaf>", 'gray', 'white'))  
printcp(defaultcv.ct)

#model1
defaultpruned.ct <- prune(defaultcv.ct, 
                       cp = 0.002)
#model2
defaultpruned.ct <- prune(defaultcv.ct, 
                          cp = 0.002)
#model3
defaultpruned.ct <- prune(defaultcv.ct, 
                          cp = 0.003)
#model4
defaultpruned.ct <- prune(defaultcv.ct, 
                          cp = 0.03)
#model5
defaultpruned.ct <- prune(defaultcv.ct, 
                          cp = 0.002)
#model6
defaultpruned.ct <- prune(defaultcv.ct, 
                          cp = 0.006)
prp(defaultpruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(defaultpruned.ct$frame$var == "<leaf>", 'gray', 'white'))  

defaultprune.pred.valid <- predict(defaultpruned.ct,valid.data,type = "class")
confusionMatrix(defaultprune.pred.valid, as.factor(valid.data$default.payment.next.month), positive ='1' )

#boosted forest

library(adabag)
library(rpart) 
library(caret)

train.data$default.payment.next.month <- as.factor(train.data$default.payment.next.month )

##boosting tree
set.seed(1)
#model1
defaultboost <- boosting(default.payment.next.month ~ ., data = train.data)
#model2
defaultboost <- boosting(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION +
                           MARRIAGE + AGE+
                           +PAY_Aug+  PAY_Jul+PAY_Jun+PAY_May+PAY_Apr
                         +BILL_AMT_Aug +BILL_AMT_Jul+ BILL_AMT_Jun +BILL_AMT_May+BILL_AMT_Apr
                         +PAY_AMT_Aug +PAY_AMT_Jul+PAY_AMT_Jun + PAY_AMT_May + PAY_AMT_Apr, data = train.data)
#model 3

defaultboost <- boosting(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION +
                           MARRIAGE + AGE+
                           +  PAY_Jul+PAY_Jun+PAY_May+PAY_Apr
                         +BILL_AMT_Jul+ BILL_AMT_Jun +BILL_AMT_May+BILL_AMT_Apr
                         +PAY_AMT_Jul+PAY_AMT_Jun + PAY_AMT_May + PAY_AMT_Apr, data = train.data)

#model 4
defaultboost <- boosting(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION +
                           MARRIAGE + AGE+
                           +PAY_Jun+PAY_May+PAY_Apr
                         + BILL_AMT_Jun +BILL_AMT_May+BILL_AMT_Apr
                         +PAY_AMT_Jun + PAY_AMT_May + PAY_AMT_Apr, data = train.data)

#model 5
defaultboost <- boosting(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION +
                           MARRIAGE + AGE+
                           +PAY_May+PAY_Apr
                         +BILL_AMT_May+BILL_AMT_Apr
                         + PAY_AMT_May + PAY_AMT_Apr, data = train.data)

#model 6
defaultboost <- boosting(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION +
                           MARRIAGE + AGE+
                           +PAY_Apr
                         +BILL_AMT_Apr
                         + PAY_AMT_Apr, data = train.data)

#Predict using Valid data

defaultBoost.pred.valid <- predict(defaultboost,valid.data,type = "class")
# generate confusion matrix for validation data
confusionMatrix(as.factor(defaultBoost.pred.valid$class), as.factor(valid.data$default.payment.next.month ), positive ='1')

library(randomForest)
## random forest
#Model 1
defaultRF <- randomForest(as.factor(default.payment.next.month) ~ ., data = train.data, ntree = 500, 
                       mtry = 4, nodesize = 5, importance = TRUE)  

#Model 2
defaultRF <- randomForest(as.factor(default.payment.next.month) ~ LIMIT_BAL + SEX + EDUCATION +
                            MARRIAGE + AGE+
                            +PAY_Aug+  PAY_Jul+PAY_Jun+PAY_May+PAY_Apr
                          +BILL_AMT_Aug +BILL_AMT_Jul+ BILL_AMT_Jun +BILL_AMT_May+BILL_AMT_Apr
                          +PAY_AMT_Aug +PAY_AMT_Jul+PAY_AMT_Jun + PAY_AMT_May + PAY_AMT_Apr, data = train.data, ntree = 500, 
                          mtry = 4, nodesize = 5, importance = TRUE) 
#Model 3
defaultRF <- randomForest(as.factor(default.payment.next.month) ~ LIMIT_BAL + SEX + EDUCATION +
                            MARRIAGE + AGE+
                            +  PAY_Jul+PAY_Jun+PAY_May+PAY_Apr
                          +BILL_AMT_Jul+ BILL_AMT_Jun +BILL_AMT_May+BILL_AMT_Apr
                          +PAY_AMT_Jul+PAY_AMT_Jun + PAY_AMT_May + PAY_AMT_Apr, data = train.data, ntree = 500, 
                          mtry = 4, nodesize = 5, importance = TRUE)
#Model 4

defaultRF <- randomForest(as.factor(default.payment.next.month) ~ LIMIT_BAL + SEX + EDUCATION +
                            MARRIAGE + AGE+
                            +PAY_Jun+PAY_May+PAY_Apr
                          + BILL_AMT_Jun +BILL_AMT_May+BILL_AMT_Apr
                          +PAY_AMT_Jun + PAY_AMT_May + PAY_AMT_Apr, data = train.data, ntree = 500, 
                          mtry = 4, nodesize = 5, importance = TRUE) 
#Model 5
defaultRF <- randomForest(as.factor(default.payment.next.month) ~ LIMIT_BAL + SEX + EDUCATION +
                            MARRIAGE + AGE+
                            +PAY_May+PAY_Apr
                          +BILL_AMT_May+BILL_AMT_Apr
                          + PAY_AMT_May + PAY_AMT_Apr, data = train.data, ntree = 500, 
                          mtry = 4, nodesize = 5, importance = TRUE) 
#Model 6
defaultRF <- randomForest(as.factor(default.payment.next.month) ~ LIMIT_BAL + SEX + EDUCATION +
                            MARRIAGE + AGE+
                            +PAY_Apr
                          +BILL_AMT_Apr
                          + PAY_AMT_Apr, data = train.data, ntree = 500, 
                          mtry = 4, nodesize = 5, importance = TRUE) 

defaultRF.pred.valid <- predict(defaultRF,valid.data,type = "class")
confusionMatrix(defaultRF.pred.valid, as.factor(valid.data$default.payment.next.month), positive ='1')

## variable importance plot
varImpPlot(defaultRF, type = 1)
