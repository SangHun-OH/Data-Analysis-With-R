library(readxl)
library(rpart)
library(rpart.plot)
library(caret)
library(rattle)
library(RColorBrewer)
library(doBy)
library(sampling)
library(e1071)

setwd("c:/Users/oshos/Desktop/3-2/Data Science/4weeks")

df <- read_excel("GermanCredit-data.xls")
df <- df[,-1]


origin_index <- strata(data = df, stratanames = "RESPONSE", size = c(700, 300), method = "srswor")
df <- getdata(df, origin_index)

train_index <- strata(data = df, stratanames = "RESPONSE", size = c(700*0.7, 300*0.7), method = "srswor")
train <- getdata(df, train_index)

train_raw <- as.vector(train[, 32])

test <- df[!(df$ID_unit %in% train_raw),]

train <- train[, -c(32, 33, 34)]
test <- test[, -c(32, 33, 34)]


df <- read_excel("GermanCredit-data.xls")
df <- df[,-1]


tree <- rpart(RESPONSE ~., data = train, method = "class", parms = list(split = 'gini'))
tree1 <- rpart(RESPONSE ~., data = train, method = "class", parms = list(split = 'information'))
tree2 <- rpart(RESPONSE ~ CHK_ACCT + HISTORY + DURATION + AMOUNT + SAV_ACCT, data = train, method = "class", parms = list(split = 'gini'))

fancyRpartPlot(tree)
fancyRpartPlot(tree1)
fancyRpartPlot(tree2)


printcp(tree)
plotcp(tree)
plotcp(tree1)
plotcp(tree2)

ptree<-prune(tree, cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"])
ptree1<-prune(tree1, cp= tree1$cptable[which.min(tree1$cptable[,"xerror"]),"CP"])
ptree2<-prune(tree2, cp= tree2$cptable[which.min(tree2$cptable[,"xerror"]),"CP"])

fancyRpartPlot(tree)
fancyRpartPlot(ptree)

fancyRpartPlot(tree1)
fancyRpartPlot(ptree1)

fancyRpartPlot(tree2)
fancyRpartPlot(ptree2)

rpartpred<-predict(tree, test, type='class')
confusionMatrix(table(rpartpred, test$RESPONSE))

rpartpred<-predict(tree1, test, type='class')
confusionMatrix(table(rpartpred, test$RESPONSE))

rpartpred<-predict(tree2, test, type='class')
confusionMatrix(table(rpartpred, test$RESPONSE))

rpartpred<-predict(ptree, test, type='class')
confusionMatrix(table(rpartpred, test$RESPONSE))

rpartpred<-predict(ptree1, test, type='class')
confusionMatrix(table(rpartpred, test$RESPONSE))

rpartpred<-predict(ptree2, test, type='class')
confusionMatrix(table(rpartpred, test$RESPONSE))

rf_data <- learning_curve_dat(dat = df, outcome = "RESPONSE", test_prop = 3/10)


ggplot(rf_data, aes(x = Training_Size, y = RMSE, color = Data)) + geom_smooth(method = loess, span = .8) + theme_bw()