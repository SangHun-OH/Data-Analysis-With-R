library(readxl)
library(dplyr)
library(caret)
library(nnet)
library(neuralnet)
library(devtools)
library(reshape)
library(rpart)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
#install.packages("NeuralNetTools")
library(NeuralNetTools)
library(ggplot2)
library(rattle)


setwd("c:/Users/oshos/Desktop/3-2/Data Science/8weeks/")
titanic <- read_excel(path="./titanic3.xls", sheet="titanic3")
glimpse(titanic)
titanic2 = data.frame(survived = titanic$survived, sex = titanic$sex,
                      pclass = factor(titanic$pclass), 
                      age = titanic$age,
                      fare = titanic$fare)

#명목형 변환 by One-hot encoding
encode <- dummyVars(~., data = titanic2)
titanic_df <- data.frame(predict(encode, newdata = titanic2))
titanic_df$survived <- factor(titanic_df$survived)

glimpse(titanic_df)
summary(titanic_df)
head(titanic_df)

#결측치 제거
row_na = apply(titanic_df,1,anyNA)
titanic_df<-titanic_df[!row_na,]
summary(titanic_df)


#정규화 (기존에 명목형인 변수들이 존재하므로 0,1 값 유지를 위해 정규화 사용)
n_titanic = preProcess(titanic_df, method = "range")
n_titanic_df = predict(n_titanic, titanic_df)
summary(n_titanic_df)
head(n_titanic_df)

# 데이터셋 분리
set.seed(5)
indexes = createDataPartition(n_titanic_df$survived, p = .6, list = F)
train_titanic = n_titanic_df[indexes, ]
test_titanic = n_titanic_df[-indexes, ]

#############################################################################

# 결정나무모델 

DT_model = rpart(survived~., 
                 data = train_titanic, method = "class",
                 control = rpart.control(cp=0)
)
DT_model
# 사후가지치기로 정확도 향상
printcp(DT_model)
plotcp(DT_model)
#printcp와 plotcp 상 최적의 cp값 확인 (난 0.0019 나옴)
DT_model<-prune(DT_model,cp=0.01)

fancyRpartPlot(DT_model)

#나무 모델로 측정
pred = predict(DT_model, test_titanic, type = "class" )

confusionMatrix(pred, test_titanic$survived)

#############################################################################


# 인공신경망 작성
# repeatedcv 방식 사용하여 최적 파라미터 탐색
control <-trainControl(method = "repeatedcv",
                       number=10, repeats=5)
model <- train(survived~., data=train_titanic,
               method="nnet", trControl=control,
               tuneLength=5, metric = 'Accuracy')



model
model$finalModel

# check the convergence of the final model (1 if maxit reached, 0 otherwise)
model$finalModel$convergence 

#size =1 에서 최적의 모델을 찾아야 하므로 모델 결과값에서 size 1와 decay 범위를 참조하여 모델 다시 설정
#size 1 -> decay  0, 1e-04, 1e-03, 1e-02, 1e-01 
#tuneGrid = expand.grid(size = 1, decay = c(0,10**-4,10**-3,10**-2,10**-1,2**-1,1))
tuneGrid = expand.grid(size = 1, decay = 10**(-1:-4))

tuneGrid
trControl = trainControl(method = 'repeatedcv', 
                         number = 10, 
                         repeats = 5, 
                         returnResamp = 'final')
model <- train(survived~., data=train_titanic, method="nnet", 
               trControl=control, metric = 'Accuracy',
               tuneGrid=tuneGrid)
model
model$finalModel

ggplot(model) 

plot.nnet(model)
garson(model) 


#test 데이터로 성능평가한거
pred <- predict(model$finalModel, newdata = test_titanic, type = "class") 
pred <- factor(pred, levels = c("0", "1" ))
pred
confusionMatrix(pred, test_titanic$survived)

# 학습곡선 
point_num =length(seq(50,nrow(train_titanic),50))
learn_curve <- data.frame(train_num=double(point_num),
                          train_err = double(point_num),
                          test_err = double(point_num))
limit=seq(50,nrow(train_titanic),50)

for(i in limit){
  index = (i-50)/50+1
  learn_curve$train_num[index]=i
  tuneGrid = expand.grid(size = 1, decay = 0.01)
  fit<- train(survived~., data=train_titanic[1:i,], method="nnet", 
              metric = 'Accuracy',
              tuneGrid=tuneGrid)
  learn_curve$train_err[index]<- 1-fit$results$Accuracy
  
  pred <- predict(fit$finalModel,test_titanic, type = "class") 
  pred <- factor(pred, levels = c("0", "1" ))
  cm<-confusionMatrix(pred, test_titanic$survived,positive ="1")
  learn_curve$test_err[index] <- 1-cm[["overall"]][["Accuracy"]]
}
g<-ggplot(data=learn_curve)
g<-g+geom_line(mapping=aes(x=train_num,y=test_err,color="train"))
g<-g+geom_line(mapping=aes(x=train_num,y=train_err,color="test"))
g[["labels"]] = list(x='Number of Training DataSet',y='Error')
g<-g+stat_smooth(data=learn_curve,mapping=aes(x=train_num,y=test_err,color="train"))
g<-g+stat_smooth(data=learn_curve,mapping=aes(x=train_num,y=train_err,color="test"))
g
#D.   인공 신경망 epoch 수 vs 성능의 학습곡선
max=seq(50,1000,50)

learn_curve2 <- data.frame(maxit=double(length(max)),
                           err = double(length(max)))

for(i in max){
  index = (i-50)/50+1 
  learn_curve2$maxit[index]=i
  fit<-train(survived~., data=train_titanic, method="nnet", 
             metric = 'Accuracy',maxit=i)
  learn_curve2$err[index]<- 1-max(fit$results$Accuracy)
}
g<-ggplot(data=learn_curve2)
g<-g+geom_line(mapping=aes(x=max,y=err))
g[["labels"]] = list(x='Maxit',y='Error')
g<-g+stat_smooth(data=learn_curve2,mapping=aes(x=max,y=err))
g
max(fit$results$Accuracy)


## SVM CODE
# optimization
trControl <- trainControl(method='repeatedcv', number = 10, repeats = 2)
model1 <- train(survived ~.,
                data = train,
                method = 'svmLinear2',
                metric = 'Accuracy',
                tuneLength = 10,
                trControl = trControl
)
# show model 
model1
plot(model1)
# show final model
model1$finalModel
# check the convergence of the final model 
model1$finalModel$convergence 


pred <- predict(model1, test) 
pred <- factor(pred, levels = c(0,1))
print(data.frame(test$survived, pred))

trControl <- trainControl(method='repeatedcv', number = 10, repeats = 2)
model3 <- train(survived ~.,
                data = train,
                method = 'svmRadial',
                metric = 'Accuracy',
                trControl = trControl,
                tuneLength = 5
)
model3
plot(model3)

model3$finalModel

pred <- predict(model3, newdata = test) 
pred <- factor(pred, levels = c(0,1))
print(data.frame(test$survived, pred))

