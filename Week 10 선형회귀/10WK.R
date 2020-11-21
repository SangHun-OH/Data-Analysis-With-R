library(readxl)
library(dplyr)
library(caret)
library(rpart)
library(nnet)
library(neuralnet)
library(devtools)
library(reshape)
library(NeuralNetTools)
library(e1071)
library(pROC)
library(randomForest)
source_url('https://gist.githubusercontent.com/Peque/41a9e20d6687f2f3108d/raw/85e14f3a292e126f1454864427e3a189c2fe33f3/nnet_plot_update.r')
#install.packages("MASS")
#install.packages("psych")
library(MASS)
library(psych)


setwd("c:/Users/oshos/Desktop/3-2/Data Science/10weeks/")
Boston_df <- read.csv("HousingData.csv", stringsAsFactors = TRUE)
Boston_df
library(dplyr)
library(caret)

#데이터 준비
data <- read.csv("HousingData.csv")
names(data)[1] <-c("CRIM")
data <-data[,-c(15)]
str(data)


data <- data[complete.cases(data), ]
dim(data)

#상관관계 분석
getcor<-cor(data)
dim(getcor)

for(i in 1:14){
  for(j in 1:14){
    if(abs(getcor[i,j])>0.8 && i<j){
      print(paste0("행: ",i,"열: ",j))
    }
  }
}

print(paste0(names(data)[9]," and ", names(data)[10]))
ggplot(data,aes(RAD,TAX))+geom_jitter()+geom_smooth(method ='lm')
with(data,cor(RAD,TAX,method="kendall"))
#상관관계가 서로 높은 변수가 없으므로 그대로 회귀분석 실행

# Data Partition
set.seed(1901)
indexes = createDataPartition(data$MEDV, p = 0.6, list = F)
train = data[indexes, ]
test = data[-indexes, ]


# 설명모델
control_lm <-trainControl(method = "repeatedcv",
                          number=10, repeats=5)
model_explain <- train(MEDV~., data=data,
                       method="lm", trControl=control_lm,
                       tuneLength=5, metric = 'RMSE')

fit<- model_explain$finalModel
summary(fit)
#INDUS 와 AGE의 P-val 이 0.05를 넘김 변수로 부적합!
#BUT 전체 모형의 R^2 값과 p-val을 유의함

###D. 변수 선택 Stepwise-selection을 실행 

# stepwise 방식으로 변수 선택
fit.con <- lm(MEDV~1,data = data)
fit.both <- step(fit.con, scope = list(lower = fit.con, upper = fit), direction = "both")
summary(fit.both)

#lm(formula = MEDV ~ LSTAT + RM + PTRATIO + DIS + NOX + CHAS + B + ZN + CRIM + RAD + TAX, data = data)
#다중선형회귀 분석임을 고려한 adjusted R^2 값이 0.7378로 높으며(error 비중이 낮으며 설명력이 높음)
#p-value < 유의수준(0.05)이므로 
#예측 변수가 유의미 하다고 판단할 수 있다
#Adjusted R-squared값이 증가, p-val 감소 -> 더 나은 모형임

relweights <- function(fit,...){
  R <- cor(fit$model)
  nvar <- ncol(R)
  rxx <- R[2:nvar, 2:nvar]
  rxy <- R[2:nvar, 1]
  svd <- eigen(rxx)
  evec <- svd$vectors
  ev <- svd$values
  delta <- diag(sqrt(ev))
  lambda <- evec %*% delta %*% t(evec)
  lambdasq <- lambda ^ 2
  beta <- solve(lambda) %*% rxy
  rsquare <- colSums(beta ^ 2)
  rawwgt <- lambdasq %*% beta ^ 2
  import <- (rawwgt / rsquare) * 100
  import <- as.data.frame(import)
  row.names(import) <- names(fit$model[2:nvar])
  names(import) <- "Weights"
  import <- import[order(import),1, drop=FALSE]
  dotchart(import$Weights, labels=row.names(import),
           xlab="% of R-Square", pch=19,
           main="Relative Importance of Predictor Variables",
           sub=paste("Total R-Square=", round(rsquare, digits=3)),
           ...)
  return(import)
}
result = relweights(fit.both, col="blue")
result
#변수 중요도 (그냥 추가해봄)


#유효성 검사
opar <-par(mfrow=c(2,2),oma =c(0,0,1.1,0))
plot(fit.both,las=1)
par(opar)
#잔차가 어느정도 정규성과 등분산성을 갖는것을 알 수 있다
#즉, 이 회귀식은유효하다고 볼 수 있음
vif(fit.both)
#10을 넘는 값이 없으므로 다중공선성문제도 없음을 확인


########################################################################################

# 예측모델
model_explain <- train(MEDV~., data=train,
                       method="lm", trControl=control_lm,
                       tuneLength=5, metric = 'RMSE')

str(model_lm)
fit <- model_explain$finalModel

#install.packages("forecast")
library(forecast)
accuracy(fit)

#예측모델 정확도
pre <- predict(fit, newdata = test, interval = "predict")
pre <- as.data.frame(pre)
pre <- cbind(pre, test$MEDV)
tf <- NA
pre <- cbind(pre, tf)
pre$tf[pre$`test$MEDV`>= pre$lwr & pre$`test$MEDV` <= pre$upr] <- T
pre$tf[is.na(pre$tf)] <- F
head(pre)

sum(pre$tf=="TRUE")/dim(pre)[1] #94.5 % 정확도

# k- Fold Cross validation
library(DAAG)
windows()
cvResults <- suppressWarnings(
  CVlm(data = test, 
       form.lm=MEDV ~ LSTAT + RM + PTRATIO + DIS + NOX + CHAS + B + ZN + CRIM + RAD + TAX, 
       m=5, 
       dots=FALSE, 
       seed=29, 
       legend.pos="topleft",  
       printit=FALSE, 
       main="Small symbols are predicted values while bigger ones are actuals."));  

attr(cvResults, 'ms')  # => 251.2783 mean squared error
head(cvResults)

rmse <- function(yi, yhat_i){ #rsme 측정
  sqrt(mean((yi - yhat_i)^2))
} 
rmse(test$MEDV,cvResults[,15])

