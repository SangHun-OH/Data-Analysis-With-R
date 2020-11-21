install.packages("readxl")
library(readxl)

setwd("c:/Users/oshos/Desktop/3-2/Data Science/First P")

df_sample <- read.csv("traning_dataset.csv", header = T)

df_sample

#select.column <- c("기분", "주가", "온도")

fit <- lm(결과~., data = df_sample)
summary(fit)

df_test <- read.csv("test_dataset.csv", header = T)
pre_success <- predict(fit, newdata = df_test)

cbind(df_test, pre_success)
