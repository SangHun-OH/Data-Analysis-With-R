
library(readxl)
library(dplyr) 
library(haven)
library(reshape) 
library(ggplot2) 
library(arules) 
library(arulesViz)

# cutting tree
total<-read_excel(path = "./따르릉이용시간대비.xlsx")
출발지<-total$출발지
도착지<-total$도착지
이용시간<-as.double(total$이용시간)
이용거리<-as.double(total$이용거리)
tf <- data.frame(이용시간,이용거리) #  출퇴근시간 합한 데이터셋



set.seed(133)
f <-tf[sample(nrow(tf), 100),] # 100개 비복원 추출

f_scaled <- scale(f, center = FALSE, apply(f, MARGIN = 2, FUN = max)) # 정규화


hc <- hclust(dist(f_scaled)) #kmean 군집화 진행

plot(hc)

cutree(hc, k = 1:3)  # k = no of clusters
cutree(hc, h = 1.0)  # h = height
plot.new()
plot(hc)
rect.hclust(hc, k = 2, border = 3:5)
abline(h = 1.0, col="red")


# Compare the 1 and  2grouping:
g24 <- cutree(hc, k = c(1,2))
table(grp2 = g24[,"1"], grp4 = g24[,"2"])


#  K-means clustering
# Compute k-means with k = 2
set.seed(133)
km.res <- kmeans(f_scaled, 2, nstart = 25) # 군집 갯수 2개 시작 군집수 25개
km.res #출력
# Determine K
# NbClust
library(NbClust)
nc <- NbClust(f, min.nc=2, max.nc=10, method="kmeans") #min군집2개 max 10개 kmeans 사용
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")
#가장 높은 군집도를 보여줌 x축은 군집수 y축은 사용 방식중 높게 나온 방식 수




# WithinSS - within sum of squares
wssplot <- function(tf, nc=15, seed=133){
  wss <- (nrow(tf)-1)*sum(apply(tf,2,var)) # K = 1, Total SS 
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(tf, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}# x축 군집수 y축 변수 수



wssplot(tf)

