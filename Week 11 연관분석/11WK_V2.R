install.packages("rlang")
library(haven)
library(dplyr) 
library(reshape) 
library(ggplot2) 
library(arules) 
library(arulesViz)
library(readxl)

#경로바꾸셈
setwd("c:/Users/oshos/Desktop/3-2/Data Science/11weeks/")
BIKE <- read_excel(path = "./따르릉 퇴근.xlsx")
BIKE<-as.data.frame(BIKE)


# 결측치 제거
na.omit(BIKE)
View(BIKE)
#없음
BIKE$출발지<-as.vector(BIKE$출발지)
data.list <- split(BIKE$출발지, BIKE$도착지)
data.list

data.trans <- as(data.list, "transactions")
summary(data.trans)


inspect(data.trans[1:100])
itemFrequency(data.trans)
itemFrequencyPlot(data.trans, support=0.20)
itemFrequencyPlot(data.trans, topN=5, main = "상위 5")
data.rules <- apriori(data.trans)
summary(data.rules)

data.rules <- apriori(data.trans, parameter=list(support=0.005, confidence=0.5, minlen=1))
#support = 규칙의 최소 지지도 / confidence = 규칙의 최소 신뢰도 / minlen = 규칙에 포함되는 최소 물품수
summary(data.rules)
inspect(data.rules)

inspect(sort(data.rules, by="lift"))
#lift = 향상도

 
# data.eclatRules <- eclat(data.trans, parameter = list(support=0.115, confidence = 0.8, minlen=1))
# inspect(data.eclatRules)
# subsetRules <- which(rowSums(is.subset(data.rules, data.rules, proper = T)) > 1)
# length(subsetRules)
# data.rules <- data.rules[-subsetRules]
# summary(data.rules)


plot(head(sort(data.rules, by= "lift"),30),method="graph")
#원이 연관 관계를 나타내며, 원의 크기가 Support, 색상진하기가 Lift

plot(data.rules, method="graph", interactive=T)
#더또렷함

plot(data.rules, method="paracoord")
#색은 support를 나타내며, 위 그래프는 각 선이 꺾이는 곳을 기준으로 해석

plot(data.rules, method="grouped")
#원의 크기가 지지도, 색이 향상도
#install.packages("ggmap")
#devtools::install_github("dkahle/ggmap")

library(ggmap)
register_google(key="AIzaSyBNdwIjvXnKQF9fD0KIPLGg2Hw_8uOth8o")

rule.change = as.data.frame(inspect(data.rules))
colnames(rule.change)[2] = "Direction"
rule.change = rule.change %>% arrange(-support)




LOCATION = rule.change %>% 
  mutate(FROM = gsub("\\{","",lhs),TO = gsub("\\{","",rhs)) %>%
  mutate(FROM = gsub("\\}","",FROM), TO = gsub("\\}","",TO))

LOCATION$ID = 1:nrow(LOCATION)

summary(LOCATION)
head(LOCATION)
                              
Name = unique(c(LOCATION$FROM,LOCATION$TO))
geopoint_FROM = geocode(enc2utf8(as.character(Name)))       

LOCATION_DF = data.frame(
  ID = 0:(length(Name)-1),
  Name = Name,
  LONG = geopoint_FROM$lon,
  LATT = geopoint_FROM$lat )

LOCATION_TOTAL = merge(
  LOCATION[,c('FROM','TO','support','confidence')],
  LOCATION_DF[,c('Name','LONG','LATT')],
  by.x = "FROM",by.y = 'Name',all = TRUE)

LOCATION_TOTAL = reshape::rename(LOCATION_TOTAL,c("LONG" = "LONG_F", "LATT" = "LATT_F"))
LOCATION_TOTAL = merge(LOCATION_TOTAL, LOCATION_DF[,c('Name','LONG','LATT')] , by.x = "TO", by.y = 'Name', all = FALSE)
LOCATION_TOTAL = reshape::rename(LOCATION_TOTAL,c("LONG" = "LONG_T", "LATT" = "LATT_T"))

cen = apply(geopoint_FROM,2,mean)
map = get_googlemap( "seoul", maptype = "roadmap", zoom = 11)
ggmap(map) +
  geom_point(data = LOCATION_TOTAL, aes(x = LONG_F, y= LATT_F,size = support,col = support),alpha = 0.4) +
  geom_text(data = LOCATION_TOTAL %>% filter(nchar(FROM)< 6),
            aes(x =LONG_F, y= LATT_F + 0.015, label = FROM)) + 
  geom_segment(data = LOCATION_TOTAL, 
               aes(x = LONG_F,xend = LONG_T, y = LATT_F,yend = LATT_T, 
                   col = confidence), 
               arrow = arrow(length = unit(0.4,"cm")), color = "RED", size = 1.2 ) + 
  guides(size = FALSE,col = FALSE) +
  scale_size_continuous(range = c(2,10)) + 
  scale_color_gradientn(colours = c("#12B2FF","#2066CC"))


#library("igraph")

#
# inspect(sort(data.rules, by="lift")[1:10])
