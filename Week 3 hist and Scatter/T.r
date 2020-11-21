head(iris)

x_range=seq(-0.1, 2.6, by=0.1)

setosa_hist <- hist(subset(iris, Species == "setosa")$Petal.Width, 
                    breaks = x_range, plot = FALSE)

versicolor_hist <- hist(subset(iris, Species == "versicolor")$Petal.Width, 
                        breaks = x_range, plot = FALSE)

virginica_hist <- hist(subset(iris, Species == "virginica")$Petal.Width, 
                       breaks = x_range, plot = FALSE)

y_max=max(max(setosa_hist$counts),
          max(versicolor_hist$counts),
          max(virginica_hist$counts))

plot(setosa_hist, col=adjustcolor("green",alpha=0.3),
     ann=FALSE,axes=FALSE,ylim=c(0,y_max))
plot(versicolor_hist,col=adjustcolor("red",alpha=0.3), add = TRUE)
plot(virginica_hist,col=adjustcolor("blue",alpha=0.3), add = TRUE)

title(main="Iris",
      xlab="PetalWidth(cm)",
      ylab="Frequncy",cex.main=1.6)

x_axis_tick=x_range
axis(side=1,at=x_axis_tick)
y_axis_tick=seq(0,y_max,by=0.5)
axis(side=2,at=y_axis_tick)

legend("topright",c("Iris-setosa","Iris-versicolor", "Iris-virginica"),
       fill=c("green","red","blue"))

box("figure", col="gray")

pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width,
      data = iris,
      main = "Iris Data Matrix",
      pch = 21, bg = c("green", "red", "blue")[iris$Species], oma=c(4,4,6,12))
par(xpd=TRUE)
legend(0.9, 0.65, as.vector(unique(iris$Species)),  
       fill=c("green", "red", "blue"))

 plot (formula = Sepal.Width~Sepal.Length,
      data = iris,
      ylab = "Petal 너비",
      xlab = "Petal 길이",
      main = "IRIS 산점도",
      pch = 16,
      col = c("green", "red", "blue")[iris$Species])

 