setwd("C:/Users/oshos/Desktop/3-2/Data Science/2weeks/second")
df1 <- read.csv("test_data.csv",header = T)

myapply <- function(Obj, Margin, Func, ...){
  result <- vector()
  if(Margin == 1){
    for(i in 1:nrow(Obj))
      result <- c(result, Func(Obj[I, ]),...)
    return(result)
  }
  else if(Margin == 2){
    for(i in 1:ncol(Obj))
      result <- c(result, Func(Obj[ ,i]),...)
    return(result)
  }
}
myapply(m1, 1, sum)
apply(m1, 1, sum)

mylapply <- function(Obj, Func, ...){
  result <- list()
  for(i in Obj)
    result <- c(result,Func(i))
  if(is.data.frame(Obj)){
    for(i in 1:ncol(Obj))
      names(result)[i] <- names(Obj)[i]
  }
  return(result)
}
mylapply(df1, sum)
lapply(df1, sum)

m1 <- matrix(1:24, nrow = 3, ncol = 8)
v1 <- c(1:12)
v2 <- c("a","b","c","d","e","f","g","h","i","j","k","l")
l1 <- list(1,2,3,4,5,6,7,8,9,10,11,12)
l3 <- list(3,4,5,6,7)
l2 <- list("a","b","c","d","e","f","g","h","i","j","k","l")
setwd("C:/Users/oshos/Desktop/3-2/Data Science/2weeks/second")
df1 <- read.csv("test_data2.csv",header = T)
mymapply <- function(Func, Obj, Obj2){
  result <- vector()
  max_size <- max(length(Obj), length(Obj2))
  if(is.list(Obj)){
    for(i in 1:max_size){
      i1 <- i%%length(Obj)
      i2 <- i%%length(Obj2)
      if(i1 == 0){i1 <- length(Obj)}
      if(i2 == 0){i2 <- length(Obj2)}
      result <- c(result, Func(Obj[[i1]],Obj2[[i2]]))
    }
  }
  else{
    for(i in 1:max_size){
      i1 <- i%%length(Obj)
      i2 <- i%%length(Obj2)
      if(i1 == 0){i1 <- length(Obj)}
      if(i2 == 0){i2 <- length(Obj2)}
      result <- c(result, Func(Obj[i1],Obj2[i2]))
    }
  }
  
  if(is.data.frame(Obj)){
    for(i in 1:ncol(Obj))
      names(result)[i] <- names(Obj)[i]
  }
  return(result)
}
mymapply(sum, df1, df1)
mapply(sum, df1, df1)
mymapply(sum, l1, l3)
mapply(sum, l1, l3)
mymapply(sum, v1, v1)
mapply(sum, v1, v1)
mymapply(sum, m1, m1)
mapply(sum, m1, m1)

m1 <- matrix(1:24, nrow = 3, ncol = 8)
v1 <- c(1:12)
v2 <- c("a","b","c","d","e","f","g","h","i","j","k","l")
l1 <- list(1,2,3,4,5,6,7,8,9,10,11,12)
l3 <- list(3,4,5,6,7)
l2 <- list("a","b","c","d","e","f","g","h","i","j","k","l")
setwd("C:/Users/oshos/Desktop/3-2/Data Science/2weeks/second")
df1 <- read.csv("test_data2.csv",header = T)

mymapply <- function(Func, Obj, Obj2){
  result <- vector()
  max_size <- max(length(Obj), length(Obj2))
  
  if(is.list(Obj)){
    for(i in 1:max_size){
      i1 <- i%%length(Obj)
      i2 <- i%%length(Obj2)
      if(i1 == 0){i1 <- length(Obj)}
      if(i2 == 0){i2 <- length(Obj2)}
      result <- c(result, Func(Obj[[i1]],Obj2[[i2]]))
    }
  }
  else{
    for(i in 1:max_size){
      i1 <- i%%length(Obj)
      i2 <- i%%length(Obj2)
      if(i1 == 0){i1 <- length(Obj)}
      if(i2 == 0){i2 <- length(Obj2)}
      result <- c(result, Func(Obj[i1],Obj2[i2]))
    }
  }
  if(is.data.frame(Obj)){
    for(i in 1:ncol(Obj))
      names(result)[i] <- names(Obj)[i]
  }
  return(result)
}

mymapply(sum, df1, df1)
mapply(sum, df1, df1)
mymapply(sum, l1, l3)
mapply(sum, l1, l3)
mymapply(sum, v1, v1)
mapply(sum, v1, v1)
mymapply(sum, m1, m1)
mapply(sum, m1, m1)