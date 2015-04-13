require(caret)
library(DMwR)
library(e1071)
library(mda)
#set.seed(1234)
dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)
#sample <- dataset[sample(nrow(dataset), 4000),]
trainIndex <- createDataPartition(dataset$y, p = .95, list = FALSE, times = 1)
dataTrain <- dataset[ trainIndex,]
dataTest  <- dataset[-trainIndex,]


#stdTree <- rpartXse(y ~ .,dataTrain,se=0.5)
#confusion(predict(stdTree,dataTest,type='class'),dataTest$y)

# Self Train using Decision tree

sup.res1 <- c()
sup.res2 <- c()
semi.res1 <- c()
semi.res2 <- c()
for (i in seq(0.05, 0.95, 0.05)){
  for(j in seq(1,20)){
    
  }
  trSelfT <- dataTrain
  nas <- createDataPartition(dataTrain$y, p = i, list = FALSE, times = 1)
  trSelfT[nas,'y'] <- NA
  ## Learn a tree using only the labelled cases and test it
  baseTree <- rpartXse(y~ .,trSelfT[-nas,],se=0.5)
  sup1 <- confusion(predict(baseTree,dataTest,type='class'),dataTest$y)
  sup.res1 <- c(sup.res1, (sup1[2]+sup1[3])/sum(sup1))
  sup2 <- confusion(predict(baseTree,trSelfT[nas,-58],type='class'),dataTrain[nas,]$y)
  sup.res2 <- c(sup.res2, (sup2[2]+sup2[3])/sum(sup2))
  ## The user-defined function that will be used in the self-training process
  
  f <- function(m,d) {
    l <- predict(m,d,type='class')
    c <- apply(predict(m,d),1,max)
    data.frame(cl=l,p=c)
  }
  ## Self train the same model using the semi-superside data and test the
  ## resulting model
  treeSelfT <- SelfTrain(y~ .,trSelfT,learner('rpartXse',list(se=0.5)),'f')
  semi1 <- confusion(predict(treeSelfT,dataTest,type='class'),dataTest$y)
  semi.res1 <- c(semi.res1, (semi1[2]+semi1[3])/sum(semi1))
  semi2 <- confusion(predict(treeSelfT,trSelfT[nas,-58],type='class'),dataTrain[nas,]$y)
  semi.res2 <- c(semi.res2, (semi2[2]+semi2[3])/sum(semi2))
}

yrange <- range(c(min(semi.res1,semi.res2,sup.res1,sup.res2),max(semi.res1,semi.res2,sup.res1,sup.res2)))
plot(seq(0.05,0.95,0.05),sup.res2,ylim=yrange,col='red',type='o',pch=2)
par(new=TRUE)
plot(seq(0.05,0.95,0.05),semi.res2,ylim=yrange,col='blue',type='o',pch=4)




plot(seq(0.05,0.95,0.05),sup.res1,ylim=yrange,col='red',type='o',pch=16)
par(new=TRUE)
plot(seq(0.05,0.95,0.05),semi.res1,ylim=yrange,col='blue',type='o',pch=3)
par(new=TRUE)