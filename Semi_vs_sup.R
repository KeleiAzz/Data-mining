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
for (i in seq(0.05, 0.95, 0.1)){
  sup.tmp1 <- c()
  sup.tmp2 <- c()
  semi.tmp1 <- c()
  semi.tmp2 <- c()
  for(j in seq(1,1)){
    trSelfT <- dataTrain
    nas <- createDataPartition(dataTrain$y, p = i, list = FALSE, times = 1)
    trSelfT[nas,'y'] <- NA
    ## Learn a tree using only the labelled cases and test it
    baseTree <- rpartXse(y~ .,trSelfT[-nas,],se=0.5)
    sup1 <- confusion(predict(baseTree,dataTest,type='class'),dataTest$y)
    sup.tmp1 <- c(sup.tmp1, (sup1[2]+sup1[3])/sum(sup1))
    sup2 <- confusion(predict(baseTree,trSelfT[nas,-58],type='class'),dataTrain[nas,]$y)
    sup.tmp2 <- c(sup.tmp2, (sup2[2]+sup2[3])/sum(sup2))
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
    semi.tmp1 <- c(semi.tmp1, (semi1[2]+semi1[3])/sum(semi1))
    semi2 <- confusion(predict(treeSelfT,trSelfT[nas,-58],type='class'),dataTrain[nas,]$y)
    semi.tmp2 <- c(semi.tmp2, (semi2[2]+semi2[3])/sum(semi2))
  }
  sup.res1 <- c(sup.res1, mean(sup.tmp1))
  sup.res2 <- c(sup.res2, mean(sup.tmp2))
  semi.res1 <- c(semi.res1, mean(semi.tmp1))
  semi.res2 <- c(semi.res2, mean(semi.tmp2))
  
}

yrange <- range(c(min(semi.res1,semi.res2,sup.res1,sup.res2),max(semi.res1,semi.res2,sup.res1,sup.res2)))
plot(seq(0.05,0.95,0.1),sup.res2,ylim=yrange,col='red',type='o',pch=2)
par(new=TRUE)
plot(seq(0.05,0.95,0.1),semi.res2,ylim=yrange,col='blue',type='o',pch=4)
par(new=TRUE)



plot(seq(0.05,0.95,0.1),sup.res1,ylim=yrange,col='yellow',type='o',pch=16)
par(new=TRUE)
plot(seq(0.05,0.95,0.1),semi.res1,ylim=yrange,col='green',type='o',pch=3)
#par(new=TRUE)

#sup.res1
#[1] 0.08406114 0.08930131 0.09104803 0.09454148 0.10218341 0.09410480 0.10174672 0.10327511 0.10807860 0.10917031
#[11] 0.11266376 0.12052402 0.12096070 0.12227074 0.12685590 0.13580786 0.14388646 0.15371179 0.18209607

#sup.res2
#[1] 0.07795455 0.07808219 0.07907154 0.07942857 0.08665448 0.08193598 0.08616188 0.08608571 0.08684611 0.08479652
#[11] 0.09076923 0.09329268 0.09445813 0.09895493 0.10138720 0.10411546 0.11619586 0.12582571 0.14977130

#semi.res1
#[1] 0.09126638 0.09104803 0.09344978 0.09563319 0.10021834 0.10545852 0.10109170 0.10676856 0.10960699 0.10851528
#[11] 0.11572052 0.11855895 0.12576419 0.12532751 0.13056769 0.13886463 0.14432314 0.16244541 0.18515284

#semi.res2
#[1] 0.07204545 0.07340183 0.07831050 0.07697143 0.08254113 0.08250762 0.08368146 0.08291429 0.08715084 0.08431642
#[11] 0.09051975 0.09035823 0.09498593 0.10109406 0.10515244 0.10573021 0.12140167 0.13307927 0.16101348