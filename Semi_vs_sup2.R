require(caret)
library(DMwR)
library(e1071)
library(mda)
#set.seed(1234) 
dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)
# sample <- sample(nrow(dataset), 300)
# labeled <- sample[1:100]
# dataTest <- sample[101:300]
# #trainIndex <- createDataPartition(dataset$y, p = .95, list = FALSE, times = 1)
# fixed <- dataset[labeled,]
# others <- dataset[-labeled,]
#data.unlabeled[,'y'] <- NA
#dataTrain <- dataset[ trainIndex,]
#dataTest  <- dataset[-trainIndex,]
sample <- sample(nrow(dataset), 600)
labeled <- sample[1:400]
dataTest <- sample[401:600]
#trainIndex <- createDataPartition(dataset$y, p = .95, list = FALSE, times = 1)
fixed <- dataset[labeled,]
others <- dataset[-labeled,]


sup.res1 <- c()
semi.res1 <- c()
for (i in seq(1000,4000,100)){
  sup.tmp1 <- c()
  semi.tmp1 <- c()
  for(j in seq(1,5)){
    
    nas <- sample(nrow(others), i)
    data.selfTr <- others[nas,]
    data.selfTr[,'y'] <- NA
    trSelfT <- rbind(fixed, data.selfTr)
    baseTree <- rpartXse(y~.,fixed[,1:58],se=0.2)
    sup1 <- confusion(predict(baseTree,dataset[dataTest,1:57],type='class'),
                      dataset[dataTest,58])
    sup.tmp1 <- c(sup.tmp1, (sup1[2]+sup1[3])/sum(sup1))
    f <- function(m,d) {
      l <- predict(m,d,type='class')
      c <- apply(predict(m,d),1,max)
      data.frame(cl=l,p=c)
    }

    treeSelfT <- SelfTrain(y~ .,trSelfT[,1:58],learner('rpartXse',list(se=0.2)),'f')
    semi1 <- confusion(predict(treeSelfT,dataset[dataTest,1:57],type='class'),dataset[dataTest,58])
    semi.tmp1 <- c(semi.tmp1, (semi1[2]+semi1[3])/sum(semi1))
  }
  sup.res1 <- c(sup.res1, mean(sup.tmp1))
  semi.res1 <- c(semi.res1, mean(semi.tmp1))
}

yrange <- range(c(0.1,0.2))
plot(seq(1000,4000,100),sup.res1,ylim=yrange,col='red',type='o',pch=2)
par(new=TRUE)
plot(seq(1000,4000,100),semi.res1,ylim=yrange,col='blue',type='o',pch=4)
#par(new=TRUE)




# nas <- sample(nrow(others), 4000)
# data.selfTr <- others[nas,]
# data.selfTr[,'y'] <- NA
# trSelfT <- rbind(fixed, data.selfTr)
# baseTree <- rpartXse(y~ .,fixed,se=0.5)
# confusion(predict(baseTree,others[nas, -58],type='class'),others[nas, 58])
# f <- function(m,d) {
#   l <- predict(m,d,type='class')
#   c <- apply(predict(m,d),1,max)
#   data.frame(cl=l,p=c)
# }
# 
# treeSelfT <- SelfTrain(y~ .,trSelfT,learner('rpartXse',list(se=0.5)),'f')
# confusion(predict(treeSelfT,others[nas, -58],type='class'),others[nas,]$y)
#semi.tmp1 <- c(semi.tmp1, (semi1[2]+semi1[3])/sum(semi1))

