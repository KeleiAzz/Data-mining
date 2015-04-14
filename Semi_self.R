dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)
sample <- sample(nrow(dataset), 600)
labeled <- sample[1:400]
dataTest <- sample[401:600]
#trainIndex <- createDataPartition(dataset$y, p = .95, list = FALSE, times = 1)
fixed <- dataset[labeled,]
others <- dataset[-labeled,]


semi.res1 <- c()
for (i in seq(100,900,100)){
  sup.tmp1 <- c()
  semi.tmp1 <- c()
  for(j in seq(1,5)){
    nas <- sample(nrow(others), i)
    data.selfTr <- others[nas,]
    data.selfTr[,'y'] <- NA
    trSelfT <- rbind(fixed, data.selfTr)
    f <- function(m,d) {
      l <- predict(m,d,type='class')
      c <- apply(predict(m,d),1,max)
      data.frame(cl=l,p=c)
    }
    
    treeSelfT <- SelfTrain(y~ .,trSelfT[,-(31:57)],learner('rpartXse',list(se=0.2)),'f')
    semi1 <- confusion(predict(treeSelfT,dataset[dataTest,-(31:57)],type='class'),dataset[dataTest,58])
    semi.tmp1 <- c(semi.tmp1, (semi1[2]+semi1[3])/sum(semi1))
  }
  semi.res1 <- c(semi.res1, mean(semi.tmp1))
}

yrange <- range(c(0.05,0.17))
#plot(seq(1000,4000,100),sup.res1,ylim=yrange,col='red',type='o',pch=2)
#par(new=TRUE)
plot(seq(100,900,100),semi.res1,ylim=yrange,col='blue',type='o',pch=4)
#par(new=TRUE)