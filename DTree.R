dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)
sample <- sample(nrow(dataset), 300)
labeled <- sample[1:100]
dataTest <- sample[101:300]
#trainIndex <- createDataPartition(dataset$y, p = .95, list = FALSE, times = 1)
fixed <- dataset[labeled,]
others <- dataset[-sample,]


sup.res1 <- c()
for (i in seq(100,4000,100)){
  sup.tmp1 <- c()
  for(j in seq(1,10)){
    
    nas <- sample(nrow(others), i)
    data.selfTr <- others[nas,]
    data.selfTr[,'y'] <- NA
    trSelfT <- rbind(fixed, data.selfTr)
    baseTree <- rpartXse(y~ .,fixed,se=0.5)
    sup1 <- confusion(predict(baseTree,dataset[dataTest,-58],type='class'),
                      dataset[dataTest,58])
    sup.tmp1 <- c(sup.tmp1, (sup1[2]+sup1[3])/sum(sup1))
  }
  sup.res1 <- c(sup.res1, mean(sup.tmp1))
}

yrange <- range(c(min(sup.res1),max(sup.res1)))
plot(seq(100,4000,100),sup.res1,ylim=yrange,col='red',type='o',pch=2)
#par(new=TRUE)
#plot(seq(100,4000,100),semi.res1,ylim=yrange,col='blue',type='o',pch=4)
#par(new=TRUE)