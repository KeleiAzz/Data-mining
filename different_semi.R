require(caret)
library(DMwR)
library(e1071)
library(mda)
require(RWeka,quietly=T)
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
sample <- sample(nrow(dataset), 800)
labeled <- sample[1:300]
dataTest <- sample[601:800]
#trainIndex <- createDataPartition(dataset$y, p = .95, list = FALSE, times = 1)
fixed <- dataset[labeled,]
others <- dataset[-labeled,]


sup.res1 <- c()
semi.res1 <- c()
unsup.res1 <- c()
ada.res <- c()
adac.res <- c()
bag.res <- c()
for (i in seq(100,900,100)){
  sup.tmp1 <- c()
  semi.tmp1 <- c()
  #unsup.tmp1 <- c()
  ada.tmp <- c()
  adac.tmp <- c()
  bag.tmp1 <- c()
  for(j in seq(1,10)){
    
    nas <- sample(nrow(others), i)
    data.selfTr <- others[nas,]
    #data.selfTr <- rbind(data.selfTr,dataTest)
    data.selfTr[,'y'] <- NA
    trSelfT <- rbind(fixed, data.selfTr)
    
#     baseTree <- rpartXse(y~.,fixed[,],se=0.2)
#     sup1 <- confusion(predict(baseTree,dataset[dataTest,1:57],type='class'),
#                       dataset[dataTest,58])
#     sup.tmp1 <- c(sup.tmp1, (sup1[2]+sup1[3])/(sum(sup1)))
#     
    #     #adaboost
#     ada <- AdaBoostM1(y ~ .,fixed[,],
#                       control=Weka_control(I=100))
#     ada.preds <- predict(ada,dataset[dataTest,1:57],type='class')
#     adac <- confusion(ada.preds,
#                       dataset[dataTest,58])
#     adac.tmp <- c(adac.tmp, (adac[2]+adac[3])/sum(adac))
#     

    #self train bayes
    func <- function(m,d) {
      p <- predict(m,d,type='raw')
      data.frame(cl=colnames(p)[apply(p,1,which.max)],p=apply(p,1,max))
    }
    nbST <- SelfTrain(y ~ .,trSelfT[,],learner('naiveBayes',list()),'func')
    sup1 <- confusion(predict(nbST,dataset[dataTest,1:57],type='class'),
                                         dataset[dataTest,58])
    sup.tmp1 <- c(sup.tmp1, (sup1[2]+sup1[3])/(sum(sup1)))
    
    #self train decision tree
    f <- function(m,d) {
      l <- predict(m,d,type='class')
      c <- apply(predict(m,d),1,max)
      data.frame(cl=l,p=c)
    }
    treeSelfT <- SelfTrain(y~ .,trSelfT[,],learner('rpartXse',list(se=0.2)),'f')
    semi1 <- confusion(predict(treeSelfT,dataset[dataTest,1:57],type='class'),dataset[dataTest,58])
    semi.tmp1 <- c(semi.tmp1, (semi1[2]+semi1[3])/(sum(semi1)))
    
    #bagging
#     C <- matrix(nrow=nrow(dataset[dataTest,1:57]),ncol=15)
#     n <- nrow(trSelfT)
#     for (i in seq(1,13,3)) {
#       tt <- fixed[sample(nrow(fixed),n,replace=T),]
#       t <- trSelfT[sample(n, n, replace=T), ]
#       #nas <- sample(100,50)
#       #t[nas,5] <- NA
#       f <- function(m,d) {
#         l <- predict(m,d,type='class')
#         c <- apply(predict(m,d),1,max)
#         data.frame(cl=l,p=c)
#       }
#       baseTree <- rpartXse(y~.,tt,se=0.2)
#       C[,i] <- as.character(predict(baseTree,dataset[dataTest,1:57],type='class'))
#       
#       treeSelfT <- SelfTrain(y~ .,t,learner('rpartXse',list(se=0.5)),'f')
#       C[,i+1] <- as.character(predict(treeSelfT,dataset[dataTest,1:57],type='class'))
#       
#       pred.ada <- function(m,d) {
#         p <- predict(m,d,type='probability')
#         data.frame(cl=colnames(p)[apply(p,1,which.max)],
#                    p=apply(p,1,max)
#         )
#       }
#       adaSelfT <- SelfTrain(y ~ .,t,learner('AdaBoostM1',list(control=Weka_control(I=100))),'pred.ada')
#       C[,i+2] <- as.character(predict(adaSelfT,dataset[dataTest,1:57],type='class'))
#     }
#     res <- apply(C,1,bagPrediction)
#     bag <- confusion(res,dataset[dataTest,58])
#     bag.tmp1 <- c(bag.tmp1,(bag[2]+bag[3])/(sum(bag)))
#     
    
    
    # self train adaboost
    pred.ada <- function(m,d) {
      p <- predict(m,d,type='probability')
      data.frame(cl=colnames(p)[apply(p,1,which.max)],
                 p=apply(p,1,max)
      )
    }
    
    adaSelfT <- SelfTrain(y ~ .,trSelfT[,],learner('AdaBoostM1',
                                                   list(control=Weka_control(I=100))),
                          'pred.ada')
    preds <- predict(adaSelfT,dataset[dataTest,1:57],type='class')
    ada <- confusion(preds, dataset[dataTest,58])
    ada.tmp <- c(ada.tmp, (ada[2]+ada[3])/(sum(ada)))
    
  }
  sup.res1 <- c(sup.res1, mean(sup.tmp1))
  semi.res1 <- c(semi.res1, mean(semi.tmp1))
  ada.res <- c(ada.res,mean(ada.tmp))
  adac.res <- c(adac.res,mean(adac.tmp))
  bag.res <- c(bag.res,mean(bag.tmp1))
}

yrange <- range(c(0.1,0.15))
plot(seq(100,900,100),sup.res1,ylim=yrange,col='red',type='o',pch=2)
par(new=TRUE)
plot(seq(100,900,100),semi.res1,ylim=yrange,col='blue',type='o',pch=3)
par(new=TRUE)
plot(seq(100,900,100),bag.res,ylim=yrange,col='green',type='o',pch=4)
par(new=TRUE)
plot(seq(100,900,100),ada.res,ylim=yrange,col='gray',type='o',pch=6)
par(new=TRUE)
plot(seq(100,900,100),adac.res,ylim=yrange,col='black',type='o',pch=5)

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


# 0.119 0.127 0.121 0.127 0.129 0.138 0.127 0.136 0.139

# 0.151 0.143 0.143 0.143 0.143 0.151 0.143 0.147 0.151


#200  100-900
#sup [1] 0.167 0.165 0.162 0.157 0.185 0.166 0.160 0.166 0.165
#semi [1] 0.160 0.155 0.143 0.161 0.146 0.149 0.160 0.151 0.161
#ada [1] 0.110 0.113 0.106 0.110 0.100 0.114 0.106 0.106 0.109