WOW(AdaBoostM1)
dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)

samples <- sample(4601,1000)
train <- dataset[samples[1:800],]
test <- dataset[samples[801:1000],]

require(RWeka,quietly=T)
train <- train[,]
train$y <- as.factor(train$y)
train[sample(nrow(train),200),'y'] <- NA
pred.ada <- function(m,d) {
  p <- predict(m,d,type='probability')
  data.frame(cl=colnames(p)[apply(p,1,which.max)],
             p=apply(p,1,max)
  )
}

model <- SelfTrain(y ~ .,train,
                   learner('AdaBoostM1',
                           list(control=Weka_control(I=100))),
                   'pred.ada')
preds <- predict(model,test[,],
                 type='probability')
confusion(preds, test[,58])

pred <- prediction( preds[,2], test[,58] )
perf <- performance(pred,'tpr','fpr')
plot(perf)
