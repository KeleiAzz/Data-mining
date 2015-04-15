###################################################
### Data set
###################################################
dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)

samples <- sample(4521,1100)
train <- dataset[samples[1:40],]
unlabled <- dataset[sample[201:900],]
unlabled$y <- NA
test <- dataset[samples[901:1100],]

semiTrain <- rbind(train,unlabled)

###################################
#### Decistion tree
###################################
dt <- function(train,test){
  data <- train
  model <- rpartXse(y~., data,se=0.2)
  preds <- predict(model,test,type='prob')
  return(preds)
}
dt.res <- dt(train,test)
dt.pred <- prediction( dt.res[,2], test[,'y'] )
dt.perf <- performance(dt.pred,'tpr','fpr')
#plot(dt.perf)

###################################
#### Navie Bayes 
##################################
nb <- function(train,test) {
  require(e1071,quietly=T)
  #sup <- train
  data <- train
  #data$y <- factor(data$y,levels=c('ok','fraud'))
  model <- naiveBayes(y ~ .,data)
  preds <- predict(model,test,type='raw')
  return(preds)
}
nb.res <- nb(train,test)
nb.pred <- prediction( nb.res[,2], test[,'y'] )
nb.perf <- performance(nb.pred,'tpr','fpr')
#plot(nb.perf)

###################################
#### Navie Bayes with SMOTE
##################################
nb.s <- function(train,test) {
  require(e1071,quietly=T)
  data <- train
  newData <- SMOTE(y ~ .,data,perc.over=700)
  model <- naiveBayes(y ~ .,newData)
  preds <- predict(model,test,type='raw')
  return(preds)
}
nb.s.res <- nb.s(train,test)
nb.s.pred <- prediction( nb.s.res[,2], test[,'y'] )
nb.s.perf <- performance(nb.s.pred,'tpr','fpr')
#plot(nb.s.perf)
##################################
#### Ada Boost
##################################
ab <- function(train,test) {
  require(RWeka,quietly=T)
  data <- train
  model <- AdaBoostM1(y ~ .,data,
                      control=Weka_control(I=100))
  preds <- predict(model,test,
                   type='probability')
  return(preds)
}
ab.res <- ab(train,test)
ab.pred <- prediction( ab.res[,2], test[,'y'] )
ab.perf <- performance(ab.pred,'tpr','fpr')
#plot(ab.perf)

###################################
#### Self Train with naive bayes
##################################
pred.nb <- function(m,d) {
  p <- predict(m,d,type='raw')
  data.frame(cl=colnames(p)[apply(p,1,which.max)],
             p=apply(p,1,max)
  )
}
nb.st <- function(train,test) {
  require(e1071,quietly=T)
  train <- train
  #train[which(train$Insp == 'unkn'),'Insp'] <- NA
  #train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
  model <- SelfTrain(y ~ .,train,
                     learner('naiveBayes',list()),'pred.nb')
  preds <- predict(model,test,type='raw')
  return(preds)
}
nb.st.res <- nb.st(semiTrain,test)
nb.st.pred <- prediction( nb.st.res[,2], test[,'y'] )
nb.st.perf <- performance(nb.st.pred,'tpr','fpr')
#plot(nb.st.perf)

###################################
#### Self Train with Decision tree
##################################
pred.dt <- function(m,d) {
  p <- predict(m,d,type='prob')
  data.frame(cl=colnames(p)[apply(p,1,which.max)],
             p=apply(p,1,max)
  )
}
dt.st <- function(train,test) {
  #require(e1071,quietly=T)
  train <- train
  model <- SelfTrain(y ~ .,train,
                     learner('rpartXse',list(se=0.2)),'pred.dt')
  preds <- predict(model,test,type='prob')
  return(preds)
}
dt.st.res <- dt.st(semiTrain,test)
dt.st.pred <- prediction( dt.st.res[,2], test[,'y'] )
dt.st.perf <- performance(dt.st.pred,'tpr','fpr')
#plot(dt.st.perf)
###################################
#### Self Train with AdaBoost
##################################
pred.ada <- function(m,d) {
  p <- predict(m,d,type='probability')
  data.frame(cl=colnames(p)[apply(p,1,which.max)],
             p=apply(p,1,max)
  )
}
ab.st <- function(train,test) {
  require(RWeka,quietly=T)
  train <- train
  #train[which(train$Insp == 'unkn'),'Insp'] <- NA
  #train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
  model <- SelfTrain(y ~ .,train,
                     learner('AdaBoostM1',
                             list(control=Weka_control(I=100))),
                     'pred.ada')
  preds <- predict(model,test,type='probability')
  return(preds)
}
ab.st.res <- ab.st(semiTrain,test)
ab.st.pred <- prediction( ab.st.res[,2], test[,'y'] )
ab.st.perf <- performance(ab.st.pred,'tpr','fpr')
#plot(ab.st.perf)

#################################
### ROC curve
#################################
plot(dt.perf,col='red')
par(new=TRUE)
plot(nb.perf,col='green')
par(new=TRUE)
# plot(nb.s.perf,col='gray')
# par(new=TRUE)
plot(ab.perf,col='blue')
par(new=TRUE)
plot(nb.st.perf,col='pink')
par(new=TRUE)
plot(dt.st.perf,col='yellow')
par(new=TRUE)
plot(ab.st.perf)

