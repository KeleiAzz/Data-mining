#outcome.label <- outcomeLabel(formula)
#mtry <- ifelse(is.null(mtry),floor(sqrt(ncol(data))),mtry)
data(iris)
iris[,5] <- as.factor(iris[,5])
tr <- sample(150,100)

test <- iris[-tr,]
data <- iris[tr,]

C <- matrix(nrow=nrow(test),ncol=5)
n <- nrow(data)
for (i in 1:5) {
  t <- data[sample(n, n, replace=T), ]
  nas <- sample(100,50)
  t[nas,5] <- NA
  f <- function(m,d) {
    l <- predict(m,d,type='class')
    c <- apply(predict(m,d),1,max)
    data.frame(cl=l,p=c)
  }
  treeSelfT <- SelfTrain(Species~ .,t,learner('rpartXse',list(se=0.5)),'f')
  C[,i] <- as.character(predict(treeSelfT,test,type='class'))
}
res <- apply(C,1,bagPrediction)

confusion(res,test[,5])
