library(upclass)
data(iris)

X<- as.matrix(iris[,-5])
cl<-as.matrix(iris[,5])
indtrain <- sort(sample(1:150, 30))
Xtrain <- X[indtrain,]
cltrain <- cl[indtrain]

indtest <- setdiff(1:150, indtrain)
Xtest <- X[indtest,]
cltest <- cl[indtest]
fitupmodels <- upclassify(Xtrain, cltrain,Xtest) #testing every model.
plot(fitupmodels)
