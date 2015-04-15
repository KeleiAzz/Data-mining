install.packages("caret")
install.packages("doMC")
install.packages("kernlab")
require(caret)
require(kernlab)
require(doMC)
library(bgmm)
library(mda)

dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)
sample <- dataset[sample(nrow(dataset), 1000),]
trainIndex <- createDataPartition(sample$y, p = .8, list = FALSE, times = 1)#
dataTrain <- sample[ 1:700,]
dataTest  <- sample[801:1000,]
registerDoMC(cores=5)

X_s = as.matrix(dataTrain[,53:56])  #Change here, then error occur???!!
knowns_s = as.matrix(dataTest[,53:56])
class_s = dataTest$y
rownames(X_s) <- NULL
rownames(knowns_s) <- NULL
modelSemiSupervised = semisupervised(X = X_s, knowns = knowns_s,   class = class_s)
preds = predict(modelSemiSupervised, X = as.matrix(sample[701:800,53:56]))

confusion(modelSemiSupervised$P[,2], class_s)
modelSupervised = supervised( knowns = knowns_s,   class = class_s)
plot(modelSupervised)