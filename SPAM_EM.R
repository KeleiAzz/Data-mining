require(caret)
require(kernlab)
require(doMC)
library(bgmm)
library(mda)
library(EMCluster)

dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)
sample <- dataset[sample(nrow(dataset), 1000),]
trainIndex <- createDataPartition(sample$y, p = .8, list = FALSE, times = 1)#
dataTrain <- sample[ 1:600, 1:55]
dataTest  <- sample[801:1000,1:55]
#registerDoMC(cores=5)
lab <- as.integer(sample[1:600,]$y)
lab[401:600] <- 0

ret.em <- init.EM(dataTrain, nclass = 2, lab = lab, method = "Rnd.EM")

test <- as.integer(sample[401:600,]$y)
confusion(ret.em$class[401:600], test)
