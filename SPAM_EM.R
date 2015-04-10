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

#trainIndex <- createDataPartition(sample$y, p = .8, list = FALSE, times = 1)#
#unlabel <- sample(800)
#dataTrain <- sample[ 1:800, 1:54]
#dataTest  <- sample[801:1000,1:54]
#registerDoMC(cores=5)
lab <- as.integer(sample$y)
lab[1:500] <- 0

ret.Rnd <- init.EM(sample, nclass = 2, lab = lab, method = "Rnd.EM", EMC = .EMC.Rnd)
ret.em <- init.EM(sample, nclass = 2, lab = lab, method = "em.EM")

test <- as.integer(sample[1:500,]$y)

confusion(ret.Rnd$class[1:500], test)
confusion(ret.em$class[1:500], test)
