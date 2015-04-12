require(caret)
library(DMwR)
library(e1071)
library(mda)
dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)

#trainIndex <- createDataPartition(sample$y, p = .8, list = FALSE, times = 1)
#dataTrain <- sample[ trainIndex,]
#dataTest  <- sample[-trainIndex,]

dataTotal <- dataset[sample(nrow(dataset), 1500),]

fix <- dataTotal[1351:1500,]
dataTrain <- dataTotal[1:1350,]
result <- list()
lab <- as.integer(dataTotal[,58])

# iterate for Mixmod
for (i in seq(150,1350,150)){
  temp <- list()
  for (j in seq(1,10)){
    lab <- as.integer(dataTotal[,58])
    labeled <- sample(1350, i)
    lab[-labeled] <- 0
    learn <- mixmodCluster(data=dataTotal[,1:30], dataType = "quantitative",model =mixmodGaussianModel(), 
                            nbCluster = 2, knownLabels = lab)
    prediction <- mixmodPredict(fix[,1:30],learn["bestResult"])
    temp[[j]] <- confusion(prediction["partition"], as.integer(fix[,58]))
  }
  result[[length(result)+1]] <- temp
}
