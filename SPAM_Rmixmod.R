# Accuray too low!

library(Rmixmod)
require(doMC)
registerDoMC(cores=5)
data(iris)
idt <- sample(150,100)
dataTrain <- iris[idt,]
dataTest <- iris[-idt,]
lab <- as.integer(dataTrain[,5])
lab[1:90] <- 0
#lab[lab %in% 3] <- 0
#lab[lab %in% 2] <- 0
learn <- mixmodCluster(data=dataTrain[,-5], nbCluster = 3, knownLabels = lab)


prediction <- mixmodPredict(dataTest[,-5],learn["bestResult"])
paste("accuracy= ",mean(as.integer(dataTest[,5]) == prediction["partition"])*100,"%",sep="")

dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)
sample <- dataset[sample(nrow(dataset), 1000),]
dataTrain <- sample[1:800,]
dataTest <- sample[801:1000,]

result <- c()
temp <- c()
for (i in seq(100,900,100))
{
  
  dataset$y <- as.factor(dataset$y)
  sample <- dataset[sample(nrow(dataset), 400+i),]
  dataTrain <- sample[1:(200+i),]
  dataTest <- sample[(200+i+1):(400+i),]
  
  lab <- as.integer(dataTrain[,58])
  lab[200:(200+i)] <- 0
  
  learn <- mixmodCluster(data=dataTrain[,1:30], dataType = "quantitative",model =mixmodGaussianModel(), criterion= c("BIC","ICL","NEC"), nbCluster = 2, knownLabels = lab)
  prediction <- mixmodPredict(dataTest[,1:30],learn["bestResult"])
  paste("accuracy= ",mean(as.integer(dataTest[,58]) == prediction["partition"])*100,"%",sep="")
  print(mean(as.integer(dataTest[,58]) == prediction["partition"]))
  temp <- c(temp,mean(as.integer(dataTest[,58]) == prediction["partition"]))
    
  
}
# num = 50    100   150   200   250   300   350   400   450   500   550   600   650   700   750 
# res = 0.870 0.875 0.875 0.885 0.885 0.875 0.890 0.875 0.875 0.755 0.760 0.790 0.860 0.840 0.875
# 0.865 0.720 0.810 0.860 0.685 0.825 0.725 0.815 0.790

#[1] 0.680 0.730 0.775 0.870 0.850 0.745 0.675 0.650 0.635
#0.825
sample <- sample(nrow(dataset), 400)
labeled <- sample[1:200]
dataTest <- sample[201:400]
#trainIndex <- createDataPartition(dataset$y, p = .95, list = FALSE, times = 1)
fixed <- dataset[labeled,]
others <- dataset[-labeled,]

unsup.res <- c()
for (i in seq(100,1900,100)){
  nas <- sample(nrow(others), i)
  data.selfTr <- others[nas,]
  #data.selfTr[,'y'] <- NA
  learn.sup <- mixmodCluster(data=data.selfTr[,1:30], criterion= "BIC", nbCluster = 2)
  prediction.sup <- mixmodPredict(dataset[dataTest,1:30],learn.sup["bestResult"])
  paste("accuracy= ",mean(as.integer(dataset[dataTest,58]) == prediction.sup["partition"])*100,"%",sep="")
  t <- mean(as.integer(dataset[dataTest,58]) == prediction.sup["partition"])
  if (t > 0.5){
    t <- 1-t
  }
  print(t)
  unsup.res <- c(unsup.res,t)
}


#model <- runLearner(learner, form, data[sup, ])
dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)
sample <- dataset[sample(nrow(dataset), 1500),]
dataTrain <- sample
dataTest <- sample[1350:1500,]

result <- c()
temp <- c()
for (i in seq(150,1500,150))
{
  
  #dataset$y <- as.factor(dataset$y)
  #sample <- dataset[sample(nrow(dataset), 400+i),]
  #dataTrain <- sample[1:(200+i),]
  #dataTest <- sample[(200+i+1):(400+i),]
  
  lab <- as.integer(dataTrain[,58])
  lab[(i+1):1500] <- 0
  
  learn <- mixmodCluster(data=dataTrain[,1:30], dataType = "quantitative",model =mixmodGaussianModel(), criterion= c("BIC","ICL","NEC"), nbCluster = 2, knownLabels = lab)
  prediction <- mixmodPredict(dataTest[,1:30],learn["bestResult"])
  paste("accuracy= ",mean(as.integer(dataTest[,58]) == prediction["partition"])*100,"%",sep="")
  print(mean(as.integer(dataTest[,58]) == prediction["partition"]))
  temp <- c(temp,mean(as.integer(dataTest[,58]) == prediction["partition"]))
}
# [1] 0.7218543
# [1] 0.7218543
# [1] 0.6953642
# [1] 0.6887417
# [1] 0.7417219
# [1] 0.7350993
# [1] 0.7417219
# [1] 0.7086093