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
for (i in seq(100,900,100))
{
  temp <- c()
  for (j in seq(1,10)){
    dataset$y <- as.factor(dataset$y)
    sample <- dataset[sample(nrow(dataset), 1200),]
    dataTrain <- sample[1:1000,]
    dataTest <- sample[1001:1200,]
    
    lab <- as.integer(dataTrain[,58])
    lab[1:i] <- 0
    
    learn <- mixmodCluster(data=dataTrain[,1:30], dataType = "quantitative",model =mixmodGaussianModel(), criterion= c("BIC","ICL","NEC"), nbCluster = 2, knownLabels = lab)
    prediction <- mixmodPredict(dataTest[,1:30],learn["bestResult"])
    paste("accuracy= ",mean(as.integer(dataTest[,58]) == prediction["partition"])*100,"%",sep="")
    temp <- c(temp,mean(as.integer(dataTest[,58]) == prediction["partition"]))
  }
}
# num = 50    100   150   200   250   300   350   400   450   500   550   600   650   700   750 
# res = 0.870 0.875 0.875 0.885 0.885 0.875 0.890 0.875 0.875 0.755 0.760 0.790 0.860 0.840 0.875
# 0.865 0.720 0.810 0.860 0.685 0.825 0.725 0.815 0.790
