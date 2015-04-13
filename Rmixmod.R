library(Rmixmod)
## A quantitative example with the famous geyser data set
data(geyser)
## with default values
m1 <- mixmodCluster(geyser, nbCluster=2:6)
## A qualitative example with the birds data set
data(birds)
mixmodCluster(data=birds, nbCluster = 2:5, criterion= c("BIC","ICL","NEC"),
              model = mixmodMultinomialModel())
## use graphics functions
xem <- mixmodCluster(data=geyser, nbCluster=3)
## Not run:
plot(xem)
hist(xem)
## End(Not run)
## get summary
summary(xem)
## A composite example with a heterogeneous data set
data(heterodata)
mixmodCluster(heterodata,2)


## A quantitative example with the famous iris data set
data(iris)
## with default values
new("MixmodCluster", data=iris[1:4], nbCluster=3)
getSlots("MixmodCluster")



# start by extract 10 observations from iris data set
remaining.obs<-sample(1:nrow(iris),10)
# then run a mixmodLearn() analysis without those 10 observations
learn<-mixmodLearn(iris[-remaining.obs,1:4], iris$Species[-remaining.obs])
# create a MixmodPredict to predict those 10 observations
prediction <- mixmodPredict(data=iris[remaining.obs,1:4], classificationRule=learn["bestResult"]) # show results
prediction
# compare prediction with real results
paste("accuracy= ",mean(as.integer(iris$Species[remaining.obs]) == prediction["partition"])*100 ,"%",sep="")
## A composite example with a heterogeneous data set
data(heterodatatrain)
## Learning with training data
learn <- mixmodLearn(heterodatatrain[-1],knownLabels=heterodatatrain$V1)
## Prediction on the testing data
data(heterodatatest)
prediction <- mixmodPredict(heterodatatest[-1],learn["bestResult"])
# compare prediction with real results
paste("accuracy= ",mean(heterodatatest$V1 == prediction["partition"])*100,"%",sep="")


## A quantitative example with the famous iris data set
learn.iris<-mixmodLearn(iris[1:4], iris$Species)
## get summary
summary(learn.iris)
## A qualitative example with the famous birds data set
data(birds)
birds.partition<-c(rep(1,34),rep(2,35))
learn.birds<-mixmodLearn(data=birds, knownLabels=birds.partition)
## get summary
summary(learn.birds)
## A composite example with a heterogeneous data set
data(heterodatatrain)
learn.hetero<-mixmodLearn(heterodatatrain[-1],knownLabels=heterodatatrain$V1)
## get summary
summary(learn.hetero)