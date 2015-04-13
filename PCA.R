data(iris)
head(iris, 3)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

# apply PCA - scale. = TRUE is highly 
# advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,
                 center = TRUE,
                 scale. = TRUE) 
# print method
print(ir.pca)
plot(ir.pca, type = "l")
summary(ir.pca)

predict(ir.pca, 
        newdata=tail(log.ir, 2))

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.species, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

require(caret)
trans = preProcess(iris[,1:4], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC = predict(trans, iris[,1:4])

head(PC, 3)


# Loadings
trans$rotation

dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
spam.pca <- prcomp(dataset[,1:54], center = TRUE, scale. = TRUE)
plot(spam.pca, type = "l")
print(spam.pca)

library(psych)
fa.parallel(dataset[,1:54],fa="PC",n.iter=100,show.legend=FALSE,main="Screen plot with parallel analysis")
pc<-principal(dataset[,1:54],nfactors=14)
pc
newdata <- as.matrix(dataset[,1:54])%*%pc$weights[,1:14]

sample <- sample(4601,4000)
dataTrain <- sample[1:3800]
dataTest <- sample[3801:4000]
trST <- newdata[dataTrain,]
trST <- as.data.frame(trST)
trST[,'y'] <- as.factor(dataset$y[dataTrain])
unlabel <- sample(3800,3700)
trST[unlabel,'y'] <- NA



func <- function(m,d) {
  p <- predict(m,d,type='raw')
  data.frame(cl=colnames(p)[apply(p,1,which.max)],p=apply(p,1,max))
}
nbSTbase <- naiveBayes(y ~ .,trST[-unlabel,])
confusion(predict(nbSTbase,newdata[dataTest,]),dataset$y[dataTest])
nbST <- SelfTrain(y ~ .,trST,learner('naiveBayes',list()),'func')
confusion(predict(nbST,newdata[dataTest,]),dataset$y[dataTest])


round(unclass(pc$weights),2)  
head(pc$scores)  
#fa.parallel(Harman23.cor$cov,n.obs=302,fa="pc",n.iter=100,show.legend=FALSE,main="Scree plot with parallel analysis")  
rc<-principal(Harman23.cor$cov,nfactor=2,rotate="varimax")  
round(unclass(rc$weights),2)
