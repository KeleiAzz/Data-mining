## Not run:
library(phyclust, quiet = TRUE)
X <- seq.data.toy$org
set.seed(1234)
(ret.1 <- phyclust(X, 3))
EMC.2 <- .EMC
EMC.2$substitution.model <- "HKY85"
# the same as EMC.2 <- .EMControl(substitution.model = "HKY85")
(ret.2 <- phyclust(X, 3, EMC = EMC.2))
# for semi-supervised clustering
semi.label <- rep(0, nrow(X))
semi.label[1:3] <- 1
semi.label[4:6] <- 2
semi.label[7:9] <- 3
(ret.3 <- phyclust(X, 3,  label = semi.label))

dataset <- read.csv("data/spambase/data.csv",header=FALSE,sep=";")
names <- read.csv("data/spambase/names.csv",header=FALSE,sep=";")
names(dataset) <- sapply((1:nrow(names)),function(i) toString(names[i,1]))
dataset$y <- as.factor(dataset$y)
sample <- dataset[sample(nrow(dataset), 500),]
#trainIndex <- createDataPartition(sample$y, p = .8, list = FALSE, times = 1)
unlabel <- sample(500,450)

lab <- as.integer(sample$y)
lab[unlabel] <- 0

ret.3 <- phyclust(sample[,55:57], 2, EMC = EMC.2, label = lab)



# library(phyclust, quiet = TRUE)
# set.seed(1234)
# EMC.1 <- .EMC
# EMC.1$EM.iter <- 1
# the same as EMC.1 <- .EMControl(EM.iter = 1)
# X <- seq.data.toy$org
# ret.1 <- phyclust(X, 2, EMC = EMC.1)
# ret.2 <- phyclust.e.step(X, ret.phyclust = ret.1)
# str(ret.2)
# For semi-supervised clustering.
# semi.label <- rep(0, nrow(X))
# semi.label[1:3] <- 1
# ret.3 <- phyclust.e.step(X, ret.phyclust = ret.1, label = semi.label)
