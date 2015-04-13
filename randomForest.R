library(randomForest)
Ban.rf <- randomForest(as.factor(Aktiv) ~., data=Ban)
Ban.rf # contains OOB error rate