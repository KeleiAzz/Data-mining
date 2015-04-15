library(randomForest)
Ban.rf <- randomForest(as.factor(y) ~., data=dataset[sample(4601,200),-(31:57)])
Ban.rf # contains OOB error rate

bestModelsNames <- sapply(bestScores(res.all),
                          function(x) x['nmse','system'])
learners <- c(rf='randomForest',rpart='rpartXse') 
funcs <- learners[sapply(strsplit(bestModelsNames,'\\.'),
                         function(x) x[2])]
parSetts <- lapply(bestModelsNames,
                   function(x) getVariant(x,res.all)@pars)

bestModels <- list()
for(a in 1:7) {
  form <- as.formula(paste(names(clean.algae)[11+a],'~ .'))
  bestModels[[a]] <- do.call(funcs[a],
                             c(list(form,clean.algae[,c(1:11,11+a)]),parSetts[[a]]))
}


clean.test.algae <- knnImputation(test.algae,k=10,distData=algae[,1:11])


preds <- matrix(ncol=7,nrow=140)
for(i in 1:nrow(clean.test.algae)) 
  preds[i,] <- sapply(1:7,
                      function(x) 
                        predict(bestModels[[x]],clean.test.algae[i,])
  )


avg.preds <- apply(algae[,12:18],2,mean)
apply( ((algae.sols-preds)^2),           2,mean) / 
  apply( (scale(algae.sols,avg.preds,F)^2),2,mean)