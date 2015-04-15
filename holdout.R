data(swiss)
## First the user defined function (note: can have any name)
hld.rpart <- function(form, train, test, ...) {
  require(rpart)
  model <- rpart(form, train, ...)
  preds <- predict(model, test)
  regr.eval(resp(form, test), preds,
            stats=c('mae','nmse'), train.y=resp(form, train))
}

hld.rpart(y~., train,test)
## Now the evaluation
eval.res <- holdOut(learner('hld.rpart',pars=list()),
                    dataset(Infant.Mortality ~ ., swiss),
                    hldSettings(10,0.3,1234))
## Check a summary of the results
summary(eval.res)
## Plot them
## Not run:
plot(eval.res)
