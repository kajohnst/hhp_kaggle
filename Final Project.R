setwd("/Users/johnston2410/heritage-health-prize")
alldata <- read.csv("modeling_set_Y1claims_Y2days.csv")
usedrows <- which(alldata$YEAR_t != 'Y3')
usedData <- alldata[usedrows,]

########################################
# build the model Random Forest
########################################
library(randomForest)
newModel <- randomForest(x = usedData[,-4], y = usedData[,4], xtest=NULL, ytest=NULL, ntree=100,
             mtry=if (!is.null(y) && !is.factor(y))
               max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x))),
             replace=TRUE, classwt=NULL, cutoff, strata,
             sampsize = if (replace) nrow(x) else ceiling(.632*nrow(x)),
             nodesize = if (!is.null(y) && !is.factor(y)) 5 else 1,
             maxnodes = NULL,
             importance=FALSE, localImp=FALSE, nPerm=1,
             proximity, oob.prox=proximity,
             norm.votes=TRUE, do.trace=FALSE,
             keep.forest=!is.null(y) && is.null(xtest), corr.bias=FALSE,
             keep.inbag=FALSE)


#predict for the leaderboard data
prediction <- predict(object = newModel, newdata = test[,-2], type="response",
                      norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)

max(prediction)
min(prediction)
#put on correct scale and cap
prediction <- (10^prediction)-1

prediction <- pmin(15,prediction)
prediction <- pmax(0,prediction)

#plot the submission distribution
hist(prediction, breaks=500)