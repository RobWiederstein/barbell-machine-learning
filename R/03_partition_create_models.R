file <- "./data/pml_training_clean.csv"
bb <- read.csv(file = file, header = T, stringsAsFactors = F)

#partition data
seed <- 107
set.seed(seed)
library(caret)
inTrain <- createDataPartition(y = bb$classe,
                               p = .75,
                               list = FALSE)
training <- bb[inTrain, ]
write.csv(training, "./data/training.csv", row.names = F)
testing <- bb[-inTrain, ]
write.csv(testing, "./data/testing.csv", row.names = F)

##############Set up Control Parameters
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 3,
                     classProbs = T,
                     #summaryFunction = twoClassSummary
)
#############Set up Parallel Processing
library(doMC)
registerDoMC(cores = 5)

############ Model 1--stochastic gradient boosting -- gbm
###   WARNING:  Subset to a random 1000 b/c couldn't get it to work.
if(!file.exists("./mods/fit_gbm")){
        set.seed(seed)
        training <- training[sample(1:nrow(training), size = 1000), ]
        fit.gbm <- train(classe ~ .,
                        data = training,
                        method = "gbm",
                        tuneLength = 15,
                        trControl = ctrl,
                        metric = "Accuracy",
                        preProc = c("center", "scale")
        )
        save(fit.gbm, file = "./mods/fit_gbm")
}
#######model 2 -- boosted logistic regression -- lb
if(!file.exists("./mods/fit_lb")){
        set.seed(seed)
        fit.lb <- train(classe ~ .,
                        data = training,
                        method = "LogitBoost",
                        tuneLength = 15,
                        trControl = ctrl,
                        metric = "Accuracy",
                        preProc = c("center", "scale")
        )
        save(fit.lb, file = "./mods/fit_lb")
}
######model 3 -- linear descriminate analysis -- lda
if(!file.exists("./mods/fit_lda")){
        set.seed(seed)
        fit.lda <- train(classe ~ .,
                        data = training,
                        method = "lda",
                        tuneLength = 15,
                        trControl = ctrl,
                        metric = "Accuracy",
                        preProc = c("center", "scale")
        )
        save(fit.lda, file = "./mods/fit_lda")
}
######model 4 -- k-nearest neighbor -- knn
if(!file.exists("./mods/fit_knn")){
        set.seed(seed)
        fit.knn <- train(classe ~ .,
                         data = training,
                         method = "knn",
                         tuneLength = 15,
                         trControl = ctrl,
                         metric = "Accuracy",
                         preProc = c("center", "scale")
        )
        save(fit.knn, file = "./mods/fit_knn")
}
#####model 5 -- random forest -- rf
if(!file.exists("./mods/fit_rf")){
        set.seed(seed)
        fit.rf <- train(classe ~ .,
                         data = training,
                         method = "rf",
                         tuneLength = 15,
                         trControl = ctrl,
                         metric = "Accuracy",
                         preProc = c("center", "scale")
        )
        save(fit.rf, file = "./mods/fit_rf")
}

#### model 6-- svmRadialWeights -- svmRadialWeights
if(!file.exists("./mods/fit_svm")){
        set.seed(seed)
        fit.svm <- train(classe ~ .,
                        data = training,
                        method = "svmRadialWeights",
                        tuneLength = 15,
                        trControl = ctrl,
                        metric = "Accuracy",
                        preProc = c("center", "scale")
        )
        save(fit.svm, file = "./mods/fit_svm")
}

######## model 7 --neural net -- nnet
### Warning! subset to a 1000 observations
if(!file.exists("./mods/fit_nnet")){
        set.seed(seed)
        training <- training[sample(1:nrow(training), size = 1000), ]
        fit.nnet <- train(classe ~ .,
                         data = training,
                         method = "nnet",
                         tuneLength = 15,
                         trControl = ctrl,
                         metric = "Accuracy",
                         preProc = c("center", "scale")
        )
        save(fit.nnet, file = "./mods/fit_nnet")
}

rm(list = ls())
