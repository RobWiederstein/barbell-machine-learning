mods <- list.files(path = "./mods/", pattern = "fit", full.names = T)
lapply(mods, load, .GlobalEnv)

#Model with best accuracy is rf
resamps <- resamples(list(gbm = fit.gbm,
                          lda = fit.knn,
                          knn = fit.lb,
                          lb  = fit.lda,
                          nnet = fit.nnet,
                          rf = fit.rf,
                          svm = fit.svm
)
)

#plot comparisons of models
summary(resamps)
dotplot(resamps)
file <- "./plots/model_accuracy_kappa_results.pdf"
dev.copy2pdf(file = file, width = 6, height = 4)

#Plot results of random forest model
plot(fit.rf, main = "Random Forest Model")
file <- "./plots/rf_accuracy_no_boosting_iterations.pdf"
dev.copy2pdf(file = file, width = 6, height = 4)

#Plot variable Top 20 by relative influence
rfImp <- varImp(fit.rf, scale=FALSE)
dotPlot(rfImp, main = "Top 20 Variables Ranked by Relative Importance")
file <- "./plots/rf_top_20_variables.pdf"
dev.copy2pdf(file = file, width = 6, height = 4)

##outcome predictions on the testing set
testing <- read.csv("./data/testing.csv", header = T, stringsAsFactors = F)
load("./mods/fit_rf")
rfClasses <- predict(fit.rf, newdata = testing)
rfProbs <- predict(fit.rf, newdata = testing, type = "prob")
confusionMatrix(data = rfClasses, testing$classe)
