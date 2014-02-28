wine = read.csv (url('http://www.nd.edu/~mclark19/learn/data/goodwine.csv'))

library(lattice)

library(ggplot2)

library(e1071)

library(doSNOW)
registerDoSNOW(makeCluster(3, type="SOCK"))

library(corrplot)
corrplot(cor(wine[, -c(13, 15)]), method = "number", tl.cex = 0.5)

library(caret)
set.seed(1234)

library(ggplot2)



#so that the indices will be the same when re-run
trainIndices = createDataPartition(wine$good, p = 0.8, list = F)
wanted = !colnames(wine) %in% c("free.sulfur.dioxide", "density", "quality", "color", "white")

wine_train = wine[trainIndices, wanted]

#remove quality and color, as well as density and others

wine_test = wine[-trainIndices, wanted]



wine_trainplot = predict(preProcess(wine_train[,-10], method="range"), wine_train[,-10])

featurePlot(wine_trainplot, wine_train$good, "box")



set.seed(1234)
cv_opts = trainControl(method="cv", number=10)
knn_opts = data.frame(.k=c(seq(3, 11, 2), 25, 51, 101)) #odd to avoid ties
results_knn = train(good~., data=wine_train, method="knn",
                    preProcess="range", trControl=cv_opts,
                    tuneGrid = knn_opts)
results_knn

preds_knn = predict(results_knn, wine_test[,-10])
confusionMatrix(preds_knn, wine_test[,10], positive='Good')

dotplot = dotPlot(varImp(results_knn))

