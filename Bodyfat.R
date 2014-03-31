library(rpart)

library(mboost)

#split the data into training sets
set.seed(1234)
ind <- sample(2, nrow(bodyfat), replace=TRUE, prob=c(0.7, 0.3))
bodyfat.train <- bodyfat[ind==1,]
bodyfat.test <- bodyfat[ind==2,]

#train the decision tree
myFormula <- DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth

bodyfat_rpart <- rpart(myFormula, data = bodyfat.train, control = rpart.control(minsplit = 10))


plot(bodyfat_rpart)

text(bodyfat_rpart, use.n=T)


opt <- which.min(bodyfat_rpart$cptable[,"xerror"])

cp <- bodyfat_rpart$cptable[opt, "CP"]

bodyfat_prune <- prune(bodyfat_rpart, cp = cp)

plot(bodyfat_prune)

text(bodyfat_prune, use.n=T)


DEXfat_pred <- predict(bodyfat_prune, newdata=bodyfat.test)

xlim <- range(bodyfat$DEXfat)

plot(DEXfat_pred ~ DEXfat, data=bodyfat.test, xlab="Observed", ylab="Predicted", ylim=xlim, xlim=xlim)

abline(a=0, b=1)



     

