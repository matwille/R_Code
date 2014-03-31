
library(tree)

data(iris)
names(iris)
plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
partition.tree(tree1, label="Species", add=TRUE)
legend(1,4,5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)

tree1 <- tree(Species ~ Sepal.Width + Petal.Width,data=iris)
summary(tree1)


plot(tree1)
text(tree1)



set.seed(32313)
newdata <- data.frame(Petal.Width = runif(20,0,2.5),Sepal.Width = runif(20,2,4.5))
pred1 <- predict(tree1,newdata, type="class")
plot(newdata$Petal.Width,newdata$Sepal.Width,col=as.numeric(pred1),pch=19)
partition.tree(tree1,"Species",add=TRUE)