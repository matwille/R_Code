


library(MASS)
library(tree)

treeCars <- tree(DriveTrain ~ MPG.city + MPG.highway + AirBags + EngineSize + Width + Length + Weight + Price + Cylinders + Horsepower + Wheelbase,data=Cars93)
plot(treeCars)
text(treeCars)

#cv.tree helps to prue the tree. This wil let you know how many leaves to allow in the tree.

par(mfrow=c(1,2))

#have to tell it to do misclass
plot(cv.tree(treeCars,FUN=prune.tree,method="misclass")) 

#standard method is devience
plot(cv.tree(treeCars))


#time to prune the tree. best=4 means give me the best tree that has 4 terminal nodes

pruneTree <- prune.tree(treeCars,best=4)
plot(pruneTree)
text(pruneTree)
