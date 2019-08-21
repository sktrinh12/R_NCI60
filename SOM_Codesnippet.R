library(kohonen)
data("wines")

wines

wines.train<-sample(nrow(wines),120)
wines.Xtrain<-scale(wines[wines.train,])
wines.test<-scale(wines[-wines.train,],
                  center=attr(wines.Xtrain,"scaled:center"),
                  scale=attr(wines.Xtrain,"scaled:scale"))    #get attribute scale & center
wines.grid<-somgrid(5,5,"hexagonal")
wines.grid$n.hood<-"circular"
wines.nhbrdist<-unit.distances(wines.grid, FALSE) 
wines.r<-quantile(wines.nhbrdist, 0.67) * c(1, -1) 
wines.som<-som(wines.Xtrain,wines.grid,rlen=200,alpha=c(0.05,0.01),keep.data=T)
plot(wines.som,"changes")
plot(wines.som,"dist.neighbours")
plot(wines.som,"counts")
wines.hc<-cutree(hclust(dist(wines.som$codes[[1]])),3)
plot(wines.som,"mapping",bgcol=rainbow(3)[wines.hc])
add.cluster.boundaries(wines.som,wines.hc)
wines.class<-wines.som$unit.classif
winesom.pred<-predict(wines.som,newdata=wines.test,trainX=wines.Xtrain,trainY=factor(wines.class[wines.train]))
#table(wines.class[-wines.train],winesom.pred$predictions[[1]])
map(wines.som,wines.test) #similar result as predict


training <- sample(nrow(wines), 120)
Xtraining <- scale(wines[training, ])
Xtest <- scale(wines[-training, ],
               center = attr(Xtraining, "scaled:center"),
               scale = attr(Xtraining, "scaled:scale"))
trainingdata <- list(measurements = Xtraining,
                     vintages = vintages[training])
testdata <- list(measurements = Xtest, vintages = vintages[-training])

mygrid = somgrid(5, 5, "hexagonal")
som.wines <- supersom(trainingdata, grid = mygrid)

## ################################################################
## Situation 0: obtain expected values for training data (all layers,
## also if not used in training) on the basis of the position in the map
som.prediction <- predict(som.wines)

## ################################################################
## Situation 1: obtain predictions for all layers used in training

som.prediction <- predict(som.wines, newdata = testdata)
table(vintages[-training], som.prediction$predictions[["vintages"]])


## ################################################################
## Situation 2: obtain predictions for the vintage based on the mapping
## of the sample characteristics only. There are several ways of doing this:

som.prediction <- predict(som.wines, newdata = testdata,
                          whatmap = "measurements")
table(vintages[-training], som.prediction$predictions[["vintages"]])

## same, but now indicated implicitly
som.prediction <- predict(som.wines, newdata = testdata[1])
table(vintages[-training], som.prediction$predictions[["vintages"]])

## if no names are present in the list elements whatmap needs to be
## given explicitly; note that the order of the data layers needs to be
## consistent with the kohonen object
som.prediction <- predict(som.wines, newdata = list(Xtest), whatmap = 1)
table(vintages[-training], som.prediction$predictions[["vintages"]])


## ###############################################################
## Situation 3: predictions for layers not present in the original
## data. Training data need to be provided for those layers.
som.wines <- supersom(Xtraining, grid = mygrid)
som.prediction <- predict(som.wines, newdata = testdata,
                          trainingdata = trainingdata)
table(vintages[-training], som.prediction$predictions[["vintages"]])

## ################################################################
## yeast examples, including NA values

data(yeast)
training.indices <- sample(nrow(yeast$alpha), 300)
training <- rep(FALSE, nrow(yeast$alpha))
training[training.indices] <- TRUE

## unsupervised mapping, based on the alpha layer only. Prediction
## for all layers including alpha
yeast.som <- supersom(lapply(yeast, function(x) subset(x, training)),
                      somgrid(4, 6, "hexagonal"),
                      whatmap = "alpha", maxNA.fraction = .5)
yeast.som.prediction <-
  predict(yeast.som,
          newdata = lapply(yeast, function(x) subset(x, !training)))

table(yeast$class[!training], yeast.som.prediction$prediction[["class"]])

## ################################################################
## supervised mapping - creating the map is now based on both
## alpha and class, prediction for class based on the mapping of alpha.
yeast.som2 <- supersom(lapply(yeast, function(x) subset(x, training)),
                       grid = somgrid(4, 6, "hexagonal"),
                       whatmap = c("alpha", "class"), maxNA.fraction = .5)
yeast.som2.prediction <-
  predict(yeast.som2,
          newdata = lapply(yeast, function(x) subset(x, !training)),
          whatmap = "alpha")
table(yeast$class[!training], yeast.som2.prediction$prediction[["class"]])








kohmap <- xyf(scale(wines), vintages,
              grid = somgrid(5, 5, "hexagonal"), rlen=100)
plot(kohmap, type="changes")
counts <- plot(kohmap, type="counts", shape = "straight")

## show both sets of codebook vectors in the map
par(mfrow = c(1,2))
plot(kohmap, type="codes", main = c("Codes X", "Codes Y"))

par(mfrow = c(1,1))
similarities <- plot(kohmap, type="quality", palette.name = terrain.colors)
plot(kohmap, type="mapping",
     labels = as.integer(vintages), col = as.integer(vintages),
     main = "mapping plot")

## add background colors to units according to their predicted class labels
xyfpredictions <- classmat2classvec(getCodes(kohmap, 2))
bgcols <- c("gray", "pink", "lightgreen")
plot(kohmap, type="mapping", col = as.integer(vintages),
     pchs = as.integer(vintages), bgcol = bgcols[as.integer(xyfpredictions)],
     main = "another mapping plot", shape = "straight", border = NA)

## Show 'component planes'
set.seed(7)
sommap <- som(scale(wines), grid = somgrid(6, 4, "hexagonal"))
plot(sommap, type = "property", property = getCodes(sommap, 1)[,1],
     main = colnames(getCodes(sommap, 1))[1])

## Show the U matrix
Umat <- plot(sommap, type="dist.neighbours", main = "SOM neighbour distances")
## use hierarchical clustering to cluster the codebook vectors
som.hc <- cutree(hclust(object.distances(sommap, "codes")), 5)
add.cluster.boundaries(sommap, som.hc)

## and the same for rectangular maps
set.seed(7)
sommap <- som(scale(wines),grid = somgrid(6, 4, "rectangular"))
plot(sommap, type="dist.neighbours", main = "SOM neighbour distances")
## use hierarchical clustering to cluster the codebook vectors
som.hc <- cutree(hclust(object.distances(sommap, "codes")), 5)
add.cluster.boundaries(sommap, som.hc)
# }