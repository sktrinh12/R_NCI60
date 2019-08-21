library(data.table)
library(kohonen)
trainSOM<-function(som_grid, data,w=c(1)){
  set.seed(7)
  som_grid$n.hood<-"circular"
  nhbrdist<-unit.distances(som_grid, FALSE)
  r<-quantile(nhbrdist, 0.67) * c(1, -1)
  som_model<-supersom(data,grid=som_grid,rlen=150, maxNA.fraction = .9, keep.data = T,
                      alpha=c(0.05,0.01), radius=r)
  return(som_model)
}
#r - the radius of the neighbourhood, either given as a single number or a vector (start, stop). If it is given as a single number the radius will run from the given number to the negative value of that number; as soon as the neighbourhood gets smaller than one only the winning unit will be updated. The default is to start with a value that covers 2/3 of all unit-to-unit distances.
#maxNA.fx -  the maximal fraction of values that may be NA to prevent the row or column to be removed.
d<-fread("M:\\Jason\\60CellDoseData\\som\\data_5dose3.csv")

d.som.train<-data.frame(d[sample(nrow(d),500),]) # however many you'd like

rownames(d.som.train)<-d.som.train$V1

#### estimate the size of the grid
d.c<-cov(d.som.train[,-1],use="pairwise.complete.obs")
munits=ceiling(5*dim(d.som.train)[1]^0.5)
A<-eigen(d.c)
eigval<-sort(A$values,decreasing=TRUE)[1:2]
ratio=sqrt(eigval[1]/eigval[2])
size1=min(munits,round(sqrt(munits/ratio*sqrt(0.75)))) #sqrt of 112/(1.65)(sqrt(3/4)), minimum of 112 and 8 will be 8?
size2=round(munits/size1)

d.som.train<-as.matrix(d.som.train[,-1]) #indx 2nd colm to last only

som_grid<-somgrid(xdim=size2,ydim=size1,topo="hexagonal")

som_model<-trainSOM(som_grid, list(d.som.train))

som_model$codes[[1]] # should be your codebook vectors



plot(som_model,"dist.neighbours")
plot(som_model,"changes")

pltmsom<-function(n,som_model) {
  for (i in seq(n)){
    shc<-cutree(hclust(dist(som_model$codes[[1]])),i+5)
    plot(som_model,"mapping",bgcol=rainbow(i+5)[shc],main=paste("SOM Plot with ",i+5," groups/classes"))
    add.cluster.boundaries(som_model,shc)
    readline(prompt="Press [enter] to continue")
    if (i+5 >= n){
      break
    }
  }
}

pltmsom(8,som_model)

nodecls<-som_model$unit.classif
dfn<-data.frame(matrix(NA,nrow=1,ncol=3))
colnames(dfn) <- c("nsc/expID","node","group")
for (i in seq(nodecls)){
  nsc<-row.names(d.som.train)[i]
  nd<-nodecls[i]
  grp<-shc[nodecls[i]]
  dfn<-rbind(dfn,c(nsc,nd,grp))
  if (i == length(nodecls)){dfn<-dfn[complete.cases(dfn),]; print(dfn)}
}

plot(som_model,"mapping",bgcol=rainbow(10)[shc])
grpnm<-sapply(sort(unique(as.numeric(dfn$group))),function(x) paste("Group",x))
legend("bottom", legend = grpnm, text.col = rainbow(10), bty = "o", horiz=T,cex=0.7,inset = c(0,0.15),xjust=0.5)
