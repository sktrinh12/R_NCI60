df<-read.csv("C:/users/trinh/downloads/CANCER_5_DOSE_DATA.csv")

fduniqcol<-function(df,nsc){
  require(dplyr)
  tmpdf<-df %>% filter(NSC==nsc) %>% arrange(CELLNAME)
  uniqcn<-as.vector(unique(tmpdf$CELLNAME))
  lgthcn<-length(uniqcn)
  return(list(uniqcn,lgthcn))
}
lstOfNSC<-unique(df$NSC)
dfm.u<-sapply(lstOfNSC,function(x) fduniqcol(df,x))
numcol<-c()
numcol<-sapply(seq(lstOfNSC), function(x) numcol<-c(numcol,dfm.u[2,x][[1]]))
sixtycol<-sapply(which(numcol==60),function(x) dfm.u[1,x])
unqcol<-which(!duplicated(sixtycol))
whcunq<-which(sixtycol[[unqcol[1]]] != sixtycol[[unqcol[2]]]) #what row differs between two unique column names
data.frame(sixtycol[[unqcol[1]]],sixtycol[[unqcol[2]]])[whcunq,] #the celline that is different within all the 60 columnames
table(unlist(lapply(sixtycol,function(x) all(sixtycol[unqcol[1]][[1]] %in% x))))
table(unlist(lapply(sixtycol,function(x) all(sixtycol[unqcol[2]][[1]] %in% x))))
#so unqcol[1] index of 1 has more (370) of duplicated columnames, so use this as default, standard column naming

ndf_raw<-data.frame(first=0)

concatcg <- function(df,nsc,ndf_raw){
  require('dplyr')
  tmpdf<-df %>% filter(NSC == nsc) %>% arrange(CELLNAME)
  nscexp <- paste0(as.character(nsc),'-',tmpdf$EXPID[1])
  ndf_raw$NSC <- nscexp
  lendf<-nrow(tmpdf)/5
  fivedinter<-rep(c("1","2","3","4","5"),lendf)
  tmpdf<-tmpdf %>% mutate(CELLNAME=paste0(CELLNAME,'_',fivedinter))
  for (i in 1:nrow(tmpdf)){
    nm<-paste0(tmpdf$CELLNAME[i])
    ndf_raw[nm] <- as.numeric(tmpdf$GIPRCNT[i])
  }
  ndf_raw<-ndf_raw[,-1] #remove first colmn
  }

dfm <- sapply(lstOfNSC,function(x) concatcg(df,x,ndf_raw))


fivedinter<-c("1","2","3","4","5") #5 concs for each cell line
newcolnms<-sapply(sixtycol[[1]],function(x) paste0(x,'_',fivedinter))
ndf_merge<-data.frame(matrix(NA,nrow=1,ncol=length(newcolnms)+1)) #+1 for nsc colm
colnames(ndf_merge)<-c('NSC',newcolnms)


for (j in seq(length(dfm))){
  tmpdf <- data.frame(matrix(NA,nrow=1,ncol=length(colnames(ndf_merge))))
  colnames(tmpdf) <- colnames(ndf_merge)
  tmpdf['NSC'] <- dfm[[j]]$NSC
  for (k in colnames(dfm[[j]][,-1])){ #do not include the NSC column in search
    if (k %in% colnames(ndf_merge)){
      tmpdf[k] <- dfm[[j]][k]
    } else
    {next}
  }
  ndf_merge <- rbind(ndf_merge,tmpdf)
}
ndf_merge<-ndf_merge[-1,]


##TRAIN SOM
library(kohonen)
trainSOM<-function(som_grid, data){
  set.seed(7)
  som_grid$n.hood<-"circular"
  nhbrdist<-unit.distances(som_grid, FALSE) #distanes bt grid units are calculated by fc, toroidal is se to False
  r<-quantile(nhbrdist, 0.67) * c(1, -1) #return estimates of distribution quantiles based on probability of 0.67
  som_model<-supersom(data,grid=som_grid,
                      rlen=150, 
                      maxNA.fraction = .9, #threshold for allowable NA values
                      alpha=c(0.05,0.01), 
                      radius=r,
                      keep.data = T)
  return(som_model)
}

d.somtrain<-data.frame(ndf_merge[sample(nrow(ndf_merge),2000),]) #sample fom the df
rownames(d.somtrain) <- d.somtrain$NSC
d.cov<-cov(d.somtrain[,-1],use="pairwise.complete.obs")
munits<-5*nrow(d.somtrain)^0.5
A<-eigen(d.cov)
eigval<-sort(A$values,decreasing = T)[1:2]
ratio<-eigval[1]/eigval[2]
size1=min(munits,round(sqrt(munits/ratio*sqrt(0.75)))) #sqrt of 112/(1.65)(sqrt(3/4)), minimum of 112 and 8 will be 8?
size2=round(munits/size1)
dmat.somtrain<-as.matrix(d.somtrain[,-1]) 
som_grid<-somgrid(xdim=size2,ydim=size1,topo="hexagonal")
som_model<-trainSOM(som_grid, list(dmat.somtrain))
plot(som_model,"counts")
plot(som_model,"changes")
plot(som_model,"dist.neighbours")

pltmsom<-function(n,som_model) {
  for (i in seq(n)){
    shc<-cutree(hclust(dist(som_model$codes[[1]])),i+5)
    plot(som_model,"mapping",bgcol=RColorBrewer::brewer.pal(i+5,"Set3")[shc],main=paste("SOM Plot with ",i+5," groups/classes"))
    add.cluster.boundaries(som_model,shc)
    readline(prompt="Press [enter] to continue")
    if (i+5 >= n){
      break
    }
  }
}

pltmsom(12,som_model)

#plot som with visually most appropriate amount of classifications
nodecls<-som_model$unit.classif
dfn<-data.frame(matrix(NA,nrow=1,ncol=3))
colnames(dfn) <- c("NSC-EXPID","NODE","CLASS")
shc<-cutree(hclust(dist(som_model$codes[[1]])),9)
for (i in seq(nodecls)){
  nsc<-row.names(dmat.somtrain)[i]
  nd<-nodecls[i]
  grp<-shc[nodecls[i]]
  dfn<-rbind(dfn,c(nsc,nd,grp))
  if (i == length(nodecls)){dfn<-dfn[complete.cases(dfn),]; print(dfn)}
}
plot(som_model,"mapping",bgcol=rainbow(9)[shc],main='SOM Plot of 5-dose data')
grpnm<-sapply(sort(unique(as.numeric(dfn$CLASS))),function(x) paste("Class",x))
legend("bottom", legend = grpnm, text.col = rainbow(10),bty='n',
       horiz=T,cex=1,inset = c(0,0.2),xjust=0,yjust=0,x.intersp = -0.25,
       text.width = rep(0,9))