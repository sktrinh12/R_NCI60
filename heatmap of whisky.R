library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(plyr) 

whisky<-read.csv("C:/users/trinh/downloads/python/whiskies.txt",header = T)
grep("Body",colnames(whisky)) #3rd column
grep("Floral",colnames(whisky)) #14th column
flavors<-whisky[3:14]

flavors.cor<-as.data.frame(cor(flavors,flavors,method="pearson"))

#upper triangle
flavors.flat.up<-data.frame(i = rownames(flavors.cor)[row(flavors.cor)[upper.tri(flavors.cor,diag = T)]],
           j = rownames(flavors.cor)[col(flavors.cor)[upper.tri(flavors.cor,diag = T)]],
           cor=(flavors.cor)[upper.tri(flavors.cor,diag = T)])

#lower triangle
flavors.flat.lw<-data.frame(i = rownames(flavors.cor)[row(flavors.cor)[lower.tri(flavors.cor,diag = T)]],
                         j = rownames(flavors.cor)[col(flavors.cor)[lower.tri(flavors.cor,diag = T)]],
                         cor=(flavors.cor)[lower.tri(flavors.cor,diag = T)])

#combine triangles to make full matrix
flavors.flat<-rbind(flavors.flat.up,flavors.flat.lw)

#plot heatmap on ggplot
gg<-ggplot(data=flavors.flat,aes(j,i))+geom_tile(aes(fill=cor),color="white")
gg<-gg+scale_fill_gradient2(low = "blue",mid="springgreen",high = "red")
gg<-gg+ylab("Wine characteristics") +xlab("Wine characteristics") +
  theme(legend.title = element_text(size = 11),
        legend.text = element_text(size = 11),
        plot.title = element_text(size=16),
        axis.title=element_text(size=12,face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
gg


#take correlations across distilleries by taking transpose
flavors.t<-t(flavors)
correlations<-as.matrix(cor(flavors.t,flavors.t,method="p"))

#upper triangle
flavors.t.flat.up<-data.frame(i = rownames(flavors.t.cor)[row(flavors.t.cor)[upper.tri(flavors.t.cor,diag = T)]],
                            j = rownames(flavors.t.cor)[col(flavors.t.cor)[upper.tri(flavors.t.cor,diag = T)]],
                            cor=(flavors.t.cor)[upper.tri(flavors.t.cor,diag = T)])

#lower triangle
flavors.t.flat.lw<-data.frame(i = rownames(flavors.t.cor)[row(flavors.t.cor)[lower.tri(flavors.t.cor,diag = T)]],
                            j = rownames(flavors.t.cor)[col(flavors.t.cor)[lower.tri(flavors.t.cor,diag = T)]],
                            cor=(flavors.t.cor)[lower.tri(flavors.t.cor,diag = T)])

#combine row wise since we took transpose
flavors.t.flat<-rbind(flavors.t.flat.up,flavors.t.flat.lw)

#plot heatmap of distilleries across Sctoland
gg.t<-ggplot(data=flavors.t.flat,aes(j,i))+geom_tile(aes(fill=cor),color="white")
gg.t<-gg.t+scale_fill_gradient2(low = "blue",mid="springgreen",high = "red")
gg.t<-gg.t+scale_x_discrete(breaks=seq(0,90,10))
gg.t<-gg.t+scale_y_discrete(breaks=seq(0,90,10))+ylab("Distilleries across Sctoland")+
  xlab("Distilleries across Sctoland")
gg.t

#======================================================================================
#======================================================================================
#kmeans 
flavors.fit<-kmeans(correlations,6) #6 classes/groups

#column bind grouping/class to whisky df
whisky<-cbind(whisky,flavors.fit$cluster)
#order by 'group' 
whisky<-whisky[order(whisky['flavors.fit$cluster']),]
#reset index numbering
rownames(whisky)<-seq(length=nrow(whisky))
corr<-as.matrix(cor(t(whisky[3:14]),t(whisky[3:14]),method = 'p')) #make new correlations with new re-labeled whiskyy df

#melt correlation matrix
corr.melt<-melt(corr)

#rename elements of corr.melt to the name of whisky distillery
distillery<-data.frame(num=seq_along(whisky$Distillery),distillery=whisky$Distillery)
corr.melt$Var1<-distillery[match(corr.melt$Var1,distillery$num),'distillery']
corr.melt$Var2<-distillery[match(corr.melt$Var2,distillery$num),'distillery']

#define color palette
color.cluster<-brewer.pal(8,"Set2")
white.color<-"#FFFFCC"
bkg.color<-"#CCCCCC"
color.cluster<-c(color.cluster,white.color,bkg.color)
#names(color.cluster)<-sort(c(unique(flavors.fit$cluster),"7","8"))

#generate color column that corresponds to the cluster
flavors.color<-NULL

for (i in seq(length(whisky$Distillery)))  {
  for (j in seq(length(whisky$Distillery)))  {
    if (corr[i,j] < 0.7) 
      {flavors.color<-c(flavors.color,color.cluster[8])} 
    else if (whisky$`flavors.fit$cluster`[i] == whisky$`flavors.fit$cluster`[j]) 
      {flavors.color<-c(flavors.color,color.cluster[whisky$`flavors.fit$cluster`[i]])}
    else {flavors.color<-c(flavors.color,color.cluster[7])}
  }
}

#plot heatmap
p<-ggplot(data=corr.melt,aes(Var1,Var2)) + geom_tile(aes(fill=flavors.color),color="white") 
p+ scale_fill_manual(values = color.cluster)

