data(iris)
head(iris,3)

#log transofrm
log.ir<-log(iris[,1:4])
ir.species<-iris[,5]

#apply PCA - scale = T
ir.pca<-prcomp(log.ir,center=T,scale. = T)

#print PCA
print(ir.pca)

#plot method
plot(ir.pca,type='l')

#summary method
summary(ir.pca)

#predict PCs
predict(ir.pca,newdata=tail(log.ir,2))

library(devtools)
install_github('ggbiplot','vqv')
library(ggbiplot)
g<-ggbiplot(ir.pca,obs.scale = 1,var.scale = 1,groups=ir.species,ellipse = T,circle=T)
g<-g+theme(legend.direction='horizontal',legend.position='top')
g<-g+scale_color_discrete(name='') #to remove the title 'group' in legend (enter empty space)


#show circle (similar to hotel T circle)

require(ggplot2)
theta<-seq(0,2*pi,length.out=100) #length of seq = 100
circle<-data.frame(x=cos(theta),y=sin(theta))
p<-ggplot(circle,aes(x,y))+geom_path() #draw outer circle as template
loadings<-data.frame(ir.pca$rotation,.names=row.names(ir.pca$rotation))
p+geom_text(data=loadings,mapping=aes(x=PC1,y=PC2,label=.names,colour=.names))+
              coord_fixed(ratio=1) + labs(x='PC1',y='PC2')
#this adds the PC1 and PC2 loadings of the iris dataset to the plot within a unit circle             