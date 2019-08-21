library(ggplot2)
library(plyr)
library(dplyr)
d <- read.table("C:/Users/trinh/Desktop/Book1.csv", header=F,sep=",",skip=24)
t<-c(0.25,1,2,3,4,6,8,10)
d2<-d[,3:14]
d2<-cbind(d2,t)
df<-reshape2::melt(d2,id="t") #export melt f(x) from reshape2 namespace which would conflict with plyr/dplyr ... i think
d3<-df%>% filter(!variable == "t") #remove t from dataframe to prevent from plotting against itself

lm_eqn <- function(d3){
  m <- lm(value~t, d3);
  eq <- substitute(italic(y) == a~z~b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(abs(coef(m)[2]), digits = 2), 
                        z = ifelse(sign(coef(m)[2])==1, " + ", " - "),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}

regs<-ddply(d3,.(variable),lm_eqn) #Split d3, apply lm_eqn, and return results in a data frame, d3, the variable to split from is "variable"

plotlabel<-c("V3"="one","V4"="two","V5"="three","V6"="four","V7"="five","V8"="six","V9"="seven","V10"="eight","V11"="nine","V12"="ten","V13"="eleven","V14"="twelve")

ggplot(data=d3, aes(t,value)) +geom_point(shape=1) + ylab("Abs")+ xlab(expression(paste(Glucose,~mu,)))
geom_smooth(method='lm',se=F)+
  geom_text(data=regs,aes(x=2.75,y=1,label=V1),size=3,color="red",parse = TRUE)+
  facet_wrap(~variable,ncol=4,labeller = as_labeller(plotlabel)) #coerce/transform objects to labeller f(x)

#average the std curves:

z<-rowMeans(d2[,c("V13","V14")],na.rm=T)
z<-z[1:7]
z
glucon<-c(0,.001,.002,.003,.004,.005,.006)
avgr<-cbind(z,glucon) #bind glucose conc as 2nd column
avgr<-as.data.frame(avgr) #make into a df
avgr

#linear regression
lm.avgr<-lm(z~glucon)
lm.avgr

#make function to calc ypredict
xpredict<-function(absR){
  absEq<-coef(lm.avgr)[1]
  m<-coef(lm.avgr)[2]
  xp<-(absR-absEq)/m
  return(xp)
}

xpredict(.414)




