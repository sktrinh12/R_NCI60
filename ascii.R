c14175<-read.csv('C:/users/trinh/downloads/ascii files/C14175_4509_11_15.txt',header = F, skip=2,sep='')

#grep("[0-9]{1}.[0-9]{3}",c14175$V1)
#rt.index<-which(c14175$V1=='Retention')
#rt.index<-rt.index+1
#rt<-c14175$V1[rt.index]
#rt<-droplevels(rt)
rt<-c14175$V3[!is.na(c14175$V3)]

all.nm<-c14175$V1
removetext<-c('Scan','Retention','Time')
all.nm<-gsub(paste(removetext,collapse = '|'),'',all.nm)
#all.nm<-gsub('\\s+', '',all.nm)
#all.nm[all.nm=='']<-NA
all.nm<-as.numeric(all.nm)
abs.index<-which(all.nm==250.5680)
length(abs.index)
length(rt)

all.abs<-c14175$V2
all.abs<-gsub('Time','',all.abs)
all.abs<-as.numeric(all.abs)




df<-data.frame(rt=rt,abs=all.abs[abs.index])

plot(df,type='l',col='blue',lwd=2)

#---------------
library(ggplot2)

c17897<-read.csv('C:/users/trinh/documents/nmr_workflow/r plots/C17897_4509_8.TXT',header = F, skip=18,sep='')
head(c17897,150)

plotnm<-function(filepath,nm){
  testread<-read.csv(filepath,header=F,nrows=20,sep='')
  functionwrd<-which(testread$V1 == 'FUNCTION')
  realread<-read.csv(filepath,header=F,skip=functionwrd+1,sep='')
  rt<-realread$V3[!is.na(realread$V3)]
  all.nm<-realread$V1
  removetext<-c('Scan','Retention')
  all.nm<-gsub(removetext[1],'',all.nm)
  all.nm<-gsub(removetext[2],'',all.nm)
  all.nm<-as.numeric(all.nm)
  abs.index<-which(round(all.nm)==nm)
  all.abs<-realread$V2
  all.abs<-gsub('Time','',all.abs)
  all.abs<-as.numeric(all.abs)
  df<-data.frame(rt=rt,abs=all.abs[abs.index])
  #plot(df,type='l',col='blue',lwd=2)
  ggplot(data=df,aes(x=rt,y=abs))+geom_line(size=1,colour='#CB181D')+xlab('RT (min)')+ylab(paste0('Abs @ ',nm,'nm'))+
      theme(
      panel.background = element_rect(fill = "transparent",colour = NA), 
      panel.grid.minor = element_blank(), 
      panel.grid.major = element_blank(),
      axis.text.x=element_text(size=14),
      axis.text.y=element_text(size=14),
      axis.title=element_text(size=16),
      plot.background = element_rect(fill = "transparent",colour = NA),
      panel.border = element_rect(colour = "black", fill='transparent', size=1.5),
      axis.ticks = element_line(colour='black',size = 1)
    ) 
}

plotnm('C:/users/trinh/documents/nmr_workflow/r plots/C17897_4509_8.TXT',360)
plotnm('C:/users/trinh/downloads/C8787_6_4509_15_M31-32.TXT',290)
plotnm('C:/users/trinh/downloads/C587_5_4509_16_11_C50_51.TXT',240)

ggsave(filename='C587.png',plot=last_plot(),
       path='c:/users/trinh/downloads/',width=6.5,dpi=400,bg='transparent')
