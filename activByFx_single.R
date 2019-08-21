library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(zoo)
library(tidyr)



inpath<-'c:/Users/trinh/Downloads/Exp_1710OS92_1.csv'

onedose.tmp<-read.csv(inpath,header = T,sep = ',',skip=2)
onedose<-onedose.tmp[ , apply(onedose.tmp, 2, function(x) length(is.na(x))>10)]

splitTheXstr<-function(input){ #find columns with 'X' names
  tmpodc<-NULL
  tmp.grep<-grep("^X|X.[0-9]$",colnames(input),value=T)
  for (ech in tmp.grep){
  if (length(strsplit(ech,'')[[1]])<4)
    {
    tmpodc<-c(tmpodc,ech)
  }
  }
  return(tmpodc)
}
tmp.odc<-splitTheXstr(onedose.tmp)
odNox<-onedose[,!(names(onedose) %in% tmp.odc)] #index onedose df without the 'X' named columns

findstrIndx<-function(row_lbl,rl_lst){
tmp.rl.lst<-as.vector(row_lbl)
if ('L' %in% rl_lst){indxL<-which(rl_lst=='L')}
if ('N' %in% rl_lst){indxN<-which(rl_lst=='N')}
if ('S' %in% rl_lst){indxS<-which(rl_lst=='S')}
if ('M' %in% rl_lst){indxM<-which(rl_lst=='M')}
indxGT<-which(tmp.rl.lst=='Grand Total')
listOfIndx<-c('indxL','indxN','indxS','indxM','indxGT')
tryCatch({
  for (i in listOfIndx){if (exists(eval(as.name(i)))) {return(i)}}
},
error = function(e) e)
return(list(indxL=indxL,indxN=indxN,indxS=indxS,indxGT=indxGT))
}

tmp.rl.lst<-as.vector(odNox$Row.Labels) #vector of all nscs and unncessary strings
foundstrIndx<-findstrIndx(odNox$Row.Labels,tmp.rl.lst)


fac_nsclbl_L<-NULL
fac_nsclbl_N<-NULL
fac_nsclbl_S<-NULL
lst_nscl<-tmp.rl.lst[-c(foundstrIndx$indxL,foundstrIndx$indxN:length(tmp.rl.lst))]
lst_nscN<-tmp.rl.lst[-c(foundstrIndx$indxL:foundstrIndx$indxN,foundstrIndx$indxS:length(tmp.rl.lst))]
lst_nscS<-tmp.rl.lst[-c(foundstrIndx$indxL:foundstrIndx$indxS,foundstrIndx$indxGT)]
for(vals in lst_nscl[which(as.integer(lst_nscl)>8)]){fac_nsclbl_L<-c(fac_nsclbl_L,paste0('L',as.character(vals)))}
for(vals in lst_nscN[which(as.integer(lst_nscN)>8)]){fac_nsclbl_N<-c(fac_nsclbl_N,paste0('N',as.character(vals)))}
for(vals in lst_nscS[which(as.integer(lst_nscS)>8)]){fac_nsclbl_S<-c(fac_nsclbl_S,paste0('S',as.character(vals)))}

odNostr <- odNox[-c(foundstrIndx$indxL,foundstrIndx$indxN,foundstrIndx$indxS,foundstrIndx$indxGT),] #new onedose dataframe without string
od_L <-odNostr[1:(foundstrIndx$indxN-2),] #minus 2 bc you dont want the 1 from 1st fx of N and the nsc # itself
od_L <-od_L[-c(which(as.integer(as.vector(od_L$Row.Labels))>7)),] #remove nsc label rows 

df <- odNostr %>%
  filter(grepl("^\\d+$",Row.Labels)) %>% #filter out values that are strings (Letter prefixes), only maintain digits
  mutate(RowLabels_temp = ifelse(grepl("^\\d{3,}$",Row.Labels), as.numeric(as.character(Row.Labels)), NA)) %>% #add new column; if amt of SF is > 3 than coerce into number, otherwise coerce as NA
  na.locf() %>% #replace NA with the non-NA value prior to it; repeat the NSC# 6 times
  select(-Row.Labels) %>% #inverse select (select all but) the row.labels (first column)
  distinct() %>% #similar to unique.data.frame(); retain unique/distinct rows from input table
  group_by(RowLabels_temp) %>% #ensure that the rows are grouped by new Rowlabel_temp (wil move the N prefix nsc with its corresponding group (fx:1-7))
  mutate(RowLabels_indexed = row_number()-1) %>% #new column with each row number subtract 1
  arrange(RowLabels_temp, RowLabels_indexed) %>% #arrange in order of increasing rowlabel_temp and rowlabels_index
  mutate(RowLabels_indexed = ifelse(RowLabels_indexed==0, RowLabels_temp, RowLabels_indexed)) %>% #replace 0 with the nsc#
  rename(Row.Labels=RowLabels_indexed) %>% #rename the column
  data.frame() #convert into data.frame
df <- df %>% select(-RowLabels_temp) #select all bu the temp rowlabels
df

# tmp.nsclbl<-NULL #repeat each nsc respective amt of times
# nscs.lst<-NULL #each individual nsc
fx.lst<-NULL #each individual fx identifier
# 
count=0
 for (nums in as.vector(odNostr$Row.Labels)) {
   tryCatch({
     nums<-as.integer(nums)
     if (nums < 8) {count=count+1;fx.lst<-c(fx.lst,nums)}
#     if (nums>8) {tmp.nsclbl<-c(tmp.nsclbl,rep(nums,count)); count=0;nscs.lst<-c(nscs.lst,nums)}
   },
   warning = function(w) w
   )  
}

Lnscs<-unlist(lapply(fac_nsclbl_L,function(x) rep(x,7))) #repeat each L-nsc 7 times
whichAreNSC<-which(as.integer(as.vector(odNostr$Row.Labels)) > 8)
fac_nsclbl<-c(Lnscs,fac_nsclbl_N,fac_nsclbl_S)

odL<-cbind(Lnscs,od_L) #column combine the L-nsc string with the onedose-L df
odfull<-cbind(fac_nsclbl,odNostr[-whichAreNSC,])

#function to determine whether each value is < the GI cutoff and for that fx is the total > 3? If so, enter 1
filterGI<-function(GI.num){
trs<-c(rep(0,nrow(odfull)))
tmp.od<-odfull[,3:ncol(odfull)]
for (i in seq(1:nrow(tmp.od))) {
  tryCatch({
    u<-table(tmp.od[i,]<GI.num)
    if (u[['TRUE']]>3) 
      {
      trs[i] <- 1
      }
    else 
    {next}
  },
  error = function(e) {e})
}
return(trs)
}
trs.GI50<-filterGI(50) #compute whether value is <50, is it true
trs.LC50<-filterGI(0) #compute whether value is <0, is it true
trs.GI25<-filterGI(25)  #compute whether value is <25, is it true

#make new df with fx index, fx, and active fx logic
activeDF<-data.frame(nsc_lbl=fac_nsclbl,fx=fx.lst,GI50=trs.GI50,GI25=trs.GI25,LC50=trs.LC50)
aggrDF<-aggregate(.~fx,activeDF[,2:ncol(activeDF)],FUN=sum) #compute sum of each row upon aggregation of similar fx ID (1-7)
DFmelt<-melt(aggrDF,id.vars='fx') #melt the df by stacking and adding variable

longdf<- activeDF %>% group_by(nsc_lbl) %>% complete(fx = 1:7) #use dplyr and tidyr to complete the missing fx values with NA




#barplot of 7 fxs with amt that passes GI level
ggplot(DFmelt,aes(x=fx,value))+ geom_bar(stat='identity',aes(fill=factor(variable)))+
  ylab('Active fx count')+xlab('prefraction')+
  scale_x_continuous(breaks=round(seq(0,7)))+
  scale_y_continuous(limits = c(0,18))+
  geom_text(aes(label=value),vjust=-0.5)+
  facet_grid(variable~.)+
  theme(axis.ticks.y=element_blank(),axis.text.y=element_blank(),plot.title = element_text(hjust = 0.5))+
  ggtitle('Active fxs based on GI: SOM9')+
  guides(fill=guide_legend(title="GI level"))

#heatmap of active fractions
ggplot(longdf,aes(fx,factor(nsc_lbl)))+ylab('NSC')+
  geom_tile(aes(fill=factor(GI50)),colour='black')+ #colour of lines
  scale_x_continuous(breaks=round(seq(0,7)))+
  scale_fill_manual(values=c('white',brewer.pal(10,'PiYG')[8]),labels=c('Non-active','Active'),name='GI50')+
  theme(panel.background= element_rect(fill='white'),plot.title = element_text(hjust = 0.5))+
  ggtitle('Heatmap of SOM9 plant extract prefrac fractions')



activeDF_sumAllfx_onetwo<-activeDF %>% 
  group_by(nsc_lbl) %>% 
  summarise(sumact=sum(GI50)) %>% 
  filter(sumact<3 & sumact!=0) %>% 
  filter(!grepl("N",as.character(nsc_lbl))) %>% 
  filter(!grepl("S",as.character(nsc_lbl))) %>%
  data.frame()
#these are all the NSCs that have <3 active fxs to prioritise

