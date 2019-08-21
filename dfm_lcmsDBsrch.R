library(dplyr)
library(ggplot2)
N133307_5_7<-'C:/users/trinh/downloads/R/N133307_5_7.csv'
N133307_6_12<-'C:/users/trinh/downloads/R/N133307_6_12.csv'
N133307_6_13<-'C:/users/trinh/downloads/R/N133307_6_13.csv'
N133307_6_14<-'C:/users/trinh/downloads/R/N133307_6_14.csv'
N133307_6_15<-'C:/users/trinh/downloads/R/N133307_6_15.csv'
list_nscs<-list(N133307_5_7,N133307_6_12,N133307_6_13,N133307_6_14,N133307_6_15)

parseF<-function(file){
  df<-read.csv(file,header=F,stringsAsFactors = F) #read csv file
  colnames(df)<-df[2,] #change col names to second row
  df <- df%>%
    mutate(`Score (DB)` = as.numeric(df$`Score (DB)`) ) %>%
    filter(df$`Score (DB)` >1) %>% 
    slice(2:nrow(df)) %>%
    select(`Sample Name`,`Compound ID`,Name,Cpd,Hits,`Formula (DB)`,`Score (DB)`,`Mass (DB)`,`Avg Mass`,`Base Peak`,Formula,Height,`ID Source`,`ID Techniques Applied`,Polarity,`m/z`,RT,Position,`Acq Method`,`DA Method`) %>%
    as.data.frame()
  return(df)
}

listOfDataFrames<-lapply(list_nscs,function(x) parseF(x)) #each df as a list
dfm <- do.call("rbind", listOfDataFrames) #combine all dfs

unin<-unique(dfm$`Sample Name`) #obtain unique fx names (5)
fx <- NULL
for (i in unin) {
  fx <-c(fx,rep(i,length(unique(dfm$Name)))) #repeat the fx names 17 times (# of unique compounds found)
}

listOfScores<-lapply(listOfDataFrames, '[[', 7) #7th column = Score (DB)
listOfCmpNames<-lapply(listOfDataFrames,'[[',3) #3rd column = cmpds found in DB search for each sample

nameOfCmpds<-sort(unique(dfm$Name)) #alphabetize the cmpd names
dftmp<-data.frame(Cmpd_Name=rep(nameOfCmpds,length(unique(dfm$`Sample Name`)))) #make a starting df with just names and fx labels
dftmp<-dftmp %>% mutate(Sample_Name = fx) %>% 
  mutate(cnt_1=ifelse(Sample_Name == unin[1] & Cmpd_Name %in% listOfCmpNames[[1]],1,0)) %>%
  mutate(cnt_2=ifelse(Sample_Name == unin[2] & Cmpd_Name %in% listOfCmpNames[[2]],1,0)) %>%
  mutate(cnt_3=ifelse(Sample_Name == unin[3] & Cmpd_Name %in% listOfCmpNames[[3]],1,0)) %>%
  mutate(cnt_4=ifelse(Sample_Name == unin[4] & Cmpd_Name %in% listOfCmpNames[[4]],1,0)) %>%
  mutate(cnt_5=ifelse(Sample_Name == unin[5] & Cmpd_Name %in% listOfCmpNames[[5]],1,0)) %>%
  mutate(sum = rowSums(.[3:7])) %>% #sum across rows from cnt_1 to cnt_5
  select(Cmpd_Name,Sample_Name,sum)

dftmp %>% mutate(test=ifelse(sum == 1,dfm$`Score (DB)`[which(Sample_Name %in% dfm$`Sample Name` & Cmpd_Name %in% dfm$Name)],NA))

dftmp %>% mutate(test=ifelse(sum == 1,dfm$`Score (DB)`[grepl(Sample_Name,dfm$`Sample Name`) & grepl(Cmpd_Name,dfm$Name)],NA))

p<-ggplot(dftmp,aes(Name,factor(fx)))+
  geom_tile(aes(fill=factor(sum)),colour='black')+ #colour of lines
  coord_flip()+
  scale_fill_manual(values=c('white',brewer.pal(10,'PiYG')[8]),labels=c('Not present','present'),name='Cmpd of diospyrin genus')+
  theme(panel.background= element_rect(fill='white'),plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle=45,hjust=1))+
  ggtitle('Heatmap of L133307_UTHSCY3 2nd stage active fxs')
