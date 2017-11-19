library(dplyr)
inpath<-'/Users/Downloads/Exp_1710OS92_1.csv'
onedose.tmp<-read.csv(inpath,header = T,sep = ',',skip=2) #skip 2 extraneous rows
onedose<-onedose.tmp[ , apply(onedose.tmp, 2, function(x) length(is.na(x))>10)] #remove column if there are more than 10 NA's

#grep X column string names to remove from df
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

od_clean<-onedose %>%
  select(-one_of(tmp.odc)) %>% #one_of(): variables in character vector; remove columns with X-value
  filter(grepl("^\\d+$",Row.Labels)) %>% #filter out values that are strings (Letter prefixes), only maintain digits
  mutate(RowLabels_temp = ifelse(grepl("^\\d{3,}$",Row.Labels), as.numeric(as.character(Row.Labels)), NA)) %>% #add new column; if amt of SF is > 3 than coerce into number, otherwise coerce as NA
  zoo::na.locf() %>% #replace NA with the non-NA value prior to it; repeat the NSC# 6 times
  select(-Row.Labels) %>% #inverse select (select all but) the row.labels (first column)
  distinct() %>% #similar to unique.data.frame(); retain unique/distinct rows from input table
  group_by(RowLabels_temp) %>% #ensure that the rows are grouped by new Rowlabel_temp (wil move the N prefix nsc with its corresponding group (fx:1-7))
  mutate(RowLabels_indexed = row_number()-1) %>% #new column with each row number subtract 1 -> fx 0 -8
  mutate(RowLabels_indexed = ifelse(RowLabels_indexed==0, NA, RowLabels_indexed)) %>% #replace 8 fx as 0 fx (crude)
  mutate(RowLabels_indexed=replace(RowLabels_indexed,RowLabels_indexed==8,0))%>% #replace fx 8 as 0 (for crude)
  na.omit() %>% #remove rows with NSC values (which were coerced into NAs)
  rename(Row.Labels=RowLabels_indexed) %>% #rename the column
  filter(!grepl("^\\d{3,}$",Row.Labels))%>%
  arrange(RowLabels_temp, Row.Labels) %>% #arrange in order of increasing rowlabel_temp and rowlabels_index
  data.frame() #convert into data.frame

#test<-as.data.frame(sapply(od_clean[,-((ncol(od_clean)-1):ncol(od_clean))],as.numeric) )

od_GI<-od_clean %>% 
  select(-RowLabels_temp)%>% 
  select(-Row.Labels) %>%
  mutate_all(funs(as.numeric))%>%
  mutate(GI50=rowSums(.<50))%>% mutate(GI50=ifelse(GI50>3,1,0))

activeDF=data.frame(nsc_lbl=od_clean$RowLabels_temp,fx=od_clean$Row.Labels,GI50=od_GI$GI50)

colorset<-c('#FFC0CB', '#7F7F7F', '#008000', '#FF0000', '#FA8072', '#0000FF', '#800080', '#40F4D0', '#DAA520')
f <- tidyr::gather(od_clean, key = cell_line, value = GI, -Row.Labels, -RowLabels_temp) %>% #collapses into key-value pairs (& name them cell_line and GI) but do not process rowlabels_emp and row.labels [similar to melt]
  mutate(GI = as.numeric(GI), 
         type = case_when( #allow to vectorise multiple if and else statements
           cell_line == "SF.539" ~ "Breast Cancer", 
           cell_line == "BT.549" ~ "Breast Cancer", 
           cell_line == "HS.578T" ~ "Breast Cancer",          
           cell_line == "MCF7" ~ "Breast Cancer",                        
           cell_line == "MDA.MB.231.ATCC" ~ "Breast Cancer",
           cell_line == "T.47D" ~ "Breast Cancer",
           cell_line == "MDA.MB.468" ~ "Breast Cancer",
           cell_line == "SF.268" ~ "CNS Cancer", 
           cell_line == "SF.295" ~ "CNS Cancer",
           cell_line == "SF.539" ~ "CNS Cancer",
           cell_line == "SNB.19" ~ "CNS Cancer",
           cell_line == "SNB.75" ~ "CNS Cancer",
           cell_line == "U251" ~ "CNS Cancer", 
           cell_line == "COLO.205" ~ "Colon Cancer", 
           cell_line == "HCC.2998" ~ "Colon Cancer", 
           cell_line == "HCT.116" ~ "Colon Cancer", 
           cell_line == "HCT.15" ~ "Colon Cancer", 
           cell_line == "HT29" ~ "Colon Cancer", 
           cell_line == "KM12" ~ "Colon Cancer", 
           cell_line == "SW.620" ~ "Colon Cancer", 
           cell_line == "CCRF.CEM" ~ "Leukemia Cancer", 
           cell_line == "HL.60.TB." ~ "Leukemia Cancer", 
           cell_line == "K.562" ~ "Leukemia Cancer", 
           cell_line == "MOLT.4" ~ "Leukemia Cancer", 
           cell_line == "RPMI.8226" ~ "Leukemia Cancer", 
           cell_line == "SR" ~ "Leukemia Cancer", 
           cell_line == "LOX.IMVI" ~ "Melanoma Cancer", 
           cell_line == "M14" ~ "Melanoma Cancer", 
           cell_line == "MALME.3M" ~ "Melanoma Cancer", 
           cell_line == "MDA.MB.435" ~ "Melanoma Cancer", 
           cell_line == "SK.MEL.28" ~ "Melanoma Cancer", 
           cell_line == "SK.MEL.2" ~ "Melanoma Cancer",
           cell_line == "SK.MEL.5" ~ "Melanoma Cancer",
           cell_line == "UACC.257" ~ "Melanoma Cancer",
           cell_line == "UACC.62" ~ "Melanoma Cancer",
           cell_line == "A549.ATCC" ~ "Lung Cancer",
           cell_line == "EKVX" ~ "Lung Cancer",
           cell_line == "NCI.H23" ~ "Lung Cancer",
           cell_line == "NCI.H322M" ~ "Lung Cancer",
           cell_line == "NCI.H460" ~ "Lung Cancer",
           cell_line == "NCI.H522" ~ "Lung Cancer",
           cell_line == "NCI.H226" ~ "Lung Cancer",
           cell_line == "HOP.62" ~ "Lung Cancer",
           cell_line == "HOP.92" ~ "Lung Cancer",
           cell_line == "IGROV1" ~ "Ovarian Cancer",
           cell_line == "NCI.ADR.RES" ~ "Ovarian Cancer",
           cell_line == "OVCAR.3" ~ "Ovarian Cancer",
           cell_line == "OVCAR.4" ~ "Ovarian Cancer",
           cell_line == "OVCAR.5" ~ "Ovarian Cancer",
           cell_line == "OVCAR.8" ~ "Ovarian Cancer",
           cell_line == "SK.OV.3" ~ "Ovarian Cancer",
           cell_line == "DU.145" ~ "Prostate Cancer",
           cell_line == "PC.3" ~ "Prostate Cancer",
           cell_line == "X786.0" ~ "Renal Cancer",
           cell_line == "A498" ~ "Renal Cancer",
           cell_line == "ACHN" ~ "Renal Cancer",
           cell_line == "CAKI.1" ~ "Renal Cancer",
           cell_line == "RXF.393" ~ "Renal Cancer",
           cell_line == "SN12C" ~ "Renal Cancer",
           cell_line == "TK.10" ~ "Renal Cancer",
           cell_line == "UO.31" ~ "Renal Cancer"
           ) )
ggplot(f, aes(x = Row.Labels, y = GI, group = cell_line, color = type)) + 
  geom_line() + 
  facet_wrap('RowLabels_temp') + 
  scale_color_manual(values = colorset) +
  coord_cartesian(ylim = c(-100, 150)) +
  scale_x_continuous(breaks = 0:7)+
  ylab('% GI') + xlab('Fractions')+
  guides(color=guide_legend(title="Cell Line Type"))



ggplot(activeDF,aes(fx,factor(nsc_lbl)))+ylab('NSC')+
  geom_tile(aes(fill=factor(GI50)),colour='black')+ #colour of lines
  scale_x_continuous(breaks=0:7)+
  scale_fill_manual(values=c('white',brewer.pal(10,'PiYG')[8]),labels=c('Non-active','Active'),name='GI50')+
  theme(panel.background= element_rect(fill='white'),plot.title = element_text(hjust = 0.5))+
  ggtitle('Heatmap of SOM9 plant extract prefrac fractions')


