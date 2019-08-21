inpath<-'c:/Users/trinh/Downloads/Exp_1710OS92_1.csv'
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
  na.locf() %>% #replace NA with the non-NA value prior to it; repeat the NSC# 6 times
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

test<-as.data.frame(sapply(od_clean[,-((ncol(od_clean)-1):ncol(od_clean))],as.numeric) )

od_GI<-od_clean %>% 
  select(-RowLabels_temp)%>% 
  select(-Row.Labels) %>%
  mutate_all(funs(as.numeric))%>%
  mutate(GI50=rowSums(.<50))%>% mutate(GI50=replace(GI50,GI50>3,1))

activeDF=data.frame(nsc_lbl=od_clean$RowLabels_temp,fx=od_clean$Row.Labels,GI50=od_GI$GI50)
