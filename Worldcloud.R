install.packages('tm')
library(devtools)
install_url("https://cran.r-project.org/src/contrib/Archive/slam/slam_0.1-37.tar.gz")
library(tm)
text<-readLines('c:/users/trinh/desktop/Trinh, Spencer - Literature Survey.txt',skipNul = T)
text<-paste(text,collapse=' ')
text<-gsub('[0-9]+', '', text)
text<-gsub('[!#@$%^&*()_+]+','',text)
text
vctr<-VectorSource(text)
corpus<-Corpus(vctr)
corpus

#cleaning
corpus<-tm_map(corpus,content_transformer(tolower))
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,stripWhitespace)
corpus<-tm_map(corpus,removeWords,stopwords('english')) #remove common words, not important
corpus<-tm_map(corpus,removeWords,c('can','used','also','thus','borrowed','will','due','based','shows','well','new','seen','bche','etc','one','whereas','via','within','allow','allows','include','less','luv','mlv','moreover','two','common','commonly'))

#make document-term matrix
dtm<-DocumentTermMatrix(corpus)
dtm2<-as.matrix(dtm)

#finding the most frequent terms
frequency<-colSums(dtm2)
frequency<-sort(frequency,decreasing = T)
wrds <- data.frame(word = names(frequency),freq=frequency)
head(wrds, 100)

install.packages('wordcloud')
library(wordcloud)
library(RColorBrewer)
wordcloud(wrds$word,wrds$freq,min.freq = 10, colors = brewer.pal(8,'Set2'))
box(which='outer')
