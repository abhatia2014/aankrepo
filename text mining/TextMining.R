getwd()
library(tm)
library(SnowballC)
library(qdap)
library(qdapDictionaries)
library(dplyr)
library(ggplot2)
library(scales)


# getting help on a package
library(help=qdap)
library(help=dplyr)
getSources()
#get the readers for text analysis

getReaders()


docs=Corpus(DirSource('~/Datasets/WordCloud'),readerControl=list(reader=readPDF))
docs
class(docs)
summary(docs)
inspect(docs[12])

#preparing the corpus - text manipulations

getTransformations()

#some simple transformations

#for custom transformations

mod1=content_transformer(function(x,pattern) gsub(pattern,"",x))

docs=tm_map(docs,mod1,"/")
docs=tm_map(docs,mod1,"@")
docs=tm_map(docs,mod1,"\\|")

inspect(docs[2])

# conversion of lower case

docs=tm_map(docs,content_transformer(tolower))

#remove numbers

docs=tm_map(docs,removeNumbers)

# remove punctuations

docs=tm_map(docs,removePunctuation)

#remove english stop words

docs=tm_map(docs,removeWords,stopwords("english"))

#what are stop words

length(stopwords("english"))

stopwords("english")

# remove other stopwords

docs=tm_map(docs,removeWords,c("department","email"))

#strip whitespaces

docs=tm_map(docs,stripWhitespace)

#stemming

docs=tm_map(docs,stemDocument)

#creating a Document term Matrix

dtm=DocumentTermMatrix(docs)
dtm

inspect(dtm[1:8,1000:1005])

class(dtm)
dim(dtm)

#transpose is called Term document matrix

tdm=TermDocumentMatrix(docs)

# exploring the Document Term Matrix

freq=colSums(as.matrix(dtm))
length(freq)
ord=order(freq)
freq[head(ord)]

#most frequent terms

freq[tail(ord)]

head(table(freq),20)

#convert to matrix before writing to csv

mat=as.matrix(dtm)
dim(mat)

write.csv(mat,"dtm.csv")

#removing sparse terms

dtms=removeSparseTerms(dtm,0.4)
dim(dtms)
freq=colSums(as.matrix(dtms))
freq
table(freq)

#find most frequent terms in the corpus

findFreqTerms(dtm,lowfreq=1000)

#get some more frequent terms

findFreqTerms(dtm,lowfreq=500)

#find associations with a word, specifying the correlation limit

findAssocs(dtm, "secur",corlimit=0.8)

#draw correlation plots

plot(dtm,terms=findFrequentTerms(dtm,lowfreq=1000),corThreshold=0.6)

#plotting word frequencies

freq=sort(colSums(as.matrix(dtm)),decreasing=TRUE)
head(freq,10)

#convert the matrix to a dataframe

wf=data.frame(word=names(freq),freq=freq)

#plot using ggplot

wfh=wf%>%
  filter(freq>=500)

ggplot(wfh,aes(word,freq))+geom_bar(stat="identity")+theme_bw()+
theme(axis.text.x=element_text(angle=45,hjust=1))

#making a word cloud

library(wordcloud)
wordcloud(names(freq),freq,min.freq=40)

#to reduce the clutter

wordcloud(names(freq),freq,min.freq=40,max.words=100)

#adding some color

wordcloud(names(freq),freq,min.freq=100,colors=brewer.pal(6,"Dark2"))
