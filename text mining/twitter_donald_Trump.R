library(twitteR)
library(ROAuth)
library(RCurl)
library(stringr)
library(tm)
library(ggmap)
library(plyr)
library(dplyr)
library(wordcloud)

#setup twitter authentication

api_key=  "pmdVLMK7TiXtOZRSgNJeOHkxl"
api_secret= "VkCXokPgp4sJ9Dxpgs8xLq2jwA26Ns2aw0odgMYT995apNoOYy"
access_token= "2381215371-Jt8jLfmiPvMqekXrRDbgbK0z1aTJlDMDgxvpgKP"
access_token_secret= "ZV6AulHPO2WbAV0lJDigBjdmj69rMXFA6IBZgjb7ZKh6M"

t=setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
save(t,file="twitter authentication.Rdata")

#number of tweets to request from each query

N=200
#S-radius in miles
S=200

#get latitude and longitude of cities

cities= c("DC","New York","San Fransisco","Colorado","Mountainview","Tampa","Austin","Boston",
          "Seattle","Las Vegas","Montgomery","Phoenix","Atlanta","Springfield","Madison","Salt Lake City",
          "Nashville","Raleigh","Harrisburg","Bethpage","St.Paul","Lincoln")
latlon=geocode(cities)

#get tweets about Donald Trump from each city

donald=do.call(rbind,lapply(1:length(latlon$lat), function(i) searchTwitter('Donald+Trump',
                                                                            lang = 'en',n=N,resultType = 'recent',
                                                                            geocode = paste(latlon$lat[i],latlon$lon[i],paste0(S,"mi"),sep = ","))))
donald[[20]]

#get the latitude and longitude of the tweet

donaldlat=sapply(donald,function(x) as.numeric(x$getLatitude()))
donaldlon=sapply(donald, function(x) as.numeric(x$getLongitude()))
donaldlat=sapply(donaldlat,function(z) ifelse(length(z)==0,NA,z))
donaldlon=sapply(donaldlon, function(z) ifelse(length(z)==0,NA,z))

#get the time of the tweet

donalddate=lapply(donald, function(x) x$getCreated())

#make consistent format

donalddate=sapply(donalddate,function(x) strftime(x,format="%Y-%m-%d %H:%M:%S",tz="UTC"))

donaldtext=sapply(donald, function(x) x$getText())
donaldtext=unlist(donaldtext)
donaldtext[20:30]
#check if retweeted

isretweet=sapply(donald, function(x) x$getIsRetweet())
retweeted=sapply(donald, function(x) x$getRetweeted())
retweetcount= sapply(donald,function(x) x$getRetweetCount())
favoritecount=sapply(donald,function(x) x$getFavoriteCount())
favorited=sapply(donald, function(x) x$getFavorited())

#put all of this information in a data frame

donalddf=as.data.frame(cbind(tweet=donaldtext,date=donalddate,lat=donaldlat,lon=donaldlon,
                             isretweet=isretweet,retweeted=retweeted,retweetcount=retweetcount,
                             favoritecount=favoritecount,favorited=favorited))

#lets first create a word cloud of the tweets


#convert to lower case

alltweets=donalddf$tweet

#remove punctuations

alltweets=lapply(alltweets, function(x) gsub("[[:punct:]]"," ",x))

#remove control characters

alltweets=lapply(alltweets, function(x) gsub("[[:cntrl:]]"," ",x))

#remove digits

alltweets=lapply(alltweets, function(x) gsub("\\d+"," ",x))

alltweets=lapply(alltweets,function(x) gsub("[^[:graph:]]"," ",x))

#alltweets=lapply(alltweets,function(x) gsub("[[^:alpha:]]"," ",x))
alltweets=lapply(alltweets,function(x) gsub("https"," ",x))
alltweets=lapply(alltweets,function(x) gsub("donald"," ",x))
alltweets=lapply(alltweets,function(x) gsub("trump"," ",x))
alltweets=lapply(alltweets,function(x) gsub("Donald"," ",x))
alltweets=lapply(alltweets,function(x) gsub("Trump"," ",x))

#put it back in the dataframe
donalddf$tweet=alltweets

#create corpus

corpus=Corpus(VectorSource(donalddf$tweet))

#Remove stop words

corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

#convert corpus to a plain text document

corpus=tm_map(corpus,PlainTextDocument)
library(wordcloud)
col=brewer.pal(6,"Dark2")
wordcloud(corpus,min.freq=20,random.color=T,max.word=200, random.order=T,colors=col,use.r.layout = TRUE)

