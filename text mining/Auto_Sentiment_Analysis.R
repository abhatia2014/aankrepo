
#To what extent do the automotive brands vary in cosumer sentiments on twitter

library(httr)
library(twitteR)
library(httpuv)
library(SnowballC)
library(tm)
library(wordcloud)
library(memoise)
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(pander)
?httpuv
load("twitter authentication.Rdata")
api_key=  "pmdVLMK7TiXtOZRSgNJeOHkxl"
api_secret= "VkCXokPgp4sJ9Dxpgs8xLq2jwA26Ns2aw0odgMYT995apNoOYy"
access_token= "2381215371-Jt8jLfmiPvMqekXrRDbgbK0z1aTJlDMDgxvpgKP"
access_token_secret= "ZV6AulHPO2WbAV0lJDigBjdmj69rMXFA6IBZgjb7ZKh6M"

t=setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#pull 1000 tweets (english only) that mention or are directed at the twitter handle of major car brands

brands=list("acura", "astonmartin", "audi", "bentleymotors", "bmw", "buick", "cadillac", 
            "chevrolet", "chrysler", "dodge", "ferrari", "fiatontheweb", "ford", "ThisIsGMC", 
            "honda", "hyundai", "infinitiUSA", "isuzu", "jaguar", "jeep", "kia", "lamborghini", 
            "landrover", "lexus", "lincolnmotorco", "maserati", "mazdausa", "mclarenauto", 
            "mercedesbenz", "mini", "mitsucars", "nissan", "porsche", "ramtrucks", "rollsroyce", "scion",
            "subaru_usa", "teslamotors", "toyota", "vw", "volvocarglobal")


getTweets=function(brand,n=200){
  TS=paste0("@",brand," OR ","#",brand)
  tweets=searchTwitter(TS,n=n,since=format(Sys.Date()-14),lang = "en")
  #strip retweets
  if(length(tweets)>0){
    tweets=strip_retweets(tweets)
    #convert to data.frame
    tweetdf=twListToDF(tweets)
    #add brand and return
    out=cbind(brand,tweetdf)
  } 
  return(out)
}

# get tweets for all brands in parallel

tweets_by_brand=lapply(brands, function(x) getTweets(x,200))

2*pt(5.365,791,lower.tail = FALSE)
