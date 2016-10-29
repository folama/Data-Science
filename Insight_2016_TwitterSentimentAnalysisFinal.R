#' ------------------------------------------------------------------------------------------
#' NEW CODE
#' -------------------------------------------------------------------------------------------

rm(list=ls())
setwd("C:/Users/M/Documents/Data Incubator")

install.packages(c("twitteR","streamR","ROAuth", "RCurl", "dplyr","base64enc"))
library(twitteR)
library(streamR)
library(ROAuth)
library(RCurl)
library(dplyr)
library(base64enc)

require(twitteR)
require(RCurl)
require(ROAuth)
require(streamR)
require(dplyr)
require(base64enc)

#' Twitter credentials for streamR and twitteR authentication
consumer_key <- "xxxx"
consumer_secret<- "xxxx"
access_token<- "xxxx"
access_token_secret<- "xxxx"

#' parameters and URLs for streamR authentication
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL<- "https://api.twitter.com/oauth/access_token"
authURL<- "https://api.twitter.com/oauth/authorize"

#' create an object "cred" that will save the authenticated object for later sessions
twitCred<- OAuthFactory$new(consumerKey=consumer_key,consumerSecret=consumer_secret,
                            requestURL=reqURL,accessURL=accessURL,authURL=authURL)

download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")

twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(twitCred, file = "twitCred.RData")

#' cached authentication token 
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)

#' GET TWEETS
tweets = searchTwitter("#gmo", n=500000)
length(tweets)


##' Analyze
library(plyr)
library(dplyr)
Tweets.text = laply(tweets,function(t)t$getText())

#' get lists with positive and negative words
pos = scan('C:/Users/M/Documents/Data Incubator/twitter-sentiment-analysis-master/wordbanks/positive-words.txt', what='character', comment.char=';')

neg = scan('C:/Users/M/Documents/Data Incubator/twitter-sentiment-analysis-master/wordbanks/negative-words.txt', what='character', comment.char=';')

#' analyze the words
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  
{
  
  require(plyr)
  
  require(stringr)
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with gsub():
    
    sentence = gsub('[[:punct:]]', '', sentence)
    
    sentence = gsub('[[:cntrl:]]', '', sentence)
    
    sentence = gsub('\\d+', '', sentence)
    
    #  convert to lower case:
    
    sentence = tolower(sentence)
    
    # split into words.
    
    word.list = str_split(sentence, '\\s+')
        
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    pos.matches = match(words, pos.words)
    
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    
    # need only TRUE/FALSE:
    
    pos.matches = !is.na(pos.matches)
    
    neg.matches = !is.na(neg.matches)
    
    # TRUE/FALSE is treated as 1/0 by sum():
    
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
    
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  
  return(scores.df)
  
}

#' save results
analysis = score.sentiment(Tweets.text, pos, neg)

#' get a table
table(analysis$score)

#' historgarm of score
hist(analysis$score)

#' mean of score
mean(analysis$score)

#' This analysis is simply calculating how many positive and negative words in each tweet 
#' and calculates the sentiment score by obtaining difference of +positives and -negatives
#' The positive values stand for positive tweets and the negative values for negative tweets. 
#' The mean tells you about the overall mood of my sample.
 
