#' ------------------------------------------------------------------------------------------
#' NEW CODE
#' -------------------------------------------------------------------------------------------

rm(list=ls())
setwd("C:/Users/M/Documents/Data Incubator")

#' uncomment the command below to install the necessary packages
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


#' streamR and twitteR are used to retreive tweets from the Twitter API.
#' to access the functions on these packages I complete 2 authentication processes.
#' these packages allow application access to twitter streaming API (real time collection and monitoring of tweets)
#' and  twitter API (read and post tweets with API responses)

#' Twitter credentials for streamR and twitteR authentication
consumer_key <- "BtE5igJuS1fJwvViFW7RfXpW5"
consumer_secret<- "4MYSl9CcWDeLgDOzkZsdf5s0IVPyC2gZd52hMxQncv0Z0zQ8EH"
access_token<- "32901162-ZDJomqMT3jllwFhB4IJmiOamtrpZSDPLcpTS9CC2O"
access_token_secret<- "xGExJtaE7IxC5L6G5ZewH99XFafVP6I72bMeirp7pTMPM"

#' parameters and URLs for streamR authentication
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL<- "https://api.twitter.com/oauth/access_token"
authURL<- "https://api.twitter.com/oauth/authorize"

#' Note:  You only need to create an authentication object for streamR once

#' create an object "cred" that will save the authenticated object for later sessions
twitCred<- OAuthFactory$new(consumerKey=consumer_key,consumerSecret=consumer_secret,
                            requestURL=reqURL,accessURL=accessURL,authURL=authURL)

#' Microsoft Windows users need to uncomment the following command to download a cert file
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")


#' insert the number in the R console after you run this line
#' save authenticated credentials for later sessions
#' for later use, uncomment the following command in a folder that contains twitCred.RData
#load(twitCred)
twitCred$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(twitCred, file = "twitCred.RData")

#' setup direct twitter authentication for twitteR functions
#' you will be prompted to cache authentication token
#' you will need to repeat this step unless you are running analysis from a location with a
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
  
  # we got a vector of sentences. plyr will handle a list
  
  # or a vector as an "l" for us
  
  # we want a simple array ("a") of scores back, so we use
  
  # "l" + "a" + "ply" = "laply":
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    
    sentence = gsub('[[:punct:]]', '', sentence)
    
    sentence = gsub('[[:cntrl:]]', '', sentence)
    
    sentence = gsub('\\d+', '', sentence)
    
    # and convert to lower case:
    
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    
    word.list = str_split(sentence, '\\s+')
    
    # sometimes a list() is one level of hierarchy too much
    
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    
    pos.matches = match(words, pos.words)
    
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    
    # we just want a TRUE/FALSE:
    
    pos.matches = !is.na(pos.matches)
    
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    
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

#' This analysis simply calculating how many positive and negative words in each tweet 
#' and calculates the sentiment score by obtaining difference of +positives and -negatives
#' The positive values stand for positive tweets and the negative values for negative tweets. 
#' The mean tells you about the overall mood of your sample.