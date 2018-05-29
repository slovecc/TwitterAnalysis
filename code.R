
library(twitteR)
library(base64enc)
library(httpuv)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Change the next four lines based on your own consumer_key, consume_secret, access_token, and access_secret. 
consumer_key <- 
consumer_secret <- 
access_token <- 
access_secret <- 

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
tw = suppressWarnings(twitteR::searchTwitter('#Berlusconi', n = 500, 
                            since = '2017-11-08', retryOnRateLimit = 1e3))

d = twitteR::twListToDF(tw)

class(tw)
str(tw)
tw[1:10]


############## Data prepr ############
#get text from the tweets#
match_text = sapply(tw, function(x) x$getText())
match_text[1]

##create a corpus
myCorpus= Corpus(VectorSource(match_text))
myCorpus
inspect(myCorpus[1])


##remove emojis
myCorpus =tm_map(myCorpus, content_transformer(gsub),pattern="\\W", replace=" ")

#remove URL
removeURL <-function(x) gsub("http[^[:space:]]*", "",x)
myCorpus <-tm_map(myCorpus, content_transformer(removeURL))

#to lower
myCorpus <-tm_map(myCorpus,content_transformer(tolower))

#remove anything other than English letter or space
removeNumPunt <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunt))

#remove punctuation
myCorpus=tm_map(myCorpus,removePunctuation)
#remove numbers
myCorpus=tm_map(myCorpus,removeNumbers)
#remove stopwors
myCorpus=tm_map(myCorpus,removeWords, stopwords("italian"))
#strip whithspace
myCorpus=tm_map(myCorpus,stripWhitespace)

dtm <-TermDocumentMatrix(myCorpus)
m<-as.matrix(dtm)
v<-sort(rowSums(m),decreasing = T)
d<-data.frame(word=names(v),freq=v)
head(d,10)
wordcloud(words = d$word, freq=d$freq, min.freq = 1, max.words = 100,random.color = F, 
          colors = brewer.pal(8,"Dark2"))
