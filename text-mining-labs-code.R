library(tm) 
help(package=tm)
options(java.home="usr/bin/java")
if(!require("rJava")){
  install.packages("rJava", repos="http://cran.rstudio.com/")
  library(rJava)
}

libs <- c("NLP","magrittr","openNLP")
lapply(libs,require,character.only=TRUE)

if(!require("openNLPmodels.en")){
  install.packages("openNLPmodels.en",
                 repos = "http://datacube.wu.ac.at/",
                 type = "source")
  library(openNLPmodels.en)
}

source("http://bioconductor.org/biocLite.R")
biocLite("RBGL", suppressUpdates=TRUE)
setRepositories()
install.packages("gRain", dependencies=TRUE)
library(gRain)
biocLite("Rgraphviz", suppressUpdates=TRUE)
library(Rgraphviz)
if(!require(klaR)){
  install.packages("klaR", repos="http://cran.rstudio.com/")
  library(klaR)
}

libs <- c("SnowballC","RColorBrewer","ggplot2","lattice","wordcloud")
lapply(libs,require,character.only=TRUE)
library(Rgraphviz)

libs <- c("rpart","e1071","cluster","gmodels")
lapply(libs,require,character.only=TRUE)

library(lsa)
library(topicmodels)

require(XML)
require(dplyr)
require(rvest)

##Reading text data
data(acq)
summary(acq)

path = file.path(getwd(), "text-data", "text-tm-2")
length(dir(path))
dir(path)
corpus <- Corpus(DirSource(path))
inspect(corpus)
summary(corpus)

path <- file.path(getwd(), "text-data", "SMSSpamCollection.txt")
smsSpamCollection <- readLines(path,warn=FALSE)
#as a parameter you can also use url link
summary(smsSpamCollection)
corpus <- Corpus(VectorSource(smsSpamCollection)) 
inspect(corpus)
summary(corpus)

policy.HTML.page<-readLines("http://policy.unt.edu/policy/06-003")
corpus<-Corpus(VectorSource(policy.HTML.page))
inspect(corpus)
summary(corpus)

### Reading data from Twitter
library(twitteR)

##Web scrapping
url='http://www.r-exercises.com/start-here-to-learn-r/'
topics=read_html(url)%>%html_nodes('a')%>%html_text()
topics[23:113]

url='http://www2.sas.com/proceedings/sugi30/toc.html'
names=read_html(url)%>%html_nodes('cite')%>%html_text()
names

library(XML)
#-----------------------------------------------------------------------
# Create a function that will take a year and return a dataframe
#-----------------------------------------------------------------------
GrabSkaters <- function(S) {
  # The function takes parameter S which is a string and represents the Season
  # Returns: data frame
  ## create the URL
  URL <- paste("http://www.hockey-reference.com/leagues/NHL_", 
               S, "_skaters.html", sep="")
  
  ## grab the page -- the table is parsed nicely
  tables <- readHTMLTable(URL)
  ds.skaters <- tables$stats
  
  ## I don't like dealing with factors if I don't have to
  ## and I prefer lower case
  for(i in 1:ncol(ds.skaters)) {
    ds.skaters[,i] <- as.character(ds.skaters[,i])
    names(ds.skaters) <- tolower(colnames(ds.skaters))
    }
  
  ## finally fix the columns - NAs forced by coercion warnings
  for(i in c(1, 3, 6:18)) {
    ds.skaters[,i] <- as.numeric(ds.skaters[, i])
    }
  
  ## add the year
  ds.skaters$season <- S
  ## return the dataframe
  return(ds.skaters)
  }

#----------------------------------------------------------------------- > 
# Use the function to loop over the seasons and piece together
#-----------------------------------------------------------------------
## define the seasons -- 2005 dataset doesnt exist
## if I was a good coder I would trap the error, but this works

SEASON <- as.character(c(1960:2004, 2006:2011))

## create an empy dataset that we will append to
dataset <- data.frame()
## loop over the seasons, use the function to grab the data
## and build the dataset

for (S in SEASON) {
  temp <- GrabSkaters(S)
  dataset <- rbind(dataset, temp)
  print(paste("Completed Season ", S, sep=""))
  ## pause the script so we don't kill their servers
  Sys.sleep(3)
  }

dataset

##Cleaning & normalizing text data

policy.HTML.page<-readLines("https://en.wikipedia.org/wiki/Text_mining")
#policy.HTML.page[44:106]
cleanHtmlTags <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
  }
text.d<- cleanHtmlTags(policy.HTML.page)

#Load corpus
corpus <- Corpus(VectorSource(smsSpamCollection)) 

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords,  c(c("spam", "ham"),stopwords("english")))
corpus <- tm_map(corpus, stemDocument)

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords,  c(c("spam", "ham"),stopwords("english")))
inspect(corpus)

library(SnowballC)
corpus<-tm_map(corpus, stemDocument)
inspect(corpus)

corpus<-tm_map(corpus, stripWhitespace)
inspect(corpus)

##Processing Term Document Matrix

tdm<-TermDocumentMatrix(corpus)
inspect(tdm[1:20,])
as.table(tdm[200:210,1000:1010])

sort(rowSums(as.matrix(tdm)), decreasing=TRUE)

tdm<-TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
inspect(tdm[1:20,])

findFreqTerms(tdm, lowfreq=2, highfreq=12)

findAssocs(tdm,term="cooki", corlimit=0.76)
removeSparseTerms(tdm, sparse=0.6)
tdm <- removeSparseTerms(tdm, sparse=0.05)

###Document Term Matrix
dtm <- DocumentTermMatrix(corpus)
as.table(dtm[100:110,1000:1010])
freq <- colSums(as.matrix(dtm))
ord <- order(freq)

## the most frequent terms
t(as.table(freq[tail(ord,10)]))

## the rarest terms
t(as.table(freq[head(ord,10)]))

## terms that occure at least 300 times
findFreqTerms(dtm, lowfreq=300)
barplot(head(table(freq), 150))

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wf <- data.frame(word=names(freq),freq=freq)

d<-ggplot(aes(word,freq,group=1),data=wf[freq>200,]) 
d<-d+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=45, hjust=1))
d

head(findAssocs(dtm, "come", corlimit=0.1),5)
plot(dtm,terms=findFreqTerms(dtm, lowfreq=100)[1:30], corThreshold=0.05)

##Sentiment Analysis
hu.liu.pos = scan(file.path(getwd(), "text-data", "opinion-lexicon-English","positive-words.txt"),
                  what='character', comment.char=';')
hu.liu.neg = scan(file.path(getwd(), "text-data", "opinion-lexicon-English","negative-words.txt"),
                  what='character', comment.char=';')
pos.words = c(hu.liu.pos, 'upgrade')
neg.words = c(hu.liu.neg, 'wtf', 'wait', 'waiting',
              'epicfail', 'mechanical')
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
  {
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array of scores back, so we use
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

sample = c("You're awesome and I love you",
           "I hate and hate and hate. So angry. Die!",
           "Impressed and amazed: you are peerless in your achievement of unparalleled
           mediocrity.")
result = score.sentiment(sample, pos.words, neg.words)
result

##Visualisation of text data

###Word clouds
library(wordcloud)
set.seed(123)
wordcloud(names(freq),freq,min.freq=100, colors=brewer.pal(6, "Dark2"))

tdm<-TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
# calculate the frequency of words
v <- sort(rowSums(m), decreasing=TRUE)
head(v,15)
words <- names(v)
d <- data.frame(word=words, freq=v)
wordcloud(d$word, d$freq,min.freq=80)

### Example to sum up the lab & more visualisations
#This example is based on books "Mastering machine learning with R".

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
sou2010 = paste(scan(url("http://textuploader.com/a5vq4/raw"), what="character"),collapse=" ")
sou2010=iconv(sou2010, "latin1", "ASCII", "")
sou2010

sou2011 = paste(scan(url("http://textuploader.com/a5vm0/raw"), what="character"),collapse=" ")
sou2011=iconv(sou2011, "latin1", "ASCII", "")

sou2012 = paste(scan(url("http://textuploader.com/a5vmp/raw"), what="character"),collapse=" ")
sou2012=iconv(sou2012, "latin1", "ASCII", "")

sou2013 = paste(scan(url("http://textuploader.com/a5vh0/raw"), what="character"),collapse=" ")
sou2013=iconv(sou2013, "latin1", "ASCII", "")

sou2014 = paste(scan(url("http://textuploader.com/a5vhp/raw"), what="character"),collapse=" ")
sou2014=iconv(sou2014, "latin1", "ASCII", "")

sou2015 = paste(scan(url("http://textuploader.com/a5vhb/raw"), what="character"),collapse=" ")
sou2015=iconv(sou2015, "latin1", "ASCII", "")

getwd()
write.table(sou2010, file.path(getwd(), "text-data", "text-tm-2","sou2010.txt"))
write.table(sou2011, file.path(getwd(), "text-data", "text-tm-2","sou2011.txt"))
write.table(sou2012, file.path(getwd(), "text-data", "text-tm-2","sou2012.txt"))
write.table(sou2013, file.path(getwd(), "text-data", "text-tm-2","sou2013.txt"))
write.table(sou2014, file.path(getwd(), "text-data", "text-tm-2","sou2014.txt"))
write.table(sou2015, file.path(getwd(), "text-data", "text-tm-2","sou2015.txt"))

name = file.path(getwd(), "text-data", "text-tm-2")
length(dir(name))
dir(name)
docs = Corpus(DirSource(name)) 
docs

docs = tm_map(docs, tolower)
docs = tm_map(docs, removeNumbers)
docs = tm_map(docs, removePunctuation)
docs = tm_map(docs, removeWords, stopwords("english"))
docs = tm_map(docs, stripWhitespace)
docs = tm_map(docs, stemDocument)
docs = tm_map(docs, removeWords, c("applaus","can","cant","will","that","weve",
                                   "dont","wont")) 
#docs <- tm_map(docs, PlainTextDocument)
tdm <- TermDocumentMatrix(docs)
dtm <- DocumentTermMatrix(docs)
dim(dtm)
dtm = removeSparseTerms(dtm, 0.51)
dim(dtm)
rownames(dtm) = c("2010","2011","2012","2013","2014","2015")
inspect(dtm[1:6, 1:5])
freq = colSums(as.matrix(dtm))
ord = order(-freq) #order the frequency
freq[head(ord)]
freq[tail(ord)]
head(table(freq))
tail(table(freq))
findFreqTerms(dtm, 100)
findAssocs(dtm, "busi", corlimit=0.9)
findAssocs(dtm, "job", corlimit=0.9)

wordcloud(names(freq), freq, min.freq=50,scale=c(3, .5), colors=brewer.pal(6, "Dark2"))
wordcloud(names(freq), freq, max.words=30)

freq = sort(colSums(as.matrix(dtm)), decreasing=TRUE)
wf = data.frame(word=names(freq), freq=freq)
wf = wf[1:10,]
barplot(wf$freq, names=wf$word, main="Word Frequency",
        xlab="Words", ylab="Counts", ylim=c(0,250))

#More advanced library to analyze and visualize texts
library(qdap)
state15 = gsub("\\(Applause.\\)", "", sou2015)
speech15 = data.frame(speech=state15)
sent15 = sentSplit(speech15, "speech")
sent15$year = "2015"
state10 = gsub("\\(Applause.\\)", "", sou2010)
speech10 = data.frame(speech=state10)
sent10 = sentSplit(speech10, "speech")
sent10$year = "2010"
sentences = rbind(sent10, sent15)

pol = polarity(text.var=sentences$speech,grouping.var=sentences$year)
pol

#Try to do some plots and visualize the length of speeches, their polarities etc.

pol.df = pol$all
which.min(pol.df$polarity)
pol.df$text.var[12]

#readability index
ari = with(sentences,automated_readability_index(text.var=speech, grouping.var=year))
ari

#formality
form = formality(sentences$speech, sentences$year)
form
form$form.prop.by
plot(form)

#diversity
div = diversity(sentences$speech, sentences$year)
div

dispersion_plot(sentences$speech, 
                grouping.var=sentences$year,
                c("economy","jobs","families"),
                color="black", bg.color="white")

#finding frequent words
freq2010 = freq_terms(sent10$speech, top=10, stopwords=Top200Words)
freq2010
freq2015 = freq_terms(sent15$speech, top=50, stopwords=Top200Words)
freq2015

#Lab 2

##Text similarity measures
library(lsa)

x <- c(10.4, 5.6, 3.1, 6.4, 21.7)
y <- c(-10.4, -5.6, -3.1, -6.4, -21.7)
cosine(x,y)

##Text clustering

# convert the sparse term-document matrix to a standard data frame
mydata.df <- as.data.frame(inspect(tdm))
mydata.df.scale <- scale(mydata.df)
d <- dist(mydata.df.scale, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D")
plot(fit) # display dendogram?

groups <- cutree(fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=3, border="red")

#k-means
data(acq)
data(crude)
ws <- c(acq, crude)
summary(ws)
inspect(ws)

tdm <- TermDocumentMatrix(ws, control = list(stopwords = TRUE))
wsKMeans <- kmeans(tdm, 2)
wsKMeans

##Topic modeling

myLSAspace<-lsa(tdm,3)
# display it as a textmatrix again
myNewMatrix = as.textmatrix(myLSAspace)
myNewMatrix # should look be different!
# compare two terms with the cosine measure
cosine(myNewMatrix["applause",], myNewMatrix["attack",])
# compare two documents with pearson
cor(myNewMatrix[,1], myNewMatrix[,2], method="pearson")

#topic models
library(topicmodels)
set.seed(123)
lda3 = LDA(dtm, k=3, method="Gibbs")
topics(lda3)
set.seed(456)
lda4 = LDA(dtm, k=4, method="Gibbs")
topics(lda4)
terms(lda3,20)

##POS tagging
library("openNLP")

## Some text.
s <- paste(c("Pierre Vinken, 61 years old, will join the board as a",
             "nonexecutive director Nov. 29.\n",
             "Mr. Vinken is chairman of Elsevier N.V., ",
             "the Dutch publishing group."),
           collapse = "")
s <- as.String(s)

## Need sentence and word token annotations before.
sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
a2 <- annotate(s, list(sent_token_annotator, word_token_annotator))
a2

#POS tagging
pos_tag_annotator <- Maxent_POS_Tag_Annotator()
a3 <- annotate(s, pos_tag_annotator, a2)
a3

## Determine the distribution of POS tags for word tokens.
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")
tags
table(tags)
tags

## Extract token/POS pairs (all of them): easy.
sprintf("%s/%s", s[a3w], tags)

## Extract pairs of word tokens and POS tags for second sentence:
a3ws2 <- annotations_in_spans(subset(a3, type == "word"),
                              subset(a3, type == "sentence")[2L])[[1L]]
sprintf("%s/%s", s[a3ws2], sapply(a3ws2$features, `[[`, "POS"))

#Classification

#KNN
library(class)
path <- file.path(getwd(), "text-data", "spambase","spambase.data")
spam <- read.csv(path,header=F,sep=",")

train <- rbind(spam[1:1360, ], spam[1814:3905, ])
trainCl <- train[, 58]
test <- rbind(spam[1361:1813, ], spam[3906:4601, ])
trueCl <- test[, 58]

knnCl <- knn(train[, -58], test[, -58], trainCl,k = 3, prob=TRUE)

# Confusion matrix
conf.mat <- table("Predictions" = knnCl, Actual = trueCl)
conf.mat

# Accuracy
(accuracy <- sum(diag(conf.mat))/nrow(test) * 100)


#Naive Bayes
## Example: Filtering spam SMS messages ----
## Step 2: Exploring and preparing the data ---- 

# read the sms data into the sms data frame
sms_raw <- read.csv(file.path(getwd(), "text-data","sms_spam.csv"), stringsAsFactors = FALSE)

# examine the structure of the sms data
str(sms_raw)

# convert spam/ham to factor.
sms_raw$type <- factor(sms_raw$type)

# examine the type variable more carefully
str(sms_raw$type)
table(sms_raw$type)

# build a corpus using the text mining (tm) package
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))

# examine the sms corpus
print(sms_corpus)
inspect(sms_corpus[1:3])

# clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

# examine the clean corpus
inspect(sms_corpus[1:3])
inspect(corpus_clean[1:3])

# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)
sms_dtm

# creating training and test datasets
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test  <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# word cloud visualization
library(wordcloud)

wordcloud(sms_corpus_train, min.freq = 30, random.order = FALSE)

# subset the training data into spam and ham groups
spam <- subset(sms_raw_train, type == "spam")
ham  <- subset(sms_raw_train, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

# indicator features for frequent words
findFreqTerms(sms_dtm_train, 5)
sms_dict <- findFreqTerms(sms_dtm_train, 5)
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

## Step 3: Training a model on the data ----
## Naive Bayes
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

## Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## Step 5: Improving model performance ----
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

##KNN

# Transform dtm to matrix to data frame - df is easier to work with
mat.df <- as.data.frame(data.matrix(sms_dtm), stringsAsfactors = FALSE)

# Column bind category (known classification)
mat.df <- cbind(mat.df, sms_raw$type)

# Change name of new column to "Type"
colnames(mat.df)[ncol(mat.df)] <- "Type"

# Split data by rownumber into two equal portions
train <- sample(nrow(mat.df), ceiling(nrow(mat.df) * .20))
test <- (1:ceiling(nrow(mat.df)/4))[- train]

# Isolate classifier
cl <- mat.df[, "Type"]

# Create model data and remove "category"
modeldata <- mat.df[,!colnames(mat.df) %in% "Type"]

library(class)
# Create model: training set, test set, training set classifier
knn.pred <- knn(modeldata[train, ], modeldata[test, ], cl[train])

# Confusion matrix
conf.mat <- table("Predictions" = knn.pred, Actual = cl[test])
conf.mat

# Accuracy
(accuracy <- sum(diag(conf.mat))/length(test) * 100)

# Create data frame with test data and predicted category
df.pred <- cbind(knn.pred, modeldata[test, ])
write.table(df.pred, file="output.csv", sep=";")
##Semantic similarity 
