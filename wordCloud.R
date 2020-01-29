# Install
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
install.packages("NLP")
# Load
library("NLP")
library("tm")
library("SnowballC")
library("RColorBrewer")
library("wordcloud")
library("rafalib")

setwd("C:/Users/owner/Documents/Consultoría/Fluid dynamics")

###Text mining
feed<-readLines("feed.txt")
doc.least<- Corpus(VectorSource(feed))
inspect(doc.least)

### Text transformation
toSpace <- content_transformer(function (x, pattern) gsub(pattern, " ", x))
doc.least<- tm_map(doc.least, toSpace, "/")
doc.least<- tm_map(doc.least, toSpace, "@")
doc.least<- tm_map(doc.least, toSpace, "\\|")

### Text cleaning
# Convert the text to lower case
doc.least <- tm_map(doc.least, content_transformer(tolower))
# Remove numbers
doc.least <- tm_map(doc.least, removeNumbers)
# Remove english common stopwords
doc.least <- tm_map(doc.least, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
#docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
# Remove punctuations
doc.least <- tm_map(doc.least, removePunctuation)
# Eliminate extra white spaces
doc.least <- tm_map(doc.least, stripWhitespace)
# Text stemming
#doc.least <- tm_map(doc.least, stemDocument)

### Build a term-documented matrix
#Contains the frequency of the words
dtm<- TermDocumentMatrix(doc.least)
m<- as.matrix(dtm)
v<- sort(rowSums(m), decreasing = TRUE)
d<- data.frame(word = names(v), freq = v)
head(d,10)

#mypar(1,2)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# #find words with low frequency
# findFreqTerms(dtm, lowfreq = 4)
# 
# #find associations with frequent terms
# findAssocs(dtm, terms = "software", corlimit = 0.3)
# findAssocs(dtm, terms = "like", corlimit = 0.3)
# findAssocs(dtm, terms = "needed", corlimit = 0.3)
# findAssocs(dtm, terms = "project", corlimit = 0.3)
# findAssocs(dtm, terms = "use", corlimit = 0.3)

findAssocs(dtm, c("project") , .4)


barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue",
        ylab = "Word frequencies",
        ylim = c(0,16))

sum(d$freq[6:7])/length(least)

d$freq[3]/30

