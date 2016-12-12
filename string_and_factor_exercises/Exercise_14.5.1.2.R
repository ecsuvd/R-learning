#------------------------------
#  Exercise 14.5.1 R4DS: What are the five most common words in sentences?
#------------------------------
library("tm")
library("SnowballC")

# A simple test: 
test<-sentences %>% head(5) # test on the first 5 sentences. 

# The assignment begins:
test<-sentences
a<-str_split(test, boundary("word"))
test1<-Corpus(VectorSource(a)) # loading data as a corpus.
# Text cleaning:
test1<-tm_map(test1, content_transformer(tolower)) # lower case
test1<-tm_map(test1, removeNumbers)
test1<-tm_map(test1, removeWords, stopwords("english")) # remove stop words
test1<-tm_map(test1, removePunctuation) 
test1<-tm_map(test1, stripWhitespace) 
test1<-tm_map(test1, stemDocument) # stemizing
m<-as.matrix(DocumentTermMatrix(test1))
common<-sort(colSums(m), decreasing=TRUE) %>% head(5)
paste("Most common words in Harward sentences are:", names(common)[1],",", names(common)[2],",",names(common)[3],",",names(common)[4],",",names(common)[5])
# To put the words in a data frame:
#data.frame(word=names(common), frequency=common)