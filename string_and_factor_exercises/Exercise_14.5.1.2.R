#------------------------------
#  Exercise 14.5.1.2 R4DS: What are the five most common words in sentences?
#------------------------------
library("tm")
library("SnowballC") 

text <- sentences
text_splitted <- str_split(text, boundary("word"))
text_corpus <- Corpus(VectorSource(text_splitted)) # loading data as a corpus.

# Text cleaning:
text_corpus <- tm_map(text_corpus, content_transformer(tolower)) # lower case
text_corpus <- tm_map(text_corpus, removeNumbers)
text_corpus <- tm_map(text_corpus, removeWords, stopwords("english")) # remove stop words
text_corpus <- tm_map(text_corpus, removePunctuation) 
text_corpus <- tm_map(text_corpus, stripWhitespace) 
text_corpus <- tm_map(text_corpus, stemDocument) # stemizing
m <- as.matrix(DocumentTermMatrix(text_corpus))
common <- sort(colSums(m), decreasing = TRUE) %>% head(5)
paste("Most common words in Harward sentences are:", names(common)[1],",", names(common)[2],",",names(common)[3],",",names(common)[4],",",names(common)[5])
# To put the words in a data frame:
#data.frame(word=names(common), frequency=common)