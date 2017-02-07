#------------------------------
#  Exercise 14.4.6.1 R4DS: Split up a string like "apples, pears, and bananas" into individual components.
#------------------------------
library("stringr")

c <- "apples, pears, and bananas"
a <- str_split(c, ", |and ")[[1]]
indexes <- which(a == "") # removing empty elements. 
if(length(indexes) > 0) {
  a <- a[-indexes]
} 
