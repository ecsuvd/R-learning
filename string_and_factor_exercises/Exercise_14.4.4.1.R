#------------------------------
# Exercise 14.4.4.1 R4DS: Find all words after a "number" from Harward sentences. 
#------------------------------
library("tidyverse")
library("stringr")

# To pull numbers (1 to 10) and its following words:
nn<-c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
number<-str_c(nn, collapse="|\\b")
ww<-str_c("(\\b",number,") ([^ ]+)") # regex for a word follows a number.
has_number<-str_subset(str_to_lower(sentences), ww)
str_view_all(has_number, ww) # viewing words found. 
matches<-str_extract_all(has_number, ww)
matches
# Showing together with the original sentences:
tibble(sentence=has_number, match=as.character(matches))

