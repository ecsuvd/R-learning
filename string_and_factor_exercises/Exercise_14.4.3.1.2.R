#------------------------------
# Exercise 14.4.3.1 R4DS: extract all plurals from sentences. 
#------------------------------
library("NLP")
library("tm")
library("openNLP")
library("stringr")

# This function is taken from stackoverflow.com/questions/30995232/how-to-use-opennlp-to-get-pos-tags-in-r
extractPOS<-function(x, thisPOSregex){
   x<-as.String(x)
   wordAnnotation<-annotate(x, list(Maxent_Sent_Token_Annotator(), Maxent_Word_Token_Annotator()))
   POSAnnotation<-annotate(x, Maxent_POS_Tag_Annotator(),wordAnnotation)
   POSwords<-subset(POSAnnotation, type=="word")
   tags<-sapply(POSwords$features, '[[', "POS")
   thisPOSindex<-grep(thisPOSregex, tags)
   tokenizedAndTagged<-sprintf("%s/%s", x[POSwords][thisPOSindex], tags[thisPOSindex])
   untokenizedAndTagged<-paste(tokenizedAndTagged, collapse = " ")
   untokenizedAndTagged
}
# Testing the function:
txt<-c("I have a brown purse.", "I have books written by Isaac Asimov. Where are the cartoons?")
lapply(txt, extractPOS, "NN") 

# Extracting the plurals from "sentences":
a<-lapply(sentences, extractPOS, "NNS")
b<-str_replace_all(a, "\\/NNS", "")
indexes <- which(b == "") # removing empty elements. 
if(length(indexes) > 0){
  b <- b[-indexes]
}  
# Comment: openNLP is slow on large texts and it does not guarantee to extract all nouns.   
