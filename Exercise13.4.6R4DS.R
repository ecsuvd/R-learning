library(tidyverse)
library(nycflights13)
# Exercise 13.4.6 R4DS: Investigating whether there is a connection between the age of planes and the delay
by_age<-group_by(flights, tailnum)
delay<-summarize(by_age, av_arr_del=mean(arr_delay[arr_delay>0], na.rm=TRUE), av_dep_del=mean(dep_delay[dep_delay>0], na.rm=TRUE))
delay<-planes %>% 
  select(tailnum,year) %>%
  right_join(delay, c("tailnum"="tailnum"))
delay<-mutate(delay, age=2013-year)
ggplot(delay) + 
  geom_point(aes(age, av_arr_del), na.rm=TRUE, color="blue") + 
  geom_point(aes(age, av_dep_del), na.rm=TRUE, color="red") + 
  ylab("delay") +xlab("Age of the plane")

# conclusion: the trend of delay vs the age of the plane if virtually horizantal which is interpretted as there is no (significant) dependency on the delay. 

# Exercise 13.4.6 R4DS: What weather condition make flights more likely to be delayed?
# Only delay reason at the departure will be studied. The weather information at the destination airports is not given.
del_by_origin<-flights %>%
  select(dep_delay, arr_delay, origin, time_hour) %>%
  left_join(weather, c("origin"="origin", "time_hour"="time_hour")) %>%
  filter(!is.na(year)==TRUE) %>% # filtering night times without flights
  mutate(del_or_not=ifelse(dep_delay>0,"delayed", "not_delayed")) %>%
  filter(wind_speed<1000) # possibly some erranous data
ggplot(del_by_origin) + geom_boxplot(aes(del_or_not, temp), na.rm=TRUE)
ggplot(del_by_origin) + geom_boxplot(aes(del_or_not, dewp), na.rm=TRUE)
ggplot(del_by_origin) + geom_boxplot(aes(del_or_not, humid), na.rm=TRUE)
ggplot(del_by_origin) + geom_boxplot(aes(del_or_not, wind_dir), na.rm=TRUE)
ggplot(del_by_origin) + geom_boxplot(aes(del_or_not, wind_speed), na.rm=TRUE)
ggplot(del_by_origin) + geom_boxplot(aes(del_or_not, wind_gust), na.rm=TRUE)
ggplot(del_by_origin) + geom_boxplot(aes(del_or_not, precip), na.rm=TRUE)
ggplot(del_by_origin) + geom_boxplot(aes(del_or_not, pressure), na.rm=TRUE)
ggplot(del_by_origin) + geom_boxplot(aes(del_or_not, visib), na.rm=TRUE)
# Observation: On average higher temperature, "dewp", wind speed, wind gust and lower pressure  are observed for delayed flights. 
# There were no statistical sigificance in humid, wind direction, "precip"?, "visib"? regarding the delays.

# Exercise 13.4.6 R4DS: What happened on June 13 2013? 
# testing how much of the flights were delayed or cancelled that day.
library("gridExtra")
june13<-filter(del_by_origin, year=="2013", month=="6", day=="13")
p1<-ggplot(june13) + geom_bar(aes(del_or_not), na.rm=TRUE) + ggtitle("June 13 2013")
p2<-ggplot(del_by_origin) + geom_bar(aes(del_or_not), na.rm=TRUE) + ggtitle("Yearly average delay")
grid.arrange(p1,p2, ncol=2)
#Comment: More delays occured than the average throughout the year. 

d2<-del_by_origin
d2$date=paste(d2$year, d2$month, d2$day, sep="-")
x<-d2 %>% group_by(date) %>%
  summarise(daily_ave_delay=mean(dep_delay, na.rm=TRUE), cancelled=sum(is.na(dep_delay))) 
# y<-mutate(x, the_day=ifelse(date=="2013-6-13", "yes", "no")) #  labeling June 13 data in x
p3<-ggplot(y) + geom_bar(aes(x=date, y=daily_ave_delay, fill=the_day), stat="identity") 
p4<-ggplot(y) + geom_bar(aes(x=date, y=cancelled, fill=the_day), stat="identity") 
grid.arrange(p3, p4, nrow=2)
# Comment: It seem to be that the Jun13, 2013 was one of the bad days, but not the worst. 
# Weather report will be checked on Google. 
# PS. Quality of the figure need to be improved. 

# Google: Thunderstorm in NYC. 

# Exercise 13.5.1 R4DS: What does it mean for a flight to have a missing tailnum?
x<-flights %>% 
  anti_join(planes, by="tailnum") %>%
  group_by(carrier) %>% summarise(total_flights=n())
arrange(x, desc(total_flights))
# 90 % of the flights are operated by the 2 main carriers: AA and MQ. 
# Check: ?planes states that AA and MQ report flights rather via fleet numbers than the tailnums.

# Exercise 13.5.1 R4DS: Find the 48 hours that has the worst delays. Cross reference with the weather
delay48<-del_by_origin %>%  # check row 20-25
  group_by(time_hour) %>%
  summarise(n_delays=sum(del_or_not=="delayed"), total=n(), delay_frac=n_delays/total) 
delay48$n_delays[is.na(delay48$n_delays)]<-0
delay48$delay_frac[is.na(delay48$delay_frac)]<-0
sum48<-0
sum48_2<-0
for(i in 1:nrow(delay48)) {
  sum48[i]<-sum(delay48$n_delays[which(delay48$time_hour>=delay48$time_hour[i] & delay48$time_hour<delay48$time_hour[i]+48*3600)])
  sum48_2[i]<-sum(delay48$total[which(delay48$time_hour>=delay48$time_hour[i] & delay48$time_hour<delay48$time_hour[i]+48*3600)])
}
delay48<-mutate(delay48, n_delays48=sum48, delay_frac48=sum48/sum48_2) 
delay48<-filter(delay48, time_hour<=time_hour[6910]-48*3600)
# n_delay48 and delay_frac48 corresponds to the total number of delays in the next 48 hours of the row time
ggplot(delay48) + geom_line(aes(time_hour, delay_frac48))
#delay48[which.max(delay48$delay_frac48),]
#delay48[which.max(delay48$n_delays48),]
paste("The worst 48 hours period is between", delay48$time_hour[which.max(delay48$n_delays48)], "and", +
        delay48$time_hour[which.max(delay48$n_delays48)]+48*3600)
# Google: weather history shows that it snowed heavily in NYC in the previous 10 days and it started getting warmer around the heavy delay date. 
#a<-weather[weather$time_hour=="2013-12-17 15:00:00",] # just and example to check. It's not a good check though. 

delay48$date<-as.Date(delay48$time_hour)
a<-group_by(delay48[delay48$delay_frac48>0.3,], date) %>% 
  summarise(mean(delay_frac48))
View(a)
# It seems that the 15-20 % of total flights are expected to depart delayed (>0 min) in NYC.  
# The worst delayed periods are (>30%): Jan 4-8, Mar 9-24(snow storm), Apr 20-28(cold rain), May 15-27 (tornado), 
# June 15-24(?), Jul 2-5 (holiday), Aug 9-16(storm), Aug 28-31(storm), Oct 4-6(?), Oct 16-19(?), Nov 24-26(heavy rain), Nov 30-Dec 4(holiday?),
# Dec 17-29(christmas+heavy snow). 

# Exercise 13.5.1 R4DS: Check whether an airplane belongs to a single airline.
# this is a test:
a<-tibble(car=c("aa", "aa", "bb", "bb", "bb", "aa"), tail=c("a1","a2", "b1", "b1", "b2", "b2"))
b<-count_(unique(a), vars="tail")
# count(unique(group_by(a, tail)) # above line is equivalent to this one.
filter(b, n!=1)
# Now:
check<-select(flights, carrier, tailnum)
filter(count_(unique(check), vars="tailnum"), n!=1)
# 17 planes flew under 2 different carrier

#  Exercise 14.4.4.1 R4DS: Find all words after a "number" from Harward sentences. 
# Just to pull the numbers (1 to 10) and the following words:
nn<-c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten")
number<-str_c(nn, collapse="|")
ww<-paste("(",number,") ([^ ]+)") # a word follows a number.
has_number<-str_subset(str_to_lower(sentences), ww)
matches<-str_extract_all(has_number, ww)
# Showing the original sentences (it misses double occurance of numbers):
library("stringr")
aa<-tibble(sentence = str_to_lower(sentences)) %>% 
  tidyr::extract(
    sentence, c("number", "word"), ww, 
    remove = FALSE
  )
View(filter(aa, number!="NA"))

#  Exercise 14.4.4.1 R4DS: Split up a string like "apples, pears, and bananas" into individual components.
c<-"apples, pears, and bananas"
a<-str_split(c, ",|and")[[1]]
indexes <- which(a == " ") # removing the space. 
if(length(indexes) > 0){
  a <- a[-indexes]
} 

#  Exercise 14.5.1 R4DS: What are the five most common words in sentences?
library("tm")
library("SnowballC")
# A simple test. 
#test<-"Test1 test1 ba1, ba1 ba1. Test2 test2 ba2 ba2 ba22."
test<-sentences #%>% head(5) # head is for the test
a<-str_split(test, boundary("word"))
test1<-Corpus(VectorSource(a)) # loading data as a corpus
# text cleaning
test1<-tm_map(test1, content_transformer(tolower)) # lower case
test1<-tm_map(test1, removeNumbers)
test1<-tm_map(test1, removeWords, stopwords("english")) # remove stop words
test1<-tm_map(test1, removePunctuation) 
test1<-tm_map(test1, stripWhitespace) 
test1<-tm_map(test1, stemDocument) # stemizing
m<-as.matrix(DocumentTermMatrix(test1))
common<-sort(colSums(m), decreasing=TRUE) %>% head(5)
paste("Most common words in Harward sentences are:", names(common)[1],",", names(common)[2],",",names(common)[3],",",names(common)[4],",",names(common)[5])
# to put the words in a data frame:
#data.frame(word=names(common), frequency=common)

# Exercise 14.4.3.1 R4DS: extract all plurals from sentences. 
library("NLP")
library("tm")
library("openNLP")
library("stringr")
# this is taken from stackoverflow.com/questions/30995232/how-to-use-opennlp-to-get-pos-tags-in-r
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
# testing the function:
#txt<-c("I have a brown purse.", "My books are written by Isaac Asimov. Where are the cartoons?")
#lapply(txt, extractPOS, "NN") 
a<-lapply(sentences, extractPOS, "NNS")
#openNLP is slow on large texts. It takes long time to run the script for Harward Sentences. 
#Also, it does not guarantee to extract all nouns. 
b<-str_replace_all(a, "\\/NNS", "")
indexes <- which(b == "") # removing empty elements. 
if(length(indexes) > 0){
  b <- b[-indexes]
}  
  
# Exercise 15.3.1 R4DS: Explore income distribution in forcats::gss_cat data.
library("forcats")
summarize(group_by(gss_cat, rincome))
tt<-gss_cat
tt$rincome <-as.character(tt$rincome)
tt$rincome[tt$rincome %in% c("No answer","Don't know","Refused","Not applicable")] <- "NA"
# it's probably better to have income intervals with equal width:
tt$rincome[tt$rincome %in% c("Lt $1000", "$1000 to 2999", "$3000 to 3999", "$4000 to 4999")] <- "Lt 5000"
tt$rincome[tt$rincome %in% c("$5000 to 5999", "$6000 to 6999", "$7000 to 7999", "$8000 to 9999")] <- "$5000 - 9999"
income_levels=c("Lt 5000","$5000 - 9999", "$10000 - 14999", "$15000 - 19999", "$20000 - 24999", "$25000 or more")
yy<-within(tt, rincome<-factor(rincome, levels=income_levels))  
ggplot(yy, aes(rincome)) + geom_bar()
count(yy, rincome)
# About one third of the survey participants has not provided their income. (categorial value not missing by random) 
# In some cases the reasons of not providing the income shouldn't be replaced as NA just as I did. 

  
  
  
  
  
  
  
  
  







