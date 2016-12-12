#-----------------------------
# Exercise 13.4.6 R4DS: What happened on June 13, 2013? 
#-----------------------------
library(tidyverse)
library(nycflights13)
library("gridExtra")

del_by_origin<-flights %>%
select(dep_delay, arr_delay, origin, time_hour) %>%
left_join(weather, c("origin"="origin", "time_hour"="time_hour")) %>%
filter(!is.na(year)==TRUE) %>% # filtering night times without flights
mutate(is_delayed=ifelse(dep_delay>0,"delayed", "not_delayed")) %>%
filter(wind_speed<1000) # possibly some erranous data on Feb 12

# Testing how much of the flights were delayed or cancelled that day.
june13<-filter(del_by_origin, year=="2013", month=="6", day=="13")
p1<-ggplot(june13) + geom_bar(aes(is_delayed), na.rm=TRUE) + labs(title="June 13, 2013", x="case", y="counts")
p2<-ggplot(del_by_origin) + geom_bar(aes(is_delayed), na.rm=TRUE) + labs(title="Total Delay in 2013", x="case", y="counts")
grid.arrange(p1,p2, ncol=2)
# Comment: More delays occured than the average throughout the year. 

d2<-del_by_origin # the tibble is used in Exercise 13.4.6.4 R4DS. 
d2$date=paste(d2$year, d2$month, d2$day, sep="-")
d2$date=as.Date(d2$date)
x<-d2 %>% group_by(date) %>%
  summarise(daily_ave_delay=mean(dep_delay[dep_delay>0], na.rm=TRUE), cancelled=sum(is.na(dep_delay))) 
y<-mutate(x, the_day=ifelse(date=="2013-6-13", "June 13", " ")) # marking June 13 data
p3<-ggplot(y, aes(x=date, y=daily_ave_delay, fill=the_day)) + geom_bar(stat="identity", show.legend = FALSE) + 
    labs(x="Date", y="Average Delay, min") + geom_text(aes(label=the_day))
p4<-ggplot(y, aes(x=date, y=cancelled, fill=the_day)) + geom_bar(stat="identity", show.legend = FALSE) + 
    labs(x="Date", y="Cancelled flights") + geom_text(aes(label=the_day))
grid.arrange(p3, p4, nrow=2)

# Comment: It seems to be that the Jun 13, 2013 was one of the bad days in terms of delays and cancellations, but not the worst of the year. 
# Weather report on Google shows that there was a thunderstorm in NYC.

