#------------------------------
# Exercise 13.5.1 R4DS: Find the 48 hours that has the worst delays. Cross reference with the weather.
#------------------------------
library(tidyverse)
library(nycflights13)

del_by_origin<-flights %>%
  select(dep_delay, arr_delay, origin, time_hour) %>%
  left_join(weather, c("origin"="origin", "time_hour"="time_hour")) %>%
  filter(!is.na(year)==TRUE) %>% # filtering night times without flights
  mutate(is_delayed=ifelse(dep_delay>0,"delayed", "not_delayed")) %>%
  filter(wind_speed<1000) # possibly some erranous data on Feb 12
delay48<-del_by_origin %>%  # del_by_origin is also used in previous exercises.
  group_by(time_hour) %>%
  summarise(n_delays=sum(is_delayed=="delayed"), total=n(), delay_frac=n_delays/total) 
# n_delays - number of delayed flights in a grouped 48 hours
# delay_frac - fraction of delayed flights in a grouped 48 hours
delay48$n_delays[is.na(delay48$n_delays)]<-0 # hours with no delay
delay48$delay_frac[is.na(delay48$delay_frac)]<-0
sum48<-0
sum48_2<-0
for(i in 1:nrow(delay48)) {
  sum48[i]<-sum(delay48$n_delays[which(delay48$time_hour>=delay48$time_hour[i] & delay48$time_hour<delay48$time_hour[i]+48*3600)])
  sum48_2[i]<-sum(delay48$total[which(delay48$time_hour>=delay48$time_hour[i] & delay48$time_hour<delay48$time_hour[i]+48*3600)])
}
delay48<-mutate(delay48, n_delays48=sum48, delay_frac48=sum48/sum48_2) 
delay48<-filter(delay48, time_hour<=time_hour[length(time_hour)]-48*3600)
# n_delay48 and delay_frac48 corresponds to the total number of delays in the next 48 hours of time.
ggplot(delay48) + geom_line(aes(time_hour, delay_frac48)) + labs(x="Date", y="Fraction of Delays in 48 hrs")
paste("The worst 48 hours period is between", delay48$time_hour[which.max(delay48$delay_frac48)], "and", +
        delay48$time_hour[which.max(delay48$delay_frac48)]+48*3600)
# Comment: weather history in Google shows that it snowed heavily in NYC in the previous 10 days and it started getting warmer around this date. 

delay48$date<-as.Date(delay48$time_hour)
a<-group_by(delay48[delay48$delay_frac48>0.3,], date) %>% #filtering periods with more than 30 % of delayed flights.
  summarise(mean(delay_frac48))
View(a)
# Comments: From the plot it seems that on average 15-20 % of the total flights depart delayed (>0 min) in NYC.  
# Heavily delayed periods are (>30%): Jan 4-8, Mar 9-24 (snow storm), Apr 20-28 (cold rain), May 15-27 (tornado), 
# June 15-24 (?), Jul 2-5 (holiday?), Aug 9-16 (storm), Aug 28-31 (storm), Oct 4-6 (?), Oct 16-19 (?), Nov 24-26 (heavy rain), Nov 30-Dec 4 (holiday?),
# Dec 17-29 (holiday?+heavy snow). 
