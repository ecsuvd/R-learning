# Exercise 13.4.6 R4DS: Investigating whether there is a connection between the age of planes and the delay
library(tidyverse)
library(nycflights13)
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
