#------------------------------
# Exercise 13.4.6 R4DS: Investigating whether there is a connection between age of planes and delays.
#------------------------------
library(tidyverse)
library(nycflights13)

by_age<-group_by(flights, tailnum)
delay<-summarize(by_age, av_arr_del=mean(arr_delay[arr_delay>0], na.rm=TRUE), av_dep_del=mean(dep_delay[dep_delay>0], na.rm=TRUE))
delay<-planes %>% 
  select(tailnum,year) %>%
  right_join(delay, c("tailnum"="tailnum"))
delay<-mutate(delay, age=2013-year)
ggplot(delay) + 
  geom_point(aes(age, av_arr_del, color="Arrival"), na.rm=TRUE) + 
  geom_point(aes(age, av_dep_del, color="Departure"), na.rm=TRUE) + 
  labs(x="Age of the plane", y="Delay", color="case") 
# Conclusion: Trend of delays vs. age of planes is virtually horizantal which is interpretted as there is no (significant) dependency on the delay. 
