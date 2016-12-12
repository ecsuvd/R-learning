###############################
# Exercises on relational data. 
###############################
library(tidyverse)
library(nycflights13)
#----------------------------
# Exercise 13.5.1 R4DS: What does it mean for a flight to have a missing tailnum?
#----------------------------
x<-flights %>% 
  anti_join(planes, by="tailnum") %>%
  group_by(carrier) %>% summarise(total_flights=n())
arrange(x, desc(total_flights))
# 90 % of the flights with missing tailnums are operated by 2 main carriers: AA and MQ. 
# Check: help(planes) states that AA and MQ report flights rather via fleet numbers than the tailnums.