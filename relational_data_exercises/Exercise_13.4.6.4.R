#------------------------------
# Exercise 13.4.6 R4DS: What weather condition make flights more likely to be delayed?
#------------------------------
library(tidyverse)
library(nycflights13)

# Only delay reason at the departure will be studied. Weather information at the destination airports is not given.
del_by_origin<-flights %>%
  select(dep_delay, arr_delay, origin, time_hour) %>%
  left_join(weather, c("origin"="origin", "time_hour"="time_hour")) %>%
  filter(!is.na(year)==TRUE) %>% # filtering night times without flights
  mutate(is_delayed=ifelse(dep_delay>0,"delayed", "not_delayed")) %>%
  filter(wind_speed<1000) # possibly some erranous data on Feb 12

del_by_origin %>%
select(temp:is_delayed) %>% 
gather(-is_delayed, key = "weather_att", value = "value") %>%
ggplot() + geom_boxplot(aes(x = is_delayed, y = value), na.rm=TRUE) +
facet_wrap(~ weather_att, scales = "free") +
theme(axis.title = element_text(size=1)) + 
labs(x="Status", y="Value") 

# Observations: On average higher temperature (dewpoint), wind speed, gust speed and lower pressure  are observed for delayed flights. 
# There were no statistically sigificant differences in relative humidity, wind direction, preciptation and visibility regarding the delays.
# Note: Low air pressure generally means warm and moist air which brings stormy weather (cloudy, rainy, snowy) with strong winds (weatherwizkids.com). 
# Conclusion: From the above observations stormy weather make flights more likely be delayed (=common sense knowledge).
