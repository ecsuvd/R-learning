#------------------------------
# Exercise 24.3.5.7 R4DS: Explore hypothesis that people leaving on Sundays are more likely to be business travellers who need to be somewhere on Monday.
#------------------------------
library(tidyverse)
library(modelr)
library(nycflights13)
library(lubridate)

flights1 <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  mutate(wday = wday(date, label = TRUE)) %>% 
  mutate(hour = hour(time_hour)) %>%
  group_by(wday, hour) %>% 
    summarise(
      n = n(), 
      average_distance = mean(distance)
    )
ggplot(flights1) + 
  geom_point(aes(x = hour, y = n, color = wday), stat = "identity") +
  labs(title = "Flight distribution over day time", x = "Hour", y = "Number of flights")
# Comment: Weekday flight distributions over day hours are nearly identical.
# Sunday afternoon (12 - 21 pm) flights have about the same distribution as weeekdays whereas Sunday morning flights are significantly lower than the weekdays. 

ggplot(flights1) + 
  geom_point(aes(x = hour, y = average_distance, color = wday), stat = "identity") +
  labs(title = "Average flight distance by departure time", x = "Hour", y = "Number of flights")
# Comment: Afternoon flights are in general not particularly long distance in comparison to e.g Monday morning flights.
# On the other hand, Saturday afternoon flights are longer distance than the other weekdays. 
