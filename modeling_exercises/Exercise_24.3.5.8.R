#------------------------------
# Exercise 24.3.5.8 R4DS: Write a small function to set the levels of the factor so that the week starts on Monday.
#------------------------------
library(lubridate)

wday_MtoS <- function(date){
  weekday_levels <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
  factor(wday(date, label = TRUE), levels = weekday_levels)
}
