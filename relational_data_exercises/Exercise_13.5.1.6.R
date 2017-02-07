#------------------------------
# Exercise 13.5.1.6 R4DS: Check whether an airplane belongs to a single airline.
#------------------------------
library(tidyverse)
library(nycflights13)

check <- select(flights, carrier, tailnum)
filter(count_(unique(check), vars = "tailnum"), n != 1)
# Conclusion: 17 planes flew under 2 different carriers.