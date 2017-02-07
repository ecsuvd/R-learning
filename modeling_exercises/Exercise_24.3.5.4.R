#------------------------------
# Exercise 24.3.5.4 R4DS: Create a new "wday" variable that combines the day of week, term (for Saturdays), and public holidays. 
# What do the residuals of that model look like?
#------------------------------
library(tidyverse)
library(modelr)
library(nycflights13)
library(lubridate)

# As analysis started in the book the steps are followed.
#------------------------------
daily <- flights %>% 
  mutate(date = make_date(year, month, day)) %>% 
  group_by(date) %>% 
  summarise(n = n()) %>%
  mutate(wday = wday(date, label = TRUE))
mod <- lm(n ~ wday, data = daily) # first model 
daily <- daily %>% 
  add_residuals(mod)
term <- function(date) {
  cut(date, 
	breaks = ymd(20130101, 20130605, 20130825, 20140101),
	labels = c("spring", "summer", "fall") 
	)
}
daily <- daily %>% 
  mutate(term = term(date)) 
# End of book steps.
#------------------------------
            
# Adding the new variable:
holidays <- ymd(20130101, 20130121, 20130218, 20130527, 20130704, 20130902, 20131014) 
isholiday <- ifelse(daily$date %in% holidays, "holiday", "not_holiday")
sat_term <- ifelse(daily$wday == "Sat", paste(daily$wday, daily$term, sep = "-"), paste(daily$wday))
new_var <- paste(sat_term, isholiday, sep = "-")

daily1 <- daily %>% 
  mutate(wday_term_holiday = new_var) 

# New model:
mod4 <- MASS::rlm(n ~ wday_term_holiday, data = daily1) # model is named so not to collide with other models in the book.
daily1 <- daily1 %>% 
  add_residuals(mod4, "resid4")

# Comparing with the previous model and "mod3" model in the book:
mod3 <- MASS::rlm(n ~ wday * term, data = daily1)
daily1<-daily1 %>%
  add_residuals(mod3, "resid3")
ggplot(daily1, aes(date)) +
  geom_line(aes(y = resid, color = "mod")) + 
  geom_line(aes(y = resid3, color = "mod3")) +
  geom_line(aes(y = resid4, color = "this model")) +
  labs(title = "Model Comparison", x = "Date", y = "Residuals") +
  scale_color_manual("Models", values = c("mod" = "grey", "mod3" = "black", "this model" = "red"))
# Comment: mod4 predicts better than the first model and has less steep spikes. Though overall may not be better than the mod3. 
