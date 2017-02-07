#------------------------------
# Exercise 24.3.5.5 R4DS: What happens if you fit a day of week effect that varies by month (i.e. n ~ wday * month)? Why is this not very helpful?
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
mod3 <- MASS::rlm(n ~ wday * term, data = daily)
daily <- daily %>%
  add_residuals(mod3, "resid3") # third model in the book
# End of book steps.
#------------------------------

daily2 <- daily %>% 
  mutate(month = month(date)) 
mod5 <- MASS::rlm(n ~ wday * month, data = daily2) # model is named so not to collide with other models in the book and my previous exercises.
daily2 <- daily2 %>%
  add_residuals(mod5, "resid5")
ggplot() +
  geom_line(data = daily2, aes(date, resid5, color = "this model")) + 
  geom_line(data = daily, aes(date, resid, color = "mod")) + 
  geom_line(data = daily, aes(date, resid3, color = "mod3")) + 
  labs(title = "Model Comparison", x = "Date", y = "Residuals") +
  scale_color_manual("Models", values = c("mod" = "grey", "mod3" = "black", "this model" = "red"))
# Comment: it predicts better than the first model, especially in the autumn term. Though does not predict summer season as good as the "mod3". 
