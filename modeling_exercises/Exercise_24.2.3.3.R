#------------------------------
# Exercise 24.2.3.3 R4DS: Extract the diamonds that have very high and very low residuals.
# Is there anything unusual about these diamonds?
#------------------------------
library(tidyverse)
library(modelr)
library(gridExtra)
 
# For being easier to understand, firstly, the book steps are followed .  
#------------------------------
# Filtering smaller diamonds which are 99.7 % of all. 
diamonds2 <- diamonds %>% 
  filter(carat <= 2.5) %>% 
  mutate(lprice = log2(price), lcarat = log2(carat))
# Model 1: Price vs carat
mod_diamond <- lm(lprice ~ lcarat, data = diamonds2)
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond, "lresid")
# Model 2: Price vs carat, color, cut, and clarity 
mod_diamond2 <- lm(lprice ~ lcarat + color + cut + clarity, data = diamonds2)
diamonds2 <- diamonds2 %>% 
  add_residuals(mod_diamond2, "lresid2")
# Filtering unusual cases which prices were either more than twice or less than half the predicted. 
unusual <- diamonds2 %>% 
  filter(abs(lresid2) > 1) %>% 
  add_predictions(mod_diamond2) %>% 
  mutate(pred = round(2 ^ pred)) %>% 
  select(price, pred, carat:table, x:z) %>% 
  arrange(price)
# End of book steps.
#------------------------------

# Exploration
#------------------------------
colnames(unusual)[colnames(unusual) == 'pred'] <- 'predicted'
# Checking anything unusual about their x, y, z dimensions.
sample_carat = round(seq(0.2, 2.5, 0.01), digits = 3) # round() is used to get rid of numerical errors by computer. 
xmean <- 0
ymean <- 0
zmean <- 0
for(i in seq_along(sample_carat)) {
  diamond_by_carat <- filter(diamonds2, carat == sample_carat[i]) 
  xmean[i] <- mean(diamond_by_carat$x)
  ymean[i] <- mean(diamond_by_carat$y)
  zmean[i] <- mean(diamond_by_carat$z)
}
average_size <- tibble(carat = sample_carat, x_ave = xmean, y_ave = ymean, z_ave = zmean)
p1 <- ggplot() + geom_point(data = unusual, aes(carat,x)) + geom_smooth(data = average_size, aes(carat, x_ave))
p2 <- ggplot() + geom_point(data = unusual, aes(carat,y)) + geom_smooth(data = average_size, aes(carat, y_ave))
p3 <- ggplot() + geom_point(data = unusual, aes(carat,z)) + geom_smooth(data = average_size, aes(carat, z_ave))
grid.arrange(p1,p2, p3, ncol = 3)
# Comment: Some diamonds have sizes far from the average of the same carat diamonds. 

# Any diamond has >10 % off size from the average x, y, z is to be marked 
# as well as diamonds with depth and table sizes outside recommended ranges.  
unusual <- unusual %>% left_join(average_size, by = "carat") %>% 
  mutate(wrong_carat_label = ifelse(abs(x - x_ave) / x_ave > 0.1 | abs(y - y_ave) / y_ave > 0.1 | abs(z - z_ave) / z_ave > 0.1, "Yes", "No")) %>%
  mutate(non_st_depth = ifelse(depth > 64 | depth < 58, "Yes", "No")) %>%
  mutate(non_st_table = ifelse(table > 64 | table < 53, "Yes", "No"))
# Comments: Row 2:6 seem to have wrong carat sizes registered. Row 4 is very deep. 
# Perhaps it does not reflect light nicely (plus bad clarity). That is probably why it has a significantly low price. 
# Moreover some diamonds are either too shallow/deep or have too flat tables, but yet seem to have higher price than estimated. 
# Probably something is special about them. 

# Despite my comments above, there is one interesting case: Diamond in the row 16 is large and seem to be underpriced. It is to be explored below.
# Checking the prices of larger diamonds. 
big_diamonds <- filter(diamonds, carat > 2.0) 
ggplot(big_diamonds, aes(carat, price, color = clarity)) + geom_point()  
# Comment: Clarity reduces prices of large diamonds significantly. 
# The diamond being investigated looks underpriced for its clarity category.
ggplot(big_diamonds, aes(carat, price, color = color)) + geom_point()  
ggplot(big_diamonds, aes(carat, price, color = cut)) + geom_point()  
# Comment: Underpriced for its color and cut category too. 
# My suspicion is that the diamond was either underpriced or its clarity etc label was wrongly registered. 

# End of my exploration.
#------------------------------

# My conclusions are added to the tibble:
unusual = mutate(unusual, guessed_reasons = c("depth", "wrong carat", "wrong carat", "wrong carat+deep", "wrong carat", "wrong carat", "unknown", "depth", "depth", "unknown", "unknown", "table", "depth", "table", "depth", "wrong clarity?"))
select(unusual, price, predicted, wrong_carat_label, non_st_depth, non_st_table, guessed_reasons)                  
