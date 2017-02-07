#------------------------------
# Exercise 25.2.5.1 R4DS: Do a model with a quadratic polynomial on "gapminder" data. How can you interpret the coefficients of the quadratic?
#------------------------------
library(tidyverse)
library(modelr)
library(gapminder)

# I will just follow the book step while changing the model. 
by_country <- gapminder %>% 
  group_by(country, continent) %>% 
  nest()
country_model <- function(df) {
  lm(lifeExp ~ poly(year, 2), data = df)
}
by_country <- by_country %>% 
  mutate(model = map(data, country_model))
by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )
resids <- unnest(by_country, resids)
resids %>% 
  ggplot(aes(year, resid)) +
    geom_line(aes(group = country), alpha = 1 / 3) + 
    geom_smooth(se = FALSE) +
    labs(title = "Model Fit", x = "Year", y = "Residuals")
# Comment: residuals look more flattened out along the 0 line of y-axis. 
# In general, the quadratics coefficient is expected to be negative as it becomes more difficult to extend human life at old ages. 
