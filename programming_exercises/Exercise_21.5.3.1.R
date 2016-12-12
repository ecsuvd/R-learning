#------------------------------
# Exercise 21.5.3 R4DS: Write code that uses one of the map functions to: 
# 1. Compute the mean of every column in mtcars.  
#------------------------------
mean_mtcars<-mtcars %>% map_dbl(mean)
#------------------------------
# 2. Determine the type of each column in nycflights13::flights 
#------------------------------
type_flights<-flights %>% map(typeof)
#------------------------------
# 3. Compute the number of unique values in each column of iris.
#------------------------------
unique_value_iris<-iris %>% map_int(function(x) length(unique(x)))
#------------------------------
# 4. Generate 10 random normals for each of μ=−10, 0, 10, and 100.
#------------------------------
mu=c(-10, 0, 10, 100)
random_numbers<-mu %>% map(rnorm, n=10)


