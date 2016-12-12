#------------------------------
# Exercise 19.2.1 R4DS: Write a function which rescales a vector to lie between 0 and 1, and -Inf is mapped to 0, and Inf is mapped to 1.
#------------------------------
x <- c(1:10, -Inf, Inf, -Inf, NA)
rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE, finite = TRUE)
  y<-(x - rng[1]) / (rng[2] - rng[1])
  y[x==Inf] <-1
  y[x==-Inf] <-0
  return(y)
}
rescale01(x)

