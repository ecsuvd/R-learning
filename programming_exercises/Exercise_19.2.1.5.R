#------------------------------
# Exercise 19.2.1 R4DS: Write both_na(), a function that takes 2 vectors of the same length and 
# returns the number of positions that have an NA in both vectors.
#------------------------------
# x, y vectors of the same length.
both_na<-function(x,y) {
  if (length(x)!=length(y)) {
    stop("Vector lengths are not the same.")
  } else {
    na_location_x<-which(is.na(x))
    na_location_y<-which(is.na(y))
    na_location_x[na_location_x==na_location_y]
  }
}
# test:
x=c(NA, 1:4, NA, NA)
y=c(NA, NA, 4:7, NA)
z=c(1:5, NA, 7:10)
both_na(x,y)
both_na(x,z)  
