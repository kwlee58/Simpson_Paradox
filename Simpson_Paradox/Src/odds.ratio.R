odds.ratio <- function(x, addtocounts = 0) {
  x <- x + addtocounts
  (x[1,1] * x[2,2])/(x[1,2] * x[2,1])
}
