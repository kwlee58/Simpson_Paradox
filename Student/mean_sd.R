mean.sd <-
function(x) {
  mean <- mean(x, na.rm = TRUE)
  sd <- sd(x)
  list(mean = mean, sd = sd)
}
