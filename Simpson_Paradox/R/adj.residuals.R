adj.residuals <- function(fit, ...) {
  residuals(fit, ...) / sqrt(1 - lm.influence(fit)$hat)
}
