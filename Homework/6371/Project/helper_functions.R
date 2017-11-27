rss <- function(model) {
  sum(residuals(model)^2)
}