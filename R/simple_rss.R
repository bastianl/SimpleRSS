library(pryr)

# for many statistical models, one is interested in the accuracy
# of fit of the data, often quantified by the RSS (residual sum of squares)
# For some of these methods, the residuals function has already been defined
# (such as for Recursive Partitioning Trees). However, for many it has not been defined
# here we implement a default simple_rss method to calculate the RSS when the residuals
# function is available, but use S3 to make it easily extensive when residuals is not available.

simple_rss <- function(x, ...) {
  UseMethod("simple_rss")

}

simple_rss.default <- function(x, ...) {
  # use the default residuals s3 generic
  # only available for some functions (see methods(residuals))
  if (any(paste("residuals", class(x), sep=".") %in% methods(residuals))) {
    return(sum(residuals(x)**2))
  } else {
    stop(paste("residuals function is not available for class: ", class(x)))
    return(NULL)
  }
}

simple_rss.randomForest <- function(x, ...) {
  sum((x$y - x$predicted)**2)
}
