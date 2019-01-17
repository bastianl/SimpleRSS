library(pryr)


#' Calculate RSS
#'
#' Calculate the Residual Sum of Squares (RSS) for different model types.
#' For many statistical models, one is interested in the accuracy
#' of fit of the data, often quantified by the RSS (residual sum of squares)
#' For some R statistical models, the residuals function has already been defined
#' (such as for Recursive Partitioning Trees). However, for many it has not been defined
#' here we implement a default simple_rss method to calculate the RSS when the residuals
#' function is available, but use S3 to make it easily extensive when residuals is not available.
#'
#' @param x an object, for which to calculate the RSS
#' @param ... other arguments passed to specific methods
#' @return a numeric
#'
#' @examples
#' simple_rss(lm(Petal.Width ~ Petal.Length, data=iris))
#' # 6.310096
#' library(randomForest)
#' data(iris)
#' rf <- randomForest(Sepal.Length ~ ., data=iris, importance=TRUE,
#'                    proximity=TRUE)
#' simple_rss(rf)
#' # 20.65949
#' @seealso \url{https://en.wikipedia.org/wiki/Residual_sum_of_squares}
#' @export
simple_rss <- function(x, ...) {
  UseMethod("simple_rss")
}

#' RSS default
#' Checks for availability of the "residuals" generic for the class,
#' and calculates the RSS if available.
#' @param x an object, for which to calculate the RSS
#' @param ... other arguments passed to specific methods
#' @return a numeric
#' @export
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

#' Random Forest RSS
#'
#' This method calculates the residuals manually, then calculates the RSS.
#' @param x an object, for which to calculate the RSS
#' @param ... other arguments passed to specific methods
#' @return a numeric
#' @export
#' #' @seealso \url{https://en.wikipedia.org/wiki/Random_forest}
simple_rss.randomForest <- function(x, ...) {
  sum((x$y - x$predicted)**2)
}
