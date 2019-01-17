context("S3 Method Matching")

library(datasets)
library(rpart)
library(randomForest)
data(iris)

set.seed(71)

test_that("rpart matching works", {

  partition <- rpart(Sepal.Width ~ Sepal.Length + Petal.Length + Petal.Width + Species, data = iris)

  # test simple_rss on partition model
  expect_equal(simple_rss(partition), 10.17245, tolerance=1e-3)
})

test_that("randomForest matching works", {
  rf <- randomForest(Sepal.Length ~ ., data=iris, importance=TRUE,
                     proximity=TRUE)
  # test simple_rss on Random Forest model
  expect_equal(simple_rss(rf), 20.65949, tolerance=1e-3)
})

test_that("does not match errant classes", {
  # test simple_rss on a string (should raise an error)
  expect_error(simple_rss("a"))
})

