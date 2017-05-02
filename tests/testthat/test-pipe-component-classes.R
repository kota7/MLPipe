library(testthat)
library(MLPipe)
library(R6)

context("Pipe components class")


test_that('Are pipe components valid?', {
  # find all object names
  envir <- getNamespace("MLPipe")
  obj_names <- ls(envir=envir)
  for (name in obj_names)
  {
    x <- get(name, envir=envir)
    if (!is.PipeComponentClass(x)) next
    expect_true(validate_pipe_component_class(x, verbose=1))
  }
})


test_that('"is" functions', {
  expect_true(is.PipeComponentClass(PCAExtractor))
  expect_false(is.PipeComponentClass(pipeline()))
  expect_true(is.PipeComponent(feature_standardizer()))
  expect_false(is.PipeComponentClass(mlp()))
})



