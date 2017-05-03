library(testthat)
library(MLPipe)


context('custom component')


test_that('class', {

  a <- custom_pipe_component(
    predict = function(x, ...) { "hello" }
  )
  expect_true(is.PipeComponentClass(a))
  expect_true(is.PipeComponent(a$new()))
})


test_that('bad custom class', {

  # predict and predict_proba functions cannot take y
  expect_false(validate_pipe_component(
    custom_pipe_component(predict = function(x, y, ...) { }),
    verbose=1))
  expect_false(validate_pipe_component(
    custom_pipe_component(predict_proba = function(x, y, ...) { }),
    verbose=1))

  # predict, predict_proba, initialize, set_parameters functions must allow ...
  expect_false(validate_pipe_component(
    custom_pipe_component(initialize = function() { }),
    verbose=1))
  expect_false(validate_pipe_component(
    custom_pipe_component(set_parameters = function() { }),
    verbose=1))
  expect_false(validate_pipe_component(
    custom_pipe_component(predict = function(x) { }),
    verbose=1))
  expect_false(validate_pipe_component(
    custom_pipe_component(predict_proba = function(x) { }),
    verbose=1))

  # fit, incr_fit, transform, inv_transform function must
  # take x and y and not any more
  expect_false(validate_pipe_component(
    custom_pipe_component(fit = function(x, y, z) { }),
    verbose=1))
  expect_false(validate_pipe_component(
    custom_pipe_component(incr_fit = function(y, z) { }),
    verbose=1))
  expect_false(validate_pipe_component(
    custom_pipe_component(inv_transform = function(x, z) { }),
    verbose=1))
  expect_false(validate_pipe_component(
    custom_pipe_component(transform = function(x, y, ...) { }),
    verbose=1))

  # get_paramters must run with no argument
  expect_false(validate_pipe_component(
    custom_pipe_component(get_parameters = function(a, ...) { }),
    verbose=1))


})
