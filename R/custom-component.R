#' Custom pipeline component
#'
#' Used to define a custom pipeline component.  Users can define functions \code{fit}, \code{transform}, \code{predict}, or \code{incfit} with desired behavior.
#'
#' @param classname string of class name
#' @param fit fit function
#' @param transform transform function
#' @param predict predict function
#' @param incfit incfit function
#' @param initialize intialize function
#' @param ... additional class attributes, they must be given with names and there must be no name conflict
#' @param as_private names of additional parameters to be stored as private fields
#'
#' @return an R6ClassGenerator
#'
#' @details This function is used to define a custom pipeline component.
#' Users can define functions \code{fit}, \code{transform}, \code{predict}, \code{incfit} or \code{initialize} with desired behavior, together with addional public and private fields.
#'
#' To be properly incorporated to a pipeline framework, the functions should satisfy the following properties:
#' \itemize{
#' \item{\code{fit}, \code{incfit}, \code{transform} and \code{predict} functions should take exactly two arguments \code{x} and \code{y}}
#' \item{\code{initialize}, \code{fit} and \code{incfit} functions should return \code{invisible(self)}}
#' \item{\code{transform} function should return a list of \code{x} and \code{y}}
#' }
#' Typically, \code{self$object} is used to store the fitted model object, and is updated by \code{fit} and \code{incfit} functions.  Alternatively, one may also define additional class attributes to store relevant information.
#'
#' @export
#'
#' @examples
#' OLSPipe <- custom_pipe_component(
#'   fit = function(x, y) {
#'     x <- cbind(as.matrix(x), 1)
#'     self$object <- solve(crossprod(x), crossprod(x,y))
#'     invisible(self)
#'    },
#'   predict = function(x, y=NULL) {
#'     cbind(as.matrix(x), 1) %*% self$object
#'   }
#' )
#' o <- OLSPipe$new()
#' data(mtcars)
#' x <- mtcars[, c('wt', 'am')]
#' y <- mtcars[['mpg']]
#' o$fit(x, y)
#' o$predict(x)
#'
#' MeanCalculator <- custom_pipe_component(
#'   fit = function(x, y=NULL) {
#'     self$sum <- sum(x)
#'     self$n <- length(x)
#'     invisible(self)
#'   },
#'   incfit = function(x, y=NULL) {
#'     self$n <- self$n + length(x)
#'     self$sum <- self$sum + sum(x)
#'     invisible(self)
#'   },
#'   predict = function(x=NULL, y=NULL) {
#'     self$sum / self$n
#'   },
#'   initialize = function() {
#'     invisible(self)
#'   },
#'   sum=0, n=0
#' )
#' m <- MeanCalculator$new()
#' m$fit(1:9)
#' m$predict()
#' m$incfit(10)
#' m$predict()
custom_pipe_component <- function(classname='custom',
                                  fit=NULL, transform=NULL,
                                  predict=NULL, incfit=NULL, initialize=NULL,
                                  ..., as_private=character(0))
{
  new_class <- R6::R6Class(classname, inherit=PipeComponent)
  if (is.function(fit))        new_class$set('public', 'fit', fit)
  if (is.function(incfit))     new_class$set('public', 'incfit', incfit)
  if (is.function(transform))  new_class$set('public', 'transform', transform)
  if (is.function(predict))    new_class$set('public', 'predict', predict)
  if (is.function(initialize)) new_class$set('public', 'initialize', initialize)

  new_params <- list(...)
  if (length(new_params) > 0) {
    unnamed <- if (is.null(names(new_params))) rep(TRUE, length(new_params)) else (names(new_params) == "")
    if (any(unnamed)) {
      warning('unnamed additional parameters are ignored: ', new_params[unnamed])
      new_params <- new_params[!new_params]
    }

    public_params  <- new_params[!(names(new_params) %in% as_private)]
    private_params <- new_params[names(new_params) %in% as_private]
    Map(function(name, value) new_class$set('public', name, value),
        names(public_params), public_params)
    Map(function(name, value) new_class$set('private', name, value),
        names(private_params), private_params)
  }

  new_class
}


