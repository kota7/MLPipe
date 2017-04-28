#' Create a custom component for pipeline
#' @param transofrm The function that transforms data.
#' @param fit The function that creates a model object
#' @param predict The function that makes prediction from new data
#' @param incfit The function that updates the model object
#' @param ... Parameters to be passed to other methods.
#' @return \code{PipeComponent} class object
#' @export
pipe_compoenent <- function(fit=NULL, transform=NULL,
                            predict=NULL, incfit=NULL, ...)
{
  PipeCompoenent$new(fit=fit, transform=transform,
                     predict=predict, incfit=incfit, ...)
}


#' Custom component for pipeline
#' @export
PipeComponent <- R6::R6Class(
  'PipeComponent',
  public=list(
    object=NULL, parameters=list(),

    transform = function(data) { data },
    fit = function(data) { },
    incfit = function(data) { },
    predict = function(data) { NULL },

    set_parameters = function(...)
    {
      tmp <- list(...)
      self$parameters <- self$parameters[setdiff(names(self$parameters),
                                                 names(tmp))]
      self$parameters <- c(self$parameters, tmp)
    },

    initialize = function(fit=NULL, transform=NULL,
                          predict=NULL, incfit=NULL, ...)
    {
      if (!is.null(transform)) self$transform <- transform
      if (!is.null(fit)) self$fit <- fit
      if (!is.null(predict)) self$predict <- predict
      if (!is.null(incfit)) self$incfit <- incfit
      self$set_parameters(...)
    }
  )
)

