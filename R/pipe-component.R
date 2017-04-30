

PipeComponent <- R6::R6Class(
  'PipeComponent',
  public=list(
    object=NULL, parameters=list(),

    transform = function(x, y=NULL) { list(x=x, y=y) },
    fit = function(x, y=NULL) { invisible(self) },
    incfit = function(x, y=NULL) { invisible(self) },
    predict = function(x, y=NULL) { },

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



#' Create a custom component for pipeline
#' @param ... initialization arguments for \code{PipeComponent} class
#' @return \code{PipeComponent} class object
#' @export
pipe_compoenent <- PipeComponent$new

