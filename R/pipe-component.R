
#' @export
PipeComponent <- R6::R6Class(
  'PipeComponent',
  public=list(
    object=NULL,

    transform = function(x=NULL, y=NULL) { list(x=x, y=y) },
    inv_transform = function(x=NULL, y=NULL) { list(x=x, y=y) },

    fit       = function(x=NULL, y=NULL) { invisible(NULL) },
    incr_fit  = function(x=NULL, y=NULL) { invisible(NULL) },

    predict   = function(x=NULL, ...) { NULL },
    predict_proba  = function(x=NULL, ...) { NULL },

    initialize = function(...) { invisible(self) },

    set_parameters = function(..., as_private=character(0))
    {
      params <- list(...)
      unnamed <- if (is.null(names(params))) rep(TRUE, length(params)) else names(params)==''
      if (any(unnamed)) {
        warning('unnamed parameters are ignored')
        params <- params[!unnamed]
      }
      if (length(params)==0) return(invisible(self))

      pri_params <- params[names(params) %in% as_private]
      pub_params <- params[!(names(params) %in% as_private)]

      if (length(pri_params) > 0) {
        Map(function(name, value) private[[name]] <- value,
            names(pri_params), pri_params)
      }
      if (length(pub_params) > 0) {
        Map(function(name, value) self[[name]] <- value,
            names(pub_params), pub_params)
      }

      invisible(self)
    }
  )
)



#' Default (do-nothing) pipeline component
#'
#' This is a pipeline component doing nothing.
#' It is mainly used like a virtual class,
#' where functions are defined in children classes.
#'
#' @name pipe_component
#' @aliases PipeComponent
#'
#' @section Usage:
#' \preformatted{pipe_component(...)}
#'
#' @section Arguments:
#' \describe{
#' \item{\code{...}}{not in use}
#' }
#'
#' @section Value:
#' \code{PipeComponent} class object
#'
#' @section Class Attributes:
#' \describe{
#' \item{\code{objects}}{an R object. Typically used to store a model object}
#' }
#'
#' @section Class Methods:
#' \describe{
#' \item{\preformatted{fit(x=NULL, y=NULL)}}{do nothing}
#' \item{\preformatted{transpose(x=NULL, y=NULL)}}{return \code{x} and \code{y} as-is}
#' \item{\preformatted{predict(x=NULL, ...)}}{return \code{NULL}}
#' \item{\preformatted{incr_fit(x=NULL, y=NULL)}}{do nothing}
#' \item{\preformatted{predict_proba(x=NULL, ...)}}{return \code{NULL}}
#' \item{\preformatted{inv_transform(x=NULL, y=NULL)}}{return \code{x} and \code{y} as-is}
#' }
#'
#' @examples
#' p <- pipe_component()
#' p$fit(x=c(1,5,8), y=c('a','b','a'))  # nothing happens
#' p$transform(x=1:10, y=1:10)          # return data as-is
#'
#' p$set_parameters(object=1) # use this to update attributes
#' # but cannot do so for functions
#' #p$set_parameters(fit = function() { 'hello world' })
NULL

#' @export
pipe_component <- PipeComponent$new
