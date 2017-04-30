
#' @export
PipeComponent <- R6::R6Class(
  'PipeComponent',
  public=list(
    object=NULL,

    transform = function(x=NULL, y=NULL) { list(x=x, y=y) },
    fit       = function(x=NULL, y=NULL) { invisible(NULL) },
    predict   = function(x=NULL, y=NULL) { NULL },
    incr_fit  = function(x=NULL, y=NULL) { invisible(NULL) },

    inv_transform = function(x=NULL, y=NULL) { list(x=x, y=y) },
    predict_proba  = function(x=NULL, y=NULL) { NULL },

    initialize = function() { invisible(self) }
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
#' \preformatted{pipe_component()}
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
#' \item{\preformatted{predict(x=NULL, y=NULL)}}{return \code{NULL}}
#' \item{\preformatted{incr_fit(x=NULL, y=NULL)}}{do nothing}
#' \item{\preformatted{predict_proba(x=NULL, y=NULL)}}{return \code{NULL}}
#' \item{\preformatted{inv_transform(x=NULL, y=NULL)}}{return \code{x} and \code{y} as-is}
#' }
#'
#' @examples
#' p <- pipe_component()
#' p$fit(x=c(1,5,8), y=c('a','b','a'))  # nothing happens
#' p$transform(x=1:10, y=1:10)          # return data as-is
NULL

#' @export
pipe_component <- PipeComponent$new
