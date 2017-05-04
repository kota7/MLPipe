
#' @export
PipeComponent <- R6::R6Class(
  'PipeComponent',
  public=list(
    object=NULL,
    not_parameters=c('object'),

    transform     = function(x=NULL, y=NULL) { list(x=x, y=y) },
    inv_transform = function(x=NULL, y=NULL) { list(x=x, y=y) },

    fit      = function(x=NULL, y=NULL) { invisible(NULL) },
    incr_fit = function(x=NULL, y=NULL) { invisible(NULL) },

    predict        = function(x=NULL, ...) { NULL },
    predict_proba  = function(x=NULL, ...) { NULL },

    initialize = function(...) { invisible(self) },

    set_parameters = function(...)
    {
      params <- list(...)
      unnamed <- if (is.null(names(params))) rep(TRUE, length(params)) else names(params)==''
      if (any(unnamed)) {
        warning('unnamed parameters are ignored')
        params <- params[!unnamed]
      }
      if (length(params)==0) return(invisible(self))

      flg <- names(params) %in% self$not_parameters
      if (any(flg)) {
        warning(names(params[flg]), ': not parameters')
        params <- params[!flg]
      }
      if (length(params)==0) return(invisible(self))

      Map(function(name, value) self[[name]] <- value,
          names(params), params)
      invisible(self)
    },
    get_parameters = function(param_names=character(0))
    {
      out <- as.list(self)
      # out <- Filter(function(a) !is.function(a), out)
      # flg <- names(out) %in% self$not_parameters
      # if (any(flg)) out <- out[!flg]
      # # remove if name starts with a dot (like .__enclos_env_)
      # out <- out[!grepl('^\\.', names(out))]

      all_name <- self$get_parameter_names()
      if (is.null(param_names) || length(param_names)==0) return(out[all_name])
      out[intersect(all_name, param_names)]
    },
    get_parameter_names = function()
    {
      out <- names(self)
      out <- Filter(function(a) !is.function(self[[a]]), out)
      out <- setdiff(out, self$not_parameters)
      out <- out[!grepl('^\\.', out)]
      out
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
#' \item{\code{not_parameters}}{character vector that specifies the names of fields not considered as parameters}
#' }
#'
#' @section Class Methods:
#' \describe{
#' \item{\preformatted{fit(x=NULL, y=NULL)}}{do nothing}
#' \item{\preformatted{incr_fit(x=NULL, y=NULL)}}{do nothing}
#' \item{\preformatted{transpose(x=NULL, y=NULL)}}{return \code{x} and \code{y} as-is}
#' \item{\preformatted{inv_transform(x=NULL, y=NULL)}}{return \code{x} and \code{y} as-is}
#' \item{\preformatted{predict(x=NULL, ...)}}{return \code{NULL}}
#' \item{\preformatted{predict_proba(x=NULL, ...)}}{return \code{NULL}}
#'
#' \item{\preformatted{set_parameters(...)}}{change parameter values, cannot create new fields}
#' \item{\preformatted{get_parameters(param_names=character(0)}}{return parameters as list. if param_names are the names is null or length 0, return all parameters}
#' \item{\preformatted{get_parameter_namess()}}{return the names of all parameters}
#' }
#'
#' @examples
#' p <- pipe_component()
#' p$fit(x=c(1,5,8), y=c('a','b','a'))  # nothing happens
#' p$transform(x=1:10, y=1:10)          # return data as-is
#' p$get_parameters()                   # parameters as list
#' p$get_parameter_names()              # parameter names
NULL

#' @export
pipe_component <- PipeComponent$new



#' Is this a PipeComponent class or an object?
#'
#' Checks if x is a PipeComponent class or an object
#'
#' @param x R object
#' @return logical
#' @export
#' @examples
#' is.PipeComponentClass(MLP)    # true
#' is.PipeComponentClass(mlp())  # false because this is an instance
#' is.PipeComponent(mlp())       # true
is.PipeComponent <- function(x) { inherits(x, 'PipeComponent') }

#' @export
#' @rdname is.PipeComponent
is.PipeComponentClass <- function(x) { is.R6ClassOf(x, PipeComponent) }
