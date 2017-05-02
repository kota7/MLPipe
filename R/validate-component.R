#' Validate pipe component class
#'
#' Check if an R6 class satisfies the minimum conditions for pipe components
#'
#' @param x R object, typically an \code{R6ClassGenerator}
#' @param verbose integer. if 0, no message is given. if 1, reports only invalidity. it 2+, reports all
#'
#' @return logical that indicates that \code{x} satisfies the minimum conditions for a valid pipe component class
#'
#' @details This function is used by developers of pipe component to check if a class satisfies the minimum properties for a pipe component class.  It is also used by the package developer for package test process to check the validity of the exported pipe component classes.
#' @export
#' @seealso \code{\link{custom_pipe_component}}
#' @examples
#' validate_pipe_component_class(MLP)
#' validate_pipe_component_class(PipeComponent)
validate_pipe_component_class <- function(x, verbose=2)
{

  ## must be an R6 class generator
  ## add a special check for R6 class instance for a convenient message
  if (!R6::is.R6Class(x)) {
    if (R6::is.R6(x)) {
      if (verbose >= 1) message('this is an R6 instance, not a generator')
      return(FALSE)
    }
    if (verbose >= 1) message('not a R6 class generator')
    return(FALSE)
  }
  ## must inherit pipe component or it is PipeComponent itself
  if (!(identical(PipeComponent, x$get_inherit()) ||
        identical(PipeComponent, x))) {
    if (verbose >= 1) message('does not inherit PipeComponent class')
    return(FALSE)
  }

  if (verbose >= 2) cat(':) an R6 class that inherits PipeComponent or itself\n')


  ## check for required public methods
  find_public_method <- function(x, fname)
  {
    # check if fname is x's public method
    inherit <- x$get_inherit()
    if (fname %in% names(x$public_methods)) return(x$public_methods[[fname]])
    if (is.null(inherit)) return(NULL)
    find_public_method(inherit, fname)
  }
  is_public_method <- function(x, fname) { !is.null(find_public_method(x, fname)) }

  required_public_methods <- c('fit', 'incr_fit',
                               'transform', 'inv_transform',
                               'predict', 'predict_proba',
                               'set_parameters', 'initialize')
  for (m in required_public_methods)
  {
    if (!is_public_method(x, m)) {
      if (verbose >= 1) message(m, ' method is not defined')
      return(FALSE)
    }
  }
  if (verbose >= 2) cat(':) found all required public methods\n')

  ## check the arguments of methods
  public_methods_only_xy <- c('fit', 'incr_fit',
                              'transform', 'inv_transform')
  for (m in public_methods_only_xy)
  {
    f <- find_public_method(x, m)
    stopifnot(is.function(f))
    a <- formalArgs(f)
    if (!identical(a, c('x', 'y'))) {
      if (verbose >= 1) {
        message('first two arguments of ', m,
                ' must by "x, y" where we have: ',
                paste0(a, collapse=', '))
        return(FALSE)
      }
    }
  }
  public_methods_x_not_y <- c('predict', 'predict_proba')
  for (m in public_methods_x_not_y)
  {
    f <- find_public_method(x, m)
    stopifnot(is.function(f))
    a <- formalArgs(f)
    if (length(a) < 1 || a[1] != 'x') {
      if (verbose >= 1) {
        message('first argument of ', m,
                ' must by "x" where we have: ',
                paste0(a, collapse=', '))
        return(FALSE)
      }
    }
    if ('y' %in% a) {
      if (verbose >= 1) {
        message(m, ' must not have y argument')
        return(FALSE)
      }
    }
  }
  public_methods_dots <- c('predict', 'predict_proba',
                           'set_parameters', 'initialize')
  for (m in public_methods_dots)
  {
    f <- find_public_method(x, m)
    stopifnot(is.function(f))
    a <- formalArgs(f)
    if (!('...' %in% a)) {
      if (verbose >= 1) {
        message(m, ' must accept "..." argument')
        return(FALSE)
      }
    }
  }
  if (verbose >= 2) cat(':) methods have proper arguments\n')

  if (verbose >= 2) cat(':D passed all validation!\n')
  return(TRUE)
}

