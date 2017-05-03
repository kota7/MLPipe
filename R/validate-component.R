#' Validate pipe component class
#'
#' Check if an R6 class satisfies the minimum conditions for pipe components
#'
#' @param x R object, typically an \code{R6ClassGenerator}
#' @param verbose integer. if 0, no message is given. if 1, reports only invalidity. if 2, reports simple progress. if 3+, reports details.
#'
#' @return logical that indicates that \code{x} satisfies the minimum conditions for a valid pipe component class
#'
#' @details This function is used by developers of pipe component to check if a class satisfies the minimum properties for a pipe component class.  It is also used by the package developer for package test process to check the validity of the exported pipe component classes.
#' @export
#' @seealso \code{\link{custom_pipe_component}}
#' @examples
#' validate_pipe_component(MLP)
#' validate_pipe_component(PipeComponent)
#'
#' # bad examples
#' validate_pipe_component(custom_pipe_component(predict=function(x, y, ...) { }))
#' validate_pipe_component(custom_pipe_component(get_parameters=function(z) { }))
validate_pipe_component <- function(x, verbose=3)
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
  required_public_methods <- c('fit', 'incr_fit',
                               'transform', 'inv_transform',
                               'predict', 'predict_proba',
                               'set_parameters', 'initialize',
                               'get_parameters', 'get_parameter_names')
  for (m in required_public_methods)
  {
    if (!is_public_method(x, m)) {
      if (verbose >= 1) message(m, ' method is not defined')
      return(FALSE)
    }
    if (verbose>=3) cat(' ', m, '...exists\n')
  }
  if (verbose >= 2) cat(':) found all required public methods\n')




  ## methods that must take (x, y) as arguments
  public_methods_only_xy <- c('fit', 'incr_fit', 'transform', 'inv_transform')
  arg_condition <- function(a) {(length(a) == 2) && all(a==c('x','y'))}
  mess <- 'must take (x, y) as arguments and no others'
  for (m in public_methods_only_xy)
  {
    if (!validate_public_method(x, m, arg_condition=arg_condition, arg_mess=mess,
                                quiet=(verbose<1))) return(FALSE)
    if (verbose>=3) cat(' ', m, '...good\n')
  }

  ## methods that must take x but not y
  public_methods_x_not_y <- c('predict', 'predict_proba')
  arg_condition <- function(a) { length(a) > 1 && a[1]=='x' && !('y' %in% a) }
  mess <- 'must take x but not y as argument'
  for (m in public_methods_x_not_y)
  {
    if (!validate_public_method(x, m, arg_condition=arg_condition, arg_mess=mess,
                                quiet=(verbose<1))) return(FALSE)
    if (verbose>=3) cat(' ', m, '...good\n')
  }

  ## methods that must accepts ...
  public_methods_dots <- c('predict', 'predict_proba', 'set_parameters', 'initialize')
  arg_condition <- function(a) { '...' %in% a }
  mess <- ' must accept "..." argument'
  for (m in public_methods_dots)
  {
    if (!validate_public_method(x, m, arg_condition=arg_condition, arg_mess=mess,
                                quiet=(verbose<1))) return(FALSE)
    if (verbose>=3) cat(' ', m, '...good\n')
  }


  ## methods that must be able to run with no arguments
  public_methods_no_arg_ok <- 'get_parameters'
  for (m in public_methods_no_arg_ok)
  {
    if (!validate_public_method(x, m, run_wo_arg=TRUE, quiet=(verbose<1))) return(FALSE)
    if (verbose>=3) cat(' ', m, '...good\n')
  }




  if (verbose >= 2) cat(':) methods have proper arguments\n')

  if (verbose >= 2) cat(':D passed all validation!\n')
  return(TRUE)
}

