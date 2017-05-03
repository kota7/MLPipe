is.R6ClassOf <- function(x, class)
{
  # check if x is an R6 class of a certain class, or it inherits it
  #
  # args:
  #   x    : R object
  #   class: R6 class generator
  #
  # returns:
  #   logical

  # must be an R6 class in the first place
  if (!R6::is.R6Class(x)) return(FALSE)

  # is x the class?
  if (identical(x, class)) return(TRUE)

  # maybe the parent?
  inherit <- x$get_inherit()
  if (is.null(inherit)) return(FALSE)
  is.R6ClassOf(inherit, class)
}




find_public_method <- function(x, method_name)
{
  # get a public method
  #
  # args
  #   x          : R6 class instance
  #   method_name: function name to look for
  #
  # return
  #   if found, return the function object. otherwise NULL
  inherit <- x$get_inherit()
  if (method_name %in% names(x$public_methods)) return(x$public_methods[[method_name]])
  if (is.null(inherit)) return(NULL)
  find_public_method(inherit, method_name)
}


is_public_method <- function(x, method_name)
{
  # check if x has a public method fname
  #
  # args
  #   x          : R6 class instance
  #   method_name: function name to look for
  #
  # return
  #   TRUE if the method exists
  is.function(find_public_method(x, method_name))
}


validate_public_method <- function(x, method_name, arg_condition=function(a) TRUE,
                                   arg_mess='', run_wo_arg=FALSE, quiet=FALSE)
{
  # validate the properties of a public method
  #
  # args
  #   x             : R6 class instance
  #   method_name   : function name to look for
  #   arg_condition : function "charcter vector -> a logical",
  #                   that validates the argument names of the function
  #   arg_message   : message when invalidity detected for arguments (used only if quiet=FALSE)
  #   run_wo_arg    : logical indidcating if the function should be able to run with no argument
  #   quiet         : if FALSE, prints message
  #
  # retuns
  #   logical (TRUE if no problem detected)

  f <- find_public_method(x, method_name)
  if (!is.function(f)) {
    if (!quiet) message(method_name, ': not defined')
    return(FALSE)
  }

  arg_mess <- paste(method_name, arg_mess, sep=': ')
  validate_function(f, arg_condition, arg_mess, run_wo_arg, quiet)
}

