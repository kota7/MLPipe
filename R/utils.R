#' @importFrom magrittr %>%
NULL


inv_scale <- function(x, center, scale., tol=sqrt(.Machine$double.eps))
{
  # inverse of base::scale
  if (is.numeric(scale.)) {
    flg <- (scale. < tol)
    scale.[flg] <- 1
    x <- scale(x, center=FALSE, scale=1/scale.)
    attr(x, "scaled:scale") <- NULL
  }
  if (is.numeric(center)) {
    x <- scale(x, center=-center, scale=FALSE)
    attr(x, "scaled:center") <- NULL
  }
  x
}


update_mean_and_sd <- function(m, s, n, x)
{
  # online update of mean and standard deviation
  #
  # args
  #   m: current mean
  #   s: current sd
  #   n: current nobs
  #   x: new data matrix
  #
  # returns
  #   list of updated m and s

  if (is.vector(x)) {
    dim(x) <- c(length(x), 1)
  }
  stopifnot(length(dim(x)) == 2)
  if (!is.matrix(x)) {
    x <- as.matrix(x)
  }

  new_n <- dim(x)[1] + n
  new_m <- m*n/new_n + apply(x, MARGIN=2, FUN=sum, na.rm=TRUE)/new_n

  # back compute the mean squares
  ms1 <- s^2 + m^2/(n-1)*n
  # new mean squares
  ms <- ms1*(n-1)/(new_n-1) + colSums(x^2, na.rm=TRUE)/(new_n-1)
  new_s <- sqrt(ms - new_n/(new_n-1)*new_m^2)

  list(m=new_m, s=new_s)
}





validate_function <- function(f, arg_condition, arg_mess, run_wo_arg, quiet=FALSE)
{
  # internal routine for checking the validity of a function
  #
  # args
  #   f             : function to check
  #   arg_condition : function "charcter vector -> a logical",
  #                   that validates the argument names of the function
  #   arg_message   : message when invalidity detected for arguments (used only if quiet=FALSE)
  #   run_wo_arg    : logical indidcating if the function should be able to run with no argument
  #   quiet         : if FALSE, prints message
  #
  # retuns
  #   logical (TRUE if no problem detected)

  if (!is.function(f)) {
    if (!quiet) message('not a function')
    return(FALSE)
  }

  a <- methods::formalArgs(f)
  if (!arg_condition(a)) {
    if (!quiet) message(arg_mess)
    return(FALSE)
  }
  if (run_wo_arg) {
    a <- formals(f)
    # if an argument is not ellipsis (...), and the default value is empty,
    # then cannot run without argument
    flg1 <- (names(a) != '...')
    flg2 <- lapply(a, as.character) %>% unlist() %>% `==`('')
    flg <- (flg1 & flg2)
    if (any(flg)) {
      if (!quiet) message(' requires argument(s): ', paste(names(a)[flg], collapse=', '))
      return(FALSE)
    }
  }
  TRUE
}

