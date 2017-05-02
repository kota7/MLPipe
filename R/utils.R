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





# multiple value assigment
# but slow... not in use currently
# Reference:
#   https://strugglingthroughproblems.wordpress.com/2010/08/27/matlab-style-multiple-assignment-in%C2%A0r/
#
# Usage:
#   lhs(x, y) %=% c(1, 2)
lhs <- function(...)
{
  o <- as.list(substitute(list(...)))[-1L]
  class(o) <- 'lhs'
  o
}

`%=%` <- function(l, r, ...) UseMethod('%=%')

`%=%.lhs` <- function(l, r)
{
  if (length(l) > length(r)) stop('LHS is longer than RHS')
  envir = as.environment(-1)
  for (i in seq_along(l)) do.call('<-', list(l[[i]], r[[i]]), envir=envir)
}
