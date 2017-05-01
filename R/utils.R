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
