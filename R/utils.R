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
