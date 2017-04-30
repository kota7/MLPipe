#' @importFrom magrittr %>%
NULL


inv_scale <- function(x, center, scale.)
{
  # inverse of base::scale
  if (is.numeric(scale.)) {
    x <- scale(x, center=FALSE, scale=1/scale.)
    attr(x, "scaled:scale") <- NULL
  }
  if (is.numeric(center)) {
    x <- scale(x, center=-center, scale=FALSE)
    attr(x, "scaled:center") <- NULL
  }
  x
}
