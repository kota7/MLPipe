

#' @export
PCAExtractor <- R6::R6Class(
  'PCAExtractor', inherit=PipeComponent,

  public=list(
    ncomp=1L, center=TRUE, scale=FALSE,

    fit = function(x, y=NULL)
    {
      self$object <- stats::prcomp(x, center=self$center, scale.=self$scale)
      invisible(self)
    },

    transform = function(x, y=NULL)
    {
      list(x=predict(self$object, x)[, 1:self$ncomp], y=y)
    },

    predict = function(x, ...)
    {
      predict(self$object, x)
    },

    inv_transform = function(x, y=NULL)
    {
      x <- as.matrix(x)
      x2 = tcrossprod(x, self$object$rotation[, 1:self$ncomp]) %>%
      # back scale
        inv_scale(self$object$center, self$object$scale)
      list(x=x2, y=y)
    },

    initialize=function(ncomp, center=TRUE, scale=FALSE, ...)
    {
      super$initialize()
      self$ncomp <- ncomp
      self$center <- center
      self$scale <- scale
      invisible(self)
    }
  )
)

#' Feature extraction by principal component analysis
#'
#' @name pca
#' @aliases pca_extractor PCAExtractor
#'
#' @section Usage:
#' \preformatted{pca_extractor(ncomp, center = TRUE, scale = TRUE)}
#'
#' @section Arguments:
#' \describe{
#' \item{\code{ncomp}}{number of principal components to extract}
#' \item{\code{center}}{either a logical value that indicates whether variables should be centered to zero-mean, or numeric vector of center values}
#' \item{\code{scale}}{either a logical value that indicates whether variables should be scaled to unit-variance, or numeric vector of scale values}
#' }
#'
#' @section Value:
#' \code{PCAExtractor} class object
#'
#' @section Class Methods:
#' \describe{
#' \item{\preformatted{fit(x, y = NULL)}}{conduct principal component analysis of \code{x}}
#' \item{\preformatted{transform(x, y = NULL)}}{extract principal components of \code{x}}
#' \item{\preformatted{predict(x, y = NULL)}}{returns all principal components of \code{x}}
#' }
#'
#' @section Details:
#' Uses \code{\link[stats]{prcomp}} as the backend.
#'
#' @examples
#' p <- pca_extractor(2, center=TRUE, scale=TRUE)
#' p$fit(USArrests)
#' p$transform(USArrests)
#' p$predict(USArrests)
#' # inv_transform partially recovers the original data
#' cor(as.numeric(as.matrix(USArrests)),
#'     as.numeric(p$inv_transform(p$transform(USArrests)$x)$x))
#'
#' p2 <-  pca_extractor(4, center=TRUE, scale=TRUE)
#' p2$fit(USArrests)
#' p2$transform(USArrests)
#' p2$predict(USArrests)
#' # when ncomp = ncol(x), inv_transform recovers the data perfectly
#' cor(as.numeric(as.matrix(USArrests)),
#'     as.numeric(p2$inv_transform(p2$transform(USArrests)$x)$x))
NULL

#' @export
pca_extractor <- PCAExtractor$new

