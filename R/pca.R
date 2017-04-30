


PCAExtractor <- R6::R6Class(
  'PCAExtractor',
  inherit=PipeComponent,

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

    predict = function(x, y=NULL)
    {
      predict(self$object, x)
    },

    initialize=function(ncomp, center=TRUE, scale=FALSE)
    {
      super$initialize()
      self$ncomp <- ncomp
      self$center <- center
      self$scale <- scale
    }
  )
)

#' Feature extraction by principal component analysis
#'
#' @name pca
#' @aliases pca_extractor
#' @section Usage:
#' \preformatted{pca_extractor(ncomp, center = TRUE, scale = TRUE)}
#'
#' @section Arguments:
#' \describe{
#' \item{\code{ncomp}}{number of principal components to extract}
#' \item{\code{center}}{either a logical value that indicates whether variables should be                centered to zero-mean, or numeric vector of center values}
#' \item{\code{scale}}{either a logical value that indicates whether variables should be scaled to unit-variance, or numeric vector of scale values}
#' }
#'
#' @section Class Methods:
#' \describe{
#' \item{\preformatted{fit(x, y = NULL)}}{conduct principal component analysis of \code{x}}
#' \item{\preformatted{transform(x, y = NULL)}}{extract principal components of \code{x}}
#' \item{\preformatted{predict(x, y = NULL)}}{returns all principal components of \code{x}}
#' \item{\preformatted{incfit(x, y = NULL)}}{not implemented (nothing happens)}
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
NULL

#' @export
pca_extractor <- PCAExtractor$new

