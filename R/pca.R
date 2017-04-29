

PCAExtractor <- R6::R6Class(
  'PCAExtractor',
  inherit=PipeComponent,

  public=list(
    ncomp=1L, center=TRUE, scale=FALSE,

    fit = function(x, y=NULL)
    {
      self$object <- stats::prcomp(x, center=self$center, scale.=self$scale)
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
#' @param ncomp number of principal components to extract
#' @param center a logical indicating whether variables should be shifted to
#' zero-centered, or numeric vector specifying the arbitrary center values
#' @param scale a logical indicating whether variables should be scaled to
#' unit-variance, or numeric vector specifying the arbitrary scale
#' @export
#' @examples
#' p <- pca_extractor(2, center=TRUE, scale=TRUE)
#' p$fit(USArrests)
#' p$transform(USArrests)
#' p$predict(USArrests)
pca_extractor <- PCAExtractor$new

