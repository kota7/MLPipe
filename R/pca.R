#' Feature extraction by principal component analysis
#' @param ncomp number of principal components to extract
#' @export
#' @examples
#' p <- pca(2)
#' p$fit(list(x=USArrests))
#' p$transform(list(x=USArrests))
#' p$predict(list(x=USArrests))
pca <- function(ncomp, ...) { PCA$new(ncomp, ...) }


PCA <- R6::R6Class(
  'PCA',
  inherit=PipeComponent,

  public=list(
    ncomp=1L,

    fit = function(data)
    {
      self$object <- do.call(prcomp,
                             c(list(data$x), self$parameters))
    },

    transform = function(data)
    {
      data$x <- predict(self$object, data$x)[, 1:self$ncomp]
      data
    },

    predict = function(data)
    {
      predict(self$object, data$x)
    },

    initialize=function(ncomp, ...)
    {
      super$initialize(...)
      self$ncomp <- ncomp
    }
  )
)

