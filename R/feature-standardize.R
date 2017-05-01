#' @export
FeatureStandardizer <- R6::R6Class(
  'FeatureStandardizer', inherit=PipeComponent,
  public=list(
    method='l2', centers=TRUE, scales=TRUE,
    tol=sqrt(.Machine$double.eps),

    fit = function(x, y=NULL)
    {
      if (self$method == 'l2') {
        self$centers <- apply(x, MARGIN=2, FUN=mean, na.rm=TRUE)
        self$scales  <- apply(x, MARGIN=2, FUN=sd, na.rm=TRUE)
      } else if (self$method == 'l1') {
        self$centers <- apply(x, MARGIN=2, FUN=mean, na.rm=TRUE)
        self$scales  <- apply(x, MARGIN=2, FUN=lsr::aad, na.rm=TRUE)
      } else if (self$method == 'range') {
        rng <- apply(x, MARGIN=2, FUN=range, na.rm=TRUE)
        self$centers <- rng[1,]
        self$scales  <- apply(rng, MARGIN=2, FUN=diff)
      } else {
        stop('invalid standardization method: ', method)
      }

      flg <- (self$scales < self$tol)
      if (any(flg)) {
        cat('scales less than ', self$tol, ' are ignored\n')
        self$scales[flg] <- 1
      }

      invisible(self)
    },

    transform = function(x, y=NULL)
    {
      x2 <- scale(x, center=self$centers, scale=self$scales)
      attr(x2, 'scaled:center') <- NULL
      attr(x2, 'scaled:scale')  <- NULL
      list(x=x2, y=y)
    },

    initialize = function(method='l2', tol=sqrt(.Machine$double.eps))
    {
      self$method  <- method
      self$tol <- tol

      invisible(self)
    }
  )
)

#' Feature-wise standardization
#'
#' Standardize each feature of data. Three methodologies are supported:
#' \itemize{
#' \item{"l2": standardize to zero-mean, unit-variance}
#' \item{"l1": standardize to zero-mean, unit-MAD (mean absolute deviation)}
#' \item{"range": standardize to unit interval range}
#' }
#'
#' @name feature_standardizer
#' @aliases FeatureStandardizer
#'
#' @section Usage:
#' \preformatted{feature_standardizer(method = 'l2', tol = sqrt(.Machine$double.eps)}
#'
#' @section Arguments:
#' \describe{
#' \item{method}{either "l2", "l1" or "range"}
#' \item{tol}{positive real number, scaling is not conducted if scale is below this level}
#' }
#'
#' @section Value:
#' \code{FeatureStandardizer} class object
#'
#' @section Details:
#' When fitted to data, the feature-wise centers and scales are stored in the object,
#' and transformation is conducted using these center and scale values.
#' Therefore, when applied to new data, they are not exactly standardized.
#'
#' Uses \code{\link[lsr]{aad}} as the backend calculation for "l1" method
#'
#' @examples
#' data(mtcars)
#' # zero-mean, unit-variance standardization
#' fs <- feature_standardizer(method='l2')
#' fs$fit(mtcars)
#' z <- fs$transform(mtcars)$x
#' apply(z, MARGIN=2, FUN=mean)
#' apply(z, MARGIN=2, FUN=sd)
#'
#' # zero-mean, unit-MAD (mean absolute deviation)
#' fs <- feature_standardizer(method='l1')
#' fs$fit(mtcars)
#' z <- fs$transform(mtcars)$x
#' apply(z, MARGIN=2, FUN=mean)
#' apply(z, MARGIN=2, FUN=lsr::aad)
#'
#' # standardize to unit interval
#' fs$set_parameters(method='range')
#' fs$fit(mtcars)
#' z <- fs$transform(mtcars)$x
#' apply(z, MARGIN=2, FUN=range)
NULL

#' @export
feature_standardizer <- FeatureStandardizer$new

