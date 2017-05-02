#' @export
FeatureStandardizer <- R6::R6Class(
  'FeatureStandardizer', inherit=PipeComponent,
  public=list(
    method='l2', centers=TRUE, scales=TRUE, nobs=0,
    tol=sqrt(.Machine$double.eps),

    fit = function(x, y=NULL)
    {
      self$nobs <- dim(x)[1]
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

    incr_fit = function(x, y=NULL)
    {
      # if not fitted yet, call the fit function
      if (self$nobs == 0) return(self$fit(x, y))

      n <- dim(x)[1]
      if (self$method == 'l2') {
        # update by weighted average
        tmp <- update_mean_and_sd(self$centers, self$scales, self$nobs, x)
        self$centers <- tmp$m
        self$scales  <- tmp$s
      } else if (self$method == 'l1') {
        warnings('incremental fit for L1-method is approximate')
        cen <- apply(x, MARGIN=2, FUN=mean, na.rm=TRUE)
        sca  <- apply(x, MARGIN=2, FUN=lsr::aad, na.rm=TRUE)
        # update by weighted average
        self$centers <- (n*cen + self$nobs*self$centers)/(n+self$nobs)
        self$scales  <- (n*sca + self$nobs*self$scales)/(n+self$nobs)
      } else if (self$method == 'range') {
        rng <- apply(x, MARGIN=2, FUN=range, na.rm=TRUE)
        # update by element-wise max and mins
        min1 <- self$centers
        max1 <- self$scales
        newmin <- pmin(min1, rng[1,])
        newmax <- pmax(max1, rng[2,])
        self$centers <- newmin
        self$scales <- newmax-newmin
      } else {
        stop('invalid standardization method: ', method)
      }
      self$nobs <- self$nobs + n

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

    inv_transform = function(x, y=NULL)
    {
      x2 <- inv_scale(x, center=self$centers, scale=self$scales)
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
#' @section Class Methods:
#' \describe{
#' \item{\preformatted{fit(x, y = NULL)}}{fit to \code{x} and store the center and scale values}
#' \item{\preformatted{transform(x, y = NULL)}}{returns standardized \code{x} matrix}
#' \item{\preformatted{inv_transform(x, y = NULL)}}{returns pre-standardized \code{x} matrix}
#' \item{\preformatted{incr_fit(x, y = NULL)}}{currently not implemented}
#' }
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
#' w <- fs$inv_transform(z)$x
#' range(mtcars-w)
#'
#' # zero-mean, unit-MAD (mean absolute deviation)
#' fs <- feature_standardizer(method='l1')
#' fs$fit(mtcars)
#' z <- fs$transform(mtcars)$x
#' apply(z, MARGIN=2, FUN=mean)
#' apply(z, MARGIN=2, FUN=lsr::aad)
#' w <- fs$inv_transform(z)$x
#' range(mtcars-w)
#'
#' # standardize to unit interval
#' fs$set_parameters(method='range')
#' fs$fit(mtcars)
#' z <- fs$transform(mtcars)$x
#' apply(z, MARGIN=2, FUN=range)
#' w <- fs$inv_transform(z)$x
#' range(mtcars-w)
#'
#' # incremental fit is allowed for l2 and range method
#' fs$set_parameters(method='l2')
#' fs$fit(mtcars)
#' fs2 <- feature_standardizer(method='l2')
#' fs2$incr_fit(mtcars[1:10,])
#' fs2$incr_fit(mtcars[11:32,])
#' cbind(fs$centers, fs2$centers)
#' cbind(fs$scales, fs2$scales)
#'
#' fs$set_parameters(method='range')
#' fs$fit(mtcars)
#' fs2$set_parameters(method='range')
#' fs2$incr_fit(mtcars[1:14,])
#' fs2$incr_fit(mtcars[15:32,])
#' cbind(fs$centers, fs2$centers)
#' cbind(fs$scales, fs2$scales)
NULL

#' @export
feature_standardizer <- FeatureStandardizer$new

